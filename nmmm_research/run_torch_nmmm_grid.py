"""Run a configurable NMMM grid over scenarios, feature sets, and objectives."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

import pandas as pd

from nmmm import evaluate_contribution_recovery, evaluate_prediction_fit, make_synthetic_mmm_panel
from nmmm.evaluation import evaluate_curve_recovery, evaluate_oracle_fit, evaluate_parameter_recovery, summarize_recovery
from nmmm.torch_training import fit_torch_nmmm, save_torch_nmmm_checkpoint


ROOT = Path(__file__).resolve().parent
OUT = ROOT / "outputs" / "torch_grid"


def _feature_sets(mode: str):
    if mode == "quick":
        return {
            "support_spend": ["support", "spend"],
            "rich_media": ["support", "spend", "impressions", "clicks", "grps", "reach", "frequency"],
        }
    return {
        "support_only": ["support"],
        "support_spend": ["support", "spend"],
        "rich_media": ["support", "spend", "impressions", "clicks", "grps", "reach", "frequency"],
    }


def _scenarios(mode: str):
    if mode == "quick":
        return ["standard", "messy_realistic"]
    return ["standard", "messy_realistic", "hostile_collinear", "weak_geo"]


def _seeds(mode: str):
    if mode == "quick":
        return [20260603]
    return [20260603, 20260604]


def _supervision_weights(mode: str):
    if mode == "quick":
        return [0.0, 0.25]
    return [0.0, 0.10, 0.25, 0.50]


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", choices=["quick", "full"], default="quick")
    parser.add_argument("--epochs", type=int, default=650)
    args = parser.parse_args()

    try:
        import torch  # noqa: F401
    except Exception:
        print("Torch is not installed. Install with: pip install torch")
        return

    OUT.mkdir(parents=True, exist_ok=True)
    rows = []
    run_id = 0
    for scenario in _scenarios(args.mode):
        for seed in _seeds(args.mode):
            synth = make_synthetic_mmm_panel(
                n_weeks=156,
                n_geos=8,
                curve_type="hill",
                scenario=scenario,
                seed=seed,
            )
            for feature_set_name, media_features in _feature_sets(args.mode).items():
                for supervision_weight in _supervision_weights(args.mode):
                    run_id += 1
                    name = f"{run_id:03d}_{scenario}_{seed}_{feature_set_name}_sup{supervision_weight:g}"
                    out_dir = OUT / name
                    out_dir.mkdir(parents=True, exist_ok=True)
                    print(f"Running {name}", flush=True)
                    result = fit_torch_nmmm(
                        synth.panel,
                        truth_media=synth.truth_media if supervision_weight > 0 else None,
                        curve_type="hill",
                        media_feature_inputs=media_features,
                        holdout_weeks=13,
                        epochs=args.epochs,
                        learning_rate=0.01,
                        weight_decay=1e-4,
                        validation_weeks=8,
                        early_stopping_patience=120,
                        early_stopping_min_delta=1e-5,
                        seed=seed,
                        initialize_from_baseline=True,
                        contribution_supervision_weight=supervision_weight,
                        hierarchical_media=True,
                        group_media_max_log_multiplier=0.25,
                        group_media_shrinkage=0.05,
                        market_size_col=None,
                        scale_media_by_market_size=False,
                    )
                    prediction_metrics = evaluate_prediction_fit(result.predictions)
                    contribution_recovery = evaluate_contribution_recovery(result.predictions, synth.truth_media)
                    curve_recovery = evaluate_curve_recovery(result.response_curves, synth.response_curves)
                    parameter_recovery = evaluate_parameter_recovery(result.learned_params, synth.truth_params)
                    oracle_metrics = evaluate_oracle_fit(synth.panel, synth.truth_media, holdout_weeks=13)
                    summary = summarize_recovery(
                        contribution_recovery,
                        prediction_metrics,
                        curve_recovery,
                        parameter_recovery,
                        oracle_metrics,
                    )
                    result.learned_params.to_csv(out_dir / "learned_params.csv", index=False)
                    result.training_history.to_csv(out_dir / "training_history.csv", index=False)
                    summary.to_csv(out_dir / "summary_recovery.csv", index=False)
                    synth.panel.to_csv(out_dir / "training_panel.csv", index=False)
                    synth.truth_media.to_csv(out_dir / "training_truth_media.csv", index=False)
                    synth.truth_params.to_csv(out_dir / "training_truth_params.csv", index=False)
                    synth.response_curves.to_csv(out_dir / "training_truth_response_curves.csv", index=False)
                    save_torch_nmmm_checkpoint(
                        result,
                        out_dir / "model_checkpoint.pt",
                        extra={
                            "scenario": scenario,
                            "seed": seed,
                            "feature_set": feature_set_name,
                            "media_features": media_features,
                            "contribution_supervision_weight": supervision_weight,
                            "training_data": "training_panel.csv",
                            "truth_media": "training_truth_media.csv",
                            "summary": summary.to_dict(orient="records"),
                        },
                    )
                    with open(out_dir / "settings.json", "w", encoding="utf-8") as f:
                        json.dump(result.settings, f, indent=2, sort_keys=True)

                    row = {
                        "run_id": run_id,
                        "scenario": scenario,
                        "seed": seed,
                        "feature_set": feature_set_name,
                        "media_features": ",".join(media_features),
                        "contribution_supervision_weight": supervision_weight,
                    }
                    for _, metric_row in summary.iterrows():
                        row[str(metric_row["metric"])] = float(metric_row["value"])
                    rows.append(row)
                    pd.DataFrame(rows).to_csv(OUT / f"torch_grid_summary_{args.mode}.csv", index=False)

    summary_df = pd.DataFrame(rows)
    summary_df.to_csv(OUT / f"torch_grid_summary_{args.mode}.csv", index=False)
    if not summary_df.empty and "known_truth_recovery_score_0_100" in summary_df.columns:
        print(summary_df.sort_values("known_truth_recovery_score_0_100", ascending=False).head(12).to_string(index=False))
    print(f"Outputs: {OUT}")


if __name__ == "__main__":
    main()
