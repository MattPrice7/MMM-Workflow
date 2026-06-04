"""Run a configurable grid for the hierarchical TFT MMM challenger."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

import pandas as pd

from nmmm import evaluate_contribution_recovery, evaluate_prediction_fit, make_synthetic_mmm_panel
from nmmm.evaluation import (
    evaluate_baseline_recovery,
    evaluate_curve_recovery,
    evaluate_oracle_fit,
    evaluate_parameter_recovery,
    summarize_recovery,
)
from nmmm.training_data_validation import validate_nmmm_training_data, write_training_data_validation
from nmmm.torch_tft_training import fit_tft_mmm, save_tft_mmm_checkpoint


ROOT = Path(__file__).resolve().parent
OUT = ROOT / "outputs" / "tft_grid"


def _feature_sets(mode: str) -> dict[str, list[str]]:
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


def _scenarios(mode: str) -> list[str]:
    if mode == "quick":
        return ["standard", "messy_realistic"]
    return ["standard", "messy_realistic", "hostile_collinear", "weak_geo"]


def _seeds(mode: str) -> list[int]:
    if mode == "quick":
        return [20260603]
    return [20260603, 20260604]


def _supervision_weights(mode: str) -> list[float]:
    if mode == "quick":
        return [0.0, 0.50]
    return [0.0, 0.10, 0.25, 0.50]


def _curve_types(mode: str) -> list[str]:
    if mode == "quick":
        return ["hill", "weibull"]
    return ["hill", "weibull"]


def _channels(n_channels: int = 4) -> list[str]:
    return [f"ch_{i + 1:02d}" for i in range(int(n_channels))]


def _row_from_summary(
    summary_path: Path,
    run_id: int,
    scenario: str,
    curve_type: str,
    seed: int,
    feature_set_name: str,
    media_features: list[str],
    supervision_weight: float,
) -> dict[str, object]:
    summary = pd.read_csv(summary_path)
    row: dict[str, object] = {
        "run_id": run_id,
        "scenario": scenario,
        "curve_type": curve_type,
        "seed": seed,
        "feature_set": feature_set_name,
        "media_features": ",".join(media_features),
        "contribution_supervision_weight": supervision_weight,
    }
    for _, metric_row in summary.iterrows():
        row[str(metric_row["metric"])] = float(metric_row["value"])
    return row


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", choices=["quick", "full"], default="quick")
    parser.add_argument("--epochs", type=int, default=350)
    parser.add_argument(
        "--legacy-channel-defaults",
        action="store_true",
        help="Use named/default channel roles instead of generic randomized channel parameters.",
    )
    parser.add_argument("--no-resume", action="store_true", help="Re-run completed output folders instead of resuming.")
    args = parser.parse_args()

    try:
        import torch  # noqa: F401
    except Exception:
        print("Torch is not installed. Install with: pip install torch")
        return

    OUT.mkdir(parents=True, exist_ok=True)
    rows: list[dict[str, object]] = []
    run_id = 0
    for scenario in _scenarios(args.mode):
        for seed in _seeds(args.mode):
            for curve_type in _curve_types(args.mode):
                synth = make_synthetic_mmm_panel(
                    n_weeks=156,
                    n_geos=8,
                    channels=None if args.legacy_channel_defaults else _channels(4),
                    curve_type=curve_type,
                    scenario=scenario,
                    randomize_channel_parameters=not args.legacy_channel_defaults,
                    permute_channel_roles=not args.legacy_channel_defaults,
                    zero_inflated_media=scenario in {"messy_realistic", "weak_geo"},
                    volatile_media_measurement=scenario in {"messy_realistic", "hostile_collinear"},
                    missing_media_rate=0.02 if scenario == "messy_realistic" else 0.0,
                    media_block_missing_rate=0.08 if scenario == "messy_realistic" else 0.0,
                    seed=seed,
                )
                for feature_set_name, media_features in _feature_sets(args.mode).items():
                    for supervision_weight in _supervision_weights(args.mode):
                        run_id += 1
                        name = f"{run_id:03d}_{scenario}_{curve_type}_{seed}_{feature_set_name}_sup{supervision_weight:g}"
                        out_dir = OUT / name
                        out_dir.mkdir(parents=True, exist_ok=True)
                        summary_path = out_dir / "summary_recovery.csv"
                        if not args.no_resume and summary_path.exists():
                            print(f"Skipping completed {name}", flush=True)
                            rows.append(
                                _row_from_summary(
                                    summary_path,
                                    run_id,
                                    scenario,
                                    curve_type,
                                    seed,
                                    feature_set_name,
                                    media_features,
                                    supervision_weight,
                                )
                            )
                            pd.DataFrame(rows).to_csv(OUT / f"tft_grid_summary_{args.mode}.csv", index=False)
                            continue
                        print(f"Running {name}", flush=True)
                        controls = [c for c in ["promo", "holiday", "price_index"] if c in synth.panel.columns]
                        data_validation = validate_nmmm_training_data(
                            synth.panel,
                            truth_media=synth.truth_media,
                            truth_economics=synth.truth_economics,
                            controls=controls,
                            media_feature_inputs=media_features,
                        )
                        write_training_data_validation(data_validation, out_dir)
                        result = fit_tft_mmm(
                            synth.panel,
                            truth_media=synth.truth_media if supervision_weight > 0 else None,
                            media_feature_inputs=media_features,
                            controls=controls,
                            holdout_weeks=13,
                            validation_weeks=8,
                            epochs=args.epochs,
                            learning_rate=0.004,
                            contribution_supervision_weight=supervision_weight,
                            hidden_size=48,
                            n_heads=4,
                            dropout=0.05,
                            seed=seed,
                        )
                        prediction_metrics = evaluate_prediction_fit(result.predictions)
                        contribution_recovery = evaluate_contribution_recovery(result.predictions, synth.truth_media)
                        baseline_recovery = evaluate_baseline_recovery(result.predictions, synth.panel)
                        curve_recovery = evaluate_curve_recovery(result.response_curves, synth.response_curves)
                        parameter_recovery = evaluate_parameter_recovery(result.learned_params, synth.truth_params)
                        oracle_metrics = evaluate_oracle_fit(synth.panel, synth.truth_media, holdout_weeks=13)
                        summary = summarize_recovery(
                            contribution_recovery,
                            prediction_metrics,
                            curve_recovery,
                            parameter_recovery,
                            oracle_metrics,
                            baseline_recovery=baseline_recovery,
                            model_diagnostics=result.model_diagnostics,
                        )

                        result.variable_economics.to_csv(out_dir / "variable_economics.csv", index=False)
                        result.model_diagnostics.to_csv(out_dir / "model_diagnostics.csv", index=False)
                        result.learned_params.to_csv(out_dir / "learned_params.csv", index=False)
                        result.training_history.to_csv(out_dir / "training_history.csv", index=False)
                        result.response_curves.to_csv(out_dir / "response_curves.csv", index=False)
                        baseline_recovery.to_csv(out_dir / "baseline_recovery.csv", index=False)
                        summary.to_csv(out_dir / "summary_recovery.csv", index=False)
                        synth.panel.to_csv(out_dir / "training_panel.csv", index=False)
                        synth.truth_media.to_csv(out_dir / "training_truth_media.csv", index=False)
                        synth.truth_params.to_csv(out_dir / "training_truth_params.csv", index=False)
                        synth.response_curves.to_csv(out_dir / "training_truth_response_curves.csv", index=False)
                        if synth.truth_economics is not None:
                            synth.truth_economics.to_csv(out_dir / "training_truth_economics.csv", index=False)
                        if synth.label_audit is not None:
                            synth.label_audit.to_csv(out_dir / "training_label_audit.csv", index=False)
                        save_tft_mmm_checkpoint(
                            result,
                            out_dir / "model_checkpoint.pt",
                            extra={
                                "scenario": scenario,
                                "curve_type": curve_type,
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

                        row: dict[str, object] = {
                            "run_id": run_id,
                            "scenario": scenario,
                            "curve_type": curve_type,
                            "seed": seed,
                            "feature_set": feature_set_name,
                            "media_features": ",".join(media_features),
                            "contribution_supervision_weight": supervision_weight,
                        }
                        for _, metric_row in summary.iterrows():
                            row[str(metric_row["metric"])] = float(metric_row["value"])
                        rows.append(row)
                        pd.DataFrame(rows).to_csv(OUT / f"tft_grid_summary_{args.mode}.csv", index=False)

    summary_df = pd.DataFrame(rows)
    summary_df.to_csv(OUT / f"tft_grid_summary_{args.mode}.csv", index=False)
    if not summary_df.empty and "known_truth_recovery_score_0_100" in summary_df.columns:
        print(summary_df.sort_values("known_truth_recovery_score_0_100", ascending=False).head(12).to_string(index=False))
    print(f"Outputs: {OUT}")


if __name__ == "__main__":
    main()
