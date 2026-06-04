"""Compare NMMM hierarchy / market-size choices on one known-truth panel."""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd

from nmmm import evaluate_contribution_recovery, evaluate_prediction_fit, make_synthetic_mmm_panel
from nmmm.evaluation import evaluate_curve_recovery, evaluate_oracle_fit, evaluate_parameter_recovery, summarize_recovery
from nmmm.torch_training import fit_torch_nmmm, save_torch_nmmm_checkpoint


ROOT = Path(__file__).resolve().parent
OUT = ROOT / "outputs" / "torch_ablation"


def main() -> None:
    try:
        import torch  # noqa: F401
    except Exception:
        print("Torch is not installed. Install with: pip install torch")
        return

    OUT.mkdir(parents=True, exist_ok=True)
    synth = make_synthetic_mmm_panel(
        n_weeks=156,
        n_geos=8,
        curve_type="hill",
        scenario="standard",
        seed=20260603,
    )
    configs = [
        {
            "name": "pooled_no_market_scaling",
            "hierarchical_media": False,
            "market_size_col": None,
            "scale_media_by_market_size": False,
        },
        {
            "name": "hierarchical_no_market_scaling",
            "hierarchical_media": True,
            "group_media_max_log_multiplier": 0.25,
            "group_media_shrinkage": 0.05,
            "market_size_col": None,
            "scale_media_by_market_size": False,
        },
        {
            "name": "hierarchical_population_scaled",
            "hierarchical_media": True,
            "group_media_max_log_multiplier": 0.25,
            "group_media_shrinkage": 0.05,
            "market_size_col": "population",
            "scale_media_by_market_size": True,
        },
    ]
    summary_rows = []
    for cfg in configs:
        name = cfg["name"]
        out_dir = OUT / name
        out_dir.mkdir(parents=True, exist_ok=True)
        fit_kwargs = dict(cfg)
        fit_kwargs.pop("name")
        result = fit_torch_nmmm(
            synth.panel,
            truth_media=synth.truth_media,
            curve_type="hill",
            media_feature_inputs=["support", "spend"],
            holdout_weeks=13,
            epochs=800,
            learning_rate=0.01,
            weight_decay=1e-4,
            validation_weeks=8,
            early_stopping_patience=120,
            early_stopping_min_delta=1e-5,
            seed=20260603,
            initial_decay=0.35,
            initial_curve_param=2.0,
            initial_shape=1.0,
            initial_media_coef=0.12,
            initialize_from_baseline=True,
            contribution_supervision_weight=0.25,
            **fit_kwargs,
        )
        prediction_metrics = evaluate_prediction_fit(result.predictions)
        contribution_recovery = evaluate_contribution_recovery(result.predictions, synth.truth_media)
        curve_recovery = evaluate_curve_recovery(result.response_curves, synth.response_curves)
        parameter_recovery = evaluate_parameter_recovery(result.learned_params, synth.truth_params)
        oracle_metrics = evaluate_oracle_fit(synth.panel, synth.truth_media, holdout_weeks=13)
        recovery_summary = summarize_recovery(contribution_recovery, prediction_metrics, curve_recovery, parameter_recovery, oracle_metrics)
        result.predictions.to_csv(out_dir / "predictions.csv", index=False)
        result.learned_params.to_csv(out_dir / "learned_params.csv", index=False)
        result.response_curves.to_csv(out_dir / "response_curves.csv", index=False)
        result.training_history.to_csv(out_dir / "training_history.csv", index=False)
        contribution_recovery.to_csv(out_dir / "contribution_recovery.csv", index=False)
        prediction_metrics.to_csv(out_dir / "prediction_metrics.csv", index=False)
        curve_recovery.to_csv(out_dir / "curve_recovery.csv", index=False)
        parameter_recovery.to_csv(out_dir / "parameter_recovery.csv", index=False)
        oracle_metrics.to_csv(out_dir / "oracle_prediction_metrics.csv", index=False)
        synth.panel.to_csv(out_dir / "training_panel.csv", index=False)
        synth.truth_media.to_csv(out_dir / "training_truth_media.csv", index=False)
        save_torch_nmmm_checkpoint(
            result,
            out_dir / "model_checkpoint.pt",
            extra={"ablation": name, "summary": recovery_summary.to_dict(orient="records")},
        )
        with open(out_dir / "settings.json", "w", encoding="utf-8") as f:
            json.dump(result.settings, f, indent=2, sort_keys=True)
        row = {"model_config": name}
        for _, metric_row in recovery_summary.iterrows():
            row[str(metric_row["metric"])] = float(metric_row["value"])
        summary_rows.append(row)

    summary = pd.DataFrame(summary_rows)
    summary.to_csv(OUT / "torch_ablation_summary.csv", index=False)
    print("Torch NMMM ablation complete.")
    print(f"Outputs: {OUT}")
    print(summary.to_string(index=False))


if __name__ == "__main__":
    main()
