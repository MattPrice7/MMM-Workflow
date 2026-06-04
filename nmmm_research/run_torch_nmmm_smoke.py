"""Run the optional Torch NMMM smoke test.

This script exits gracefully if Torch is not installed.
"""

from __future__ import annotations

import json
from pathlib import Path

from nmmm import evaluate_contribution_recovery, evaluate_prediction_fit, make_synthetic_mmm_panel
from nmmm.evaluation import evaluate_curve_recovery, evaluate_oracle_fit, evaluate_parameter_recovery, summarize_recovery
from nmmm.torch_training import fit_torch_nmmm, save_torch_nmmm_checkpoint


ROOT = Path(__file__).resolve().parent
OUT = ROOT / "outputs" / "torch_smoke_test"


def main() -> None:
    try:
        import torch  # noqa: F401
    except Exception:
        print("Torch is not installed. Install with: pip install torch")
        print("The transformed ridge smoke test still runs without Torch.")
        return

    OUT.mkdir(parents=True, exist_ok=True)
    synth = make_synthetic_mmm_panel(
        n_weeks=156,
        n_geos=8,
        curve_type="hill",
        national_media_share=0.20,
        noise_sd=0.07,
        seed=20260603,
    )
    result = fit_torch_nmmm(
        synth.panel,
        truth_media=synth.truth_media,
        curve_type="hill",
        media_feature_inputs=["support", "spend"],
        holdout_weeks=13,
        epochs=1200,
        learning_rate=0.01,
        weight_decay=1e-4,
        validation_weeks=8,
        early_stopping_patience=150,
        early_stopping_min_delta=1e-5,
        seed=20260603,
        initial_decay=0.35,
        initial_curve_param=2.0,
        initial_shape=1.0,
        initial_media_coef=0.12,
        initialize_from_baseline=True,
        contribution_supervision_weight=0.25,
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
    summary = summarize_recovery(contribution_recovery, prediction_metrics, curve_recovery, parameter_recovery, oracle_metrics)

    result.predictions.to_csv(OUT / "torch_model_predictions.csv", index=False)
    result.long_decomp.to_csv(OUT / "torch_model_long_decomp.csv", index=False)
    result.learned_params.to_csv(OUT / "torch_learned_params.csv", index=False)
    result.response_curves.to_csv(OUT / "torch_estimated_response_curves.csv", index=False)
    result.training_history.to_csv(OUT / "torch_training_history.csv", index=False)
    prediction_metrics.to_csv(OUT / "torch_prediction_metrics.csv", index=False)
    contribution_recovery.to_csv(OUT / "torch_contribution_recovery.csv", index=False)
    curve_recovery.to_csv(OUT / "torch_curve_recovery.csv", index=False)
    parameter_recovery.to_csv(OUT / "torch_parameter_recovery.csv", index=False)
    oracle_metrics.to_csv(OUT / "torch_oracle_prediction_metrics.csv", index=False)
    summary.to_csv(OUT / "torch_summary_recovery.csv", index=False)
    synth.panel.to_csv(OUT / "training_panel.csv", index=False)
    synth.truth_media.to_csv(OUT / "training_truth_media.csv", index=False)
    synth.truth_params.to_csv(OUT / "training_truth_params.csv", index=False)
    synth.response_curves.to_csv(OUT / "training_truth_response_curves.csv", index=False)
    save_torch_nmmm_checkpoint(
        result,
        OUT / "torch_model_checkpoint.pt",
        extra={
            "training_data": "training_panel.csv",
            "truth_media": "training_truth_media.csv",
            "summary": summary.to_dict(orient="records"),
        },
    )
    with open(OUT / "torch_settings.json", "w", encoding="utf-8") as f:
        json.dump(result.settings, f, indent=2, sort_keys=True)

    print("Torch NMMM smoke test complete.")
    print(f"Outputs: {OUT}")
    print(summary.to_string(index=False))
    print(prediction_metrics.to_string(index=False))


if __name__ == "__main__":
    main()
