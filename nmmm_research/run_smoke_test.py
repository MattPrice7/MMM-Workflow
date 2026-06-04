"""Run a lightweight known-truth NMMM smoke test."""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd

from nmmm import (
    evaluate_contribution_recovery,
    evaluate_prediction_fit,
    fit_transformed_ridge_mmm,
    make_synthetic_mmm_panel,
)
from nmmm.evaluation import evaluate_curve_recovery, evaluate_oracle_fit, evaluate_parameter_recovery, summarize_recovery


ROOT = Path(__file__).resolve().parent
OUT = ROOT / "outputs" / "smoke_test"


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)

    synth = make_synthetic_mmm_panel(
        n_weeks=156,
        n_geos=8,
        curve_type="hill",
        national_media_share=0.20,
        noise_sd=0.07,
        seed=20260603,
    )
    result = fit_transformed_ridge_mmm(
        synth.panel,
        curve_type="hill",
        holdout_weeks=13,
        ridge_lambda=25.0,
        positive_media=True,
    )

    prediction_metrics = evaluate_prediction_fit(result.predictions)
    contribution_recovery = evaluate_contribution_recovery(result.predictions, synth.truth_media)
    curve_recovery = evaluate_curve_recovery(result.response_curves, synth.response_curves)
    parameter_recovery = evaluate_parameter_recovery(result.selected_transforms, synth.truth_params)
    oracle_metrics = evaluate_oracle_fit(synth.panel, synth.truth_media, holdout_weeks=13)
    summary = summarize_recovery(contribution_recovery, prediction_metrics, curve_recovery, parameter_recovery, oracle_metrics)

    synth.panel.to_csv(OUT / "synthetic_panel.csv", index=False)
    synth.truth_media.to_csv(OUT / "truth_media.csv", index=False)
    synth.truth_params.to_csv(OUT / "truth_params.csv", index=False)
    synth.response_curves.to_csv(OUT / "truth_response_curves.csv", index=False)
    result.predictions.to_csv(OUT / "model_predictions.csv", index=False)
    result.long_decomp.to_csv(OUT / "model_long_decomp.csv", index=False)
    result.selected_transforms.to_csv(OUT / "selected_transforms.csv", index=False)
    result.response_curves.to_csv(OUT / "estimated_response_curves.csv", index=False)
    result.coefficients.to_csv(OUT / "coefficients.csv", index=False)
    prediction_metrics.to_csv(OUT / "prediction_metrics.csv", index=False)
    contribution_recovery.to_csv(OUT / "contribution_recovery.csv", index=False)
    curve_recovery.to_csv(OUT / "curve_recovery.csv", index=False)
    parameter_recovery.to_csv(OUT / "parameter_recovery.csv", index=False)
    oracle_metrics.to_csv(OUT / "oracle_prediction_metrics.csv", index=False)
    summary.to_csv(OUT / "summary_recovery.csv", index=False)
    with open(OUT / "settings.json", "w", encoding="utf-8") as f:
        json.dump(result.settings, f, indent=2, sort_keys=True)

    print("NMMM smoke test complete.")
    print(f"Outputs: {OUT}")
    print(summary.to_string(index=False))
    print(prediction_metrics.to_string(index=False))


if __name__ == "__main__":
    main()
