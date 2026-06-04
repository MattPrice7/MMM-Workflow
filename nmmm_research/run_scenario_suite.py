"""Run transparent MMM baselines across tougher synthetic scenarios."""

from __future__ import annotations

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
OUT = ROOT / "outputs" / "scenario_suite"


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    scenarios = ["standard", "messy_realistic", "hostile_collinear", "weak_geo"]
    summary_rows = []
    for i, scenario in enumerate(scenarios):
        synth = make_synthetic_mmm_panel(
            n_weeks=156,
            n_geos=8,
            curve_type="hill",
            scenario=scenario,
            seed=20260603 + i,
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
        recovery_summary = summarize_recovery(contribution_recovery, prediction_metrics, curve_recovery, parameter_recovery, oracle_metrics)
        scenario_dir = OUT / scenario
        scenario_dir.mkdir(parents=True, exist_ok=True)
        synth.panel.to_csv(scenario_dir / "synthetic_panel.csv", index=False)
        synth.truth_media.to_csv(scenario_dir / "truth_media.csv", index=False)
        synth.truth_params.to_csv(scenario_dir / "truth_params.csv", index=False)
        result.predictions.to_csv(scenario_dir / "model_predictions.csv", index=False)
        result.selected_transforms.to_csv(scenario_dir / "selected_transforms.csv", index=False)
        result.response_curves.to_csv(scenario_dir / "estimated_response_curves.csv", index=False)
        prediction_metrics.to_csv(scenario_dir / "prediction_metrics.csv", index=False)
        contribution_recovery.to_csv(scenario_dir / "contribution_recovery.csv", index=False)
        curve_recovery.to_csv(scenario_dir / "curve_recovery.csv", index=False)
        parameter_recovery.to_csv(scenario_dir / "parameter_recovery.csv", index=False)
        oracle_metrics.to_csv(scenario_dir / "oracle_prediction_metrics.csv", index=False)
        recovery_summary.to_csv(scenario_dir / "summary_recovery.csv", index=False)

        row = {"scenario": scenario}
        for _, metric_row in recovery_summary.iterrows():
            row[str(metric_row["metric"])] = float(metric_row["value"])
        summary_rows.append(row)

    summary = pd.DataFrame(summary_rows)
    summary.to_csv(OUT / "scenario_suite_summary.csv", index=False)
    print("NMMM scenario suite complete.")
    print(f"Outputs: {OUT}")
    print(summary.to_string(index=False))


if __name__ == "__main__":
    main()
