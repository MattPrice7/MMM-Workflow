"""Run a smoke test for the hierarchical TFT MMM challenger."""

from __future__ import annotations

import json
from pathlib import Path

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
OUT = ROOT / "outputs" / "tft_smoke_test"


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
    media_features = ["support", "spend", "impressions", "clicks", "grps", "reach", "frequency"]
    controls = ["promo", "holiday", "price_index"]
    data_validation = validate_nmmm_training_data(
        synth.panel,
        truth_media=synth.truth_media,
        truth_economics=synth.truth_economics,
        controls=controls,
        media_feature_inputs=media_features,
    )
    write_training_data_validation(data_validation, OUT)
    result = fit_tft_mmm(
        synth.panel,
        truth_media=synth.truth_media,
        media_feature_inputs=media_features,
        controls=controls,
        holdout_weeks=13,
        validation_weeks=8,
        epochs=500,
        learning_rate=0.004,
        contribution_supervision_weight=0.50,
        hidden_size=48,
        n_heads=4,
        dropout=0.05,
        seed=20260603,
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

    result.predictions.to_csv(OUT / "tft_predictions.csv", index=False)
    result.long_decomp.to_csv(OUT / "tft_long_decomp.csv", index=False)
    result.variable_economics.to_csv(OUT / "tft_variable_economics.csv", index=False)
    result.model_diagnostics.to_csv(OUT / "tft_model_diagnostics.csv", index=False)
    result.learned_params.to_csv(OUT / "tft_learned_params.csv", index=False)
    result.response_curves.to_csv(OUT / "tft_response_curves.csv", index=False)
    result.training_history.to_csv(OUT / "tft_training_history.csv", index=False)
    prediction_metrics.to_csv(OUT / "tft_prediction_metrics.csv", index=False)
    contribution_recovery.to_csv(OUT / "tft_contribution_recovery.csv", index=False)
    baseline_recovery.to_csv(OUT / "tft_baseline_recovery.csv", index=False)
    curve_recovery.to_csv(OUT / "tft_curve_recovery.csv", index=False)
    parameter_recovery.to_csv(OUT / "tft_parameter_recovery.csv", index=False)
    oracle_metrics.to_csv(OUT / "tft_oracle_prediction_metrics.csv", index=False)
    summary.to_csv(OUT / "tft_summary_recovery.csv", index=False)
    synth.panel.to_csv(OUT / "training_panel.csv", index=False)
    synth.truth_media.to_csv(OUT / "training_truth_media.csv", index=False)
    if synth.truth_economics is not None:
        synth.truth_economics.to_csv(OUT / "training_truth_economics.csv", index=False)
    if synth.label_audit is not None:
        synth.label_audit.to_csv(OUT / "training_label_audit.csv", index=False)
    save_tft_mmm_checkpoint(
        result,
        OUT / "tft_model_checkpoint.pt",
        extra={"summary": summary.to_dict(orient="records"), "training_data": "training_panel.csv"},
    )
    with open(OUT / "tft_settings.json", "w", encoding="utf-8") as f:
        json.dump(result.settings, f, indent=2, sort_keys=True)

    print("TFT MMM smoke test complete.")
    print(f"Outputs: {OUT}")
    print(summary.to_string(index=False))
    print(prediction_metrics.to_string(index=False))


if __name__ == "__main__":
    main()
