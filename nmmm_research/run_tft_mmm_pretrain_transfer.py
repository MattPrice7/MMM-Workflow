"""Test TFT synthetic pretraining transfer without target contribution labels."""

from __future__ import annotations

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
from nmmm.torch_tft_training import fit_tft_mmm, load_tft_mmm_checkpoint, save_tft_mmm_checkpoint


ROOT = Path(__file__).resolve().parent
OUT = ROOT / "outputs" / "tft_pretrain_transfer"
MEDIA_FEATURES = ["support", "spend", "impressions", "clicks", "grps", "reach", "frequency"]
GENERIC_CHANNELS = ["ch_01", "ch_02", "ch_03", "ch_04"]
PRETRAIN_PANELS = [
    {"scenario": "standard", "curve_type": "hill"},
    {"scenario": "messy_realistic", "curve_type": "weibull"},
    {"scenario": "hostile_collinear", "curve_type": "hill"},
    {"scenario": "weak_geo", "curve_type": "weibull"},
]
TARGET_CURVE_TYPE = "weibull"


def _score(result, synth, out_dir: Path, label: str) -> dict[str, object]:
    out_dir.mkdir(parents=True, exist_ok=True)
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
    summary.to_csv(out_dir / f"{label}_summary_recovery.csv", index=False)
    result.variable_economics.to_csv(out_dir / f"{label}_variable_economics.csv", index=False)
    result.model_diagnostics.to_csv(out_dir / f"{label}_model_diagnostics.csv", index=False)
    baseline_recovery.to_csv(out_dir / f"{label}_baseline_recovery.csv", index=False)
    result.training_history.to_csv(out_dir / f"{label}_training_history.csv", index=False)
    result.response_curves.to_csv(out_dir / f"{label}_response_curves.csv", index=False)
    result.learned_params.to_csv(out_dir / f"{label}_learned_params.csv", index=False)
    row = {"run": label}
    for _, metric_row in summary.iterrows():
        row[str(metric_row["metric"])] = float(metric_row["value"])
    return row


def main() -> None:
    try:
        import torch  # noqa: F401
    except Exception:
        print("Torch is not installed. Install with: pip install torch")
        return

    OUT.mkdir(parents=True, exist_ok=True)

    target_synth = make_synthetic_mmm_panel(
        n_weeks=156,
        n_geos=8,
        channels=GENERIC_CHANNELS,
        curve_type=TARGET_CURVE_TYPE,
        scenario="messy_realistic",
        randomize_channel_parameters=True,
        permute_channel_roles=True,
        zero_inflated_media=True,
        volatile_media_measurement=True,
        missing_media_rate=0.02,
        media_block_missing_rate=0.08,
        seed=20260604,
    )
    target_controls = [c for c in ["promo", "holiday", "price_index"] if c in target_synth.panel.columns]
    target_validation = validate_nmmm_training_data(
        target_synth.panel,
        truth_media=target_synth.truth_media,
        truth_economics=target_synth.truth_economics,
        controls=target_controls,
        media_feature_inputs=MEDIA_FEATURES,
    )
    write_training_data_validation(target_validation, OUT, prefix="target_training_data_validation_for_evaluation_only")

    pretrained = None
    for panel_i, panel_spec in enumerate(PRETRAIN_PANELS, start=1):
        scenario = str(panel_spec["scenario"])
        curve_type = str(panel_spec["curve_type"])
        seed = 20260600 + panel_i
        pretrain_dir = OUT / f"pretrain_panel_{panel_i:02d}_{scenario}_{curve_type}"
        pretrain_checkpoint = pretrain_dir / "model_checkpoint.pt"
        if pretrain_checkpoint.exists():
            print(f"Loading saved pretraining brain after panel {panel_i}: {pretrain_checkpoint}", flush=True)
            loaded = load_tft_mmm_checkpoint(pretrain_checkpoint)
            pretrained_state = loaded.get("model_state")
            pretrained = type(
                "LoadedPretrainState",
                (),
                {"model_state": pretrained_state},
            )()
            continue
        print(f"Pretraining supervised TFT brain on synthetic panel {panel_i}: {scenario}", flush=True)
        pretrain_synth = make_synthetic_mmm_panel(
            n_weeks=156,
            n_geos=8,
            channels=GENERIC_CHANNELS,
            curve_type=curve_type,
            scenario=scenario,
            randomize_channel_parameters=True,
            permute_channel_roles=True,
            zero_inflated_media=True,
            volatile_media_measurement=True,
            missing_media_rate=0.02,
            media_block_missing_rate=0.08,
            seed=seed,
        )
        pretrain_controls = [c for c in ["promo", "holiday", "price_index"] if c in pretrain_synth.panel.columns]
        pretrain_validation = validate_nmmm_training_data(
            pretrain_synth.panel,
            truth_media=pretrain_synth.truth_media,
            truth_economics=pretrain_synth.truth_economics,
            controls=pretrain_controls,
            media_feature_inputs=MEDIA_FEATURES,
        )
        write_training_data_validation(pretrain_validation, pretrain_dir)
        pretrained = fit_tft_mmm(
            pretrain_synth.panel,
            truth_media=pretrain_synth.truth_media,
            media_feature_inputs=MEDIA_FEATURES,
            controls=pretrain_controls,
            holdout_weeks=13,
            validation_weeks=8,
            epochs=220,
            learning_rate=0.003,
            contribution_supervision_weight=0.50,
            hidden_size=48,
            n_heads=4,
            dropout=0.05,
            seed=seed,
            initial_model_state=pretrained.model_state if pretrained is not None else None,
        )
        pretrain_dir.mkdir(parents=True, exist_ok=True)
        pretrain_synth.panel.to_csv(pretrain_dir / "panel.csv", index=False)
        pretrain_synth.truth_media.to_csv(pretrain_dir / "truth_media.csv", index=False)
        pretrain_synth.truth_params.to_csv(pretrain_dir / "truth_params.csv", index=False)
        pretrain_synth.response_curves.to_csv(pretrain_dir / "truth_response_curves.csv", index=False)
        if pretrain_synth.truth_economics is not None:
            pretrain_synth.truth_economics.to_csv(pretrain_dir / "truth_economics.csv", index=False)
        if pretrain_synth.label_audit is not None:
            pretrain_synth.label_audit.to_csv(pretrain_dir / "label_audit.csv", index=False)
        pretrained.model_diagnostics.to_csv(pretrain_dir / "model_diagnostics.csv", index=False)
        pretrained.variable_economics.to_csv(pretrain_dir / "variable_economics.csv", index=False)
        save_tft_mmm_checkpoint(
            pretrained,
            pretrain_checkpoint,
            extra={
                "pretrain_panel_index": panel_i,
                "scenario": scenario,
                "curve_type": curve_type,
                "seed": seed,
                "training_data": "panel.csv",
                "truth_media": "truth_media.csv",
            },
        )
    if pretrained is None:
        raise RuntimeError("No pretraining panels were run.")
    save_tft_mmm_checkpoint(
        pretrained,
        OUT / "pretrained_tft_model_checkpoint.pt",
        extra={
            "training_data": "pretrain_panel_*/panel.csv",
            "truth_media": "pretrain_panel_*/truth_media.csv",
            "curve_families": [p["curve_type"] for p in PRETRAIN_PANELS],
            "note": "Final pretrained brain after sequentially updating through every saved pretraining panel checkpoint.",
        },
    )

    print("Training target unsupervised from scratch", flush=True)
    target_scratch = fit_tft_mmm(
        target_synth.panel,
        truth_media=None,
        media_feature_inputs=MEDIA_FEATURES,
        controls=target_controls,
        holdout_weeks=13,
        validation_weeks=8,
        epochs=350,
        learning_rate=0.004,
        contribution_supervision_weight=0.0,
        hidden_size=48,
        n_heads=4,
        dropout=0.05,
        seed=20260604,
    )

    print("Fine-tuning target from pretrained brain without target contribution labels", flush=True)
    target_transfer = fit_tft_mmm(
        target_synth.panel,
        truth_media=None,
        media_feature_inputs=MEDIA_FEATURES,
        controls=target_controls,
        holdout_weeks=13,
        validation_weeks=8,
        epochs=250,
        learning_rate=0.0015,
        contribution_supervision_weight=0.0,
        hidden_size=48,
        n_heads=4,
        dropout=0.05,
        seed=20260604,
        initial_model_state=pretrained.model_state,
    )

    print("Training direct supervised target reference", flush=True)
    target_supervised = fit_tft_mmm(
        target_synth.panel,
        truth_media=target_synth.truth_media,
        media_feature_inputs=MEDIA_FEATURES,
        controls=target_controls,
        holdout_weeks=13,
        validation_weeks=8,
        epochs=350,
        learning_rate=0.004,
        contribution_supervision_weight=0.50,
        hidden_size=48,
        n_heads=4,
        dropout=0.05,
        seed=20260604,
    )

    rows = [
        _score(target_scratch, target_synth, OUT, "target_unsupervised_scratch"),
        _score(target_transfer, target_synth, OUT, "target_pretrained_unsupervised"),
        _score(target_supervised, target_synth, OUT, "target_direct_supervised_reference"),
    ]
    target_synth.panel.to_csv(OUT / "target_panel.csv", index=False)
    target_synth.truth_media.to_csv(OUT / "target_truth_media_for_evaluation_only.csv", index=False)
    target_synth.truth_params.to_csv(OUT / "target_truth_params_for_evaluation_only.csv", index=False)
    target_synth.response_curves.to_csv(OUT / "target_truth_response_curves_for_evaluation_only.csv", index=False)
    if target_synth.truth_economics is not None:
        target_synth.truth_economics.to_csv(OUT / "target_truth_economics_for_evaluation_only.csv", index=False)
    if target_synth.label_audit is not None:
        target_synth.label_audit.to_csv(OUT / "target_label_audit_for_evaluation_only.csv", index=False)
    save_tft_mmm_checkpoint(
        target_transfer,
        OUT / "target_pretrained_unsupervised_checkpoint.pt",
        extra={"training_data": "target_panel.csv", "truth_media": "not_used_for_training", "target_curve_type": TARGET_CURVE_TYPE},
    )
    with open(OUT / "transfer_settings.json", "w", encoding="utf-8") as f:
        json.dump(
            {
                "pretrain_scenario": "standard",
                "pretrain_scenarios": PRETRAIN_SCENARIOS,
                "target_scenario": "messy_realistic",
                "channels": GENERIC_CHANNELS,
                "media_features": MEDIA_FEATURES,
                "note": "Target truth labels are used only for evaluation. Pretraining uses randomized generic-channel synthetic panels.",
            },
            f,
            indent=2,
            sort_keys=True,
        )
    summary_df = pd.DataFrame(rows)
    summary_df.to_csv(OUT / "tft_pretrain_transfer_summary.csv", index=False)
    print(summary_df.to_string(index=False))
    print(f"Outputs: {OUT}")


if __name__ == "__main__":
    main()
