"""Stress-test hardened TFT MMM across different data availability levels."""

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
OUT = ROOT / "outputs" / "tft_data_level_suite"


def _channels(n_channels: int) -> list[str]:
    return [f"ch_{i + 1:02d}" for i in range(int(n_channels))]


def _feature_sets() -> dict[str, list[str]]:
    return {
        "support_only": ["support"],
        "support_spend": ["support", "spend"],
        "rich_media": ["support", "spend", "impressions", "clicks", "grps", "reach", "frequency"],
    }


def _configs(mode: str) -> list[dict[str, object]]:
    base = [
        {
            "case": "short_one_geo_national_media",
            "n_weeks": 52,
            "n_geos": 1,
            "n_channels": 3,
            "scenario": "weak_geo",
            "national_media_share": 1.0,
            "controls_mode": "none",
            "control_availability": "none",
            "feature_set": "support_spend",
            "curve_type": "hill",
        },
        {
            "case": "two_year_four_geo_missing_media",
            "n_weeks": 104,
            "n_geos": 4,
            "n_channels": 4,
            "scenario": "messy_realistic",
            "missing_media_rate": 0.03,
            "media_block_missing_rate": 0.12,
            "controls_mode": "standard",
            "control_availability": "standard",
            "feature_set": "rich_media",
            "curve_type": "weibull",
        },
        {
            "case": "three_year_eight_geo_hostile_collinear",
            "n_weeks": 156,
            "n_geos": 8,
            "n_channels": 6,
            "scenario": "hostile_collinear",
            "collinear_media_strength": 0.85,
            "controls_mode": "standard",
            "control_availability": "standard",
            "feature_set": "support_spend",
            "curve_type": "hill",
        },
    ]
    if mode == "full":
        base.extend(
            [
                {
                    "case": "four_year_twenty_geo_many_channels",
                    "n_weeks": 208,
                    "n_geos": 20,
                    "n_channels": 12,
                    "scenario": "messy_realistic",
                    "missing_media_rate": 0.02,
                    "media_block_missing_rate": 0.10,
                    "controls_mode": "rich",
                    "control_availability": "rich",
                    "feature_set": "rich_media",
                    "curve_type": "weibull",
                },
                {
                    "case": "weak_geo_rich_media_no_controls",
                    "n_weeks": 156,
                    "n_geos": 8,
                    "n_channels": 5,
                    "scenario": "weak_geo",
                    "national_media_share": 0.90,
                    "controls_mode": "none",
                    "control_availability": "none",
                    "feature_set": "rich_media",
                    "curve_type": "hill",
                },
                {
                    "case": "support_only_zero_flighting",
                    "n_weeks": 104,
                    "n_geos": 8,
                    "n_channels": 8,
                    "scenario": "standard",
                    "zero_inflated_media": True,
                    "controls_mode": "partial",
                    "control_availability": "partial",
                    "feature_set": "support_only",
                    "curve_type": "weibull",
                },
                {
                    "case": "noisy_proxy_controls",
                    "n_weeks": 156,
                    "n_geos": 8,
                    "n_channels": 6,
                    "scenario": "messy_realistic",
                    "missing_media_rate": 0.02,
                    "media_block_missing_rate": 0.08,
                    "controls_mode": "rich",
                    "control_availability": "noisy_proxy",
                    "feature_set": "rich_media",
                    "curve_type": "hill",
                },
            ]
        )
    return base


def _controls_for(mode: str) -> list[str]:
    if mode == "none":
        return []
    if mode == "partial":
        return ["promo", "holiday"]
    if mode in {"rich", "noisy_proxy"}:
        return ["promo", "holiday", "price_index", "competitor_index", "macro_index", "category_trend"]
    return ["promo", "holiday", "price_index"]


def _row_from_summary(
    summary_path: Path,
    run_id: int,
    cfg: dict[str, object],
    supervision_weight: float,
) -> dict[str, object]:
    summary = pd.read_csv(summary_path)
    row: dict[str, object] = {
        "run_id": run_id,
        "case": cfg["case"],
        "n_weeks": cfg["n_weeks"],
        "n_geos": cfg["n_geos"],
        "n_channels": cfg["n_channels"],
        "curve_type": cfg.get("curve_type", "hill"),
        "feature_set": cfg["feature_set"],
        "controls_mode": cfg.get("controls_mode", "standard"),
        "contribution_supervision_weight": supervision_weight,
    }
    for _, metric_row in summary.iterrows():
        row[str(metric_row["metric"])] = float(metric_row["value"])
    return row


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", choices=["quick", "full"], default="quick")
    parser.add_argument("--epochs", type=int, default=180)
    parser.add_argument("--supervision", choices=["none", "supervised", "both"], default="both")
    parser.add_argument("--no-resume", action="store_true", help="Re-run completed output folders instead of resuming.")
    args = parser.parse_args()

    try:
        import torch  # noqa: F401
    except Exception:
        print("Torch is not installed. Install with: pip install torch")
        return

    OUT.mkdir(parents=True, exist_ok=True)
    rows: list[dict[str, object]] = []
    supervision_weights = [0.0, 0.50] if args.supervision == "both" else ([0.50] if args.supervision == "supervised" else [0.0])
    run_id = 0
    features = _feature_sets()
    for cfg_i, cfg in enumerate(_configs(args.mode), start=1):
        synth = make_synthetic_mmm_panel(
            n_weeks=int(cfg["n_weeks"]),
            n_geos=int(cfg["n_geos"]),
            channels=_channels(int(cfg["n_channels"])),
            curve_type=str(cfg.get("curve_type", "hill")),
            scenario=str(cfg["scenario"]),
            national_media_share=float(cfg.get("national_media_share", 0.25)),
            collinear_media_strength=float(cfg.get("collinear_media_strength", 0.0)),
            missing_media_rate=float(cfg.get("missing_media_rate", 0.0)),
            media_block_missing_rate=float(cfg.get("media_block_missing_rate", 0.0)),
            zero_inflated_media=bool(cfg.get("zero_inflated_media", True)),
            randomize_channel_parameters=True,
            permute_channel_roles=True,
            volatile_media_measurement=True,
            business_shocks=True,
            control_availability=str(cfg.get("control_availability", cfg.get("controls_mode", "standard"))),
            seed=20260610 + cfg_i,
        )
        media_features = features[str(cfg["feature_set"])]
        controls = _controls_for(str(cfg.get("controls_mode", "standard")))
        holdout_weeks = min(13, max(4, int(cfg["n_weeks"]) // 6))
        validation_weeks = min(8, max(2, int(cfg["n_weeks"]) // 10))
        for supervision_weight in supervision_weights:
            run_id += 1
            run_name = f"{run_id:03d}_{cfg['case']}_sup{supervision_weight:g}"
            out_dir = OUT / run_name
            out_dir.mkdir(parents=True, exist_ok=True)
            summary_path = out_dir / "summary_recovery.csv"
            if not args.no_resume and summary_path.exists():
                print(f"Skipping completed {run_name}", flush=True)
                rows.append(_row_from_summary(summary_path, run_id, cfg, supervision_weight))
                pd.DataFrame(rows).to_csv(OUT / f"tft_data_level_summary_{args.mode}.csv", index=False)
                continue
            print(f"Running {run_name}", flush=True)
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
                holdout_weeks=holdout_weeks,
                validation_weeks=validation_weeks,
                epochs=args.epochs,
                learning_rate=0.004,
                contribution_supervision_weight=supervision_weight,
                hidden_size=48,
                n_heads=4,
                dropout=0.05,
                seed=20260610 + cfg_i,
            )
            prediction_metrics = evaluate_prediction_fit(result.predictions)
            contribution_recovery = evaluate_contribution_recovery(result.predictions, synth.truth_media)
            baseline_recovery = evaluate_baseline_recovery(result.predictions, synth.panel)
            curve_recovery = evaluate_curve_recovery(result.response_curves, synth.response_curves)
            parameter_recovery = evaluate_parameter_recovery(result.learned_params, synth.truth_params)
            oracle_metrics = evaluate_oracle_fit(synth.panel, synth.truth_media, holdout_weeks=holdout_weeks)
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
            result.training_history.to_csv(out_dir / "training_history.csv", index=False)
            result.response_curves.to_csv(out_dir / "response_curves.csv", index=False)
            baseline_recovery.to_csv(out_dir / "baseline_recovery.csv", index=False)
            contribution_recovery.to_csv(out_dir / "contribution_recovery.csv", index=False)
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
                    "case": cfg["case"],
                    "config": cfg,
                    "media_features": media_features,
                    "controls": controls,
                    "contribution_supervision_weight": supervision_weight,
                    "summary": summary.to_dict(orient="records"),
                },
            )
            with open(out_dir / "settings.json", "w", encoding="utf-8") as f:
                json.dump({"config": cfg, "model_settings": result.settings}, f, indent=2, sort_keys=True)
            row: dict[str, object] = {
                "run_id": run_id,
                "case": cfg["case"],
                "n_weeks": cfg["n_weeks"],
                "n_geos": cfg["n_geos"],
                "n_channels": cfg["n_channels"],
                "feature_set": cfg["feature_set"],
                "curve_type": cfg.get("curve_type", "hill"),
                "controls_mode": cfg.get("controls_mode", "standard"),
                "contribution_supervision_weight": supervision_weight,
            }
            for _, metric_row in summary.iterrows():
                row[str(metric_row["metric"])] = float(metric_row["value"])
            rows.append(row)
            pd.DataFrame(rows).to_csv(OUT / f"tft_data_level_summary_{args.mode}.csv", index=False)

    summary_df = pd.DataFrame(rows)
    summary_df.to_csv(OUT / f"tft_data_level_summary_{args.mode}.csv", index=False)
    if "known_truth_recovery_score_0_100" in summary_df.columns:
        print(summary_df.sort_values("known_truth_recovery_score_0_100", ascending=False).to_string(index=False))
    print(f"Outputs: {OUT}")


if __name__ == "__main__":
    main()
