"""Harsh holdout validation for the neural curve-prior model.

This trains on mixed synthetic MMM panels, then scores deliberately harder
holdout panels with no target labels in training. The point is fallback
calibration and robustness, not making the headline metric look friendly.
"""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd

from nmmm import make_synthetic_mmm_panel
from nmmm.curve_prior_model import (
    build_curve_prior_dataset,
    evaluate_curve_prior_predictions,
    fit_curve_prior_model,
    predict_curve_prior_model,
    save_curve_prior_model,
)
from run_curve_prior_low_data_suite import (
    KPI_VARIANTS,
    LOW_DATA_REGIMES,
    _apply_low_data_regime,
    _available_media_inputs,
    _tag_low_data_panel,
)


ROOT = Path(__file__).resolve().parent
DEFAULT_OUT = ROOT / "outputs" / "curve_prior_harsh_holdout"


def _make_panels(
    *,
    holdout: bool,
    n_weeks: int,
    n_geos: int,
    n_channels: int,
    repeats: int,
    seed: int,
    scenario_filter: list[str] | None = None,
    kpi_variant_filter: list[str] | None = None,
    regime_filter: set[str] | None = None,
) -> list:
    channels = [f"ch_{i + 1:02d}" for i in range(int(n_channels))]
    scenarios = ["standard", "messy_realistic", "hostile_collinear", "weak_geo"] if not holdout else ["messy_realistic", "weak_geo"]
    if scenario_filter:
        scenarios = [s for s in scenarios if s in set(scenario_filter)]
    regimes = LOW_DATA_REGIMES if not holdout else [
        r
        for r in LOW_DATA_REGIMES
        if r["name"]
        in {
            "geo_support_spend_population",
            "geo_support_spend_no_population",
            "geo_spend_population",
            "geo_support_population",
            "geo_sales_national_media_population",
            "national_only_support_spend",
        }
    ]
    if regime_filter:
        regimes = [r for r in regimes if str(r["name"]) in regime_filter]
    kpi_items = [(k, v) for k, v in KPI_VARIANTS.items() if not kpi_variant_filter or k in set(kpi_variant_filter)]
    panels = []
    next_seed = int(seed)
    for scenario in scenarios:
        for kpi_variant, kpi_settings in kpi_items:
            for repeat in range(int(repeats)):
                split_group = f"{'holdout' if holdout else 'train'}_{scenario}_{kpi_variant}_{repeat:02d}"
                harsh_kwargs = {}
                if holdout:
                    harsh_kwargs = {
                        "national_media_share": 0.90 if scenario == "weak_geo" else 0.65,
                        "collinear_media_strength": 0.88 if scenario == "weak_geo" else 0.78,
                        "missing_media_rate": 0.08,
                        "media_block_missing_rate": 0.16,
                        "noise_sd": max(float(kpi_settings["noise_sd"]), 0.13),
                        "control_availability": "none" if repeat % 2 == 0 else "noisy_proxy",
                        "business_shocks": True,
                    }
                else:
                    harsh_kwargs = {
                        "missing_media_rate": 0.02 if scenario == "messy_realistic" else 0.0,
                        "media_block_missing_rate": 0.08 if scenario == "messy_realistic" else 0.0,
                        "control_availability": "noisy_proxy" if scenario == "messy_realistic" else "standard",
                    }
                base = make_synthetic_mmm_panel(
                    n_weeks=int(n_weeks),
                    n_geos=int(n_geos),
                    channels=channels,
                    curve_type="mixed",
                    scenario=scenario,
                    randomize_channel_parameters=True,
                    permute_channel_roles=True,
                    zero_inflated_media=True,
                    volatile_media_measurement=True,
                    seed=next_seed,
                    kpi_scale_multiplier=float(kpi_settings["kpi_scale_multiplier"]),
                    **harsh_kwargs,
                )
                next_seed += 1
                for regime in regimes:
                    panel = _apply_low_data_regime(base, regime)
                    panels.append(
                        _tag_low_data_panel(
                            panel,
                            split_group=split_group,
                            scenario=f"{'harsh_' if holdout else ''}{scenario}",
                            geo_regime=str(regime["geo"]),
                            measurement_regime=str(regime["name"]),
                            kpi_variant=str(kpi_variant),
                        )
                    )
    return panels


def _add_curve_error_columns(pred: pd.DataFrame) -> pd.DataFrame:
    out = pred.copy()
    curve_cols = [c for c in out.columns if c.startswith("curve_prior_p")]
    blend_cols = [c for c in out.columns if c.startswith("conservative_blend_curve_p")]
    true_cols = [c for c in out.columns if c.startswith("true_curve_p")]
    if curve_cols and true_cols:
        out["model_curve_mae"] = np.mean(np.abs(out[curve_cols].to_numpy(float) - out[true_cols].to_numpy(float)), axis=1)
    if blend_cols and true_cols:
        out["conservative_blend_curve_mae"] = np.mean(np.abs(out[blend_cols].to_numpy(float) - out[true_cols].to_numpy(float)), axis=1)
    return out


def _fallback_calibration(pred: pd.DataFrame) -> pd.DataFrame:
    scored = _add_curve_error_columns(pred)
    if "fallback_default_weight" not in scored.columns:
        return pd.DataFrame()
    scored = scored.copy()
    unique_n = scored["fallback_default_weight"].nunique(dropna=True)
    if unique_n < 2:
        scored["fallback_bucket"] = "all"
    else:
        scored["fallback_bucket"] = pd.qcut(
            scored["fallback_default_weight"],
            q=min(4, unique_n),
            duplicates="drop",
        ).astype(str)
    agg_cols = {
        "row_n": ("fallback_default_weight", "size"),
        "mean_predicted_fallback": ("fallback_default_weight", "mean"),
        "mean_true_fallback": ("true_fallback_default_weight", "mean"),
        "mean_diagnostic_fallback": ("diagnostic_default_weight_target", "mean"),
        "mean_truth_default_curve_mae": ("truth_default_curve_mae", "mean"),
        "mean_model_curve_mae": ("model_curve_mae", "mean"),
        "mean_conservative_blend_curve_mae": ("conservative_blend_curve_mae", "mean"),
    }
    return scored.groupby("fallback_bucket", dropna=False).agg(**agg_cols).reset_index()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUT)
    parser.add_argument("--epochs", type=int, default=180)
    parser.add_argument("--hidden-size", type=int, default=112)
    parser.add_argument("--dropout", type=float, default=0.10)
    parser.add_argument("--concavity-penalty-weight", type=float, default=0.25)
    parser.add_argument("--weeks", type=int, default=156)
    parser.add_argument("--geos", type=int, default=8)
    parser.add_argument("--channels", type=int, default=8)
    parser.add_argument("--train-repeats", type=int, default=2)
    parser.add_argument("--holdout-repeats", type=int, default=1)
    parser.add_argument("--seed", type=int, default=20260710)
    parser.add_argument("--quick", action="store_true")
    return parser.parse_args()


def main() -> None:
    try:
        import torch  # noqa: F401
    except Exception:
        print("Torch is not installed. Install with: pip install torch")
        return
    args = parse_args()
    if args.quick:
        args.epochs = min(args.epochs, 8)
        args.weeks = min(args.weeks, 72)
        args.geos = min(args.geos, 4)
        args.channels = min(args.channels, 4)
        args.train_repeats = 1
        args.holdout_repeats = 1
        train_scenarios = ["standard", "messy_realistic"]
        holdout_scenarios = ["weak_geo"]
        kpi_variants = ["sales_like"]
        regimes = {"geo_support_spend_population", "geo_sales_national_media_population", "national_only_support_spend"}
    else:
        train_scenarios = None
        holdout_scenarios = None
        kpi_variants = None
        regimes = None
    args.output_dir.mkdir(parents=True, exist_ok=True)
    train_panels = _make_panels(
        holdout=False,
        n_weeks=args.weeks,
        n_geos=args.geos,
        n_channels=args.channels,
        repeats=args.train_repeats,
        seed=args.seed,
        scenario_filter=train_scenarios,
        kpi_variant_filter=kpi_variants,
        regime_filter=regimes,
    )
    holdout_panels = _make_panels(
        holdout=True,
        n_weeks=args.weeks,
        n_geos=args.geos,
        n_channels=args.channels,
        repeats=args.holdout_repeats,
        seed=args.seed + 10000,
        scenario_filter=holdout_scenarios,
        kpi_variant_filter=kpi_variants,
        regime_filter=regimes,
    )
    media_inputs = _available_media_inputs(train_panels + holdout_panels)
    train_dataset = build_curve_prior_dataset(train_panels, media_feature_inputs=media_inputs)
    holdout_dataset = build_curve_prior_dataset(holdout_panels, media_feature_inputs=media_inputs)
    fitted = fit_curve_prior_model(
        train_dataset,
        epochs=args.epochs,
        hidden_size=args.hidden_size,
        dropout=args.dropout,
        concavity_penalty_weight=args.concavity_penalty_weight,
        seed=args.seed,
    )
    holdout = predict_curve_prior_model(fitted, holdout_dataset)
    train_metrics = evaluate_curve_prior_predictions(fitted)
    holdout_metrics = evaluate_curve_prior_predictions(holdout)
    fallback_calibration = _fallback_calibration(holdout.predictions)
    fitted.predictions.to_csv(args.output_dir / "train_curve_prior_predictions.csv", index=False)
    holdout.predictions.to_csv(args.output_dir / "harsh_holdout_curve_prior_predictions.csv", index=False)
    fitted.training_history.to_csv(args.output_dir / "train_curve_prior_training_history.csv", index=False)
    train_metrics.to_csv(args.output_dir / "train_curve_prior_metrics.csv", index=False)
    holdout_metrics.to_csv(args.output_dir / "harsh_holdout_curve_prior_metrics.csv", index=False)
    fallback_calibration.to_csv(args.output_dir / "harsh_holdout_fallback_calibration.csv", index=False)
    save_curve_prior_model(
        fitted,
        args.output_dir / "harsh_holdout_trained_curve_prior_checkpoint.pt",
        extra={
            "train_panel_count": len(train_panels),
            "holdout_panel_count": len(holdout_panels),
            "train_example_count": int(len(train_dataset.features)),
            "holdout_example_count": int(len(holdout_dataset.features)),
            "media_inputs": media_inputs,
            "concavity_penalty_weight": args.concavity_penalty_weight,
            "scope": "harsh holdout validation for neural curve-prior fallback calibration",
        },
    )
    print("Harsh holdout curve-prior validation complete.")
    print(f"Outputs: {args.output_dir}")
    print("Holdout metrics:")
    print(holdout_metrics.to_string(index=False))
    print("Fallback calibration:")
    print(fallback_calibration.to_string(index=False))


if __name__ == "__main__":
    main()
