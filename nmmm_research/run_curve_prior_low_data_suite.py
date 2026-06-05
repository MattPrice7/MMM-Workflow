"""Low-data stress suite for the neural response-curve prior model.

This script answers the agency-practical question: how much can the curve-prior
model recover when the available data are limited to common MMM ingredients such
as geo KPI, support/spend, and optional population?

It trains on known-truth synthetic panels and evaluates curve/adstock/fallback
recovery by measurement regime. The model output is still a prior builder, not a
claim that saturation is identified from weak observational data alone.
"""

from __future__ import annotations

import argparse
from dataclasses import replace
from pathlib import Path
from typing import Dict, Iterable

import pandas as pd

from nmmm import make_synthetic_mmm_panel
from nmmm.curve_prior_model import (
    build_curve_prior_dataset,
    evaluate_curve_prior_predictions,
    fit_curve_prior_model,
    save_curve_prior_model,
)
from run_curve_prior_model_smoke import (
    _apply_geo_regime,
    _apply_measurement_regime,
    _channel_cols,
    _tag_panel,
)


ROOT = Path(__file__).resolve().parent
DEFAULT_OUT = ROOT / "outputs" / "curve_prior_low_data_suite"

LOW_DATA_REGIMES = [
    {"name": "geo_support_spend_population", "geo": "full_geo", "measurement": "support_spend", "population": True},
    {"name": "geo_support_spend_no_population", "geo": "full_geo", "measurement": "support_spend", "population": False},
    {"name": "geo_spend_population", "geo": "full_geo", "measurement": "spend_only", "population": True},
    {"name": "geo_support_population", "geo": "full_geo", "measurement": "support_only", "population": True},
    {
        "name": "geo_sales_national_media_population",
        "geo": "geo_sales_national_media",
        "measurement": "support_spend",
        "population": True,
    },
    {"name": "national_only_support_spend", "geo": "national_only", "measurement": "support_spend", "population": False},
    {"name": "geo_rich_media_population", "geo": "full_geo", "measurement": "rich_media", "population": True},
]

GEO_MEDIA_PATTERNS = [
    "mixed",
    "all_channels_similar_geo_trends",
    "some_channels_similar_geo_trends",
    "some_channels_differentiated",
    "one_channel_geo_differentiated",
    "embedded_geo_lift_test",
    "population_distributed_national",
]

HARSHNESS_PROFILES = {
    "minimum_clean": {
        "control_availability": "none",
        "missing_media_rate": 0.0,
        "media_block_missing_rate": 0.0,
        "volatile_media_measurement": False,
        "business_shocks": False,
        "zero_inflated_media": False,
        "evolving_media_costs": True,
        "noise_floor": 0.06,
    },
    "minimum_messy": {
        "control_availability": "none",
        "missing_media_rate": 0.04,
        "media_block_missing_rate": 0.10,
        "volatile_media_measurement": True,
        "business_shocks": True,
        "zero_inflated_media": True,
        "evolving_media_costs": True,
        "noise_floor": 0.10,
    },
    "public_macro_proxy": {
        "control_availability": "public_macro",
        "missing_media_rate": 0.025,
        "media_block_missing_rate": 0.06,
        "volatile_media_measurement": True,
        "business_shocks": True,
        "zero_inflated_media": True,
        "evolving_media_costs": True,
        "noise_floor": 0.085,
    },
    "rich_messy": {
        "control_availability": "noisy_proxy",
        "missing_media_rate": 0.02,
        "media_block_missing_rate": 0.08,
        "volatile_media_measurement": True,
        "business_shocks": True,
        "zero_inflated_media": True,
        "evolving_media_costs": True,
        "noise_floor": 0.075,
    },
}

KPI_VARIANTS = {
    "sales_like": {"kpi_scale_multiplier": 1.0, "noise_sd": 0.06},
    "subscriptions_like": {"kpi_scale_multiplier": 0.04, "noise_sd": 0.09},
    "leads_like": {"kpi_scale_multiplier": 0.10, "noise_sd": 0.08},
}


def _drop_population_if_needed(synth, keep_population: bool):
    if keep_population:
        return synth
    panel = synth.panel.drop(columns=["population"], errors="ignore")
    return replace(synth, panel=panel)


def _tag_low_data_panel(
    synth,
    *,
    split_group: str,
    scenario: str,
    geo_regime: str,
    measurement_regime: str,
    kpi_variant: str,
    geo_media_pattern: str = "unknown",
    harshness_profile: str = "unknown",
):
    synth = _tag_panel(
        synth,
        split_group=split_group,
        scenario=scenario,
        geo_regime=geo_regime,
        measurement_regime=measurement_regime,
    )
    setattr(synth, "curve_prior_kpi_variant", str(kpi_variant))
    setattr(synth, "curve_prior_geo_media_pattern", str(geo_media_pattern))
    setattr(synth, "curve_prior_harshness_profile", str(harshness_profile))
    return synth


def _apply_low_data_regime(synth, regime: Dict[str, object]):
    out = _apply_geo_regime(synth, str(regime["geo"]))
    out = _apply_measurement_regime(out, str(regime["measurement"]))
    out = _drop_population_if_needed(out, bool(regime["population"]))
    return out


def _make_low_data_panels(
    *,
    n_weeks: int,
    n_geos: int,
    n_channels: int,
    repeats: int,
    seed: int,
    scenarios: Iterable[str],
    kpi_variants: Iterable[str],
    geo_media_patterns: Iterable[str],
    harshness_profiles: Iterable[str],
    regimes: Iterable[Dict[str, object]] = LOW_DATA_REGIMES,
) -> list:
    panels = []
    channels = [f"ch_{i + 1:02d}" for i in range(int(n_channels))]
    next_seed = int(seed)
    for scenario in scenarios:
        for kpi_variant in kpi_variants:
            kpi_settings = KPI_VARIANTS[str(kpi_variant)]
            for harshness_profile in harshness_profiles:
                profile = HARSHNESS_PROFILES[str(harshness_profile)]
                for geo_media_pattern in geo_media_patterns:
                    for repeat in range(int(repeats)):
                        split_group = f"{scenario}_{kpi_variant}_{harshness_profile}_{geo_media_pattern}_{repeat:02d}"
                        base = make_synthetic_mmm_panel(
                            n_weeks=int(n_weeks),
                            n_geos=int(n_geos),
                            channels=channels,
                            curve_type="mixed",
                            scenario=str(scenario),
                            randomize_channel_parameters=True,
                            permute_channel_roles=True,
                            zero_inflated_media=bool(profile["zero_inflated_media"]),
                            volatile_media_measurement=bool(profile["volatile_media_measurement"]),
                            evolving_media_costs=bool(profile["evolving_media_costs"]),
                            geo_media_pattern=str(geo_media_pattern),
                            missing_media_rate=float(profile["missing_media_rate"]),
                            media_block_missing_rate=float(profile["media_block_missing_rate"]),
                            control_availability=str(profile["control_availability"]),
                            business_shocks=bool(profile["business_shocks"]),
                            noise_sd=max(float(kpi_settings["noise_sd"]), float(profile["noise_floor"])),
                            seed=next_seed,
                            kpi_scale_multiplier=float(kpi_settings["kpi_scale_multiplier"]),
                        )
                        next_seed += 1
                        for regime in regimes:
                            panel = _apply_low_data_regime(base, regime)
                            panels.append(
                                _tag_low_data_panel(
                                    panel,
                                    split_group=split_group,
                                    scenario=str(scenario),
                                    geo_regime=str(regime["geo"]),
                                    measurement_regime=str(regime["name"]),
                                    kpi_variant=str(kpi_variant),
                                    geo_media_pattern=str(geo_media_pattern),
                                    harshness_profile=str(harshness_profile),
                                )
                            )
    return panels


def _available_media_inputs(panels: list) -> list[str]:
    suffix_to_feature = {
        "_support": "support",
        "_spend": "spend",
        "_impressions": "impressions",
        "_clicks": "clicks",
        "_grps": "grps",
        "_reach": "reach",
        "_frequency": "frequency",
    }
    cols = set()
    for synth in panels:
        cols.update(_channel_cols(synth.panel, suffixes=suffix_to_feature.keys()))
    return [feature for suffix, feature in suffix_to_feature.items() if any(c.endswith(suffix) for c in cols)]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUT)
    parser.add_argument("--epochs", type=int, default=220)
    parser.add_argument("--hidden-size", type=int, default=96)
    parser.add_argument("--dropout", type=float, default=0.08)
    parser.add_argument("--concavity-penalty-weight", type=float, default=0.25)
    parser.add_argument("--weeks", type=int, default=156)
    parser.add_argument("--geos", type=int, default=8)
    parser.add_argument("--channels", type=int, default=8)
    parser.add_argument("--repeats", type=int, default=2)
    parser.add_argument("--seed", type=int, default=20260690)
    parser.add_argument(
        "--scenarios",
        nargs="+",
        default=["standard", "messy_realistic", "hostile_collinear", "weak_geo"],
    )
    parser.add_argument("--kpi-variants", nargs="+", default=list(KPI_VARIANTS.keys()))
    parser.add_argument("--geo-media-patterns", nargs="+", default=GEO_MEDIA_PATTERNS)
    parser.add_argument("--harshness-profiles", nargs="+", default=list(HARSHNESS_PROFILES.keys()))
    parser.add_argument("--sequence-length", type=int, default=104)
    parser.add_argument("--quick", action="store_true", help="Run a small compile/sanity version.")
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
        args.repeats = min(args.repeats, 1)
        args.channels = min(args.channels, 4)
        args.weeks = min(args.weeks, 72)
        args.geos = min(args.geos, 4)
        args.scenarios = ["messy_realistic"]
        args.kpi_variants = args.kpi_variants[:1]
        args.geo_media_patterns = ["embedded_geo_lift_test", "population_distributed_national"]
        args.harshness_profiles = ["minimum_messy"]
        selected_regimes = [
            r
            for r in LOW_DATA_REGIMES
            if r["name"] in {"geo_support_spend_population", "geo_sales_national_media_population", "national_only_support_spend"}
        ]
    else:
        selected_regimes = LOW_DATA_REGIMES

    args.output_dir.mkdir(parents=True, exist_ok=True)
    panels = _make_low_data_panels(
        n_weeks=args.weeks,
        n_geos=args.geos,
        n_channels=args.channels,
        repeats=args.repeats,
        seed=args.seed,
        scenarios=args.scenarios,
        kpi_variants=args.kpi_variants,
        geo_media_patterns=args.geo_media_patterns,
        harshness_profiles=args.harshness_profiles,
        regimes=selected_regimes,
    )
    media_inputs = _available_media_inputs(panels)
    dataset = build_curve_prior_dataset(
        panels,
        media_feature_inputs=media_inputs,
        sequence_length=args.sequence_length,
    )
    result = fit_curve_prior_model(
        dataset,
        epochs=args.epochs,
        hidden_size=args.hidden_size,
        dropout=args.dropout,
        concavity_penalty_weight=args.concavity_penalty_weight,
        seed=args.seed,
    )
    metrics = evaluate_curve_prior_predictions(result)
    result.predictions.to_csv(args.output_dir / "low_data_curve_prior_predictions.csv", index=False)
    result.training_history.to_csv(args.output_dir / "low_data_curve_prior_training_history.csv", index=False)
    metrics.to_csv(args.output_dir / "low_data_curve_prior_metrics.csv", index=False)
    dataset.features.to_csv(args.output_dir / "low_data_curve_prior_training_features.csv", index=False)
    save_curve_prior_model(
        result,
        args.output_dir / "low_data_curve_prior_model_checkpoint.pt",
        extra={
            "panel_count": len(panels),
            "training_example_count": int(len(dataset.features)),
            "media_inputs": media_inputs,
            "concavity_penalty_weight": args.concavity_penalty_weight,
            "regimes": [dict(r) for r in selected_regimes],
            "kpi_variants": args.kpi_variants,
            "geo_media_patterns": args.geo_media_patterns,
            "harshness_profiles": args.harshness_profiles,
            "scope": "low-data response-curve/adstock prior builder stress suite",
        },
    )
    print("Low-data curve-prior suite complete.")
    print(f"Outputs: {args.output_dir}")
    print(f"Panels: {len(panels)}")
    print(f"Training examples: {len(dataset.features)}")
    print(f"Media inputs: {media_inputs}")
    print(metrics.to_string(index=False))
    print(result.training_history.tail(5).to_string(index=False))


if __name__ == "__main__":
    main()
