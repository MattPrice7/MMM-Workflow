"""Smoke test for the neural response-curve prior model.

This trains a black-box-ish neural encoder to predict conservative curve/adstock
priors from realistic synthetic MMM panels. It is not a full MMM and does not
estimate final ROI/contribution.
"""

from __future__ import annotations

from dataclasses import replace
from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd

from nmmm import make_synthetic_mmm_panel
from nmmm.curve_prior_model import (
    build_curve_prior_dataset,
    evaluate_curve_prior_predictions,
    fit_curve_prior_model,
    save_curve_prior_model,
)


ROOT = Path(__file__).resolve().parent
OUT = ROOT / "outputs" / "curve_prior_smoke"
CHANNELS = [f"ch_{i + 1:02d}" for i in range(10)]
RICH_MEDIA_SUFFIXES = ["_support", "_spend", "_impressions", "_clicks", "_grps", "_reach", "_frequency"]


def _channel_cols(panel: pd.DataFrame, suffixes: Iterable[str] = RICH_MEDIA_SUFFIXES) -> list[str]:
    cols = []
    for col in panel.columns:
        if any(str(col).endswith(suffix) for suffix in suffixes):
            cols.append(str(col))
    return cols


def _aggregate_truth_media_to_national(truth_media: pd.DataFrame) -> pd.DataFrame:
    keep_first = [
        "product_id",
        "true_decay",
        "true_curve_type",
        "true_curve_param",
        "true_shape",
        "true_coef",
        "national_repeated_media",
    ]
    sum_cols = ["support", "spend", "adstock", "true_contribution"]
    mean_cols = ["saturated_support"]
    rows = []
    for keys, group in truth_media.groupby(["date", "channel"], dropna=False):
        row = {"date": keys[0], "geo_id": "national", "channel": keys[1], "geo_scale": 1.0}
        for col in sum_cols:
            if col in group.columns:
                row[col] = float(pd.to_numeric(group[col], errors="coerce").sum())
        for col in mean_cols:
            if col in group.columns:
                row[col] = float(pd.to_numeric(group[col], errors="coerce").mean())
        for col in keep_first:
            if col in group.columns:
                row[col] = group[col].iloc[0]
        rows.append(row)
    return pd.DataFrame(rows)


def _to_national_only(synth):
    panel = synth.panel.copy()
    panel["date"] = pd.to_datetime(panel["date"])
    media_cols = _channel_cols(panel)
    sum_cols = [c for c in ["kpi", "true_baseline", "true_media_contribution", "true_signal", "true_noise", "population"] if c in panel.columns]
    mean_cols = [c for c in panel.columns if c not in {"date", "geo_id", "group_id", "product_id"} and c not in media_cols and c not in sum_cols]
    media_agg = panel.groupby("date", as_index=False)[media_cols + sum_cols].sum(min_count=1)
    if mean_cols:
        controls = panel.groupby("date", as_index=False)[mean_cols].mean(numeric_only=True)
        media_agg = media_agg.merge(controls, on="date", how="left")
    media_agg["geo_id"] = "national"
    media_agg["group_id"] = "national"
    media_agg["product_id"] = "product_total"
    cols = ["date", "geo_id", "group_id", "product_id"] + [c for c in media_agg.columns if c not in {"date", "geo_id", "group_id", "product_id"}]
    media_agg = media_agg[cols]
    truth_media = _aggregate_truth_media_to_national(synth.truth_media)
    return replace(synth, panel=media_agg, truth_media=truth_media)


def _to_geo_sales_national_media(synth):
    panel = synth.panel.copy()
    panel["date"] = pd.to_datetime(panel["date"])
    n_geos = max(panel["geo_id"].nunique(), 1)
    for col in _channel_cols(panel):
        national = panel.groupby("date")[col].sum(min_count=1) / n_geos
        panel[col] = panel["date"].map(national)
    return replace(synth, panel=panel)


def _apply_measurement_regime(synth, measurement: str):
    panel = synth.panel.copy()
    measurement = str(measurement)
    if measurement == "spend_only":
        drop_suffixes = ["_support", "_impressions", "_clicks", "_grps", "_reach", "_frequency"]
        drop_cols = _channel_cols(panel, suffixes=drop_suffixes)
        panel = panel.drop(columns=drop_cols, errors="ignore")
    elif measurement == "support_only":
        drop_cols = _channel_cols(panel, suffixes=["_spend"])
        panel = panel.drop(columns=drop_cols, errors="ignore")
    elif measurement == "support_spend":
        drop_suffixes = ["_impressions", "_clicks", "_grps", "_reach", "_frequency"]
        drop_cols = _channel_cols(panel, suffixes=drop_suffixes)
        panel = panel.drop(columns=drop_cols, errors="ignore")
    elif measurement == "rich_media":
        pass
    else:
        raise ValueError(f"Unknown measurement regime: {measurement}")
    return replace(synth, panel=panel)


def _apply_geo_regime(synth, geo_regime: str):
    if geo_regime == "full_geo":
        return synth
    if geo_regime == "geo_sales_national_media":
        return _to_geo_sales_national_media(synth)
    if geo_regime == "national_only":
        return _to_national_only(synth)
    raise ValueError(f"Unknown geo regime: {geo_regime}")


def _tag_panel(synth, *, split_group: str, scenario: str, geo_regime: str, measurement_regime: str):
    setattr(synth, "curve_prior_split_group", split_group)
    setattr(synth, "curve_prior_scenario", scenario)
    setattr(synth, "curve_prior_geo_regime", geo_regime)
    setattr(synth, "curve_prior_measurement_regime", measurement_regime)
    return synth


def _make_training_panels() -> list:
    panels = []
    scenarios = ["standard", "messy_realistic", "hostile_collinear", "weak_geo"]
    curve_types = ["mixed"]
    geo_regimes = ["full_geo", "geo_sales_national_media", "national_only"]
    measurement_regimes = ["rich_media", "support_spend", "spend_only", "support_only"]
    seed = 20260670
    for scenario in scenarios:
        for curve_type in curve_types:
            for repeat in range(3):
                split_group = f"{scenario}_{curve_type}_{repeat:02d}"
                base = make_synthetic_mmm_panel(
                    n_weeks=156,
                    n_geos=8,
                    channels=CHANNELS,
                    curve_type=curve_type,
                    scenario=scenario,
                    randomize_channel_parameters=True,
                    permute_channel_roles=True,
                    zero_inflated_media=scenario in {"messy_realistic", "weak_geo"},
                    volatile_media_measurement=scenario in {"messy_realistic", "hostile_collinear"},
                    missing_media_rate=0.02 if scenario == "messy_realistic" else 0.0,
                    media_block_missing_rate=0.08 if scenario == "messy_realistic" else 0.0,
                    control_availability="noisy_proxy" if scenario == "messy_realistic" else "standard",
                    seed=seed,
                )
                seed += 1
                for geo_regime in geo_regimes:
                    for measurement in measurement_regimes:
                        panel = _apply_measurement_regime(_apply_geo_regime(base, geo_regime), measurement)
                        panels.append(
                            _tag_panel(
                                panel,
                                split_group=split_group,
                                scenario=scenario,
                                geo_regime=geo_regime,
                                measurement_regime=measurement,
                            )
                        )
    return panels


def main() -> None:
    try:
        import torch  # noqa: F401
    except Exception:
        print("Torch is not installed. Install with: pip install torch")
        return

    OUT.mkdir(parents=True, exist_ok=True)
    panels = _make_training_panels()
    dataset = build_curve_prior_dataset(
        panels,
        media_feature_inputs=["support", "spend", "impressions", "clicks", "grps", "reach", "frequency"],
    )
    result = fit_curve_prior_model(dataset, epochs=450, hidden_size=128, dropout=0.08, seed=20260670)
    metrics = evaluate_curve_prior_predictions(result)
    result.predictions.to_csv(OUT / "curve_prior_predictions.csv", index=False)
    result.training_history.to_csv(OUT / "curve_prior_training_history.csv", index=False)
    metrics.to_csv(OUT / "curve_prior_metrics.csv", index=False)
    dataset.features.to_csv(OUT / "curve_prior_training_features.csv", index=False)
    save_curve_prior_model(
        result,
        OUT / "curve_prior_model_checkpoint.pt",
        extra={
            "panel_count": len(panels),
            "training_example_count": int(len(dataset.features)),
            "split_group_count": int(dataset.features["split_group"].nunique()) if "split_group" in dataset.features.columns else None,
            "scope": "response-curve/adstock prior builder, not final causal ROI model",
        },
    )
    print("Curve-prior smoke complete.")
    print(f"Outputs: {OUT}")
    print(metrics.to_string(index=False))
    print(result.training_history.tail(5).to_string(index=False))


if __name__ == "__main__":
    main()
