"""Known-truth synthetic MMM panel generator for NMMM research."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Optional

import numpy as np
import pandas as pd

from .transforms import (
    SUPPORTED_CURVE_TYPES,
    apply_saturation,
    curve_parameter_from_anchor,
    finite_median_positive,
    geometric_adstock_1d,
)


@dataclass
class SyntheticMMMData:
    panel: pd.DataFrame
    truth_media: pd.DataFrame
    truth_params: pd.DataFrame
    response_curves: pd.DataFrame
    truth_economics: Optional[pd.DataFrame] = None
    label_audit: Optional[pd.DataFrame] = None


def _weekly_dates(n_weeks: int, start: str) -> pd.DatetimeIndex:
    return pd.date_range(pd.Timestamp(start), periods=int(n_weeks), freq="W-SUN")


def _media_series(
    rng: np.random.Generator,
    n: int,
    base: float,
    season_phase: float,
    ramp: bool,
    zero_inflated: bool = False,
) -> np.ndarray:
    t = np.arange(n)
    season = 1.0 + 0.18 * np.sin(2 * np.pi * t / 52.0 + season_phase)
    noise = rng.lognormal(mean=0.0, sigma=0.22, size=n)
    pulse = np.ones(n)
    if ramp and n >= 80:
        start = int(rng.integers(18, max(19, n - 20)))
        length = int(rng.integers(4, 10))
        lift = float(rng.uniform(1.35, 2.25))
        pulse[start : min(n, start + length)] *= lift
    if ramp and n >= 100:
        start2 = int(rng.integers(45, max(46, n - 12)))
        length2 = int(rng.integers(3, 8))
        cut = float(rng.uniform(0.1, 0.55))
        pulse[start2 : min(n, start2 + length2)] *= cut
    support = np.maximum(base * season * noise * pulse, 0.0)
    if zero_inflated and n >= 52:
        active = np.ones(n, dtype=bool)
        n_blackouts = int(rng.integers(1, 4))
        for _ in range(n_blackouts):
            start = int(rng.integers(0, max(1, n - 6)))
            length = int(rng.integers(2, min(13, max(3, n - start + 1))))
            active[start : min(n, start + length)] = False
        if rng.uniform() < 0.35:
            active[:] = False
            n_flights = int(rng.integers(2, 6))
            for _ in range(n_flights):
                start = int(rng.integers(0, max(1, n - 6)))
                length = int(rng.integers(4, min(18, max(5, n - start + 1))))
                active[start : min(n, start + length)] = True
        support = np.where(active, support, 0.0)
    return support


def _audit_synthetic_labels(
    panel: pd.DataFrame,
    truth_media: pd.DataFrame,
    truth_params: pd.DataFrame,
    response_curves: pd.DataFrame,
    truth_economics: pd.DataFrame,
) -> pd.DataFrame:
    """Return label-consistency checks for synthetic truth outputs."""
    rows = []
    tm = truth_media.copy()
    if {"true_coef", "saturated_support", "geo_scale", "true_contribution"}.issubset(tm.columns):
        expected = (
            pd.to_numeric(tm["true_coef"], errors="coerce")
            * pd.to_numeric(tm["saturated_support"], errors="coerce")
            * pd.to_numeric(tm["geo_scale"], errors="coerce")
        )
        actual = pd.to_numeric(tm["true_contribution"], errors="coerce")
        err = actual - expected
        rows.append(
            {
                "check": "truth_media_contribution_equals_coef_times_saturation_times_geo_scale",
                "max_abs_error": float(np.nanmax(np.abs(err))),
                "mean_abs_error": float(np.nanmean(np.abs(err))),
                "passed": bool(np.nanmax(np.abs(err)) < 1e-8),
            }
        )
    if {"true_signal", "true_baseline", "true_media_contribution"}.issubset(panel.columns):
        signal_err = (
            pd.to_numeric(panel["true_signal"], errors="coerce")
            - pd.to_numeric(panel["true_baseline"], errors="coerce")
            - pd.to_numeric(panel["true_media_contribution"], errors="coerce")
        )
        rows.append(
            {
                "check": "panel_true_signal_equals_baseline_plus_media",
                "max_abs_error": float(np.nanmax(np.abs(signal_err))),
                "mean_abs_error": float(np.nanmean(np.abs(signal_err))),
                "passed": bool(np.nanmax(np.abs(signal_err)) < 1e-8),
            }
        )
    if {"kpi", "true_signal", "true_noise"}.issubset(panel.columns):
        kpi_err = (
            pd.to_numeric(panel["kpi"], errors="coerce")
            - pd.to_numeric(panel["true_signal"], errors="coerce")
            - pd.to_numeric(panel["true_noise"], errors="coerce")
        )
        rows.append(
            {
                "check": "panel_kpi_equals_true_signal_plus_noise",
                "max_abs_error": float(np.nanmax(np.abs(kpi_err))),
                "mean_abs_error": float(np.nanmean(np.abs(kpi_err))),
                "passed": bool(np.nanmax(np.abs(kpi_err)) < 1e-8),
            }
        )
    if {"channel", "true_total_spend", "true_total_incremental_contribution", "true_roi_like"}.issubset(truth_economics.columns):
        econ = truth_economics.copy()
        expected_roi = pd.to_numeric(econ["true_total_incremental_contribution"], errors="coerce") / np.maximum(
            pd.to_numeric(econ["true_total_spend"], errors="coerce"),
            1e-12,
        )
        roi_err = pd.to_numeric(econ["true_roi_like"], errors="coerce") - expected_roi
        expected_cpo = pd.to_numeric(econ["true_total_spend"], errors="coerce") / np.maximum(
            pd.to_numeric(econ["true_total_incremental_contribution"], errors="coerce"),
            1e-12,
        )
        cpo_err = pd.to_numeric(econ["true_cost_per_incremental_outcome"], errors="coerce") - expected_cpo
        rows.append(
            {
                "check": "truth_economics_roi_and_cost_are_derived_from_totals",
                "max_abs_error": float(max(np.nanmax(np.abs(roi_err)), np.nanmax(np.abs(cpo_err)))),
                "mean_abs_error": float(np.nanmean(np.abs(pd.concat([roi_err, cpo_err], ignore_index=True)))),
                "passed": bool(max(np.nanmax(np.abs(roi_err)), np.nanmax(np.abs(cpo_err))) < 1e-8),
            }
        )
    if {"channel", "pct_of_anchor_support", "true_incremental_contribution", "true_saturation"}.issubset(response_curves.columns):
        max_violation = 0.0
        anchor_errors = []
        for _, curve in response_curves.sort_values(["channel", "pct_of_anchor_support"]).groupby("channel"):
            y = pd.to_numeric(curve["true_incremental_contribution"], errors="coerce").to_numpy(float)
            diffs = np.diff(y[np.isfinite(y)])
            if len(diffs):
                max_violation = max(max_violation, float(np.max(np.maximum(-diffs, 0.0))))
            anchor = curve.iloc[(pd.to_numeric(curve["pct_of_anchor_support"], errors="coerce") - 1.0).abs().argsort()[:1]]
            if not anchor.empty:
                channel = str(anchor["channel"].iloc[0])
                expected_anchor = truth_params.loc[truth_params["channel"].astype(str).eq(channel), "anchor_saturation"]
                if len(expected_anchor):
                    anchor_errors.append(float(anchor["true_saturation"].iloc[0] - expected_anchor.iloc[0]))
        rows.append(
            {
                "check": "response_curves_are_monotone_and_match_anchor_saturation",
                "max_abs_error": float(max(max_violation, np.nanmax(np.abs(anchor_errors)) if anchor_errors else 0.0)),
                "mean_abs_error": float(np.nanmean(np.abs(anchor_errors)) if anchor_errors else 0.0),
                "passed": bool(max_violation < 1e-8 and (not anchor_errors or np.nanmax(np.abs(anchor_errors)) < 1e-6)),
            }
        )
    return pd.DataFrame(rows)


def make_synthetic_mmm_panel(
    n_weeks: int = 156,
    n_geos: int = 8,
    start: str = "2022-01-02",
    channels: Optional[List[str]] = None,
    curve_type: str = "hill",
    scenario: str = "standard",
    national_media_share: float = 0.25,
    collinear_media_strength: float = 0.0,
    missing_media_rate: float = 0.0,
    media_block_missing_rate: float = 0.0,
    zero_inflated_media: bool = False,
    randomize_channel_parameters: bool = False,
    permute_channel_roles: bool = False,
    volatile_media_measurement: bool = False,
    business_shocks: bool = False,
    control_availability: str = "standard",
    kpi_scale_multiplier: float = 1.0,
    noise_sd: float = 0.06,
    seed: int = 20260603,
) -> SyntheticMMMData:
    """Generate a realistic panel with known MMM truth.

    The panel is wide at date x geo x product grain. Media support/spend columns
    are named `{channel}_support` and `{channel}_spend`.
    """
    rng = np.random.default_rng(seed)
    scenario = str(scenario).lower()
    curve_type = str(curve_type).lower()
    if scenario == "messy_realistic":
        national_media_share = max(national_media_share, 0.35)
        collinear_media_strength = max(collinear_media_strength, 0.35)
        missing_media_rate = max(missing_media_rate, 0.015)
        media_block_missing_rate = max(media_block_missing_rate, 0.02)
        zero_inflated_media = True
        volatile_media_measurement = True
        business_shocks = True
        noise_sd = max(noise_sd, 0.075)
    elif scenario == "hostile_collinear":
        collinear_media_strength = max(collinear_media_strength, 0.75)
        national_media_share = max(national_media_share, 0.45)
        volatile_media_measurement = True
        noise_sd = max(noise_sd, 0.08)
    elif scenario == "weak_geo":
        national_media_share = max(national_media_share, 0.80)
        collinear_media_strength = max(collinear_media_strength, 0.25)
        zero_inflated_media = True
        noise_sd = max(noise_sd, 0.07)
    elif scenario != "standard":
        raise ValueError("scenario must be one of: standard, messy_realistic, hostile_collinear, weak_geo")
    if curve_type != "mixed" and curve_type not in SUPPORTED_CURVE_TYPES:
        raise ValueError(f"curve_type must be 'mixed' or one of: {', '.join(SUPPORTED_CURVE_TYPES)}")
    collinear_media_strength = float(np.clip(collinear_media_strength, 0.0, 0.95))
    missing_media_rate = float(np.clip(missing_media_rate, 0.0, 0.50))
    control_availability = str(control_availability).lower()
    if control_availability not in {"none", "partial", "standard", "rich", "noisy_proxy"}:
        raise ValueError("control_availability must be one of: none, partial, standard, rich, noisy_proxy")
    kpi_scale_multiplier = max(float(kpi_scale_multiplier), 1e-6)
    channels = channels or ["tv", "search", "social", "display"]
    curve_type_by_channel: Dict[str, str] = {}
    for ch in channels:
        if curve_type == "mixed":
            curve_type_by_channel[ch] = str(rng.choice(SUPPORTED_CURVE_TYPES))
        else:
            curve_type_by_channel[ch] = curve_type
    dates = _weekly_dates(n_weeks, start)
    geos = [f"geo_{i + 1:02d}" for i in range(n_geos)]
    product_id = "product_total"
    role_defaults: Dict[str, Dict[str, float]] = {
        "tv": {"decay": 0.58, "shape": 1.25, "anchor_sat": 0.45, "coef": 720.0, "cpm": 10.0},
        "search": {"decay": 0.18, "shape": 0.95, "anchor_sat": 0.55, "coef": 410.0, "cpm": 2.8},
        "social": {"decay": 0.36, "shape": 1.15, "anchor_sat": 0.50, "coef": 520.0, "cpm": 4.2},
        "display": {"decay": 0.25, "shape": 1.05, "anchor_sat": 0.60, "coef": 230.0, "cpm": 2.1},
    }
    channel_defaults: Dict[str, Dict[str, float]] = {}
    role_values = list(role_defaults.values())
    if permute_channel_roles:
        rng.shuffle(role_values)
    for ch in channels:
        if randomize_channel_parameters:
            channel_defaults[ch] = {
                "decay": float(rng.uniform(0.15, 0.55)),
                "shape": float(rng.uniform(0.75, 1.65)),
                "anchor_sat": float(rng.uniform(0.28, 0.72)),
                "coef": float(rng.lognormal(np.log(430.0), 0.45)),
                "cpm": float(rng.lognormal(np.log(4.8), 0.55)),
            }
        elif ch in role_defaults:
            channel_defaults[ch] = dict(role_defaults[ch])
        elif permute_channel_roles and role_values:
            channel_defaults[ch] = dict(role_values[len(channel_defaults) % len(role_values)])
        else:
            channel_defaults[ch] = {
                "decay": float(rng.uniform(0.15, 0.55)),
                "shape": float(rng.uniform(0.85, 1.35)),
                "anchor_sat": float(rng.uniform(0.42, 0.62)),
                "coef": float(rng.uniform(180, 620)),
                "cpm": float(rng.uniform(2.0, 9.0)),
            }

    geo_scale = pd.Series(rng.lognormal(mean=0.0, sigma=0.28, size=n_geos), index=geos)
    geo_scale = geo_scale / geo_scale.mean()
    geo_base = pd.Series(rng.uniform(850, 1250, size=n_geos), index=geos) * geo_scale
    geo_population = (900000 * geo_scale * rng.lognormal(mean=0.0, sigma=0.12, size=n_geos)).round().astype(int)

    rows = []
    media_store: Dict[str, Dict[str, np.ndarray]] = {ch: {} for ch in channels}
    spend_store: Dict[str, Dict[str, np.ndarray]] = {ch: {} for ch in channels}
    national_flags: Dict[str, bool] = {}

    shared_signals = {
        "national": _media_series(
            rng,
            n_weeks,
            base=float(rng.uniform(90, 170)),
            season_phase=float(rng.uniform(0, 2 * np.pi)),
            ramp=True,
            zero_inflated=zero_inflated_media,
        )
    }

    for ch_i, ch in enumerate(channels):
        is_national = rng.uniform() < national_media_share
        national_flags[ch] = bool(is_national)
        national_support = _media_series(
            rng,
            n_weeks,
            base=float(rng.uniform(65, 180)) * (1.0 + 0.15 * ch_i),
            season_phase=float(rng.uniform(0, 2 * np.pi)),
            ramp=True,
            zero_inflated=zero_inflated_media,
        )
        for geo in geos:
            if is_national:
                support = national_support * float(geo_scale[geo])
            else:
                support = _media_series(
                    rng,
                    n_weeks,
                    base=float(rng.uniform(45, 160)) * float(geo_scale[geo]),
                    season_phase=float(rng.uniform(0, 2 * np.pi)),
                    ramp=True,
                    zero_inflated=zero_inflated_media,
                )
            if collinear_media_strength > 0:
                shared = shared_signals["national"] * float(geo_scale[geo]) * float(rng.uniform(0.75, 1.25))
                support = (1.0 - collinear_media_strength) * support + collinear_media_strength * shared
            cpm_volatility = 0.20 if volatile_media_measurement else 0.06
            spend = support * channel_defaults[ch]["cpm"] * rng.lognormal(0.0, cpm_volatility, n_weeks)
            media_store[ch][geo] = support
            spend_store[ch][geo] = spend

    media_feature_names = ["support", "spend", "impressions", "clicks", "grps", "reach", "frequency"]
    missing_blocks: Dict[str, Dict[str, Dict[str, np.ndarray]]] = {
        ch: {geo: {feature: np.zeros(n_weeks, dtype=bool) for feature in media_feature_names} for geo in geos}
        for ch in channels
    }
    if media_block_missing_rate > 0:
        for ch in channels:
            for geo in geos:
                for feature_name in media_feature_names:
                    if rng.uniform() < media_block_missing_rate:
                        start = int(rng.integers(0, max(1, n_weeks - 4)))
                        length = int(rng.integers(3, min(13, max(4, n_weeks - start + 1))))
                        missing_blocks[ch][geo][feature_name][start : min(n_weeks, start + length)] = True

    truth_rows = []
    params_rows = []
    contribution_by_geo = {geo: np.zeros(n_weeks) for geo in geos}

    for ch in channels:
        decay = channel_defaults[ch]["decay"]
        ch_curve_type = curve_type_by_channel[ch]
        shape = channel_defaults[ch]["shape"]
        if ch_curve_type == "threshold":
            shape = max(3.0, shape * 4.5)
        elif ch_curve_type == "gompertz":
            shape = max(1.2, shape * 2.0)
        elif ch_curve_type == "linear_plateau":
            shape = 1.0
        elif ch_curve_type == "near_linear":
            shape = min(shape, 0.90)
        anchor_sat = channel_defaults[ch]["anchor_sat"]
        if ch_curve_type == "near_linear":
            anchor_sat = min(anchor_sat, 0.35)
        elif ch_curve_type == "linear_plateau":
            anchor_sat = float(np.clip(anchor_sat, 0.35, 0.75))
        elif ch_curve_type == "threshold":
            anchor_sat = float(np.clip(anchor_sat, 0.25, 0.65))
        coef = channel_defaults[ch]["coef"]
        adstock_all = []
        for geo in geos:
            adstock = geometric_adstock_1d(media_store[ch][geo], decay)
            adstock_all.append(adstock)
        anchor = finite_median_positive(np.concatenate(adstock_all), fallback=100.0)
        curve_param = curve_parameter_from_anchor(anchor, anchor_sat, curve_type=ch_curve_type, shape=shape)
        for geo in geos:
            support = media_store[ch][geo]
            spend = spend_store[ch][geo]
            adstock = geometric_adstock_1d(support, decay)
            saturated = apply_saturation(adstock, curve_param, shape=shape, curve_type=ch_curve_type)
            contribution = coef * saturated * float(geo_scale[geo])
            contribution_by_geo[geo] += contribution
            for i, date in enumerate(dates):
                truth_rows.append(
                    {
                        "date": date,
                        "geo_id": geo,
                        "product_id": product_id,
                        "channel": ch,
                        "support": float(support[i]),
                        "spend": float(spend[i]),
                        "adstock": float(adstock[i]),
                        "saturated_support": float(saturated[i]),
                        "true_contribution": float(contribution[i]),
                        "true_decay": float(decay),
                        "true_curve_type": ch_curve_type,
                        "true_curve_param": float(curve_param),
                        "true_shape": float(shape),
                        "true_coef": float(coef),
                        "geo_scale": float(geo_scale[geo]),
                        "national_repeated_media": bool(national_flags[ch]),
                    }
                )
        params_rows.append(
            {
                "channel": ch,
                "curve_type": ch_curve_type,
                "decay": float(decay),
                "curve_param": float(curve_param),
                "shape": float(shape),
                "coef": float(coef),
                "cpm": float(channel_defaults[ch]["cpm"]),
                "anchor_saturation": float(anchor_sat),
                "anchor_support": float(anchor),
                "national_repeated_media": bool(national_flags[ch]),
            }
        )

    for geo in geos:
        t = np.arange(n_weeks)
        trend = 0.55 * t
        season = 95 * np.sin(2 * np.pi * t / 52.0 + rng.uniform(0, 2 * np.pi))
        holiday = ((pd.Series(dates).dt.month.eq(11)) | (pd.Series(dates).dt.month.eq(12))).astype(int).to_numpy()
        promo = (rng.uniform(size=n_weeks) < 0.13).astype(float)
        price_index = 1.0 + 0.04 * np.sin(2 * np.pi * t / 26.0 + rng.uniform(0, 2 * np.pi)) + rng.normal(0, 0.015, n_weeks)
        competitor_index = 1.0 + 0.05 * np.sin(2 * np.pi * t / 39.0 + rng.uniform(0, 2 * np.pi)) + rng.normal(0, 0.02, n_weeks)
        macro_index = 1.0 + 0.025 * np.sin(2 * np.pi * t / 104.0 + rng.uniform(0, 2 * np.pi)) + rng.normal(0, 0.01, n_weeks)
        category_trend = 0.25 * t + 35.0 * np.sin(2 * np.pi * t / 52.0 + rng.uniform(0, 2 * np.pi))
        unobserved_promo = (rng.uniform(size=n_weeks) < 0.05).astype(float)
        control_contrib = (
            130 * promo
            - 420 * (price_index - 1.0)
            + 65 * holiday
            - 260 * (competitor_index - 1.0)
            + 180 * (macro_index - 1.0)
            + 0.35 * category_trend
            + 180 * unobserved_promo
        )
        shock_contrib = np.zeros(n_weeks, dtype=float)
        structural_break = np.zeros(n_weeks, dtype=float)
        if business_shocks and n_weeks >= 80:
            for _ in range(2):
                shock_start = int(rng.integers(20, max(21, n_weeks - 12)))
                shock_len = int(rng.integers(3, 8))
                shock_size = float(rng.normal(0.0, 190.0))
                shock_contrib[shock_start : min(n_weeks, shock_start + shock_len)] += shock_size
            if rng.uniform() < 0.50:
                break_start = int(rng.integers(30, max(31, n_weeks - 20)))
                structural_break[break_start:] = float(rng.normal(0.0, 140.0))
        baseline = geo_base[geo] + trend + season + control_contrib
        baseline = baseline + shock_contrib + structural_break
        signal = baseline + contribution_by_geo[geo]
        noise = rng.normal(0, max(1.0, noise_sd * np.std(signal)), n_weeks)
        y = signal + noise
        for i, date in enumerate(dates):
            row = {
                "date": date,
                "geo_id": geo,
                "group_id": geo,
                "product_id": product_id,
                "population": int(geo_population[geo]),
                "kpi": float(kpi_scale_multiplier * y[i]),
                "true_baseline": float(kpi_scale_multiplier * baseline[i]),
                "true_media_contribution": float(kpi_scale_multiplier * contribution_by_geo[geo][i]),
                "true_signal": float(kpi_scale_multiplier * signal[i]),
                "true_noise": float(kpi_scale_multiplier * noise[i]),
            }
            if control_availability in {"partial", "standard", "rich", "noisy_proxy"}:
                row["promo"] = float(promo[i])
                row["holiday"] = float(holiday[i])
            if control_availability in {"standard", "rich", "noisy_proxy"}:
                row["price_index"] = float(price_index[i])
            if control_availability in {"rich", "noisy_proxy"}:
                noise_level = 0.03 if control_availability == "noisy_proxy" else 0.0
                row["competitor_index"] = float(competitor_index[i] + rng.normal(0.0, noise_level))
                row["macro_index"] = float(macro_index[i] + rng.normal(0.0, noise_level / 2.0))
                row["category_trend"] = float(category_trend[i] + rng.normal(0.0, 8.0 if control_availability == "noisy_proxy" else 0.0))
            for ch in channels:
                support_value = float(media_store[ch][geo][i])
                spend_value = float(spend_store[ch][geo][i])
                impression_noise = 0.28 if volatile_media_measurement else 0.07
                impressions_value = support_value * 1000.0 * float(rng.lognormal(0.0, impression_noise))
                if volatile_media_measurement and rng.uniform() < 0.12:
                    impressions_value *= float(rng.uniform(0.65, 1.45))
                if ch.lower() in {"search", "affiliate", "retail_search"}:
                    ctr = float(rng.uniform(0.018, 0.055))
                elif ch.lower() in {"social", "display", "video"}:
                    ctr = float(rng.uniform(0.002, 0.018))
                else:
                    ctr = float(rng.uniform(0.0004, 0.006))
                ctr_noise = 0.35 if volatile_media_measurement else 0.12
                clicks_value = impressions_value * ctr * float(rng.lognormal(0.0, ctr_noise))
                grps_value = support_value * float(rng.uniform(0.25, 1.35))
                frequency_value = float(rng.lognormal(np.log(2.4), 0.28))
                if volatile_media_measurement:
                    frequency_value *= float(rng.lognormal(0.0, 0.18))
                frequency_value = max(1.0, frequency_value)
                reach_value = max(1.0, impressions_value / frequency_value)
                feature_values = {
                    "support": support_value,
                    "spend": spend_value,
                    "impressions": impressions_value,
                    "clicks": clicks_value,
                    "grps": grps_value,
                    "reach": reach_value,
                    "frequency": frequency_value,
                }
                for feature_name, feature_value in list(feature_values.items()):
                    is_missing = missing_media_rate > 0 and rng.uniform() < missing_media_rate
                    is_missing = is_missing or bool(missing_blocks[ch][geo][feature_name][i])
                    if is_missing:
                        feature_values[feature_name] = np.nan
                row[f"{ch}_support"] = feature_values["support"]
                row[f"{ch}_spend"] = feature_values["spend"]
                row[f"{ch}_impressions"] = feature_values["impressions"]
                row[f"{ch}_clicks"] = feature_values["clicks"]
                row[f"{ch}_grps"] = feature_values["grps"]
                row[f"{ch}_reach"] = feature_values["reach"]
                row[f"{ch}_frequency"] = feature_values["frequency"]
            rows.append(row)

    panel = pd.DataFrame(rows)
    truth_media = pd.DataFrame(truth_rows)
    truth_media["true_contribution"] = truth_media["true_contribution"] * kpi_scale_multiplier
    truth_media["true_coef"] = truth_media["true_coef"] * kpi_scale_multiplier
    truth_params = pd.DataFrame(params_rows)
    truth_params["coef"] = truth_params["coef"] * kpi_scale_multiplier
    response_curves = []
    economics_rows = []
    for _, p in truth_params.iterrows():
        ch_truth = truth_media.loc[truth_media["channel"].astype(str).eq(str(p["channel"]))].copy()
        support_sum = float(ch_truth["support"].sum())
        spend_sum = float(ch_truth["spend"].sum())
        contribution_sum = float(ch_truth["true_contribution"].sum())
        support_positive = pd.to_numeric(ch_truth["support"], errors="coerce").to_numpy(float)
        spend_positive = pd.to_numeric(ch_truth["spend"], errors="coerce").to_numpy(float)
        spend_per_support = spend_positive / np.maximum(support_positive, 1e-12)
        spend_per_support = spend_per_support[np.isfinite(spend_per_support) & (support_positive > 0)]
        median_spend_per_support = float(np.median(spend_per_support)) if len(spend_per_support) else float(p["cpm"])
        grid = np.linspace(0.0, 2.0, 81)
        steady_state_adstock = p["anchor_support"] * grid
        steady_state_support = steady_state_adstock * max(1.0 - p["decay"], 1e-6)
        steady_state_spend = steady_state_support * median_spend_per_support
        sat = apply_saturation(steady_state_adstock, p["curve_param"], shape=p["shape"], curve_type=p["curve_type"])
        contribution_curve = p["coef"] * sat
        mroi_curve = np.gradient(contribution_curve, np.maximum(steady_state_spend, 1e-12))
        roi_curve = contribution_curve / np.maximum(steady_state_spend, 1e-12)
        for pct, raw_support, adstock, spend, s, c, roi, mroi in zip(
            grid,
            steady_state_support,
            steady_state_adstock,
            steady_state_spend,
            sat,
            contribution_curve,
            roi_curve,
            mroi_curve,
        ):
            response_curves.append(
                {
                    "channel": p["channel"],
                    "curve_type": p["curve_type"],
                    "pct_of_anchor_support": float(pct),
                    "steady_state_support": float(raw_support),
                    "steady_state_spend": float(spend),
                    "steady_state_adstock": float(adstock),
                    "true_saturation": float(s),
                    "true_incremental_contribution": float(c),
                    "true_roi_like": float(roi) if pct > 0 else np.nan,
                    "true_mroi_like": float(mroi) if pct > 0 else np.nan,
                }
            )
        idx_current = int(np.argmin(np.abs(grid - 1.0)))
        economics_rows.append(
            {
                "channel": p["channel"],
                "true_total_support": support_sum,
                "true_total_spend": spend_sum,
                "true_total_incremental_contribution": contribution_sum,
                "true_roi_like": contribution_sum / spend_sum if spend_sum > 0 else np.nan,
                "true_cost_per_incremental_outcome": spend_sum / contribution_sum if contribution_sum > 0 else np.nan,
                "true_mroi_like_at_anchor": float(mroi_curve[idx_current]),
                "true_cost_per_marginal_outcome_at_anchor": (1.0 / float(mroi_curve[idx_current])) if float(mroi_curve[idx_current]) > 0 else np.nan,
                "median_spend_per_support": median_spend_per_support,
                "label_basis": "actual_truth_media_totals_and_transform_response_curve",
            }
        )
    truth_economics = pd.DataFrame(economics_rows)
    label_audit = _audit_synthetic_labels(panel, truth_media, truth_params, pd.DataFrame(response_curves), truth_economics)

    return SyntheticMMMData(
        panel=panel.sort_values(["geo_id", "date"]).reset_index(drop=True),
        truth_media=truth_media.sort_values(["geo_id", "date", "channel"]).reset_index(drop=True),
        truth_params=truth_params,
        response_curves=pd.DataFrame(response_curves),
        truth_economics=truth_economics,
        label_audit=label_audit,
    )
