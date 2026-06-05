"""Neural response-curve prior model.

This module is deliberately narrower than the full NMMM/TFT lane. It predicts
curve/adstock priors and identifiability diagnostics; it does not predict final
ROI, contribution, or incrementality.

The model output is a flexible monotone response grid, not a named Hill/Weibull
parameterization. Named curves are used only as synthetic truth generators and
audit labels.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional

import numpy as np
import pandas as pd

from .training_data_validation import validate_nmmm_training_data


CURVE_GRID = np.array([0.0, 0.10, 0.25, 0.50, 0.75, 0.90, 1.0, 1.25, 1.50, 2.0], dtype=float)


@dataclass
class CurvePriorDataset:
    features: pd.DataFrame
    feature_columns: List[str]
    channel_sequence_columns: List[str]
    geo_sequence_columns: List[str]
    set_feature_columns: List[str]
    curve_targets: np.ndarray
    adstock_targets: np.ndarray
    saturation_targets: np.ndarray
    confidence_targets: np.ndarray
    fallback_targets: np.ndarray
    uncertainty_targets: np.ndarray
    curve_target_quality: Optional[np.ndarray] = None
    channel_sequences: Optional[np.ndarray] = None
    geo_sequences: Optional[np.ndarray] = None
    set_features: Optional[np.ndarray] = None
    set_channel_mask: Optional[np.ndarray] = None
    target_channel_index: Optional[np.ndarray] = None


@dataclass
class CurvePriorResult:
    predictions: pd.DataFrame
    training_history: pd.DataFrame
    feature_columns: List[str]
    settings: Dict[str, Any]
    model_state: Optional[Dict[str, Any]] = None


def _require_torch_nn():
    try:
        import torch
        import torch.nn as nn
        import torch.nn.functional as F
    except Exception as exc:  # pragma: no cover - optional dependency.
        raise ImportError("Torch is required for the neural curve-prior model.") from exc
    return torch, nn, F


def _safe_numeric(values: Iterable[object]) -> np.ndarray:
    arr = pd.to_numeric(pd.Series(values), errors="coerce").to_numpy(float)
    return arr[np.isfinite(arr)]


def _stats(values: Iterable[object], prefix: str) -> Dict[str, float]:
    arr = pd.to_numeric(pd.Series(values), errors="coerce").to_numpy(float)
    finite = arr[np.isfinite(arr)]
    pos = finite[finite > 0]
    out = {
        f"{prefix}_available": float(finite.size > 0),
        f"{prefix}_missing_rate": float(1.0 - finite.size / max(len(arr), 1)),
        f"{prefix}_positive_share": float(np.mean(finite > 0)) if finite.size else 0.0,
        f"{prefix}_mean_positive": float(np.mean(pos)) if pos.size else 0.0,
        f"{prefix}_median_positive": float(np.median(pos)) if pos.size else 0.0,
        f"{prefix}_cv_positive": float(np.std(pos) / max(abs(np.mean(pos)), 1e-12)) if pos.size >= 3 else 0.0,
        f"{prefix}_max_to_median_positive": float(np.max(pos) / max(np.median(pos), 1e-12)) if pos.size else 0.0,
        f"{prefix}_p90_to_median_positive": float(np.percentile(pos, 90) / max(np.median(pos), 1e-12)) if pos.size else 0.0,
    }
    if pos.size >= 4:
        diffs = np.diff(pos)
        out[f"{prefix}_mean_abs_change_ratio"] = float(np.mean(np.abs(diffs)) / max(np.mean(pos), 1e-12))
    else:
        out[f"{prefix}_mean_abs_change_ratio"] = 0.0
    return out


def _safe_corr(x: Iterable[object], y: Iterable[object]) -> float:
    x_arr = pd.to_numeric(pd.Series(x), errors="coerce").to_numpy(float)
    y_arr = pd.to_numeric(pd.Series(y), errors="coerce").to_numpy(float)
    ok = np.isfinite(x_arr) & np.isfinite(y_arr)
    if ok.sum() < 4 or np.nanstd(x_arr[ok]) <= 1e-12 or np.nanstd(y_arr[ok]) <= 1e-12:
        return 0.0
    return float(np.corrcoef(x_arr[ok], y_arr[ok])[0, 1])


def _ordered_dates(panel: pd.DataFrame, sequence_length: int) -> pd.DatetimeIndex:
    dates = pd.DatetimeIndex(pd.to_datetime(panel["date"]).dropna().sort_values().unique())
    if sequence_length and len(dates) > sequence_length:
        dates = dates[-int(sequence_length) :]
    return dates


def _pad_sequence_matrix(matrix: np.ndarray, sequence_length: int) -> np.ndarray:
    arr = np.asarray(matrix, dtype=float)
    if not sequence_length or arr.shape[0] == sequence_length:
        return arr
    if arr.shape[0] > sequence_length:
        return arr[-int(sequence_length) :, :]
    pad = np.zeros((int(sequence_length) - arr.shape[0], arr.shape[1]), dtype=float)
    return np.vstack([pad, arr])


def _date_series(panel: pd.DataFrame, col: str, dates: pd.DatetimeIndex, agg: str = "sum") -> np.ndarray:
    if col not in panel.columns:
        return np.full(len(dates), np.nan, dtype=float)
    tmp = panel[["date", col]].copy()
    tmp["date"] = pd.to_datetime(tmp["date"])
    tmp[col] = pd.to_numeric(tmp[col], errors="coerce")
    if agg == "mean":
        grouped = tmp.groupby("date")[col].mean()
    else:
        grouped = tmp.groupby("date")[col].sum(min_count=1)
    return grouped.reindex(dates).to_numpy(float)


def _robust_z(values: Iterable[object], log_positive: bool = False) -> np.ndarray:
    arr = pd.to_numeric(pd.Series(values), errors="coerce").to_numpy(float)
    if log_positive:
        arr = np.where(np.isfinite(arr), np.log1p(np.maximum(arr, 0.0)), np.nan)
    finite = np.isfinite(arr)
    out = np.zeros_like(arr, dtype=float)
    if finite.sum() < 3:
        return out
    med = float(np.nanmedian(arr[finite]))
    iqr = float(np.nanpercentile(arr[finite], 75) - np.nanpercentile(arr[finite], 25))
    scale = iqr / 1.349 if iqr > 1e-8 else float(np.nanstd(arr[finite]))
    scale = scale if scale > 1e-8 else 1.0
    out[finite] = (arr[finite] - med) / scale
    return np.clip(out, -8.0, 8.0)


def _value_and_missing(values: Iterable[object], log_positive: bool = True) -> tuple[np.ndarray, np.ndarray]:
    arr = pd.to_numeric(pd.Series(values), errors="coerce").to_numpy(float)
    missing = (~np.isfinite(arr)).astype(float)
    return _robust_z(arr, log_positive=log_positive), missing


def _residualized_kpi_signal(panel: pd.DataFrame, dates: pd.DatetimeIndex, controls: List[str]) -> np.ndarray:
    y = _date_series(panel, "kpi", dates, agg="sum")
    t = np.linspace(-1.0, 1.0, len(dates))
    design = [np.ones(len(dates)), t, np.sin(2 * np.pi * np.arange(len(dates)) / 52.0), np.cos(2 * np.pi * np.arange(len(dates)) / 52.0)]
    for control in controls:
        if control in panel.columns:
            design.append(_robust_z(_date_series(panel, control, dates, agg="mean"), log_positive=False))
    x = np.column_stack(design)
    ok = np.isfinite(y) & np.all(np.isfinite(x), axis=1)
    if ok.sum() < x.shape[1] + 3:
        return _robust_z(y, log_positive=False)
    beta, *_ = np.linalg.lstsq(x[ok], y[ok], rcond=None)
    resid = y - x @ beta
    return _robust_z(resid, log_positive=False)


def _build_channel_sequence(
    panel: pd.DataFrame,
    channel: str,
    dates: pd.DatetimeIndex,
    media_feature_inputs: List[str],
    controls: List[str],
    include_residualized_kpi_signal: bool,
) -> tuple[np.ndarray, List[str]]:
    cols: List[str] = []
    blocks: List[np.ndarray] = []
    for feature in media_feature_inputs:
        values = _date_series(panel, f"{channel}_{feature}", dates, agg="sum")
        value_z, missing = _value_and_missing(values, log_positive=True)
        blocks.extend([value_z, missing])
        cols.extend([f"{feature}_value_z", f"{feature}_missing_mask"])
    if include_residualized_kpi_signal:
        blocks.append(_residualized_kpi_signal(panel, dates, controls))
        cols.append("residualized_kpi_signal")
    return np.column_stack(blocks).astype(float), cols


def _date_geo_matrix(panel: pd.DataFrame, col: str, dates: pd.DatetimeIndex, group_col: str = "geo_id") -> np.ndarray:
    if col not in panel.columns or group_col not in panel.columns:
        return np.full((len(dates), 1), np.nan, dtype=float)
    tmp = panel[["date", group_col, col]].copy()
    tmp["date"] = pd.to_datetime(tmp["date"])
    tmp[col] = pd.to_numeric(tmp[col], errors="coerce")
    pivot = tmp.pivot_table(index="date", columns=group_col, values=col, aggfunc="sum")
    return pivot.reindex(dates).to_numpy(float)


def _per_population_matrix(panel: pd.DataFrame, value_col: str, dates: pd.DatetimeIndex, group_col: str = "geo_id") -> np.ndarray:
    value = _date_geo_matrix(panel, value_col, dates, group_col=group_col)
    population = _date_geo_matrix(panel, "population", dates, group_col=group_col)
    if value.shape != population.shape:
        return np.full_like(value, np.nan, dtype=float)
    return value / np.maximum(population, 1.0)


def _geo_matrix_features(mat: np.ndarray, prefix: str) -> tuple[List[np.ndarray], List[str]]:
    arr = np.asarray(mat, dtype=float)
    finite = np.isfinite(arr)
    finite_count = np.sum(finite, axis=1)
    valid_any = finite_count > 0
    filled = np.where(finite, arr, 0.0)
    total = np.sum(filled, axis=1)
    mean = np.divide(total, np.maximum(finite_count, 1), out=np.zeros_like(total), where=finite_count > 0)
    centered = np.where(finite, arr - mean.reshape(-1, 1), 0.0)
    sd = np.sqrt(np.divide(np.sum(centered**2, axis=1), np.maximum(finite_count, 1), out=np.zeros_like(total), where=finite_count > 0))
    cv = sd / np.maximum(np.abs(mean), 1e-8)
    zero_share = np.divide(
        np.sum((filled <= 0) & finite, axis=1),
        np.maximum(finite_count, 1),
        out=np.ones_like(total),
        where=finite_count > 0,
    )
    positive = np.maximum(filled, 0.0)
    row_sum = np.maximum(np.sum(positive, axis=1), 1e-8)
    top_share = np.max(positive, axis=1) / row_sum
    top_share = np.where(valid_any, top_share, 1.0)
    log_total = np.log1p(np.maximum(total, 0.0))
    change = np.r_[0.0, np.diff(log_total)]
    pct = np.diff(np.log1p(positive), axis=0)
    ramp_heterogeneity = np.r_[0.0, np.std(pct, axis=1)] if pct.size else np.zeros(arr.shape[0])
    features = [
        _robust_z(total, log_positive=True),
        np.nan_to_num(cv, nan=0.0, posinf=0.0, neginf=0.0),
        np.nan_to_num(zero_share, nan=1.0),
        np.nan_to_num(top_share, nan=1.0),
        _robust_z(change, log_positive=False),
        np.nan_to_num(ramp_heterogeneity, nan=0.0, posinf=0.0, neginf=0.0),
    ]
    cols = [
        f"{prefix}_total_z",
        f"{prefix}_geo_cv",
        f"{prefix}_geo_zero_share",
        f"{prefix}_top_geo_share",
        f"{prefix}_national_change_z",
        f"{prefix}_geo_ramp_heterogeneity",
    ]
    return features, cols


def _build_geo_time_sequence(panel: pd.DataFrame, channel: str, dates: pd.DatetimeIndex) -> tuple[np.ndarray, List[str]]:
    blocks: List[np.ndarray] = []
    cols: List[str] = []
    for feature in ["support", "spend"]:
        value_col = f"{channel}_{feature}"
        for mat, prefix in [
            (_date_geo_matrix(panel, value_col, dates), feature),
            (_per_population_matrix(panel, value_col, dates), f"{feature}_per_population"),
        ]:
            feat_blocks, feat_cols = _geo_matrix_features(mat, prefix)
            blocks.extend(feat_blocks)
            cols.extend(feat_cols)
    kpi_blocks, kpi_cols = _geo_matrix_features(_date_geo_matrix(panel, "kpi", dates), "kpi")
    blocks.extend(kpi_blocks[:4])
    cols.extend(kpi_cols[:4])
    return np.column_stack(blocks).astype(float), cols


def _global_channel_series(panel: pd.DataFrame, channel: str, suffix: str) -> pd.Series:
    col = f"{channel}{suffix}"
    if col not in panel.columns:
        return pd.Series(np.nan, index=sorted(pd.to_datetime(panel["date"]).unique()))
    tmp = panel[["date", col]].copy()
    tmp["date"] = pd.to_datetime(tmp["date"])
    tmp[col] = pd.to_numeric(tmp[col], errors="coerce")
    return tmp.groupby("date")[col].sum(min_count=1).sort_index()


def _geo_variation(panel: pd.DataFrame, channel: str, suffix: str, group_col: str = "geo_id") -> float:
    col = f"{channel}{suffix}"
    if col not in panel.columns or group_col not in panel.columns:
        return 0.0
    tmp = panel[["date", group_col, col]].copy()
    tmp["date"] = pd.to_datetime(tmp["date"])
    tmp[col] = pd.to_numeric(tmp[col], errors="coerce")
    pivot = tmp.pivot_table(index="date", columns=group_col, values=col, aggfunc="sum")
    if pivot.shape[1] < 2:
        return 0.0
    arr = pivot.to_numpy(float)
    if np.isfinite(arr).sum() < 6:
        return 0.0
    return float(np.nanmean(np.nanvar(arr, axis=1)) / max(np.nanvar(arr), 1e-12))


def _window_mean(arr: np.ndarray, start: int, end: int) -> np.ndarray:
    if end <= start:
        return np.full(arr.shape[1], np.nan, dtype=float)
    with np.errstate(invalid="ignore"):
        return np.nanmean(arr[start:end, :], axis=0)


def _quasi_geo_lift_features(
    panel: pd.DataFrame,
    channel: str,
    channels: List[str],
    dates: pd.DatetimeIndex,
    media_suffix: str = "_support",
    prefix: str = "qgeo_support",
    pre_periods: int = 4,
    post_periods: int = 4,
    max_events: int = 40,
) -> Dict[str, float]:
    """Return observational geo-shock diagnostics for one channel.

    This is intentionally a feature layer, not a causal lift-test engine. It
    summarizes whether geo-level media movement contains clean, signed,
    donor-comparable variation that a stronger quasi-geo estimator could use.
    """
    media_col = f"{channel}{media_suffix}"
    if "geo_id" not in panel.columns or media_col not in panel.columns or "kpi" not in panel.columns:
        return {f"{prefix}_{name}": 0.0 for name in [
            "available",
            "event_count",
            "clean_event_count",
            "up_event_count",
            "down_event_count",
            "mean_abs_shock_strength",
            "median_abs_shock_strength",
            "positive_effect_share",
            "signed_response_corr",
            "median_abs_lift_to_kpi_sd",
            "other_media_contamination_mean",
            "donor_media_contamination_mean",
            "identifiable_score",
        ]}
    media = _date_geo_matrix(panel, media_col, dates)
    kpi = _date_geo_matrix(panel, "kpi", dates)
    if media.shape[1] < 2 or kpi.shape != media.shape or len(dates) < pre_periods + post_periods + 1:
        return {f"{prefix}_{name}": 0.0 for name in [
            "available",
            "event_count",
            "clean_event_count",
            "up_event_count",
            "down_event_count",
            "mean_abs_shock_strength",
            "median_abs_shock_strength",
            "positive_effect_share",
            "signed_response_corr",
            "median_abs_lift_to_kpi_sd",
            "other_media_contamination_mean",
            "donor_media_contamination_mean",
            "identifiable_score",
        ]}
    finite_media = media[np.isfinite(media)]
    positive_media = finite_media[finite_media > 0]
    media_scale = float(np.nanmedian(positive_media)) if positive_media.size else 0.0
    material = max(1e-8, 0.30 * media_scale)
    if media_scale <= 1e-12:
        return {f"{prefix}_{name}": 0.0 for name in [
            "available",
            "event_count",
            "clean_event_count",
            "up_event_count",
            "down_event_count",
            "mean_abs_shock_strength",
            "median_abs_shock_strength",
            "positive_effect_share",
            "signed_response_corr",
            "median_abs_lift_to_kpi_sd",
            "other_media_contamination_mean",
            "donor_media_contamination_mean",
            "identifiable_score",
        ]}

    other_mats = {
        other: _date_geo_matrix(panel, f"{other}{media_suffix}", dates)
        for other in channels
        if other != channel and f"{other}{media_suffix}" in panel.columns
    }
    event_rows: List[Dict[str, float]] = []
    kpi_sd = float(np.nanstd(kpi))
    for t in range(pre_periods, len(dates) - post_periods + 1):
        media_pre = _window_mean(media, t - pre_periods, t)
        media_post = _window_mean(media, t, t + post_periods)
        media_delta = media_post - media_pre
        kpi_delta = _window_mean(kpi, t, t + post_periods) - _window_mean(kpi, t - pre_periods, t)
        for g in range(media.shape[1]):
            donor_idx = [i for i in range(media.shape[1]) if i != g]
            if not donor_idx:
                continue
            treated_delta = float(media_delta[g])
            donor_delta = float(np.nanmedian(media_delta[donor_idx]))
            net_media_delta = treated_delta - donor_delta
            if not np.isfinite(net_media_delta) or abs(net_media_delta) < material:
                continue
            treated_kpi_delta = float(kpi_delta[g])
            donor_kpi_delta = float(np.nanmedian(kpi_delta[donor_idx]))
            lift = treated_kpi_delta - donor_kpi_delta
            if not np.isfinite(lift):
                continue
            other_contam = 0.0
            for mat in other_mats.values():
                if mat.shape != media.shape:
                    continue
                other_delta = _window_mean(mat, t, t + post_periods) - _window_mean(mat, t - pre_periods, t)
                other_net = float(other_delta[g] - np.nanmedian(other_delta[donor_idx]))
                if np.isfinite(other_net):
                    other_scale = max(float(np.nanmedian(np.abs(other_delta[np.isfinite(other_delta)]))), material)
                    other_contam = max(other_contam, abs(other_net) / max(other_scale, 1e-8))
            donor_contam = abs(donor_delta) / max(abs(treated_delta), media_scale, 1e-8)
            event_rows.append(
                {
                    "net_media_delta": net_media_delta,
                    "shock_strength": abs(net_media_delta) / max(media_scale, 1e-8),
                    "lift": lift,
                    "signed_effect": 1.0 if np.sign(net_media_delta) * np.sign(lift) > 0 else 0.0,
                    "lift_to_kpi_sd": abs(lift) / max(kpi_sd, 1e-8),
                    "other_contam": min(other_contam, 10.0),
                    "donor_contam": min(donor_contam, 10.0),
                }
            )
    if not event_rows:
        return {
            f"{prefix}_available": 1.0,
            f"{prefix}_event_count": 0.0,
            f"{prefix}_clean_event_count": 0.0,
            f"{prefix}_up_event_count": 0.0,
            f"{prefix}_down_event_count": 0.0,
            f"{prefix}_mean_abs_shock_strength": 0.0,
            f"{prefix}_median_abs_shock_strength": 0.0,
            f"{prefix}_positive_effect_share": 0.0,
            f"{prefix}_signed_response_corr": 0.0,
            f"{prefix}_median_abs_lift_to_kpi_sd": 0.0,
            f"{prefix}_other_media_contamination_mean": 0.0,
            f"{prefix}_donor_media_contamination_mean": 0.0,
            f"{prefix}_identifiable_score": 0.0,
        }
    events = pd.DataFrame(event_rows).sort_values("shock_strength", ascending=False).head(int(max_events))
    clean = events[(events["other_contam"] <= 1.0) & (events["donor_contam"] <= 1.0)].copy()
    response_corr = _safe_corr(events["net_media_delta"], events["lift"])
    clean_share = len(clean) / max(len(events), 1)
    sign_share = float(events["signed_effect"].mean())
    strength = float(np.clip(events["shock_strength"].median() / 2.0, 0.0, 1.0))
    identifiable = float(np.clip(0.35 * clean_share + 0.35 * sign_share + 0.30 * strength, 0.0, 1.0))
    return {
        f"{prefix}_available": 1.0,
        f"{prefix}_event_count": float(len(events)),
        f"{prefix}_clean_event_count": float(len(clean)),
        f"{prefix}_up_event_count": float(np.sum(events["net_media_delta"] > 0)),
        f"{prefix}_down_event_count": float(np.sum(events["net_media_delta"] < 0)),
        f"{prefix}_mean_abs_shock_strength": float(events["shock_strength"].mean()),
        f"{prefix}_median_abs_shock_strength": float(events["shock_strength"].median()),
        f"{prefix}_positive_effect_share": sign_share,
        f"{prefix}_signed_response_corr": response_corr,
        f"{prefix}_median_abs_lift_to_kpi_sd": float(events["lift_to_kpi_sd"].median()),
        f"{prefix}_other_media_contamination_mean": float(events["other_contam"].mean()),
        f"{prefix}_donor_media_contamination_mean": float(events["donor_contam"].mean()),
        f"{prefix}_identifiable_score": identifiable,
    }


def _positive_share(values: Iterable[object]) -> float:
    arr = pd.to_numeric(pd.Series(values), errors="coerce").to_numpy(float)
    finite = arr[np.isfinite(arr)]
    return float(np.mean(finite > 0)) if finite.size else 0.0


def _cv_positive(values: Iterable[object]) -> float:
    arr = pd.to_numeric(pd.Series(values), errors="coerce").to_numpy(float)
    pos = arr[np.isfinite(arr) & (arr > 0)]
    if pos.size < 3:
        return 0.0
    return float(np.std(pos) / max(abs(np.mean(pos)), 1e-12))


def _build_set_feature_matrix(
    panel: pd.DataFrame,
    channels: List[str],
    target_channel: str,
    support_series: Dict[str, pd.Series],
    spend_series: Dict[str, pd.Series],
    y: pd.Series,
) -> tuple[np.ndarray, List[str], int]:
    target_support = support_series.get(target_channel, pd.Series(dtype=float))
    rows: List[List[float]] = []
    target_index = 0
    for i, ch in enumerate(channels):
        if ch == target_channel:
            target_index = i
        support = support_series.get(ch, pd.Series(dtype=float))
        spend = spend_series.get(ch, pd.Series(dtype=float))
        rows.append(
            [
                float(ch == target_channel),
                _positive_share(support),
                _cv_positive(support),
                _positive_share(spend),
                _cv_positive(spend),
                _safe_corr(support, spend),
                _safe_corr(support, y),
                _geo_variation(panel, ch, "_support"),
                _geo_variation(panel, ch, "_spend"),
                _safe_corr(support, target_support),
                abs(_safe_corr(support, target_support)),
            ]
        )
    cols = [
        "is_target_channel",
        "support_positive_share",
        "support_cv_positive",
        "spend_positive_share",
        "spend_cv_positive",
        "support_spend_corr",
        "support_kpi_corr",
        "geo_support_variation",
        "geo_spend_variation",
        "corr_with_target_support",
        "abs_corr_with_target_support",
    ]
    return np.asarray(rows, dtype=float), cols, int(target_index)


def _curve_target_with_status(
    response_curves: pd.DataFrame,
    channel: str,
    grid: np.ndarray = CURVE_GRID,
) -> tuple[np.ndarray, float, float, str]:
    curve = response_curves.loc[response_curves["channel"].astype(str).eq(str(channel))].copy()
    if curve.empty:
        return _default_curve_grid(grid), 0.15, 1.0, "missing_curve_rows"
    required = {"pct_of_anchor_support", "true_incremental_contribution"}
    if not required.issubset(curve.columns):
        return _default_curve_grid(grid), 0.15, 1.0, "missing_curve_columns"
    pct = pd.to_numeric(curve["pct_of_anchor_support"], errors="coerce").to_numpy(float)
    y = pd.to_numeric(curve["true_incremental_contribution"], errors="coerce").to_numpy(float)
    ok = np.isfinite(pct) & np.isfinite(y)
    if ok.sum() < 4:
        return _default_curve_grid(grid), 0.25, 1.0, "too_few_curve_points"
    order = np.argsort(pct[ok])
    target = np.interp(grid, pct[ok][order], y[ok][order])
    target = np.maximum(target, 0.0)
    scale = float(np.max(target))
    if scale <= 1e-12:
        return _default_curve_grid(grid), 0.20, 1.0, "zero_or_negative_curve"
    target = target / scale
    target[0] = 0.0
    target = np.maximum.accumulate(target)
    return np.clip(target, 0.0, 1.0), 1.0, 0.0, "ok"


def _curve_target(response_curves: pd.DataFrame, channel: str, grid: np.ndarray = CURVE_GRID) -> np.ndarray:
    return _curve_target_with_status(response_curves, channel, grid=grid)[0]


def _target_saturation(response_curves: pd.DataFrame, channel: str) -> float:
    curve = response_curves.loc[response_curves["channel"].astype(str).eq(str(channel))].copy()
    if curve.empty or "true_saturation" not in curve.columns:
        return 0.50
    curve["_dist"] = (pd.to_numeric(curve["pct_of_anchor_support"], errors="coerce") - 1.0).abs()
    row = curve.sort_values("_dist").head(1)
    return float(np.clip(pd.to_numeric(row["true_saturation"], errors="coerce").iloc[0], 0.0, 1.0))


def _diagnostic_fallback_targets(row: pd.Series) -> tuple[float, float, float]:
    learnability = float(row.get("learnability_score_0_100", 50.0)) / 100.0
    high_collinear = bool(row.get("high_collinearity_flag", False))
    national = bool(row.get("national_repeated_media_flag", False))
    sparse = bool(row.get("sparse_flighting_flag", False))
    always_on = bool(row.get("always_on_flag", False))
    fallback = 1.0 - learnability
    fallback += 0.15 if high_collinear else 0.0
    fallback += 0.10 if national else 0.0
    fallback += 0.10 if sparse else 0.0
    fallback += 0.06 if always_on else 0.0
    fallback = float(np.clip(fallback, 0.05, 0.95))
    confidence = float(np.clip(1.0 - fallback, 0.05, 0.95))
    uncertainty = float(np.clip(0.04 + 0.30 * fallback, 0.05, 0.35))
    return confidence, fallback, uncertainty


def _truth_calibrated_fallback_targets(
    curve_target: np.ndarray,
    diagnostic_fallback: float,
    grid: np.ndarray = CURVE_GRID,
    target_quality: float = 1.0,
) -> tuple[float, float, float, float]:
    """Blend weak-data diagnostics with known-truth default-curve adequacy.

    This is synthetic-training-only. It lets the fallback head learn when a
    conservative default is actually close to truth, while still raising fallback
    weight when the observed data are weak or confounded.
    """
    default_curve = _default_curve_grid(grid)
    default_mae = float(np.mean(np.abs(np.asarray(curve_target, dtype=float) - default_curve)))
    default_adequacy = float(np.clip(1.0 - default_mae / 0.18, 0.0, 1.0))
    diagnostic_fallback = float(np.clip(diagnostic_fallback, 0.05, 0.95))
    target_quality = float(np.clip(target_quality, 0.0, 1.0))
    fallback = 1.0 - (1.0 - diagnostic_fallback) * (1.0 - 0.75 * default_adequacy * target_quality)
    if target_quality < 0.50:
        fallback = max(fallback, 0.80 - 0.30 * target_quality)
    fallback = float(np.clip(fallback, 0.05, 0.95))
    confidence = float(np.clip(1.0 - fallback, 0.05, 0.95))
    uncertainty = float(np.clip(0.04 + 0.24 * diagnostic_fallback + 0.35 * default_mae + 0.12 * (1.0 - target_quality), 0.05, 0.45))
    return confidence, fallback, uncertainty, default_mae


def _default_curve_grid(grid: np.ndarray = CURVE_GRID, anchor_saturation: float = 0.50) -> np.ndarray:
    """Conservative monotone default curve used only for blending weak evidence."""
    grid = np.asarray(grid, dtype=float)
    anchor_saturation = float(np.clip(anchor_saturation, 1e-6, 1.0 - 1e-6))
    scale = (1.0 - anchor_saturation) / anchor_saturation
    raw = grid / (grid + scale + 1e-12)
    denom = max(float(raw[-1]), 1e-12)
    out = np.clip(raw / denom, 0.0, 1.0)
    out[0] = 0.0
    return np.maximum.accumulate(out)


def build_curve_prior_dataset(
    panels: List[Any],
    media_feature_inputs: Optional[List[str]] = None,
    controls: Optional[List[str]] = None,
    grid: np.ndarray = CURVE_GRID,
    sequence_length: int = 104,
    include_residualized_kpi_signal: bool = True,
) -> CurvePriorDataset:
    """Create per-channel curve-prior training examples from synthetic panels."""
    media_feature_inputs = media_feature_inputs or ["support", "spend"]
    rows: List[Dict[str, Any]] = []
    channel_sequences: List[np.ndarray] = []
    geo_sequences: List[np.ndarray] = []
    set_matrices: List[np.ndarray] = []
    target_channel_indices: List[int] = []
    channel_sequence_columns: List[str] = []
    geo_sequence_columns: List[str] = []
    set_feature_columns: List[str] = []
    curve_targets: List[np.ndarray] = []
    adstock_targets: List[float] = []
    saturation_targets: List[float] = []
    confidence_targets: List[float] = []
    fallback_targets: List[float] = []
    uncertainty_targets: List[float] = []
    curve_target_quality: List[float] = []

    for panel_id, synth in enumerate(panels):
        panel = synth.panel.copy()
        dates = _ordered_dates(panel, sequence_length=sequence_length)
        split_group = getattr(synth, "curve_prior_split_group", panel_id)
        data_scenario = getattr(synth, "curve_prior_scenario", "unknown")
        geo_regime = getattr(synth, "curve_prior_geo_regime", "unknown")
        measurement_regime = getattr(synth, "curve_prior_measurement_regime", "unknown")
        kpi_variant = getattr(synth, "curve_prior_kpi_variant", "unknown")
        channels = sorted(synth.truth_params["channel"].astype(str).unique())
        local_controls = controls or [c for c in ["promo", "holiday", "price_index", "competitor_index", "macro_index", "category_trend"] if c in panel.columns]
        validation = validate_nmmm_training_data(
            panel,
            truth_media=synth.truth_media,
            truth_economics=synth.truth_economics,
            channels=channels,
            controls=local_controls,
            media_feature_inputs=media_feature_inputs,
        )
        learn = validation.channel_learnability.set_index("channel") if not validation.channel_learnability.empty else pd.DataFrame()
        support_series = {ch: _global_channel_series(panel, ch, "_support") for ch in channels}
        spend_series = {ch: _global_channel_series(panel, ch, "_spend") for ch in channels}
        y = panel.groupby(pd.to_datetime(panel["date"]))["kpi"].sum(min_count=1).sort_index()
        for ch in channels:
            support = support_series[ch]
            spend = spend_series[ch]
            params = synth.truth_params.loc[synth.truth_params["channel"].astype(str).eq(ch)].head(1)
            channel_seq, channel_cols = _build_channel_sequence(
                panel,
                ch,
                dates,
                media_feature_inputs=media_feature_inputs,
                controls=local_controls,
                include_residualized_kpi_signal=include_residualized_kpi_signal,
            )
            geo_seq, geo_cols = _build_geo_time_sequence(panel, ch, dates)
            channel_seq = _pad_sequence_matrix(channel_seq, sequence_length)
            geo_seq = _pad_sequence_matrix(geo_seq, sequence_length)
            set_matrix, set_cols, target_index = _build_set_feature_matrix(panel, channels, ch, support_series, spend_series, y)
            if not channel_sequence_columns:
                channel_sequence_columns = channel_cols
            if not geo_sequence_columns:
                geo_sequence_columns = geo_cols
            if not set_feature_columns:
                set_feature_columns = set_cols
            feature_row: Dict[str, Any] = {
                "panel_id": panel_id,
                "split_group": str(split_group),
                "data_scenario": str(data_scenario),
                "geo_regime": str(geo_regime),
                "measurement_regime": str(measurement_regime),
                "kpi_variant": str(kpi_variant),
                "channel": ch,
                "truth_curve_type": str(params["curve_type"].iloc[0]) if not params.empty and "curve_type" in params.columns else "unknown",
                "n_weeks": float(panel["date"].nunique()),
                "n_geos": float(panel["geo_id"].nunique()) if "geo_id" in panel.columns else 1.0,
                "has_geo_sales": float(panel["geo_id"].nunique() > 1) if "geo_id" in panel.columns else 0.0,
                "has_population": float("population" in panel.columns and pd.to_numeric(panel.get("population"), errors="coerce").notna().any()),
                "control_count": float(len(local_controls)),
                "has_controls": float(len(local_controls) > 0),
                "geo_support_variation": _geo_variation(panel, ch, "_support"),
                "geo_spend_variation": _geo_variation(panel, ch, "_spend"),
                "support_spend_corr": _safe_corr(support, spend),
                "support_kpi_corr": _safe_corr(support, y),
                "spend_kpi_corr": _safe_corr(spend, y),
            }
            feature_row.update(_stats(support, "support"))
            feature_row.update(_stats(spend, "spend"))
            feature_row.update(_quasi_geo_lift_features(panel, ch, channels, dates, media_suffix="_support", prefix="qgeo_support"))
            feature_row.update(_quasi_geo_lift_features(panel, ch, channels, dates, media_suffix="_spend", prefix="qgeo_spend"))
            other_corrs = []
            for other, other_support in support_series.items():
                if other != ch:
                    other_corrs.append(abs(_safe_corr(support, other_support)))
            feature_row["max_abs_other_channel_support_corr"] = float(np.nanmax(other_corrs)) if other_corrs else 0.0
            feature_row["mean_abs_other_channel_support_corr"] = float(np.nanmean(other_corrs)) if other_corrs else 0.0
            curve_target, target_quality, target_defaulted, target_reason = _curve_target_with_status(synth.response_curves, ch, grid=grid)
            if ch in learn.index:
                for col, val in learn.loc[ch].items():
                    if isinstance(val, (bool, np.bool_)):
                        feature_row[col] = float(val)
                    elif isinstance(val, str):
                        continue
                    else:
                        feature_row[col] = float(val) if pd.notna(val) else 0.0
                _, diagnostic_fallback, _ = _diagnostic_fallback_targets(learn.loc[ch])
            else:
                diagnostic_fallback = 0.50
            conf, fallback, uncertainty, default_curve_mae = _truth_calibrated_fallback_targets(
                curve_target,
                diagnostic_fallback=diagnostic_fallback,
                grid=grid,
                target_quality=target_quality,
            )
            feature_row["diagnostic_default_weight_target"] = diagnostic_fallback
            feature_row["truth_default_curve_mae"] = default_curve_mae
            feature_row["curve_target_quality"] = target_quality
            feature_row["curve_target_defaulted"] = target_defaulted
            feature_row["curve_target_default_reason"] = str(target_reason)
            adstock = float(params["decay"].iloc[0]) if not params.empty and "decay" in params.columns else 0.30
            rows.append(feature_row)
            channel_sequences.append(channel_seq)
            geo_sequences.append(geo_seq)
            set_matrices.append(set_matrix)
            target_channel_indices.append(target_index)
            curve_targets.append(curve_target)
            adstock_targets.append(float(np.clip(adstock, 0.0, 0.95)))
            saturation_targets.append(_target_saturation(synth.response_curves, ch))
            confidence_targets.append(conf)
            fallback_targets.append(fallback)
            uncertainty_targets.append(uncertainty)
            curve_target_quality.append(target_quality)

    features = pd.DataFrame(rows)
    excluded = {
        "panel_id",
        "split_group",
        "data_scenario",
        "geo_regime",
        "measurement_regime",
        "kpi_variant",
        "channel",
        "truth_curve_type",
        "diagnostic_default_weight_target",
        "truth_default_curve_mae",
        "curve_target_quality",
        "curve_target_defaulted",
        "curve_target_default_reason",
    }
    feature_columns = [c for c in features.columns if c not in excluded and pd.api.types.is_numeric_dtype(features[c])]
    features[feature_columns] = features[feature_columns].replace([np.inf, -np.inf], np.nan).fillna(0.0)
    max_channels = max((m.shape[0] for m in set_matrices), default=0)
    set_tensor = np.zeros((len(set_matrices), max_channels, len(set_feature_columns)), dtype=float)
    set_mask = np.zeros((len(set_matrices), max_channels), dtype=float)
    for i, matrix in enumerate(set_matrices):
        n_channels = matrix.shape[0]
        set_tensor[i, :n_channels, :] = np.nan_to_num(matrix, nan=0.0, posinf=0.0, neginf=0.0)
        set_mask[i, :n_channels] = 1.0
    return CurvePriorDataset(
        features=features,
        feature_columns=feature_columns,
        channel_sequence_columns=channel_sequence_columns,
        geo_sequence_columns=geo_sequence_columns,
        set_feature_columns=set_feature_columns,
        curve_targets=np.vstack(curve_targets).astype(float),
        adstock_targets=np.asarray(adstock_targets, dtype=float),
        saturation_targets=np.asarray(saturation_targets, dtype=float),
        confidence_targets=np.asarray(confidence_targets, dtype=float),
        fallback_targets=np.asarray(fallback_targets, dtype=float),
        uncertainty_targets=np.asarray(uncertainty_targets, dtype=float),
        curve_target_quality=np.asarray(curve_target_quality, dtype=float),
        channel_sequences=np.nan_to_num(np.stack(channel_sequences), nan=0.0, posinf=0.0, neginf=0.0) if channel_sequences else None,
        geo_sequences=np.nan_to_num(np.stack(geo_sequences), nan=0.0, posinf=0.0, neginf=0.0) if geo_sequences else None,
        set_features=set_tensor if len(set_matrices) else None,
        set_channel_mask=set_mask if len(set_matrices) else None,
        target_channel_index=np.asarray(target_channel_indices, dtype=int) if target_channel_indices else None,
    )


def build_monotone_curve_prior_net(
    input_dim: int,
    curve_points: int = len(CURVE_GRID),
    hidden_size: int = 96,
    dropout: float = 0.05,
    channel_sequence_dim: int = 0,
    geo_sequence_dim: int = 0,
    set_feature_dim: int = 0,
):
    """Create the neural curve-prior model."""
    torch, nn, F = _require_torch_nn()

    class MonotoneCurvePriorNet(nn.Module):
        def __init__(self):
            super().__init__()
            self.latent_dim = max(16, hidden_size // 2)
            self.has_channel_sequence = channel_sequence_dim > 0
            self.has_geo_sequence = geo_sequence_dim > 0
            self.has_set_features = set_feature_dim > 0
            self.aggregate_backbone = nn.Sequential(
                nn.Linear(input_dim, hidden_size),
                nn.LayerNorm(hidden_size),
                nn.SiLU(),
                nn.Dropout(dropout),
                nn.Linear(hidden_size, hidden_size),
                nn.LayerNorm(hidden_size),
                nn.SiLU(),
                nn.Dropout(dropout),
            )
            if self.has_channel_sequence:
                self.channel_temporal_stem = nn.Sequential(
                    nn.Conv1d(channel_sequence_dim, self.latent_dim, kernel_size=5, padding=2),
                    nn.SiLU(),
                    nn.Dropout(dropout),
                    nn.Conv1d(self.latent_dim, self.latent_dim, kernel_size=3, padding=1),
                    nn.SiLU(),
                )
            if self.has_geo_sequence:
                self.geo_temporal_stem = nn.Sequential(
                    nn.Conv1d(geo_sequence_dim, self.latent_dim, kernel_size=5, padding=2),
                    nn.SiLU(),
                    nn.Dropout(dropout),
                    nn.Conv1d(self.latent_dim, self.latent_dim, kernel_size=3, padding=1),
                    nn.SiLU(),
                )
            if self.has_set_features:
                self.set_projection = nn.Linear(set_feature_dim, self.latent_dim)
                encoder_layer = nn.TransformerEncoderLayer(
                    d_model=self.latent_dim,
                    nhead=4,
                    dim_feedforward=max(hidden_size, self.latent_dim * 2),
                    dropout=dropout,
                    batch_first=True,
                )
                self.set_encoder = nn.TransformerEncoder(encoder_layer, num_layers=1)

            fusion_dim = hidden_size
            fusion_dim += self.latent_dim if self.has_channel_sequence else 0
            fusion_dim += self.latent_dim if self.has_geo_sequence else 0
            fusion_dim += self.latent_dim * 2 if self.has_set_features else 0
            self.fusion = nn.Sequential(
                nn.Linear(fusion_dim, hidden_size),
                nn.LayerNorm(hidden_size),
                nn.SiLU(),
                nn.Dropout(dropout),
            )
            self.increment_head = nn.Linear(hidden_size, curve_points - 1)
            self.adstock_head = nn.Linear(hidden_size, 1)
            self.saturation_head = nn.Linear(hidden_size, 1)
            self.confidence_head = nn.Linear(hidden_size, 1)
            self.fallback_head = nn.Linear(hidden_size, 1)
            self.uncertainty_head = nn.Linear(hidden_size, 1)

        def _encode_temporal(self, stem, sequence):
            encoded = stem(sequence.transpose(1, 2))
            return encoded.mean(dim=-1)

        def _encode_set(self, set_features, set_channel_mask, target_channel_index):
            set_h = self.set_projection(set_features)
            padding_mask = set_channel_mask <= 0 if set_channel_mask is not None else None
            encoded = self.set_encoder(set_h, src_key_padding_mask=padding_mask)
            if target_channel_index is None:
                target_channel_index = torch.zeros(encoded.shape[0], dtype=torch.long, device=encoded.device)
            gather_rows = torch.arange(encoded.shape[0], device=encoded.device)
            target_h = encoded[gather_rows, target_channel_index]
            if set_channel_mask is None:
                pooled_h = encoded.mean(dim=1)
            else:
                weights = set_channel_mask.unsqueeze(-1).to(encoded.dtype)
                pooled_h = torch.sum(encoded * weights, dim=1) / torch.clamp(torch.sum(weights, dim=1), min=1.0)
            return torch.cat([target_h, pooled_h], dim=-1)

        def forward(self, x, channel_sequence=None, geo_sequence=None, set_features=None, set_channel_mask=None, target_channel_index=None):
            pieces = [self.aggregate_backbone(x)]
            if self.has_channel_sequence and channel_sequence is not None:
                pieces.append(self._encode_temporal(self.channel_temporal_stem, channel_sequence))
            if self.has_geo_sequence and geo_sequence is not None:
                pieces.append(self._encode_temporal(self.geo_temporal_stem, geo_sequence))
            if self.has_set_features and set_features is not None:
                pieces.append(self._encode_set(set_features, set_channel_mask, target_channel_index))
            h = self.fusion(torch.cat(pieces, dim=-1))
            increments = F.softplus(self.increment_head(h)) + 1e-6
            curve_body = torch.cumsum(increments, dim=-1)
            curve_body = curve_body / torch.clamp(curve_body[:, -1:], min=1e-6)
            zeros = torch.zeros((curve_body.shape[0], 1), dtype=curve_body.dtype, device=curve_body.device)
            curve = torch.cat([zeros, curve_body], dim=-1)
            return {
                "curve": curve,
                "adstock_decay": 0.95 * torch.sigmoid(self.adstock_head(h)).squeeze(-1),
                "saturation_score": torch.sigmoid(self.saturation_head(h)).squeeze(-1),
                "confidence": torch.sigmoid(self.confidence_head(h)).squeeze(-1),
                "fallback_weight": torch.sigmoid(self.fallback_head(h)).squeeze(-1),
                "uncertainty_width": F.softplus(self.uncertainty_head(h)).squeeze(-1),
            }

    return MonotoneCurvePriorNet()


def fit_curve_prior_model(
    dataset: CurvePriorDataset,
    validation_fraction: float = 0.20,
    epochs: int = 350,
    learning_rate: float = 0.003,
    weight_decay: float = 1e-4,
    hidden_size: int = 96,
    dropout: float = 0.05,
    seed: int = 20260604,
    split_group_col: str = "split_group",
    concavity_penalty_weight: float = 0.25,
) -> CurvePriorResult:
    """Fit a monotone neural curve-prior model on synthetic known truth."""
    torch, nn, F = _require_torch_nn()
    torch.manual_seed(seed)
    rng = np.random.default_rng(seed)
    raw_x = dataset.features[dataset.feature_columns].to_numpy(float)
    n = raw_x.shape[0]
    idx = np.arange(n)
    validation_strategy = "row_random"
    if split_group_col in dataset.features.columns and dataset.features[split_group_col].nunique(dropna=False) > 1:
        groups = pd.Series(dataset.features[split_group_col].astype(str).unique())
        group_idx = np.arange(len(groups))
        rng.shuffle(group_idx)
        n_val_groups = max(1, int(round(len(groups) * validation_fraction)))
        val_groups = set(groups.iloc[group_idx[:n_val_groups]].astype(str))
        val_mask = dataset.features[split_group_col].astype(str).isin(val_groups).to_numpy()
        val_idx = idx[val_mask]
        train_idx = idx[~val_mask]
        if len(train_idx) == 0:
            train_idx = idx
            val_idx = idx[:0]
        validation_strategy = f"grouped_by_{split_group_col}"
    else:
        rng.shuffle(idx)
        n_val = max(1, int(round(n * validation_fraction))) if n >= 8 else max(0, n // 5)
        val_idx = idx[:n_val]
        train_idx = idx[n_val:] if n_val else idx
    x_center = np.mean(raw_x[train_idx], axis=0)
    x_scale = np.std(raw_x[train_idx], axis=0)
    x_scale = np.where(x_scale > 1e-8, x_scale, 1.0)
    x_np = (raw_x - x_center) / x_scale

    device = torch.device("cpu")
    model = build_monotone_curve_prior_net(
        input_dim=len(dataset.feature_columns),
        curve_points=dataset.curve_targets.shape[1],
        hidden_size=hidden_size,
        dropout=dropout,
        channel_sequence_dim=dataset.channel_sequences.shape[-1] if dataset.channel_sequences is not None else 0,
        geo_sequence_dim=dataset.geo_sequences.shape[-1] if dataset.geo_sequences is not None else 0,
        set_feature_dim=dataset.set_features.shape[-1] if dataset.set_features is not None else 0,
    ).to(device)
    optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate, weight_decay=weight_decay)
    x = torch.tensor(x_np, dtype=torch.float32, device=device)
    channel_sequence_t = (
        torch.tensor(dataset.channel_sequences, dtype=torch.float32, device=device) if dataset.channel_sequences is not None else None
    )
    geo_sequence_t = torch.tensor(dataset.geo_sequences, dtype=torch.float32, device=device) if dataset.geo_sequences is not None else None
    set_features_t = torch.tensor(dataset.set_features, dtype=torch.float32, device=device) if dataset.set_features is not None else None
    set_channel_mask_t = torch.tensor(dataset.set_channel_mask, dtype=torch.float32, device=device) if dataset.set_channel_mask is not None else None
    target_channel_index_t = (
        torch.tensor(dataset.target_channel_index, dtype=torch.long, device=device) if dataset.target_channel_index is not None else None
    )
    curve_y = torch.tensor(dataset.curve_targets, dtype=torch.float32, device=device)
    adstock_y = torch.tensor(dataset.adstock_targets, dtype=torch.float32, device=device)
    saturation_y = torch.tensor(dataset.saturation_targets, dtype=torch.float32, device=device)
    confidence_y = torch.tensor(dataset.confidence_targets, dtype=torch.float32, device=device)
    fallback_y = torch.tensor(dataset.fallback_targets, dtype=torch.float32, device=device)
    uncertainty_y = torch.tensor(dataset.uncertainty_targets, dtype=torch.float32, device=device)
    curve_quality_y = torch.tensor(
        dataset.curve_target_quality if dataset.curve_target_quality is not None else np.ones(n, dtype=float),
        dtype=torch.float32,
        device=device,
    )
    grid_delta_t = torch.tensor(np.diff(CURVE_GRID), dtype=torch.float32, device=device).reshape(1, -1)
    train_t = torch.tensor(train_idx, dtype=torch.long, device=device)
    val_t = torch.tensor(val_idx, dtype=torch.long, device=device) if len(val_idx) else train_t
    history = []
    best_state = None
    best_val = float("inf")
    patience = 80
    stale = 0

    def loss_for(indices):
        out = model(
            x[indices],
            channel_sequence=channel_sequence_t[indices] if channel_sequence_t is not None else None,
            geo_sequence=geo_sequence_t[indices] if geo_sequence_t is not None else None,
            set_features=set_features_t[indices] if set_features_t is not None else None,
            set_channel_mask=set_channel_mask_t[indices] if set_channel_mask_t is not None else None,
            target_channel_index=target_channel_index_t[indices] if target_channel_index_t is not None else None,
        )
        pred_marginal = out["curve"][:, 1:] - out["curve"][:, :-1]
        true_marginal = curve_y[indices][:, 1:] - curve_y[indices][:, :-1]
        pred_slope = pred_marginal / torch.clamp(grid_delta_t, min=1e-6)
        quality = torch.clamp(curve_quality_y[indices], min=0.05, max=1.0)
        curve_loss = torch.mean(torch.mean((out["curve"] - curve_y[indices]) ** 2, dim=1) * quality)
        marginal_loss = torch.mean(torch.mean((pred_marginal - true_marginal) ** 2, dim=1) * quality)
        scalar_loss = (
            F.mse_loss(out["adstock_decay"], adstock_y[indices])
            + F.mse_loss(out["saturation_score"], saturation_y[indices])
            + F.mse_loss(out["confidence"], confidence_y[indices])
            + F.mse_loss(out["fallback_weight"], fallback_y[indices])
            + F.mse_loss(out["uncertainty_width"], uncertainty_y[indices])
        )
        smoothness = torch.mean((pred_slope[:, 1:] - pred_slope[:, :-1]) ** 2)
        concavity_violation = torch.relu(pred_slope[:, 1:] - pred_slope[:, :-1])
        concavity_loss = torch.mean(concavity_violation**2)
        return curve_loss + 0.50 * marginal_loss + 0.25 * scalar_loss + 0.02 * smoothness + float(concavity_penalty_weight) * concavity_loss

    for epoch in range(1, int(epochs) + 1):
        model.train()
        optimizer.zero_grad()
        train_loss = loss_for(train_t)
        train_loss.backward()
        optimizer.step()
        model.eval()
        with torch.no_grad():
            val_loss = loss_for(val_t)
        history.append({"epoch": epoch, "train_loss": float(train_loss), "validation_loss": float(val_loss)})
        if float(val_loss) < best_val - 1e-6:
            best_val = float(val_loss)
            best_state = {k: v.detach().cpu().clone() for k, v in model.state_dict().items()}
            stale = 0
        else:
            stale += 1
        if stale >= patience:
            break
    if best_state is not None:
        model.load_state_dict(best_state)

    model.eval()
    with torch.no_grad():
        out = model(
            x,
            channel_sequence=channel_sequence_t,
            geo_sequence=geo_sequence_t,
            set_features=set_features_t,
            set_channel_mask=set_channel_mask_t,
            target_channel_index=target_channel_index_t,
        )
    metadata_cols = [
        c
        for c in [
            "panel_id",
            "split_group",
            "data_scenario",
            "geo_regime",
            "measurement_regime",
            "kpi_variant",
            "channel",
            "truth_curve_type",
            "diagnostic_default_weight_target",
            "truth_default_curve_mae",
            "curve_target_quality",
            "curve_target_defaulted",
            "curve_target_default_reason",
        ]
        if c in dataset.features.columns
    ]
    pred = dataset.features[metadata_cols].copy()
    split_role = np.full(n, "train", dtype=object)
    split_role[val_idx] = "validation"
    pred["split_role"] = split_role
    curve_pred = np.asarray(out["curve"].detach().cpu().tolist(), dtype=float)
    fallback_pred = np.asarray(out["fallback_weight"].detach().cpu().tolist(), dtype=float)
    default_curve = _default_curve_grid(CURVE_GRID)
    blended_curve = (1.0 - fallback_pred.reshape(-1, 1)) * curve_pred + fallback_pred.reshape(-1, 1) * default_curve.reshape(1, -1)
    for i, pct in enumerate(CURVE_GRID):
        pred[f"curve_prior_p{int(round(pct * 100)):03d}"] = curve_pred[:, i]
        pred[f"default_curve_p{int(round(pct * 100)):03d}"] = default_curve[i]
        pred[f"conservative_blend_curve_p{int(round(pct * 100)):03d}"] = blended_curve[:, i]
    pred["adstock_decay_prior_mean"] = np.asarray(out["adstock_decay"].detach().cpu().tolist(), dtype=float)
    pred["saturation_score_prior_mean"] = np.asarray(out["saturation_score"].detach().cpu().tolist(), dtype=float)
    pred["confidence_score"] = np.asarray(out["confidence"].detach().cpu().tolist(), dtype=float)
    pred["fallback_default_weight"] = fallback_pred
    pred["uncertainty_width"] = np.asarray(out["uncertainty_width"].detach().cpu().tolist(), dtype=float)
    pred["true_adstock_decay"] = dataset.adstock_targets
    pred["true_saturation_score"] = dataset.saturation_targets
    pred["true_confidence_score"] = dataset.confidence_targets
    pred["true_fallback_default_weight"] = dataset.fallback_targets
    pred["true_uncertainty_width"] = dataset.uncertainty_targets
    for i, pct in enumerate(CURVE_GRID):
        pred[f"true_curve_p{int(round(pct * 100)):03d}"] = dataset.curve_targets[:, i]

    return CurvePriorResult(
        predictions=pred,
        training_history=pd.DataFrame(history),
        feature_columns=dataset.feature_columns,
        settings={
            "curve_grid": CURVE_GRID.tolist(),
            "hidden_size": hidden_size,
            "dropout": dropout,
            "concavity_penalty_weight": float(concavity_penalty_weight),
            "epochs_requested": int(epochs),
            "epochs_run": int(len(history)),
            "validation_fraction": validation_fraction,
            "validation_strategy": validation_strategy,
            "validation_row_n": int(len(val_idx)),
            "train_row_n": int(len(train_idx)),
            "feature_center": x_center.tolist(),
            "feature_scale": x_scale.tolist(),
            "model_type": "monotone_neural_curve_prior_net",
            "architecture": "aggregate_mlp_plus_channel_tcn_plus_geo_tcn_plus_set_transformer",
            "loss_notes": "curve and marginal losses are down-weighted for defaulted/low-quality curve targets; positive second differences are penalized as conservative concavity pressure",
            "channel_sequence_columns": dataset.channel_sequence_columns,
            "geo_sequence_columns": dataset.geo_sequence_columns,
            "set_feature_columns": dataset.set_feature_columns,
            "scope": "curve/adstock prior builder, not final causal ROI model",
        },
        model_state={
            "state_dict": model.state_dict(),
            "feature_columns": dataset.feature_columns,
            "channel_sequence_columns": dataset.channel_sequence_columns,
            "geo_sequence_columns": dataset.geo_sequence_columns,
            "set_feature_columns": dataset.set_feature_columns,
        },
    )


def predict_curve_prior_model(result: CurvePriorResult, dataset: CurvePriorDataset) -> CurvePriorResult:
    """Score a new curve-prior dataset with a fitted model checkpoint."""
    if not result.model_state or "state_dict" not in result.model_state:
        raise ValueError("result.model_state with a fitted state_dict is required for prediction.")
    torch, _, _ = _require_torch_nn()
    feature_columns = list(result.model_state.get("feature_columns") or result.feature_columns)
    missing_features = [c for c in feature_columns if c not in dataset.features.columns]
    raw_x = dataset.features.reindex(columns=feature_columns, fill_value=0.0).to_numpy(float)
    center = np.asarray(result.settings.get("feature_center", np.zeros(len(feature_columns))), dtype=float)
    scale = np.asarray(result.settings.get("feature_scale", np.ones(len(feature_columns))), dtype=float)
    if center.shape[0] != len(feature_columns) or scale.shape[0] != len(feature_columns):
        raise ValueError("Stored feature center/scale do not match fitted feature columns.")
    scale = np.where(scale > 1e-8, scale, 1.0)
    x_np = (raw_x - center) / scale
    channel_cols = list(result.model_state.get("channel_sequence_columns") or [])
    geo_cols = list(result.model_state.get("geo_sequence_columns") or [])
    set_cols = list(result.model_state.get("set_feature_columns") or [])
    if dataset.channel_sequences is not None and dataset.channel_sequences.shape[-1] != len(channel_cols):
        raise ValueError("Holdout channel sequence columns do not match the fitted model.")
    if dataset.geo_sequences is not None and dataset.geo_sequences.shape[-1] != len(geo_cols):
        raise ValueError("Holdout geo sequence columns do not match the fitted model.")
    if dataset.set_features is not None and dataset.set_features.shape[-1] != len(set_cols):
        raise ValueError("Holdout set-feature columns do not match the fitted model.")

    device = torch.device("cpu")
    model = build_monotone_curve_prior_net(
        input_dim=len(feature_columns),
        curve_points=dataset.curve_targets.shape[1],
        hidden_size=int(result.settings.get("hidden_size", 96)),
        dropout=float(result.settings.get("dropout", 0.05)),
        channel_sequence_dim=len(channel_cols),
        geo_sequence_dim=len(geo_cols),
        set_feature_dim=len(set_cols),
    ).to(device)
    model.load_state_dict(result.model_state["state_dict"])
    model.eval()
    x = torch.tensor(x_np, dtype=torch.float32, device=device)
    channel_sequence_t = (
        torch.tensor(dataset.channel_sequences, dtype=torch.float32, device=device) if dataset.channel_sequences is not None else None
    )
    geo_sequence_t = torch.tensor(dataset.geo_sequences, dtype=torch.float32, device=device) if dataset.geo_sequences is not None else None
    set_features_t = torch.tensor(dataset.set_features, dtype=torch.float32, device=device) if dataset.set_features is not None else None
    set_channel_mask_t = torch.tensor(dataset.set_channel_mask, dtype=torch.float32, device=device) if dataset.set_channel_mask is not None else None
    target_channel_index_t = (
        torch.tensor(dataset.target_channel_index, dtype=torch.long, device=device) if dataset.target_channel_index is not None else None
    )
    with torch.no_grad():
        out = model(
            x,
            channel_sequence=channel_sequence_t,
            geo_sequence=geo_sequence_t,
            set_features=set_features_t,
            set_channel_mask=set_channel_mask_t,
            target_channel_index=target_channel_index_t,
        )
    metadata_cols = [
        c
        for c in [
            "panel_id",
            "split_group",
            "data_scenario",
            "geo_regime",
            "measurement_regime",
            "kpi_variant",
            "channel",
            "truth_curve_type",
            "diagnostic_default_weight_target",
            "truth_default_curve_mae",
            "curve_target_quality",
            "curve_target_defaulted",
            "curve_target_default_reason",
        ]
        if c in dataset.features.columns
    ]
    pred = dataset.features[metadata_cols].copy()
    pred["split_role"] = "holdout_prediction"
    curve_pred = np.asarray(out["curve"].detach().cpu().tolist(), dtype=float)
    fallback_pred = np.asarray(out["fallback_weight"].detach().cpu().tolist(), dtype=float)
    default_curve = _default_curve_grid(CURVE_GRID)
    blended_curve = (1.0 - fallback_pred.reshape(-1, 1)) * curve_pred + fallback_pred.reshape(-1, 1) * default_curve.reshape(1, -1)
    for i, pct in enumerate(CURVE_GRID):
        pred[f"curve_prior_p{int(round(pct * 100)):03d}"] = curve_pred[:, i]
        pred[f"default_curve_p{int(round(pct * 100)):03d}"] = default_curve[i]
        pred[f"conservative_blend_curve_p{int(round(pct * 100)):03d}"] = blended_curve[:, i]
    pred["adstock_decay_prior_mean"] = np.asarray(out["adstock_decay"].detach().cpu().tolist(), dtype=float)
    pred["saturation_score_prior_mean"] = np.asarray(out["saturation_score"].detach().cpu().tolist(), dtype=float)
    pred["confidence_score"] = np.asarray(out["confidence"].detach().cpu().tolist(), dtype=float)
    pred["fallback_default_weight"] = fallback_pred
    pred["uncertainty_width"] = np.asarray(out["uncertainty_width"].detach().cpu().tolist(), dtype=float)
    pred["true_adstock_decay"] = dataset.adstock_targets
    pred["true_saturation_score"] = dataset.saturation_targets
    pred["true_confidence_score"] = dataset.confidence_targets
    pred["true_fallback_default_weight"] = dataset.fallback_targets
    pred["true_uncertainty_width"] = dataset.uncertainty_targets
    for i, pct in enumerate(CURVE_GRID):
        pred[f"true_curve_p{int(round(pct * 100)):03d}"] = dataset.curve_targets[:, i]
    settings = dict(result.settings)
    settings["prediction_missing_feature_columns"] = missing_features
    settings["prediction_row_n"] = int(len(pred))
    return CurvePriorResult(
        predictions=pred,
        training_history=result.training_history.copy(),
        feature_columns=feature_columns,
        settings=settings,
        model_state=result.model_state,
    )


def evaluate_curve_prior_predictions(result: CurvePriorResult) -> pd.DataFrame:
    pred = result.predictions
    curve_cols = [c for c in pred.columns if c.startswith("curve_prior_p")]
    conservative_blend_curve_cols = [c for c in pred.columns if c.startswith("conservative_blend_curve_p")]
    true_cols = [c for c in pred.columns if c.startswith("true_curve_p")]
    rows: List[Dict[str, Any]] = []

    def add_curve_metrics(sub: pd.DataFrame, scope: str, est_cols: List[str], prefix: str) -> None:
        if not est_cols or not true_cols:
            return
        est = sub[est_cols].to_numpy(float)
        truth = sub[true_cols].to_numpy(float)
        mae = np.mean(np.abs(est - truth), axis=1)
        marginal_slope = np.diff(est, axis=1) / np.maximum(np.diff(CURVE_GRID).reshape(1, -1), 1e-8)
        concavity = np.diff(marginal_slope, axis=1)
        corr = []
        for e, t in zip(est, truth):
            corr.append(np.corrcoef(e, t)[0, 1] if np.std(e) > 1e-10 and np.std(t) > 1e-10 else np.nan)
        rows.extend(
            [
                {"scope": scope, "metric": f"{prefix}_curve_grid_mae_mean", "value": float(np.nanmean(mae))},
                {"scope": scope, "metric": f"{prefix}_curve_grid_mae_median", "value": float(np.nanmedian(mae))},
                {"scope": scope, "metric": f"{prefix}_curve_shape_corr_median", "value": float(np.nanmedian(corr))},
                {"scope": scope, "metric": f"{prefix}_curve_monotonic_violation_share", "value": float(np.mean(np.diff(est, axis=1) < -1e-8))},
                {"scope": scope, "metric": f"{prefix}_curve_concavity_violation_share", "value": float(np.mean(concavity > 1e-8))},
            ]
        )

    def add_metrics(sub: pd.DataFrame, scope: str) -> None:
        if sub.empty:
            return
        rows.append({"scope": scope, "metric": "row_n", "value": float(len(sub))})
        add_curve_metrics(sub, scope, curve_cols, "model")
        add_curve_metrics(sub, scope, conservative_blend_curve_cols, "conservative_blend")
        for est_col, true_col, metric in [
            ("adstock_decay_prior_mean", "true_adstock_decay", "adstock_decay_mae"),
            ("saturation_score_prior_mean", "true_saturation_score", "saturation_score_mae"),
            ("confidence_score", "true_confidence_score", "confidence_score_mae"),
            ("fallback_default_weight", "true_fallback_default_weight", "fallback_weight_mae"),
            ("uncertainty_width", "true_uncertainty_width", "uncertainty_width_mae"),
        ]:
            if est_col in sub.columns and true_col in sub.columns:
                rows.append({"scope": scope, "metric": metric, "value": float(np.mean(np.abs(sub[est_col] - sub[true_col])))})

    add_metrics(pred, "all")
    if "split_role" in pred.columns:
        for split_role, sub in pred.groupby("split_role", dropna=False):
            add_metrics(sub, f"split:{split_role}")
    if "truth_curve_type" in pred.columns:
        for curve_type, sub in pred.groupby("truth_curve_type", dropna=False):
            add_metrics(sub, f"truth_curve_type:{curve_type}")
    if "data_scenario" in pred.columns:
        for scenario, sub in pred.groupby("data_scenario", dropna=False):
            add_metrics(sub, f"scenario:{scenario}")
    for group_col, label in [
        ("geo_regime", "geo_regime"),
        ("measurement_regime", "measurement_regime"),
        ("kpi_variant", "kpi_variant"),
    ]:
        if group_col in pred.columns:
            for value, sub in pred.groupby(group_col, dropna=False):
                add_metrics(sub, f"{label}:{value}")

    return pd.DataFrame(rows)


def save_curve_prior_model(result: CurvePriorResult, path: str | Path, extra: Optional[Dict[str, Any]] = None) -> None:
    torch, _, _ = _require_torch_nn()
    payload = {
        "model_state": result.model_state,
        "settings": result.settings,
        "feature_columns": result.feature_columns,
        "extra": extra or {},
    }
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    torch.save(payload, path)
    with open(path.with_suffix(".json"), "w", encoding="utf-8") as f:
        json.dump({k: v for k, v in payload.items() if k != "model_state"}, f, indent=2, sort_keys=True)
