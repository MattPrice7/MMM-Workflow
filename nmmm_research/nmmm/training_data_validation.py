"""Training-data validation for NMMM experiments.

These checks are not model metrics. They answer whether the data panel is a
reasonable source of causal-ish MMM training/evaluation signal before a neural
model is allowed to learn from it.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional

import numpy as np
import pandas as pd


@dataclass
class TrainingDataValidation:
    summary: pd.DataFrame
    channel_learnability: pd.DataFrame
    cost_efficiency_drift: pd.DataFrame
    spend_support_decoupling: pd.DataFrame
    leakage_flags: pd.DataFrame
    label_consistency: pd.DataFrame


def _finite_numeric(x: Iterable[object]) -> np.ndarray:
    arr = pd.to_numeric(pd.Series(x), errors="coerce").to_numpy(float)
    return arr[np.isfinite(arr)]


def _safe_corr(x: Iterable[object], y: Iterable[object]) -> float:
    x_arr = pd.to_numeric(pd.Series(x), errors="coerce").to_numpy(float)
    y_arr = pd.to_numeric(pd.Series(y), errors="coerce").to_numpy(float)
    ok = np.isfinite(x_arr) & np.isfinite(y_arr)
    if ok.sum() < 4 or np.nanstd(x_arr[ok]) <= 1e-12 or np.nanstd(y_arr[ok]) <= 1e-12:
        return np.nan
    return float(np.corrcoef(x_arr[ok], y_arr[ok])[0, 1])


def _cv_positive(x: Iterable[object]) -> float:
    arr = _finite_numeric(x)
    arr = arr[arr > 0]
    if arr.size < 3:
        return np.nan
    return float(np.nanstd(arr) / max(abs(np.nanmean(arr)), 1e-12))


def _infer_channels(panel: pd.DataFrame, support_suffix: str, spend_suffix: str) -> List[str]:
    suffixes = [support_suffix, spend_suffix, "_impressions", "_clicks", "_grps", "_reach", "_frequency"]
    channels = set()
    for col in panel.columns:
        for suffix in suffixes:
            if str(col).endswith(suffix):
                channels.add(str(col)[: -len(suffix)])
    return sorted(channels)


def _aggregate_channel(panel: pd.DataFrame, channel: str, date_col: str, support_suffix: str, spend_suffix: str) -> pd.DataFrame:
    support_col = f"{channel}{support_suffix}"
    spend_col = f"{channel}{spend_suffix}"
    cols = [date_col]
    if support_col in panel.columns:
        cols.append(support_col)
    if spend_col in panel.columns:
        cols.append(spend_col)
    if len(cols) == 1:
        return pd.DataFrame()
    agg = panel[cols].copy()
    agg[date_col] = pd.to_datetime(agg[date_col])
    for col in cols:
        if col != date_col:
            agg[col] = pd.to_numeric(agg[col], errors="coerce")
    return agg.groupby(date_col, as_index=False).sum(min_count=1)


def _geo_variation_ratio(panel: pd.DataFrame, value_col: str, date_col: str, group_col: str) -> float:
    if value_col not in panel.columns or group_col not in panel.columns:
        return np.nan
    wide = panel[[date_col, group_col, value_col]].copy()
    wide[date_col] = pd.to_datetime(wide[date_col])
    wide[value_col] = pd.to_numeric(wide[value_col], errors="coerce")
    pivot = wide.pivot_table(index=date_col, columns=group_col, values=value_col, aggfunc="sum")
    if pivot.shape[1] < 2:
        return 0.0
    arr = pivot.to_numpy(float)
    finite = np.isfinite(arr)
    if finite.sum() < 6:
        return np.nan
    across_geo_var = np.nanmean(np.nanvar(arr, axis=1))
    total_var = np.nanvar(arr)
    return float(across_geo_var / max(total_var, 1e-12))


def _national_repeated_flag(panel: pd.DataFrame, value_col: str, date_col: str, group_col: str) -> bool:
    if value_col not in panel.columns or group_col not in panel.columns:
        return False
    pivot = (
        panel[[date_col, group_col, value_col]]
        .assign(**{date_col: lambda d: pd.to_datetime(d[date_col]), value_col: lambda d: pd.to_numeric(d[value_col], errors="coerce")})
        .pivot_table(index=date_col, columns=group_col, values=value_col, aggfunc="sum")
    )
    if pivot.shape[1] < 2:
        return True
    arr = pivot.to_numpy(float)
    row_means = np.nanmean(arr, axis=1)
    centered = arr - row_means.reshape(-1, 1)
    rel_spread = np.nanmean(np.nanstd(centered, axis=1) / np.maximum(np.abs(row_means), 1e-8))
    return bool(np.isfinite(rel_spread) and rel_spread < 0.03)


def _channel_learnability(
    panel: pd.DataFrame,
    channels: List[str],
    date_col: str,
    group_col: str,
    support_suffix: str,
    spend_suffix: str,
) -> pd.DataFrame:
    rows = []
    support_cols = {ch: f"{ch}{support_suffix}" for ch in channels if f"{ch}{support_suffix}" in panel.columns}
    channel_support = {ch: pd.to_numeric(panel[col], errors="coerce") for ch, col in support_cols.items()}
    for ch in channels:
        support_col = f"{ch}{support_suffix}"
        spend_col = f"{ch}{spend_suffix}"
        support = pd.to_numeric(panel[support_col], errors="coerce") if support_col in panel.columns else pd.Series(np.nan, index=panel.index)
        spend = pd.to_numeric(panel[spend_col], errors="coerce") if spend_col in panel.columns else pd.Series(np.nan, index=panel.index)
        finite_support = support[np.isfinite(support)]
        finite_spend = spend[np.isfinite(spend)]
        nonzero_support_share = float(np.mean(finite_support > 0)) if len(finite_support) else np.nan
        nonzero_spend_share = float(np.mean(finite_spend > 0)) if len(finite_spend) else np.nan
        support_cv = _cv_positive(support)
        spend_cv = _cv_positive(spend)
        geo_variation = _geo_variation_ratio(panel, support_col, date_col, group_col)
        max_corr = np.nan
        if support_col in panel.columns:
            corrs = []
            for other, other_values in channel_support.items():
                if other == ch:
                    continue
                corrs.append(abs(_safe_corr(support, other_values)))
            if corrs:
                max_corr = float(np.nanmax(corrs))
        support_spend_corr = _safe_corr(support, spend)
        national_repeated = _national_repeated_flag(panel, support_col, date_col, group_col)
        enough_variation = bool(np.nan_to_num(support_cv) >= 0.15 or np.nan_to_num(spend_cv) >= 0.15)
        enough_nonzero = bool(np.nan_to_num(nonzero_support_share) >= 0.15 and np.nan_to_num(nonzero_support_share) <= 0.95)
        enough_geo = bool(np.nan_to_num(geo_variation) >= 0.05)
        not_too_collinear = bool(not np.isfinite(max_corr) or max_corr < 0.90)
        score = 0.0
        score += 25.0 if enough_variation else max(0.0, min(20.0, np.nan_to_num(support_cv, nan=0.0) * 80.0))
        score += 20.0 if enough_nonzero else 8.0 if np.nan_to_num(nonzero_support_share, nan=0.0) > 0 else 0.0
        score += 20.0 if enough_geo else max(0.0, min(15.0, np.nan_to_num(geo_variation, nan=0.0) * 100.0))
        score += 20.0 if not_too_collinear else max(0.0, 20.0 * (1.0 - min(max_corr, 1.0)))
        score += 15.0 if np.isfinite(support_spend_corr) and support_spend_corr >= 0.35 else 8.0
        if national_repeated:
            score = min(score, 70.0)
        if np.nan_to_num(nonzero_support_share, nan=0.0) < 0.03:
            score = min(score, 35.0)
        if np.nan_to_num(max_corr, nan=0.0) >= 0.97:
            score = min(score, 55.0)
        if score >= 80:
            confidence = "causal_estimate_supported"
        elif score >= 65:
            confidence = "directionally_useful"
        elif score >= 45:
            confidence = "weak_signal"
        elif score >= 25:
            confidence = "not_identifiable"
        else:
            confidence = "forecast_only_do_not_use_for_roi"
        rows.append(
            {
                "channel": ch,
                "learnability_score_0_100": float(round(score, 2)),
                "real_data_confidence_flag": confidence,
                "nonzero_support_share": nonzero_support_share,
                "nonzero_spend_share": nonzero_spend_share,
                "support_cv_positive": support_cv,
                "spend_cv_positive": spend_cv,
                "geo_variation_ratio": geo_variation,
                "max_abs_support_corr_with_other_channel": max_corr,
                "support_spend_corr": support_spend_corr,
                "always_on_flag": bool(np.nan_to_num(nonzero_support_share, nan=0.0) > 0.95),
                "sparse_flighting_flag": bool(np.nan_to_num(nonzero_support_share, nan=0.0) < 0.15),
                "high_collinearity_flag": bool(np.nan_to_num(max_corr, nan=0.0) >= 0.90),
                "national_repeated_media_flag": national_repeated,
            }
        )
    return pd.DataFrame(rows)


def _cost_efficiency_drift(
    panel: pd.DataFrame,
    channels: List[str],
    date_col: str,
    support_suffix: str,
    spend_suffix: str,
) -> pd.DataFrame:
    rows = []
    for ch in channels:
        agg = _aggregate_channel(panel, ch, date_col, support_suffix, spend_suffix)
        support_col = f"{ch}{support_suffix}"
        spend_col = f"{ch}{spend_suffix}"
        if agg.empty or support_col not in agg.columns or spend_col not in agg.columns:
            continue
        support = pd.to_numeric(agg[support_col], errors="coerce").to_numpy(float)
        spend = pd.to_numeric(agg[spend_col], errors="coerce").to_numpy(float)
        cpm = spend / np.maximum(support, 1e-12)
        ok = np.isfinite(cpm) & (support > 0)
        if ok.sum() < 8:
            rows.append({"channel": ch, "cost_efficiency_drift_flag": True, "diagnostic": "too_few_positive_periods"})
            continue
        cpm_ok = cpm[ok]
        early = cpm_ok[: max(3, len(cpm_ok) // 3)]
        late = cpm_ok[-max(3, len(cpm_ok) // 3) :]
        early_med = float(np.nanmedian(early))
        late_med = float(np.nanmedian(late))
        drift_ratio = float(late_med / early_med) if early_med > 0 else np.nan
        x = np.arange(len(cpm_ok), dtype=float)
        slope = float(np.polyfit(x, cpm_ok, 1)[0]) if len(cpm_ok) >= 3 else np.nan
        rel_slope = slope / max(abs(float(np.nanmedian(cpm_ok))), 1e-12) * max(len(cpm_ok) - 1, 1)
        flag = bool(np.isfinite(drift_ratio) and (drift_ratio >= 1.35 or drift_ratio <= 0.74 or abs(rel_slope) >= 0.35))
        rows.append(
            {
                "channel": ch,
                "positive_period_n": int(ok.sum()),
                "early_median_cost_per_support": early_med,
                "late_median_cost_per_support": late_med,
                "late_to_early_cost_ratio": drift_ratio,
                "relative_linear_drift": float(rel_slope) if np.isfinite(rel_slope) else np.nan,
                "cost_efficiency_drift_flag": flag,
                "diagnostic": "persistent_cost_efficiency_drift" if flag else "no_large_persistent_drift",
            }
        )
    return pd.DataFrame(rows)


def _spend_support_decoupling(
    panel: pd.DataFrame,
    channels: List[str],
    date_col: str,
    support_suffix: str,
    spend_suffix: str,
) -> pd.DataFrame:
    rows = []
    for ch in channels:
        agg = _aggregate_channel(panel, ch, date_col, support_suffix, spend_suffix)
        support_col = f"{ch}{support_suffix}"
        spend_col = f"{ch}{spend_suffix}"
        if agg.empty or support_col not in agg.columns or spend_col not in agg.columns:
            continue
        support = pd.to_numeric(agg[support_col], errors="coerce")
        spend = pd.to_numeric(agg[spend_col], errors="coerce")
        support_change = support.pct_change(fill_method=None).replace([np.inf, -np.inf], np.nan)
        spend_change = spend.pct_change(fill_method=None).replace([np.inf, -np.inf], np.nan)
        ok = support_change.notna() & spend_change.notna()
        if ok.sum() == 0:
            continue
        spend_up_support_flat = (spend_change.abs() >= 0.25) & (support_change.abs() <= 0.05)
        support_up_spend_flat = (support_change.abs() >= 0.25) & (spend_change.abs() <= 0.05)
        opposite = np.sign(spend_change) * np.sign(support_change) < 0
        decoupled = ok & (spend_up_support_flat | support_up_spend_flat | opposite)
        rows.append(
            {
                "channel": ch,
                "support_spend_corr": _safe_corr(support, spend),
                "periods_checked": int(ok.sum()),
                "decoupled_period_share": float(decoupled.sum() / max(ok.sum(), 1)),
                "spend_change_without_support_change_share": float((ok & spend_up_support_flat).sum() / max(ok.sum(), 1)),
                "support_change_without_spend_change_share": float((ok & support_up_spend_flat).sum() / max(ok.sum(), 1)),
                "opposite_direction_change_share": float((ok & opposite).sum() / max(ok.sum(), 1)),
                "spend_support_decoupling_flag": bool(decoupled.sum() / max(ok.sum(), 1) >= 0.12),
            }
        )
    return pd.DataFrame(rows)


def _leakage_flags(panel: pd.DataFrame, controls: List[str], channels: List[str]) -> pd.DataFrame:
    rows = []
    controls_set = {str(c) for c in controls}
    media_cols = set()
    for ch in channels:
        for suffix in ["_support", "_spend", "_impressions", "_clicks", "_grps", "_reach", "_frequency"]:
            media_cols.add(f"{ch}{suffix}")
    high_risk_tokens = [
        "true_",
        "oracle",
        "decomp",
        "contribution",
        "incremental",
        "roi",
        "mroi",
        "prediction",
        "pred",
        "residual",
        "future",
        "lead",
        "post_treatment",
        "downstream",
    ]
    mediator_tokens = ["click", "conversion", "subscriber", "order", "lead"]
    for col in panel.columns:
        col_str = str(col)
        lower = col_str.lower()
        risk = []
        if any(tok in lower for tok in high_risk_tokens):
            risk.append("possible_target_or_decomposition_leakage")
        if col_str in controls_set and any(tok in lower for tok in mediator_tokens):
            risk.append("possible_post_treatment_mediator_used_as_control")
        if col_str in controls_set and col_str in media_cols:
            risk.append("media_column_also_used_as_control")
        if risk:
            if col_str in controls_set:
                severity = "high"
            elif lower.startswith("true_"):
                severity = "synthetic_truth_only"
            else:
                severity = "medium"
            rows.append(
                {
                    "column": col_str,
                    "used_as_control": bool(col_str in controls_set),
                    "leakage_risk": ";".join(sorted(set(risk))),
                    "severity": severity,
                    "recommended_action": "exclude_from_model_inputs_unless_this_is_synthetic_truth_for_evaluation",
                }
            )
    return pd.DataFrame(rows)


def _label_consistency(
    panel: pd.DataFrame,
    truth_media: Optional[pd.DataFrame],
    truth_economics: Optional[pd.DataFrame],
    date_col: str,
    group_col: str,
    y_col: str,
) -> pd.DataFrame:
    rows = []
    if truth_media is not None and not truth_media.empty and "true_media_contribution" in panel.columns:
        truth = truth_media.copy()
        truth[date_col] = pd.to_datetime(truth[date_col])
        p = panel.copy()
        p[date_col] = pd.to_datetime(p[date_col])
        contrib = (
            truth.groupby([date_col, group_col], as_index=False)["true_contribution"]
            .sum()
            .rename(columns={"true_contribution": "_truth_media_contribution"})
        )
        merged = p[[date_col, group_col, "true_media_contribution"]].merge(contrib, on=[date_col, group_col], how="inner")
        err = pd.to_numeric(merged["true_media_contribution"], errors="coerce") - pd.to_numeric(
            merged["_truth_media_contribution"], errors="coerce"
        )
        rows.append(
            {
                "check": "panel_true_media_contribution_matches_truth_media_sum",
                "max_abs_error": float(np.nanmax(np.abs(err))) if len(err) else np.nan,
                "mean_abs_error": float(np.nanmean(np.abs(err))) if len(err) else np.nan,
                "passed": bool(len(err) > 0 and np.nanmax(np.abs(err)) < 1e-6),
            }
        )
    if {"true_signal", "true_baseline", "true_media_contribution"}.issubset(panel.columns):
        err = (
            pd.to_numeric(panel["true_signal"], errors="coerce")
            - pd.to_numeric(panel["true_baseline"], errors="coerce")
            - pd.to_numeric(panel["true_media_contribution"], errors="coerce")
        )
        rows.append(
            {
                "check": "panel_true_signal_matches_baseline_plus_media",
                "max_abs_error": float(np.nanmax(np.abs(err))),
                "mean_abs_error": float(np.nanmean(np.abs(err))),
                "passed": bool(np.nanmax(np.abs(err)) < 1e-6),
            }
        )
    if {y_col, "true_signal", "true_noise"}.issubset(panel.columns):
        err = pd.to_numeric(panel[y_col], errors="coerce") - pd.to_numeric(panel["true_signal"], errors="coerce") - pd.to_numeric(
            panel["true_noise"], errors="coerce"
        )
        rows.append(
            {
                "check": "panel_kpi_matches_true_signal_plus_noise",
                "max_abs_error": float(np.nanmax(np.abs(err))),
                "mean_abs_error": float(np.nanmean(np.abs(err))),
                "passed": bool(np.nanmax(np.abs(err)) < 1e-6),
            }
        )
    if truth_media is not None and truth_economics is not None and not truth_media.empty and not truth_economics.empty:
        tm = truth_media.copy()
        econ = truth_economics.copy()
        totals = tm.groupby("channel", as_index=False).agg(
            _truth_spend=("spend", "sum"),
            _truth_contribution=("true_contribution", "sum"),
        )
        merged = econ.merge(totals, on="channel", how="inner")
        if not merged.empty and {"true_total_spend", "true_total_incremental_contribution", "true_roi_like"}.issubset(merged.columns):
            roi_expected = merged["_truth_contribution"] / np.maximum(merged["_truth_spend"], 1e-12)
            roi_err = pd.to_numeric(merged["true_roi_like"], errors="coerce") - roi_expected
            rows.append(
                {
                    "check": "truth_economics_matches_truth_media_totals",
                    "max_abs_error": float(np.nanmax(np.abs(roi_err))),
                    "mean_abs_error": float(np.nanmean(np.abs(roi_err))),
                    "passed": bool(np.nanmax(np.abs(roi_err)) < 1e-8),
                }
            )
    if not rows:
        rows.append(
            {
                "check": "synthetic_truth_labels_available",
                "max_abs_error": np.nan,
                "mean_abs_error": np.nan,
                "passed": False,
                "note": "No synthetic truth labels were supplied. This is expected for real data.",
            }
        )
    return pd.DataFrame(rows)


def _summary_from_parts(parts: Dict[str, pd.DataFrame]) -> pd.DataFrame:
    learn = parts["channel_learnability"]
    drift = parts["cost_efficiency_drift"]
    decouple = parts["spend_support_decoupling"]
    leakage = parts["leakage_flags"]
    labels = parts["label_consistency"]
    high_leakage_n = int((leakage.get("severity", pd.Series(dtype=str)) == "high").sum()) if not leakage.empty else 0
    rows = [
        {"metric": "channel_n", "value": int(len(learn)), "status": "info"},
        {
            "metric": "mean_learnability_score_0_100",
            "value": float(learn["learnability_score_0_100"].mean()) if not learn.empty else np.nan,
            "status": "pass" if not learn.empty and learn["learnability_score_0_100"].mean() >= 65 else "warn",
        },
        {
            "metric": "not_identifiable_or_forecast_only_channel_n",
            "value": int(learn["real_data_confidence_flag"].isin(["not_identifiable", "forecast_only_do_not_use_for_roi"]).sum()) if not learn.empty else 0,
            "status": "warn" if not learn.empty and learn["real_data_confidence_flag"].isin(["not_identifiable", "forecast_only_do_not_use_for_roi"]).any() else "pass",
        },
        {
            "metric": "cost_efficiency_drift_channel_n",
            "value": int(drift.get("cost_efficiency_drift_flag", pd.Series(dtype=bool)).fillna(False).sum()) if not drift.empty else 0,
            "status": "warn" if not drift.empty and drift.get("cost_efficiency_drift_flag", pd.Series(dtype=bool)).fillna(False).any() else "pass",
        },
        {
            "metric": "spend_support_decoupling_channel_n",
            "value": int(decouple.get("spend_support_decoupling_flag", pd.Series(dtype=bool)).fillna(False).sum()) if not decouple.empty else 0,
            "status": "warn" if not decouple.empty and decouple.get("spend_support_decoupling_flag", pd.Series(dtype=bool)).fillna(False).any() else "pass",
        },
        {
            "metric": "high_leakage_risk_column_n",
            "value": high_leakage_n,
            "status": "warn" if high_leakage_n > 0 else "pass",
        },
        {
            "metric": "synthetic_label_checks_passed",
            "value": int(labels.get("passed", pd.Series(dtype=bool)).fillna(False).sum()) if not labels.empty else 0,
            "status": "pass" if not labels.empty and labels.get("passed", pd.Series(dtype=bool)).fillna(False).all() else "warn",
        },
    ]
    return pd.DataFrame(rows)


def validate_nmmm_training_data(
    panel: pd.DataFrame,
    truth_media: Optional[pd.DataFrame] = None,
    truth_economics: Optional[pd.DataFrame] = None,
    channels: Optional[List[str]] = None,
    controls: Optional[List[str]] = None,
    media_feature_inputs: Optional[List[str]] = None,
    date_col: str = "date",
    group_col: str = "geo_id",
    y_col: str = "kpi",
    support_suffix: str = "_support",
    spend_suffix: str = "_spend",
) -> TrainingDataValidation:
    """Validate NMMM synthetic or real training data.

    The checks intentionally separate identification/readiness diagnostics from
    model performance. For real data, label checks will be absent or warning
    only; for synthetic pretraining panels, label checks should pass.
    """
    if date_col not in panel.columns:
        raise ValueError(f"panel is missing date_col: {date_col}")
    if group_col not in panel.columns:
        raise ValueError(f"panel is missing group_col: {group_col}")
    if y_col not in panel.columns:
        raise ValueError(f"panel is missing y_col: {y_col}")
    channels = channels or _infer_channels(panel, support_suffix, spend_suffix)
    controls = controls or []
    _ = media_feature_inputs or ["support", "spend"]
    parts: Dict[str, pd.DataFrame] = {
        "channel_learnability": _channel_learnability(panel, channels, date_col, group_col, support_suffix, spend_suffix),
        "cost_efficiency_drift": _cost_efficiency_drift(panel, channels, date_col, support_suffix, spend_suffix),
        "spend_support_decoupling": _spend_support_decoupling(panel, channels, date_col, support_suffix, spend_suffix),
        "leakage_flags": _leakage_flags(panel, controls, channels),
        "label_consistency": _label_consistency(panel, truth_media, truth_economics, date_col, group_col, y_col),
    }
    parts["summary"] = _summary_from_parts(parts)
    return TrainingDataValidation(
        summary=parts["summary"],
        channel_learnability=parts["channel_learnability"],
        cost_efficiency_drift=parts["cost_efficiency_drift"],
        spend_support_decoupling=parts["spend_support_decoupling"],
        leakage_flags=parts["leakage_flags"],
        label_consistency=parts["label_consistency"],
    )


def write_training_data_validation(
    validation: TrainingDataValidation,
    output_dir: str | Path,
    prefix: str = "training_data_validation",
) -> None:
    """Write validation tables with stable filenames."""
    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)
    validation.summary.to_csv(out / f"{prefix}_summary.csv", index=False)
    validation.channel_learnability.to_csv(out / f"{prefix}_channel_learnability.csv", index=False)
    validation.cost_efficiency_drift.to_csv(out / f"{prefix}_cost_efficiency_drift.csv", index=False)
    validation.spend_support_decoupling.to_csv(out / f"{prefix}_spend_support_decoupling.csv", index=False)
    validation.leakage_flags.to_csv(out / f"{prefix}_leakage_flags.csv", index=False)
    validation.label_consistency.to_csv(out / f"{prefix}_label_consistency.csv", index=False)
