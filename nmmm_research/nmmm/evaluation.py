"""Evaluation helpers for known-truth NMMM experiments."""

from __future__ import annotations

from typing import Optional

import numpy as np
import pandas as pd


def evaluate_prediction_fit(predictions: pd.DataFrame, y_col: str = "y_actual", pred_col: str = "y_pred") -> pd.DataFrame:
    rows = []
    for label, mask in {
        "all": np.ones(len(predictions), dtype=bool),
        "train": predictions.get("is_train", pd.Series(True, index=predictions.index)).astype(bool).to_numpy(),
        "holdout": ~predictions.get("is_train", pd.Series(True, index=predictions.index)).astype(bool).to_numpy(),
    }.items():
        if mask.sum() == 0:
            continue
        y = pd.to_numeric(predictions.loc[mask, y_col], errors="coerce").to_numpy(float)
        p = pd.to_numeric(predictions.loc[mask, pred_col], errors="coerce").to_numpy(float)
        ok = np.isfinite(y) & np.isfinite(p)
        if ok.sum() == 0:
            continue
        resid = y[ok] - p[ok]
        ss_res = float(np.sum(resid**2))
        ss_tot = float(np.sum((y[ok] - np.mean(y[ok])) ** 2))
        rows.append(
            {
                "sample": label,
                "n": int(ok.sum()),
                "rmse": float(np.sqrt(np.mean(resid**2))),
                "mae": float(np.mean(np.abs(resid))),
                "r2": float(1.0 - ss_res / ss_tot) if ss_tot > 0 else np.nan,
                "mape": float(np.mean(np.abs(resid) / np.maximum(np.abs(y[ok]), 1e-6))),
            }
        )
    return pd.DataFrame(rows)


def evaluate_oracle_fit(
    panel: pd.DataFrame,
    truth_media: pd.DataFrame,
    date_col: str = "date",
    group_col: str = "geo_id",
    y_col: str = "kpi",
    baseline_col: str = "true_baseline",
    contribution_col: str = "true_contribution",
    holdout_weeks: int = 13,
) -> pd.DataFrame:
    """Compute the synthetic-data oracle/noise-ceiling fit.

    Oracle prediction is the true noiseless signal:
    true baseline + true media contribution. R2 below 1 is expected when the
    observed KPI includes noise.
    """
    required_panel = {date_col, group_col, y_col, baseline_col}
    required_truth = {date_col, group_col, contribution_col}
    if not required_panel.issubset(panel.columns) or not required_truth.issubset(truth_media.columns):
        return pd.DataFrame()

    p = panel.copy()
    t = truth_media.copy()
    p[date_col] = pd.to_datetime(p[date_col])
    t[date_col] = pd.to_datetime(t[date_col])
    contrib = (
        t.groupby([date_col, group_col], as_index=False)[contribution_col]
        .sum()
        .rename(columns={contribution_col: "true_media_contribution"})
    )
    oracle = p[[date_col, group_col, y_col, baseline_col]].merge(contrib, on=[date_col, group_col], how="left")
    oracle["true_media_contribution"] = oracle["true_media_contribution"].fillna(0.0)
    oracle["oracle_signal"] = pd.to_numeric(oracle[baseline_col], errors="coerce") + oracle["true_media_contribution"]
    dates = np.sort(oracle[date_col].unique())
    if holdout_weeks > 0 and len(dates) > holdout_weeks:
        cutoff = dates[-int(holdout_weeks)]
        oracle["is_train"] = oracle[date_col] < cutoff
    else:
        oracle["is_train"] = True

    out = evaluate_prediction_fit(
        oracle.rename(columns={y_col: "y_actual", "oracle_signal": "y_pred"}),
        y_col="y_actual",
        pred_col="y_pred",
    )
    if not out.empty:
        out["fit_type"] = "oracle_true_signal"
        noise = pd.to_numeric(oracle[y_col], errors="coerce") - oracle["oracle_signal"]
        out["oracle_noise_rmse_all"] = float(np.sqrt(np.nanmean(noise**2)))
    return out


def evaluate_contribution_recovery(
    predictions: pd.DataFrame,
    truth_media: pd.DataFrame,
    date_col: str = "date",
    group_col: str = "geo_id",
    channel_col: str = "channel",
    contribution_col: str = "true_contribution",
    spend_col: str = "spend",
) -> pd.DataFrame:
    pred = predictions.copy()
    pred[date_col] = pd.to_datetime(pred[date_col])
    truth = truth_media.copy()
    truth[date_col] = pd.to_datetime(truth[date_col])

    rows = []
    for channel in sorted(truth[channel_col].astype(str).unique()):
        est_col = f"{channel}_estimated_contribution"
        if est_col not in pred.columns:
            continue
        merged = truth.loc[truth[channel_col].astype(str).eq(channel), [date_col, group_col, contribution_col, spend_col]].merge(
            pred[[date_col, group_col, est_col, "is_train"]],
            on=[date_col, group_col],
            how="left",
        )
        for sample, mask in {
            "all": np.ones(len(merged), dtype=bool),
            "train": merged["is_train"].fillna(False).astype(bool).to_numpy(),
            "holdout": ~merged["is_train"].fillna(True).astype(bool).to_numpy(),
        }.items():
            if mask.sum() == 0:
                continue
            true = pd.to_numeric(merged.loc[mask, contribution_col], errors="coerce").to_numpy(float)
            est = pd.to_numeric(merged.loc[mask, est_col], errors="coerce").to_numpy(float)
            spend = pd.to_numeric(merged.loc[mask, spend_col], errors="coerce").to_numpy(float)
            ok = np.isfinite(true) & np.isfinite(est)
            if ok.sum() < 3:
                continue
            corr = np.corrcoef(true[ok], est[ok])[0, 1] if np.std(true[ok]) > 1e-12 and np.std(est[ok]) > 1e-12 else np.nan
            true_total = float(np.sum(true[ok]))
            est_total = float(np.sum(est[ok]))
            spend_total = float(np.nansum(spend[ok]))
            rows.append(
                {
                    "channel": channel,
                    "sample": sample,
                    "n": int(ok.sum()),
                    "contribution_corr": float(corr) if np.isfinite(corr) else np.nan,
                    "contribution_mae": float(np.mean(np.abs(est[ok] - true[ok]))),
                    "contribution_bias_total": float(est_total - true_total),
                    "true_total_contribution": true_total,
                    "estimated_total_contribution": est_total,
                    "true_roi_like": float(true_total / spend_total) if spend_total > 0 else np.nan,
                    "estimated_roi_like": float(est_total / spend_total) if spend_total > 0 else np.nan,
                    "roi_like_error": float((est_total - true_total) / spend_total) if spend_total > 0 else np.nan,
                }
            )
    return pd.DataFrame(rows)


def evaluate_baseline_recovery(
    predictions: pd.DataFrame,
    panel: pd.DataFrame,
    date_col: str = "date",
    group_col: str = "geo_id",
    true_baseline_col: str = "true_baseline",
    estimated_baseline_col: str = "estimated_baseline",
) -> pd.DataFrame:
    """Evaluate whether the model recovers the synthetic non-media baseline."""
    if true_baseline_col not in panel.columns or estimated_baseline_col not in predictions.columns:
        return pd.DataFrame()
    pred = predictions.copy()
    actual = panel.copy()
    pred[date_col] = pd.to_datetime(pred[date_col])
    actual[date_col] = pd.to_datetime(actual[date_col])
    merged = actual[[date_col, group_col, true_baseline_col]].merge(
        pred[[date_col, group_col, estimated_baseline_col, "is_train"]],
        on=[date_col, group_col],
        how="inner",
    )
    rows = []
    for sample, mask in {
        "all": np.ones(len(merged), dtype=bool),
        "train": merged["is_train"].fillna(False).astype(bool).to_numpy(),
        "holdout": ~merged["is_train"].fillna(True).astype(bool).to_numpy(),
    }.items():
        if mask.sum() == 0:
            continue
        true = pd.to_numeric(merged.loc[mask, true_baseline_col], errors="coerce").to_numpy(float)
        est = pd.to_numeric(merged.loc[mask, estimated_baseline_col], errors="coerce").to_numpy(float)
        ok = np.isfinite(true) & np.isfinite(est)
        if ok.sum() < 3:
            continue
        corr = np.corrcoef(true[ok], est[ok])[0, 1] if np.std(true[ok]) > 1e-12 and np.std(est[ok]) > 1e-12 else np.nan
        rows.append(
            {
                "sample": sample,
                "n": int(ok.sum()),
                "baseline_corr": float(corr) if np.isfinite(corr) else np.nan,
                "baseline_rmse": float(np.sqrt(np.mean((est[ok] - true[ok]) ** 2))),
                "baseline_mae": float(np.mean(np.abs(est[ok] - true[ok]))),
                "baseline_bias_total": float(np.sum(est[ok] - true[ok])),
            }
        )
    return pd.DataFrame(rows)


def evaluate_curve_recovery(
    estimated_curves: pd.DataFrame,
    truth_curves: pd.DataFrame,
    channel_col: str = "channel",
) -> pd.DataFrame:
    """Compare estimated and true response curve shapes when synthetic truth exists."""
    if estimated_curves is None or truth_curves is None or estimated_curves.empty or truth_curves.empty:
        return pd.DataFrame()
    est = estimated_curves.copy()
    truth = truth_curves.copy()
    est_pct_col = "pct_of_current_support" if "pct_of_current_support" in est.columns else "pct_of_anchor_support"
    truth_pct_col = "pct_of_anchor_support" if "pct_of_anchor_support" in truth.columns else "pct_of_current_support"
    est_y_col = "estimated_incremental_contribution"
    truth_y_col = "true_incremental_contribution"
    required_est = {channel_col, est_pct_col, est_y_col}
    required_truth = {channel_col, truth_pct_col, truth_y_col}
    if not required_est.issubset(est.columns) or not required_truth.issubset(truth.columns):
        return pd.DataFrame()
    if "geo_id" in est.columns:
        est = est.loc[est["geo_id"].astype(str).eq("__global__") | est["geo_id"].isna()].copy()
    est["_pct_round"] = pd.to_numeric(est[est_pct_col], errors="coerce").round(3)
    truth["_pct_round"] = pd.to_numeric(truth[truth_pct_col], errors="coerce").round(3)
    rows = []
    for channel in sorted(set(est[channel_col].astype(str)).intersection(set(truth[channel_col].astype(str)))):
        merged = est.loc[est[channel_col].astype(str).eq(channel), ["_pct_round", est_y_col]].merge(
            truth.loc[truth[channel_col].astype(str).eq(channel), ["_pct_round", truth_y_col]],
            on="_pct_round",
            how="inner",
        )
        if len(merged) < 6:
            continue
        e = pd.to_numeric(merged[est_y_col], errors="coerce").to_numpy(float)
        t = pd.to_numeric(merged[truth_y_col], errors="coerce").to_numpy(float)
        ok = np.isfinite(e) & np.isfinite(t)
        if ok.sum() < 6:
            continue
        corr = np.corrcoef(e[ok], t[ok])[0, 1] if np.std(e[ok]) > 1e-12 and np.std(t[ok]) > 1e-12 else np.nan
        area_true = float(np.trapz(t[ok]))
        area_est = float(np.trapz(e[ok]))
        max_true = float(np.max(t[ok]))
        max_est = float(np.max(e[ok]))
        order = np.argsort(merged.loc[ok, "_pct_round"].to_numpy(float))
        e_ordered = e[ok][order]
        diffs = np.diff(e_ordered)
        monotonic_violation_share = float(np.mean(diffs < -1e-8)) if len(diffs) else np.nan
        negative_marginal_area = float(np.sum(np.abs(np.minimum(diffs, 0.0)))) if len(diffs) else 0.0
        second_diff = np.diff(diffs) if len(diffs) > 1 else np.array([])
        rows.append(
            {
                "channel": channel,
                "curve_corr": float(corr) if np.isfinite(corr) else np.nan,
                "curve_area_error_pct": float((area_est - area_true) / max(abs(area_true), 1e-6)),
                "curve_max_error_pct": float((max_est - max_true) / max(abs(max_true), 1e-6)),
                "curve_mae": float(np.mean(np.abs(e[ok] - t[ok]))),
                "monotonic_violation_share": monotonic_violation_share,
                "negative_marginal_area": negative_marginal_area,
                "marginal_instability": float(np.mean(second_diff**2)) if len(second_diff) else np.nan,
                "n_points": int(ok.sum()),
            }
        )
    return pd.DataFrame(rows)


def evaluate_parameter_recovery(
    estimated_params: pd.DataFrame,
    truth_params: pd.DataFrame,
    channel_col: str = "channel",
) -> pd.DataFrame:
    """Compare learned media parameters to synthetic truth where scales are compatible."""
    if estimated_params is None or truth_params is None or estimated_params.empty or truth_params.empty:
        return pd.DataFrame()
    est = estimated_params.copy()
    truth = truth_params.copy()
    if "param_level" in est.columns:
        est = est.loc[est["param_level"].astype(str).eq("global")].copy()
    if channel_col not in est.columns or channel_col not in truth.columns:
        return pd.DataFrame()
    rows = []
    merged = est.merge(truth, on=channel_col, how="inner", suffixes=("_est", "_true"))
    for _, row in merged.iterrows():
        out = {"channel": row[channel_col]}
        if "decay_est" in row.index and "decay_true" in row.index:
            out["decay_error"] = float(row["decay_est"] - row["decay_true"])
            out["abs_decay_error"] = abs(out["decay_error"])
        elif "decay" in est.columns and "decay" in truth.columns:
            out["decay_error"] = float(row["decay_est"] - row["decay_true"])
            out["abs_decay_error"] = abs(out["decay_error"])
        if "shape_est" in row.index and "shape_true" in row.index:
            out["shape_error"] = float(row["shape_est"] - row["shape_true"])
            out["abs_shape_error"] = abs(out["shape_error"])
        rows.append(out)
    return pd.DataFrame(rows)


def _score_component(value: float, higher_is_better: bool = True, cap: float = 1.0) -> Optional[float]:
    if value is None or not np.isfinite(value):
        return None
    if higher_is_better:
        return float(np.clip(value, 0.0, 1.0))
    return float(1.0 - np.clip(abs(value) / max(cap, 1e-6), 0.0, 1.0))


def summarize_recovery(
    contribution_recovery: pd.DataFrame,
    prediction_metrics: Optional[pd.DataFrame] = None,
    curve_recovery: Optional[pd.DataFrame] = None,
    parameter_recovery: Optional[pd.DataFrame] = None,
    oracle_metrics: Optional[pd.DataFrame] = None,
    baseline_recovery: Optional[pd.DataFrame] = None,
    model_diagnostics: Optional[pd.DataFrame] = None,
) -> pd.DataFrame:
    rows = []
    if prediction_metrics is not None and not prediction_metrics.empty:
        holdout = prediction_metrics.loc[prediction_metrics["sample"].eq("holdout")]
        if not holdout.empty:
            rows.append({"metric": "holdout_r2", "value": float(holdout["r2"].iloc[0])})
            rows.append({"metric": "holdout_rmse", "value": float(holdout["rmse"].iloc[0])})
            rows.append({"metric": "diagnostic_prediction_fit_only", "value": 1.0})
    if oracle_metrics is not None and not oracle_metrics.empty:
        oracle_holdout = oracle_metrics.loc[oracle_metrics["sample"].eq("holdout")]
        if not oracle_holdout.empty:
            rows.append({"metric": "oracle_holdout_r2_noise_ceiling", "value": float(oracle_holdout["r2"].iloc[0])})
            rows.append({"metric": "oracle_holdout_rmse_noise_floor", "value": float(oracle_holdout["rmse"].iloc[0])})
    if not contribution_recovery.empty:
        all_rows = contribution_recovery.loc[contribution_recovery["sample"].eq("all")]
        if not all_rows.empty:
            rows.append({"metric": "median_channel_contribution_corr", "value": float(np.nanmedian(all_rows["contribution_corr"]))})
            rows.append({"metric": "median_abs_roi_like_error", "value": float(np.nanmedian(np.abs(all_rows["roi_like_error"])))})
    if baseline_recovery is not None and not baseline_recovery.empty:
        all_base = baseline_recovery.loc[baseline_recovery["sample"].eq("all")]
        if not all_base.empty:
            rows.append({"metric": "baseline_corr", "value": float(all_base["baseline_corr"].iloc[0])})
            rows.append({"metric": "baseline_rmse", "value": float(all_base["baseline_rmse"].iloc[0])})
    if curve_recovery is not None and not curve_recovery.empty:
        rows.append({"metric": "median_curve_corr", "value": float(np.nanmedian(curve_recovery["curve_corr"]))})
        rows.append({"metric": "median_abs_curve_area_error_pct", "value": float(np.nanmedian(np.abs(curve_recovery["curve_area_error_pct"])))})
        if "monotonic_violation_share" in curve_recovery.columns:
            rows.append({"metric": "max_curve_monotonic_violation_share", "value": float(np.nanmax(curve_recovery["monotonic_violation_share"]))})
        if "marginal_instability" in curve_recovery.columns:
            rows.append({"metric": "median_curve_marginal_instability", "value": float(np.nanmedian(curve_recovery["marginal_instability"]))})
    if parameter_recovery is not None and not parameter_recovery.empty:
        if "abs_decay_error" in parameter_recovery.columns:
            rows.append({"metric": "median_abs_decay_error", "value": float(np.nanmedian(parameter_recovery["abs_decay_error"]))})
        if "abs_shape_error" in parameter_recovery.columns:
            rows.append({"metric": "median_abs_shape_error", "value": float(np.nanmedian(parameter_recovery["abs_shape_error"]))})
    if model_diagnostics is not None and not model_diagnostics.empty:
        if "baseline_stealing_ratio" in model_diagnostics.columns:
            rows.append({"metric": "max_baseline_stealing_ratio", "value": float(np.nanmax(model_diagnostics["baseline_stealing_ratio"]))})
        if "counterfactual_gap_rmse" in model_diagnostics.columns:
            rows.append({"metric": "median_counterfactual_gap_rmse", "value": float(np.nanmedian(model_diagnostics["counterfactual_gap_rmse"]))})

    score_inputs = {r["metric"]: r["value"] for r in rows}
    if "holdout_r2" in score_inputs and "oracle_holdout_r2_noise_ceiling" in score_inputs:
        rows.append(
            {
                "metric": "holdout_r2_gap_to_oracle",
                "value": float(score_inputs["oracle_holdout_r2_noise_ceiling"] - score_inputs["holdout_r2"]),
            }
        )
    components = []
    weights = []
    for metric, weight, hib, cap in [
        ("median_channel_contribution_corr", 0.40, True, 1.0),
        ("median_abs_roi_like_error", 0.25, False, 0.75),
        ("median_curve_corr", 0.20, True, 1.0),
        ("baseline_corr", 0.10, True, 1.0),
        ("median_abs_decay_error", 0.10, False, 0.50),
        ("median_abs_shape_error", 0.05, False, 0.75),
    ]:
        comp = _score_component(score_inputs.get(metric), higher_is_better=hib, cap=cap)
        if comp is not None:
            components.append(comp * weight)
            weights.append(weight)
    if weights:
        rows.append({"metric": "known_truth_recovery_score_0_100", "value": float(100.0 * sum(components) / sum(weights))})
    return pd.DataFrame(rows)
