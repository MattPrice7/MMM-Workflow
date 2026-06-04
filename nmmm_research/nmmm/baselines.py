"""Transparent MMM-style baselines for NMMM benchmarking."""

from __future__ import annotations

import json
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Tuple

import numpy as np
import pandas as pd

from .transforms import (
    apply_saturation,
    curve_parameter_from_anchor,
    finite_median_positive,
    geometric_adstock_1d,
)


@dataclass
class TransformedRidgeMMMResult:
    predictions: pd.DataFrame
    long_decomp: pd.DataFrame
    selected_transforms: pd.DataFrame
    response_curves: pd.DataFrame
    coefficients: pd.DataFrame
    settings: Dict[str, object]

    def to_json_summary(self) -> str:
        return json.dumps(self.settings, indent=2, sort_keys=True)


def _infer_channels(df: pd.DataFrame, support_suffix: str) -> List[str]:
    return sorted([c[: -len(support_suffix)] for c in df.columns if c.endswith(support_suffix)])


def _make_train_mask(df: pd.DataFrame, date_col: str, holdout_weeks: int, train_col: Optional[str]) -> np.ndarray:
    if train_col and train_col in df.columns:
        return df[train_col].astype(bool).to_numpy()
    dates = pd.to_datetime(df[date_col])
    cutoff = np.sort(dates.unique())[-max(int(holdout_weeks), 1)]
    return (dates < cutoff).to_numpy() if holdout_weeks > 0 else np.ones(len(df), dtype=bool)


def _adstock_by_group(df: pd.DataFrame, col: str, date_col: str, group_col: str, decay: float) -> np.ndarray:
    out = np.zeros(len(df), dtype=float)
    for _, idx in df.sort_values(date_col).groupby(group_col).groups.items():
        idx_arr = np.asarray(list(idx), dtype=int)
        ordered = df.loc[idx_arr].sort_values(date_col).index.to_numpy()
        out[ordered] = geometric_adstock_1d(df.loc[ordered, col].to_numpy(), decay)
    return out


def _safe_corr(x: np.ndarray, y: np.ndarray) -> float:
    ok = np.isfinite(x) & np.isfinite(y)
    if ok.sum() < 6:
        return 0.0
    x_ok = x[ok]
    y_ok = y[ok]
    if np.std(x_ok) <= 1e-12 or np.std(y_ok) <= 1e-12:
        return 0.0
    return float(np.corrcoef(x_ok, y_ok)[0, 1])


def _build_base_features(df: pd.DataFrame, date_col: str, group_col: str, controls: Iterable[str]) -> Tuple[pd.DataFrame, List[str]]:
    dates = pd.to_datetime(df[date_col])
    week_index = (dates - dates.min()).dt.days.to_numpy() / 7.0
    base = pd.DataFrame(
        {
            "trend": week_index,
            "season_sin_52": np.sin(2 * np.pi * week_index / 52.0),
            "season_cos_52": np.cos(2 * np.pi * week_index / 52.0),
        },
        index=df.index,
    )
    for col in controls:
        if col in df.columns:
            base[col] = pd.to_numeric(df[col], errors="coerce").fillna(0.0).to_numpy()
    dummies = pd.get_dummies(df[group_col].astype(str), prefix="group", drop_first=True, dtype=float)
    base = pd.concat([base, dummies.set_index(df.index)], axis=1)
    return base, list(base.columns)


def _fit_ridge_raw_coefficients(
    X: pd.DataFrame,
    y: np.ndarray,
    train_mask: np.ndarray,
    media_cols: List[str],
    ridge_lambda: float,
    positive_media: bool = True,
) -> Tuple[np.ndarray, float, np.ndarray]:
    x = X.to_numpy(dtype=float)
    y_arr = np.asarray(y, dtype=float)
    ok = train_mask & np.isfinite(y_arr) & np.all(np.isfinite(x), axis=1)
    if ok.sum() < max(8, min(30, X.shape[1] + 2)):
        raise ValueError("Not enough complete training rows for transformed ridge baseline.")

    active = np.ones(X.shape[1], dtype=bool)
    media_idx = np.array([X.columns.get_loc(c) for c in media_cols if c in X.columns], dtype=int)

    for _ in range(5):
        xa = x[:, active]
        mean = xa[ok].mean(axis=0)
        scale = xa[ok].std(axis=0)
        scale = np.where(scale <= 1e-12, 1.0, scale)
        z = (xa - mean) / scale
        z_design = np.column_stack([np.ones(len(z)), z])
        penalty = np.eye(z_design.shape[1]) * float(ridge_lambda)
        penalty[0, 0] = 0.0
        beta_scaled = np.linalg.pinv(z_design[ok].T @ z_design[ok] + penalty) @ z_design[ok].T @ y_arr[ok]
        raw_active = beta_scaled[1:] / scale
        intercept = float(beta_scaled[0] - np.sum(raw_active * mean))
        raw_full = np.zeros(X.shape[1], dtype=float)
        raw_full[active] = raw_active
        if not positive_media:
            return raw_full, intercept, ok
        bad_media = media_idx[(active[media_idx]) & (raw_full[media_idx] < 0)]
        if bad_media.size == 0:
            return raw_full, intercept, ok
        active[bad_media] = False

    return raw_full, intercept, ok


def _choose_channel_transform(
    df: pd.DataFrame,
    y_resid: np.ndarray,
    train_mask: np.ndarray,
    channel: str,
    support_col: str,
    date_col: str,
    group_col: str,
    curve_type: str,
    decay_grid: Iterable[float],
    anchor_saturation_grid: Iterable[float],
    shape_grid: Iterable[float],
) -> Tuple[pd.Series, Dict[str, float]]:
    best_score = -np.inf
    best_feature = None
    best_params: Dict[str, float] = {}
    support = pd.to_numeric(df[support_col], errors="coerce").fillna(0.0).to_numpy()
    for decay in decay_grid:
        adstock = _adstock_by_group(df, support_col, date_col, group_col, decay)
        anchor_support = finite_median_positive(adstock[train_mask], fallback=finite_median_positive(support, 1.0))
        for anchor_saturation in anchor_saturation_grid:
            for shape in shape_grid:
                curve_param = curve_parameter_from_anchor(
                    anchor_support,
                    anchor_saturation=anchor_saturation,
                    curve_type=curve_type,
                    shape=shape,
                )
                transformed = apply_saturation(adstock, curve_param, shape=shape, curve_type=curve_type)
                score = abs(_safe_corr(transformed[train_mask], y_resid[train_mask]))
                if score > best_score:
                    best_score = score
                    best_feature = transformed
                    best_params = {
                        "channel": channel,
                        "decay": float(decay),
                        "curve_type": curve_type,
                        "anchor_support": float(anchor_support),
                        "anchor_saturation": float(anchor_saturation),
                        "curve_param": float(curve_param),
                        "shape": float(shape),
                        "selection_abs_corr": float(score),
                    }
    return pd.Series(best_feature, index=df.index, name=f"{channel}__media_x"), best_params


def fit_transformed_ridge_mmm(
    panel: pd.DataFrame,
    channels: Optional[List[str]] = None,
    date_col: str = "date",
    group_col: str = "geo_id",
    y_col: str = "kpi",
    support_suffix: str = "_support",
    spend_suffix: str = "_spend",
    controls: Optional[List[str]] = None,
    curve_type: str = "hill",
    holdout_weeks: int = 13,
    train_col: Optional[str] = None,
    ridge_lambda: float = 10.0,
    positive_media: bool = True,
    decay_grid: Optional[List[float]] = None,
    anchor_saturation_grid: Optional[List[float]] = None,
    shape_grid: Optional[List[float]] = None,
) -> TransformedRidgeMMMResult:
    """Fit a transparent transformed-ridge MMM benchmark.

    This is a challenger baseline for NMMM research, not a replacement for the
    R Stan model.
    """
    df = panel.copy()
    df[date_col] = pd.to_datetime(df[date_col])
    df = df.sort_values([group_col, date_col]).reset_index(drop=True)
    channels = channels or _infer_channels(df, support_suffix)
    controls = controls or [c for c in ["promo", "holiday", "price_index"] if c in df.columns]
    decay_grid = decay_grid or [0.0, 0.15, 0.30, 0.45, 0.60, 0.75]
    anchor_saturation_grid = anchor_saturation_grid or [0.35, 0.50, 0.65]
    shape_grid = shape_grid or [0.80, 1.00, 1.25, 1.50]
    train_mask = _make_train_mask(df, date_col, holdout_weeks, train_col)
    y = pd.to_numeric(df[y_col], errors="coerce").to_numpy(dtype=float)

    base_x, _ = _build_base_features(df, date_col, group_col, controls)
    base_coef, base_intercept, _ = _fit_ridge_raw_coefficients(
        base_x,
        y,
        train_mask,
        media_cols=[],
        ridge_lambda=ridge_lambda,
        positive_media=False,
    )
    y_resid = y - (base_intercept + base_x.to_numpy(dtype=float) @ base_coef)

    media_features = {}
    transform_rows = []
    for channel in channels:
        support_col = f"{channel}{support_suffix}"
        if support_col not in df.columns:
            continue
        feature, params = _choose_channel_transform(
            df,
            y_resid,
            train_mask,
            channel,
            support_col,
            date_col,
            group_col,
            curve_type,
            decay_grid,
            anchor_saturation_grid,
            shape_grid,
        )
        media_features[feature.name] = feature
        transform_rows.append(params)

    media_x = pd.DataFrame(media_features, index=df.index)
    x_full = pd.concat([base_x, media_x], axis=1)
    media_cols = list(media_x.columns)
    coef, intercept, complete_train = _fit_ridge_raw_coefficients(
        x_full,
        y,
        train_mask,
        media_cols=media_cols,
        ridge_lambda=ridge_lambda,
        positive_media=positive_media,
    )
    y_pred = intercept + x_full.to_numpy(dtype=float) @ coef
    coef_df = pd.DataFrame({"feature": x_full.columns, "coefficient": coef})

    pred = df[[date_col, group_col, y_col]].copy()
    pred["is_train"] = train_mask
    pred["is_complete_train"] = complete_train
    pred["y_actual"] = y
    pred["y_pred"] = y_pred
    pred["residual"] = pred["y_actual"] - pred["y_pred"]

    long_rows = []
    baseline_pred = np.full(len(df), intercept, dtype=float)
    for feature, beta in zip(x_full.columns, coef):
        contrib = x_full[feature].to_numpy(dtype=float) * beta
        if feature in media_cols:
            channel = feature.replace("__media_x", "")
            pred[f"{channel}_estimated_contribution"] = contrib
            long_rows.extend(
                {
                    "date": pred.loc[i, date_col],
                    "geo_id": pred.loc[i, group_col],
                    "variable": channel,
                    "component": "media",
                    "estimated_contribution": float(contrib[i]),
                }
                for i in range(len(pred))
            )
        else:
            baseline_pred += contrib
    pred["estimated_baseline"] = baseline_pred

    selected = pd.DataFrame(transform_rows)
    curves = []
    coef_lookup = dict(zip(coef_df["feature"], coef_df["coefficient"]))
    for _, row in selected.iterrows():
        channel = row["channel"]
        support_col = f"{channel}{support_suffix}"
        spend_col = f"{channel}{spend_suffix}"
        current_support = finite_median_positive(df.loc[train_mask, support_col], fallback=row["anchor_support"])
        current_spend = finite_median_positive(df.loc[train_mask, spend_col], fallback=np.nan) if spend_col in df else np.nan
        beta = float(coef_lookup.get(f"{channel}__media_x", 0.0))
        for pct in np.linspace(0.0, 2.5, 101):
            support = current_support * pct
            steady = support / max(1.0 - float(row["decay"]), 1e-6)
            sat = apply_saturation([steady], row["curve_param"], shape=row["shape"], curve_type=row["curve_type"])[0]
            contribution = beta * sat
            curves.append(
                {
                    "channel": channel,
                    "pct_of_current_support": float(pct),
                    "support": float(support),
                    "spend": float(current_spend * pct) if np.isfinite(current_spend) else np.nan,
                    "steady_state_adstock": float(steady),
                    "estimated_incremental_contribution": float(contribution),
                    "estimated_roi_like": float(contribution / (current_spend * pct)) if pct > 0 and np.isfinite(current_spend) and current_spend > 0 else np.nan,
                    "curve_type": row["curve_type"],
                    "decay": float(row["decay"]),
                    "curve_param": float(row["curve_param"]),
                    "shape": float(row["shape"]),
                }
            )

    settings = {
        "model_type": "transformed_ridge_mmm_baseline",
        "curve_type": curve_type,
        "channels": channels,
        "controls": controls,
        "holdout_weeks": holdout_weeks,
        "ridge_lambda": ridge_lambda,
        "positive_media": positive_media,
        "purpose": "known_truth_benchmark_for_nmmm",
    }

    return TransformedRidgeMMMResult(
        predictions=pred,
        long_decomp=pd.DataFrame(long_rows),
        selected_transforms=selected,
        response_curves=pd.DataFrame(curves),
        coefficients=coef_df,
        settings=settings,
    )
