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
    curve_targets: np.ndarray
    adstock_targets: np.ndarray
    saturation_targets: np.ndarray
    confidence_targets: np.ndarray
    fallback_targets: np.ndarray


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


def _curve_target(response_curves: pd.DataFrame, channel: str, grid: np.ndarray = CURVE_GRID) -> np.ndarray:
    curve = response_curves.loc[response_curves["channel"].astype(str).eq(str(channel))].copy()
    if curve.empty:
        return np.linspace(0.0, 1.0, len(grid))
    pct = pd.to_numeric(curve["pct_of_anchor_support"], errors="coerce").to_numpy(float)
    y = pd.to_numeric(curve["true_incremental_contribution"], errors="coerce").to_numpy(float)
    ok = np.isfinite(pct) & np.isfinite(y)
    if ok.sum() < 4:
        return np.linspace(0.0, 1.0, len(grid))
    order = np.argsort(pct[ok])
    target = np.interp(grid, pct[ok][order], y[ok][order])
    target = np.maximum(target, 0.0)
    scale = float(np.max(target))
    if scale <= 1e-12:
        return np.linspace(0.0, 1.0, len(grid))
    target = target / scale
    target[0] = 0.0
    target = np.maximum.accumulate(target)
    return np.clip(target, 0.0, 1.0)


def _target_saturation(response_curves: pd.DataFrame, channel: str) -> float:
    curve = response_curves.loc[response_curves["channel"].astype(str).eq(str(channel))].copy()
    if curve.empty or "true_saturation" not in curve.columns:
        return 0.50
    curve["_dist"] = (pd.to_numeric(curve["pct_of_anchor_support"], errors="coerce") - 1.0).abs()
    row = curve.sort_values("_dist").head(1)
    return float(np.clip(pd.to_numeric(row["true_saturation"], errors="coerce").iloc[0], 0.0, 1.0))


def _difficulty_targets(row: pd.Series) -> tuple[float, float]:
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
    return confidence, fallback


def build_curve_prior_dataset(
    panels: List[Any],
    media_feature_inputs: Optional[List[str]] = None,
    controls: Optional[List[str]] = None,
    grid: np.ndarray = CURVE_GRID,
) -> CurvePriorDataset:
    """Create per-channel curve-prior training examples from synthetic panels."""
    media_feature_inputs = media_feature_inputs or ["support", "spend"]
    rows: List[Dict[str, Any]] = []
    curve_targets: List[np.ndarray] = []
    adstock_targets: List[float] = []
    saturation_targets: List[float] = []
    confidence_targets: List[float] = []
    fallback_targets: List[float] = []

    for panel_id, synth in enumerate(panels):
        panel = synth.panel.copy()
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
            feature_row: Dict[str, Any] = {
                "panel_id": panel_id,
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
            other_corrs = []
            for other, other_support in support_series.items():
                if other != ch:
                    other_corrs.append(abs(_safe_corr(support, other_support)))
            feature_row["max_abs_other_channel_support_corr"] = float(np.nanmax(other_corrs)) if other_corrs else 0.0
            feature_row["mean_abs_other_channel_support_corr"] = float(np.nanmean(other_corrs)) if other_corrs else 0.0
            if ch in learn.index:
                for col, val in learn.loc[ch].items():
                    if isinstance(val, (bool, np.bool_)):
                        feature_row[col] = float(val)
                    elif isinstance(val, str):
                        continue
                    else:
                        feature_row[col] = float(val) if pd.notna(val) else 0.0
                conf, fallback = _difficulty_targets(learn.loc[ch])
            else:
                conf, fallback = 0.50, 0.50
            adstock = float(params["decay"].iloc[0]) if not params.empty and "decay" in params.columns else 0.30
            rows.append(feature_row)
            curve_targets.append(_curve_target(synth.response_curves, ch, grid=grid))
            adstock_targets.append(float(np.clip(adstock, 0.0, 0.95)))
            saturation_targets.append(_target_saturation(synth.response_curves, ch))
            confidence_targets.append(conf)
            fallback_targets.append(fallback)

    features = pd.DataFrame(rows)
    excluded = {"panel_id", "channel", "truth_curve_type"}
    feature_columns = [c for c in features.columns if c not in excluded and pd.api.types.is_numeric_dtype(features[c])]
    features[feature_columns] = features[feature_columns].replace([np.inf, -np.inf], np.nan).fillna(0.0)
    return CurvePriorDataset(
        features=features,
        feature_columns=feature_columns,
        curve_targets=np.vstack(curve_targets).astype(float),
        adstock_targets=np.asarray(adstock_targets, dtype=float),
        saturation_targets=np.asarray(saturation_targets, dtype=float),
        confidence_targets=np.asarray(confidence_targets, dtype=float),
        fallback_targets=np.asarray(fallback_targets, dtype=float),
    )


def build_monotone_curve_prior_net(input_dim: int, curve_points: int = len(CURVE_GRID), hidden_size: int = 96, dropout: float = 0.05):
    """Create the neural curve-prior model."""
    torch, nn, F = _require_torch_nn()

    class MonotoneCurvePriorNet(nn.Module):
        def __init__(self):
            super().__init__()
            self.backbone = nn.Sequential(
                nn.Linear(input_dim, hidden_size),
                nn.LayerNorm(hidden_size),
                nn.SiLU(),
                nn.Dropout(dropout),
                nn.Linear(hidden_size, hidden_size),
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

        def forward(self, x):
            h = self.backbone(x)
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
) -> CurvePriorResult:
    """Fit a monotone neural curve-prior model on synthetic known truth."""
    torch, nn, F = _require_torch_nn()
    torch.manual_seed(seed)
    rng = np.random.default_rng(seed)
    x_np = dataset.features[dataset.feature_columns].to_numpy(float)
    x_center = np.mean(x_np, axis=0)
    x_scale = np.std(x_np, axis=0)
    x_scale = np.where(x_scale > 1e-8, x_scale, 1.0)
    x_np = (x_np - x_center) / x_scale
    n = x_np.shape[0]
    idx = np.arange(n)
    rng.shuffle(idx)
    n_val = max(1, int(round(n * validation_fraction))) if n >= 8 else max(0, n // 5)
    val_idx = idx[:n_val]
    train_idx = idx[n_val:] if n_val else idx

    device = torch.device("cpu")
    model = build_monotone_curve_prior_net(
        input_dim=len(dataset.feature_columns),
        curve_points=dataset.curve_targets.shape[1],
        hidden_size=hidden_size,
        dropout=dropout,
    ).to(device)
    optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate, weight_decay=weight_decay)
    x = torch.tensor(x_np, dtype=torch.float32, device=device)
    curve_y = torch.tensor(dataset.curve_targets, dtype=torch.float32, device=device)
    adstock_y = torch.tensor(dataset.adstock_targets, dtype=torch.float32, device=device)
    saturation_y = torch.tensor(dataset.saturation_targets, dtype=torch.float32, device=device)
    confidence_y = torch.tensor(dataset.confidence_targets, dtype=torch.float32, device=device)
    fallback_y = torch.tensor(dataset.fallback_targets, dtype=torch.float32, device=device)
    train_t = torch.tensor(train_idx, dtype=torch.long, device=device)
    val_t = torch.tensor(val_idx, dtype=torch.long, device=device) if len(val_idx) else train_t
    history = []
    best_state = None
    best_val = float("inf")
    patience = 80
    stale = 0

    def loss_for(indices):
        out = model(x[indices])
        curve_loss = F.mse_loss(out["curve"], curve_y[indices])
        pred_marginal = out["curve"][:, 1:] - out["curve"][:, :-1]
        true_marginal = curve_y[indices][:, 1:] - curve_y[indices][:, :-1]
        marginal_loss = F.mse_loss(pred_marginal, true_marginal)
        scalar_loss = (
            F.mse_loss(out["adstock_decay"], adstock_y[indices])
            + F.mse_loss(out["saturation_score"], saturation_y[indices])
            + F.mse_loss(out["confidence"], confidence_y[indices])
            + F.mse_loss(out["fallback_weight"], fallback_y[indices])
        )
        smoothness = torch.mean((pred_marginal[:, 1:] - pred_marginal[:, :-1]) ** 2)
        return curve_loss + 0.50 * marginal_loss + 0.25 * scalar_loss + 0.02 * smoothness

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
        out = model(x)
    metadata_cols = [c for c in ["panel_id", "channel", "truth_curve_type"] if c in dataset.features.columns]
    pred = dataset.features[metadata_cols].copy()
    curve_pred = np.asarray(out["curve"].detach().cpu().tolist(), dtype=float)
    for i, pct in enumerate(CURVE_GRID):
        pred[f"curve_prior_p{int(round(pct * 100)):03d}"] = curve_pred[:, i]
    pred["adstock_decay_prior_mean"] = np.asarray(out["adstock_decay"].detach().cpu().tolist(), dtype=float)
    pred["saturation_score_prior_mean"] = np.asarray(out["saturation_score"].detach().cpu().tolist(), dtype=float)
    pred["confidence_score"] = np.asarray(out["confidence"].detach().cpu().tolist(), dtype=float)
    pred["fallback_default_weight"] = np.asarray(out["fallback_weight"].detach().cpu().tolist(), dtype=float)
    pred["uncertainty_width"] = np.asarray(out["uncertainty_width"].detach().cpu().tolist(), dtype=float)
    pred["true_adstock_decay"] = dataset.adstock_targets
    pred["true_saturation_score"] = dataset.saturation_targets
    pred["true_confidence_score"] = dataset.confidence_targets
    pred["true_fallback_default_weight"] = dataset.fallback_targets
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
            "epochs_requested": int(epochs),
            "epochs_run": int(len(history)),
            "validation_fraction": validation_fraction,
            "feature_center": x_center.tolist(),
            "feature_scale": x_scale.tolist(),
            "model_type": "monotone_neural_curve_prior_net",
            "scope": "curve/adstock prior builder, not final causal ROI model",
        },
        model_state={"state_dict": model.state_dict(), "feature_columns": dataset.feature_columns},
    )


def evaluate_curve_prior_predictions(result: CurvePriorResult) -> pd.DataFrame:
    pred = result.predictions
    curve_cols = [c for c in pred.columns if c.startswith("curve_prior_p")]
    true_cols = [c for c in pred.columns if c.startswith("true_curve_p")]
    rows = []
    if curve_cols and true_cols:
        est = pred[curve_cols].to_numpy(float)
        truth = pred[true_cols].to_numpy(float)
        mae = np.mean(np.abs(est - truth), axis=1)
        corr = []
        for e, t in zip(est, truth):
            corr.append(np.corrcoef(e, t)[0, 1] if np.std(e) > 1e-10 and np.std(t) > 1e-10 else np.nan)
        rows.extend(
            [
                {"metric": "curve_grid_mae_mean", "value": float(np.nanmean(mae))},
                {"metric": "curve_grid_mae_median", "value": float(np.nanmedian(mae))},
                {"metric": "curve_shape_corr_median", "value": float(np.nanmedian(corr))},
                {"metric": "curve_monotonic_violation_share", "value": float(np.mean(np.diff(est, axis=1) < -1e-8))},
            ]
        )
    for est_col, true_col, metric in [
        ("adstock_decay_prior_mean", "true_adstock_decay", "adstock_decay_mae"),
        ("saturation_score_prior_mean", "true_saturation_score", "saturation_score_mae"),
        ("confidence_score", "true_confidence_score", "confidence_score_mae"),
        ("fallback_default_weight", "true_fallback_default_weight", "fallback_weight_mae"),
    ]:
        if est_col in pred.columns and true_col in pred.columns:
            rows.append({"metric": metric, "value": float(np.mean(np.abs(pred[est_col] - pred[true_col])))})
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
