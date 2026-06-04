"""Training utilities for the optional Torch NMMM model."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Any

import numpy as np
import pandas as pd

from .torch_models import build_interpretable_neural_mmm


@dataclass
class TorchNMMMResult:
    predictions: pd.DataFrame
    long_decomp: pd.DataFrame
    learned_params: pd.DataFrame
    response_curves: pd.DataFrame
    training_history: pd.DataFrame
    settings: Dict[str, object]
    model_state: Optional[Dict[str, Any]] = None


def save_torch_nmmm_checkpoint(
    result: TorchNMMMResult,
    path: str | Path,
    extra: Optional[Dict[str, Any]] = None,
) -> None:
    """Save reusable neural model weights plus training metadata."""
    if result.model_state is None:
        raise ValueError("No model_state is available on this TorchNMMMResult.")
    torch = _require_torch()
    payload = {
        "model_state": result.model_state,
        "settings": result.settings,
        "extra": extra or {},
        "learned_params": result.learned_params.to_dict(orient="records"),
        "training_history": result.training_history.to_dict(orient="records"),
    }
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    torch.save(payload, path)


def load_torch_nmmm_checkpoint(path: str | Path) -> Dict[str, Any]:
    """Load a saved NMMM checkpoint payload."""
    torch = _require_torch()
    return torch.load(Path(path), map_location="cpu")


def _require_torch():
    try:
        import torch
    except Exception as exc:  # pragma: no cover - depends on optional install.
        raise ImportError(
            "Torch is required for neural NMMM training. Install with `pip install torch`."
        ) from exc
    return torch


def _infer_channels(df: pd.DataFrame, support_suffix: str) -> List[str]:
    return sorted([c[: -len(support_suffix)] for c in df.columns if c.endswith(support_suffix)])


def _balanced_arrays(
    panel: pd.DataFrame,
    channels: List[str],
    controls: List[str],
    date_col: str,
    group_col: str,
    y_col: str,
    support_suffix: str,
    spend_suffix: str = "_spend",
    media_feature_inputs: Optional[List[str]] = None,
    market_size_col: Optional[str] = None,
) -> Dict[str, object]:
    df = panel.copy()
    df[date_col] = pd.to_datetime(df[date_col])
    groups = sorted(df[group_col].astype(str).unique())
    dates = sorted(df[date_col].unique())
    idx = pd.MultiIndex.from_product([groups, dates], names=[group_col, date_col])
    wide = df.set_index([group_col, date_col]).reindex(idx).reset_index()
    y = pd.to_numeric(wide[y_col], errors="coerce").to_numpy(float).reshape(len(groups), len(dates))
    media_feature_inputs = media_feature_inputs or ["support"]
    media = np.zeros((len(groups), len(dates), len(channels), len(media_feature_inputs)), dtype=float)
    media_missing = np.zeros_like(media, dtype=bool)
    for j, channel in enumerate(channels):
        for f_i, feature_name in enumerate(media_feature_inputs):
            if feature_name == "support":
                col = f"{channel}{support_suffix}"
            elif feature_name == "spend":
                col = f"{channel}{spend_suffix}"
            else:
                col = f"{channel}_{feature_name}"
            if col in wide.columns:
                parsed = pd.to_numeric(wide[col], errors="coerce")
                missing = parsed.isna().to_numpy(bool).reshape(len(groups), len(dates))
                values = parsed.fillna(0.0).to_numpy(float).reshape(len(groups), len(dates))
            else:
                values = np.zeros((len(groups), len(dates)), dtype=float)
                missing = np.ones((len(groups), len(dates)), dtype=bool)
            media[:, :, j, f_i] = values
            media_missing[:, :, j, f_i] = missing
    control_arr = np.zeros((len(groups), len(dates), len(controls)), dtype=float)
    for k, control in enumerate(controls):
        control_arr[:, :, k] = (
            pd.to_numeric(wide[control], errors="coerce")
            .fillna(0.0)
            .to_numpy(float)
            .reshape(len(groups), len(dates))
        )
    market_size = None
    if market_size_col and market_size_col in wide.columns:
        market_size = (
            pd.to_numeric(wide[market_size_col], errors="coerce")
            .to_numpy(float)
            .reshape(len(groups), len(dates))
        )
    return {
        "wide": wide,
        "groups": groups,
        "dates": dates,
        "y": y,
        "media": media,
        "media_missing": media_missing,
        "media_feature_inputs": media_feature_inputs,
        "controls": control_arr,
        "market_size": market_size,
    }


def fit_torch_nmmm(
    panel: pd.DataFrame,
    truth_media: Optional[pd.DataFrame] = None,
    channels: Optional[List[str]] = None,
    date_col: str = "date",
    group_col: str = "geo_id",
    y_col: str = "kpi",
    support_suffix: str = "_support",
    spend_suffix: str = "_spend",
    media_feature_inputs: Optional[List[str]] = None,
    controls: Optional[List[str]] = None,
    curve_type: str = "hill",
    holdout_weeks: int = 13,
    epochs: int = 1200,
    learning_rate: float = 0.02,
    weight_decay: float = 1e-4,
    validation_weeks: int = 8,
    early_stopping_patience: int = 150,
    early_stopping_min_delta: float = 1e-5,
    seed: int = 20260603,
    verbose_every: int = 200,
    initial_decay: float = 0.35,
    initial_curve_param: float = 2.0,
    initial_shape: float = 1.0,
    initial_media_coef: float = 0.15,
    initialize_from_baseline: bool = False,
    hierarchical_media: bool = True,
    group_media_max_log_multiplier: float = 0.25,
    group_media_shrinkage: float = 0.05,
    market_size_col: Optional[str] = None,
    scale_media_by_market_size: bool = False,
    contribution_supervision_weight: float = 0.0,
    truth_channel_col: str = "channel",
    truth_contribution_col: str = "true_contribution",
) -> TorchNMMMResult:
    """Train the constrained neural MMM on a balanced panel."""
    torch = _require_torch()
    torch.manual_seed(seed)
    df = panel.copy()
    df[date_col] = pd.to_datetime(df[date_col])
    channels = channels or _infer_channels(df, support_suffix)
    controls = controls or [c for c in ["promo", "holiday", "price_index"] if c in df.columns]
    media_feature_inputs = media_feature_inputs or ["support", "spend"]
    arrays = _balanced_arrays(
        df,
        channels,
        controls,
        date_col,
        group_col,
        y_col,
        support_suffix,
        spend_suffix,
        media_feature_inputs,
        market_size_col,
    )
    y = arrays["y"]
    media = arrays["media"]
    controls_arr = arrays["controls"]
    market_size = arrays["market_size"]
    groups = arrays["groups"]
    dates = arrays["dates"]
    wide = arrays["wide"]

    n_groups, n_time, n_channels, n_media_features = media.shape
    train_time_mask = np.ones(n_time, dtype=bool)
    if holdout_weeks > 0:
        train_time_mask[-int(holdout_weeks) :] = False
    train_mask = np.broadcast_to(train_time_mask.reshape(1, -1), (n_groups, n_time))
    fit_time_mask = train_time_mask.copy()
    val_time_mask = np.zeros(n_time, dtype=bool)
    if validation_weeks > 0 and train_time_mask.sum() > validation_weeks + 8:
        train_indices = np.where(train_time_mask)[0]
        val_idx = train_indices[-int(validation_weeks) :]
        val_time_mask[val_idx] = True
        fit_time_mask[val_idx] = False
    fit_mask = np.broadcast_to(fit_time_mask.reshape(1, -1), (n_groups, n_time))
    val_mask = np.broadcast_to(val_time_mask.reshape(1, -1), (n_groups, n_time))

    market_size_reference = np.nan
    if scale_media_by_market_size:
        if market_size is None:
            raise ValueError("scale_media_by_market_size=True requires market_size_col.")
        market_train = market_size[train_mask]
        market_train = market_train[np.isfinite(market_train) & (market_train > 0)]
        if market_train.size == 0:
            raise ValueError("market_size_col has no positive finite training values.")
        market_size_reference = float(np.median(market_train))
        market_rel = np.where(np.isfinite(market_size) & (market_size > 0), market_size / market_size_reference, np.nan)
        group_rel = np.nanmedian(market_rel, axis=1)
        group_rel = np.where(np.isfinite(group_rel) & (group_rel > 0), group_rel, 1.0)
        media = media / group_rel.reshape(-1, 1, 1, 1)

    y_train = y[train_mask & np.isfinite(y)]
    y_center = float(np.mean(y_train))
    y_scale = float(np.std(y_train) if np.std(y_train) > 1e-8 else 1.0)
    y_scaled = (np.nan_to_num(y, nan=y_center) - y_center) / y_scale

    media_scale = np.ones(n_channels, dtype=float)
    media_feature_scale = np.ones((n_channels, n_media_features), dtype=float)
    for j in range(n_channels):
        vals = media[:, :, j, 0][train_mask]
        vals = vals[np.isfinite(vals) & (vals > 0)]
        media_scale[j] = float(np.median(vals)) if vals.size else 1.0
        for f_i in range(n_media_features):
            f_vals = media[:, :, j, f_i][train_mask]
            f_vals = f_vals[np.isfinite(f_vals) & (f_vals > 0)]
            media_feature_scale[j, f_i] = float(np.median(f_vals)) if f_vals.size else 1.0
    media_scaled = media / media_feature_scale.reshape(1, 1, n_channels, n_media_features)

    control_center = np.zeros(len(controls), dtype=float)
    control_scale = np.ones(len(controls), dtype=float)
    if len(controls):
        for k in range(len(controls)):
            vals = controls_arr[:, :, k][train_mask]
            control_center[k] = float(np.nanmean(vals))
            sd = float(np.nanstd(vals))
            control_scale[k] = sd if sd > 1e-8 else 1.0
        controls_scaled = (controls_arr - control_center.reshape(1, 1, -1)) / control_scale.reshape(1, 1, -1)
    else:
        controls_scaled = controls_arr

    contribution_target_scaled = None
    contribution_target_mask = None
    if truth_media is not None and contribution_supervision_weight > 0:
        truth = truth_media.copy()
        truth[date_col] = pd.to_datetime(truth[date_col])
        truth_grid = pd.DataFrame(
            {
                group_col: np.repeat(groups, n_time),
                date_col: list(dates) * n_groups,
            }
        )
        target = np.zeros((n_groups, n_time, n_channels), dtype=float)
        target_mask = np.zeros((n_groups, n_time, n_channels), dtype=bool)
        for j, channel in enumerate(channels):
            ch_truth = truth.loc[truth[truth_channel_col].astype(str).eq(str(channel))]
            if ch_truth.empty:
                continue
            ch_sum = (
                ch_truth.groupby([group_col, date_col], as_index=False)[truth_contribution_col]
                .sum()
                .rename(columns={truth_contribution_col: "_true_contribution"})
            )
            merged = truth_grid.merge(ch_sum, on=[group_col, date_col], how="left")
            values = pd.to_numeric(merged["_true_contribution"], errors="coerce").to_numpy(float).reshape(n_groups, n_time)
            target[:, :, j] = np.nan_to_num(values, nan=0.0) / max(y_scale, 1e-6)
            target_mask[:, :, j] = np.isfinite(values)
        contribution_target_scaled = target
        contribution_target_mask = target_mask & np.broadcast_to(fit_time_mask.reshape(1, -1, 1), target_mask.shape)

    init_decay = initial_decay
    init_curve_param = initial_curve_param
    init_shape = initial_shape
    init_media_coef = initial_media_coef
    baseline_init_used = False
    if initialize_from_baseline:
        from .baselines import fit_transformed_ridge_mmm

        baseline_result = fit_transformed_ridge_mmm(
            df,
            channels=channels,
            date_col=date_col,
            group_col=group_col,
            y_col=y_col,
            support_suffix=support_suffix,
            spend_suffix=spend_suffix,
            controls=controls,
            curve_type=curve_type,
            holdout_weeks=holdout_weeks,
            ridge_lambda=25.0,
            positive_media=True,
        )
        selected = baseline_result.selected_transforms.set_index("channel") if not baseline_result.selected_transforms.empty else pd.DataFrame()
        coef_lookup = dict(zip(baseline_result.coefficients["feature"], baseline_result.coefficients["coefficient"]))
        init_decay = []
        init_curve_param = []
        init_shape = []
        init_media_coef = []
        for j, channel in enumerate(channels):
            if channel in selected.index:
                init_decay.append(float(selected.loc[channel, "decay"]))
                init_curve_param.append(max(float(selected.loc[channel, "curve_param"]) / max(media_scale[j], 1e-6), 1e-6))
                init_shape.append(float(selected.loc[channel, "shape"]))
                beta_raw_y = max(float(coef_lookup.get(f"{channel}__media_x", 0.0)), 1e-6)
                init_media_coef.append(max(beta_raw_y / max(y_scale, 1e-6), 1e-6))
            else:
                init_decay.append(float(initial_decay))
                init_curve_param.append(float(initial_curve_param))
                init_shape.append(float(initial_shape))
                init_media_coef.append(float(initial_media_coef))
        baseline_init_used = True

    t = np.linspace(0.0, 1.0, n_time, dtype=float).reshape(1, n_time, 1)
    time_features = np.concatenate(
        [
            np.broadcast_to(t, (n_groups, n_time, 1)),
            np.sin(2 * np.pi * np.broadcast_to(t, (n_groups, n_time, 1))),
            np.cos(2 * np.pi * np.broadcast_to(t, (n_groups, n_time, 1))),
            np.ones((n_groups, n_time, 1), dtype=float),
        ],
        axis=-1,
    )

    device = torch.device("cpu")
    model = build_interpretable_neural_mmm(
        n_channels=n_channels,
        n_media_features=n_media_features,
        n_controls=len(controls),
        n_groups=n_groups,
        curve_type=curve_type,
        initial_decay=init_decay,
        initial_curve_param=init_curve_param,
        initial_shape=init_shape,
        initial_media_coef=init_media_coef,
        hierarchical_media=hierarchical_media,
        group_media_max_log_multiplier=group_media_max_log_multiplier,
    ).to(device)
    optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate, weight_decay=weight_decay)
    # Use Python lists instead of Torch's NumPy bridge. Some local Torch wheels
    # are compiled against NumPy 1.x and fail with NumPy 2.x arrays.
    media_t = torch.tensor(media_scaled.tolist(), dtype=torch.float32, device=device)
    controls_t = torch.tensor(controls_scaled.tolist(), dtype=torch.float32, device=device)
    y_t = torch.tensor(y_scaled.tolist(), dtype=torch.float32, device=device)
    mask_t = torch.tensor((fit_mask & np.isfinite(y)).tolist(), dtype=torch.bool, device=device)
    val_mask_t = torch.tensor((val_mask & np.isfinite(y)).tolist(), dtype=torch.bool, device=device)
    contribution_target_t = None
    contribution_target_mask_t = None
    if contribution_target_scaled is not None and contribution_target_mask is not None:
        contribution_target_t = torch.tensor(contribution_target_scaled.tolist(), dtype=torch.float32, device=device)
        contribution_target_mask_t = torch.tensor(contribution_target_mask.tolist(), dtype=torch.bool, device=device)
    group_index = torch.arange(n_groups, dtype=torch.long, device=device)
    time_t = torch.tensor(time_features.tolist(), dtype=torch.float32, device=device)

    history = []
    best_state = None
    best_val_loss = None
    epochs_without_improvement = 0
    for epoch in range(1, int(epochs) + 1):
        optimizer.zero_grad()
        out = model(media_t, controls_t if len(controls) else None, group_index, time_t)
        err = out["y_hat"][mask_t] - y_t[mask_t]
        loss = torch.mean(err**2)
        media_penalty = 0.0005 * torch.mean(out["media_contribution"][mask_t] ** 2)
        if hierarchical_media:
            group_penalty = group_media_shrinkage * torch.mean(out["group_media_raw"] ** 2)
        else:
            group_penalty = torch.tensor(0.0, dtype=torch.float32, device=device)
        if contribution_target_t is not None and bool(contribution_target_mask_t.any().item()):
            contribution_err = out["media_contribution"][contribution_target_mask_t] - contribution_target_t[contribution_target_mask_t]
            contribution_loss = torch.mean(contribution_err**2)
        else:
            contribution_loss = torch.tensor(0.0, dtype=torch.float32, device=device)
        total_loss = loss + media_penalty + group_penalty + float(contribution_supervision_weight) * contribution_loss
        total_loss.backward()
        optimizer.step()
        with torch.no_grad():
            if bool(val_mask_t.any().item()):
                val_err = out["y_hat"][val_mask_t] - y_t[val_mask_t]
                val_loss = torch.mean(val_err**2)
                val_loss_value = float(val_loss.detach().cpu().item())
            else:
                val_loss_value = float(loss.detach().cpu().item())
        if best_val_loss is None or val_loss_value < best_val_loss - early_stopping_min_delta:
            best_val_loss = val_loss_value
            best_state = {k: v.detach().clone() for k, v in model.state_dict().items()}
            epochs_without_improvement = 0
        else:
            epochs_without_improvement += 1
        if epoch == 1 or epoch == epochs or (verbose_every and epoch % verbose_every == 0):
            history.append(
                {
                    "epoch": epoch,
                    "train_mse_scaled": float(loss.detach().cpu().item()),
                    "validation_mse_scaled": val_loss_value,
                    "media_penalty": float(media_penalty.detach().cpu().item()),
                    "group_media_penalty": float(group_penalty.detach().cpu().item()),
                    "contribution_supervision_loss": float(contribution_loss.detach().cpu().item()),
                    "objective": float(total_loss.detach().cpu().item()),
                    "best_validation_mse_scaled": best_val_loss,
                }
            )
        if early_stopping_patience and epochs_without_improvement >= int(early_stopping_patience):
            history.append(
                {
                    "epoch": epoch,
                    "train_mse_scaled": float(loss.detach().cpu().item()),
                    "validation_mse_scaled": val_loss_value,
                    "media_penalty": float(media_penalty.detach().cpu().item()),
                    "group_media_penalty": float(group_penalty.detach().cpu().item()),
                    "contribution_supervision_loss": float(contribution_loss.detach().cpu().item()),
                    "objective": float(total_loss.detach().cpu().item()),
                    "best_validation_mse_scaled": best_val_loss,
                    "early_stopped": True,
                }
            )
            break

    if best_state is not None:
        model.load_state_dict(best_state)

    with torch.no_grad():
        out = model(media_t, controls_t if len(controls) else None, group_index, time_t)
        y_hat_scaled = np.array(out["y_hat"].detach().cpu().tolist(), dtype=float)
        baseline_scaled = np.array(out["baseline"].detach().cpu().tolist(), dtype=float)
        media_contrib_scaled = np.array(out["media_contribution"].detach().cpu().tolist(), dtype=float)
        decay = np.array(out["decay"].detach().cpu().tolist(), dtype=float)
        curve_param = np.array(out["curve_param"].detach().cpu().tolist(), dtype=float)
        shape = np.array(out["shape"].detach().cpu().tolist(), dtype=float)
        coef = np.array(out["coef"].detach().cpu().tolist(), dtype=float)
        group_media_multiplier = np.array(out["group_media_multiplier"].detach().cpu().tolist(), dtype=float)
        media_feature_weights = np.array(out["media_feature_weights"].detach().cpu().tolist(), dtype=float)

    pred = wide[[date_col, group_col]].copy()
    pred["is_train"] = train_mask.reshape(-1)
    pred["y_actual"] = y.reshape(-1)
    pred["y_pred"] = (y_center + y_scale * y_hat_scaled).reshape(-1)
    pred["estimated_baseline"] = (y_center + y_scale * baseline_scaled).reshape(-1)
    pred["residual"] = pred["y_actual"] - pred["y_pred"]
    long_rows = []
    for j, channel in enumerate(channels):
        contrib = y_scale * media_contrib_scaled[:, :, j]
        pred[f"{channel}_estimated_contribution"] = contrib.reshape(-1)
        for g_i, group in enumerate(groups):
            for t_i, date in enumerate(dates):
                long_rows.append(
                    {
                        "date": date,
                        "geo_id": group,
                        "variable": channel,
                        "component": "media",
                        "group_media_multiplier": float(group_media_multiplier[g_i, j]),
                        "estimated_contribution": float(contrib[g_i, t_i]),
                    }
                )

    params = pd.DataFrame(
        {
            "channel": channels,
            "curve_type": curve_type,
            "decay": decay,
            "curve_param_on_scaled_adstock": curve_param,
            "shape": shape,
            "coef_on_scaled_y": coef,
            "media_support_scale": media_scale,
            "media_feature_inputs": ",".join(media_feature_inputs),
            "market_size_col": market_size_col,
            "scale_media_by_market_size": bool(scale_media_by_market_size),
            "market_size_reference": market_size_reference,
            "coef_on_raw_y": coef * y_scale,
        }
    )

    group_params = []
    for g_i, group in enumerate(groups):
        for j, channel in enumerate(channels):
            group_params.append(
                {
                    "channel": channel,
                    "geo_id": group,
                    "group_media_multiplier": float(group_media_multiplier[g_i, j]),
                }
            )
    group_params_df = pd.DataFrame(group_params)

    feature_params = []
    for j, channel in enumerate(channels):
        for f_i, feature_name in enumerate(media_feature_inputs):
            feature_params.append(
                {
                    "channel": channel,
                    "media_feature": feature_name,
                    "media_feature_weight": float(media_feature_weights[j, f_i]),
                    "media_feature_scale": float(media_feature_scale[j, f_i]),
                }
            )
    feature_params_df = pd.DataFrame(feature_params)

    curves = []
    for j, channel in enumerate(channels):
        support_current = float(media_scale[j])
        spend_col = f"{channel}{spend_suffix}"
        current_spend = np.nan
        if spend_col in df.columns:
            vals = pd.to_numeric(df.loc[df[date_col].isin(dates[:-holdout_weeks] if holdout_weeks else dates), spend_col], errors="coerce")
            vals = vals[np.isfinite(vals) & (vals > 0)]
            current_spend = float(np.median(vals)) if len(vals) else np.nan
        for pct in np.linspace(0.0, 2.5, 101):
            support_scaled = pct
            steady_scaled = support_scaled / max(1.0 - float(decay[j]), 1e-6)
            if curve_type == "hill":
                sat = steady_scaled ** shape[j] / (steady_scaled ** shape[j] + curve_param[j] ** shape[j] + 1e-8)
            else:
                sat = 1.0 - np.exp(-((steady_scaled / max(curve_param[j], 1e-8)) ** shape[j]))
            contribution = y_scale * coef[j] * sat
            curves.append(
                {
                    "channel": channel,
                    "geo_id": "__global__",
                    "pct_of_current_support": float(pct),
                    "support": float(support_current * pct),
                    "spend": float(current_spend * pct) if np.isfinite(current_spend) else np.nan,
                    "estimated_incremental_contribution": float(contribution),
                    "estimated_roi_like": float(contribution / (current_spend * pct)) if pct > 0 and np.isfinite(current_spend) and current_spend > 0 else np.nan,
                    "curve_type": curve_type,
                    "decay": float(decay[j]),
                    "curve_param_on_scaled_adstock": float(curve_param[j]),
                    "shape": float(shape[j]),
                }
            )
            if hierarchical_media:
                for g_i, group in enumerate(groups):
                    curves.append(
                        {
                            "channel": channel,
                            "geo_id": group,
                            "pct_of_current_support": float(pct),
                            "support": float(support_current * pct),
                            "spend": float(current_spend * pct) if np.isfinite(current_spend) else np.nan,
                            "estimated_incremental_contribution": float(contribution * group_media_multiplier[g_i, j]),
                            "estimated_roi_like": float((contribution * group_media_multiplier[g_i, j]) / (current_spend * pct)) if pct > 0 and np.isfinite(current_spend) and current_spend > 0 else np.nan,
                            "curve_type": curve_type,
                            "decay": float(decay[j]),
                            "curve_param_on_scaled_adstock": float(curve_param[j]),
                            "shape": float(shape[j]),
                        }
                    )

    settings = {
        "model_type": "interpretable_neural_mmm",
        "curve_type": curve_type,
        "channels": channels,
        "controls": controls,
        "holdout_weeks": holdout_weeks,
        "validation_weeks": int(validation_weeks),
        "early_stopping_patience": int(early_stopping_patience),
        "early_stopping_min_delta": float(early_stopping_min_delta),
        "epochs": int(epochs),
        "learning_rate": float(learning_rate),
        "weight_decay": float(weight_decay),
        "initial_decay": initial_decay if isinstance(initial_decay, (int, float)) else "vector",
        "initial_curve_param": initial_curve_param if isinstance(initial_curve_param, (int, float)) else "vector",
        "initial_shape": initial_shape if isinstance(initial_shape, (int, float)) else "vector",
        "initial_media_coef": initial_media_coef if isinstance(initial_media_coef, (int, float)) else "vector",
        "media_feature_inputs": media_feature_inputs,
        "contribution_supervision_weight": float(contribution_supervision_weight),
        "contribution_supervision_used": bool(truth_media is not None and contribution_supervision_weight > 0),
        "initialize_from_baseline": bool(initialize_from_baseline),
        "baseline_init_used": bool(baseline_init_used),
        "hierarchical_media": bool(hierarchical_media),
        "group_media_max_log_multiplier": float(group_media_max_log_multiplier),
        "group_media_shrinkage": float(group_media_shrinkage),
        "market_size_col": market_size_col,
        "scale_media_by_market_size": bool(scale_media_by_market_size),
        "market_size_reference": market_size_reference,
        "target_scaling": {"center": y_center, "scale": y_scale},
        "note": "Research prototype. Compare contribution recovery against transparent baselines before trusting.",
    }
    model_state = {
        "state_dict": {k: v.detach().cpu() for k, v in model.state_dict().items()},
        "channels": channels,
        "controls": controls,
        "groups": groups,
        "dates": [str(d) for d in dates],
        "media_feature_inputs": media_feature_inputs,
        "media_feature_scale": media_feature_scale.tolist(),
        "media_support_scale": media_scale.tolist(),
        "control_center": control_center.tolist(),
        "control_scale": control_scale.tolist(),
        "target_scaling": {"center": y_center, "scale": y_scale},
        "model_constructor": {
            "n_channels": int(n_channels),
            "n_media_features": int(n_media_features),
            "n_controls": int(len(controls)),
            "n_groups": int(n_groups),
            "curve_type": curve_type,
            "hierarchical_media": bool(hierarchical_media),
            "group_media_max_log_multiplier": float(group_media_max_log_multiplier),
        },
    }
    return TorchNMMMResult(
        predictions=pred,
        long_decomp=pd.DataFrame(long_rows),
        learned_params=pd.concat(
            [
                params.assign(param_level="global"),
                group_params_df.assign(param_level="group"),
                feature_params_df.assign(param_level="media_feature"),
            ],
            ignore_index=True,
            sort=False,
        ),
        response_curves=pd.DataFrame(curves),
        training_history=pd.DataFrame(history),
        settings=settings,
        model_state=model_state,
    )
