"""Training loop for the hierarchical TFT MMM challenger."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional

import numpy as np
import pandas as pd

from .torch_tft_mmm import build_hierarchical_tft_mmm
from .torch_training import _balanced_arrays, _infer_channels, _require_torch


@dataclass
class TFTMMMResult:
    predictions: pd.DataFrame
    long_decomp: pd.DataFrame
    variable_economics: pd.DataFrame
    model_diagnostics: pd.DataFrame
    learned_params: pd.DataFrame
    response_curves: pd.DataFrame
    training_history: pd.DataFrame
    settings: Dict[str, object]
    model_state: Optional[Dict[str, Any]] = None


def save_tft_mmm_checkpoint(result: TFTMMMResult, path: str | Path, extra: Optional[Dict[str, Any]] = None) -> None:
    if result.model_state is None:
        raise ValueError("No model_state is available on this TFTMMMResult.")
    torch = _require_torch()
    payload = {
        "model_state": result.model_state,
        "settings": result.settings,
        "extra": extra or {},
        "channels": result.settings.get("channels"),
        "media_feature_inputs": result.settings.get("media_feature_inputs"),
        "controls": result.settings.get("controls"),
        "summary": (extra or {}).get("summary"),
        "variable_economics": result.variable_economics.to_dict(orient="records"),
        "model_diagnostics": result.model_diagnostics.to_dict(orient="records"),
        "learned_params": result.learned_params.to_dict(orient="records"),
        "training_history": result.training_history.to_dict(orient="records"),
    }
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    torch.save(payload, path)


def load_tft_mmm_checkpoint(path: str | Path) -> Dict[str, Any]:
    torch = _require_torch()
    return torch.load(Path(path), map_location="cpu")


def _truth_contribution_tensor(
    truth_media: pd.DataFrame,
    groups: List[str],
    dates: List[pd.Timestamp],
    channels: List[str],
    date_col: str,
    group_col: str,
    channel_col: str,
    contribution_col: str,
    y_scale: float,
    fit_time_mask: np.ndarray,
) -> tuple[np.ndarray, np.ndarray]:
    truth = truth_media.copy()
    truth[date_col] = pd.to_datetime(truth[date_col])
    grid = pd.DataFrame({group_col: np.repeat(groups, len(dates)), date_col: list(dates) * len(groups)})
    target = np.zeros((len(groups), len(dates), len(channels)), dtype=float)
    mask = np.zeros_like(target, dtype=bool)
    for j, channel in enumerate(channels):
        ch_truth = truth.loc[truth[channel_col].astype(str).eq(str(channel))]
        if ch_truth.empty:
            continue
        ch_sum = (
            ch_truth.groupby([group_col, date_col], as_index=False)[contribution_col]
            .sum()
            .rename(columns={contribution_col: "_true_contribution"})
        )
        merged = grid.merge(ch_sum, on=[group_col, date_col], how="left")
        values = pd.to_numeric(merged["_true_contribution"], errors="coerce").to_numpy(float).reshape(len(groups), len(dates))
        target[:, :, j] = np.nan_to_num(values, nan=0.0) / max(y_scale, 1e-6)
        mask[:, :, j] = np.isfinite(values)
    mask = mask & np.broadcast_to(fit_time_mask.reshape(1, -1, 1), mask.shape)
    return target, mask


def fit_tft_mmm(
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
    holdout_weeks: int = 13,
    validation_weeks: int = 8,
    epochs: int = 800,
    learning_rate: float = 0.004,
    weight_decay: float = 1e-4,
    contribution_supervision_weight: float = 0.25,
    counterfactual_consistency_weight: float = 0.05,
    counterfactual_penalty_every: int = 10,
    response_monotonicity_weight: float = 0.01,
    marginal_smoothness_weight: float = 0.001,
    response_penalty_every: int = 10,
    baseline_stealing_relative_threshold: float = 0.10,
    group_media_shrinkage: float = 0.03,
    contribution_l1_weight: float = 0.0001,
    early_stopping_patience: int = 120,
    early_stopping_min_delta: float = 1e-5,
    hidden_size: int = 48,
    n_heads: int = 4,
    dropout: float = 0.05,
    seed: int = 20260603,
    truth_channel_col: str = "channel",
    truth_contribution_col: str = "true_contribution",
    initial_model_state: Optional[Dict[str, Any]] = None,
) -> TFTMMMResult:
    """Fit a TFT-style MMM with explicit contribution outputs."""
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
        None,
    )
    y = arrays["y"]
    media = arrays["media"]
    media_missing = arrays.get("media_missing", np.zeros_like(media, dtype=bool))
    controls_arr = arrays["controls"]
    groups = arrays["groups"]
    dates = arrays["dates"]
    wide = arrays["wide"]
    n_groups, n_time, n_channels, n_media_features = media.shape

    train_time_mask = np.ones(n_time, dtype=bool)
    if holdout_weeks > 0:
        train_time_mask[-int(holdout_weeks) :] = False
    fit_time_mask = train_time_mask.copy()
    val_time_mask = np.zeros(n_time, dtype=bool)
    if validation_weeks > 0 and train_time_mask.sum() > validation_weeks + 8:
        train_idx = np.where(train_time_mask)[0]
        val_idx = train_idx[-int(validation_weeks) :]
        val_time_mask[val_idx] = True
        fit_time_mask[val_idx] = False
    train_mask = np.broadcast_to(train_time_mask.reshape(1, -1), (n_groups, n_time))
    fit_mask = np.broadcast_to(fit_time_mask.reshape(1, -1), (n_groups, n_time))
    val_mask = np.broadcast_to(val_time_mask.reshape(1, -1), (n_groups, n_time))

    y_train = y[train_mask & np.isfinite(y)]
    y_center = float(np.mean(y_train))
    y_scale = float(np.std(y_train) if np.std(y_train) > 1e-8 else 1.0)
    y_scaled = (np.nan_to_num(y, nan=y_center) - y_center) / y_scale

    media_feature_scale = np.ones((n_channels, n_media_features), dtype=float)
    support_scale = np.ones(n_channels, dtype=float)
    for j in range(n_channels):
        for f_i in range(n_media_features):
            vals = media[:, :, j, f_i][train_mask]
            vals = vals[np.isfinite(vals) & (vals > 0)]
            media_feature_scale[j, f_i] = float(np.median(vals)) if vals.size else 1.0
        support_scale[j] = media_feature_scale[j, 0]
    media_scaled = media / media_feature_scale.reshape(1, 1, n_channels, n_media_features)
    media_scaled = np.nan_to_num(media_scaled, nan=0.0, posinf=0.0, neginf=0.0)

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
        contribution_target_scaled, contribution_target_mask = _truth_contribution_tensor(
            truth_media,
            groups,
            dates,
            channels,
            date_col,
            group_col,
            truth_channel_col,
            truth_contribution_col,
            y_scale,
            fit_time_mask,
        )

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
    model = build_hierarchical_tft_mmm(
        n_channels=n_channels,
        n_media_features=n_media_features,
        n_controls=len(controls),
        n_groups=n_groups,
        hidden_size=hidden_size,
        n_heads=n_heads,
        dropout=dropout,
    ).to(device)
    if initial_model_state is not None:
        state_dict = initial_model_state.get("state_dict") if "state_dict" in initial_model_state else initial_model_state
        try:
            model.load_state_dict(state_dict, strict=False)
        except RuntimeError as exc:
            raise ValueError(
                "initial_model_state is not compatible with this TFT shape. "
                "Use the same channel count, media feature inputs, controls, groups, hidden_size, and heads."
            ) from exc
    optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate, weight_decay=weight_decay)

    media_t = torch.tensor(media_scaled.tolist(), dtype=torch.float32, device=device)
    media_missing_t = torch.tensor(media_missing.tolist(), dtype=torch.float32, device=device)
    controls_t = torch.tensor(controls_scaled.tolist(), dtype=torch.float32, device=device)
    y_t = torch.tensor(y_scaled.tolist(), dtype=torch.float32, device=device)
    fit_mask_t = torch.tensor((fit_mask & np.isfinite(y)).tolist(), dtype=torch.bool, device=device)
    val_mask_t = torch.tensor((val_mask & np.isfinite(y)).tolist(), dtype=torch.bool, device=device)
    group_index = torch.arange(n_groups, dtype=torch.long, device=device)
    time_t = torch.tensor(time_features.tolist(), dtype=torch.float32, device=device)
    contribution_target_t = None
    contribution_mask_t = None
    if contribution_target_scaled is not None:
        contribution_target_t = torch.tensor(contribution_target_scaled.tolist(), dtype=torch.float32, device=device)
        contribution_mask_t = torch.tensor(contribution_target_mask.tolist(), dtype=torch.bool, device=device)

    def _zero_channel(channel_index: int):
        zero_media = media_t.clone()
        zero_missing = media_missing_t.clone()
        zero_media[:, :, channel_index, :] = 0.0
        zero_missing[:, :, channel_index, :] = 0.0
        return zero_media, zero_missing

    def _counterfactual_losses(current_out):
        if counterfactual_consistency_weight <= 0 and baseline_stealing_relative_threshold <= 0:
            zero = torch.tensor(0.0, dtype=torch.float32, device=device)
            return zero, zero
        was_training = model.training
        model.eval()
        full_out = model(media_t, controls_t if len(controls) else None, group_index, time_t, media_missing=media_missing_t)
        cf_loss = torch.tensor(0.0, dtype=torch.float32, device=device)
        baseline_delta_loss = torch.tensor(0.0, dtype=torch.float32, device=device)
        for j in range(n_channels):
            zero_media, zero_missing = _zero_channel(j)
            zero_out = model(
                zero_media,
                controls_t if len(controls) else None,
                group_index,
                time_t,
                media_missing=zero_missing,
            )
            cf_effect = full_out["y_hat"] - zero_out["y_hat"]
            gap = cf_effect - full_out["media_contribution"][:, :, j]
            cf_loss = cf_loss + torch.mean(gap[fit_mask_t] ** 2)
            baseline_delta = full_out["baseline"] - zero_out["baseline"]
            baseline_delta_loss = baseline_delta_loss + torch.mean(baseline_delta[fit_mask_t] ** 2)
        if was_training:
            model.train()
        return cf_loss / max(n_channels, 1), baseline_delta_loss / max(n_channels, 1)

    def _response_shape_losses():
        if response_monotonicity_weight <= 0 and marginal_smoothness_weight <= 0:
            zero = torch.tensor(0.0, dtype=torch.float32, device=device)
            return zero, zero
        was_training = model.training
        model.eval()
        grid = torch.linspace(0.0, 2.25, 8, dtype=torch.float32, device=device)
        monotonicity_loss = torch.tensor(0.0, dtype=torch.float32, device=device)
        smoothness_loss = torch.tensor(0.0, dtype=torch.float32, device=device)
        zero_missing = torch.zeros_like(media_missing_t)
        for j in range(n_channels):
            curve_points = []
            for pct in grid:
                scenario_media = torch.zeros_like(media_t)
                scenario_media[:, :, j, :] = pct
                sc_out = model(
                    scenario_media,
                    controls_t if len(controls) else None,
                    group_index,
                    time_t,
                    media_missing=zero_missing,
                )
                curve_points.append(sc_out["media_contribution"][:, :, j])
            curve = torch.stack(curve_points, dim=-1)
            diffs = curve[:, :, 1:] - curve[:, :, :-1]
            monotonicity_loss = monotonicity_loss + torch.mean(torch.relu(-diffs)[fit_mask_t])
            if diffs.shape[-1] > 1:
                second = diffs[:, :, 1:] - diffs[:, :, :-1]
                smoothness_loss = smoothness_loss + torch.mean(second[fit_mask_t] ** 2)
        if was_training:
            model.train()
        return monotonicity_loss / max(n_channels, 1), smoothness_loss / max(n_channels, 1)

    history = []
    best_state = None
    best_val_loss = None
    bad_epochs = 0
    for epoch in range(1, int(epochs) + 1):
        optimizer.zero_grad()
        out = model(media_t, controls_t if len(controls) else None, group_index, time_t, media_missing=media_missing_t)
        fit_err = out["y_hat"][fit_mask_t] - y_t[fit_mask_t]
        fit_loss = torch.mean(fit_err**2)
        if contribution_target_t is not None and bool(contribution_mask_t.any().item()):
            contrib_err = out["media_contribution"][contribution_mask_t] - contribution_target_t[contribution_mask_t]
            contribution_loss = torch.mean(contrib_err**2)
        else:
            contribution_loss = torch.tensor(0.0, dtype=torch.float32, device=device)
        group_penalty = group_media_shrinkage * torch.mean(out["group_media_raw"] ** 2)
        contribution_l1 = contribution_l1_weight * torch.mean(torch.abs(out["media_contribution"][fit_mask_t]))
        if counterfactual_penalty_every > 0 and (epoch == 1 or epoch % int(counterfactual_penalty_every) == 0):
            counterfactual_loss, baseline_delta_loss = _counterfactual_losses(out)
        else:
            counterfactual_loss = torch.tensor(0.0, dtype=torch.float32, device=device)
            baseline_delta_loss = torch.tensor(0.0, dtype=torch.float32, device=device)
        if response_penalty_every > 0 and (epoch == 1 or epoch % int(response_penalty_every) == 0):
            monotonicity_loss, marginal_smoothness_loss = _response_shape_losses()
        else:
            monotonicity_loss = torch.tensor(0.0, dtype=torch.float32, device=device)
            marginal_smoothness_loss = torch.tensor(0.0, dtype=torch.float32, device=device)
        loss = (
            fit_loss
            + contribution_supervision_weight * contribution_loss
            + counterfactual_consistency_weight * counterfactual_loss
            + response_monotonicity_weight * monotonicity_loss
            + marginal_smoothness_weight * marginal_smoothness_loss
            + group_penalty
            + contribution_l1
        )
        loss.backward()
        optimizer.step()
        with torch.no_grad():
            if bool(val_mask_t.any().item()):
                val_err = out["y_hat"][val_mask_t] - y_t[val_mask_t]
                val_loss_value = float(torch.mean(val_err**2).detach().cpu().item())
            else:
                val_loss_value = float(fit_loss.detach().cpu().item())
        if best_val_loss is None or val_loss_value < best_val_loss - early_stopping_min_delta:
            best_val_loss = val_loss_value
            best_state = {k: v.detach().clone() for k, v in model.state_dict().items()}
            bad_epochs = 0
        else:
            bad_epochs += 1
        if epoch == 1 or epoch == epochs or epoch % 100 == 0:
            history.append(
                {
                    "epoch": epoch,
                    "fit_mse_scaled": float(fit_loss.detach().cpu().item()),
                    "validation_mse_scaled": val_loss_value,
                    "contribution_supervision_loss": float(contribution_loss.detach().cpu().item()),
                    "counterfactual_consistency_loss": float(counterfactual_loss.detach().cpu().item()),
                    "baseline_delta_loss": float(baseline_delta_loss.detach().cpu().item()),
                    "response_monotonicity_loss": float(monotonicity_loss.detach().cpu().item()),
                    "marginal_smoothness_loss": float(marginal_smoothness_loss.detach().cpu().item()),
                    "group_penalty": float(group_penalty.detach().cpu().item()),
                    "contribution_l1": float(contribution_l1.detach().cpu().item()),
                    "objective": float(loss.detach().cpu().item()),
                    "best_validation_mse_scaled": best_val_loss,
                }
            )
        if early_stopping_patience and bad_epochs >= int(early_stopping_patience):
            history.append({"epoch": epoch, "early_stopped": True, "best_validation_mse_scaled": best_val_loss})
            break
    if best_state is not None:
        model.load_state_dict(best_state)
    model.eval()

    with torch.no_grad():
        out = model(media_t, controls_t if len(controls) else None, group_index, time_t, media_missing=media_missing_t)
        y_hat = np.array(out["y_hat"].detach().cpu().tolist(), dtype=float)
        baseline = np.array(out["baseline"].detach().cpu().tolist(), dtype=float)
        contrib = np.array(out["media_contribution"].detach().cpu().tolist(), dtype=float)
        feature_weights = np.array(out["media_feature_weights"].detach().cpu().tolist(), dtype=float)
        global_effect = np.array(out["global_effect"].detach().cpu().tolist(), dtype=float)
        group_mult = np.array(out["group_media_multiplier"].detach().cpu().tolist(), dtype=float)

    pred = wide[[date_col, group_col]].copy()
    pred["is_train"] = train_mask.reshape(-1)
    pred["y_actual"] = y.reshape(-1)
    pred["y_pred"] = (y_center + y_scale * y_hat).reshape(-1)
    pred["estimated_baseline"] = (y_center + y_scale * baseline).reshape(-1)
    pred["residual"] = pred["y_actual"] - pred["y_pred"]

    long_rows = []
    support_feature_idx = media_feature_inputs.index("support") if "support" in media_feature_inputs else None
    spend_feature_idx = media_feature_inputs.index("spend") if "spend" in media_feature_inputs else None
    for j, channel in enumerate(channels):
        ch_contrib = y_scale * contrib[:, :, j]
        pred[f"{channel}_estimated_contribution"] = ch_contrib.reshape(-1)
        for g_i, group in enumerate(groups):
            for t_i, date in enumerate(dates):
                support_value = (
                    float(media[g_i, t_i, j, support_feature_idx])
                    if support_feature_idx is not None
                    else np.nan
                )
                spend_value = (
                    float(media[g_i, t_i, j, spend_feature_idx])
                    if spend_feature_idx is not None
                    else np.nan
                )
                support_missing = (
                    bool(media_missing[g_i, t_i, j, support_feature_idx])
                    if support_feature_idx is not None
                    else True
                )
                spend_missing = (
                    bool(media_missing[g_i, t_i, j, spend_feature_idx])
                    if spend_feature_idx is not None
                    else True
                )
                long_rows.append(
                    {
                        "date": date,
                        "geo_id": group,
                        "variable": channel,
                        "component": "media",
                        "support": support_value,
                        "spend": spend_value,
                        "support_missing": support_missing,
                        "spend_missing": spend_missing,
                        "estimated_contribution": float(ch_contrib[g_i, t_i]),
                        "group_media_multiplier": float(group_mult[g_i, j]),
                    }
                )

    params = []
    for j, channel in enumerate(channels):
        params.append(
            {
                "param_level": "global",
                "channel": channel,
                "global_effect_on_scaled_y": float(global_effect[j]),
                "coef_on_raw_y_proxy": float(global_effect[j] * y_scale),
                "media_support_scale": float(support_scale[j]),
            }
        )
        for f_i, feature in enumerate(media_feature_inputs):
            params.append(
                {
                    "param_level": "media_feature",
                    "channel": channel,
                    "media_feature": feature,
                    "media_feature_weight": float(feature_weights[j, f_i]),
                    "media_feature_scale": float(media_feature_scale[j, f_i]),
                }
            )
        for g_i, group in enumerate(groups):
            params.append(
                {
                    "param_level": "group",
                    "channel": channel,
                    "geo_id": group,
                    "group_media_multiplier": float(group_mult[g_i, j]),
                }
            )

    curves = []
    for j, channel in enumerate(channels):
        spend_col = f"{channel}{spend_suffix}"
        current_spend = np.nan
        if spend_col in df.columns:
            vals = pd.to_numeric(df.loc[df[date_col].isin(dates[:-holdout_weeks] if holdout_weeks else dates), spend_col], errors="coerce")
            vals = vals[np.isfinite(vals) & (vals > 0)]
            current_spend = float(np.median(vals)) if len(vals) else np.nan
        feature_profile = np.ones((1, n_time, n_channels, n_media_features), dtype=float)
        for pct in np.linspace(0.0, 2.5, 101):
            scenario_media = np.zeros_like(media_scaled)
            scenario_media[:, :, :, :] = 0.0
            scenario_media[:, :, j, 0] = pct
            if n_media_features > 1:
                scenario_media[:, :, j, 1:] = pct
            with torch.no_grad():
                sc_out = model(
                    torch.tensor(scenario_media.tolist(), dtype=torch.float32, device=device),
                    controls_t if len(controls) else None,
                    group_index,
                    time_t,
                    media_missing=torch.zeros_like(media_missing_t),
                )
                sc_contrib = np.array(sc_out["media_contribution"].detach().cpu().tolist(), dtype=float)
            contribution = float(y_scale * np.nanmedian(sc_contrib[:, :, j]))
            curves.append(
                {
                    "channel": channel,
                    "geo_id": "__global__",
                    "pct_of_current_support": float(pct),
                    "support": float(support_scale[j] * pct),
                    "spend": float(current_spend * pct) if np.isfinite(current_spend) else np.nan,
                    "estimated_incremental_contribution": contribution,
                    "estimated_roi_like": float(contribution / (current_spend * pct)) if pct > 0 and np.isfinite(current_spend) and current_spend > 0 else np.nan,
                    "curve_type": "tft_mmm_intervention",
                }
            )

    curve_df = pd.DataFrame(curves)
    economics_rows = []
    train_flat = train_mask.reshape(-1)
    for j, channel in enumerate(channels):
        contrib_col = f"{channel}_estimated_contribution"
        contribution_values = pd.to_numeric(pred.loc[train_flat, contrib_col], errors="coerce")
        total_contribution = float(contribution_values.sum(skipna=True))
        spend_values = pd.Series(dtype=float)
        spend_col = f"{channel}{spend_suffix}"
        if spend_col in wide.columns:
            spend_values = pd.to_numeric(wide.loc[train_flat, spend_col], errors="coerce")
        total_spend = float(spend_values.sum(skipna=True)) if len(spend_values) else np.nan
        support_values = pd.Series(dtype=float)
        support_col = f"{channel}{support_suffix}"
        if support_col in wide.columns:
            support_values = pd.to_numeric(wide.loc[train_flat, support_col], errors="coerce")
        total_support = float(support_values.sum(skipna=True)) if len(support_values) else np.nan
        roi_like = total_contribution / total_spend if np.isfinite(total_spend) and total_spend > 0 else np.nan
        cost_per_incremental_outcome = total_spend / total_contribution if total_contribution > 0 and np.isfinite(total_spend) else np.nan
        ch_curve = curve_df.loc[curve_df["channel"].astype(str).eq(str(channel))].sort_values("pct_of_current_support")
        marginal_roi_like = np.nan
        if not ch_curve.empty:
            lower = ch_curve.loc[ch_curve["pct_of_current_support"] < 1.0].tail(1)
            upper = ch_curve.loc[ch_curve["pct_of_current_support"] > 1.0].head(1)
            if not lower.empty and not upper.empty:
                delta_contrib = float(upper["estimated_incremental_contribution"].iloc[0] - lower["estimated_incremental_contribution"].iloc[0])
                delta_spend = float(upper["spend"].iloc[0] - lower["spend"].iloc[0])
                if np.isfinite(delta_spend) and abs(delta_spend) > 1e-12:
                    marginal_roi_like = delta_contrib / delta_spend
        economics_rows.append(
            {
                "channel": channel,
                "train_total_support": total_support,
                "train_total_spend": total_spend,
                "train_total_incremental_contribution": total_contribution,
                "roi_like": roi_like,
                "cost_per_incremental_outcome": cost_per_incremental_outcome,
                "mroi_like": marginal_roi_like,
                "marginal_cost_per_incremental_outcome": (1.0 / marginal_roi_like) if np.isfinite(marginal_roi_like) and marginal_roi_like > 0 else np.nan,
                "coef_on_raw_y_proxy": float(global_effect[j] * y_scale),
                "mean_group_media_multiplier": float(np.nanmean(group_mult[:, j])),
                "note": "ROI-like metrics divide predicted incremental KPI by spend; for non-revenue KPIs read as outcome per cost, not financial ROI.",
            }
        )
    variable_economics = pd.DataFrame(economics_rows)

    diagnostic_rows = []
    with torch.no_grad():
        full_out = model(media_t, controls_t if len(controls) else None, group_index, time_t, media_missing=media_missing_t)
        for j, channel in enumerate(channels):
            zero_media, zero_missing = _zero_channel(j)
            zero_out = model(
                zero_media,
                controls_t if len(controls) else None,
                group_index,
                time_t,
                media_missing=zero_missing,
            )
            cf_effect = np.array((full_out["y_hat"] - zero_out["y_hat"]).detach().cpu().tolist(), dtype=float) * y_scale
            reported = np.array(full_out["media_contribution"][:, :, j].detach().cpu().tolist(), dtype=float) * y_scale
            base_delta = np.array((full_out["baseline"] - zero_out["baseline"]).detach().cpu().tolist(), dtype=float) * y_scale
            mask = fit_mask & np.isfinite(y)
            reported_abs = float(np.nanmedian(np.abs(reported[mask]))) if np.any(mask) else np.nan
            baseline_abs = float(np.nanmedian(np.abs(base_delta[mask]))) if np.any(mask) else np.nan
            cf_gap_rmse = float(np.sqrt(np.nanmean((cf_effect[mask] - reported[mask]) ** 2))) if np.any(mask) else np.nan
            ratio = baseline_abs / max(reported_abs, 1e-8) if np.isfinite(baseline_abs) and np.isfinite(reported_abs) else np.nan
            diagnostic_rows.append(
                {
                    "channel": channel,
                    "median_abs_reported_contribution": reported_abs,
                    "median_abs_counterfactual_effect": float(np.nanmedian(np.abs(cf_effect[mask]))) if np.any(mask) else np.nan,
                    "median_abs_baseline_delta_when_zeroed": baseline_abs,
                    "max_abs_baseline_delta_when_zeroed": float(np.nanmax(np.abs(base_delta[mask]))) if np.any(mask) else np.nan,
                    "baseline_stealing_ratio": ratio,
                    "counterfactual_gap_rmse": cf_gap_rmse,
                    "baseline_stealing_flag": bool(np.isfinite(ratio) and ratio > baseline_stealing_relative_threshold),
                    "note": "Baseline should be media-blind; nonzero baseline deltas indicate architecture or implementation leakage.",
                }
            )
    model_diagnostics = pd.DataFrame(diagnostic_rows)

    settings = {
        "model_type": "hierarchical_tft_mmm",
        "channels": channels,
        "controls": controls,
        "media_feature_inputs": media_feature_inputs,
        "holdout_weeks": int(holdout_weeks),
        "validation_weeks": int(validation_weeks),
        "epochs": int(epochs),
        "learning_rate": float(learning_rate),
        "weight_decay": float(weight_decay),
        "contribution_supervision_weight": float(contribution_supervision_weight),
        "counterfactual_consistency_weight": float(counterfactual_consistency_weight),
        "counterfactual_penalty_every": int(counterfactual_penalty_every),
        "response_monotonicity_weight": float(response_monotonicity_weight),
        "marginal_smoothness_weight": float(marginal_smoothness_weight),
        "response_penalty_every": int(response_penalty_every),
        "baseline_stealing_relative_threshold": float(baseline_stealing_relative_threshold),
        "hidden_size": int(hidden_size),
        "n_heads": int(n_heads),
        "dropout": float(dropout),
        "initialized_from_model_state": initial_model_state is not None,
        "target_scaling": {"center": y_center, "scale": y_scale},
        "note": "TFT-style MMM challenger. Uses explicit contribution head; attention is not treated as attribution.",
    }
    model_state = {
        "state_dict": {k: v.detach().cpu() for k, v in model.state_dict().items()},
        "channels": channels,
        "controls": controls,
        "groups": groups,
        "dates": [str(d) for d in dates],
        "media_feature_inputs": media_feature_inputs,
        "media_feature_scale": media_feature_scale.tolist(),
        "control_center": control_center.tolist(),
        "control_scale": control_scale.tolist(),
        "target_scaling": {"center": y_center, "scale": y_scale},
        "model_constructor": {
            "n_channels": int(n_channels),
            "n_media_features": int(n_media_features),
            "n_controls": int(len(controls)),
            "n_groups": int(n_groups),
            "hidden_size": int(hidden_size),
            "n_heads": int(n_heads),
            "dropout": float(dropout),
        },
    }
    return TFTMMMResult(
        predictions=pred,
        long_decomp=pd.DataFrame(long_rows),
        variable_economics=variable_economics,
        model_diagnostics=model_diagnostics,
        learned_params=pd.DataFrame(params),
        response_curves=curve_df,
        training_history=pd.DataFrame(history),
        settings=settings,
        model_state=model_state,
    )
