"""Torch-ready interpretable neural MMM scaffold.

Torch is imported only when a neural model is requested so the rest of the
research lane can run in lightweight environments.
"""

from __future__ import annotations


def _require_torch():
    try:
        import torch
        import torch.nn as nn
        import torch.nn.functional as F
    except Exception as exc:  # pragma: no cover - depends on optional install.
        raise ImportError(
            "Torch is required for InterpretableNeuralMMM. Install with `pip install torch` "
            "or use the transformed ridge baseline first."
        ) from exc
    return torch, nn, F


def build_interpretable_neural_mmm(
    n_channels: int,
    n_media_features: int = 1,
    n_controls: int = 0,
    n_groups: int = 1,
    curve_type: str = "hill",
    hidden_baseline: int = 16,
    initial_decay: float = 0.35,
    initial_curve_param: float = 2.0,
    initial_shape: float = 1.0,
    initial_media_coef: float = 0.15,
    hierarchical_media: bool = True,
    group_media_max_log_multiplier: float = 0.25,
):
    """Create a constrained neural MMM model.

    Expected forward inputs:
    - media_support: tensor [batch, time, channel] or [batch, time, channel, media_feature]
    - controls: optional tensor [batch, time, control]
    - group_index: optional tensor [batch]

    Returns a dict with y_hat, media_contribution, baseline, and learned params.
    """
    torch, nn, F = _require_torch()
    curve_type = curve_type.lower()
    if curve_type not in {"hill", "weibull"}:
        raise ValueError("curve_type must be 'hill' or 'weibull'.")

    def _expand(value, n, min_value=None, max_value=None):
        if isinstance(value, (list, tuple)):
            vals = [float(v) for v in value]
        else:
            vals = [float(value)] * int(n)
        if len(vals) != int(n):
            raise ValueError("Initial parameter vectors must have length n_channels.")
        if min_value is not None:
            vals = [max(float(min_value), v) for v in vals]
        if max_value is not None:
            vals = [min(float(max_value), v) for v in vals]
        return vals

    def logit(p):
        p = min(max(float(p), 1e-6), 1.0 - 1e-6)
        return float(torch.log(torch.tensor(p / (1.0 - p), dtype=torch.float32)))

    def inv_softplus(x):
        x = max(float(x), 1e-6)
        return float(torch.log(torch.expm1(torch.tensor(x, dtype=torch.float32))))

    decay_init = [logit(v) for v in _expand(initial_decay, n_channels, 1e-6, 1.0 - 1e-6)]
    curve_param_init = [inv_softplus(v) for v in _expand(initial_curve_param, n_channels, 1e-6, None)]
    shape_init = [inv_softplus(v) for v in _expand(initial_shape, n_channels, 1e-6, None)]
    coef_init = [inv_softplus(v) for v in _expand(initial_media_coef, n_channels, 1e-8, None)]

    class InterpretableNeuralMMM(nn.Module):
        def __init__(self):
            super().__init__()
            self.n_channels = int(n_channels)
            self.n_media_features = int(max(n_media_features, 1))
            self.n_controls = int(n_controls)
            self.curve_type = curve_type
            self.hierarchical_media = bool(hierarchical_media)
            self.group_media_max_log_multiplier = float(group_media_max_log_multiplier)
            self.media_feature_logits = nn.Parameter(torch.zeros(n_channels, self.n_media_features))
            self.decay_logit = nn.Parameter(torch.tensor(decay_init, dtype=torch.float32))
            self.curve_param_raw = nn.Parameter(torch.tensor(curve_param_init, dtype=torch.float32))
            self.shape_raw = nn.Parameter(torch.tensor(shape_init, dtype=torch.float32))
            self.coef_raw = nn.Parameter(torch.tensor(coef_init, dtype=torch.float32))
            self.group_media_raw = nn.Parameter(torch.zeros(max(int(n_groups), 1), n_channels))
            self.control = nn.Linear(n_controls, 1, bias=False) if n_controls > 0 else None
            self.group_intercept = nn.Embedding(max(int(n_groups), 1), 1)
            self.time_baseline = nn.Sequential(
                nn.Linear(4, hidden_baseline),
                nn.SiLU(),
                nn.Linear(hidden_baseline, 1),
            )

        def _adstock(self, media_support):
            decay = torch.sigmoid(self.decay_logit).view(1, 1, -1)
            states = []
            carry = torch.zeros_like(media_support[:, 0, :])
            for t in range(media_support.shape[1]):
                carry = media_support[:, t, :] + decay[:, 0, :] * carry
                states.append(carry)
            return torch.stack(states, dim=1)

        def _combine_media_features(self, media_support):
            if media_support.ndim == 3:
                return media_support
            if media_support.ndim != 4:
                raise ValueError("media_support must be [batch,time,channel] or [batch,time,channel,feature].")
            if media_support.shape[-2] != self.n_channels:
                raise ValueError("media_support channel dimension does not match n_channels.")
            if media_support.shape[-1] != self.n_media_features:
                raise ValueError("media_support feature dimension does not match n_media_features.")
            weights = F.softmax(self.media_feature_logits, dim=-1).view(1, 1, self.n_channels, self.n_media_features)
            return torch.sum(media_support * weights, dim=-1)

        def _saturate(self, adstocked):
            curve_param = F.softplus(self.curve_param_raw).view(1, 1, -1) + 1e-6
            shape = F.softplus(self.shape_raw).view(1, 1, -1) + 1e-3
            x = torch.clamp(adstocked, min=0.0)
            if self.curve_type == "hill":
                xp = torch.pow(x + 1e-8, shape)
                cp = torch.pow(curve_param, shape)
                return xp / (xp + cp + 1e-8)
            return 1.0 - torch.exp(-torch.pow(x / curve_param, shape))

        def forward(self, media_support, controls=None, group_index=None, time_features=None):
            media_support = self._combine_media_features(media_support)
            if media_support.shape[-1] != self.n_channels:
                raise ValueError("media_support channel dimension does not match n_channels.")
            batch, time, _ = media_support.shape
            adstocked = self._adstock(media_support)
            saturated = self._saturate(adstocked)
            if group_index is None:
                group_index = torch.zeros(batch, dtype=torch.long, device=media_support.device)
            global_coef = F.softplus(self.coef_raw).view(1, 1, -1)
            if self.hierarchical_media:
                group_media_multiplier = torch.exp(
                    self.group_media_max_log_multiplier * torch.tanh(self.group_media_raw[group_index])
                ).view(batch, 1, -1)
            else:
                group_media_multiplier = torch.ones(batch, 1, self.n_channels, device=media_support.device)
            effective_coef = global_coef * group_media_multiplier
            media_contribution = saturated * effective_coef
            y_media = media_contribution.sum(dim=-1, keepdim=True)

            group_base = self.group_intercept(group_index).view(batch, 1, 1).expand(batch, time, 1)

            if time_features is None:
                t = torch.linspace(0.0, 1.0, time, device=media_support.device).view(1, time, 1).expand(batch, time, 1)
                time_features = torch.cat(
                    [
                        t,
                        torch.sin(2.0 * torch.pi * t),
                        torch.cos(2.0 * torch.pi * t),
                        torch.ones_like(t),
                    ],
                    dim=-1,
                )
            baseline = group_base + self.time_baseline(time_features)
            if self.control is not None and controls is not None:
                baseline = baseline + self.control(controls)
            y_hat = baseline + y_media
            return {
                "y_hat": y_hat.squeeze(-1),
                "media_contribution": media_contribution,
                "baseline": baseline.squeeze(-1),
                "decay": torch.sigmoid(self.decay_logit),
                "curve_param": F.softplus(self.curve_param_raw),
                "shape": F.softplus(self.shape_raw) + 1e-3,
                "coef": F.softplus(self.coef_raw),
                "group_media_multiplier": group_media_multiplier.squeeze(1),
                "group_media_raw": self.group_media_raw,
                "media_feature_weights": F.softmax(self.media_feature_logits, dim=-1),
            }

    return InterpretableNeuralMMM()
