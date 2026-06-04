"""TFT-style neural MMM with explicit MMM output heads.

This is not a forecast-only TFT. It uses TFT-inspired sequence components
but returns media contribution, baseline, response curves, and effect proxies.
"""

from __future__ import annotations


def _require_torch():
    try:
        import torch
        import torch.nn as nn
        import torch.nn.functional as F
    except Exception as exc:  # pragma: no cover - optional runtime dependency.
        raise ImportError("Torch is required for the TFT MMM challenger.") from exc
    return torch, nn, F


def build_hierarchical_tft_mmm(
    n_channels: int,
    n_media_features: int = 1,
    n_controls: int = 0,
    n_groups: int = 1,
    hidden_size: int = 48,
    n_heads: int = 4,
    dropout: float = 0.05,
    group_media_max_log_multiplier: float = 0.25,
):
    """Create a hierarchical TFT-style MMM.

    Inputs:
    - media: [group, time, channel, media_feature]
    - media_missing: optional [group, time, channel, media_feature] indicator
    - controls: [group, time, control]
    - group_index: [group]
    - time_features: [group, time, time_feature]

    Outputs:
    - y_hat
    - baseline
    - media_contribution [group, time, channel]
    - learned feature weights and group multipliers
    """
    torch, nn, F = _require_torch()
    n_channels = int(n_channels)
    n_media_features = int(max(n_media_features, 1))
    n_controls = int(max(n_controls, 0))
    n_groups = int(max(n_groups, 1))
    hidden_size = int(hidden_size)
    n_heads = int(max(n_heads, 1))
    if hidden_size % n_heads != 0:
        n_heads = 1

    class GatedResidualNetwork(nn.Module):
        def __init__(self, size: int):
            super().__init__()
            self.fc1 = nn.Linear(size, size)
            self.fc2 = nn.Linear(size, size)
            self.gate = nn.Linear(size, size)
            self.norm = nn.LayerNorm(size)

        def forward(self, x):
            z = F.elu(self.fc1(x))
            z = self.fc2(z)
            g = torch.sigmoid(self.gate(x))
            return self.norm(x + g * z)

    class HierarchicalTFTMMM(nn.Module):
        def __init__(self):
            super().__init__()
            self.n_channels = n_channels
            self.n_media_features = n_media_features
            self.n_controls = n_controls
            self.group_media_max_log_multiplier = float(group_media_max_log_multiplier)

            self.media_feature_logits = nn.Parameter(torch.zeros(n_channels, n_media_features))
            self.group_media_raw = nn.Parameter(torch.zeros(n_groups, n_channels))
            self.channel_embedding = nn.Parameter(torch.randn(n_channels, hidden_size) * 0.04)
            self.group_embedding = nn.Embedding(n_groups, hidden_size)

            self.control_projection = nn.Linear(n_controls, hidden_size, bias=False) if n_controls > 0 else None
            self.time_projection = nn.Linear(4, hidden_size)

            self.baseline_encoder_lstm = nn.LSTM(hidden_size, hidden_size, batch_first=True)
            self.static_enrichment = GatedResidualNetwork(hidden_size)
            self.attention = nn.MultiheadAttention(hidden_size, n_heads, dropout=dropout, batch_first=True)
            self.post_attention = GatedResidualNetwork(hidden_size)
            self.dropout = nn.Dropout(dropout)

            self.baseline_head = nn.Sequential(
                nn.Linear(hidden_size, hidden_size),
                nn.SiLU(),
                nn.Linear(hidden_size, 1),
            )
            self.channel_context = nn.Linear(hidden_size, hidden_size)
            self.media_value_context = nn.Linear(1, hidden_size)
            self.media_missing_context = nn.Linear(n_media_features, hidden_size)
            self.contribution_head = nn.Sequential(
                nn.Linear(hidden_size, hidden_size),
                nn.SiLU(),
                nn.Linear(hidden_size, 1),
            )
            self.global_effect_raw = nn.Parameter(torch.zeros(n_channels))

        def _combine_media(self, media):
            if media.ndim == 3:
                return media
            if media.ndim != 4:
                raise ValueError("media must be [group,time,channel] or [group,time,channel,feature].")
            media = torch.nan_to_num(media, nan=0.0, posinf=0.0, neginf=0.0)
            weights = F.softmax(self.media_feature_logits, dim=-1).view(1, 1, self.n_channels, self.n_media_features)
            return torch.sum(media * weights, dim=-1)

        def forward(self, media, controls=None, group_index=None, time_features=None, media_missing=None):
            media_signal = torch.clamp(self._combine_media(media), min=0.0)
            batch, time, _ = media_signal.shape
            if media_missing is None:
                if media.ndim == 4:
                    media_missing = torch.zeros_like(media)
                else:
                    media_missing = torch.zeros(batch, time, self.n_channels, self.n_media_features, device=media_signal.device)
            else:
                media_missing = media_missing.float()
            if group_index is None:
                group_index = torch.arange(batch, dtype=torch.long, device=media_signal.device)
            if time_features is None:
                t = torch.linspace(0.0, 1.0, time, device=media_signal.device).view(1, time, 1).expand(batch, time, 1)
                time_features = torch.cat(
                    [t, torch.sin(2.0 * torch.pi * t), torch.cos(2.0 * torch.pi * t), torch.ones_like(t)],
                    dim=-1,
                )

            # Baseline path is deliberately media-blind: it can use group,
            # controls, time, seasonality, holidays, and trend proxies, but not
            # media values or media missingness.
            x = self.time_projection(time_features)
            if self.control_projection is not None and controls is not None:
                x = x + self.control_projection(controls)
            group_context = self.group_embedding(group_index).view(batch, 1, -1)
            x = x + group_context

            encoded, _ = self.baseline_encoder_lstm(x)
            enriched = self.static_enrichment(encoded + group_context)
            causal_mask = torch.triu(
                torch.ones(time, time, device=media_signal.device, dtype=torch.bool),
                diagonal=1,
            )
            attn_out, attn_weights = self.attention(enriched, enriched, enriched, attn_mask=causal_mask)
            h = self.post_attention(enriched + self.dropout(attn_out))

            baseline = self.baseline_head(h).squeeze(-1)

            h_channel = self.channel_context(h).unsqueeze(2)
            ch = self.channel_embedding.view(1, 1, self.n_channels, -1)
            media_value_h = self.media_value_context(torch.log1p(media_signal).unsqueeze(-1))
            media_missing_h = self.media_missing_context(media_missing)
            # Channel contribution is isolated: channel j sees only channel j's
            # media signal/missingness plus allowed media-blind context.
            raw_contribution = F.softplus(
                self.contribution_head(torch.tanh(h_channel + ch + media_value_h + media_missing_h)).squeeze(-1)
            )
            exposure_gate = media_signal / (1.0 + media_signal)
            global_effect = F.softplus(self.global_effect_raw).view(1, 1, self.n_channels) + 1e-6
            group_multiplier = torch.exp(
                self.group_media_max_log_multiplier * torch.tanh(self.group_media_raw[group_index])
            ).view(batch, 1, self.n_channels)
            media_contribution = raw_contribution * exposure_gate * global_effect * group_multiplier
            y_hat = baseline + media_contribution.sum(dim=-1)
            return {
                "y_hat": y_hat,
                "baseline": baseline,
                "media_contribution": media_contribution,
                "media_signal": media_signal,
                "media_feature_weights": F.softmax(self.media_feature_logits, dim=-1),
                "global_effect": global_effect.squeeze(0).squeeze(0),
                "group_media_multiplier": group_multiplier.squeeze(1),
                "group_media_raw": self.group_media_raw,
                "attention_weights": attn_weights,
            }

    return HierarchicalTFTMMM()
