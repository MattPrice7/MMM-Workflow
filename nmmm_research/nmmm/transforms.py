"""Shared MMM transforms for the NMMM research lane."""

from __future__ import annotations

import math
from typing import Iterable

import numpy as np


def geometric_adstock_1d(x: Iterable[float], decay: float) -> np.ndarray:
    """Apply geometric adstock to a single ordered series."""
    x_arr = np.asarray(x, dtype=float)
    decay = float(np.clip(decay, 0.0, 0.999))
    out = np.zeros_like(x_arr, dtype=float)
    carry = 0.0
    for i, value in enumerate(x_arr):
        value = 0.0 if not np.isfinite(value) else value
        carry = value + decay * carry
        out[i] = carry
    return out


def hill_saturation(x: Iterable[float], half_saturation: float, slope: float = 1.0) -> np.ndarray:
    """Hill saturation: x^s / (x^s + ec^s)."""
    x_arr = np.maximum(np.asarray(x, dtype=float), 0.0)
    half_saturation = max(float(half_saturation), 1e-12)
    slope = max(float(slope), 1e-6)
    xp = np.power(x_arr, slope)
    hp = math.pow(half_saturation, slope)
    return xp / (xp + hp + 1e-12)


def weibull_saturation(x: Iterable[float], scale: float, shape: float = 1.0) -> np.ndarray:
    """Weibull CDF-style saturation: 1 - exp(-(x / scale)^shape)."""
    x_arr = np.maximum(np.asarray(x, dtype=float), 0.0)
    scale = max(float(scale), 1e-12)
    shape = max(float(shape), 1e-6)
    return 1.0 - np.exp(-np.power(x_arr / scale, shape))


def curve_parameter_from_anchor(
    support_anchor: float,
    anchor_saturation: float = 0.5,
    curve_type: str = "hill",
    shape: float = 1.0,
) -> float:
    """Convert a saturation-at-anchor belief into the curve parameter.

    For Hill this returns half_saturation/ec. For Weibull it returns scale.
    """
    support_anchor = max(float(support_anchor), 1e-12)
    anchor_saturation = float(np.clip(anchor_saturation, 1e-6, 1.0 - 1e-6))
    shape = max(float(shape), 1e-6)
    curve_type = str(curve_type).lower()
    if curve_type == "hill":
        return support_anchor * math.pow((1.0 - anchor_saturation) / anchor_saturation, 1.0 / shape)
    if curve_type == "weibull":
        return support_anchor / math.pow(-math.log(1.0 - anchor_saturation), 1.0 / shape)
    raise ValueError(f"Unsupported curve_type: {curve_type}")


def apply_saturation(
    x: Iterable[float],
    curve_param: float,
    shape: float = 1.0,
    curve_type: str = "hill",
) -> np.ndarray:
    """Apply a named saturation curve."""
    curve_type = str(curve_type).lower()
    if curve_type == "hill":
        return hill_saturation(x, half_saturation=curve_param, slope=shape)
    if curve_type == "weibull":
        return weibull_saturation(x, scale=curve_param, shape=shape)
    raise ValueError(f"Unsupported curve_type: {curve_type}")


def finite_median_positive(x: Iterable[float], fallback: float = 1.0) -> float:
    """Median of finite positive values with a safe fallback."""
    arr = np.asarray(x, dtype=float)
    keep = arr[np.isfinite(arr) & (arr > 0)]
    if keep.size == 0:
        return float(fallback)
    return float(np.median(keep))
