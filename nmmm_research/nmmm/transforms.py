"""Shared MMM transforms for the NMMM research lane."""

from __future__ import annotations

import math
from typing import Iterable

import numpy as np

SUPPORTED_CURVE_TYPES = (
    "hill",
    "weibull",
    "gompertz",
    "concave",
    "threshold",
    "linear_plateau",
    "near_linear",
)


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


def gompertz_saturation(x: Iterable[float], scale: float, shape: float = 1.0) -> np.ndarray:
    """Modified Gompertz curve normalized to start at zero and saturate at one."""
    x_arr = np.maximum(np.asarray(x, dtype=float), 0.0)
    scale = max(float(scale), 1e-12)
    shape = max(float(shape), 1e-6)
    raw0 = math.exp(-shape)
    raw = np.exp(-shape * np.exp(-x_arr / scale))
    return np.clip((raw - raw0) / max(1.0 - raw0, 1e-12), 0.0, 1.0)


def concave_saturation(x: Iterable[float], scale: float, shape: float = 1.0) -> np.ndarray:
    """Smooth concave saturation with a heavy tail."""
    x_arr = np.maximum(np.asarray(x, dtype=float), 0.0)
    scale = max(float(scale), 1e-12)
    shape = max(float(shape), 1e-6)
    return np.clip(1.0 - np.power(1.0 + x_arr / scale, -shape), 0.0, 1.0)


def threshold_saturation(x: Iterable[float], scale: float, shape: float = 6.0) -> np.ndarray:
    """S-shaped threshold-like saturation normalized to start at zero."""
    x_arr = np.maximum(np.asarray(x, dtype=float), 0.0)
    scale = max(float(scale), 1e-12)
    shape = max(float(shape), 1e-6)
    low = 1.0 / (1.0 + math.exp(shape))
    raw = 1.0 / (1.0 + np.exp(-shape * (x_arr / scale - 1.0)))
    return np.clip((raw - low) / max(1.0 - low, 1e-12), 0.0, 1.0)


def linear_plateau_saturation(x: Iterable[float], scale: float, shape: float = 1.0) -> np.ndarray:
    """Near-linear response until a plateau."""
    x_arr = np.maximum(np.asarray(x, dtype=float), 0.0)
    scale = max(float(scale), 1e-12)
    return np.clip(x_arr / scale, 0.0, 1.0)


def _saturation_given_scale(x: Iterable[float], scale: float, shape: float, curve_type: str) -> np.ndarray:
    curve_type = str(curve_type).lower()
    if curve_type == "hill":
        return hill_saturation(x, half_saturation=scale, slope=shape)
    if curve_type in {"weibull", "near_linear"}:
        return weibull_saturation(x, scale=scale, shape=shape)
    if curve_type == "gompertz":
        return gompertz_saturation(x, scale=scale, shape=shape)
    if curve_type == "concave":
        return concave_saturation(x, scale=scale, shape=shape)
    if curve_type == "threshold":
        return threshold_saturation(x, scale=scale, shape=shape)
    if curve_type == "linear_plateau":
        return linear_plateau_saturation(x, scale=scale, shape=shape)
    raise ValueError(f"Unsupported curve_type: {curve_type}")


def _scale_from_anchor_by_bisection(
    support_anchor: float,
    anchor_saturation: float,
    curve_type: str,
    shape: float,
) -> float:
    """Find scale so saturation(support_anchor) matches anchor_saturation."""
    lo = support_anchor * 1e-6
    hi = support_anchor * 1e6
    for _ in range(90):
        mid = math.sqrt(lo * hi)
        sat = float(_saturation_given_scale([support_anchor], mid, shape, curve_type)[0])
        if sat > anchor_saturation:
            lo = mid
        else:
            hi = mid
    return math.sqrt(lo * hi)


def curve_parameter_from_anchor(
    support_anchor: float,
    anchor_saturation: float = 0.5,
    curve_type: str = "hill",
    shape: float = 1.0,
) -> float:
    """Convert a saturation-at-anchor belief into the curve parameter.

    For Hill this returns half_saturation/ec. For other supported synthetic
    families it returns the scale parameter.
    """
    support_anchor = max(float(support_anchor), 1e-12)
    anchor_saturation = float(np.clip(anchor_saturation, 1e-6, 1.0 - 1e-6))
    shape = max(float(shape), 1e-6)
    curve_type = str(curve_type).lower()
    if curve_type == "hill":
        return support_anchor * math.pow((1.0 - anchor_saturation) / anchor_saturation, 1.0 / shape)
    if curve_type == "weibull":
        return support_anchor / math.pow(-math.log(1.0 - anchor_saturation), 1.0 / shape)
    if curve_type == "near_linear":
        return support_anchor / math.pow(-math.log(1.0 - anchor_saturation), 1.0 / max(shape, 0.75))
    if curve_type == "concave":
        denom = math.pow(1.0 / (1.0 - anchor_saturation), 1.0 / shape) - 1.0
        return support_anchor / max(denom, 1e-12)
    if curve_type == "linear_plateau":
        return support_anchor / anchor_saturation
    if curve_type in {"gompertz", "threshold"}:
        return _scale_from_anchor_by_bisection(support_anchor, anchor_saturation, curve_type, shape)
    raise ValueError(f"Unsupported curve_type: {curve_type}")


def apply_saturation(
    x: Iterable[float],
    curve_param: float,
    shape: float = 1.0,
    curve_type: str = "hill",
) -> np.ndarray:
    """Apply a named saturation curve."""
    curve_type = str(curve_type).lower()
    return _saturation_given_scale(x, curve_param, shape, curve_type)


def finite_median_positive(x: Iterable[float], fallback: float = 1.0) -> float:
    """Median of finite positive values with a safe fallback."""
    arr = np.asarray(x, dtype=float)
    keep = arr[np.isfinite(arr) & (arr > 0)]
    if keep.size == 0:
        return float(fallback)
    return float(np.median(keep))
