# Simulator Strategy

The NMMM lane should use stronger simulators than the tiny fallback generator.

## Preferred External Simulator

Use `pysimmmulator` when installed:

```bash
pip install pysimmmulator
```

Why:

- It is a Python MMM simulator intended to generate MMM inputs plus true channel ROI.
- It is adapted from Meta's open-source `siMMMulator`.
- It supports config-driven simulation, which is useful for repeatable hostile test suites.

Adapter:

```python
from nmmm.external_simulators import run_pysimmmulator_config

result = run_pysimmmulator_config(
    config_path="/path/to/config.yaml",
    output_path="/path/to/output_folder",
)
```

## Keep The Local Simulator

The local simulator stays because we need full control over cases that may not be easy in third-party configs:

- geo sales with national repeated media
- partial geo media
- product/geo/group panel structures
- known true decompositions at row x channel level
- clean and contaminated ramp periods
- intentionally collinear media
- missing spend/support patterns
- small sample hostile tests

Current local presets:

- `standard`
- `messy_realistic`
- `hostile_collinear`
- `weak_geo`

Local media feature columns now include:

- `{channel}_support`
- `{channel}_spend`
- `{channel}_impressions`
- `{channel}_clicks`
- `{channel}_grps`
- `{channel}_reach`
- `{channel}_frequency`

## Validation Principle

NMMM should be judged on:

1. Prediction fit.
2. Known contribution recovery.
3. ROI / cost-per-KPI recovery.
4. Curve recovery.
5. Robustness under collinearity, missing data, weak variation, delayed effects, and shocks.

Forecast accuracy alone is not enough.
