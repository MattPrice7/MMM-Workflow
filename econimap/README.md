# econimap

`econimap` is an analyst-facing R package for MMM estimation, observational geo
evidence, response-curve planning, optimization, and reporting. Stable core
implementations live in package-native modules under `R/`; synchronized
standalone scripts remain under `inst/scripts` for script-first workflows.

This package directory is the canonical source of truth. See
[`SOURCE_OF_TRUTH.md`](SOURCE_OF_TRUTH.md); implementation designs live in
[`docs/`](docs/).

## Current Core

- `hier_mmm.R` / `hier_mmm.stan`: joint-estimated shared variable-level curves,
  hierarchical group-level coefficients, keyed pooling, decomposition, ROI/mROI,
  and scenario hooks.
- `quasi_geo_test.R`: quasi-geo / observational geo evidence scanner with
  synthetic-control, TBR/DiD-style fallback diagnostics, blocked ridge tuning,
  expanded placebos, multi-treated-market pooling, and conservative curve-prior reads.
- `optimizer_scenario_planner.R`: response-curve scenario planning and budget
  optimization from fitted MMM results or supplied curve tables.
- `mmm_deck_output_builder.R`: analyst and client-facing MMM output tables,
  charts, and dashboard helpers.
- `bau_response_curves.R`: conservative fallback response-curve creation when a
  full MMM is not available.
- `run_sequential_hierarchical_bayes()`: national total-paid-media root by
  default, separate spend/support scope, train-only root evidence, continuous
  identification-driven effectiveness/curve handoff, and an optional joint
  Stan child fit using reference-spend calibration evidence. Weak identification
  increases auditable prior regularization; it does not stop valid branches.

## Usage

```r
library(econimap)

# Public functions are loaded into the package namespace.
econimap_dependency_manifest()
econimap_script_path("hier_mmm.stan")

# If you want classic script-style loading into your global environment:
load_econimap_scripts()
```

The original scripts are bundled under:

```r
econimap_script_dir()
```

## Development Direction

Development remains conservative: public function names stay backward-compatible,
core behavior is tested in package-native modules, and standalone scripts are
mechanically synchronized from those modules.

Linear non-media treatments can use `non_media_baseline_values` (`min` by
default, or `max`, `mean`, `median`, `zero`, or a numeric value). Controls use
`control_reference_values` (`mean` by default) for noncausal reporting
contrasts. Coefficient, contribution, elasticity, and standardized-effect
priors preserve supplied SD or inverse-variance precision in the prior audit.
