# econimap

`econimap` is a light R package shell for the MMM Workflow scripts. It keeps the
active analyst scripts intact while making them easier to install, load, test,
and eventually refactor into smaller modules.

## Current Core

- `hier_mmm.R` / `hier_mmm.stan`: joint-estimated shared variable-level curves,
  hierarchical group-level coefficients, keyed pooling, decomposition, ROI/mROI,
  and scenario hooks.
- `quasi_geo_test.R`: quasi-geo / observational geo evidence scanner with
  synthetic-control, TBR/DiD-style fallback diagnostics, scoring, and prior reads.
- `optimizer_scenario_planner.R`: response-curve scenario planning and budget
  optimization from fitted MMM results or supplied curve tables.
- `mmm_deck_output_builder.R`: analyst and client-facing MMM output tables,
  charts, and dashboard helpers.
- `bau_response_curves.R`: conservative fallback response-curve creation when a
  full MMM is not available.

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

This package shell is intentionally conservative. The next step is to move
stable internals from `inst/scripts` into smaller files under `R/` while keeping
the current public function names backward-compatible.
