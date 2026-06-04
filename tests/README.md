# Tests

The executable tests ship inside `../full_package_scripts/tests/` so they can source the scripts by relative path.

## Quick Targeted Suites

Run from `full_package_scripts/`:

```r
source("tests/run_bundle_tests.R")
```

Targeted files that are most important for the current priorities:

- `tests/test_hier_mmm_stan_contract.R`
- `tests/test_transform_consistency.R`
- `tests/test_hier_mmm_hostile_sampling.R`
- `tests/test_quasi_geo_evidence_classes.R`
- `tests/test_quasi_geo_to_stan_handoff.R`
- `tests/test_prior_deck_hardening.R`

## Heavy Tests

Run heavier tests only after core patches are stable:

- `run_deep_mmm_hardening.R`
- `run_full_mmm_bundle_validation.R`

The full validation wrapper is CI-safe by default and does not install packages or CmdStan unless explicitly enabled.

