# MMM Latest Scripts Folder

This folder is a local copy of the current script set from the Codex working bundle.

Updated: 2026-05-27

Included:
- Main workflow front door: `mmm_workflow.R`
- Prior recovery / prior workflow scripts
- Quasi geo and dose-response scripts
- Hierarchical MMM R + Stan scripts
- Optimizer / scenario planner
- Deck/reporting helper
- Validation scripts
- DMA population helper copied from Downloads
- `SCRIPT_ROADMAP.md`, which is the script-by-script production backlog and should be the first place to add new ideas before changing code.

Notes:
- Source `mmm_workflow.R` for the normal analyst workflow.
- Run `tests/run_bundle_tests.R` for quick source/targeted checks.
- Source `run_deep_mmm_hardening.R` in RStudio for heavier synthetic consultant-workflow checks.
- Source `run_full_mmm_bundle_validation.R` only when you want the full local validation wrapper; Stan compile is off by default.
- `run_mmm_dma_population()` wraps `pull_dma_population.R` without sourcing it automatically, because that helper pulls Census/GitHub data over the internet.
- `fit_hier_mmm()` can attach raw `spend` and `support` to decomposition outputs when `spend_map` and `raw_output_data` are supplied.
- Curved media variables support `curve_type = "weibull"` or `"hill"` in metadata.
- Fixed-curve Stan runs precompute adstock/saturation transforms as data, and Stan fit outputs retain the geometry settings used for sampler diagnostics.
- Stan fits now write model-readiness diagnostics and issue-level review notes, and the sensitivity runner preserves partial results when a variant fails.
- `quasi_geo_test.R` can hand channel-specific usable quasi-geo evidence into Stan coefficient priors without allocating bundle/confounded shocks back to individual channels.
- `quasi_geo_test.R` also writes analyst evidence summaries by variable and estimand so messy events are retained, scored, and routed to calibration, directional-prior, diagnostic, or ignore/filter use.
- Business priors can be entered as `coef`, `roi`, `mroi`, `ikpc`, or `cpkpi` with explicit mean, SD/precision, and distribution audit fields either directly through `fit_hier_mmm(..., business_priors = ...)` or through the prior-workflow helper functions.
- `hier_mmm.R` supports geometry sensitivity runs, model-readiness diagnostics, fixed-curve precomputation, optional Hill or Weibull media curves, and raw spend/support attachment to decomposition outputs.
- `hier_mmm.R` can consume direct media metadata with or without `anchor_saturation`: no explicit curve-rate/anchor defaults to 50% saturation at median active support, and omitted `rrate` / `dvalue` default to no adstock and shape 1 unless overridden.
- `optimizer_scenario_planner.R` is a standalone point-estimate scenario planner and greedy marginal-response budget optimizer. It always optimizes against response curves, preferably pulled from `fit$response_curves`, generated from a fitted Stan MMM object when needed, or supplied directly as a precomputed response-curve table. It includes fixed-budget, target-KPI, target cost-per-KPI/ROI planning modes, grid-based saturation/headroom diagnostics, and explicit spend/support preservation from response-curve sheets.
- `synthetic_mmm_data_generators.R` contains reusable known-truth MMM, quasi-geo, and decomposition data generators for tests, demos, hostile validation, and future Neural MMM work.
