# MMM Workflow Project

MMM workflow that includes Bayesian MMM, quasi-geo lift, optimizer/scenario
planner, BAU response-curve creation, chart builder, and project TFT neural
MMM (NMMM) research.

This is the clean working folder for the current MMM script bundle.

## Folder Map

- `full_package_scripts/`
  - Current runnable analyst-facing script bundle.
  - Source `mmm_workflow.R` from this folder for day-to-day work.
  - Includes the shippable `tests/` folder because validation claims should stay executable.

- `docs/`
  - Workflow README, checkpoint summary, script roadmap, testing notes, and alignment docs copied from the latest script bundle.

- `industry_standard_docs/`
  - Short references and design notes tying the workflow back to Google Meridian, Google Matched Markets/TBR, Meta GeoLift, Robyn, LightweightMMM, and PyMC-Marketing.

- `tests/`
  - Pointers for which test suites to run and when. The actual executable tests live under `full_package_scripts/tests/` so they can source scripts by relative path.

- `public_data_pilot/`
  - Small public-data pilot assets for non-client testing. Generated outputs are intentionally not copied here.

- `examples/chart_builder_showcase/`
  - Rolling synthetic chart-builder showcase, separated from the scripts.
  - Rebuild with `Rscript build_chart_builder_showcase.R`.
  - Latest outputs live in `examples/chart_builder_showcase/output/`, including static HTML, CSV/PNG tables, and a Shiny app.

- `archives/`
  - Rolling zip snapshots for easy handoff.

## Recommended Use

1. Work in `full_package_scripts/`.
2. Add new ideas to `full_package_scripts/SCRIPT_ROADMAP.md` before changing code.
3. Run targeted tests after major Stan, quasi-geo, prior, or chart-builder changes.
4. Refresh the rolling zip after a stable checkpoint.

## Current Priorities

1. Harden `hier_mmm.R` / `hier_mmm.stan` as the production model layer.
2. Continue hardening `quasi_geo_test.R` for matched-market, TBR, synthetic-control, placebo, and evidence-routing behavior.
3. Improve `mmm_deck_output_builder.R` into a clearer client-facing and internal-QA chart builder.
4. Keep prior-recovery scripts stable until the core Stan/quasi/deck path is stronger.
