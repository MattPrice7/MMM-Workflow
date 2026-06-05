# MMM Workflow Script Roadmap

This file is the working backlog for the script bundle. It separates production-critical work from cleanup and presentation polish.

## Current Priority

1. `hier_mmm.R` / `hier_mmm.stan`
   - Keep as the top-priority production model layer.
   - Add channel-level median/half-saturation anchor overrides so each channel can use its own anchor, for example `median_anchor = 0.30` instead of a global 0.50 median non-zero support rule.
   - Revisit the default curve family. Meridian uses Hill + adstock as the standard media transform, with Hill applied after adstock by default. This bundle supports both `weibull` and `hill`; before changing the default, add tests that compare Weibull defaults to Hill slope-1 concave curves and verify response-curve/optimizer behavior.
   - Done: add holiday-control generation inside the Stan workflow so analysts can request holiday dummies without hand-building columns. Options include holiday week, week before, week after, country/region sets such as US major and EU major, and user-supplied holiday calendars.
   - Keep UCM inside Stan for joint estimation with media. A separate pre-fit UCM helper can be added later for diagnostics or initialization, but it should not replace the joint model path.
   - Future cleanup: split internals into modules for metadata, validation, Stan data creation, transforms, UCM/baseline, execution, posterior extraction, decomposition, diagnostics, decisioning, and export.

2. `quasi_geo_test.R`
   - Current status: signed event detection, synthetic control, TBR/DiD fallback, donor placebo, leave-one-donor-out sensitivity, overlap diagnostics, bundle handling, raw-scale economics, and analyst evidence summaries are implemented.
   - Refine evidence classification so fallback methods are not unfairly blocked. A failed synthetic-control attempt should downgrade evidence only if TBR/DiD also fail or diagnostics are too weak.
   - Add multi-geo treated-cell estimation. If several markets move together and some markets remain untreated, estimate the treated cell as a group and/or estimate each treated geo with donor exclusion for other treated geos.
   - National repeated media: when media changes the same across all markets, there is no geo-identifiable untreated donor pool. Keep this diagnostic-only by default. A future national interrupted-time-series/TBR module can aggregate to a national market, but it should be labeled time-series evidence rather than geo-lift evidence.
   - Add blocked pre-period cross-validation for ridge synthetic-control lambda selection.
   - Add prospective matched-market design simulation and required-ramp/MDE planning.

3. `mmm_deck_output_builder.R`
   - Split outputs into client-facing, appendix, and internal QA charts.
   - Add a chart registry with `chart_id`, `chart_name`, `audience`, `required_columns`, `skip_if_missing_columns`, `business_question_answered`, and `recommended_slide_title`.
   - Fix consultant chart date parsing, including Excel serial dates using origin `1899-12-30`.
   - Add guards for missing fit columns and graceful chart skipping.
   - Add client color palettes and channel color overrides.
   - Add response-curve charts with current spend/support markers and spend percentage scenarios.
   - Add ROI, cost-per-KPI, mROI/mCPA, contribution, due-to bridge, funnel, saturation/headroom, and quasi-geo evidence charts.
   - Interactive chart-builder UX backlog:
     - Curves should default to all variables, allow multi-select, and show q05/q95 bands only when one variable is selected.
     - Contribution trend should allow a one-variable filtered view with q05/q95 bands when draw-level decomposition columns are available.
     - Add 2D posterior distribution views by variable, such as contribution vs ROI, contribution vs mROI, and ROI vs saturation/headroom, using posterior draws when present.
     - Replace oversized spreadsheet-like tables with smaller chart-ready tables focused on ROI/cost-per-KPI, percent change, due-to, fair-share, and executive summary metrics.
     - Add flexible period comparisons, for example Q1-Q3 2024 vs Q1-Q3 2025, quarter-over-quarter, year-over-year, and custom selected periods.
     - Add chart-type switches where useful, for example stacked bar, grouped bar, line, waterfall, dot plot, scatter/bubble, or compact table.
     - Simplify theme controls to Default light, Default dark, and Custom.
   - Prefer table outputs plus static HTML/PNG as the stable default. Excel is useful for consultant workflows; Shiny/Plotly are good optional interactive layers, not the only delivery path.

4. `mmm_prior_workflow.R` / `semi_univariate_prior_builder_production_final.R` / `prior_recovery_builder.R`
   - Add channel-level curve anchor overrides and audit fields.
   - Keep prior conversion formulas explicit for coefficient, ROI, mROI, IKPC, CPKPI, cost-per-KPI, contribution, and contribution-share inputs.
   - Keep true inverse-variance precision rather than replacing it with vague confidence labels.
   - Consider splitting prior recovery into modules only after core Stan/quasi/deck work stabilizes.

5. Project/package structure
   - Current bundle is a script library with shippable tests.
   - Future formal package structure should include `DESCRIPTION`, `NAMESPACE`, `R/`, `inst/stan/`, `tests/testthat/`, example data, and optional `renv.lock`.
   - Preserve backward-compatible public function names during any refactor.

## Tests To Keep Shipping

Tests should ship with the bundle. For a formal R package they should move into `tests/testthat/`; for the current script bundle they remain in `tests/`.

Required coverage:
- clean source of every front-door script
- fixed-curve and sampled-curve Stan data contracts
- train/holdout indexing
- zero-random-effect and zero-extra-control cases
- coefficient prior mean, SD, precision, bounds, and business-prior conversion
- decomposition reconciliation: actual approximately equals prediction plus residual, and long contributions sum to wide predictions
- quasi-geo no-event, signed up/down/turn-on/turn-off, contamination, no donors, national repeated media, bundle shocks, and fallback methods
- chart builder missing columns, bad dates, zero spend, negative contribution, and output schema stability

## Industry Alignment Notes

- Google Meridian uses geo-level hierarchical MMM with non-linear media transformations and random coefficients by geography where data supports it.
- Meridian uses adstock for lag and Hill for saturation; the default `hill_before_adstock = False` applies Hill after adstock.
- Meridian treats media spend mainly as the ROI denominator unless spend itself is chosen as the modeled media execution unit.
- Meta GeoLift and Google Matched Markets/TBR motivate the quasi-geo diagnostics: pre-fit quality, placebo checks, power/MDE, donor contamination, and matched-market/TBR fallback logic.
