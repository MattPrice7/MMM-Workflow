# Final MMM Curve + Hierarchical Model Bundle

This bundle is a consultant-facing workflow for building defensible MMM priors when client data quality varies. It supports national-only data, geo KPI with national or partial geo marketing data, and full geo-level panels. It does not claim causal proof without experiments; when geo lift tests are unavailable, the workflow makes the evidence tier explicit and keeps weakly identified priors appropriately loose.

Analyst-facing script folders stay clean, but release bundles should include the `tests/` folder so validation claims remain executable. Run tests after major model, prior, or quasi-geo changes.

For maintenance, use `SCRIPT_ROADMAP.md` as the script-by-script backlog. It separates production-critical Stan/quasi-geo work from chart polish, package-structure cleanup, and future workflow ideas.

A rolling synthetic chart-builder showcase lives outside the scripts at:

`/Users/mattprice/Documents/Codex/MMM_Workflow/examples/chart_builder_showcase`

Run `build_chart_builder_showcase.R` from that folder after chart-builder changes to refresh the static HTML, PNG charts, CSV tables, and Shiny app preview.

## Files

- `mmm_workflow.R`
  - Main source file for new analyst workbooks.
  - Loads the diagnostic, prior, quasi-geo, reporting, and hierarchical model modules in the correct order.
  - Adds clear aliases such as `run_mmm_workflow()`, `build_mmm_priors()`, `run_mmm_quasi_geo_test()`, and `run_mmm_reporting()` without breaking legacy names.

- `marketing_mix_diagnostic_builder_production_final.R`
  - Spend/support diagnostic builder.
  - Produces funnel, channel, support, and curve-anchor diagnostics.
  - Use `make_curve_anchors_from_diagnostic()` to hand anchors into the prior builder.

- `semi_univariate_prior_builder_production_final.R`
  - Backward-compatible implementation file for the prior recovery builder.
  - Builds adstock, curve-rate / anchor-saturation, saturation-shape, and coefficient priors from observed timing, adstock search, saturation anchoring, ramp-period curve learning, multivariate ridge scans, seasonality, holidays, and evidence diagnostics.
  - Adds data-granularity routing, transformed-variable collinearity/VIF diagnostics, benchmark-prior blending, and `coef_hierarchy_scale` handoff for weak or national-only geo media.

- `prior_recovery_builder.R`
  - Prior-module source file and alias layer for `prior_builder()` / `prior_recovery_builder()`.
  - Use `mmm_workflow.R` as the main front door, and use this file only when you want the prior module by itself.

- `mmm_prior_workflow.R`
  - Analyst-facing orchestration layer.
  - Runs granularity audit, optional market-size normalization, mix diagnostics, prior build, prior audit, response curve export, and metadata handoff in one call.
  - Adds KPI economics-to-coefficient benchmark conversion and calibration template helpers.

- `mmm_deck_output_builder.R`
  - Reporting layer for decomposition outputs.
  - Builds deck-ready CSV tables, PNG charts, a static HTML dashboard, optional Excel workbook output, and an optional Shiny/Plotly/DT app folder.
  - Includes channel rollups, arbitrary-depth `rollup_path` reporting metadata, period-over-period change, due-to contribution movement, funnel summary, channel-level KPI economics, and variable/channel role mapping.
  - `rollup_path` is metadata only. Parent nodes such as `paid_social` or `meta` are used for reporting/economics rollups and are not modeled unless they also appear as explicit modeled variables.
  - Can ingest `optimizer_output` from `run_optimizer_scenario_planner()` and export optimizer scenario comparison, recommended plan, group rollup, saturation/headroom, response-curve, and uncertainty tables.
  - Adds optimizer charts for current vs recommended spend, scenario incremental contribution, response curves, marginal response, and saturation/headroom when the required columns are available.
  - Includes a chart registry with audience tags (`client`, `appendix`, `internal_qa`), required tables/columns, availability, and recommended slide titles.
  - Handles long-only decomposition inputs gracefully when fit columns are missing, and parses Excel serial dates with origin `1899-12-30`.
  - Uses generic KPI economics: outcome per cost and cost per outcome. Revenue ROI/value metrics are optional when a KPI value is available.

- `optimizer_scenario_planner.R`
  - Standalone optimizer and scenario planner.
  - Always optimizes against response curves. With a fitted Stan MMM object, it uses `fit$response_curves` when available; otherwise it can generate the same curve points from the fitted transform/coefficient path used by decomposition. It can also accept a precomputed response-curve table for hand-built curves, external models, or baseline planning before a full MMM fit exists.
  - Outputs current plan economics, response curves, grid-based saturation/headroom diagnostics, all-channel and custom scenarios, optimized spend plan, allocation history, ROI, mROI, cost per KPI, and diagnostics.
  - Preserves explicit spend/support points from supplied response-curve sheets, so CPM/CPP/support-based curves can be audited without forcing every row into a linear spend multiplier. When curves are supplied in support units such as impressions, GRPs, rating points, visits, clicks, or leads, `support_cost_map` can price those curves using CPM, CPP, cost per point, or cost per support unit. Scenario plans can also be entered as planned support units, and the planner converts them to the matching response-curve spend/multiplier path. Grid and hybrid optimizers enforce channel min/max/fixed spend constraints against the actual candidate spend on the curve, which also covers zero-current launch channels and nonlinear spend/support curves.
  - Uses a transparent greedy marginal-response allocator by default. Optional search modes include `optimizer_method = "grid"` exhaustive grid search, `optimizer_method = "hybrid"` coarse grid plus continuous local refinement, `optimizer_method = "robust_grid"` posterior/draw-aware grid search, and `optimizer_method = "robust_hybrid"` robust grid plus continuous local refinement. Robust modes can optimize q05 contribution, expected utility, probability of clearing a target, q05 ROI, or q95 cost per KPI. All paths support min/max, locked channel, fixed spend, min spend, max spend, total budget, and budget-change constraints. Grid/hybrid modes also support `variable_group_map` plus `group_constraints` for portfolio, product, line-of-business, or channel-family caps/floors/share limits and emit `optimization_group_rollup`. If `variable_group_map` contains `rollup_path` but no explicit `planning_group`, the planner infers the first meaningful reporting node, e.g. `paid_social` from `total_media > paid_social > meta > meta_campaign_1`.
  - Includes target planning: minimum budget needed for a KPI target, and maximum budget that stays within target cost per KPI or ROI thresholds.
  - Current optimizer recommendation is point-estimate decision support unless a robust optimizer is selected. When draw-level response curves are supplied through `response_curve_draws`, `fit$response_curves_draws`, or `uncertainty = "draws"` with a Stan fit, the planner also outputs q05/q50/q95 and custom-quantile scenario and optimized-plan uncertainty tables. These include incremental contribution, incremental ROI, expected profit, q05/custom profit, probability profit is positive, and probability incremental contribution is positive. Profit fields require `value_per_kpi`; KPI-only workflows can still use contribution and cost-per-KPI uncertainty.

- `bau_response_curves.R`
  - Standalone BAU response-curve creator for pre-model planning, hand-built curves, or conservative defaults when a full MMM fit is not available.
  - Uses historical support/spend flighting, optional group/model-ID rows, optional population scaling, Hill or Weibull curves, and channel-specific median saturation anchors. The default anchor is 50% saturation at median active support. Curve normalization defaults to active training rows where raw current-period support is positive; `curve_normalization_scope = "all_train"` remains available as a sensitivity option.
  - Outputs optimizer-compatible response-curve rows when a business scale is supplied through current contribution, ROI-like outcome per cost, or cost-per-KPI. If no business scale is supplied, it still returns shape/audit curves but marks them `optimizer_ready = FALSE`.

- `quasi_experimental_dose_response_analysis.R`
  - Standalone analyst-facing wrapper for observational quasi-experimental dose-response checks.
  - Uses the same ramp, pooled geo/segment, future-spend placebo, and spend-support logic as the prior builder.

- `quasi_geo_test.R`
  - Standalone quasi-geo test engine for detecting natural or unannounced geo tests.
  - Scans geo x week signed media ramps, down-ramps, mixed windows, and cutoffs; builds ridge synthetic-control counterfactuals from donor geos when estimable; keeps messy candidate events by default; then diagnoses evidence quality, lift uncertainty, MDE/power proxy, pre-period placebo signal, and low/medium/high support dose response for curve-prior direction.
  - If synthetic-control weights or donor support are weak, TBR / DiD fallback evidence can still be used as downgraded evidence when the fallback counterfactual is estimable. Synthetic-control failure is only treated as a blocker when all counterfactual paths fail or the event is not geo-identifiable.
  - Preserves raw spend for `incremental_spend`, cost per outcome, and ROI-like outputs even when modeled media is mean-indexed for comparability.
  - Can hand channel-specific usable quasi-geo evidence into Stan coefficient priors with `qgt_build_stan_prior_handoff()` and `qgt_apply_stan_prior_handoff()`. Bundle/confounded evidence stays in bundle/diagnostic outputs and is not allocated back to individual channels unless separately identified.
  - Writes event, variable, and estimand evidence summaries so imperfect events can be reviewed, filtered, and routed to calibration, directional-prior, diagnostic, or ignore/filter use.
  - If media changes identically across all markets, or if all candidate donor markets move the same channel at the same time, the script treats the event as not geo-identifiable. It preserves the diagnostic read, but does not convert it into geo-lift calibration or channel-specific Stan priors. A separate future national TBR/interrupted-time-series diagnostic could be useful, but it should remain lower-tier time-series evidence rather than quasi-geo evidence.

- `synthetic_mmm_data_generators.R`
  - Support utility with reusable known-truth MMM panels, quasi-geo event panels, and simple decomposition outputs for tests, demos, hostile validation, and future Neural MMM experiments.

- `hier_mmm.R`
  - Final hierarchical Bayesian MMM wrapper.
  - Default baseline is Fourier + group intercept.
  - UCM is still supported for final joint estimation.
  - Reads `coef_hierarchy_scale` from metadata to shrink group coefficient variation for national-only or weakly geo-resolved variables.
  - Supports `curve_type = "weibull"` or `"hill"` for curved media variables. Weibull remains the default.
  - Uses a generic `group_col` model-id input. That column can represent a geo, product, retailer, line of business, product-geo, product-retailer, LOB-product-geo, or any other analyst-defined modeling cell. The current Stan model has one hierarchical group/model-id dimension; use a composite ID for multi-dimensional cells unless/until crossed random effects are added. For variables marked `coef_hierarchy_scope = "keyed"`, pass one-time `coef_hierarchy_part_indices` to pool groups sharing selected pieces of the composite ID. Example: with `group_col = "region_retailer_product"`, `coef_hierarchy_part_indices = 3` pools within product. Indices are R-style one-based by default.
  - Accepts direct `anchor_saturation` in Stan metadata. Media rows with no explicit curve-rate or anchor default to 50% saturation at median active support. When `rrate` / `dvalue` fields are omitted, the wrapper defaults to no adstock and shape 1 unless you override them, with default/audit flags retained in metadata. `curve_normalization_scope = "active_train"` is the default so flighted media is normalized on active support weeks rather than having off-weeks dilute the curve scale; `all_train` remains available for sensitivity/backward comparison.
  - Accepts direct business priors through `fit_hier_mmm(..., business_priors = ...)`: `coef`, `roi`, `mroi`, `ikpc`, and `cpkpi` inputs are converted to coefficient priors on the Stan model scale using training rows only, with SD/precision audit fields retained in `business_prior_audit`.
  - When `spend_map` is supplied, attaches raw spend and raw support to `long_decomp` and `wide_decomp` so outputs can feed KPI economics, response-curve, and deck workflows without a separate join.
  - Outputs `response_curves` at the total variable level by default, with optional group/model-id rows when the fitted data has meaningful group structure or `response_curve_scope = "both"` / `"group"` is requested. The CSV sheet is written as `response_curves.csv` when output writing is enabled.
  - Can optionally output posterior-draw response curves with `create_response_curve_draws = TRUE`; these write `response_curves_draws.csv` and feed uncertainty-aware optimizer/scenario outputs.
  - Writes sampler diagnostics and sampler-setting recommendations when treedepth, R-hat, ESS, divergence, or BFMI flags appear.
  - Writes `diagnostics_model_readiness.csv` and `diagnostics_model_readiness_issues.csv` so analyst review separates true blockers from short-chain smoke-test warnings, collinearity, weak fit, prior-stickiness, and other review notes.
  - Includes geometry controls for centered/non-centered coefficients, group intercepts, UCM state effects, predictor centering, sampler metric, and max treedepth. Fixed curves are precomputed as Stan data, so the common fixed-curve-prior workflow avoids unnecessary autodiff work.
  - Roadmap items include built-in holiday dummy generation for selectable holiday calendars and optional posterior contribution intervals by variable/group/period.

- `hier_mmm.stan`
  - Stan model used by `hier_mmm.R`.
  - Keep this file in the same folder as `hier_mmm.R`.

## Recommended Source Order

For new workbooks, source the full workflow front door:

```r
source("mmm_workflow.R")
```

The explicit legacy source order is still:

```r
source("marketing_mix_diagnostic_builder_production_final.R")
source("prior_recovery_builder.R")
source("mmm_prior_workflow.R")
source("mmm_deck_output_builder.R")
source("quasi_experimental_dose_response_analysis.R")
source("quasi_geo_test.R")
source("hier_mmm.R")
```

## One-Call Analyst Workflow

Use this when you want a repeatable prior-building run and exported audit files.

```r
workflow <- run_mmm_prior_workflow(
  input_data = dt,
  date_col = "week",
  dep_var_col = "dep_vol",
  variable_map = variable_map,
  channel_map = channel_map,
  geo_col = "geo",                  # set NULL for national data
  population_col = "population",    # optional
  scale_media_by_market_size = FALSE,
  scale_dep_var_by_market_size = FALSE,
  coef_bounds = coef_bounds,
  fixed_rrate_by_var = fixed_rrate_by_var,
  benchmark_priors = NULL,
  prior_args = list(
    week_end_day = "Sunday",
    use_holidays = TRUE,
    total_level_shared_curve = TRUE,
    estimate_cvalue_from_data = "auto",
    cvalue_anchor_method = "industry_hybrid"
  ),
  response_curve_multipliers = seq(0, 2, by = 0.05),
  output_dir = "mmm_prior_workflow_outputs"
)

# Equivalent new alias:
# workflow <- run_mmm_workflow(...)

metadata <- workflow$metadata
prior_audit <- workflow$prior_audit
evidence_summary <- workflow$evidence_summary
response_curves <- workflow$response_curves
```

When you have population, households, or another market-size denominator, set `population_col`, `households_col`, or `market_size_col` and turn on `scale_media_by_market_size`. This creates per-market-size modeled inputs while keeping `spend_col` unscaled for ROI and budget accounting.

If you have KPI economics priors from prior MMMs, experiments, or business benchmarks, use the metric that matches the KPI. Revenue businesses might use ROI or marginal ROI. Subscription, lead, trial, or application businesses might use cost per KPI/outcome or incremental KPI per cost.

```r
business_prior_template <- make_mmm_business_prior_template(workflow$metadata$variable)

coef_benchmarks <- make_coef_benchmark_priors_from_business_priors(
  input_data = workflow$data,
  prior_output = workflow$prior_output,
  business_priors = data.table(
    variable = "tv",
    prior_metric = "cpkpi",          # coef, roi, mroi, ikpc, cpkpi
    prior_mean = 25,
    prior_precision = 0.04,          # precision is preserved unless you pass max_precision
    prior_distribution = "normal",
    evidence_source = "client benchmark"
  ),
  dep_var_col = workflow$dep_var_col
)

metadata <- apply_benchmark_priors_to_metadata(
  workflow$metadata,
  coef_benchmarks
)
```

The same helper accepts wide-column inputs such as `roi`, `mroi`, `outcome_per_cost`, `cost_per_outcome`, `ikpc`, and `cpkpi`. `prior_distribution` is preserved for audit, while the current Stan handoff uses a normal approximation on the model coefficient scale. Keep that distinction visible when entering non-normal business priors.

If you later get lift tests:

```r
lift_template <- make_mmm_calibration_template(workflow$metadata$variable)
```

## Deck and Dashboard Outputs

After fitting a model, use `wide_decomp`, `long_decomp`, and optional raw/modcut spend to create the kinds of tables and charts that feed an MMM deck.

```r
report <- run_mmm_deck_output_builder(
  long_decomp = fit$long_decomp,
  wide_decomp = fit$wide_decomp,
  raw_data = modcut,                 # optional, but needed for cost-per-KPI metrics
  optimizer_output = planner,         # optional: adds optimizer/scenario tables and charts
  channel_map = data.table(          # optional: combine splits into client-facing rollups
    variable = c("meta_campaign_1", "meta_campaign_2", "search"),
    rollup_path = c(
      "total_media > paid_social > meta > meta_campaign_1",
      "total_media > paid_social > meta > meta_campaign_2",
      "total_media > paid_search > search"
    ),
    role = c("media", "media", "media")
  ),
  output_dir = "mmm_deck_outputs",
  prefix = "brand_a",
  media_variables = c("tv", "search", "social"),
  time_col = "week",
  group_col = "geo",                 # optional
  period_granularity = "quarter",
  write_html = TRUE,
  write_charts = TRUE,
  write_excel = FALSE,               # requires openxlsx if TRUE
  write_shiny = TRUE                 # writes a Shiny app folder
)
```

Primary outputs:

- `tables/*_contribution_by_variable.csv`
- `tables/*_contribution_by_channel.csv`
- `tables/*_contribution_by_period_variable.csv`
- `tables/*_period_due_to_variable.csv`
- `tables/*_period_due_to_channel.csv`
- `tables/*_funnel_summary.csv`
- `tables/*_kpi_economics.csv`
- `tables/*_kpi_economics_by_channel.csv`
- `tables/*_optimizer_scenario_comparison.csv`
- `tables/*_optimizer_plan.csv`
- `tables/*_optimizer_response_curves.csv`
- `tables/*_chart_registry.csv`
- `tables/*_fit_diagnostics.csv`
- `charts/*.png`
- `*_mmm_deck_dashboard.html`
- `shiny_app/app.R` when `write_shiny = TRUE`

To run the generated Shiny app later:

```r
install.packages(c("shiny", "plotly", "DT", "ggplot2", "data.table"))
shiny::runApp("mmm_deck_outputs/shiny_app")
```

Use `kpi_value_per_outcome` only when the business can credibly value one KPI outcome. Otherwise, keep reporting in cost per subscriber, cost per lead, applications per dollar, or the KPI-specific equivalent.

## Adaptive Workflow

```r
# 0. Classify the data you actually have
granularity <- diagnose_mmm_data_granularity(
  input_data = dt,
  date_col = "week",
  dep_var_col = "dep_vol",
  variable_map = variable_map,
  geo_col = "geo" # set NULL for national/aggregate data
)

# 1. Build diagnostic anchors from spend/support and business context
mix_diag <- diagnose_marketing_mix(
  input_data = dt,
  week_col = "week",
  sales_col = "dep_vol",
  channel_map = channel_map,
  target_media_intensity = NULL,
  desired_funnel_mix = NULL,
  desired_channel_mix_total = NULL,
  desired_channel_mix_within_funnel = NULL
)

curve_anchors <- make_curve_anchors_from_diagnostic(mix_diag)

# 2. Build curve and coefficient priors.
# Collinearity diagnostics are on transformed x_handoff, not only raw spend/support.
prior_out <- prior_builder(
  input_data = dt,
  date_col = "week",
  dep_var_col = "dep_vol",
  variable_map = variable_map,
  curve_anchors = curve_anchors,
  week_end_day = "Sunday",
  total_level_shared_curve = TRUE,
  diagnose_collinearity = TRUE,
  adjust_priors_for_collinearity = TRUE
)

# 3. Apply data-granularity adjustments.
# This shrinks group-level coefficient variation for national-only media in geo panels.
prior_out <- apply_data_granularity_adjustments_to_prior_output(
  prior_out,
  granularity
)

# 4. Optional: blend credible external priors from previous MMMs, benchmarks, or experiments.
# This is the right place to create "strong" priors for multicollinear variables.
metadata <- make_hier_metadata_from_prior_output(prior_out)
# metadata <- apply_benchmark_priors_to_metadata(metadata, benchmark_priors)

# 5. Run default final model: Fourier baseline, Student-t likelihood, fixed dvalue
fit_fourier <- fit_hier_mmm(
  data = model_dt,
  metadata_input = metadata,
  dep_var_col = "dep_vol",
  group_col = "mod_id",
  time_col = "week",
  entity_col = "entity",
  spend_map = data.table(variable = c("tv", "search"), spend_col = c("tv_spend", "search_spend")),
  raw_output_data = model_dt,        # attaches raw spend/support to decomps
  intercept_type = "fourier",
  likelihood = "student_t",
  estimate_dvalue = FALSE
)

# 5b. Scenario planning and budget optimization from the fitted curves
planner <- run_optimizer_scenario_planner(
  fit_obj = fit_fourier,
  spend_map = data.table(variable = c("tv", "search"), spend_col = c("tv_spend", "search_spend")),
  raw_data = model_dt,
  optimizer_method = "hybrid",
  total_budget = 1000000,
  constraints = data.table(
    variable = c("tv", "search"),
    min_multiplier = c(0.50, 0.75),
    max_multiplier = c(2.00, 1.50)
  ),
  variable_group_map = data.table(
    variable = c("tv", "search"),
    planning_group = c("upper_funnel", "lower_funnel")
  ),
  group_constraints = data.table(
    planning_group = "upper_funnel",
    max_share = 0.45
  ),
  scenario_multipliers = c(0.80, 1.00, 1.20),
  output_dir = "optimizer_outputs"
)

# 6. Optional final UCM joint estimation after Fourier version is sane
fit_ucm <- fit_hier_mmm_ucm_final(
  data = model_dt,
  metadata_input = metadata,
  dep_var_col = "dep_vol",
  group_col = "mod_id",
  time_col = "week",
  entity_col = "entity",
  estimate_dvalue = FALSE
)
```

## Data-Level Routing

- `national_or_aggregate_mmm`: run national/aggregate MMM. Use strong external KPI economics/contribution priors where available, holdouts, response-curve review, and sensitivity runs.
- `geo_kpi_geo_media_hierarchical_mmm`: run hierarchical geo MMM. Shared variable-level curves plus group-level coefficients are appropriate because media has cross-geo variation.
- `mixed_geo_kpi_partial_geo_media_mmm`: run hierarchical geo MMM carefully. Let geo-varying media vary by group, but shrink national-only media with `coef_hierarchy_scale`; require external priors or aggregation for multicollinear channels.
- `geo_kpi_national_media_mmm`: use geo outcomes mainly for baseline/control learning. National repeated media does not identify geo-specific media response; aggregate or strongly shrink group variation.

## Prior Rules

- Do not treat the semi-univariate builder as a standalone causal model. Its job is to extract defendable evidence, create transparent prior centers, and decide how loose those priors should be when the data is weak. The preferred production path is still: external calibration or business/KPI economics where available, quasi-experimental geo/segment or historical ramp evidence where credible, multivariate residual/profile scans, prior predictive sanity checks, then joint Stan estimation when available.
- Strong priors should come from strong evidence: experiments, previous MMMs, vetted benchmarks, reach/frequency saturation evidence, or business constraints.
- The prior builder now flags transformed-variable multicollinearity and VIF. When collinearity is high and no external prior is supplied, it relaxes data-derived coefficient precision and marks the variable as requiring external identification or aggregation.
- `multivariate_coef_scan = TRUE` runs a ridge-style joint scan across transformed media variables. By default it is diagnostic; set `multivariate_coef_prior_mode = "auto"` when you want the ridge center to replace the univariate/profile coefficient only for high-collinearity cases where the ridge scan has a valid sign. Even then, the prior remains loose unless stronger evidence is available.
- The builder now reports contribution and elasticity sanity diagnostics (`implied_contribution_share`, `implied_elasticity`, `implied_cost_per_outcome`, `sanity_bound_class`, `sanity_bound_flags`). Use `sanity_bounds` to add channel-specific guardrails from business review, prior MMMs, experiments, or finance constraints.
- Missing data is explicit. Use `missing_data_policy = "linear_interpolate"`, `"zero_fill"`, `"drop_rows"`, or `"warn_keep"` rather than silently accepting gaps. The workflow records `missing_data_class`, before/after missing shares, and a missing-data risk multiplier that loosens priors when the curve had to learn through missing inputs.
- The curve-rate prior now starts from an industry-hybrid saturation anchor by default. The older 50% saturation-at-median-active-spend rule is still available with `cvalue_anchor_method = "median_active"` for backward compatibility, but the preferred analyst-facing input is `anchor_saturation`. This is curve-family aware: Weibull and Hill convert the same anchor into different internal inverse-scale/rate values.
- The internal Stan field is still named `cvalue` for backward compatibility, but it should be read as `curve_rate` / inverse scale, not as Hill EC50. For Hill, EC50 on the normalized curve-input scale is `1 / curve_rate` when `dvalue = 1`.
- The curve rate can then be refined by a guarded ramp-period grid search when spend/support moved meaningfully enough to identify curve shape. For each candidate curve rate, the coefficient is re-estimated, so this is a profile-style joint curve/coefficient search around the anchored curve. This is observational evidence, not a substitute for lift tests, so outputs include the anchor, data-driven curve-rate read, final source, data-improvement score, anchor method, anchor data weight, and ramp-period diagnostics.
- The prior builder also records observed diminishing-returns evidence by comparing residualized low-spend and high-spend marginal response. When high-spend response is meaningfully lower, it records `observed_curve_evidence_class` and `observed_marginal_slope_ratio`; the slope-implied curve-rate read becomes one candidate in the guarded data-driven curve grid.
- When a meaningful ramp shows no observed diminishing returns, the builder can select a flatter curve candidate. This is especially useful for historically under-spent channels where the observed range may support a near-linear curve rather than the default half-saturation shape.
- If `pooled_ramp_group_col` is supplied, group-level ramp evidence is estimated per geo/segment and pooled into a curve-rate read. When enough groups agree, that pooled value can become the preferred curve rate in the profile curve/coefficient search. The workflow automatically sets `pooled_ramp_group_col = geo_col` in `run_mmm_prior_workflow()` when geo data is available.
- The pooled ramp logic now guards against noisy geo reads that mostly say "flat" while total-level residualized evidence says "diminishing." Those cases are labeled `pooled_mixed_or_conflicting_ramps`, and the total-level curve evidence can remain the safer final curve source.
- To run that evidence layer by itself, use `run_quasi_experimental_dose_response_analysis()`. It frames the output as observational quasi-experimental dose-response evidence: useful for saturation/diminishing-returns priors, but not equivalent to randomized geo lift.
- The hierarchical Stan wrapper now uses observational ramp evidence when it is present in metadata. The R wrapper sends the observed curve-rate evidence into Stan as an additional internal curve-rate prior term; reliability controls how tight that extra prior is.
- Quasi-geo test results can also be converted into Stan coefficient-prior metadata with `qgt_build_stan_prior_handoff()` and `qgt_apply_stan_prior_handoff()`. This handoff is intentionally conservative: it uses channel-specific usable evidence for coefficient priors, keeps bundle shocks as bundle evidence, and does not invent curve priors from quasi-geo reads alone.
- The future-spend placebo guard compares current spend signal to future spend signal. If future spend explains the KPI almost as well as current spend, the output records `future_spend_placebo_class` and relaxes confidence because the channel may be picking up planning bias, seasonality, or demand anticipation.
- These ramp diagnostics are built as observational quasi-experiment heuristics, not replacements for randomized lift calibration. Reliability is higher when ramps are material, repeated across groups, directionally consistent, and pass placebo checks; it is lower when future-spend placebo warnings, mixed group signals, or conflicting marginal slopes appear.
- Actionable curve anchors increase curve-rate precision. Weak anchors remain directional.
- If rrate search hits the upper bound, the builder uses a plateau fallback by default: it chooses the smallest rrate whose fit is effectively indistinguishable from the upper-bound fit, and records `rrate_raw_best`, `rrate_plateau_adjusted`, and `rrate_selection_reason`.
- Under-spent, sparse, or over-saturated channels still receive a best-available curve, but the output flags `spend_level_class`, `under_spend_flag`, `over_spend_flag`, and saturation diagnostics so you know whether the curve is mostly extrapolation or a flat high-spend read.
- Start with `estimate_dvalue = FALSE`; do not estimate saturation shape unless the data and external evidence justify it.
- Prefer KPI economics/contribution priors over raw coefficient priors when you have spend and business benchmarks. `make_coef_benchmark_priors_from_kpi_economics()` converts ROI, cost-per-outcome, or outcome-per-cost into metadata-scale coefficient priors, and `apply_benchmark_priors_to_metadata()` blends them into the metadata handoff.
- For multicollinear variables, the univariate coefficient is often an upper-end contribution read because omitted correlated channels can be absorbed into the single-channel estimate. Use `coef_prior_half_sensitivity` for review, or set `coef_center_shrinkage = 0.5` when you intentionally want conservative hand-built curves. This is available as an option, not a universal default, because clean single-channel tests recover better without shrinking.
- For hand-built curves, inspect `workflow$response_curves` and `workflow$prior_audit`. Do not use a tight curve when `analyst_action` says external evidence or aggregation is needed.

## Hierarchical Geo Scaling

The Stan wrapper now separates KPI scaling from media/control scaling:

- `dep_mean_index_scope = "group"` remains the default so each geo/market outcome is modeled on its own KPI scale.
- `x_mean_index_scope = "group"` is the conservative fallback when media is not comparable across markets.
- `x_mean_index_scope = "global"` is preferable when media has already been made comparable, for example with population, households, target audience, impressions/reach, or other market-size exposure normalization.

This matters because group-level media mean-indexing says each market's own historical average is the reference exposure. That is a fallback assumption, not proof that a lower-spend market truly saturates faster. When good geo media and population/market-size fields are available, normalize exposure first, then use global media scaling so the shared curve is closer to a real cross-market exposure curve.

If you have geo KPI/sales but only national media and no population, household, audience, impression, reach, or comparable exposure denominator, do not estimate separate geo curves by default. Use geo outcomes for baseline/control information, keep one shared channel curve, and strongly pool or aggregate national media coefficients. Without comparable geo media exposure, separate geo curves mostly learn scale artifacts.

The Stan model uses joint-estimated shared variable-level curves and hierarchical group-level coefficients. Groups can still have different effective raw-unit curves through scaling and group coefficient differences, but the model does not freely estimate separate group-level adstock, curve-rate, or saturation-shape parameters.

For channel splits, estimate separate curves only when the splits have enough independent variation and genuinely different response behavior. Paid search branded and unbranded are often good candidates for separate curves if both are well supported. If split-level spend is thin or highly collinear, use a parent-channel curve with split-level coefficients or aggregate the split for curve creation. Spend-weighted total-channel curves are useful for deck review and budget storytelling, but they should be labeled as rollups rather than evidence that every split saturates identically.

For curved variables, use `curve_type = "weibull"` for the current default concave/adstock response or `curve_type = "hill"` when you want the bounded Hill-style saturation form. Both use the same canonical R/Stan transform path for fitting, decomposition, contribution summaries, ROI/mROI, and marginal-response helpers. You can give Stan `anchor_saturation` directly; the wrapper converts that anchor to the correct internal curve-rate parameter for the selected curve family.

Example conservative prior run for high-collinearity hand-built curves:

```r
prior_out <- prior_builder(
  input_data = dt,
  date_col = "week",
  dep_var_col = "dep_vol",
  variable_map = variable_map,
  coef_bounds = coef_bounds,
  estimate_cvalue_from_data = "auto",
  cvalue_anchor_method = "industry_hybrid",
  coef_center_shrinkage = 0.5,
  diagnose_collinearity = TRUE,
  adjust_priors_for_collinearity = TRUE
)
```

## Validation

Use the shippable validation entry point for routine checks:

```r
Rscript tests/run_bundle_tests.R
```

By default this runs P0 reproducibility checks and quasi-geo regressions. Optional heavier checks are enabled with environment switches:

```r
RUN_STAN_TESTS=true Rscript tests/test_p0_reproducibility.R
RUN_CORE_SYNTHETIC_TESTS=true Rscript tests/run_bundle_tests.R
RUN_STAN_SMOKE_TESTS=true Rscript tests/run_bundle_tests.R
```

Run the fast synthetic suite with:

```r
Rscript test_prior_and_diagnostic_workflow.R
```

Run the full local validation, including CmdStan availability, Stan compile/sampling smoke test, decomposition outputs, report outputs, and LOO extraction, with:

```r
Rscript run_full_mmm_bundle_validation.R
```

By default, this validation script is CI-safe: it does not install R packages or CmdStan. Set `INSTALL_MISSING_PACKAGES <- TRUE` or `INSTALL_CMDSTAN_IF_MISSING <- TRUE` inside the script only when you explicitly want local installation.

The current bundle passed 99 core synthetic checks plus the full Stan smoke validation on CmdStan 2.39.0. The Stan smoke uses short chains, so it verifies that the pipeline runs end to end; production fits still need normal convergence review.

Run hostile prior-evidence stress tests with:

```r
Rscript test_hostile_mmm_scenarios.R
```

Those tests currently pass 22 checks covering correlated channels, recoverable versus unseparable multicollinearity, weak ramps, missing media blocks, delayed effects, and noisy quasi-geo dose-response evidence. Passing means the workflow either recovers the useful signal or refuses to become overconfident.

Run the quasi-geo regression suite with:

```r
Rscript test_quasi_geo_test.R
```

That script now passes 20 checks covering hidden lift, no-lift safeguards, signed down-ramp detection, up-media/down-KPI downgrading, raw spend preservation under normalization, stable no-event schemas, and retention of variables without positive usable events in the dose-response summary.

For deeper known-value recovery:

```r
Rscript test_deep_curve_stan_recovery.R
```

That script passed 23 checks for ramp curve/coef recovery, pooled geo dose-response recovery, a tougher quasi-geo dose-response stress case, Stan adstock/curve-rate/coef recovery, fit quality, zero divergences, and observed curve-rate prior activation. Short-chain Stan smoke tests still record treedepth as a sampler-efficiency diagnostic; use production warmup/sampling before interpreting treedepth as a model failure.

For the weakest geo workflow, where you have geo-level KPI/sales and only national media repeated across geos:

```r
Rscript test_geo_sales_national_media_mean_indexing.R
```

That script passed 9 checks. In this exact setup, group vs global media mean-indexing is identical because the same national media series is repeated for every geo. Group-level KPI mean-indexing lets the model recover a shared percentage response across different market sizes, but national media still does not identify materially different geo media effects; keep `coef_hierarchy_scale` small and treat geo data mainly as baseline/noise information unless true geo media variation or exposure normalization becomes available.

## Industry Comparison

- Google Meridian emphasizes causal assumptions, control variables, geo-level modeling, reach/frequency, priors, calibration, response curves, and budget optimization. This bundle aligns on Bayesian priors, geo hierarchy, holdouts, response curves, and decision outputs, but it still uses coefficient-space Stan estimation rather than Meridian's ROI-first parameterization.
- Google Meridian recommends ROI as the default media prior type for revenue settings and notes that coefficient priors are harder to interpret. For non-revenue KPIs, use the equivalent business metric such as cost per subscriber, cost per lead, application per dollar, or trial per dollar; coefficient priors remain the model-scale implementation detail.
- Google's geo-level MMM research shows geo-level data can tighten credible intervals, but estimates deteriorate when more media variables must be imputed at geo level. The new granularity audit and `coef_hierarchy_scale` are designed for that exact mixed-data case.
- Meta Robyn addresses multicollinearity through ridge regularization and encourages calibration with experiments. This bundle's Bayesian analog is prior precision plus explicit collinearity diagnostics; without experiments, it flags the need for external priors or channel aggregation rather than over-trusting unstable univariate estimates.

References:

- Google Meridian introduction: https://developers.google.com/meridian/docs/basics/meridian-introduction
- Google Meridian treatment priors: https://developers.google.com/meridian/docs/advanced-modeling/how-to-choose-treatment-prior-types
- Google Meridian ROI priors and calibration: https://developers.google.com/meridian/docs/advanced-modeling/roi-priors-and-calibration
- Google geo-level Bayesian hierarchical MMM paper: https://research.google/pubs/geo-level-bayesian-hierarchical-media-mix-modeling/
- Google LightweightMMM: https://github.com/google/lightweight_mmm
- Meta Robyn analyst guide: https://facebookexperimental.github.io/Robyn/docs/analysts-guide-to-MMM/
- Meta Robyn key features: https://facebookexperimental.github.io/Robyn/docs/features/
- PyMC-Marketing MMM API: https://www.pymc-marketing.io/en/0.9.0/api/generated/pymc_marketing.mmm.mmm.MMM.html
- Full alignment notes: `INDUSTRY_ALIGNMENT.md`

## Practical Defaults

- Use `likelihood = "student_t"` for weekly MMM unless you have a reason not to.
- Use `intercept_type = "fourier"` as the first model baseline.
- Use `fit_hier_mmm_ucm_final()` only after the Fourier version looks sane.
- Review `prior_out$identification_diagnostics`, `prior_out$collinearity_pairs`, and `granularity$variable_granularity` before sampling.
- In one-call workflow runs, start with `workflow$prior_audit` and `workflow$response_curves`.
- In geo Stan runs, use `x_mean_index_scope = "global"` only after exposure/media has been normalized enough to be comparable across markets; otherwise keep the default `group` scope and treat curve interpretation as weaker.
- `center_predictors_for_sampling` is an optional geometry setting, not a causal assumption. Use `run_hier_mmm_sensitivity()` to compare it against the uncentered path on your data; do not pick it solely from a short-chain smoke run.
- One-chain sensitivity runs are geometry screens only. R-hat is not interpreted for one-chain fits; production review should use multiple chains, with 4 chains preferred.
- Keep `hier_mmm.R` and `hier_mmm.stan` in the same folder.
