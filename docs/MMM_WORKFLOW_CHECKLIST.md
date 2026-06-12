# MMM Workflow Package Checklist

This is the living checklist for the MMM package. Add ideas here first, then promote them into code, tests, and docs. Keep broad refactors on the back burner until core logic is stable.

## Package Goal

Build an analyst-facing MMM package whose core is a transparent Bayesian Stan MMM, supported by quasi-geo evidence, scenario planning, reporting, and prior-recovery utilities. Each major script should be usable on its own, with clear inputs, stable outputs, strong defaults, and enough flexibility for real consultant workflows.

The package does include a Bayesian model. Documentation should not imply that the workflow only approximates or replaces Bayesian modeling. The defensible hierarchy is:

1. `hier_mmm.R` / `hier_mmm.stan`: final Bayesian MMM and decomposition engine.
2. `quasi_geo_test.R`: quasi-geo / observational geo-lift evidence engine.
3. `optimizer_scenario_planner.R`: optimizer / scenario planner.
4. Meridian/core-method gaps: business priors, calibration, reach/frequency, priors, diagnostics.
5. `mmm_deck_output_builder.R`: client and QA reporting layer.
6. Neural MMM research project: high-interest future project, not production replacement yet.
7. Prior-recovery and diagnostic scripts: back-burner support layers until the core Stan, quasi-geo, optimizer, and reporting files are production-strong.

## Package-Level To Do

- [x] Keep shippable tests in `tests/`.
- [x] Keep rolling latest script folder and zip.
- [x] Keep a clean project folder: `MMM_Workflow/`.
- [x] Core files must stand on their own: Stan, quasi-geo, optimizer, and chart builder should not require the prior-recovery workflow.
- [x] Add explicit core standalone contract test covering Stan, quasi-geo, optimizer, deck builder, and workflow aliases.
- [x] Add cleaned analyst idea workflow: raw scratchpad, dated review notes, and cleaned idea backlog.
- [x] Add optional `rollup_path` metadata for reporting and planning rollups, e.g. `total_media > paid_social > meta > meta_campaign_1`, without modeling parent rows or double-counting parent variables.
- [ ] Add optional semantic metadata columns later for richer slicers: `parent_channel`, `sub_channel`, `placement`, `creative`, `funnel_stage`, and `channel_family`.
- [x] Add formal installable package shell with `DESCRIPTION`, `NAMESPACE`, `R/`, `inst/stan/`, `inst/scripts/`, and package smoke tests.
- [ ] Convert smoke tests to fuller `testthat` structure later, with examples and optional `renv.lock`.
- [x] Split the main package-native Stan, quasi-geo, optimizer, deck, and BAU logic into smaller internal modules while preserving public analyst-facing function names and standalone script copies.
- [ ] Add one centralized config object/list for common workflow settings.
- [x] Add package-native `package_info` output versioning to core Stan, quasi-geo, optimizer, deck, and BAU workflow results.
- [ ] Add package-level examples for Stan-only, quasi-geo-only, optimizer-only, chart-builder-only, and full workflow usage.
- [ ] Decide GitHub workflow. Pushing requires a repo remote plus usable GitHub credentials/connector in this environment.

## 1. Stan MMM Model

Core files: `hier_mmm.R`, `hier_mmm.stan`.

Goal: production-grade Bayesian MMM with joint-estimated media, baseline/UCM, priors, decompositions, diagnostics, decisioning outputs, and clear audit tables. UCM components should remain joint-estimated inside Stan when enabled.

Done:

- [x] Joint-estimated shared variable-level curves and hierarchical group-level coefficients.
- [x] Group-specific effective raw-unit curves through scaling and group coefficient differences.
- [x] No freely estimated group-level adstock, curve-rate, or saturation-shape parameters.
- [x] Optional `weibull` and `hill` curve types.
- [x] Canonical media-transform helper used by decomposition, contribution, ROI/mROI, and marginal-response paths.
- [x] Fixed-curve precomputation to reduce Stan autodiff work.
- [x] Geometry options for centered/non-centered coefficients, predictor centering, sampler metric, and treedepth.
- [x] Raw spend/support attachment to decomposition outputs when `spend_map` and raw output data are supplied.
- [x] Final response-curve sheet at total variable level, with optional market/group or product-geo rows.
- [x] Stan model-readiness diagnostics and issue-level review tables.
- [x] Prior vs posterior diagnostics.
- [x] Sensitivity runner that preserves successful variants when another variant fails.
- [x] Data-granularity shrinkage for national-only media in geo panels.
- [x] Product/entity/source-entity halo structure is supported through `source_entity`, `entity_col`, `group_col`, and explicit halo variables.
- [x] `group_col` is generic and can be product, product-geo, product-retailer, line of business, or a composite model ID such as LOB-product-geo. Current Stan hierarchy has one group/model-id dimension; crossed random effects across separate dimensions are a future extension.
- [x] Coefficient outputs include generic `group_value` / `model_id` aliases while preserving legacy `mod_id`.
- [x] Direct channel-level `anchor_saturation` inputs flow into Stan metadata and are converted to the correct curve-family-specific internal curve rate for Hill or Weibull.
- [x] Direct Stan media metadata defaults to 50% saturation at median active support when no explicit curve-rate or anchor is supplied.
- [x] Direct Stan media metadata no longer requires analyst-supplied `rrate`, `rrate_precision`, `dvalue`, or `dvalue_precision` columns; no-adstock and shape-1 defaults are applied unless overridden, and default/audit flags are retained.
- [x] `rrate` and curve-rate are estimated in Stan when `sample_curve_parameters = "always"`; fixed-curve mode intentionally uses the R-precomputed transform path for speed and geometry.
- [x] Add `curve_normalization_scope = c("active_train", "all_train")` to Stan prep/fit, R-side transforms, decomposition, contribution sums, ROI/mROI, response curves, warm-start prior means, and BAU curves. Default is active training rows where raw current-period support is positive.
- [x] Direct business-prior inputs are available in the main Stan workflow wrapper through `fit_hier_mmm(..., business_priors = ...)`.
- [x] Business-prior inputs accept `coef`, `roi`, `mroi`, `ikpc`, and `cpkpi` with SD or precision and convert to true inverse-variance coefficient precision on the Stan scale.
- [x] Business-prior conversion uses training rows only when holdouts exist and writes a `business_prior_audit` table.
- [x] Built-in holiday/control generation is available through `holiday_config` in `prepare_stan_data_hier_mmm()` and `fit_hier_mmm()`, with US major, EU major, global major, week-before/week-of/week-after windows, and custom holiday calendars.

Active / Next:

- [ ] Validate Hill and Weibull defaults against Meridian-style Hill-after-adstock behavior before changing the default curve family.
- [ ] Add a central prior-scale parser so public inputs can accept either SD or precision consistently. Default analyst-facing input should be SD; internals can convert to true inverse-variance precision. Do this as an API cleanup pass, not as a piecemeal column rename.
- [ ] Make mROI / marginal CPKPI priors use true marginal-curve conversion, not average ROI conversion.
- [ ] Add `kpi_value_per_outcome` to Stan output economics so revenue ROI can be computed when appropriate.
- [ ] Add richer prior predictive simulation before sampling.
- [ ] Add posterior contribution intervals by variable/group/period as optional draw-based output.
- [ ] Add prior audit table comparing input prior, converted Stan prior, posterior estimate, and posterior interval.
- [ ] Add diagnostics for baseline/UCM absorbing too much unexplained shock.
- [ ] Validate brand-equity KPIs such as awareness, consideration, subscriptions, leads, or other non-revenue outcomes. Report KPI economics as cost per KPI / outcome per cost unless a value per KPI is supplied.
- [ ] Add reach/frequency modeling when reach, frequency, impressions, or population are available.
- [x] Add optional context/effect-modifier helpers before adding time-varying coefficients. Implemented in Stan as off-by-default train-standardized context multipliers with sign constraints, real-column defaults, self-context blocking, time-context blocking by default, and context risk diagnostics.
- [ ] Future / maybe: evaluate highly regularized time-varying media coefficients only after simpler interactions, controls, and baseline diagnostics are mature.
- [ ] Future / maybe: evaluate optional time-varying effectiveness multipliers, tightly regularized around 1.0 with smooth random-walk or AR(1) structure, gated per channel and off by default.
- [ ] Future / maybe: extend the context modifier to true smooth AR/random-walk time variation only if needed; keep current context-key version as the safer default.
- [ ] Future / maybe: evaluate brand-equity or long-run media-stock states only when external signals exist, such as awareness, organic search, branded search, consideration, or other demand indicators.
- [x] Add explicit single-level hierarchy/pooling keys derived from arbitrary composite model-ID parts, e.g. "pool within product" or "pool within product + retailer". This is separate from `source_entity`; do not overload halo/source metadata as the hierarchy-family key. Current implementation uses per-variable `coef_hierarchy_scope = "keyed"` plus one-time `coef_hierarchy_part_indices`, with per-variable metadata `hierarchy_part_indices` overrides when needed. Indices are R-style one-based by default. A one-family keyed map collapses to the regular global hierarchy.
- [x] Make `auto` hierarchy conservative by role: media and reach/frequency treatment roles can auto-sample group coefficient hierarchy; controls/base/non-media variables require explicit `global` or `keyed` opt-in.
- [ ] Future / maybe: add optional richer `model_id_parts` metadata, e.g. `model_id`, `dimension_name`, `dimension_value`, so analysts can document flexible model-cell pieces such as DMA, product, LOB, retailer, store type, segment, or platform. The core model remains generic and uses part indices rather than hardcoded dimension names.
- [ ] Future / maybe: add true multi-level nested/crossed hierarchy, e.g. global -> product -> region-product -> group. Do this only after single-level keyed pooling is stable and tested on larger panels.

Important interpretation:

- Business priors converted to coefficient scale are acceptable for fixed-curve workflows. If curves are sampled, those priors become approximate unless the model natively places the prior on ROI/mROI/contribution and computes beta jointly.

## 2. Quasi-Geo / Observational Geo Test

Core file: `quasi_geo_test.R`.

Goal: detect and estimate usable natural geo shocks, unannounced geo tests, ramps, cutoffs, and bundle movements. Preserve imperfect evidence, score it, and route it to calibration, directional prior, diagnostic, or ignore/filter use without overclaiming causality.

Done:

- [x] Signed event detection: up-ramp, down-ramp, turn-on, turn-off, mixed, none.
- [x] Synthetic-control style donor matching with ridge weights.
- [x] TBR / DiD fallback paths.
- [x] Raw-scale incremental outcome, spend, ROI-like, and cost-per-outcome outputs.
- [x] Bundle/campaign-level evidence without allocating bundle lift back to individual channels.
- [x] Channel-specific usable evidence can hand coefficient priors into Stan.
- [x] National-repeated media is diagnostic-only / not geo-identifiable.
- [x] Synchronized same-channel shocks across all donor markets are diagnostic-only / not geo-identifiable, even when media levels differ by market.
- [x] Evidence summaries by event, variable, and estimand.
- [x] Optional `channel` / `rollup_path` metadata flows into quasi-geo events, variable summaries, rollup summaries, and prior recommendation tables for reporting rollups such as total Media or total Social without changing event estimation.
- [x] Donor placebo and leave-one-donor-out diagnostics.
- [x] Donor/other-media contamination checks.
- [x] Stable no-event schemas.
- [x] Refine evidence classification so synthetic-control failure is a downgrade when TBR/DiD succeeds, and only becomes a blocker when all counterfactual paths fail or the event is not geo-identifiable.

Active / Next:

- [ ] Add multi-market treated-cell estimation when several markets move together and other markets remain untreated.
- [ ] Add prospective matched-market design simulator and required-ramp / MDE planning.
- [ ] Add blocked pre-period cross-validation for ridge synthetic-control lambda selection.
- [x] Report donor weight concentration and flag if one donor dominates.
- [ ] Expand placebo lift distribution diagnostics and multiple-testing warnings.
- [x] Add explicit event overlap detection across variables/geos/windows.
- [ ] Future / separate from quasi-geo: optional national interrupted-time-series/TBR diagnostic for all-market media shocks, clearly labeled as lower-tier time-series context and never routed as geo-lift calibration.
- [ ] Add a dedicated quasi-geo evidence report pack for analysts.
- [ ] Future / maybe: evaluate rollup-level quasi-geo estimands only when the rollup has identifiable variation. `rollup_path` is currently reporting metadata; it should not silently turn branded + non-branded search into a causal total-search estimand unless the event design supports that rollup.

## 3. Optimizer / Scenario Planner

Core file: `optimizer_scenario_planner.R`.

Goal: use fitted response curves and KPI economics to simulate future plans, compare scenarios, and optimize spend/support subject to business constraints. This should be the next major standalone module after Stan and quasi-geo.

Done:

- [x] Standalone optimizer/scenario planner script.
- [x] Optimize against variable-level response curves pulled from `fit$response_curves`, generated from a fitted Stan object when needed, or supplied directly as a precomputed response-curve table. Geo/product-geo curve rows can be supplied and are aggregated to variable-level totals for optimizer use unless a future geo-level planning mode is added.
- [x] Support simple all-channel scenario multipliers and custom variable-level scenario plans.
- [x] Support total budget, budget change, min/max multiplier, locked channel, fixed spend, min spend, and max spend constraints.
- [x] Support target-response and target-efficiency planning: minimum budget for KPI target, maximum budget within target cost per KPI or ROI.
- [x] Output current plan, response curves, grid-based saturation/headroom, custom scenarios, optimized plan, allocation history, spend, contribution, ROI, mROI, cost per KPI, and diagnostics.
- [x] Preserve explicit spend/support points from supplied response-curve sheets instead of forcing every curve into `current_spend * multiplier`.
- [x] Point-estimate mode is explicitly labeled.
- [x] Add saturation/headroom summary table.
- [x] Add `optimizer_method = "grid"` as a less-greedy exhaustive grid-search option for smaller channel sets / coarser planning grids, while keeping greedy marginal allocation as the default.
- [x] Add `optimizer_method = "hybrid"` as a coarse-grid plus continuous local-refinement option.
- [x] Add posterior/draw uncertainty summaries when draw-level response curves are available: scenario and optimized-plan q05/q50/q95 for contribution, contribution lift, ROI, mROI, cost per KPI, and value per cost.
- [x] Add Stan helper `build_response_curves_draws_hier_mmm()` and `fit_hier_mmm(create_response_curve_draws = TRUE)` for opt-in draw-level response curve sheets.
- [x] Add `optimizer_method = "robust_grid"` to select plans using posterior/draw objectives such as q05 contribution, expected utility, probability of clearing a target, q05 ROI, or q95 cost per KPI.
- [x] Add `optimizer_method = "robust_hybrid"` as a coarse robust-grid plus continuous local-refinement option.
- [x] Add grouped/product-level constraints and rollups through `variable_group_map`, `group_constraints`, and `optimization_group_rollup`.
- [x] Allow `variable_group_map$rollup_path` to infer planning groups from arbitrary-depth metadata when explicit `planning_group` is not supplied.
- [x] Add optimizer tests for locked channels plus grouped/product-level cap and share constraints.
- [x] Enforce channel min/max/fixed spend constraints against actual curve spend, not only multiplier-converted spend.
- [x] Add response-curve tests for zero-current launch channels, nonlinear explicit spend curves, infeasible constraints, missing spend/current spend, and negative contribution diagnostics.
- [x] Add support-input planning with CPM/CPP/cost-per-support conversion where spend is not the execution variable.
- [x] Add optimizer tests for support-only curves priced through CPM and custom scenarios entered as planned support units.
- [x] Add custom conservative uncertainty quantiles plus incremental contribution, incremental ROI, expected profit, q05/custom profit, and probability-positive metrics for scenario and optimized-plan uncertainty outputs.
- [x] Add robust optimizer objective support for expected profit, custom-quantile incremental contribution, custom-quantile incremental ROI, custom-quantile profit, probability profit positive, and probability incremental contribution positive.
- [x] Add optimizer outputs into deck builder charts.
- [x] Add curve confidence/evidence labels into optimizer outputs when supplied by Stan response-curve draws, quasi-geo evidence, or analyst-provided curve metadata.
- [x] Add future flighting and cost assumptions to optimizer scenarios, so planning can distinguish scaling historical support, changing cost per support unit, and changing flight timing.
- [x] Add analyst-facing `driver` aliases to optimizer outputs while preserving `variable` for backward compatibility.

## 3.1 Meridian / Core Method Gaps

These are cross-cutting core gaps that should be addressed before cosmetic reporting work.

- [ ] Evaluate native ROI/mROI/contribution priors inside Stan versus current coefficient-scale conversion.
- [ ] Add calibration hooks for external experiments, platform lift tests, or previous MMMs with explicit prior precision.
- [ ] Add reach/frequency or impressions/population-aware curve bounds.
- [ ] Add stronger formal model health and interpretation workflow.
- [ ] Add stronger causal-methodology documentation separating randomized calibration, quasi-geo evidence, observational priors, and final Bayesian model outputs.
- [ ] Keep Meridian alignment explicit: Hill after adstock, geo hierarchy where data supports it, population/exposure normalization where available, and ROI/KPI economics outputs.

## 4. Chart / Deck Builder

Core file: `mmm_deck_output_builder.R`.

Goal: produce consultant-ready tables and charts from decompositions, spend/support, quasi-geo output, and optimizer scenarios. This is important, but below Stan/geo/optimizer for now.

Done:

- [x] Deck-ready decomposition tables/charts.
- [x] KPI decomposition funnel table/chart.
- [x] Cost per KPI / outcome per cost outputs.
- [x] Channel rollups for split variables.
- [x] Add arbitrary-depth `rollup_path` support through `channel_map`, with `variable_rollup_map`, contribution-by-rollup-node, period rollup, and KPI economics rollup tables.
- [x] Static HTML/CSV/PNG outputs with optional Excel/Shiny paths.
- [x] Add optimizer outputs into deck builder charts.
- [x] Removed chart registry from analyst-facing and exported deck outputs.
- [x] Fix chart date parsing, including Excel serial dates with origin `1899-12-30`.
- [x] Add guards for missing fit columns and graceful chart/table skipping.
- [x] Add response-curve charts with current spend/support markers and spend-percent scenarios from optimizer response curves.
- [x] Add marginal ROI / marginal CPKPI curve charts from optimizer response curves when `mroi` is available.
- [x] Add saturation/headroom summary charts.
- [x] Add current vs optimized budget scenario charts.
- [x] Add separated rolling synthetic chart-builder showcase with static HTML, PNG charts, CSV tables, and Shiny app preview.

Next:

- [ ] Add client color palette/channel color inputs.
- [ ] Add searchable dropdown slicers and more flexible chart filters.
- [ ] Add period-change filters so analysts can compare selected periods, not only static totals.
- [ ] Add channel/subchannel filters using `rollup_path` plus optional metadata such as placement, creative, and funnel stage.
- [ ] Add fair share index and bubble charts where axes can compare spend, contribution, ROI/cost-per-KPI, and bubble size can represent contribution or spend.
- [ ] Add posterior/credible interval bands to contribution, response-curve, ROI, and mROI charts when draw-level outputs are available.
- [ ] Add posterior diagnostic plots for coefficients, prior-vs-posterior shifts, and parameter uncertainty.
- [ ] Add quasi-geo treated-vs-synthetic, media shock, donor weights, placebo distribution, and evidence-prior audit charts.
- [ ] Future: add a dedicated Excel chart workbook builder for consultant workflows.
- [ ] Future: keep the Excel chart workbook separate from the Shiny/static deck builder so analyst Excel customization does not complicate the production chart/export contract.

Recommendation:

- Use tables + static HTML/PNG/Excel as the stable deck-building default. Shiny/Plotly are useful exploratory layers, but they should not be the only client-delivery path.

## 5. Neural MMM Research Project

Core file: not yet created.

Goal: explore a simulation-trained neural / amortized MMM estimator that can learn from known-truth synthetic panels and potentially real calibrated examples. This is a research project, not a replacement for the transparent Stan model until it proves defensible.

Research backlog:

- [ ] Decide R vs Python. Python likely has better ML tooling.
- [x] Add reusable local known-truth generators in `synthetic_mmm_data_generators.R` for MMM panels, quasi-geo events, and decomposition outputs.
- [ ] Expand synthetic data beyond local generators. Consider Meta `siMMMulator` as a benchmark/input source rather than inventing everything from scratch.
- [ ] Test schema-flexible variable encoders for geo/product/time panels.
- [ ] Explore media/control-specific output heads for ROI, adstock, saturation, baseline, halo effects, and missing-variable bias.
- [ ] Explore cross-geo attention, temporal attention, and hierarchical channel-effect pooling.
- [ ] Decide what labels to provide: funnel, channel role, support type, promo/weather/control, product/market, optional creative/message metadata.
- [ ] Add interpretability diagnostics. Attention alone is not enough for causal trust.
- [ ] Compare against Stan on hostile synthetic cases before using on real client data.

## Back Burner / Support Layers

### Prior Recovery Scripts

Core files: `mmm_prior_workflow.R`, `semi_univariate_prior_builder_production_final.R`, `prior_recovery_builder.R`.

- [x] Spend/support diagnostics and curve-anchor handoff.
- [x] KPI economics priors: coefficient, ROI, mROI, IKPC, CPKPI, cost per KPI, outcome per cost.
- [x] Data-driven curve-rate profile search on historical ramps.
- [x] Observed low/high spend marginal-response diagnostics.
- [x] Pooled geo/segment ramp evidence.
- [x] Multivariate ridge coefficient scan for multicollinear prior recovery.
- [x] Future-spend placebo guard.
- [x] Missing media/support handling.
- [ ] Keep stable; no further prior-recovery changes until the core Stan, quasi-geo, optimizer, and reporting files are production-strong.
- [ ] Optional parent/child/subchild ridge or NNLS allocation workflow.
- [ ] Optional Robyn-style ridge prior-recovery script if useful later.
- [ ] BAU response curves: evaluate safe optional rrate/adstock estimation only with guardrails, e.g. bounded search, active-support anchor, no automatic tightening, and diagnostics when higher rrate simply improves fit by absorbing baseline trend.

### Data Pullers / Public Inputs

- [x] DMA population helper exists.
- [ ] Expand only if useful: zip, DMA, state, region, macroeconomic data, consumer sentiment, trend data.
- [ ] Keep network/API pulls function-based and cacheable. No network calls on source.

### Package / CI / Tests

- [x] Source and targeted tests ship with bundle.
- [ ] Convert tests to `testthat` in formal package.
- [ ] Add end-to-end smoke test: mock data -> quasi-geo -> Stan input -> small/fake Stan run -> decomposition -> chart/optimizer output.
- [ ] Add stable output schema tests for all exported tables.
- [ ] Add CI-safe switches for CmdStan-heavy tests.

## Current Validation Snapshot

- [x] Source/smoke tests passing.
- [x] Core standalone contract tests passing.
- [x] Stan contract tests passing.
- [x] Transform consistency tests passing.
- [x] Hostile Stan sampling tests passing at smoke-test scale.
- [x] Quasi-geo evidence-class tests passing.
- [x] Quasi-geo-to-Stan handoff tests passing.
- [x] Prior/deck hardening tests passing.
- [x] Optimizer/scenario planner regression tests passing.

## Known Limits

- Mean-indexed geo media without population is a fallback assumption, not equivalent to comparable per-capita exposure.
- Severe unseparable multicollinearity still needs aggregation, experiments, benchmarks, or business priors.
- National media repeated across all geos is not geo-identifiable.
- Short-chain Stan tests are plumbing/geometry checks, not production convergence proof.
- Neural MMM is a research track until it beats transparent Stan on known-truth and hostile simulations.
