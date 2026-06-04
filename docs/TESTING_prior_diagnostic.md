# Prior Estimator + Funnel Diagnostic Test Report

## Scope

This test pass covers the synthetic prior/diagnostic/report suite and the full bundle validation script:

- `marketing_mix_diagnostic_builder_production_final.R`
- `semi_univariate_prior_builder_production_final.R`
- `mmm_prior_workflow.R`
- `mmm_deck_output_builder.R`
- `quasi_experimental_dose_response_analysis.R`
- `quasi_geo_test.R`
- `hier_mmm.R`
- `hier_mmm.stan`
- `test_hostile_mmm_scenarios.R`
- `test_deep_curve_stan_recovery.R`
- `test_geo_sales_national_media_mean_indexing.R`

## Test Command

```r
Rscript test_prior_and_diagnostic_workflow.R
```

```r
Rscript test_hostile_mmm_scenarios.R
```

```r
Rscript test_quasi_geo_test.R
```

```r
Rscript run_full_mmm_bundle_validation.R
```

## Result

99 core checks passed, 0 failed.

The hostile prior-evidence suite passed 22 checks, 0 failed. It covers correlated channels, recoverable versus unseparable multicollinearity, weak ramps, missing media blocks, delayed effects, and noisy quasi-geo dose-response evidence.

The standalone quasi-geo test suite passed 10 checks, 0 failed. It verifies hidden geo-lift event detection, donor synthetic-control counterfactuals on estimable rows, positive lift recovery, diagnostic reasons, lift uncertainty intervals, pre-period placebo diagnostics, quality scoring, no-lift overclaim protection, dose-response summary output, and lower high-support marginal response.

The full validation script also completed successfully after CmdStan was installed locally:

- CmdStan 2.39.0 was available through `cmdstanr`.
- `hier_mmm.stan` compiled successfully.
- The hierarchical Stan smoke test sampled 2 chains with 100 warmup and 100 sampling iterations.
- The Stan smoke metadata included `stan_observed_cvalue`, and validation asserted that Stan received `use_observed_cvalue_prior = 1`.
- Decomposition outputs, diagnostics, CSV writes, deck/report outputs, and LOO extraction completed.
- The Stan wrapper wrote sampler-setting recommendations when sampler flags were present.

The smoke test intentionally uses short chains. It verifies that the Stan/data/output pipeline works; it is not intended to prove production convergence.

The deeper known-value recovery script also passed:

```r
Rscript test_deep_curve_stan_recovery.R
```

That script passed 23 checks covering single-channel ramp cvalue/coef recovery, pooled geo/segment quasi-experimental dose-response recovery, a tougher quasi-geo dose-response stress case, Stan rrate/cvalue/coef recovery, fit quality, divergence checks, and observed-cvalue prior activation. It records treedepth as a diagnostic because the current Stan parameterization can recover known values accurately while still sampling inefficiently.

The geo-sales + national-media weak-data script also passed:

```r
Rscript test_geo_sales_national_media_mean_indexing.R
```

That script passed 9 checks covering repeated national-media group/global media scaling equivalence, geo KPI group mean-indexing, Stan rrate/cvalue/coef recovery, coefficient pooling across geos, fit quality, and divergence checks. It also records treedepth as a sampler-efficiency diagnostic.

Detailed results are written to:

- `test_outputs/prior_diagnostic_test_results.csv`
- `test_outputs/prior_diagnostic_test_artifacts.rds`
- `test_outputs/deck_report/`
- `test_outputs/hier_stan_smoke/`
- `test_outputs/hier_stan_smoke_report/`

## What Was Tested

- Exact single-channel coefficient recovery with known adstock, saturation, cvalue, and coefficient.
- Fixed rrate handoff and cvalue recovery.
- Rrate grid-search recovery from synthetic data generated with a known rrate.
- Wrong-sign positive-media guardrail: negative estimated effects are floored and not treated as production-tight priors.
- Transformed-variable collinearity diagnostics and precision relaxation.
- Mixed geo data routing with national repeated media and geo-varying media.
- `coef_hierarchy_scale` handoff for national-only media in geo panels.
- External benchmark-prior blending into metadata.
- Funnel diagnostic spend totals, support mismatch flags, reliability reduction, negative modeled support validation, and anchor handoff fields.
- Analyst-facing wrapper workflow, market-size scaling, response curve export, KPI economics-to-coefficient benchmark conversion, and calibration template fields.
- Deck/reporting output builder with known contribution math, period slicers, group contribution table, KPI cost-per-outcome and outcome-per-cost math, PNG charts, static HTML dashboard, generated Shiny app parsing, and a no-spend/modcut fallback path.
- Deck/reporting channel rollups, period-over-period change tables, due-to contribution movement tables, and channel-level KPI economics for combined channel splits.
- Data-driven `cvalue` refinement on spend-ramp periods where the true curve does not match the default 50% saturation anchor.
- Observed diminishing-returns evidence: low-spend versus high-spend residualized marginal response, slope-implied `cvalue`, and a false-positive guard on a linear response.
- Flat-ramp fallback: when a meaningful ramp shows no observed diminishing returns, the estimator can select a flatter curve and records the reason explicitly.
- Future-spend placebo guard: current-effect synthetic data passes, while a future-effect synthetic case is flagged.
- Pooled group-level ramp evidence: multi-geo ramp tests recover a known cvalue, can drive the selected cvalue center, and carry `stan_observed_cvalue` into the hierarchical metadata handoff.
- Quasi-geo dose-response stress case: noisy geo/segment ramp reads that falsely imply flat response are flagged as mixed/conflicting when total-level residualized ramp evidence still supports diminishing returns.
- Geo-sales + national-media weak-data case: repeated national media with no population/per-capita field recovers the known shared percentage effect when geo coefficients are strongly pooled.
- Workflow wrapper routing: `geo_col` is automatically passed into pooled ramp diagnostics when available.
- Industry-hybrid base `cvalue` anchoring and explicit median-anchor fallback.
- Half-coefficient sensitivity in a severe multicollinearity simulation, plus explicit `coef_center_shrinkage = 0.5` behavior.
- Rrate upper-bound plateau fallback.
- Under-spend and over-spend curve support guardrails.
- Missing-data policies and diagnostics, including interpolation risk multipliers and metadata handoff.
- Contribution, elasticity, cost-per-outcome, and outcome-per-cost sanity checks with variable-level `sanity_bounds`.
- Hostile stress cases where success means either recovering the useful signal or refusing to become overconfident.
- Multivariate ridge prior recovery: when correlated channels have independent movement, ridge improves the coefficient centers versus univariate/profile estimates; when channels are nearly unseparable, the workflow keeps priors loose and requires external evidence.

## Analyst Interpretation

The prior estimator can recover known coefficients and curves when the data-generating setup matches its assumptions, but it should not be treated as a standalone causal model. It should not be expected to recover exact channel-level coefficients in a multichannel system when other correlated media are omitted from each single-channel fit. That is why the workflow now flags transformed-variable multicollinearity, checks missing data and prior-predictive plausibility, and relaxes prior precision unless you provide stronger external evidence.

For hand-built response curves, the most reliable outputs are:

- curve scale and cvalue behavior when the anchor is credible,
- directional coefficient priors in clean single-channel or weakly correlated settings,
- explicit warnings when multicollinearity makes data-derived coefficients fragile,
- geo/national media classification for deciding whether to pool, shrink, aggregate, or require external priors.

The outputs are still priors, not proof of causal effects. Treat high-collinearity variables as requiring benchmark, experiment, previous-MMM, or business-constraint evidence before making a tight curve.

The reporting output builder is a presentation layer. It does not create causal evidence, but it gives the analyst a cleaner way to inspect decompositions, period cuts, fit diagnostics, contribution shares, and KPI economics before copying charts into a deck.

The ramp-period `cvalue` learner improves curve recovery in a synthetic case with meaningful spend ramps and known saturation. It remains guarded: if ramp periods are too sparse or fit improvement is weak, the builder keeps the base anchor and records why.

The observed diminishing-returns check now asks whether the dependent variable shows lower residualized marginal response at high spend than low spend. When it does, the script records `observed_curve_evidence_class`, `observed_marginal_slope_ratio`, and `observed_slope_cvalue`; the slope-implied cvalue is added as a candidate in the guarded cvalue grid. A linear-response synthetic test verifies that this diagnostic does not automatically label spend variation as saturation.

The flat-ramp test covers the underspent-channel case: a channel ramps materially, but the known data-generating process is close to linear across the observed range. The estimator now selects a flatter cvalue candidate and records `observed_flat_ramp_equivalent_fit` / `meaningful_spend_ramps_no_observed_diminishing_returns_flatter_curve` rather than forcing the default median-saturation shape.

The future-spend placebo check helps catch planning-bias and seasonality leakage. If spend several weeks after the KPI explains the KPI about as well as current spend, the workflow flags the channel for looser priors or review.

The pooled group-ramp test covers the geo/segment case: multiple groups have independent spend movement and a shared known saturation curve. The estimator now meta-analyzes those group ramp reads into `pooled_ramp_cvalue`; when the evidence is reliable, it can drive the profile cvalue/coef search and also populate the Stan observational cvalue prior fields.

The base `cvalue` anchor is now industry-hybrid by default. The median-active anchor remains available and test-protected, but the hybrid anchor is safer when spend is highly consistent and the observed median is more a reflection of budget pacing than saturation.

The half-coefficient test is deliberately nuanced. In clean single-channel recovery, the unshrunk coefficient is better. In a severe two-channel multicollinearity simulation, the half coefficient is much closer to the known truth because the univariate estimates absorb the other correlated channel. For that reason, `coef_center_shrinkage = 0.5` is an explicit conservative option rather than the default.

The centered Stan predictor parameterization was tested as a possible treedepth optimization. It remains available through `center_predictors_for_sampling`, but it is off by default because short-chain known-value curve recovery was worse than the production uncentered path. Treedepth remains a sampler-efficiency issue to revisit separately from the quasi-geo evidence layer.
