# MMM Workflow Checkpoint Summary

## Checkpoints

- `rolling_latest_core_group_optimizer_support_risk_deck_showcase_quasi_fallback`
  - Current rolling state after adding generic `group_value` / `model_id` coefficient-output aliases, documenting one composite group/model-id hierarchy for product-geo/product-retailer/LOB cells, adding `optimizer_method = "grid"`, `"hybrid"`, `"robust_grid"`, and `"robust_hybrid"`, adding opt-in Stan posterior-draw response curves and draw-based scenario/optimized-plan uncertainty summaries, adding optimizer planning-group constraints/rollups for product, portfolio, line-of-business, and channel-family caps/floors/share limits, adding arbitrary-depth `rollup_path` metadata for optimizer planning groups and deck rollup-node reporting without modeling parent rows, hardening optimizer actual-spend constraints for zero-current and nonlinear spend curves, adding CPM/CPP/cost-per-support support-input planning for support-only response curves and planned-support scenarios, adding custom conservative uncertainty quantiles plus incremental contribution/ROI, profit, and probability-positive risk metrics, adding optimizer scenario/response-curve/headroom charts and chart registry support to the deck builder, hardening deck date parsing and missing fit-column handling, adding a separated rolling synthetic chart-builder showcase with Shiny preview, hardening quasi-geo TBR/DiD fallback classification so synthetic-control failure downgrades rather than blocks when fallback evidence is estimable, blocking synchronized same-channel all-donor-market shocks from calibration/prior use as not geo-identifiable, and passing targeted deck, optimizer, source, and core standalone tests.
- `checkpoint_01_pre_tests_after_workflow_patch.zip`
  - State after the initial adaptive workflow, collinearity, and geo-granularity patch.
- `checkpoint_02_tests_passing.zip`
  - State after synthetic prior/funnel tests passed: 28 checks, 0 failures.
- `checkpoint_03_pre_priority_upgrades.zip`
  - Clean starting point before priority upgrades requested after the analyst-workflow clarification.
- `checkpoint_04_wrapper_market_size_response_curves.zip`
  - State after the one-call wrapper, market-size scaling, prior audit, response curve export, ROI-to-coef conversion, industry-alignment notes, and 40 passing tests.
- `checkpoint_05_pre_kpi_economics_generalization.zip`
  - State before generalizing ROI-specific helpers to KPI economics such as outcome-per-cost and cost-per-outcome.
- `checkpoint_06_kpi_economics_generalized.zip`
  - State after KPI economics generalization and 42 passing tests.
- `checkpoint_07_pre_deck_output_builder.zip`
  - State before adding the decomposition reporting and dashboard output layer.
- `checkpoint_08_deck_output_builder_tests_passing.zip`
  - State after adding `mmm_deck_output_builder.R`, static HTML/CSV/PNG outputs, optional Excel and Shiny app export, no-spend fallback handling, docs, and 55 passing tests.
- `checkpoint_09_pre_data_driven_cvalue_coef_tests.zip`
  - State before adding ramp-period data-driven `cvalue` refinement and coefficient-shrinkage sensitivity tests.
- `checkpoint_10_data_driven_cvalue_coef_shrinkage.zip`
  - State after adding ramp-period `cvalue` learning, `coef_prior_half_sensitivity`, optional `coef_center_shrinkage`, docs, and 64 passing tests.
- `checkpoint_11_pre_rrate_plateau_fallback.zip`
  - State before adding rrate upper-bound plateau fallback and under/over-spend channel guards.
- `checkpoint_12_curve_anchor_rrate_spend_guards.zip`
  - State after changing default `cvalue` base anchoring to industry-hybrid, adding rrate upper-bound plateau fallback, under/over-spend guards, docs, and 72 passing tests.
- `checkpoint_13_observed_diminishing_returns.zip`
  - State after adding observed low/high spend marginal-response diagnostics, slope-implied cvalue candidates, flat-ramp no-diminishing fallback, future-spend placebo guard, docs, and 82 passing tests.
- `checkpoint_14_pooled_group_ramp_stan_handoff.zip`
  - State after adding pooled geo/segment ramp evidence, automatic workflow routing from `geo_col`, Stan observational cvalue priors, docs, and 87 passing tests.
- `checkpoint_15_full_validation_cmdstan.zip`
  - State after installing CmdStan locally, fixing narrow `cmdstanr`/`data.table` compatibility issues in `hier_mmm.R`, and passing the full validation script including Stan compile, sampling, decompositions, diagnostics, output writes, and LOO extraction.
- `checkpoint_16_deep_recovery_dose_response_scaling.zip`
  - State after adding the standalone quasi-experimental dose-response wrapper, deep known-value recovery tests, Stan/prior curve-scale alignment, hierarchical media scaling controls, channel rollups/due-to deck tables, and 90 fast checks plus 15 deep recovery checks.
- `checkpoint_17_geo_sales_national_media_recovery.zip`
  - State after adding and passing the focused geo-sales + national-media weak-data Stan test: 9 checks covering repeated national-media scaling equivalence, group KPI mean-indexing, shared coefficient recovery, strong geo pooling, fit quality, and divergence diagnostics.
- `checkpoint_18_quasi_geo_guard_centering_review.zip`
  - State after adding a tougher quasi-geo dose-response stress test, tightening pooled-ramp conflict handling, testing centered Stan predictor sampling, and keeping centered sampling off by default because known-value recovery was stronger on the production uncentered path. Validated with 90 fast checks, 23 deep prior/quasi/Stan checks, and 9 weak-data geo-sales/national-media checks.
- `checkpoint_19_prior_evidence_hardening_hostile_tests.zip`
  - State after hardening the semi-univariate layer into an explicit prior-evidence builder, adding missing-data policies/diagnostics, contribution/elasticity/KPI-economics sanity bounds, quasi-geo `group_col`/`segment_col` aliases, a less aggressive rrate plateau fallback, prior-audit surfacing of evidence quality, and hostile stress tests. Validated with 97 core checks, 14 hostile checks, 23 deep prior/quasi/Stan checks, and 9 weak-data geo-sales/national-media checks.
- `checkpoint_20_prior_recovery_ridge_checklist.zip`
  - State after adding the forward-facing `prior_recovery_builder.R` alias, a living workflow checklist, multivariate ridge coefficient scans, recoverable/unseparable multicollinearity tests, and prior-audit metadata for ridge recovery. Validated with 97 core checks and 22 hostile checks.
- `checkpoint_21_quasi_geo_test_engine.zip`
  - State after adding `quasi_geo_test.R`, a standalone quasi-geo test engine for detecting natural or unannounced geo tests with ridge synthetic-control donor matching, soft qualification scores, lift/marginal-response estimates, and low/medium/high support dose-response summaries. Validated with 7 quasi-geo hidden-lift checks.
- `checkpoint_22_quasi_geo_diagnose_all_events.zip`
  - State after changing `quasi_geo_test.R` to retain candidate events by default, estimate when possible, mark non-estimable rows explicitly, and diagnose evidence quality after the fact instead of applying hard clean/dirty qualification. Validated with 97 core checks, 22 hostile prior-evidence checks, 8 quasi-geo hidden-lift checks, Excel export, Stan smoke, LOO extraction, and Shiny app generation.
- `checkpoint_23_workflow_cleanup_quasi_geo_uncertainty.zip`
  - State after adding `mmm_workflow.R` as the full-workflow source file, cleaner public aliases, an analyst-facing prior evidence summary, quasi-geo lift uncertainty/pre-period placebo diagnostics, a no-lift quasi-geo overclaim test, and Stan sampler-setting recommendations. Validated with 99 core checks, 22 hostile prior-evidence checks, 10 quasi-geo checks, 23 deep prior/quasi/Stan checks, 9 weak-data geo-sales/national-media checks, Excel export, Stan smoke, LOO extraction, and Shiny app generation.
- `checkpoint_24_quasi_geo_signed_events.zip`
  - State after making quasi-geo event detection signed, retaining up ramps, down ramps, mixed ramps, and media cutoffs with signed treatment intensity, signed incremental media, signed incremental outcome, numeric evidence scores, and downgraded rather than discarded conflicting directions.
- `checkpoint_25_bundle_hardening_priority1.zip`
  - State after hardening raw-spend preservation under normalization, stable no-event quasi-geo schemas, script-directory-aware sourcing, CI-safe validation defaults, RNG preservation in stochastic helpers, and wrapper cleanup.
- `checkpoint_26_core_cleanup_quasi_stan.zip`
  - State after a focused cleanup of `quasi_geo_test.R` and `hier_mmm.R`/`hier_mmm.stan`: centralized quasi-geo empty output schemas, explicit `alpha_centered` warm-start initialization, and Stan comments clarifying fixed-curve/static-parameter behavior. Validated with source checks, quasi-geo regression tests, and Stan warm-start init smoke.
- `checkpoint_27_p0_reproducibility_hardening.zip`
  - State after restoring shippable `tests/` validation, fixing variable-specific prior shrinkage lookup, deterministic bundle sourcing, training-only prior/quasi-geo holdout handling, source-safe DMA population pulling, and dependency manifest helpers. Validated with default bundle tests, optional Stan compile, and short-chain Stan smoke.
- `checkpoint_28_transform_quasi_geo_hardening.zip`
  - State after adding the canonical R-side media transform to match Stan/decomposition order, routing contribution/ROI/mROI/optimizer calculations through it, adding quasi-geo estimand classification outputs (`event_estimates_all`, calibration/directional/diagnostic partitions), bundle-shock handling, national-repeated media diagnostics, raw-scale KPI economics checks, and targeted source/smoke tests.
- `checkpoint_29_quasi_geo_transform_targeted_patch.zip`
  - State after strengthening transform consistency tests against an independent fitted/decomposition path, fixing synthetic-control intercept prediction, preserving raw dependent-variable lift for ROI/cost priors under geo mean-indexing, detecting turn-ons/turn-offs and turn-on contamination, calculating true bundle support/spend across co-moving channels, returning all/filtered dose-response summaries, and adding analyst-facing confidence bands. Validated with source checks, targeted transform/quasi-geo smoke tests, and the selected bundle smoke suite.
- Rolling `mmm_latest_scripts` state, 2026-05-26
  - State after adding an opt-in deep consultant-workflow hardening test, replacing the stale full-validation runner with a CI-safe wrapper, and refreshing the single rolling zip. Validated with the source/targeted bundle suite plus 13 deeper synthetic workflow checks covering prior workflow outputs, quasi-geo hidden shocks/bundles/national media diagnostics, and deck KPI economics.
- Rolling `mmm_latest_scripts` Stan/quasi hardening, 2026-05-26
  - State after adding no-sampling Stan contract coverage for curve-only, linear-only, mixed bound blocks, centered predictors, sampled dvalue, national repeated media, geo-varying media hierarchy, coefficient overrides, and zero-training-group failures. Also added safer sampler diagnostic min/max handling and GeoLift-style donor placebo plus leave-one-donor-out sensitivity diagnostics to quasi-geo evidence scoring. Validated with the selected bundle suite: 18 source/P0 checks, 10 transform checks, 11 Stan contract checks, 35 quasi-geo checks, and 4 prior/deck checks.
- Rolling `mmm_latest_scripts` quasi-geo TBR fallback, 2026-05-26
  - State after adding Google Matched Markets/TBR-style fallback counterfactuals to `quasi_geo_test.R`. Synthetic control remains primary when donor support and pre-fit are strong; otherwise the script can select time-based regression or DiD fallback while preserving synthetic-control, TBR, and DiD readouts for audit. Validated with the selected bundle suite: 18 source/P0 checks, 10 transform checks, 11 Stan contract checks, 36 quasi-geo checks, and 4 prior/deck checks.
- Rolling `mmm_latest_scripts` hostile Stan sampling, 2026-05-26
  - State after adding an opt-in hostile Stan sampling test covering correlated media, delayed effects, holdout rows, national repeated media, geo-varying media, ROI/mROI reconciliation, prior-posterior diagnostics, and decomposition arithmetic. The test exposed and fixed a fixed-curve Stan data bug where `sample_curve_parameter` was scalar when multiple curves were present. Auto coefficient hierarchy is now more conservative by default (`coef_hierarchy_auto_min_groups = 5`) to avoid weak small-geo hierarchy funnels unless explicitly lowered. Hostile short-chain sampling passed 11 checks with strong fit/reconciliation, but also surfaced max-treedepth pressure through sampler recommendations. Default selected bundle suite passed with 18 source/P0 checks, 10 transform checks, 13 Stan contract checks, 36 quasi-geo checks, and 4 prior/deck checks.
- Rolling `mmm_latest_scripts` Stan efficiency pass, 2026-05-26
  - State after reducing unused Stan latent state dimensions for non-level baselines (`N_state_innov = 0` unless the stochastic level is active) and adding contract coverage for level versus non-level baselines. Also stress-tested small-panel shared-intercept and shared-Fourier variants; those did not improve treedepth in the hostile smoke and were not retained as default-changing complexity. Validated with source checks, 15 Stan contract checks, and the hostile Stan smoke: 12 checks passed, strong decomposition/reconciliation, max-treedepth pressure still correctly surfaced.
- Rolling `mmm_latest_scripts` Stan/quasi/deck output pass, 2026-05-26
  - State after adding conservative quasi-geo-to-Stan coefficient-prior handoff, preserving bundle/confounded quasi evidence outside individual channel priors, supporting `weibull` and `hill` curve types in R and Stan, attaching raw spend and support to Stan decomposition outputs when `spend_map` is supplied, and adding a KPI decomposition funnel table/chart to the deck builder. Validated with targeted transform, Stan contract, quasi-geo handoff, prior/deck checks, selected bundle tests, and a Stan compile after the curve-type contract change.
- Rolling `mmm_latest_scripts` business-prior input pass, 2026-05-26
  - State after adding a unified business-prior template and conversion helper for `coef`, `roi`, `mroi`, `ikpc`, `cpkpi`, `outcome_per_cost`, and `cost_per_outcome` inputs. Prior distribution names and original mean/SD/precision fields are preserved for audit, while the Stan metadata handoff remains a transparent normal approximation on coefficient scale. Targeted prior/deck tests now cover all major business-prior input types and uncapped precision preservation.
- Rolling `mmm_latest_scripts` Stan geometry/efficiency pass, 2026-05-27
  - State after removing unused centered/non-centered group-intercept parameters, dropping shared-alpha group intercept state, precomputing fixed-curve adstock/saturation transforms as Stan data, preserving actual geometry settings in fit outputs, and making the sensitivity runner accept geometry variants. The hostile Stan smoke still intentionally surfaces max-treedepth under very short warmup, but fixed-curve runtime dropped sharply and a longer 300-warmup geometry probe completed with zero divergences and zero max-treedepth hits across base, centered-predictor, and shared-alpha variants. Validated with source smoke, Stan compile smoke, 21 Stan contract checks, 10 transform checks, 6 quasi-geo-to-Stan handoff checks, and 12 hostile Stan sampling checks.
- Rolling `mmm_latest_scripts` Stan readiness/sensitivity pass, 2026-05-27
  - State after adding analyst-facing model-readiness diagnostics (`diagnostics_model_readiness.csv` and issue-level detail), fixing one-chain R-hat misuse, and hardening `run_hier_mmm_sensitivity()` so `...` arguments do not duplicate fixed defaults and failed variants return error rows instead of killing the full bakeoff. A smaller public Meridian sensitivity run completed with zero divergences and zero treedepth hits for both centered and uncentered predictor variants; centered predictors improved BFMI slightly but fit was essentially unchanged, so this remains a data-dependent geometry option rather than a default flip. Validated with source smoke, 22 Stan contract checks, 10 transform checks, and 13 hostile Stan sampling checks.
- Rolling `mmm_latest_scripts` quasi evidence/project organization pass, 2026-05-27
  - State after adding analyst-facing quasi-geo evidence summaries by event, variable, and estimand; routing bundle, national-repeated, channel-specific, and diagnostic evidence into clearer recommended actions; updating the script roadmap with Stan curve-anchor, holiday-control, deck-builder, and package-structure priorities; and creating a clean `MMM_Workflow` project folder for the current package, tests, docs, and public-data pilot assets. Validated with targeted quasi-geo evidence-class tests and quasi-geo-to-Stan handoff tests.

## Current Upgrade Priority

1. Add a single analyst-facing wrapper so day-to-day use is one clean call. Done in `mmm_prior_workflow.R`.
2. Add clean population/household/market-size normalization options for future Meridian-like workflows. Done.
3. Add prior audit outputs that tell an analyst whether to use, loosen, aggregate, or seek external evidence. Done.
4. Keep calibration/benchmark hooks extensible for geo lift tests, platform lift reads, or prior MMMs. Started with KPI economics-to-coef conversion and lift-test template.
5. Add deck/dashboard outputs from decomposition and modcut spend. Done in `mmm_deck_output_builder.R`.
6. Add guarded data-driven curve learning from meaningful spend ramps. Done in `semi_univariate_prior_builder_production_final.R`.
7. Add conservative coefficient-center shrinkage option for severe univariate over-attribution. Done with `coef_center_shrinkage` and `coef_prior_half_sensitivity`.
8. Add rrate upper-bound plateau fallback. Done.
9. Add under-spend and over-spend curve-support guardrails. Done.
10. Replace pure median-active default cvalue anchoring with industry-hybrid anchoring while preserving median-active as an option. Done.
11. Extend tests before packaging another tested bundle. Done: 97 checks passing plus full Stan smoke validation, 22 hostile checks, and 23 deep recovery checks.
12. Add a base-level observed diminishing-returns diagnostic so spend variation can inform curve shape beyond ramp-weighted RMSE. Done.
13. Add explicit flat-ramp handling for underspent channels where ramp tests show no observed diminishing returns. Done.
14. Add future-spend placebo guard for planning-bias and leakage review. Done.
15. Add pooled group-level ramp evidence and ensure Stan uses observational ramp cvalue priors when metadata contains them. Done.
16. Add standalone observational quasi-experimental dose-response script. Done.
17. Add hierarchical media mean-index scope controls for better geo exposure handling. Done.
18. Add consultant deck channel rollups and period due-to movement tables. Done.
19. Add focused weak-data Stan test for geo sales plus national media only. Done.
20. Add quasi-geo dose-response stress coverage and guard against noisy flat geo reads overriding total-level diminishing evidence. Done.
21. Test centered Stan predictor sampling as a treedepth optimization. Done; retained as optional and exposed through geometry/sensitivity settings rather than forced as a default.
22. Add missing-data policies, missingness risk multipliers, and metadata handoff. Done.
23. Add contribution/elasticity/KPI-economics sanity bounds and prior-audit surfacing. Done.
24. Add hostile synthetic stress tests for correlated channels, weak ramps, missing media blocks, delayed effects, and noisy quasi-geo evidence. Done.
25. Add forward-facing prior recovery naming while keeping old source file compatible. Done with `prior_recovery_builder.R`.
26. Add multivariate ridge scans for coefficient prior recovery under multicollinearity. Done.
27. Add recoverable versus unseparable multicollinearity tests. Done.
28. Add living checklist for workflow scope and future ideas. Done in `MMM_WORKFLOW_CHECKLIST.md`.
29. Add standalone quasi-geo test engine for hidden/natural geo tests. Done in `quasi_geo_test.R`.
30. Change quasi-geo test defaults to diagnose all candidate events after estimation rather than hard-dropping contaminated events. Done.
31. Add full-workflow source file and cleaner public aliases without breaking old entry points. Done.
32. Add prior evidence summary for analyst-facing layer review. Done.
33. Add quasi-geo lift uncertainty, placebo diagnostics, and no-lift overclaim protection. Done.
34. Add Stan sampler-setting recommendations from diagnostics. Done.
35. Reduce unused Stan state dimensions for non-level baselines. Done.
36. Reduce fixed-curve Stan autodiff work and unused alpha geometry. Done.
37. Add analyst-facing Stan model-readiness outputs and robust sensitivity runner summaries. Done.

## Missing Capabilities To Close

- Tests now ship in `tests/` with a quick runner and an opt-in deep workflow hardening test; this is still not a formal R package or CI setup.
- Reach/frequency-specific curve logic is still not implemented.
- No formal package structure or CI.
- The Shiny app is generated and parse-tested, but not browser/runtime-click tested here.
- The Stan smoke test uses short chains for speed, so it verifies compilation/data/output plumbing rather than production-quality convergence.
- One-chain sensitivity runs are useful for geometry screening, but R-hat is now suppressed for one-chain diagnostics and final production review should use multiple chains.
- Short-chain hostile Stan tests can still hit max treedepth by design. After the fixed-curve precompute and alpha-geometry cleanup, longer-warmup synthetic probes cleared divergences and treedepth hits; persistent production treedepth should now be treated mainly as an identification/prior-strength/data-design issue rather than script plumbing.
