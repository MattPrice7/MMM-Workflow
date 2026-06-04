# Industry Alignment Notes

Last reviewed against public sources on May 27, 2026.

## Core Design Principles

1. Treat this as a staged workflow.
   - Google Meridian frames MMM as pre-modeling, modeling, diagnostics, post-modeling, and optimization. The wrapper now mirrors that pattern with data audit, mix diagnostic, prior build, prior audit, response curves, and optional metadata handoff.

2. Prefer geo-level data when it is real, but do not overclaim geo signal.
   - Meridian emphasizes geo-level modeling because it adds statistical information and geo-level insight.
   - This workflow classifies media as geo-level, partially geo-resolved, or national repeated, then shrinks or flags national-only media in geo panels.

3. Use population or market-size normalization when available.
   - Meridian-oriented data schemas include a geo population field. The new `normalize_mmm_market_size_inputs()` helper adds a clean path for per-population, per-household, or per-market-size media/KPI scaling while preserving unscaled spend for cost and budget accounting.
   - The hierarchical Stan wrapper now separates KPI scaling from media/control scaling through `dep_mean_index_scope` and `x_mean_index_scope`. Use group KPI scaling for market comparability, but prefer globally scaled media only after exposure has been normalized enough to be comparable across markets.

4. Keep exposure and spend separate.
   - Robyn distinguishes paid media exposure variables from paid media spend variables and recommends checking the spend/exposure relationship. This workflow keeps `modeled_x_col`, `support_col`, and `spend_col` separate and reports spend/support mismatch.

5. Handle multicollinearity explicitly.
   - Robyn uses ridge regularization and multi-objective selection because MMM coefficients are unstable under correlated media. This workflow's Bayesian-prior analog is transformed-variable correlation/VIF diagnostics plus precision relaxation and "external prior or aggregate" recommendations.

6. Think in KPI economics/contribution priors, even if the implementation needs coefficients.
   - Meridian emphasizes ROI prior parameterization and calibration for revenue settings. For non-revenue KPIs, the same idea should be expressed as cost per outcome or outcome per cost, such as cost per subscriber, cost per lead, or subscriptions per dollar. The new `make_coef_benchmark_priors_from_kpi_economics()` helper converts these beliefs into coefficient-scale priors for this R workflow.

7. Make response curves inspectable.
   - Meridian, LightweightMMM, Robyn, and PyMC-Marketing all treat response curves and prior/posterior inspection as central model outputs. The new `build_prior_response_curves()` helper creates response-curve tables directly from the prior estimator for analyst use when no joint model is available.

8. Use observed spend movement, but label it as observational.
   - When experiments and joint estimation are unavailable, the prior builder now checks whether residualized high-spend marginal response is lower than low-spend marginal response. This can supply a directional slope-implied `cvalue` candidate. If a meaningful ramp shows no diminishing returns, the builder can select a flatter candidate instead. When geo or segment media variation is available, group-level ramp reads are pooled into a shared curve candidate. A future-spend placebo guard helps catch planning bias and seasonality leakage. Mixed geo reads are explicitly labeled when noisy flat group signals conflict with total-level diminishing evidence. These are reported as observational quasi-experimental dose-response evidence rather than experimental calibration.

9. Carry observational curve evidence into the joint model.
   - The hierarchical Stan model now accepts an additional observed-cvalue prior term from `stan_observed_cvalue` metadata. This lets group-level ramp evidence inform the joint curve posterior instead of stopping at the semi-univariate prior file.

10. Treat semi-univariate outputs as prior evidence, not truth.
   - A purely univariate curve finder is not defensible as the final answer under common MMM problems such as correlated media, seasonality, targeting, demand anticipation, and omitted controls. This workflow therefore wraps the univariate/profile layer in multivariate residualization, transformed-variable collinearity checks, quasi-experimental ramp diagnostics, future-spend placebo checks, missing-data gates, contribution/elasticity sanity bounds, and joint Stan handoff. When those layers disagree, the workflow relaxes precision or asks for aggregation/external priors rather than tightening the curve.

11. Use ridge-style recovery when collinearity is the actual problem.
   - The prior recovery builder now runs a multivariate ridge scan across transformed media variables. This is closer to the regularized-estimation logic used by many practical MMM workflows than pure single-channel fitting. It can improve coefficient centers when correlated channels have enough independent movement, while still leaving the handoff loose and flagged when the channels are not truly separable.

12. Avoid false precision when geo exposure is not comparable.
   - If geo KPI is available but media is only national repeated, or geo media cannot be normalized by population, households, audience, impressions, reach, or another exposure denominator, the safer default is one shared channel curve with strongly pooled geo coefficients. Separate geo curves should require comparable geo exposure and enough independent variation.

13. Keep calibration optional and evidence-weighted.
   - Experiments and lift tests are best-practice anchors, but they are not always available. The workflow therefore accepts benchmarks, prior MMMs, and future lift-test templates without pretending that weak evidence is equivalent to experimental calibration.

14. Add prior-predictive sanity gates before client use.
   - Response curves should be checked against plausible contribution share, elasticity, and KPI-economics ranges before they become deck or model priors. The workflow now records implied contribution, elasticity, cost per outcome, and outcome per cost, and can accept variable-specific `sanity_bounds` so domain knowledge constrains curve handoff without hiding the math.

15. Keep geo-identification separate from national time-series evidence.
   - If media changes identically across all markets, there is no untreated geo donor pool. The workflow should preserve the event as diagnostic/time-series evidence, not geo-lift calibration. If only a subset of markets moves together, the next best extension is a treated-cell synthetic-control or matched-market/TBR read that excludes co-treated markets from donors.

16. Keep charting delivery pragmatic.
   - Static tables, PNGs, and HTML are the stable deck-building default. Excel workbooks are useful for consultant review and manual deck assembly. Shiny/Plotly are best treated as optional exploratory layers because they add runtime dependencies and are less portable for client-facing decks.

## Source Touchpoints

- Google Meridian introduction: https://developers.google.com/meridian/docs/basics/meridian-introduction
- Google Meridian model configuration and ROI calibration: https://developers.google.com/meridian/docs/user-guide/configure-model
- Google Meridian media saturation and lagging: https://developers.google.com/meridian/docs/basics/media-saturation-lagging
- Google Cloud Cortex Meridian schema fields, including geo/media/media spend/population/ROI priors: https://docs.cloud.google.com/cortex/docs/meridian
- Google Matched Markets repository: https://github.com/google/matched_markets
- Google LightweightMMM API, including prior/posterior and geo data utilities: https://lightweight-mmm.readthedocs.io/en/latest/api.html
- Google LightweightMMM repository: https://github.com/google/lightweight_mmm
- Meta GeoLift documentation: https://facebookincubator.github.io/GeoLift/
- Meta Robyn analyst guide: https://facebookexperimental.github.io/Robyn/docs/analysts-guide-to-MMM/
- Meta Robyn key features: https://facebookexperimental.github.io/Robyn/docs/features/
- PyMC-Marketing MMM API: https://www.pymc-marketing.io/en/0.9.0/api/generated/pymc_marketing.mmm.mmm.MMM.html

## What This Workflow Still Does Not Replace

- Geo lift tests or randomized experiments.
- A fully joint Bayesian/hierarchical MMM posterior.
- Reach/frequency modeling when only impressions or spend are available.
- True causal identification under severe multicollinearity.
- Business review of implausible KPI economics, contribution, and response-curve shapes.
