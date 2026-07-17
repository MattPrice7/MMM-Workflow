# Sequential Hierarchical Bayes Design

The default parent-to-child handoff now uses explicit partial pooling inside
Stan. Parent posterior uncertainty constrains the training-period,
spend-weighted aggregate child effectiveness once; a model-estimated
One shared layer-level `tau_effectiveness` governs sibling dispersion around
distinct latent parent centers. Adstock uses one independent shared
`tau_adstock` on the logit-retention scale. Individual child saturation priors
remain generic, with optional collective parent-shape reconciliation.

Identification diagnostics remain visible for audit and branch qualification,
but they no longer convert directly into child prior precision in the default
hierarchical-tau path. Legacy reference-calibration and coefficient-conversion
paths remain explicit opt-ins for reproducibility.

## Purpose and Terminology

Econimap's proposed coarse-to-fine workflow uses the safest identifiable
higher-level media evidence to regularize progressively more granular models.
Every layer refits the same KPI at its own selected media grain.  A child is
informed by its parent but is not algebraically forced to reproduce the parent.

Because parent and child often reuse the same KPI history, this is **staged
empirical Bayes / modular Bayes**, not a single fully coherent joint posterior.
Econimap must therefore label inherited priors as *data-derived*, inflate their
uncertainty for data reuse and transfer error, and report sensitivity to both
the inherited and a weak/default prior.  It must never present the child
interval as if the parent and child were independent confirmations.

The product-facing name can remain **Sequential Hierarchical Bayes**.  The
technical documentation and audit tables must use the more precise term above.

## What Exists Today

- The Stan MMM jointly estimates shared variable-level curves and hierarchical
  group-level coefficients.
- `coef_hierarchy_scope` supports no pooling, global pooling, and one-level
  keyed pooling; `coef_hierarchy_part_indices` can derive the latter from a
  composite group/model ID.
- `rollup_path` supports arbitrary-depth reporting and planning rollups.
- Business-prior conversion, curve priors, quasi-geo evidence, prior/posterior
  diagnostics, and model audit outputs already exist.
- Phase 1 sequential support fits a frequentist total-paid-media root on the
  nationally aggregated KPI and total paid spend by default. It uses blocked
  bootstrap and a guarded linear versus concave adstock/Hill total-spend
  search. It then transfers equal-effectiveness evidence through a joint
  reference-spend calibration likelihood, plus estimable adstock and
  parent-regularized
  saturation priors, to either modeled leaves or a selected spend-rollup depth
  in the existing joint Stan child model.

These are foundations, not the full staged system. `rollup_path` creates a
real aggregate target layer at any requested declared depth, and an explicit
continuation can pass fitted parent response-curve posterior draws to the next
deeper selected layer. There is not yet a hidden fully automatic multi-stage
runner: analysts deliberately choose each material depth and review each
parent fit before continuation. `hierarchy_key` is metadata, not a multi-layer
prior runner; and the current implementation does not add a shared
sibling-variable latent prior inside Stan.

## Core Architecture

Econimap should represent three separate, explicit graphs:

| Graph | What it represents | Used for |
|---|---|---|
| Business graph | Where the KPI is generated: brand, product, market, retailer, customer segment, etc. | Panel/model scope, reporting, target comparability |
| Media graph | How a media lever decomposes: total media, funnel, channel, subchannel, provider, campaign, etc. | Sequential media model layers and prior propagation |
| Driver graph | How non-media drivers decompose: price, trade, distribution, competition, availability, etc. | Governance, reporting, optional staged treatment modeling |

Graphs are user-defined rooted forests, not names parsed from underscores.
Composite `group_col` values remain supported for a model cell, but any future
pooling family is derived from declared graph keys, not inferred string parts.
The current part-index interface remains backward compatible.

### Required Tables

`rollup_nodes`

- `node_id`, `graph_type` (`business`, `media`, `driver`), `parent_node_id`,
  `node_name`, `depth`, `active`, `modeled`, `aggregation_basis`
- media-only fields: `treatment_class`, `support_type`, `support_comparable`,
  `cost_basis`, `curve_family_allowed`, `adstock_family_allowed`
- driver-only fields: `driver_governance` (`control`, `treatment`,
  `moderator`, `diagnostic`), `causal_claim_allowed`

`rollup_edges`

- `parent_node_id`, `child_node_id`, `allocation_basis` (`spend`, `support`,
  `user_share`, `unknown`), `relevance_group`, `allow_parameter_propagation`

`variable_map`

- existing variable metadata plus `media_node_id` or `driver_node_id`,
  `business_node_id`, `model_scope_id`, `observed_spend_col`,
  `support_col`, `spend_equivalent_col`, `support_cost_type`, and optional
  `pooling_family_id`.

`model_layer_config`

- `layer_id`, `parent_layer_id`, `media_node_id`, `business_scope_id`,
  `model_type`, `max_depth`, `requested`, `selection_mode`,
  `prior_transfer_mode`, `fit_mode` (`MCMC`, `VI`, `MAP`), and data version.

`model_registry`, `prior_ledger`, and `reconciliation_audit`

- immutable IDs for every fit, source posterior, transformed prior, user
  override, data snapshot, software version, diagnostics, and resulting child
  fit.  No generated prior may exist without this provenance.

## Model Sequencing

1. Validate graph structure, variable/node mappings, panel keys, units, and
   parent-child aggregation rules.  Stop on cycles or mixed support units that
   are being aggregated without a declared spend-equivalent conversion.
2. Start with an opt-in **total paid-media** root on nationally aggregated KPI
   and total-media spend, using spend rather than raw heterogeneous support for
   cross-media aggregation. A geo-panel root is an explicit secondary mode and
   is appropriate only when geo media variation and root heterogeneity are
   sufficiently identified.
   This is the default sequential anchor, not a channel-level jump. If the
   portfolio root is unstable, retain it as wide evidence or define selected
   homogeneous parent blocks before going deeper.
   A target layer is optional: analysts may go directly from total paid media
   to the first media family, channel, subchannel, provider, or any other
   declared depth. They do not need to fit every intermediate layer merely
   because it exists in `rollup_path`.
3. Run pre-fit identification diagnostics and produce a continuous shrinkage
   strength for each child branch. Strong evidence weakens inherited influence;
   weak or collinear evidence strengthens parent/default regularization. Weak
   identification alone never prunes or stops a valid Bayesian branch.
4. Fit a parsimonious parent model and verify fit, sampler, prior/posterior,
   stability, and curve-identifiability diagnostics.
5. Convert the parent posterior into **candidate inherited priors** for the
   selected children.  Apply transfer and data-reuse uncertainty before a
   child model sees the prior.
6. Fit each approved child branch against the original KPI.  Independent
   branches may run in parallel.  Use MAP/VI for screening only; use MCMC for
   material final decisions.
7. Compare each child-implied aggregate with its parent.  Surface disagreement
   as a reconciliation diagnostic, never as an automatic constraint.
8. Stop at the configured maximum depth. Retain structurally invalid or
   unsupported branches at an auditable parent/remainder grain while valid
   siblings continue. Identification weakness changes regularization and the
   prior-dominance label, not structural branch validity.

## Prior Construction Rules

### Effectiveness

The transferable primary quantity is effectiveness, normally log outcome per
cost (or log ROI) for paid media.  At cross-channel levels use observed spend;
within a genuinely comparable media family, use outcome per support unit when
support is credible and consistently defined.

A parent total-media ROI is a portfolio average, not proof that every child
has the same ROI.  Do not split parent contribution into deterministic child
contributions.  Instead, use a joint child prior such as:

`log(effectiveness_child) ~ Normal(log(effectiveness_parent), transfer_sd)`

where `transfer_sd` includes parent uncertainty, data-reuse inflation,
parent-child heterogeneity, allocation ambiguity, support/cost mismatch, and
an optional user override.  This centers the family on the parent without
asserting a child allocation.  The child model can depart when its independent
variation supports it.

Phase 1 records an `ikpc` audit prior for every child: the center is parent
effectiveness, while implied contribution is `observed child spend x parent
effectiveness`. By default it applies this evidence in Stan as a joint normal
likelihood on the child's modeled contribution at that observed reference
spend/support, rather than converting it once into a coefficient prior. This
preserves the effectiveness meaning when a child adstock or saturation curve
updates. `sequential_effectiveness_application = "coefficient_approximation"`
remains available only for compatibility. The child fit remains joint and may
disagree with parent evidence.

### Optional Media Rollup Depths

For Phase 1, a `rollup_path` is a media graph expressed from coarse to fine.
For example:

```text
total_paid_media > social > meta > meta_campaign_1
```

Use `>` as the canonical hierarchy delimiter. A slash remains part of a label,
so `CTV/OLV` is one node rather than an accidental extra level. `|` remains a
legacy alternate delimiter.

The root is always total paid media. Numeric `rollup_depth` values are counted
below that optional root node: depth 1 is `social`, depth 2 is `meta`, and
depth 3 is `meta_campaign_1`. `rollup_depth = "leaf"` retains the original
modeled variables. Thus `rollup_depth = 3` is a legal direct handoff from the
total-media root even when depth 1 and 2 are not fitted.

`build_sequential_rollup_plan()` previews a multi-depth graph and
`build_sequential_rollup_layer()` creates one selected layer. The aggregate
uses **observed spend only** across media. It intentionally does not sum raw
impressions, GRPs, clicks, reach, or other heterogeneous support units. Source
media metadata rows are replaced by the generated aggregate rows, preventing
double counting in the child model. The generated layer defaults to the
package-wide Hill curve family; pass `curve_type_default = "weibull"` when a
Weibull child layer is intentionally required.

For a deeper sequential fit, first run a material parent stage with
`fit_child = TRUE`, inspect its fit and posterior diagnostics, then call
`continue_sequential_hierarchical_bayes()` for a deeper numeric depth. The
continuation derives parent outcome-per-cost from the parent's total-scope
response-curve posterior draws at the observed spend point. It never silently
falls back to the original total-media root when a fitted parent is available.

### Root and Handoff Safety

The default root aggregates KPI and total paid spend to national time before
fitting. A `hierarchical_panel` root is explicit and only allowed when total
media has genuine within-period group variation. Its continuous identification
screen informs pooling and reporting but does not impose a heuristic pre-fit
veto. The root compares a linear total-spend limit with a constrained
Hill/geometric-adstock profile-likelihood fit. Nonlinear parameters are optimized
from deterministic Sobol multistarts while linear baseline, control, and media
coefficients are solved conditionally. The old rrate/anchor grid is retained only
as a diagnostic and can never be selected. The linear result remains the default
unless the nonlinear curve clears the configured AICc guardrail. Flat profiles,
repeated boundary solutions, or unstable multistart convergence explicitly
recommend the Bayesian fallback rather than manufacturing a precise curve.
Moving-block residual bootstrap draws preserve the original media timeline,
Fourier calendar phase, and adstock history. Each replicate reselects linear
versus nonlinear form and re-optimizes rrate, half-saturation, and steepness, so
transferred uncertainty includes transform-selection and nonlinear-parameter
uncertainty rather than only conditional fit error.

Spend scope and modeled-support scope are separate contracts. National spend
drives national root reconciliation and ROI economics; actual geo support drives
the child likelihood and geo-identification diagnostics. Mechanically allocated
support never creates hierarchical identification. Raw spend and raw support
remain separately preserved, with source-unit reconstruction audits for GRP and
reach conversions. Intensive metrics such as frequency, CPM, CPC, rates, and
indices are not treated as additive media exposure.

The root and child share one baseline and train/holdout contract. Root curve
selection and bootstrap use training periods only. For the matched shared-effect
Fourier root, held-out rows also produce a media-plus-baseline versus
baseline-only comparison using training-only scaling and preserved adstock
history. A positive in-sample root that does not improve this holdout is labeled
`positive_in_sample_not_transferable_holdout` and is not transferred as positive
child evidence. Mixed-effect and knot-root holdout scoring remain explicitly
unavailable rather than being misreported. The currently enforced shared
staged baseline is flat or Fourier. Shared linear-trend and joint UCM staged
baselines remain unsupported until the same design can be applied at every
benchmark and child layer.

Each root and child layer emits an observational identification screen: active
support, residualized variation after group/time structure, and media-media
correlation. Parent uncertainty and parent-to-child transfer uncertainty widen
the inherited distribution. Weak child identification instead strengthens
regularization toward that distribution; it does not manufacture extra child freedom
by widening the prior. Strong child identification modestly relaxes inherited
shrinkage so a clean child can approach a direct model. A weak branch never
silently blocks stronger siblings. When both parent and child evidence are
weak, the model uses a broad validated default with strong regularization and
labels the result predominantly default-prior-driven. Negative, near-zero, and
inconclusive parent effects become explicit neutral, wide reference-spend
calibration evidence; they are not dropped by coefficient conversion or
transferred as a negative paid-media effect.

```r
stage_channel <- run_sequential_hierarchical_bayes(
  data, metadata, dep_var_col = "kpi", group_col = "model_id",
  time_col = "week", entity_col = "brand", rollup_depth = 2,
  fit_child = TRUE
)

stage_subchannel <- continue_sequential_hierarchical_bayes(
  parent_stage = stage_channel, data = data, metadata_input = metadata,
  dep_var_col = "kpi", group_col = "model_id", time_col = "week",
  entity_col = "brand", rollup_depth = 3, fit_child = TRUE
)
```

Contribution is an aggregate reconciliation target, not the default child
prior metric.  If it is propagated, use a portfolio-level soft read or an
explicit share model with large allocation uncertainty.

### Curve Parameters

- Propagate adstock sequentially as an estimable parent-informed distribution:
  root bootstrap candidate evidence to children, then fitted parent posterior
  to the next layer. Siblings share the parent-informed center and uncertainty.
  It is never hard-fixed or reset to a generic default after a fitted parent.
  The current implementation is shared-parent regularization of independently
  estimated child parameters, not a latent sibling hierarchy. Analysts should
  override or disable transfer where cadence or media mechanics are incompatible.
- Do not treat a parent saturation curve as an independent target for each
  child. The production default is `saturation_handoff = "generic_child_prior"`,
  which leaves child saturation generic and individually estimable. In the
  opt-in `saturation_handoff = "collective_parent_shape_reconciliation"` mode,
  child saturation priors still remain generic or weak peer-informed. A soft joint
  Stan likelihood compares the **ratio** of aggregate child response at a
  perturbed support level to the same observed sibling mix at reference support.
  Parent ratios are evaluated from common posterior draws and enter as one
  covariance-aware multivariate likelihood. Effectiveness-level calibration is
  intentionally separate and remains responsible for response level. The shape
  term preserves the observed mix and context while scaling support at several
  levels; it identifies aggregate curvature without claiming weak siblings share
  a saturation curve.
- Collective shape reconciliation is not production-ready. A focused one-chain,
  624-row continuation required about 2,222 seconds for 60 warmup plus 30
  sampling iterations and hit maximum treedepth on every sampled transition.
  Keep it opt-in until gradient cost and posterior geometry improve and a
  convergence-valid recovery comparison passes.
- The former raw-contribution reconciliation mode was removed. It reused parent
  response-level evidence already handled by effectiveness calibration and did
  not isolate saturation shape. `independent_parent_prior` remains available
  only as an explicit compatibility choice. Never copy a raw `cvalue` between
  differently scaled children.
- Share or strongly regularize curve shape only within a comparable parent
  branch. Default to a fixed or strongly regularized shape at deep grains unless the data demonstrates
  independent curve information.
- Keep each curve prior's original transform, units, reference window, and
  conversion in the ledger.

### Prior Width

`transfer_sd^2 = reuse_inflation^2 * parent_variance + heterogeneity_variance +
allocation_variance + unit_mismatch_variance + override_variance`

This is intentionally conservative. Stronger parent evidence reduces the first
term; strong independent child evidence relaxes inherited shrinkage, while weak
child evidence strengthens regularization. Prior precision is capped and must be shown
next to the untransferred default-prior sensitivity result.

External randomized experiments remain the highest-quality calibration
source.  Quasi-geo evidence can modify a transfer prior only with its full
placebo, pre-fit, contamination, and power audit attached.

## Identification Diagnostics

The layer audit must preserve continuous measures, not just a score:

- active periods, zeros, gaps, pulses, spend/support magnitude, and variation;
- residual within-group variation after common trend, seasonality, controls,
  and shared national media movements;
- media-media and media-baseline collinearity, partial signal, and condition
  diagnostics;
- fit and predictive stability across reasonable baseline/lag specifications,
  rolling windows, bootstrap or resampling where feasible;
- curve/adstock posterior width, boundary pressure, prior-posterior shift, and
  sensitivity to reasonable prior widths;
- sampler health, posterior predictive checks, holdout behavior, materiality,
  and parent-child reconciliation;
- support comparability, observed versus imputed spend, and zero-spend /
  nonzero-support flags.

The decision engine returns continuous evidence, final prior width/precision,
posterior movement, and a data/parent/default/user-prior dominance label.
Structural failures remain explicit errors or parent/remainder nodes; weak
identification alone is not a stop decision.

## Non-Media and Free Support

Price, trade, promotion, distribution, competitor activity, and availability
are not automatically causal treatments.  Their graph nodes require explicit
governance: control-only, treatment, moderator, or diagnostic.  Causal claims
need an identification rationale, not merely a coefficient.

For free or added-value placements, preserve `observed_spend = 0`, support,
and a separately labelled `spend_equivalent` when a CPM/CPC/CPV/CPP conversion
is supplied.  Report outcome per support, observed-spend ROI where defined,
and imputed-spend ROI separately; never silently turn imputed value into
observed spend.

## Computation and Auditability

- Cache model objects and posterior summaries by immutable content hashes over
  source, Stan code, validation code, data, metadata, rollups, media scope,
  baseline, transfer settings, fit arguments, seed, and validation regime.
- Run sibling branches in parallel only after a parent is frozen.
- Retain posterior draws needed for transfer; do not pass only rounded means.
- Use MCMC for final material layers.  MAP/VI are screening aids and require a
  selected MCMC comparison before being trusted for decision outputs.
- Export a prior ledger, model registry, layer diagnostic table, and
  parent-child reconciliation table with every run.

## Phased Implementation

### Phase 0: contracts and audit foundation

Add graph schemas, model registry, prior ledger, version hashes, and clear
documentation.  Preserve current public functions and current `rollup_path`
behavior.

### Phase 1: one media parent to one child layer

Implemented: total paid-media frequentist root, candidate-reselecting blocked
bootstrap, continuous linear/nonlinear model-averaged curve evidence, separate
spend/support scope, train-only root evidence, data-reuse inflation, continuous
identification-driven prior strength, equal-effectiveness child priors, optional
direct handoff to any declared spend-rollup depth, `rollup_path` ledger fields,
optional root calibration penalty, optional child Stan fit, and explicit fitted
parent-posterior handoff to a deeper selected layer. A fully automatic plan
runner and a true latent sibling hierarchy remain future work.

### Phase 2: diagnostics and selective branching

Add layer suitability diagnostics, candidate branch recommendations, sibling
parallelism, and parent-child reconciliation.  Support multiple roots and
explicit user overrides.

### Phase 3: automatic plan execution and evidence integration

Add analyst-approved automatic plan execution across selected depths,
quasi-geo/experiment evidence ingestion, support-equivalence rules, and driver
governance.

### Phase 4: proof before default automation

Build known-truth simulations covering aggregation masking, bad parents,
correlated children, sparse flights, free support, halo, weak/strong geo
variation, and true child departures.  Compare against a single joint MMM,
parsimonious-only model, and generic priors.  Promote automatic depth
recommendations only when interval calibration and recovery improve.

## Open Decisions

- Whether the first public release is explicitly called `sequential_empirical_bayes`
  in code while retaining the product-facing name.
- The default transfer uncertainty inflation and how it is calibrated from
  simulation rather than hard-coded.
- Whether portfolio-level parent consistency is a posterior diagnostic only or
  an optional soft calibration likelihood.
- Which graph editor/interface belongs in the first analyst workflow.
- The minimum proof required before use on campaign/provider/creative layers.
