# 2026-06-10 Scratchpad Review

Reviewed the new scratchpad notes and sorted them without clearing the raw inbox.

## Handled / Corrected

- `rrate` is already in `hier_mmm.stan` and is estimated when `sample_curve_parameters = "always"`. Fixed-curve modes intentionally precompute adstock/saturation in R and therefore do not estimate `rrate`; this is a speed/geometry option, not a missing model feature.
- `anchor_saturation` is correctly interpreted as "percent saturated at median active support." For Hill and Weibull, this gets converted into the relevant internal curve-rate parameter. It is not literally always `cvalue`, and it is not EC50 unless the curve family/shape implies that relationship.
- The Stan header documentation was cleaned up so optional curve parameters no longer look required.
- Quasi-geo already carries `rollup_path` metadata for reporting rollups. It should not automatically claim pooled rollup causal effects unless the rollup has identifiable variation.
- `PROJECT_CONTRIBUTION_LOG.md` already exists in `docs/`; future project-management/resume notes should route there rather than into the technical checklist.

## Promoted To Active / Near-Term

- Evaluate active-only curve normalization before changing defaults. Current behavior:
  - median saturation anchor uses active raw-support weeks, excluding raw zero weeks;
  - adstocked support normalization uses all training rows.
  Candidate option: `curve_normalization_scope = c("all_train", "active_train")`. This must be implemented consistently across Stan, R-side transforms, decomposition, response curves, optimizer, and BAU curves before any default changes.
- Add a central prior-scale parser later so analyst-facing priors can use SD by default while still accepting precision and preserving true inverse-variance precision internally.
- Add clearer metadata explanations/validators for `source_entity`, `coef_hierarchy_scale`, `modeled_x_basis`, `trade_same_unit`, `halo_enabled`, and `include_in_mix_diagnostic`.
- BAU response curves should eventually support guarded optional rrate/adstock estimation, but only as a weak diagnostic. A univariate rrate search can overfit trend and should not tighten priors by itself.

## Future / Maybe

- Optional model-ID part metadata can describe arbitrary group pieces such as DMA, product, LOB, retailer, store type, segment, or platform. Use this for future pooling instructions such as "pool within product" or "pool within product + retailer." Do not overload `source_entity` for this.
- Optional context-varying effectiveness multipliers are plausible but should stay advanced/off-by-default. They need tight priors, clear sign/bound controls, and diagnostics so they do not become a baseline/media attribution escape hatch.
- A separate Excel chart workbook builder is useful for consultant workflows, but it should stay separate from the Shiny/static deck builder.
- NMMM/channel metadata remains research. A neural model may benefit from some metadata, but simulations should randomize channel names/order/parameters so it cannot memorize "TV always behaves like X."

## Direct Field Answers

- `source_entity`: source/halo metadata. Example: a chips TV variable in a pretzels model can be marked as sourced from chips, so halo diagnostics know it is not native pretzels media. It is not the main hierarchy key.
- `coef_hierarchy_scale`: multiplier on group-level coefficient variation. Lower values shrink group coefficients more strongly toward the shared variable effect, useful for national-only or weakly geo-resolved media.
- `modeled_x_basis`: audit/reporting label for whether the modeled input is support, spend, or custom. It is not always redundant because the modeled column may be impressions while spend is only used for economics, or spend may be the modeled execution variable.
- `trade_same_unit`: means trade/promo support is in the same units as the KPI, so a hard cap like "trade contribution cannot exceed trade units" may be defensible.
- `halo_enabled`: should default false. It exists only to explicitly allow same-unit trade/promo contribution above observed trade units when the analyst has a defensible halo/spillover reason.
- `include_in_mix_diagnostic`: controls whether a spend column enters mix/funnel diagnostics. Useful when a variable has spend-like data that should not be treated as paid media spend.

## Not Applied Yet

- Did not change active-only normalization in code because that affects core transform semantics and needs coordinated tests.
- Did not add free time-varying beta. If added later, use a multiplier around the response curve, tightly regularized around 1.0, and only for named hypotheses.
- Did not clear the scratchpad.
