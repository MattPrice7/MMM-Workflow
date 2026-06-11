# Analyst Idea Backlog

This is the cleaned, non-train-of-thought backlog promoted from the scratchpad. It is intentionally lighter than `MMM_WORKFLOW_CHECKLIST.md`; only stable implementation decisions should graduate into the formal checklist.

## Active To Do

- Stan / curve-input contract:
  - document sampled-curve vs fixed-curve behavior clearly
  - keep the median saturation anchor based on active raw-support weeks
  - active-only normalization is now implemented; monitor real-data behavior and keep `all_train` as a sensitivity option
- Prior input cleanup:
  - add a central SD/precision parser later
  - default analyst-facing scale should be SD
  - preserve true inverse-variance precision internally and in audit outputs
- Improve chart-builder usability:
  - searchable dropdown slicers
  - client color palettes and channel color overrides
  - fair share index
  - bubble charts with spend, efficiency, and contribution
  - period-change filters
  - response, ROI, and mROI curve cleanup
  - credible-interval bands when posterior draw outputs are supplied
- Add posterior diagnostic chart outputs:
  - coefficient posterior distributions
  - prior-vs-posterior shifts
  - contribution intervals when draw-level decompositions are available
  - response-curve and optimizer uncertainty intervals

## Future

- Add optional semantic variable metadata for richer slicers:
  - placement
  - creative
  - funnel stage
  - channel family
  - campaign or platform labels
- Extend chart-builder slicers to use `rollup_path` plus optional semantic metadata.
- Add optional `model_id_parts` metadata for flexible model-cell descriptions such as geo, product, line of business, retailer, store type, segment, platform, or any future modeling dimension.
- Add hierarchy-family metadata for opt-in pooling across products, channels, or model ID parts.
- Add explicit explanations and validators for metadata fields that are easy to confuse:
  - `source_entity` = source/halo metadata, not the hierarchy key
  - `coef_hierarchy_scale` = group-level pooling strength multiplier
  - `modeled_x_basis` = label for support/spend/custom modeled input basis
  - `trade_same_unit` = whether trade/promo units can be capped against KPI units
  - `include_in_mix_diagnostic` = whether a spend column should enter mix/funnel diagnostics
- BAU response curves:
  - evaluate optional guarded rrate/adstock estimation
  - do not use univariate rrate search as strong evidence without quasi-geo or joint-model support
- Add explicit interaction/effect-modifier helpers for selected relationships, such as upper-funnel media modifying search response.
- Add brand-equity or long-run media-stock state only when supporting external signals exist, such as awareness, consideration, organic search, branded search, or other demand indicators.
- Add quasi-geo and optimizer chart packs once the core chart builder UI is more stable.

## Maybe / Research

- Time-varying media coefficients with tight smoothing priors.
- Context-varying effectiveness multipliers for named hypotheses, off by default and tightly regularized.
- Neural curve estimation metadata experiments: compare "media-only metadata" versus richer channel/context labels, but prevent memorizing channel names as fixed truth through randomized simulation labels and channel permutations.
- Crossed random effects across product, geo, retailer, and other model ID dimensions.
- Latent mediation from non-interactable channels into interactable channels.
- Neural MMM / TFT as a challenger model after hostile known-truth validation.

## Current Position

`rollup_path` is now the default arbitrary-depth rollup mechanism for optimizer planning groups and deck reporting. Parent rollup nodes remain metadata, not modeled variables.

The main production path should stay:

1. Stan MMM and decomposition.
2. Quasi-geo evidence.
3. Optimizer / scenario planner.
4. Chart builder.
5. Neural research.

The ideas above should improve that path without re-centering the package around support-layer scripts.
