# Analyst Idea Backlog

This is the cleaned, non-train-of-thought backlog promoted from the scratchpad. It is intentionally lighter than `MMM_WORKFLOW_CHECKLIST.md`; only stable implementation decisions should graduate into the formal checklist.

## Active To Do

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
- Add explicit interaction/effect-modifier helpers for selected relationships, such as upper-funnel media modifying search response.
- Add brand-equity or long-run media-stock state only when supporting external signals exist, such as awareness, consideration, organic search, branded search, or other demand indicators.
- Add quasi-geo and optimizer chart packs once the core chart builder UI is more stable.

## Maybe / Research

- Time-varying media coefficients with tight smoothing priors.
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
