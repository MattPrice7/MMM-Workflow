# 2026-06-05 Scratchpad Review

## What You Added

- Keep `ANALYST_THOUGHTS_SCRATCHPAD.md` as a raw inbox, then have Codex move acknowledged notes into a cleaned review/backlog file.
- Treat placement, creative, and channel breakouts as separate variables when data supports it, but add metadata to map those breakouts back to total channels.
- Keep `group_col` / model ID flexible, but consider future metadata for model ID parts such as geo, product, line of business, retailer, store type, segment, or platform.
- Consider optional cross-product or hierarchy-family pooling later, especially for product-specific pooling, global channel pooling, and halo variables.
- Consider whether posterior plots and two-dimensional parameter diagnostics should be Stan diagnostics, chart-builder outputs, or both.
- Consider context-varying or time-varying coefficients, such as TV being more effective in certain seasons or search being more effective when upper-funnel media is active.
- Consider upper-funnel effects on lower-funnel tactics and whether search or organic search should reflect media-driven demand spillover.
- Consider whether media can move the baseline or brand-equity state, not only produce short-run incremental contribution.
- Improve chart builder usability: client-sendable version, optimizer/scenario backend, bubble charts, fair share index, searchable slicers, channel/subchannel filters, period-change filters, custom colors, response/ROI/mROI curves, and credible intervals.
- Keep NMMM/TFT ideas as a research track with stronger synthetic training data, missingness masks, counterfactual decomposition consistency, monotonic/smooth response constraints, and hostile validation.

## Codex Response

- Scratchpad workflow: accepted and implemented. The scratchpad is now a clean inbox, and this file is the first cleaned review note.
- Placement and creative breakouts: good practical idea. The current model can already ingest these as separate variables. The next useful upgrade is metadata for `parent_channel`, `sub_channel`, `placement`, `creative`, or similar rollups for charts, optimizer constraints, and reporting. This does not need a Stan architecture change.
- Flexible model ID: current `group_col` is already generic. A future `model_id_parts` metadata layer is useful for rollups and optional hierarchy instructions, but not urgent for the core model.
- Cross-product and crossed hierarchy: valid but should be opt-in later. The current single group/model-id hierarchy is safer. Crossed random effects across product, geo, retailer, etc. can become difficult to identify and should not be added until the core package is stable.
- Halo variables: explicit variables and metadata are safer than automatic pooling. Halo effects are often asymmetric, so they should not inherit the same pooling assumptions as own-product media unless the analyst opts in.
- Posterior plots: should live in chart-builder diagnostics, sourced from Stan outputs. Stan should expose the necessary posterior summaries/draw tables; the chart builder should render them.
- Time-varying/context-varying coefficients: conceptually real but high-risk. If added later, they should be highly regularized, opt-in, and probably implemented as coefficient multipliers or a small number of pre-specified interactions rather than free time-varying media effects.
- Upper-funnel effects on lower-funnel/search: useful future feature, but start with explicit interaction/proxy variables and diagnostics before adding a latent mediation/state-space layer.
- Media moving the base: real as brand equity or long-run demand-building, but very hard to identify from normal MMM data alone. A future brand-equity state or long-adstock stock could be useful when there are external brand/search/organic signals.
- Chart builder: these are active usability gaps. The highest-value next charting upgrades are color/theme controls, searchable slicers, period filters, fair share/bubble charts, curve fixes, and credible intervals where draw-level outputs exist.
- NMMM/TFT: remains research, not production. The strongest path is using realistic synthetic panels and known-truth recovery to challenge the transparent Stan workflow, not replacing it yet.

## Promotion Decision

Move to active to-do:

- Parent/subchannel/placement/creative metadata for rollups, chart slicers, and optimizer grouping.
- Chart-builder UI and chart polish: searchable slicers, client palettes, curve display, fair share/bubble charts, period filters, and credible intervals from available posterior/draw outputs.
- Posterior parameter/contribution diagnostic charts in the chart builder.

Move to future:

- `model_id_parts` metadata for flexible model-cell decomposition and optional hierarchy instructions.
- Product/channel hierarchy-family metadata for opt-in pooling rules.
- Explicit upper-funnel to lower-funnel interaction helpers and diagnostics.
- Brand-equity or media-stock state when external brand/search/organic signals are available.

Move to maybe / research:

- Time-varying media coefficients.
- Crossed random effects across multiple group dimensions.
- Neural MMM/TFT as a production challenger.
- Latent mediation from non-interactable channels into interactable channels or baseline.
