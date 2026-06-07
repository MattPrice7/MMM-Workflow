# 2026-06-07 Scratchpad Review

## Promoted / Implemented

- Posterior density charts: remove any connected quantile trace; show the density curve as the main line, q05/q95 as vertical interval bounds, and q50 as the median marker/dashed reference.
- Quasi-geo rollups: carry optional `channel` and `rollup_path` metadata from `variable_map` into quasi-geo events, summaries, and prior recommendations for reporting rollups such as total Media or total Social. This remains reporting metadata and does not create a pooled causal estimand.

## Added To Future / Maybe

- Excel chart workbook builder: a separate consultant-facing Excel output script that builds the core deck charts/tables in workbook form.
- Time-varying effectiveness multipliers: optional, gated, tightly regularized smooth multipliers around 1.0; off by default.
- Context-varying effectiveness modifiers: optional named hypotheses such as seasonal TV effectiveness or TV/social synergy, with explicit metadata, tight priors, sign constraints when justified, and min/max multiplier bounds.

## Not Defaulting

- Latent week-to-week media effectiveness movement should not be a default model feature. It can become a media/baseline attribution escape hatch unless heavily regularized and clearly audited.
