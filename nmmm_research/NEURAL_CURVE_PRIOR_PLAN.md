# Neural Response-Curve Prior Plan

This lane is a curve-prior creator, not a replacement for the causal MMM.

## Design Choice

The model is a black-box neural diagnostics encoder with constrained,
auditable outputs:

- flexible monotone response grid over spend/support levels
- conservative fallback-blended response grid for weak evidence review
- adstock decay prior
- saturation score
- confidence score
- fallback/default weight
- uncertainty width

It does not output Hill, Weibull, or any other named curve parameter. Named
curves are used only to generate diverse synthetic truth and to audit recovery.
The primary model curve is the learned monotone grid. The conservative blend is
reported separately because it can be safer for weak evidence, but it is not
always the best point estimate.

## Current Architecture

- Aggregate diagnostics MLP for stable summary features.
- Per-channel temporal TCN stem over support, spend, impressions, clicks, GRPs,
  reach, frequency, missingness masks, and optional residualized KPI signal.
- Geo-time TCN stem over target-channel geo variation, population-scaled geo
  support/spend where population exists, national movement, staggered-ramp
  heterogeneity, concentration, and KPI geo-shock diagnostics.
- Cross-channel Set Transformer over all channels with a target-channel marker,
  so collinearity, co-movement, and conditional identifiability can be learned.
- Shared output heads for monotone curve grid, adstock decay, saturation score,
  confidence, fallback/default weight, and uncertainty width.
- Quasi-geo diagnostic feature layer that scans geo-specific up/down media
  shocks, donor-market KPI deltas, other-media contamination, donor
  contamination, sign consistency, and identifiability score. This is a feature
  layer, not a replacement for the production quasi-geo lift script.

## Why This Shape

Saturation is weakly identified from observational MMM data without clean
experiments, quasi-geo shocks, or strong support variation. A neural model can
learn useful regularities from many known-truth panels, but it should still
surface when the data do not identify the curve. That is why the output includes
confidence, fallback weight, and uncertainty rather than pretending every curve
is equally knowable.

## Current Synthetic Truth Families

- Hill
- Weibull
- Gompertz
- concave heavy-tail
- threshold / S-shaped
- linear plateau
- near-linear

The smoke suite trains on mixed-family panels and does not provide curve-family
labels as model inputs.

## Promotion Criteria

This model should remain a prior/diagnostic layer until it proves it improves
Stan or optimizer decisions on held-out known-truth simulations. Promotion
should be based on curve/adstock/decision recovery, not only R2.

## Low-Data Suite

`run_curve_prior_low_data_suite.py` now tests agency-practical data regimes:

- geo KPI + geo support/spend + population
- geo KPI + geo support/spend without population
- geo KPI + spend only + population
- geo KPI + support only + population
- geo KPI + national repeated media + population
- national-only support/spend
- richer geo media as an upper benchmark

Synthetic panels now rotate through explicit geo-identification patterns:

- all channels follow similar geo trends
- some channels follow similar geo trends
- some channels are geo differentiated
- one target channel has distinctive geo movement
- an embedded geo-lift-like treated-market shock exists
- national media is distributed to geos by population
- mixed/random combinations

It also rotates KPI scales/noise for sales-like, subscriptions-like, and
leads-like outcomes, plus harshness profiles for minimum clean data, minimum
messy data, public macro proxies, and richer noisy controls. The saved outputs
include predictions, metrics, training features, training history, and a
reusable Torch checkpoint.

The fallback/default target is synthetic-truth calibrated: diagnostics say how
weak or confounded the observed evidence is, and known-truth training labels say
when the conservative default curve is actually close enough to truth to lean on
more heavily. On real data, truth is absent, so the model can only infer this
behavior from diagnostics learned during synthetic pretraining.

## Conservative Shape Guardrails

Curve outputs are monotone by construction. Training also penalizes increasing
marginal slopes on the response grid, using interval-adjusted slopes rather than
raw grid increments. This makes concavity/diminishing returns the default
pressure while still allowing threshold or S-shaped curves when the data and
known-truth synthetic labels make that worthwhile.

Missing, invalid, or all-zero synthetic curve labels no longer become fake
linear targets. They fall back to the conservative default curve, get low target
quality, are down-weighted in curve/marginal loss, and push fallback/uncertainty
targets upward.

## Harsh Holdout

`run_curve_prior_harsh_holdout.py` trains on one set of synthetic panels and
scores separate harsher holdout panels. Holdouts deliberately include higher
national media repetition, stronger collinearity, media missingness blocks,
business shocks, weak/no controls, and noisy KPI variants. Outputs include
holdout predictions, holdout metrics, fallback calibration buckets, and a saved
checkpoint.

Quasi-geo features preserve event counts even for imperfect data, but they also
report `national_common_trend_score` so population-scaled national media is not
mistaken for clean geo identification just because larger markets have larger
raw media deltas.

## Next Hardening

- benchmark against Meridian-style default anchors
- test downstream impact on Stan priors and optimizer scenarios
- add by-truth-family and by-data-regime recovery scorecards
- calibrate fallback blending against downstream Stan/optimizer performance
