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

## Next Hardening

- add direct sequence and geo-variation encoders
- add quasi-geo / ramp evidence features when available
- benchmark against Meridian-style default anchors
- test downstream impact on Stan priors and optimizer scenarios
- add by-truth-family and by-data-regime recovery scorecards
- calibrate fallback blending against downstream Stan/optimizer performance
