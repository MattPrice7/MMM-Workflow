# NMMM Full Grid Analysis

Run:

```text
run_torch_nmmm_grid.py --mode full --epochs 350
```

Rows: `96`

Dimensions:

- scenarios: `standard`, `messy_realistic`, `hostile_collinear`, `weak_geo`
- seeds: `20260603`, `20260604`
- media features: `support_only`, `support_spend`, `rich_media`
- synthetic contribution supervision weights: `0`, `0.10`, `0.25`, `0.50`

Primary metric:

```text
known_truth_recovery_score_0_100
```

This score emphasizes contribution recovery, ROI-like recovery, curve recovery,
and parameter recovery. R2 is diagnostic only.

## Main Findings

1. Synthetic contribution supervision is the strongest improvement lever.

Mean known-truth recovery score by supervision weight:

```text
0.50 -> 87.3
0.25 -> 86.3
0.10 -> 85.3
0.00 -> 84.2
```

This suggests the issue is not that neural models cannot recover MMM outputs.
The issue is training objective and training data. When the model is shown true
contribution labels in simulation, it learns more MMM-correct decompositions.

2. More media feature types are useful but not automatically better.

Mean known-truth recovery score:

```text
support_spend -> 85.9
support_only  -> 85.8
rich_media    -> 85.7
```

The richer media feature set won in some messy-realistic cases, but the average
gain was small. The model can use spend/impressions/clicks/GRPs/reach/frequency,
but it still needs enough scenarios to learn which signals matter.

3. NMMM beat the transparent transformed-MMM baseline on known-truth recovery in
every tested scenario when using the best NMMM config.

```text
scenario            baseline   best_nmmm   lift
standard              84.0       90.3      +6.3
messy_realistic       79.3       85.8      +6.5
hostile_collinear     78.8       89.0     +10.1
weak_geo              81.5       91.4      +9.9
```

4. R2 gap to oracle is still worse for NMMM.

The transparent baseline often has better prediction fit. NMMM is winning on
recovery but not forecast fit. This is acceptable for research, but it means the
objective still needs work before this can be trusted.

## Current Recommendation

Continue the NMMM project.

Do not make TFT the default yet. The constrained interpretable hierarchical NMMM
is already beating the transparent baseline on known-truth recovery in this
grid. The next step should be better simulation pretraining and objective design.

TFT should be tested as a challenger after this model is stable:

- if TFT improves recovery, promote it
- if TFT only improves R2, do not promote it

## Next Tests

1. Run more seeds and PySiMMMulator scenarios.
2. Add multi-panel pretraining instead of training one model from scratch per panel.
3. Add contribution-share / ROI stability penalties.
4. Add baseline stealing diagnostics.
5. Add held-out simulation transfer tests:
   - train on standard + messy
   - test on hostile collinear + weak geo
   - no contribution labels in evaluation

## Industry Alignment

Google's public MMM work emphasizes carryover/adstock, shape/saturation effects,
priors, calibration, contribution, ROI, mROI, and response curves rather than
raw forecast-only modeling:

- Google Research: Bayesian MMM with carryover and shape effects
  <https://research.google/pubs/pub46001>
- Google Meridian ROI, mROI, and response-curve definitions
  <https://developers.google.com/meridian/docs/basics/roi-mroi-response-curves>
- Google LightweightMMM supports carryover and Hill-adstock transforms
  <https://github.com/google/lightweight_mmm>

The NMMM lane should keep those same estimands as the scoreboard.
