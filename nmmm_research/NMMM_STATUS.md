# NMMM Status

## Current Position

NMMM is now a separate Python research lane. It is not production-ready and
should not replace the R Stan MMM workflow yet.

The main neural baseline is a constrained/interpretable neural MMM: adstock,
saturation, positive media contribution, and hierarchical group x media
multipliers.

A first hierarchical TFT-style challenger now exists. It is not a forecast-only
TFT: it has explicit MMM outputs for variable contribution, coefficient proxy,
response curves, ROI-like / cost-per-KPI, and mROI-like reads. Attention is not
treated as attribution.

## Modeling Defaults

- role-aware, semantics-blind inputs
- media and controls passed separately
- media variables can include multiple numeric signals, currently support and spend
- no channel-name / funnel-position text as model features
- shared media adstock and saturation curves
- shared positive media coefficients
- shrunken group x media coefficient multipliers
- optional market-size scaling, off by default
- optional initialization from transparent transformed-MMM baseline
- optional supervised contribution loss when synthetic truth labels are available
- validation split and early stopping inside the training period
- reusable `.pt` checkpoints for the trained model brain and training metadata
- TFT-style sequence challenger with explicit contribution/curve/economics heads
- media-blind TFT baseline path and channel-isolated media contribution path
- counterfactual decomposition consistency diagnostics
- media missingness masks instead of treating missing support/spend as true zero
- simulator label audits for contribution, ROI/cost, response curves, baseline,
  KPI, and noise identities
- pre-training data validation for channel learnability, leakage risk,
  spend/support decoupling, and persistent cost-efficiency drift

## Latest Smoke Read

The transparent transformed-MMM baseline remains the standard to beat.

The headline metric is now `known_truth_recovery_score_0_100`, not R2. It
weights contribution recovery, ROI-like recovery, curve recovery, and parameter
recovery. Holdout R2 is retained as a diagnostic only.

Recent full grid with support/spend/rich media inputs and synthetic
contribution-label training:

- 96 NMMM runs completed
- best NMMM beat transparent transformed-MMM baseline on known-truth recovery in
  all four tested scenarios
- best lift was largest in hostile collinearity and weak-geo cases
- synthetic contribution supervision was the strongest improvement lever
- support+spend was slightly best on average, but rich media won some messy cases
- NMMM still has a larger R2 gap to oracle than the transparent baseline

Scenario-level best NMMM versus transparent baseline:

```text
standard:          +6.3 recovery points
messy_realistic:   +6.5 recovery points
hostile_collinear:+10.1 recovery points
weak_geo:          +9.9 recovery points
```

Interpretation: synthetic contribution labels and spend/support inputs improve
MMM recovery, which suggests the issue is training/objective design more than
an impossibility of neural recovery. The lower R2 is still not just a
noise-ceiling artifact on the current smoke test; the oracle ceiling is very
high.

First hierarchical TFT smoke before baseline isolation:

```text
known-truth recovery score:        98.0 / 100
median contribution correlation:   0.969
median ROI-like absolute error:    0.0135
median response-curve correlation: 0.999
holdout R2:                        0.978
oracle/noise-ceiling holdout R2:   0.9999
```

Interpretation: this was promising but likely optimistic. The first TFT allowed
the shared sequence state to see media before producing the baseline, which
created a baseline-stealing risk.

Hardened TFT after media-blind baseline and channel-isolated contribution path:

```text
standard smoke known-truth recovery:     86.6 / 100
median contribution correlation:         0.868
median response-curve correlation:       0.993
baseline delta when media zeroed:        0.000
counterfactual gap RMSE:                 ~0.00004
```

Interpretation: attribution recovery dropped, but the architecture is now much
more defensible. Baseline is media-blind, reported channel contribution matches
the full-vs-zeroed counterfactual contribution, and baseline stealing diagnostics
are explicit.

Latest training-data realism / label-consistency smoke after adding richer
control availability, noisy proxy controls, KPI scale variation, and audited
economics labels:

```text
known-truth recovery score:        97.9 / 100
median contribution correlation:   0.980
median ROI-like absolute error:    0.026
median response-curve correlation: 0.995
holdout R2:                        0.965
oracle/noise-ceiling holdout R2:   0.9999
baseline zeroing delta:            0.000
counterfactual gap RMSE:           ~0.00005
```

The simulator audit passed these identities before training:

```text
truth_media_contribution = coef * saturation * geo_scale
true_signal = true_baseline + true_media_contribution
kpi = true_signal + true_noise
ROI/cost labels = contribution/spend arithmetic from truth totals
response curves are monotone and match the anchor saturation
```

Interpretation: the training labels are now internally consistent enough to
support neural pretraining. The result is still synthetic evidence, not a
production claim.

Latest training-data validation layer:

```text
training_data_validation_summary.csv
training_data_validation_channel_learnability.csv
training_data_validation_cost_efficiency_drift.csv
training_data_validation_spend_support_decoupling.csv
training_data_validation_leakage_flags.csv
training_data_validation_label_consistency.csv
```

The smoke panel validation passed:

```text
mean channel learnability:          88.0 / 100
not identifiable / forecast only:   0 channels
cost-efficiency drift flags:        0 channels
spend/support decoupling flags:     0 channels
high leakage-risk input columns:    0
synthetic label checks passed:      4
```

This is a pre-training gate, not a model score. It addresses whether a channel
has enough variation, geo signal, nonzero weeks, and independent movement to
support causal/economic interpretation.

First quick TFT grid before baseline isolation, 8 runs across
standard/messy-realistic panels:

```text
best supervised TFT score:       98.7
worst supervised TFT score:      97.6
best unsupervised TFT score:     87.5
worst unsupervised TFT score:    84.5
best supervised contribution r:  0.983
best unsupervised contribution r:0.944
```

Hardened quick TFT grid, 8 runs across standard/messy-realistic panels:

```text
best supervised score:        94.9
worst supervised score:       85.4
best unsupervised score:      86.5
worst unsupervised score:     80.7
best messy supervised score:  94.9
```

Interpretation: the hardened model is slower and less overfit-looking. Messy
supervised recovery remains strong. Standard-panel recovery is lower because the
model no longer gets a media-aware baseline shortcut. This is the better
research foundation.

Hardened full TFT grid, 96 runs across standard, messy-realistic, hostile
collinearity, and weak-geo panels:

```text
mean known-truth recovery:             90.3 / 100
median known-truth recovery:           90.6 / 100
supervised 0.50 mean recovery:         93.9 / 100
unsupervised mean recovery:            84.2 / 100
mean contribution correlation:         0.889
mean baseline correlation:             0.935
mean curve correlation:                0.980
max baseline stealing ratio:           0.000
median counterfactual gap RMSE:        ~0.00004
```

Scenario averages:

```text
hostile_collinear: 91.1
weak_geo:          90.1
messy_realistic:   90.0
standard:          89.9
```

Interpretation: the hardened TFT is not only fitting; it is preserving
counterfactual decomposition consistency under generic randomized channels.
Contribution supervision still matters a lot, but the unsupervised model is no
longer collapsing.

First TFT pretraining transfer test before baseline isolation:

```text
target unsupervised from scratch:       85.0
target pretrained, no target labels:    95.1
target direct supervised reference:     84.7
```

Interpretation: this is the first strong sign that saving and reusing a
synthetically trained TFT brain may matter. The direct supervised reference
underperformed in this single seed, so do not treat it as a true upper bound.
This needs repeated seeds and a larger pretraining mix before any production
claim.

Hardened multi-panel transfer test:

```text
target unsupervised from scratch:          79.8
target pretrained, no target labels:       96.4
target direct supervised reference:        95.0
```

Setup: the pretrained brain was trained sequentially on four randomized generic
channel panels: standard, messy realistic, hostile collinear, and weak geo. The
target panel used generic channel names, randomized channel parameters, volatile
media measurement, zero flighting, and missing blocks. Target truth labels were
used only for evaluation in the pretrained-unsupervised fit.

Interpretation: this is the strongest NMMM result so far. It suggests the
right path is broad synthetic pretraining plus no-label target fine-tuning, not
plain target-only fitting.

Full data-level suite, 12 runs across one-geo/national-media, missing-media,
hostile-collinear, weak-geo/no-controls, zero-flighting, and a large
20-geo/12-channel panel:

```text
best score:                 94.4
median score:               88.3
large 20-geo/12-channel:    79.4 unsupervised, 80.3 supervised
one-geo national media:     88.5 unsupervised, 91.6 supervised
zero-flighting support only:88.8 unsupervised, 91.4 supervised
```

Interpretation: broad data-level coverage now exists. The main weak spot is
large high-channel panels: the model runs and remains internally consistent,
but needs stronger architecture or more training to recover attribution at that
scale.

Earlier mixed Hill/Weibull quick data-level suite after label-audited simulator
updates:

```text
weibull missing-media supervised:       91.8
weibull missing-media unsupervised:     85.9
one-geo national supervised:            89.7
one-geo national unsupervised:          84.8
hostile-collinear supervised:           83.3
hostile-collinear unsupervised:         82.5
all training label audits:              passed
```

Interpretation: curve-family rotation did not break the TFT path. The Weibull
missing-media case recovered well. Hostile collinearity remains the main
attribution weakness.

Current neural response-curve prior lane:

```text
truth generators: Hill, Weibull, Gompertz, concave, threshold,
                  linear-plateau, near-linear
model output:     flexible monotone response grid, not named curve params
scope:            curve/adstock prior and identifiability diagnostics
not scope:        final causal ROI/contribution estimator
```

Latest grouped-validation mixed-family curve-prior smoke:

```text
training examples:               1,440 channel examples
validation split:                held-out synthetic panel groups
model curve grid MAE mean:       0.0647 all / 0.0580 validation
model curve grid MAE median:     0.0535 all / 0.0569 validation
median curve-shape correlation:  0.9928 all / 0.9926 validation
monotonic violation share:       0.0000
adstock decay MAE:               0.0904 all / 0.1210 validation
saturation score MAE:            0.1071 all / 0.1175 validation
fallback weight MAE:             0.0191 all / 0.0202 validation
weakest curve family:            threshold / sharp S-curve
```

## What Is Working

- known-truth local simulator
- harder local scenario suite
- PySiMMMulator import/adapter
- transparent transformed-MMM benchmark
- Torch constrained MMM model
- hierarchical group x media multipliers
- population/market-size scaling as an explicit option
- baseline-based initialization
- fit/validation/holdout separation
- contribution and ROI recovery scorecards
- curve and parameter recovery scorecards
- known-truth recovery score that treats R2 as diagnostic only
- learned numeric media-feature weights for support vs spend
- full grid runner across scenarios, seeds, support types, and supervision
- richer simulator media fields: support, spend, impressions, clicks, GRPs,
  reach, frequency
- checkpoint save/load helpers so training runs are not wasted
- TFT checkpoint save/load with top-level channels, feature inputs, score
  summary, and variable economics metadata
- sequential TFT pretraining checkpoints after every synthetic panel, so long
  pretraining runs can resume from saved brains instead of wasting completed
  training
- full hostile TFT grid runner with resume support
- data-level TFT suite with resume support
- simulator control availability modes: none, partial, standard, rich, and
  noisy_proxy
- KPI scale randomization and per-channel truth economics labels
- saved `training_label_audit.csv` and `training_truth_economics.csv` for smoke,
  grid, data-level, and pretraining-transfer runs
- saved training-data validation packs for TFT smoke, grid, data-level, and
  pretraining-transfer runs

## Main Open Issues

1. The full hostile TFT grid now passes, but the large 20-geo/12-channel case
   needs architecture/training improvements.
2. The best NMMM configs beat the transparent transformed-MMM benchmark on the
   current grid, but not every NMMM config does.
3. Better fit can still mean worse attribution.
4. Population scaling improved prediction in one ablation but hurt attribution.
5. The objective likely needs stronger ROI/curve-shape regularization for
   high-channel panels.
6. PySiMMMulator scenarios still need to be wired into a repeatable suite.
7. The model still needs broader multi-scenario pretraining with more unseen
   data families, variable counts, media measurement gaps, and KPI scales.
8. Simulator realism now covers more cases, but PySiMMMulator and other
   external synthetic sources should still be used as adversarial generators.
9. Saved TFT brains are currently shape-specific. The model can train on any
   channel count from scratch, but transfer across different channel counts or
   media-feature schemas needs a more channel-invariant architecture.
10. Double-ML/orthogonalization, uncertainty ensembles, do-intervention
    evaluation, falsification/placebo diagnostics, and external simulator
    adversarial suites are still future hardening items.

## Next Best Changes

1. Improve high-channel scalability:
   - lower-rank channel factorization
   - channel batching for counterfactual diagnostics
   - stronger contribution-share regularization
   - longer or staged training for 10+ channel panels
   - channel-invariant pretraining so a saved brain can transfer to unseen
     channel counts and schemas

2. Expand pretraining:
   - more randomized panels
   - held-out scenario families
   - repeated transfer seeds
   - no target-label fine-tuning
   - rotate control availability, KPI scales, media measurement types, channel
     counts, geos, zero flighting, and missingness patterns

3. Add do-intervention evaluation:
   - compare model-implied `do(media = 0)`, `+25%`, `-25%`, and budget
     reallocation lifts against known synthetic truth
   - keep this as evaluation/training objective research, not ordinary R2

4. Add identification hardening:
   - Double ML / orthogonalized residual scans
   - treatment-assignment diagnostics for demand-driven media
   - fake-date/fake-channel/negative-control falsification diagnostics
   - uncertainty intervals from ensembles/bootstrap/conformal layers

5. Build PySiMMMulator scenario configs and run:
   - transparent baseline
   - pooled NMMM
   - hierarchical NMMM
   - population-scaled NMMM

6. Add neural scenario suite output table:
   - prediction fit
   - contribution recovery
   - ROI / cost-per-KPI recovery
   - curve recovery
   - failure reason flags

7. Harden true pretraining workflow:
   - train on many simulated panels with truth labels
   - save the model brain
   - evaluate transfer on held-out scenario families
   - then test real-data fine-tuning without truth labels
   - repeat transfer tests across seeds so one optimizer run does not dominate

Checkpoint location pattern:

```text
outputs/torch_smoke_test/torch_model_checkpoint.pt
outputs/torch_grid/<run_id>_<scenario>_<seed>_<feature_set>_<sup>/model_checkpoint.pt
outputs/tft_smoke_test/tft_model_checkpoint.pt
```
