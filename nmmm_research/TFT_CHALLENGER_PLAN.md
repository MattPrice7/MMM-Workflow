# Hierarchical TFT Challenger Plan

We can start the TFT challenger now that NMMM checkpoints and training data are
saved.

## Important Distinction

A pure TFT forecast model is not enough for MMM.

MMM needs:

- variable contribution
- coefficient / effect direction
- response curves
- ROI / cost-per-KPI
- marginal ROI / marginal cost-per-KPI
- baseline stealing diagnostics

So the challenger should be a TFT-style sequence model with MMM heads, not a
forecast-only TFT whose attention scores are treated as attribution.

## Proposed Architecture

Inputs:

- numeric media features by variable:
  - support
  - spend
  - impressions
  - clicks
  - GRPs
  - reach
  - frequency
- numeric controls
- time features
- group/modID index
- optional market-size/population scalars

Forbidden by default:

- channel name text
- funnel position labels
- tactic category labels
- analyst confidence labels

Allowed structural role metadata:

- media vs control
- support vs spend vs other numeric support type
- group index
- time index
- missingness masks

Core layers:

1. Media feature encoder per variable.
2. Variable selection / gating over media features.
3. Sequence encoder with attention or TFT-style temporal blocks.
4. Hierarchical group embeddings.
5. MMM contribution head:
   - nonnegative media contribution by variable x time x group
   - optional group x media multipliers
6. Forecast head:
   - baseline + media contribution + controls
7. Curve head:
   - response curve implied by intervention grid, not attention score

Training objectives:

- KPI prediction loss
- synthetic contribution supervision loss when truth exists
- ROI / cost-per-KPI recovery loss when truth exists
- curve recovery loss when truth exists
- contribution smoothness / stability penalty
- baseline stealing penalty
- group multiplier shrinkage

Synthetic labels must be transformation-consistent before they are used. The
training panel now carries audited truth tables for media contribution,
economics, and response curves. ROI-like and cost-per-outcome labels are derived
from the same contribution and spend totals used by the decomposition labels;
they are not separate hand-entered targets.

Evaluation:

- oracle R2 gap
- known-truth recovery score
- contribution recovery
- ROI-like / cost-per-KPI recovery
- response curve recovery
- parameter/lag recovery where applicable
- baseline stealing diagnostics
- pre-training data validation:
  - channel learnability
  - support/spend decoupling
  - persistent cost-efficiency drift
  - target/decomposition leakage flags
  - synthetic label consistency

## Decision Rule

Promote TFT only if it beats the interpretable hierarchical NMMM on known-truth
recovery across held-out simulator scenarios.

Do not promote TFT if it only improves R2.

## Current Implementation

Initial implementation files:

- `nmmm/torch_tft_mmm.py`
- `nmmm/torch_tft_training.py`
- `run_tft_mmm_smoke.py`

The first version is TFT-style rather than a full library-grade TFT. It uses
variable gating, recurrent temporal encoding, causal attention, hierarchical
group/media multipliers, and explicit MMM contribution outputs.

The first standard synthetic smoke passed and writes:

- model checkpoint / brain: `outputs/tft_smoke_test/tft_model_checkpoint.pt`
- training data: `outputs/tft_smoke_test/training_panel.csv`
- truth contribution labels: `outputs/tft_smoke_test/training_truth_media.csv`
- truth economics labels: `outputs/tft_smoke_test/training_truth_economics.csv`
- label consistency audit: `outputs/tft_smoke_test/training_label_audit.csv`
- contribution decomposition: `outputs/tft_smoke_test/tft_long_decomp.csv`
- variable economics: `outputs/tft_smoke_test/tft_variable_economics.csv`
- response curves: `outputs/tft_smoke_test/tft_response_curves.csv`
- training data validation:
  `outputs/tft_smoke_test/training_data_validation_*.csv`

Early smoke read before baseline isolation:

```text
known-truth recovery score:        98.0 / 100
median contribution correlation:   0.969
median response-curve correlation: 0.999
holdout R2:                        0.978
```

Hardened architecture changes:

- baseline path is media-blind
- contribution heads are channel-isolated
- missing media features pass separate masks
- reported contribution is penalized against full-vs-channel-zeroed
  counterfactual contribution
- baseline-stealing diagnostics are written for every run
- response curves get monotonicity and marginal smoothness penalties

Hardened smoke read:

```text
known-truth recovery score:        86.6 / 100
median contribution correlation:   0.868
median response-curve correlation: 0.993
baseline zeroing delta:            0.000
counterfactual gap RMSE:           ~0.00004
```

First quick grid read before baseline isolation:

```text
supervised TFT known-truth score range:   97.6 to 98.7
unsupervised TFT known-truth score range: 84.5 to 87.5
```

This confirms the architecture can recover MMM objects when the objective has
decomposition signal. It also confirms the risk: KPI fit alone is not enough.
Next step is synthetic pretraining and hostile-grid testing to see whether the
decomposition signal transfers when real contribution labels are unavailable.

Hardened quick grid read:

```text
best supervised score:        94.9
best unsupervised score:      86.5
worst unsupervised score:     80.7
```

This is a more conservative but cleaner baseline. The earlier higher results
should not be treated as the bar because the architecture was less defensible.

Hardened full grid read:

```text
runs:                              96
mean known-truth recovery:          90.3
supervised 0.50 mean recovery:      93.9
unsupervised mean recovery:         84.2
mean contribution correlation:      0.889
mean baseline correlation:          0.935
max baseline stealing ratio:        0.000
```

This is the best evidence so far that the custom TFT-style architecture is
learning MMM objects rather than just forecasting. The score is not perfect,
but it is stable across hostile collinearity and weak-geo settings.

First transfer test:

```text
target unsupervised from scratch:      85.0
target pretrained, no target labels:   95.1
direct supervised target reference:    84.7
```

That result supports continuing the TFT path. It is not a production claim yet:
the direct supervised reference had a weak optimizer run, and the transfer
result needs repeated seeds and more pretraining scenarios.

Hardened multi-panel transfer test:

```text
target unsupervised from scratch:       79.8
target pretrained, no target labels:    96.4
direct supervised target reference:     95.0
```

This is now the most important result in the TFT lane. The model was pretrained
on several randomized generic-channel panels and then fine-tuned on a hard
target panel without target contribution labels. That is closer to the real
workflow than direct supervised target training.

Data-level suite read:

```text
one-geo national media:      88.5 unsupervised, 91.6 supervised
missing-media panel:         85.3 unsupervised, 88.1 supervised
hostile collinear panel:     82.1 unsupervised, 85.3 supervised
weak geo/no controls:        87.7 unsupervised, 94.4 supervised
zero flighting:              88.8 unsupervised, 91.4 supervised
20 geo / 12 channel panel:   79.4 unsupervised, 80.3 supervised
```

The large high-channel case is the current weakness. It runs, saves checkpoints,
and preserves counterfactual consistency, but attribution recovery is not yet
good enough there.

Mixed-curve quick data-level read:

```text
weibull missing-media supervised score:      91.8
weibull missing-media unsupervised score:    85.9
hill hostile-collinear supervised score:     83.3
all label audits:                            passed
```

This adds a useful guard against Hill-only over-specialization. The TFT is still
not told the curve family as metadata; it has to recover response behavior from
the media/KPI panel.

Latest label-audited smoke after expanding simulator realism:

```text
known-truth recovery score:        97.9 / 100
median contribution correlation:   0.980
median ROI-like absolute error:    0.026
median response-curve correlation: 0.995
baseline zeroing delta:            0.000
counterfactual gap RMSE:           ~0.00005
```

This run used audited labels where contribution, ROI/cost, response curves,
baseline, KPI, and noise reconcile back to the same synthetic data-generating
process. That is now a gate for trusting NMMM training data.

The current smoke also writes pre-training causal-readiness diagnostics. Latest
smoke read:

```text
mean learnability score:        88.0 / 100
high leakage-risk inputs:       0
cost-efficiency drift flags:    0
spend/support decoupling flags: 0
label consistency checks:       passed
```

These diagnostics do not prove causality. They decide whether contribution,
ROI-like, mROI-like, and response-curve outputs can be interpreted as causal
evidence, directional evidence, weak signal, or forecast-only.

Current transfer limitation: checkpoints are reusable when the target shape is
compatible with the pretrained shape. Training from scratch supports arbitrary
channel counts, but transferring a saved brain to a different number of
channels or media features needs a more channel-invariant architecture.

Future causal hardening from the current roadmap:

- Double ML / orthogonalized residual scans for media effects after controls.
- Treatment-assignment diagnostics to flag demand-driven media and promo-driven
  spend.
- Explicit do-intervention evaluation on `media = 0`, `+25%`, `-25%`, and budget
  reallocations against known synthetic truth.
- Falsification tests with fake dates, fake channels, shuffled media, and
  negative controls as diagnostics only.
- Uncertainty from ensembles/bootstrap/conformal intervals.
- External PySiMMMulator adversarial scenarios so the TFT does not only win on
  its native simulator.

The TFT should only be promoted if it beats the interpretable NMMM on
known-truth recovery, not merely on holdout R2.
