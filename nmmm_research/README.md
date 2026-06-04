# NMMM Research Lane

This folder is intentionally separate from the R MMM production scripts.

Goal: test whether neural or ML-enhanced MMM can recover incrementality, response curves, ROI-like metrics, and decomposition as well as or better than interpretable MMM baselines when ground truth is known.

## Current Contents

- `nmmm/simulator.py`: synthetic MMM panel generator with known media
  contributions, adstock, saturation, controls, geo effects, economics labels,
  and label-consistency audits.
- `nmmm/transforms.py`: canonical adstock and saturation transforms shared by simulator and baselines.
- `nmmm/baselines.py`: transform-based ridge MMM benchmark using only NumPy/Pandas.
- `nmmm/evaluation.py`: prediction and contribution-recovery metrics.
- `nmmm/training_data_validation.py`: training-data readiness checks for label
  consistency, channel learnability, spend/support decoupling, cost-efficiency
  drift, and leakage-risk columns.
- `nmmm/torch_models.py`: Torch-ready interpretable neural MMM scaffold. It imports Torch only when used.
- `nmmm/torch_training.py`: optional Torch training loop and output extraction.
- `nmmm/torch_tft_mmm.py`: TFT-style MMM model with contribution heads.
- `nmmm/torch_tft_training.py`: TFT training, checkpointing, decomposition, response curves, and economics extraction.
- `nmmm/external_simulators.py`: optional PySiMMMulator adapter.
- `run_smoke_test.py`: runnable known-truth smoke test.
- `run_torch_nmmm_smoke.py`: optional neural smoke test after Torch is installed.
- `SIMULATOR_STRATEGY.md`: simulator plan and why external + local generators both matter.
- `MODELING_PRINCIPLES.md`: metadata-blind default and known-truth recovery standard.
- `NMMM_GRID_ANALYSIS.md`: latest full-grid readout and next-test recommendation.
- `TFT_CHALLENGER_PLAN.md`: design for a TFT-style MMM challenger with explicit contribution/curve heads.

## Why This Starts With Baselines

The first standard is not forecasting accuracy. The first standard is known-truth recovery:

- Does the model recover channel contribution?
- Does it recover relative ROI / cost-per-KPI?
- Does it find plausible response curves?
- Does it avoid baseline stealing?
- Does it beat a transparent transformed-MMM benchmark?

This follows the same practical discipline as mature MMM workflows: response transforms, decomposition, contribution, saturation, and business metrics must remain auditable before model flexibility is trusted.

The current neural lane has two challengers:

- an interpretable neural MMM with MMM-native transforms and hierarchical group
  x media multipliers
- a hardened TFT-style MMM with a media-blind baseline path, channel-isolated
  media contribution heads, counterfactual decomposition diagnostics, and
  explicit contribution/ROI/curve outputs

R2 is reported only as a diagnostic; the primary research score is known-truth
recovery across contribution, ROI-like metrics, response curves, baseline
recovery, and parameters where applicable.

Synthetic contribution labels can be used during simulation training. Real
client data will usually not have those labels, so the research path is:

1. Train/pretrain on many simulated panels where contribution truth exists.
2. Validate on held-out simulator scenarios.
3. Fine-tune or apply to real data without using unavailable contribution labels.
4. Trust the neural lane only if it beats transparent MMM baselines on recovery,
   not just prediction fit.

The simulator now writes an explicit training-data contract:

- `training_truth_media.csv`: row-level true media contribution by group,
  period, and channel.
- `training_truth_economics.csv`: channel-level true support, spend,
  contribution, ROI-like, cost-per-incremental-outcome, mROI-like, and
  marginal-cost labels.
- `training_label_audit.csv`: automated checks that contribution, baseline,
  KPI/noise, economics, and response-curve labels all come from the same
  transform math.

Those labels are intentionally redundant. If ROI-like, contribution, and curve
labels ever disagree with each other, the audit should fail before a model is
trained on that panel.

The TFT runners also write a broader training-data validation pack:

- `training_data_validation_summary.csv`
- `training_data_validation_channel_learnability.csv`
- `training_data_validation_cost_efficiency_drift.csv`
- `training_data_validation_spend_support_decoupling.csv`
- `training_data_validation_leakage_flags.csv`
- `training_data_validation_label_consistency.csv`

These are pre-training diagnostics. They are meant to catch weak causal
identification, always-on media, high channel collinearity, support/spend
decoupling, structural CPM/CPC drift, target leakage, and broken synthetic truth
labels before interpreting contribution or ROI outputs.

## Run Smoke Test

Use the bundled Codex Python runtime:

```bash
/Users/mattprice/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 /Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/run_smoke_test.py
```

Outputs are written to:

```text
/Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/outputs/smoke_test/
```

Run the tougher local scenario suite:

```bash
/Users/mattprice/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 /Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/run_scenario_suite.py
```

## Optional Neural Dependencies

The smoke test currently needs only:

- numpy
- pandas

For neural modeling later:

```bash
pip install torch
```

For stronger external MMM simulation:

```bash
pip install pysimmmulator
```

Optional future packages:

```bash
pip install scikit-learn matplotlib pyarrow scipy
```

After Torch is installed:

```bash
/Users/mattprice/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 /Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/run_torch_nmmm_smoke.py
```

Compare hierarchy and market-size choices:

```bash
/Users/mattprice/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 /Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/run_torch_nmmm_ablation.py
```

Run a broader NMMM grid:

```bash
/Users/mattprice/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 /Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/run_torch_nmmm_grid.py --mode quick
```

Use `--mode full` for the longer scenario/seed/objective sweep.

Run the TFT-style MMM challenger smoke test:

```bash
/Users/mattprice/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 /Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/run_tft_mmm_smoke.py
```

Run a TFT challenger grid:

```bash
/Users/mattprice/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 /Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/run_tft_mmm_grid.py --mode quick
```

Use `--mode full` for the longer hostile scenario sweep.

Grid outputs are written to:

```text
/Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/outputs/tft_grid/
```

Run the data-level TFT suite:

```bash
/Users/mattprice/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 /Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/run_tft_mmm_data_level_suite.py --mode quick
```

Use `--mode full` for one-geo, missing-media, hostile-collinear, weak-geo,
zero-flighting, and large high-channel cases.

Run a first TFT pretraining-transfer test:

```bash
/Users/mattprice/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 /Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/run_tft_mmm_pretrain_transfer.py
```

That test saves every synthetic pretraining panel, truth labels, and an
intermediate `model_checkpoint.pt` after each panel. The brain is updated
sequentially across panels, then saved again as
`pretrained_tft_model_checkpoint.pt` before target fine-tuning without target
contribution labels.

Current TFT smoke outputs are written to:

```text
/Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/outputs/tft_smoke_test/
```

The checkpoint is:

```text
/Users/mattprice/Documents/Codex/MMM_Workflow/nmmm_research/outputs/tft_smoke_test/tft_model_checkpoint.pt
```

The TFT smoke writes MMM-native outputs: long decomposition with support/spend,
variable economics with ROI-like and mROI-like reads, learned parameter proxies,
response curves, prediction diagnostics, known-truth recovery scorecards, and
the training panel/truth labels used for the run. The hardened TFT also writes
model diagnostics for baseline stealing and counterfactual decomposition
consistency. The saved `training_label_audit.csv` should pass before treating a
run as valid evidence.

Current modeling default:

- role-aware, semantics-blind inputs
- mixed synthetic truth panels across Hill, Weibull, Gompertz, concave,
  threshold, linear-plateau, and near-linear response families
- learned response-grid outputs rather than named curve-family labels or fixed
  Hill/Weibull parameters
- shrunken group x media multipliers
- no automatic population scaling unless explicitly requested and validated
- saved TFT brains are reusable for compatible model shapes; transfer across
  different channel counts/media schemas is a future architecture hardening item

The neural curve-prior lane is intentionally different from the Stan MMM curve
parameterization. Synthetic named curves are training truth generators and audit
labels only. The neural prior model predicts a monotone response grid, adstock
prior, saturation score, confidence, fallback weight, and uncertainty width.

## Status

This is a research/prototype lane, not production. The R core scripts remain the main analyst-facing workflow until NMMM proves itself against known-truth tests.
