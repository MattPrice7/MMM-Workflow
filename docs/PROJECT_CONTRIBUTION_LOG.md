# MMM Workflow Project Contribution Log

Purpose: keep a clear record of my role, decisions, and contributions to this
AI-assisted MMM workflow. This is written for future resume, portfolio, and
interview use.

## One-Line Project Summary

Designed and product-managed an open-source, analyst-facing MMM workflow that
combines Bayesian hierarchical MMM, quasi-experimental geo/ramp diagnostics,
prior/evidence handling, optimizer/scenario planning, deck-ready reporting, and
experimental neural MMM research.

## How I Used AI

I used AI as a coding and research accelerator, not as a replacement for my
analytics judgment.

My role was to define the business problem, prioritize the workflow, pressure
test statistical assumptions, identify edge cases, review model behavior, and
decide what belonged in the core analyst workflow versus experimental research.

AI helped convert those requirements into code, tests, documentation, and
prototype implementations. I repeatedly challenged outputs when they were too
heuristic, too toy-like, not aligned with MMM practice, or likely to overclaim
causal evidence.

## My Project Management Role

- Defined the roadmap around practical MMM consultant constraints: limited
  geo lift testing, inconsistent geo-level media, missing population scalars,
  multicollinear channels, and KPI outcomes that may not be revenue.
- Prioritized the core production workflow over lower-value support scripts:
  Stan MMM, quasi-geo evidence, optimizer/scenario planner, and deck outputs.
- Kept the project organized into rolling latest scripts, tests, docs, examples,
  and experimental NMMM research.
- Created a running checklist and scratchpad process to capture ideas without
  letting speculative work distract from the core package.
- Required repeated smoke tests, targeted tests, synthetic known-truth tests, and
  checkpoint summaries instead of relying on documentation claims.
- Reframed weak methods as diagnostic/directional evidence rather than allowing
  them to become overconfident priors.

## Core Features I Drove

### Hierarchical Stan MMM

- Built a joint-estimated Bayesian MMM with shared variable-level response
  curves and hierarchical group-level coefficients.
- Supported flexible group/model IDs so the same script can model national,
  geo, product, product-geo, retailer, line-of-business, or other custom grains.
- Added transform consistency across fitting, decomposition, ROI/mROI,
  optimizer, and marginal-response logic.
- Added spend/support attachment to model outputs so decompositions can flow
  directly into economics and charting.
- Pushed for curve flexibility: Hill and Weibull support, median saturation
  anchors, and channel-specific anchor settings.
- Hardened Stan geometry and sampler options so users can choose safer/faster
  parameterizations when posterior geometry is difficult.

### Quasi-Geo / Observational Lift Evidence

- Reframed ramp detection as quasi-experimental dose-response evidence rather
  than a perfect geo-lift substitute.
- Required signed event detection: up-ramp, down-ramp, turn-on, turn-off, mixed,
  and no-event cases.
- Preserved imperfect evidence instead of discarding it, with scoring,
  downgrade reasons, recommended use, and confidence bands.
- Added bundle/campaign-level handling when multiple channels move together,
  without incorrectly allocating bundle lift back to individual channels.
- Required diagnostics inspired by GeoLift and matched-market/TBR thinking:
  pre-fit quality, donor quality, contamination, placebo/falsification checks,
  MDE/power logic, lookback sensitivity, and donor contamination.
- Added raw-scale ROI/cost-per-KPI logic so mean-indexing does not corrupt
  business interpretation.

### Optimizer / Scenario Planner

- Added an optimizer/scenario-planner concept built around response curves,
  not just coefficients.
- Supported use with fitted MMM outputs or externally supplied response curves.
- Added national and group-level allocation thinking, spend bounds, locked
  channels, minimums/maximums, budget scenarios, and conservative objective
  options.
- Pushed for posterior/uncertainty-aware decision outputs such as conservative
  contribution, ROI, profit probability, and risk-adjusted scenarios.

### Consultant Deck / Reporting Builder

- Added deck-ready tables and charts for actual vs fitted, contribution,
  period-over-period due-to, spend vs contribution, ROI/cost-per-KPI, fair-share
  index, response curves, and quasi-geo diagnostics.
- Separated analyst QA outputs from client-facing outputs.
- Added customization requirements: client colors, chart themes, filters,
  period controls, variable selection, and flexible KPI economics.
- Identified charting gaps early: response-curve visibility, weekly granularity,
  axis formatting, theme controls, and Excel-like customization.

### NMMM Research Lane

- Created a separate Python research lane for neural MMM rather than mixing it
  into the production R workflow prematurely.
- Required the neural model to optimize for known-truth contribution, ROI,
  response curves, and baseline recovery, not just R2.
- Drove a custom TFT-style MMM architecture with:
  - media-blind baseline path
  - channel-isolated contribution path
  - explicit contribution/economics/curve outputs
  - counterfactual decomposition consistency checks
  - baseline stealing diagnostics
  - missingness masks for media features
- Required realistic synthetic training data and label audits before trusting
  model results.
- Added training-data validation for learnability, leakage risk,
  spend/support decoupling, cost-efficiency drift, and synthetic label
  consistency.

## Edge Cases I Made Sure Were Considered

- No geo lift tests available.
- Geo-level KPI but only national media.
- Geo-level media without population/per-capita scalars.
- One-geo/national-only data.
- Multiple products, retailers, store types, line-of-business, or arbitrary
  model IDs.
- Channels with high multicollinearity.
- Channels that are always on or heavily under/over-spent.
- Channels with little historical variation.
- Media turn-ons, turn-offs, down-ramps, and spend cutoffs.
- Multiple channels moving together.
- National-repeated media that is not geo-identifiable.
- Missing support, missing spend, block missingness, and support-only data.
- Spend/support decoupling from CPM/CPC/CPP changes.
- Media quality drift, creative fatigue, auction inflation, and platform
  efficiency changes.
- KPI outcomes that are not revenue, requiring cost-per-KPI rather than ROI.
- Negative or diagnostic contributions in charting/economics.
- Holdout leakage, post-treatment controls, downstream KPI leakage, and
  decomposition target leakage.

## Ways Of Thinking I Brought To The Project

- Treat MMM as a decision-support system, not only a fitting exercise.
- Separate causal evidence, directional evidence, diagnostics, and forecasts.
- Avoid overclaiming observational evidence as randomized causal proof.
- Keep business metrics flexible: ROI when revenue exists, cost-per-KPI when the
  KPI is subscriptions, leads, orders, visits, or another non-dollar outcome.
- Prefer interpretable, auditable production workflows before experimental
  neural models.
- Use neural MMM as a challenger model that must beat transparent baselines on
  known-truth recovery, not just predictive fit.
- Make every major output traceable back to inputs, assumptions, diagnostics,
  and versioned scripts.
- Design for consultant reality: partial data, messy inputs, client deadlines,
  limited lift tests, and changing stakeholder questions.

## Resume Bullet Drafts

- Product-managed and architected an AI-assisted MMM workflow spanning Bayesian
  hierarchical modeling, quasi-experimental geo/ramp diagnostics, optimizer
  scenario planning, and deck-ready reporting.
- Designed analyst-facing guardrails for MMM under real-world data constraints,
  including missing geo media, unavailable lift tests, multicollinear channels,
  national-repeated media, and non-revenue KPI economics.
- Led development of quasi-geo evidence scoring that preserves imperfect
  observational shocks while separating calibration-grade, directional,
  diagnostic, and unusable evidence.
- Directed hardening of a Stan-based hierarchical MMM with shared response
  curves, group-level coefficient pooling, transform-consistent decomposition,
  ROI/mROI outputs, and spend/support reconciliation.
- Built an experimental neural MMM research lane with known-truth simulation,
  TFT-style contribution heads, baseline-stealing diagnostics, and training-data
  validation focused on contribution and ROI recovery instead of R2.
- Established repeatable validation practices, including synthetic known-truth
  tests, smoke tests, leakage checks, label-consistency audits, and edge-case
  scenario suites.

## Interview Talking Points

- I did not ask AI to "make an MMM." I scoped the workflow around the problems
  MMM consultants actually face and used AI to accelerate implementation.
- I repeatedly pushed back on methods that were too heuristic or likely to
  overstate causal confidence.
- I treated industry tools like Meridian, GeoLift, matched markets/TBR, Robyn,
  Stan, and TFT research as reference points, but adapted them to the practical
  constraints of my workflow.
- The core package is designed to stand on its own; support scripts and neural
  research are intentionally separated so experimental work does not weaken the
  production path.
- My main contribution was not typing every line of code. It was defining the
  statistical and business requirements, identifying failure modes, forcing
  validation, prioritizing work, and making the system usable for analysts.

## Current Honest Status

The core R workflow is the production candidate. The NMMM lane is promising but
still research. The package is strongest where it is transparent, auditable, and
diagnostic-heavy: Stan MMM, quasi-geo evidence, optimizer/scenario planning, and
reporting. Neural MMM should remain a challenger until it proves stable across
external simulators, more hostile synthetic regimes, and real-world validation.

