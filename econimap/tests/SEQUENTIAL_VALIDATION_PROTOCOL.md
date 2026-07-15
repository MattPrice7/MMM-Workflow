# Sequential Validation Protocol

Sequential empirical Bayes is not promoted on in-sample fit alone. Results are
classified by the evidence source below.

## 1. Contract and geometry checks

Required on every code change:

- input, prior-transfer, and holdout-leakage unit tests;
- Stan compilation;
- divergences, tree-depth hits, BFMI, R-hat, and effective sample size;
- decomposition and parent-to-child reconciliation checks.

These establish implementation correctness and sampler behavior. They do not
establish causal or parameter recovery.

## 2. Internal synthetic diagnostics

`test_sequential_hierarchical_bayes_stan_validation.R` creates controlled
regimes for regression testing. Its data-generating process uses Econimap
transforms, so it can identify regressions and compare direct versus sequential
paths under identical assumptions. It is never sufficient evidence that the
sequential workflow outperforms other MMM implementations.

Direct and sequential leaf fits must start from identical generic metadata,
training rows, baseline, curve freedom, bounds, and sampling settings. Truth
metadata is retained only for scoring. Any parent-derived transfer is the sole
intended difference.

## 3. Independent known-truth recovery

Primary recovery evidence must come from a simulator whose outcome and media
mechanics were not authored to match Econimap. The native Meta `siMMMulator`
outcome/contribution fields are one such source when preserved as generated.
Do not replace its outcome with an Econimap Hill/Weibull construction and then
call the result external recovery evidence.

Report total contribution error, contribution-share MAE, effectiveness error,
adstock/curve recovery where comparable, interval coverage, posterior width,
holdout error, and sampler diagnostics across predeclared scenario families and
seeds.

## 4. External product and compatibility panels

Google Meridian's published simulated geo panel is useful for schema,
population/exposure, geo-scale, holdout, diagnostic, and output compatibility.
Unless its complete generating truth is independently available, it is not used
for contribution or curve recovery claims. An actual Meridian fit on that panel
is reported as an external product benchmark, not an apples-to-apples prior
comparison.

## Prior-comparison rule

Do not label an Econimap prior conversion as "Meridian-equivalent" unless the
parameterization, KPI type, prior distribution, and relevant scaling match the
documented Meridian specification. Otherwise report it as an Econimap
sensitivity analysis only.
