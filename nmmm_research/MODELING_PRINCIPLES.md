# NMMM Modeling Principles

## Default: Semantics-Blind, Role-Aware Learning

The neural model should not use semantic channel metadata by default, but it
should know each variable's structural modeling role.

Do not feed these fields as model features:

- channel name text, such as TV, Search, Trade, Social
- funnel position
- tactic category
- brand/non-brand labels
- analyst confidence labels
- manually assigned channel archetypes

Reason: these fields can let the model learn analyst stereotypes instead of learning from observed support, spend, geography, timing, controls, and outcomes.

Allowed structural metadata:

- variable role: media, control, target, group index, time index
- which numeric column is support vs spend
- observed vs missing masks
- grouping keys needed to build a panel tensor
- sign/constraint role if explicitly part of the model design

Reason: the model needs to know which variables receive media transformations
such as adstock, saturation, contribution, ROI, and response curves. This is not
the same as telling the model that a variable is TV, search, trade, upper funnel,
or lower funnel.

## What The Model May Use

The model may use:

- support and spend histories
- time structure
- geo/product/group panel structure
- controls such as price, promo, holidays, distribution, macro
- optional market-size scalars when available
- numeric masks showing observed vs missing data

The model still needs variable columns to be separated so it can estimate
variable-specific adstock, saturation, contribution, and response curves. Those
variable names are kept as output labels, not explanatory text features.

Spend and support/impressions are valid numeric business context. The model can
learn how much to use each signal for a variable. This differs from feeding the
model semantic text like "TV" or "trade".

When synthetic contribution labels are available, they may be used as supervised
training targets. In real client data those labels are usually not available, so
this is a simulation pretraining/evaluation tool rather than a direct production
input.

## Hierarchical Default

The neural MMM should be hierarchical by default when the data has groups such
as geos, products, retailers, or flexible modIDs.

Initial production-minded shape:

- shared variable-level adstock and saturation curves
- shared positive variable-level media coefficients
- shrunken group x media coefficient multipliers
- group-specific baseline intercepts
- optional media scaling by market size / population when available

This lets groups differ without giving every geo/product/modID a completely
free curve. Fully group-specific curves should be treated as a later ablation,
because they can overfit quickly unless the panel is rich.

Market-size scaling should be explicit, not automatic. Population can make
media support more comparable across geos when support is raw volume, but it
can distort attribution if support is already comparable exposure. The NMMM
ablation runner should compare pooled, hierarchical, and population-scaled
variants before choosing the default for a dataset.

## Optional Future Ablation

Semantic metadata can be added later only as an explicit ablation:

1. Train metadata-blind model.
2. Train metadata-aware model.
3. Compare known-truth contribution recovery, not just prediction fit.
4. Keep metadata only if it improves recovery without creating obvious leakage or stereotype effects.

## Simulator Standard

NMMM should be judged on realistic hostile simulation suites, including:

- clean known-truth cases
- highly collinear media
- delayed media effects
- national media repeated across geos
- partial geo media
- missing support/spend
- shocks and structural breaks
- weak controls
- short time series
- low variation / always-on channels
- over-spent and under-spent channels

PySiMMMulator should be used as the preferred external simulator once installed, while the local simulator remains available for custom geo/product/group edge cases.
