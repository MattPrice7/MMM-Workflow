# Analyst Thoughts Scratchpad

Use this as a raw inbox for quick notes, half-formed ideas, examples, and questions.

Workflow:
1. Add rough notes below `## Inbox`.
2. Codex will periodically summarize the notes in `analyst_note_reviews/`.
3. Ideas worth tracking will be promoted into `MMM_WORKFLOW_CHECKLIST.md` or `SCRIPT_ROADMAP.md`.
4. Project management and design notes will be put into `docs/PROJECT_CONTRIBUTION_LOG.md`.
5. After acknowledgement / implementation review, Codex can archive the raw notes and clear this scratchpad back to this template.

Latest archive:
- Raw notes: `analyst_note_archives/2026-06-10_scratchpad_raw.md`
- Cleaned review: `analyst_note_reviews/2026-06-10_scratchpad_review.md`

## Inbox

I think just remove the basic time trend context variable. its just a foot gun and doesnt fully represent the real idea.

Residual AR(1)
Models autocorrelated errors.
Effectiveness AR(1)
Models latent time-varying media effectiveness.

tighter priors on context default


stan doesnt currently use the hierarchy key
## Keyed Pooling: Current Implementation vs Intended Implementation

### Current Implementation

The package currently has the metadata scaffolding for keyed pooling, but the Stan backend does not yet estimate keyed pooling families.

At the metadata/input layer, the model can accept fields such as:

```r
coef_hierarchy_scope = "keyed"
hierarchy_key
hierarchy_part_indices
model_id_parts
```

The intended idea is already represented: a `mod_id` or group identifier can be split into parts, such as:

```text
region_retailer_product
```

with part indices like:

```text
0 = region
1 = retailer
2 = product
```

So a hierarchy key like `2` should mean “pool groups that share the same product,” and a key like `0,2` should mean “pool groups that share the same region-product combination.”

However, in the current implementation, `coef_hierarchy_scope = "keyed"` is treated as metadata-only. The current Stan backend supports the standard/global group-level coefficient hierarchy, but it does not yet construct separate pooling families based on `hierarchy_key` or `hierarchy_part_indices`.

As a result, keyed variables are currently blocked from active coefficient hierarchy estimation, with a blocker reason similar to:

```text
keyed_hierarchy_metadata_only_current_stan_global_hierarchy
```

So the current behavior is:

```text
auto/global → can use the current group-level coefficient hierarchy
none        → disables coefficient hierarchy
keyed       → preserves intended pooling metadata, but does not estimate keyed pooling yet
```

In short: keyed pooling is designed at the metadata layer, but not yet implemented in the Stan estimation layer.

---

## Intended Keyed Pooling Implementation

The intended implementation should use the group/model ID parts to create pooling families.

For example, suppose the group ID is:

```text
region_retailer_product
```

with groups such as:

```text
east_walmart_chips
east_target_chips
west_walmart_chips
west_target_chips
east_walmart_pretzels
west_target_pretzels
```

### Example 1: hierarchy key = `2`

This means pool by product only.

Pooling families would be:

```text
chips:
  east_walmart_chips
  east_target_chips
  west_walmart_chips
  west_target_chips

pretzels:
  east_walmart_pretzels
  west_target_pretzels
```

Model concept:

```text
beta[group, channel] ~ Normal(beta_key[product, channel], tau[channel])
```

### Example 2: hierarchy key = `0,2`

This means pool by region-product.

Pooling families would be:

```text
east_chips:
  east_walmart_chips
  east_target_chips

west_chips:
  west_walmart_chips
  west_target_chips

east_pretzels:
  east_walmart_pretzels

west_pretzels:
  west_target_pretzels
```

Model concept:

```text
beta[group, channel] ~ Normal(beta_key[region_product, channel], tau[channel])
```

The required Stan data structure would likely include:

```r
K_hierarchy_keys
group_to_hierarchy_key[G]
```

where each group maps to one keyed pooling family.

For each coefficient/channel, the model would estimate a key-level coefficient and then partially pool group coefficients toward that key-level mean:

```text
beta_key[key, channel] ~ prior
beta_group[group, channel] ~ Normal(beta_key[group_key[group], channel], tau[channel])
```

This would allow the hierarchy to be based on shared parts of the group identifier rather than pooling every group globally.

The first implementation should probably support one active keyed hierarchy per model run, such as:

```r
hierarchy_part_indices = c(2)
```

or:

```r
hierarchy_part_indices = c(0, 2)
```

This keeps the model identifiable and relatively simple.

---

## Separate Extension: Multi-Hierarchy Version

The multi-hierarchy version is more advanced than keyed pooling.

Keyed pooling chooses one hierarchy key and pools groups within that selected key. For example:

```text
group → region_product family
```

A true multi-hierarchy model would estimate multiple nested pooling levels at the same time.

Example:

```text
global
  → product
    → region_product
      → region_retailer_product group
```

Model concept:

```text
beta_global[channel]

beta_product[product, channel]
  ~ Normal(beta_global[channel], tau_product[channel])

beta_region_product[region_product, channel]
  ~ Normal(beta_product[product_of_region_product], tau_region_product[channel])

beta_group[group, channel]
  ~ Normal(beta_region_product[region_product_of_group], tau_group[channel])
```

Using the same example, the model would learn:

```text
overall media effect
  → product-level effect for chips
    → east-chips effect
      → east-walmart-chips group effect
```

This is more powerful because it lets information flow through several levels rather than through only one selected key.

However, it is also harder to identify. It adds more latent parameters, more variance components, and more opportunities for weakly identified shrinkage. It should come after the single-keyed pooling implementation is stable.

Recommended development order:

```text
1. Current global hierarchy
2. Single-level keyed pooling
3. Optional multi-hierarchy pooling
```

The near-term priority should be implementing single-level keyed pooling in Stan. The multi-hierarchy version should be treated as a later extension for larger datasets with enough groups per level to support the additional hierarchy.
