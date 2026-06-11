# 2026-06-11 Scratchpad Review

## Decisions

- Plain `time` context remains disabled by default. It can behave like a drift term and absorb attribution, so it should only be used through `allow_time_context = TRUE` for deliberate sensitivity testing.
- Residual AR(1) and latent effectiveness AR(1) are distinct future ideas. Residual AR(1) models autocorrelated errors; effectiveness AR(1) models time-varying media effectiveness. Neither should be added casually before the current context modifier and hierarchy paths are stable.
- Context modifiers should stay tightly regularized by default and should be used for named business hypotheses, such as upper-funnel media making search more effective or macro pressure increasing price sensitivity.
- `source_entity` should remain source/halo metadata, not the hierarchy pooling key.
- `coef_hierarchy_scope` remains per variable because different variables may need different pooling behavior: `none`, `global`, `auto`, or `keyed`.
- The keyed hierarchy family definition should be a one-time model setting. Use `coef_hierarchy_part_indices` to say which part(s) of the composite `group_col` define the pooling family.
- Use R-style one-based indexing for `coef_hierarchy_part_indices`. Example: if `group_col` is `region_retailer_product`, then `coef_hierarchy_part_indices = 3` pools within product.

## Implemented

- Active single-level keyed coefficient pooling is now wired into `hier_mmm.stan`.
- `fit_hier_mmm()` and `prepare_stan_data_hier_mmm()` now accept one-time `coef_hierarchy_part_indices` and `coef_hierarchy_index_base`.
- The keyed path supports arbitrary composite model IDs without hardcoded names like DMA/product/LOB.
- The model supports one keyed hierarchy definition per run. Conflicting metadata definitions fail clearly instead of silently falling back to global pooling.
- Keyed pooling stays coefficient-only. Curve parameters remain shared variable-level parameters across groups.
- Documentation and tests were updated to distinguish active single-level keyed pooling from future true multi-level hierarchy.

## Future / Maybe

- Add true multi-level nested or crossed hierarchy only after single-level keyed pooling is stable on larger panels.
- Consider residual AR(1) only as a residual/noise model, not as media effectiveness.
- Consider smooth latent effectiveness AR(1) only as a heavily regularized, opt-in research feature.
