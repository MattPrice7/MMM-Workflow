# Analyst Thoughts Scratchpad

This file is intentionally separate from `MMM_WORKFLOW_CHECKLIST.md`.
Use it for rough notes, examples, open questions, and train-of-thought ideas before deciding whether anything belongs in the formal roadmap.

## Optimizer / Scenario Planner Notes

- The optimizer is conceptually similar to Excel Solver / What-If analysis:
  - What-If/scenario planning: evaluate plans at specified channel multipliers or spend levels.
  - Solver-style optimization: choose spend/support levels subject to budget, min/max, locked-channel, fixed-spend, ROI, cost-per-KPI, or KPI target constraints.
- Current implementation:
  - `optimizer_method = "greedy"`: marginal-response allocation in small chunks.
  - `optimizer_method = "grid"`: less-greedy exhaustive grid search over multiplier combinations for smaller channel sets or coarse grids.
  - Posterior-draw scenario outputs report uncertainty bands for contribution, incremental contribution, ROI, incremental ROI, mROI, cost per KPI, value per cost, profit, and probability-positive metrics.
  - Conservative/risk thresholds are configurable through a custom uncertainty quantile, so q05-style reads can become q10, q35, or another analyst-selected level.
  - Profit metrics require `value_per_kpi`; otherwise the planner still reports contribution, incremental contribution, and KPI-efficiency uncertainty.

## Group / Hierarchy Notes

Current Stan status:

- `group_col` is generic and can be a geo, product, retailer, line of business, or a composite modeling cell such as `dma_chips`, `dma_pretzels`, `dma_popcorn`, or `LOB_product_dma`.
- Current Stan hierarchy has one group/model-id dimension.
- Coefficients pool across groups for the same variable when coefficient hierarchy is enabled.
- The current model does not yet have crossed random effects such as separate product-level pooling plus geo-level pooling plus retailer-level pooling.

Snack-company example:

- If modeling pretzels only:
  - `group_col` could be `dma` or `pretzel_dma`.
  - Pooling across DMAs within pretzels is natural and currently supported.
- If modeling chips, pretzels, and popcorn together:
  - `group_col` could be composite cells like `chips_dma`, `pretzels_dma`, `popcorn_dma`.
  - If the variable is shared as `tv`, current hierarchical coefficient pooling can borrow strength across all product-DMA groups for `tv`.
  - If products should not pool together by default, use product-specific variables such as `chips_tv`, `pretzels_tv`, and `popcorn_tv`, or add future hierarchy controls.

Open design question:

- Add optional hierarchy structures later:
  - product-specific pooling only within product,
  - global pooling across products for the same channel,
  - geo-level pooling within product,
  - crossed product and geo effects,
  - asymmetric halo structures where source-product effects are not exchangeable.
- Halo caution:
  - A chips-to-pretzels halo may be structurally different from a pretzels-to-chips halo.
  - Halo variables probably need source and target metadata and should not automatically share the same pooling structure as own-product media.

## Questions To Revisit

- Should cross-product pooling be opt-in only?
- Should hierarchy families be declared in metadata, e.g. `pooling_family = "pretzels"` or `pooling_family = "snacks"`?
- Should halo variables default to no cross-source pooling unless explicitly allowed?
- Should optimizer support product-level constraints, geo-level constraints, and total-business constraints separately?
- should we plot out posterior plots, the 2d distribution of each variable. should this be an output of stan or in the chart builder diagnostics or both

## chart builder
- version that can be sent / shared with the client. attatch optimizer / scenario planner to back end
- bubble chart where y = roi/cpkpi/etc, x = spend, bubble size = contribution
- same idea for fair share index
- more slicers
- I couldnt get custom colors to work
- curves look a bit weird, why are all the points at one, shouldnt the points be where the current spend is on the curve, shouldnt x axis be % of current spend
- roi and mroi curves


## Note from ai
1. Add future flighting/cost assumptions to the optimizer.
2. Add curve confidence labels into optimizer outputs.
3. Add formal calibration_evidence table.
4. Add prospective geo-test design / market selection.
5. Add multi-cell geo test support.
6. Add reach/frequency support later.

Priority fixes for NMMM TFT model:

1. Split the model into a media-blind baseline path and a media contribution path.
   - Baseline may use time, geo, controls, seasonality, holidays, trend, and market structure.
   - Baseline should not directly see media inputs.

2. Add counterfactual decomposition consistency loss.
   - For each channel j:
     reported_contribution_j should approximate y_hat(full_media) - y_hat(media_with_channel_j_zeroed)
   - Penalize large gaps.

3. Add baseline stealing diagnostics.
   - Measure how much baseline changes when each channel is zeroed.
   - Flag if baseline_delta is large relative to reported channel contribution.

4. Add channel-isolated contribution heads.
   - Each channel contribution should depend primarily on its own media signal, plus allowed context.
   - Avoid letting one channel’s contribution be freely determined by all other channel inputs.

5. Add missingness masks for media features.
   - Do not treat missing media as true zero.
   - Include separate missing indicators by channel x feature.

6. Add monotonicity/smoothness constraints on response curves.
   - Penalize response curves that decrease as spend/support increases.
   - Penalize unstable marginal ROI shapes.

7. Run full hostile TFT grid.
   - standard
   - messy_realistic
   - hostile_collinear
   - weak_geo
   - support_only
   - support_spend
   - rich_media
   - supervised vs unsupervised
   - pretrained unsupervised transfer
   - multiple seeds

8. Make pretraining multi-panel.
   - Train on many simulated panels, not one standard panel.
   - Test transfer on held-out scenario families with no target contribution labels.

9. Keep TFT as challenger until it beats the interpretable NMMM across held-out simulations.
   - Promote only on known-truth recovery, not R2.

   Training data fixes for NMMM/TFT:

1. Randomize true channel parameters across panels.
   - coef
   - decay
   - shape
   - saturation anchor
   - contribution share
   - spend efficiency
   - channel rank/order

2. Randomly permute channel ordering/names during synthetic pretraining.
   - Prevent the model from memorizing "tv = high effect" or "display = low effect."

3. Fix or remove frequency.
   - Current frequency is constant at 1.0.
   - Generate realistic frequency variation or drop the feature.

4. Make rich media features less mechanically tied to support.
   - Add CPM volatility.
   - Add CTR volatility.
   - Add tracking noise.
   - Add platform-specific measurement gaps.
   - Allow spend to rise while impressions fall and vice versa.

5. Add true missingness masks.
   - Do not fill missing media with zero without telling the model it was missing.
   - Add channel-feature missing indicators.
   - Test support missing, spend missing, all media missing, and block missingness.

6. Add zero-inflated media flighting.
   - Campaigns should turn on/off.
   - Some channels should have true zero weeks.
   - Add bursts, long flights, short flights, and blackout periods.

7. Add harder baseline shocks.
   - Unobserved promo.
   - Competitor shocks.
   - pricing shocks.
   - structural breaks.
   - trend changes. sometimes have a category trend var in the data, sometimes not? or let the model decide trend, cycle, seasonality etc. maybe sometimes have these included in data given to the model and sometimes not? Holidays too
   - holiday changes by year.

8. Add more panel diversity.
   - variable weeks: 52, 104, 156, 208
   - variable geos: 1, 4, 8, 20+
   - variable channels: 2 to 15. sometimes variables broken out into sub channels ie pdsoc_meta, pdsoc_tiktok, pdsrch_branded, pdsrch_nonbranded
   - multiple products/brands
   - different KPI scales

9. Add harsher national-media scenarios.
   - national media repeated across geos
   - no geo-level media variation
   - population-scaled KPI
   - weak geo signal

10. Add rolling validation splits.
   - Do not rely only on the final 13-week holdout.
   - Test multiple time windows.

11. Keep truth labels for synthetic pretraining.
   - true contribution
   - true baseline
   - true response curves
   - true ROI / cost-per-KPI
   - true mROI
   - true channel parameters

   just really think about all the data we need to rotate in and out. cometitive, different macros, different base, different controls, different media, etc.