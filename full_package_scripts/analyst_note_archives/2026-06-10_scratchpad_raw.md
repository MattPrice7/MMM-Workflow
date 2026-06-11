# Analyst Thoughts Scratchpad

Use this as a raw inbox for quick notes, half-formed ideas, examples, and questions.

Workflow:
1. Add rough notes below `## Inbox`.
2. Codex will periodically summarize the notes in `analyst_note_reviews/`.
3. Ideas worth tracking will be promoted into `MMM_WORKFLOW_CHECKLIST.md` or `SCRIPT_ROADMAP.md`.
4. Project management and design notes will be put into 'PROJECT_CONTRIBUTION_LOG.md'
5. After acknowledgement, this scratchpad can be cleared back to this template.
  - after acknowledgment / implentation review the original idea / request to ensure it was completed entirely before clearing scratch pad. put a check next to each line item as its completed or put in its designated area

## Inbox

## more urgent
- I see rrate is not estimated in the model, this needs to be in the model
- should bau  curve creator univariately estimate rrate? do this in a safe way so it doesnt just hit bounds because the higher the rrate the higher it explains. and then median anchor should be exclude where raw weeks = 0, not adstocked.
- need you to think about this idea. if saturation is based off of non zero median weeks, shouldnt the mean indexing be based on non zero values across the period? maybe both active period vs whole period should be options? which is correct? what is the implication of both. the reason I think its bad to do whole period is because when there is flighted media that take into account the 0 weeks that we are trying to exclude from the median spend. or is mean indexing wrong entirely. but the more I think about it the more I think it should be based on nonzero meanindexed spend
  - mean(carry[train_mask & x_raw > 0], na.rm = TRUE). confirm and let me know your thoughts on this before applying. we also need to make sure that this is consistent across scripts

- A global variance type input so that it can be consistent across everything and the model knows if it is looking at sd or precision defaulting to sd. every col name should be generalized so we dont need precision and sd cols. basically if varience typpe = precision then everywhere a value would be standard deviation is precision. I think that could just do a pretty simple conversion.
  - I dont know if we should apply this idea now or wait till we do broad refactoring / clean up into formal package structure. Follow up on when you think we should begin this process


- would it be helpful for quasi geo to have the same roll up column that we added to stan? maybe it would only be applied afterwards, but still it could show, okay this is your total social, or at the very least this is your total media right? or should it check the roll ups to try to identify the pooled causal effect by itself? so for example if the script cant find branded and non branded, maybe it can find total paid search. I guess its already looking for combinations of variables so it would do that anyway?

- can we add another script that just builds all the charts that I want in excel. make it entirely separate from the rshiny chart builder. Ive seen some very good excel dashboards

- take a look at what I wrote for source entity on this page. I dont entirely understand what source entity, but I think something like the feature i'm talking about on that note should be implemented


## Optional context-varying effectiveness multiplier

Do not directly replace beta with a free beta_t.

Base model:
effect[k,t] = beta[k] * response_curve(x[k,t])

Optional context-varying version:



Stan can implement this internally as:
m[k,t] = exp(u[k,t])



Metadata input pattern into the context column:
For time:
(time, gamma_prior, gamma_sd, gamma_change_prior, gamma_change_sd, rho_prior, rho_sd, sign)
  - sign(optional):
    - +: context can only have positive effect = positive relationship between context and multiplier
    - -: context can only have negative effect = inverse relationship between context and multiplier
    - +-: default, context can have any effect 


For static context effects, shorter form is allowed:
(context, gamma_prior, gamma_sd, sign)

Examples:
variable, context
pd_soc, (time, 0, 0.10, 0, .02, 0.90, .03, +-)
pd_soc, (tv, 0, 0.03, +)
pd_soc, (fourior, 0, 0.10)

I need you to think about how it should be handled if we want multiple context multipliers for a variable, because I don't want the number of meta data cols blowing up. I think use one context metadata column and allow multiple context tuples inside the same cell, but I could see this being messy so let me know if you have a better idea.

Interpretation:
pd_soc = target media variable
time/tv/ucm_seasonality/fourier = context driver
gamma_prior = expected multiplier, usually 0
gamma_sd = allowed movement around multiplier prior
gamma_change_prior = expected period-to-period change, usually 0
gamma_change_sd = tightness on period-to-period movement
rho_prior = expected persistence of context effect
rho_sd = tightness around rho_prior

For time context:
u[k,t] = rho[k] * u[k,t-1] + epsilon[k,t]
epsilon[k,t] ~ Normal(change_prior, change_sd[k])
m[k,t] = exp(u[k,t])

For non-time context:
m[k,t] = exp(gamma_context[k] * context_value[t])

Use this only as an optional advanced feature. Default should be no context multiplier.

- rule for meta data inputs / modeled varaibles:
  * A column becomes a modeled variable only if it has its own metadata row.
  * A column becomes a context-only variable only if it is referenced inside another variable’s context cell.
  * A column is ignored if it is neither modeled nor referenced as context.
  * make sure im not missing anything

- let me know your thoughts on NMMM being given channel info. like would it be good for it to learn, this is what tv can look like. or is it better for it to try to learn the patterns of the data with only the knowledge that its media. we already know mmm data is often not enough for the data to find all the patterns needed.


## Some model defaults I need you to review, and check for best practice and make sure nothing is missing

MODEL-LEVEL INPUT DEFAULTS

dependent_type:
  revenue | non_revenue

revenue_per_kpi:
  optional

total_media_contribution_prior_mean:
  0.20

total_media_contribution_prior_sd:
  0.15

total_media_contribution_prior_applies_to:
  paid_media_total

total_media_contribution_split:
  spend_weighted_google_style

channel_prior_mean_i:
  total_media_contribution_prior_mean * spend_i / sum(spend)

channel_prior_sd_i:
  total_media_contribution_prior_sd * spend_i / sqrt(sum(spend^2))


METADATA ROWS

Required:

  variable: the variable being modeled: ex tv_grps, price, csent, f_trade
  effect_type
  
  
Optional:
 
MEDIA DEFAULT

effect_type = media

If dependent_type = revenue:
  use Meridian-style ROI prior

If dependent_type = non_revenue:
  use total_media_contribution_prior
  default mean = 0.20
  default sd = 0.15
  split by spend using Google-style logic

curve_type:
  optional
  default = hill or current package default

anchor_saturation:
  optional
  default = 0.50 at non-zero median modeled media/support, per capita like google if available

rrate:
  optional
  estimated by model
  weak/default prior

dvalue:
  optional
  default = 1
  fixed by default unless estimate_dvalue = TRUE


REACH_FREQUENCY DEFAULT

effect_type = reach_frequency

same as media


TRADE SAME UNIT AS KPI

effect_type = trade

Only special rule:
  contribution cannot exceed trade units unless halo is enabled

Default prior:
  mean = 0.40 * trade units
  sd = 0.30 * trade units
  min = 0
  max = trade units


TRADE DIFFERENT UNIT THAN KPI

effect_type = trade

No special default prior:
  use generic coefficient prior unless user supplies conversion / CPKPI / contribution prior


PROMO

effect_type = promo

Only special rule:
  if same unit as KPI, can use same-unit trade cap logic

Otherwise:
  generic coefficient prior unless user supplies stronger prior

  for trade and promo think about if theres any way we can cap false predictions. im not sure if promo should even be here, because isnt promo typically promo value, not promo units/sales. also think if we need anything for distribution, tdp, acv, etc im not sure if we're missing hard rules there. if theres no real rules we can just go to the default. the analyst can handle adding in bounds




EVERYTHING ELSE

No special default.
Use generic model prior:
  coef = 0
  coef_precision = 1
  coef_bound = free

  ## inputs: any we dont need? any could be improved? any were misssing?

  METADATA INPUT DEFINITIONS

driver: Business-level name for the modeled driver; examples: tv, meta, branded_search.

variable: Actual column in modcut used in the model; examples: tv_grps, meta_imps, tv_spend.

effect_type: Driver type used for defaults/reporting; examples: media, trade, price.

support_col: Exposure/support column tied to the driver; examples: tv_grps, meta_imps, search_clicks.

spend_col: Spend/cost column tied to the driver; examples: tv_spend, meta_spend, search_spend.

source_entity: Optional source/group for hierarchy or halo diagnostics; examples: GLOBAL, brand_a, geo_1. #is this so you can tell which variables are hierarchical and which are not, and which are hierachical within their more granular group - eg chips_tv is only hieracical where group has "chips" in the name ie dma_chips. seems like we can have some function where the group is "," or "_" separated values so that way we can have indefinite granular group levels and the model can figure out what belongs to what group, and we dont have to come up with specific names like dma, product, lob, payment type, etc. the user would probably just have to provide and index number if the group pattern provided is dma_chips, they would say chips is in position 1 (or 2 idk which is better) so the hierarchy can only look across groups where the value in position 1 matches. or maybe the group level is region_retailer_product. If I wanted the model to be hierarchical by product I could have the input be 2. or if I wanted it to be hierarchical across region and product I could do 1,3. so everywhere 1 matches and 3 matches is its own hierarchical model. let me know if this idea makes sense, and if theres a better way to incorporate it. maybe this is a different input we need outside of source entity. but explain what source entity is to me.

coef: Optional coefficient prior mean; examples: 0, 0.05, -0.20. #were currently coming up with defaults listed above

coef_precision: Optional coefficient prior precision where sd = sqrt(1 / precision); examples: 1, 4, 100.

coef_bound: Optional coefficient bound; examples: pos, neg, (0,3).

coef_hierarchy_scale: Optional strength of group-level coefficient pooling; examples: 0, 1, 2. #dont really understand this one

curve_type: Saturation curve family; examples: hill, weibull, none(blank, na, 0, etc) are there other curve typoes we should support?

anchor_saturation: Optional saturation level at median active support; examples: 0.50, 0.35, 0.70. #when this is active other curve parameters should be blank or n/a or 0 I think, because its calculating the curve. I also think that 50% or 48% is the active default when a curve is present but no curve values are provided

rrate: Optional adstock/retention prior center, estimated by model; examples: 0.10, 0.30, 0.60.

rrate_precision: Optional adstock prior precision; examples: 1, 4, 25.

cvalue: Optional internal curve-rate prior center; examples: 0.50, 1.00, 2.00.

curve_rate: Alias for cvalue; examples: 0.50, 1.00, 2.00.

cvalue_precision: Optional curve-rate prior precision; examples: 1, 4, 25.

curve_rate_precision: Alias for cvalue_precision; examples: 1, 4, 25.

dvalue: Optional curve shape/slope prior center; examples: 1.00, 1.50, 2.00.

dvalue_precision: Optional curve shape prior precision; examples: 1, 4, 25.

curve_bound: Optional shared bound applied to curve parameters unless overridden; examples: (0,1), (0.05,8), blank.

rrate_bound: Optional adstock-specific bound; examples: (0,0.9), (0.1,0.7), blank.

cvalue_bound: Optional curve-rate-specific bound; examples: (0.05,8), (0.25,4), blank.

curve_rate_bound: Alias for cvalue_bound; examples: (0.05,8), (0.25,4), blank.

dvalue_bound: Optional shape-specific bound; examples: (0.5,2), (1,3), blank.

rrate_lower: Optional numeric lower bound for rrate; examples: 0, 0.05, 0.20.

rrate_upper: Optional numeric upper bound for rrate; examples: 0.90, 0.70, 0.50.

cvalue_lower: Optional numeric lower bound for cvalue/curve_rate; examples: 0.05, 0.10, 0.25.

cvalue_upper: Optional numeric upper bound for cvalue/curve_rate; examples: 8, 4, 2.

dvalue_lower: Optional numeric lower bound for dvalue; examples: 0.50, 0.75, 1.00.

dvalue_upper: Optional numeric upper bound for dvalue; examples: 2.00, 3.00, 4.00.

has_curve: Optional explicit curve flag; examples: 1, 0. #I think media already defaults to havinf a curve, I guess this is fine though

rollup_path: Optional reporting/planning hierarchy; examples: total_media > video > tv, total_media > social > meta.

channel: Optional reporting alias if different from driver; examples: TV, Meta, Paid Search.

funnel_stage: Optional reporting/classification field; examples: upper, middle, lower.

support_type: Optional support unit label; examples: grps, impressions, clicks.

modeled_x_basis: Optional label for whether variable is support/spend/custom; examples: support, spend, custom. #I dont understand this one, seems like its redundent but lmk.

trade_same_unit: Optional flag for trade cap logic; examples: TRUE, FALSE. #I dont understand this one

halo_enabled: Optional flag allowing same-unit trade contribution above trade units; examples: TRUE, FALSE. #seenms like this should not exist and always be false

include_in_mix_diagnostic: Optional flag for spend mix diagnostics; examples: TRUE, FALSE. #explain this one