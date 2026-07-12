test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
candidate_roots <- unique(c(
  if (!is.na(test_file) && file.exists(test_file)) dirname(dirname(test_file)) else character(),
  getwd(), file.path(getwd(), ".."), Sys.getenv("R_PACKAGE_DIR")
))
source_root <- candidate_roots[vapply(candidate_roots, function(path) {
  file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))
}, logical(1))]
root_dir <- if (length(source_root)) source_root[1] else getwd()
if (!requireNamespace("data.table", quietly = TRUE)) stop("Sequential test requires data.table.")
suppressPackageStartupMessages(library(data.table))
if (length(source_root)) {
  invisible(lapply(sort(list.files(file.path(root_dir, "R"), pattern = "[.]R$", full.names = TRUE)), source))
} else {
  library(econimap)
  list2env(as.list(asNamespace("econimap"), all.names = TRUE), envir = .GlobalEnv)
}

set.seed(42)
periods <- seq.Date(as.Date("2023-01-02"), by = "week", length.out = 104L)
groups <- paste0("geo_", 1:4)
truth_effectiveness <- 0.75
synthetic <- rbindlist(lapply(seq_along(groups), function(gg) {
  n <- length(periods)
  tv_spend <- pmax(20, 170 + 35 * sin(seq_len(n) / 5 + gg / 3) + rnorm(n, sd = 28))
  search_spend <- pmax(10, 95 + 24 * cos(seq_len(n) / 7 + gg / 5) + rnorm(n, sd = 21))
  macro <- rnorm(n)
  data.table(
    period = periods,
    geo = groups[gg],
    entity = "brand",
    tv_support = tv_spend * 10,
    search_support = search_spend * 20,
    tv_spend,
    search_spend,
    macro,
    kpi = 1200 + gg * 40 + truth_effectiveness * (tv_spend + search_spend) + 20 * macro + rnorm(n, sd = 20)
  )
}))
metadata <- data.table(
  variable = c("tv_support", "search_support", "macro"),
  role = c("media", "media", "control"),
  spend_col = c("tv_spend", "search_spend", NA_character_),
  rollup_path = c(
    "total_paid_media > video > tv",
    "total_paid_media > search > paid_search",
    "business_controls > macro"
  ),
  coef = 0,
  coef_precision = 1,
  coef_bound = c("pos", "pos", "free")
)
spend_map <- metadata[role == "media", .(variable, spend_col)]

# The current fixture contains no independent seasonal baseline, so every
# compared model deliberately uses zero Fourier terms.
root <- fit_parsimonious_total_media_root(
  data = synthetic,
  metadata_input = metadata,
  dep_var_col = "kpi",
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = spend_map,
  root_control_cols = "macro",
  root_fourier_harmonics = 0L,
  root_media_transform = "linear",
  root_bootstrap_reps = 30L,
  root_block_length = 4L,
  seed = 91L
)
stopifnot(abs(root$root_summary$root_effectiveness[1] - truth_effectiveness) < 0.12)
stopifnot(root$root_summary$root_bootstrap_successful[1] >= 20L)
stopifnot(root$root_summary$root_scope[1] == "national")
stopifnot(nrow(root$root_panel) == length(periods))
stopifnot(all(root$bootstrap_draws$bootstrap_method == "moving_block_residual_original_media_timeline"))
stopifnot(all(root$bootstrap_draws$original_media_timeline_preserved))
stopifnot(all(root$bootstrap_draws$curve_selection_repeated))
stopifnot(all(root$root_panel$root_adstock_order__ == root$root_panel$root_time_index__))
stopifnot(nrow(root$national_spend) == length(periods) * nrow(spend_map))

# A linear root emits broad generic curve defaults and labels them as generic;
# it never mislabels the 50% anchor as parent nonlinear evidence.
linear_curve_evidence <- econ_seq_root_rrate_distribution(root)
stopifnot(linear_curve_evidence$curve_prior_available[1])
stopifnot(!linear_curve_evidence$parent_curve_evidence_available[1])
stopifnot(abs(linear_curve_evidence$rrate_prior_mean[1] - 0.20) < 1e-12)
stopifnot(abs(linear_curve_evidence$anchor_saturation_prior_mean[1] - 0.50) < 1e-12)
stopifnot(linear_curve_evidence$curve_prior_mode[1] == "predominantly_generic_curve_default")
stopifnot(grepl("generic_default", linear_curve_evidence$rrate_prior_source[1], fixed = TRUE))

linear_md <- copy(metadata)
linear_md[role == "media", `:=`(rrate = 0.18, rrate_precision = 9, anchor_saturation = 0.44,
                                 anchor_saturation_precision = 4, cvalue_from_anchor = TRUE)]
linear_handoff <- build_sequential_effectiveness_priors(
  root_fit = root,
  data = root$canonical_data,
  metadata_input = linear_md,
  time_col = "period"
)
stopifnot(all(linear_handoff$business_priors$curve_prior_available))
linear_md_after <- econ_seq_apply_rrate_priors(linear_md, linear_handoff$business_priors)
stopifnot(all(abs(linear_md_after[role == "media", rrate] - 0.20) < 1e-12))
stopifnot(all(abs(linear_md_after[role == "media", anchor_saturation] - 0.50) < 1e-12))

# Nonlinear transfer is conditional on actual model evidence and weighted by
# bootstrap selection frequency rather than the selected model alone.
nonlinear_root <- root
nonlinear_root$bootstrap_draws <- data.table(
  draw = 1:20,
  fit_ok = TRUE,
  root_effectiveness = seq(0.70, 0.80, length.out = 20L),
  root_curve_type = c(rep("adstock_hill", 16L), rep("linear", 4L)),
  root_rrate = c(seq(0.15, 0.35, length.out = 16L), rep(0, 4L)),
  root_anchor_saturation = c(seq(0.40, 0.60, length.out = 16L), rep(NA_real_, 4L))
)
nonlinear_evidence <- econ_seq_root_rrate_distribution(nonlinear_root)
stopifnot(nonlinear_evidence$curve_prior_available[1])
stopifnot(abs(nonlinear_evidence$root_nonlinear_model_weight[1] - 0.8) < 1e-12)
stopifnot(is.finite(nonlinear_evidence$rrate_prior_precision[1]))
stopifnot(is.finite(nonlinear_evidence$anchor_saturation_prior_precision[1]))

# There is no 50% evidence cliff: adjacent bootstrap weights produce adjacent
# prior widths and both remain valid model-averaged curve priors.
moderate_low <- nonlinear_root
moderate_low$bootstrap_draws[, root_curve_type := c(rep("adstock_hill", 9L), rep("linear", 11L))]
moderate_low$bootstrap_draws[root_curve_type == "linear", `:=`(root_rrate = 0, root_anchor_saturation = NA_real_)]
moderate_high <- nonlinear_root
moderate_high$bootstrap_draws[, root_curve_type := c(rep("adstock_hill", 11L), rep("linear", 9L))]
moderate_high$bootstrap_draws[root_curve_type == "linear", `:=`(root_rrate = 0, root_anchor_saturation = NA_real_)]
low_curve <- econ_seq_root_rrate_distribution(moderate_low)
high_curve <- econ_seq_root_rrate_distribution(moderate_high)
stopifnot(low_curve$curve_prior_available[1] && high_curve$curve_prior_available[1])
stopifnot(abs(low_curve$rrate_prior_sd[1] - high_curve$rrate_prior_sd[1]) < 0.08)

# Identification strength continuously changes inherited shrinkage. Weak
# children get tighter parent shrinkage; well-identified children get room to
# approach a direct fit.
child_id <- data.table(
  variable = c("tv_support", "search_support"),
  identification_recommendation = c("fit", "strong_parent_shrinkage"),
  active_row_n = c(400L, 400L),
  identification_strength_0_1 = c(0.90, 0.30),
  parent_shrinkage_multiplier = c(0.75, 2.60)
)
identified_handoff <- build_sequential_effectiveness_priors(
  root_fit = nonlinear_root,
  data = nonlinear_root$canonical_data,
  metadata_input = metadata,
  time_col = "period",
  child_identification = child_id
)
identified <- identified_handoff$business_priors
stopifnot(identified[variable == "tv_support", branch_decision] == "fit")
stopifnot(identified[variable == "search_support", branch_decision] == "strong_parent_shrinkage")
stopifnot(identified[variable == "tv_support", prior_sd] > identified[variable == "search_support", prior_sd])
stopifnot(identified[variable == "tv_support", rrate_prior_precision] < identified[variable == "search_support", rrate_prior_precision])
prior_audit <- econ_seq_sequential_prior_posterior_audit(identified)
stopifnot(all(c(
  "parent_prior_center", "parent_uncertainty", "data_reuse_inflation",
  "child_heterogeneity_allowance", "child_identification_score",
  "final_shrinkage_multiplier", "final_prior_sd", "final_prior_precision",
  "posterior_movement_away_from_prior", "prior_dominance_classification"
) %in% names(prior_audit)))

# Identification diagnostics use modeled support, not spend. Here spend is
# perfectly collinear while support is independently varying.
support_diag_data <- data.table(
  period = rep(1:60, each = 2),
  geo = rep(c("a", "b"), 60),
  shared_spend_a = rep(seq(10, 100, length.out = 60), each = 2),
  shared_spend_b = rep(seq(10, 100, length.out = 60), each = 2)
)
set.seed(812)
support_diag_data[, `:=`(
  support_a = runif(.N, 20, 100),
  support_b = runif(.N, 15, 90)
)]
support_diag_data[, kpi := 200 + 0.5 * support_a + 0.3 * support_b + rnorm(.N, sd = 5)]
support_diag <- econ_seq_layer_identification_diagnostics(
  data = support_diag_data,
  spend_map = data.table(
    variable = c("support_a", "support_b"),
    model_support_col = c("support_a", "support_b"),
    spend_col = c("shared_spend_a", "shared_spend_b"),
    support_hierarchical_variation_eligible = c(TRUE, FALSE)
  ),
  group_col = "geo",
  time_col = "period",
  dep_var_col = "kpi"
)
stopifnot(max(support_diag$by_variable$max_abs_media_correlation, na.rm = TRUE) < 0.30)
stopifnot(all(support_diag$by_variable$model_support_col == c("support_a", "support_b")))
stopifnot(!support_diag$by_variable[variable == "support_b", support_hierarchical_variation_eligible])
stopifnot(all(support_diag$by_variable$spend_total > 0))
collinear_diag_data <- copy(support_diag_data)
collinear_diag_data[, support_b := support_a + rnorm(.N, sd = 0.001)]
collinear_diag <- econ_seq_layer_identification_diagnostics(
  data = collinear_diag_data,
  spend_map = data.table(
    variable = c("support_a", "support_b"),
    model_support_col = c("support_a", "support_b"),
    spend_col = c("shared_spend_a", "shared_spend_b")
  ),
  group_col = "geo",
  time_col = "period",
  dep_var_col = "kpi"
)
stopifnot(all(collinear_diag$by_variable$max_abs_media_correlation > 0.99))
stopifnot(all(collinear_diag$by_variable$parent_shrinkage_multiplier >
                support_diag$by_variable$parent_shrinkage_multiplier))
stopifnot(!any(collinear_diag$by_variable$identification_recommendation %in% c("stop", "prune", "require_prior")))
stopifnot(all(identified$rrate_pooling_mode == "shared_parent_regularization_no_latent_sibling_pool"))

# All-NA overrides are not user priors. A valid mean plus uncertainty is.
negative_root <- root
negative_root$root_summary[, `:=`(
  root_effectiveness = -0.20,
  root_effectiveness_sd = 0.06,
  root_effectiveness_status = "negative_not_transferable"
)]
weak_id <- data.table(
  variable = c("tv_support", "search_support"),
  identification_recommendation = c("strong_parent_shrinkage", "strong_parent_shrinkage"),
  active_row_n = c(100L, 100L),
  identification_strength_0_1 = c(0.30, 0.10),
  parent_shrinkage_multiplier = c(2.5, 4)
)
invalid_override <- data.table(variable = "search_support", prior_mean = NA_real_, prior_sd = NA_real_)
negative_handoff <- build_sequential_effectiveness_priors(
  root_fit = negative_root,
  data = negative_root$canonical_data,
  metadata_input = metadata,
  time_col = "period",
  child_identification = weak_id,
  child_prior_overrides = invalid_override
)
stopifnot(!negative_handoff$business_priors[variable == "search_support", user_prior_override_valid])
stopifnot(all(negative_handoff$business_priors$branch_decision == "fit"))
stopifnot(all(negative_handoff$business_priors$prior_dominance_classification == "default_prior_driven"))
stopifnot(nrow(negative_handoff$reference_calibration_input) == 0L)
valid_override <- data.table(variable = "search_support", prior_mean = 0.35, prior_sd = 0.15)
valid_handoff <- build_sequential_effectiveness_priors(
  root_fit = negative_root,
  data = negative_root$canonical_data,
  metadata_input = metadata,
  time_col = "period",
  child_identification = weak_id,
  child_prior_overrides = valid_override
)
stopifnot(valid_handoff$business_priors[variable == "search_support", branch_decision] == "fit")
stopifnot(valid_handoff$business_priors[variable == "search_support", user_prior_override_valid])
stopifnot(nrow(valid_handoff$reference_calibration_input) == 1L)

# Enforced actions alter the modeled grain. Pruned/unresolved children are not
# left as independent generic-prior variables, while all spend is preserved.
branch_data <- synthetic[, .(period, geo, entity, kpi, tv_support = tv_spend,
                             search_support = search_spend, tv_spend, search_spend)]
branch_prior <- copy(identified)
branch_prior[variable == "search_support", branch_decision := "prune"]
enforced <- econ_seq_enforce_branch_decisions(
  data = branch_data,
  metadata_input = metadata[role == "media"],
  spend_map = spend_map,
  prior_table = branch_prior
)
stopifnot("tv_support" %in% enforced$spend_map$variable)
stopifnot(!"search_support" %in% enforced$spend_map$variable)
stopifnot(any(enforced$action_audit$enforced_action == "parent_remainder"))
stopifnot(enforced$reconciliation$max_abs_row_spend_reconciliation_error[1] < 1e-10)

# Below a fitted parent, a stop retains that parent branch while a strong
# sibling parent proceeds at the deeper grain.
branch_panel <- data.table(
  period = rep(1:20, each = 2),
  geo = rep(c("a", "b"), 20),
  entity = "brand"
)
branch_panel[, `:=`(
  a1 = runif(.N, 1, 5),
  a2 = runif(.N, 1, 5),
  b1 = runif(.N, 1, 5),
  kpi = runif(.N, 100, 120)
)]
branch_panel[, `:=`(parent_a = a1 + a2, parent_b = b1)]
branch_md <- data.table(
  variable = c("a1", "a2", "b1"), role = "media", spend_col = c("a1", "a2", "b1"),
  coef = 0, coef_precision = 1,
  rollup_path = c("total_paid_media > parent_a > a1", "total_paid_media > parent_a > a2", "total_paid_media > parent_b > b1")
)
parent_layer <- list(
  data = branch_panel,
  metadata = data.table(variable = c("parent_a", "parent_b"), role = "media",
                        spend_col = c("parent_a", "parent_b"), coef = 0, coef_precision = 1),
  spend_map = data.table(variable = c("parent_a", "parent_b"), spend_col = c("parent_a", "parent_b"))
)
parent_branch_prior <- data.table(
  variable = c("a1", "a2", "b1"),
  branch_decision = c("stop", "fit", "fit"),
  sequential_parent_id = c("parent_a", "parent_a", "parent_b"),
  child_spend_total = c(sum(branch_panel$a1), sum(branch_panel$a2), sum(branch_panel$b1)),
  prior_mean = 0.4,
  prior_sd = 0.2,
  parent_positive_effect_transferred = TRUE,
  user_prior_override_valid = FALSE,
  implied_child_contribution_mean = 0.4 * c(sum(branch_panel$a1), sum(branch_panel$a2), sum(branch_panel$b1))
)
parent_enforced <- econ_seq_enforce_branch_decisions(
  data = branch_panel,
  metadata_input = branch_md,
  spend_map = branch_md[, .(variable, spend_col)],
  prior_table = parent_branch_prior,
  parent_layer = parent_layer
)
stopifnot(setequal(parent_enforced$spend_map$variable, c("parent_a", "b1")))
stopifnot(parent_enforced$action_audit[variable == "a1", enforced_action] == "parent_retained")
stopifnot(parent_enforced$action_audit[variable == "b1", independent_child_retained])
stopifnot(parent_enforced$reconciliation$max_abs_row_spend_reconciliation_error[1] < 1e-10)

# Spend rollups remain exact, slash-containing labels remain literal, and a
# fitted numeric stage can continue all the way back to original leaves.
rollup_data <- copy(synthetic)
rollup_data[, `:=`(
  meta_spend = pmax(3, 45 + 15 * sin(seq_len(.N) / 6) + rnorm(.N, sd = 8)),
  tiktok_spend = pmax(2, 28 + 12 * cos(seq_len(.N) / 9) + rnorm(.N, sd = 6))
)]
rollup_data[, `:=`(meta_support = meta_spend * 18, tiktok_support = tiktok_spend * 15)]
rollup_data[, kpi := kpi + 0.55 * meta_spend + 0.35 * tiktok_spend]
rollup_metadata <- data.table(
  variable = c("tv_support", "meta_support", "tiktok_support", "macro"),
  role = c("media", "media", "media", "control"),
  spend_col = c("tv_spend", "meta_spend", "tiktok_spend", NA_character_),
  coef = 0,
  coef_precision = 1,
  coef_bound = c("pos", "pos", "pos", "free"),
  rollup_path = c(
    "total_paid_media > CTV/OLV > tv",
    "total_paid_media > social > meta",
    "total_paid_media > social > tiktok",
    "business_controls > macro"
  )
)
rollup_spend <- rollup_metadata[role == "media", .(variable, spend_col)]
slash <- econ_seq_rollup_map(rollup_metadata, "tv_support")
stopifnot(slash$rollup_parent[1] == "CTV/OLV")
layer_one <- build_sequential_rollup_layer(rollup_data, rollup_metadata, rollup_spend, rollup_depth = 1L)
leaf_layer <- econ_seq_build_leaf_layer(rollup_data, rollup_metadata, rollup_spend)
stopifnot(leaf_layer$is_leaf_layer)
stopifnot(setequal(leaf_layer$spend_map$variable, rollup_spend$variable))
raw_total <- rowSums(as.matrix(rollup_data[, rollup_spend$spend_col, with = FALSE]))
layer_total <- rowSums(as.matrix(layer_one$data[, layer_one$spend_map$spend_col, with = FALSE]))
stopifnot(max(abs(raw_total - layer_total)) < 1e-10)

parent_effect <- setNames(c(0.50, 0.62), layer_one$spend_map$variable)
parent_draws <- rbindlist(lapply(layer_one$spend_map$variable, function(v) {
  spend <- sum(layer_one$data[[layer_one$spend_map[variable == v, spend_col]]])
  effect <- parent_effect[[v]] + seq(-0.03, 0.03, length.out = 24L)
  data.table(.draw = as.character(seq_along(effect)), scope = "total", variable = v,
             spend_multiplier = 1, roi = effect, contribution = effect * spend, current_spend = spend)
}))
fake_curve_draws <- function(variables, format) {
  n <- nrow(layer_one$spend_map)
  if (variables == "rrate") {
    out <- sapply(seq_len(n), function(i) seq(0.12 + i / 100, 0.20 + i / 100, length.out = 24L))
    colnames(out) <- paste0("rrate[", seq_len(n), "]")
    return(out)
  }
  if (variables == "cvalue") {
    out <- sapply(seq_len(n), function(i) seq(0.8 + i / 20, 1.1 + i / 20, length.out = 24L))
    colnames(out) <- paste0("cvalue[", seq_len(n), "]")
    return(out)
  }
  if (variables == "dvalue") {
    out <- matrix(1, nrow = 24L, ncol = n)
    colnames(out) <- paste0("dvalue[", seq_len(n), "]")
    return(out)
  }
  stop("unexpected draw variable")
}
fake_parent_fit <- list(
  response_curves_draws = parent_draws,
  fit = list(draws = fake_curve_draws),
  variable_lookup = layer_one$metadata[role == "media", .(
    variable, has_curve = 1L, curve_param_idx = seq_len(.N), curve_type
  )]
)
leaf_handoff <- build_sequential_effectiveness_priors_from_parent_fit(
  parent_fit = fake_parent_fit,
  parent_layer = layer_one,
  child_layer = leaf_layer,
  time_col = "period"
)
stopifnot(setequal(leaf_handoff$business_priors$variable, rollup_spend$variable))
stopifnot(all(leaf_handoff$business_priors$sequential_child_layer == "leaf"))
stopifnot(all(leaf_handoff$business_priors$curve_prior_available))

# Parent effectiveness is recomputed per draw as total contribution / total
# spend, even if upstream ROI values or row granularity are misleading.
split_draws <- rbindlist(list(copy(parent_draws), copy(parent_draws)))
split_draws[, `:=`(contribution = contribution / 2, current_spend = current_spend / 2, roi = roi * 1.3)]
split_summary <- suppressWarnings(econ_seq_parent_effectiveness_draws(
  parent_fit = list(response_curves_draws = split_draws),
  parent_layer = layer_one
))
expected_effect <- parent_draws[, .(expected = mean(contribution / current_spend)), by = variable]
stopifnot(max(abs(split_summary$summary[expected_effect, on = "variable"]$parent_effectiveness - expected_effect$expected)) < 1e-10)
stopifnot(all(split_summary$roi_aggregation_audit$reported_roi_used_for_transfer == FALSE))

# One baseline contract is propagated to every Bayesian layer. Conflicting
# child settings fail unless the difference is deliberately audited.
shared_baseline <- econ_seq_baseline_contract(
  root_trend_spec = "none",
  root_fourier_harmonics = 0L,
  root_season_period = 52L,
  control_cols = "macro"
)
baseline_applied <- econ_seq_apply_baseline_contract(
  list(intercept_type = "flat", ucm_spec = list(level = FALSE, season = FALSE, cycle = FALSE)),
  shared_baseline
)
stopifnot(baseline_applied$fit_args$intercept_type == "flat")
baseline_conflict <- try(econ_seq_apply_baseline_contract(
  list(intercept_type = "fourier"),
  shared_baseline
), silent = TRUE)
stopifnot(inherits(baseline_conflict, "try-error"))

# National-root training periods use the same holdout contract as the child
# MMM, so future periods cannot leak into sequential parent evidence.
holdout_fixture <- data.table(
  geo = rep(c("a", "b"), each = 6L),
  period = rep(seq.Date(as.Date("2024-01-01"), by = "week", length.out = 6L), 2L),
  explicit_holdout = rep(c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE), 2L)
)
last_n_training <- econ_seq_training_time_values(
  holdout_fixture, "geo", "period", holdout_last_n = 2L
)
explicit_training <- econ_seq_training_time_values(
  holdout_fixture, "geo", "period", holdout_col = "explicit_holdout", holdout_value = TRUE
)
stopifnot(length(last_n_training) == 4L)
stopifnot(identical(sort(last_n_training), sort(explicit_training)))
mixed_holdout <- copy(holdout_fixture)
mixed_holdout[geo == "b" & period == max(period), explicit_holdout := FALSE]
stopifnot(inherits(try(econ_seq_training_time_values(
  mixed_holdout, "geo", "period", holdout_col = "explicit_holdout"
), silent = TRUE), "try-error"))

continued <- continue_sequential_hierarchical_bayes(
  parent_stage = list(
    child_fit = fake_parent_fit,
    rollup_layer = layer_one,
    root_fit = list(
      canonical_data = rollup_data,
      canonical_metadata = rollup_metadata,
      canonical_spend_map = rollup_spend,
      spend_map = rollup_spend
    ),
    source_rollup_map = NULL,
    baseline_spec = shared_baseline
  ),
  data = rollup_data,
  metadata_input = rollup_metadata,
  dep_var_col = "kpi",
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = rollup_spend,
  rollup_depth = "leaf",
  fit_child = FALSE
)
stopifnot(continued$rollup_layer$is_leaf_layer)
stopifnot(setequal(continued$rollup_layer$spend_map$variable, rollup_spend$variable))
stopifnot(continued$rollup_layer$branch_action_reconciliation$max_abs_row_spend_reconciliation_error[1] < 1e-10)

# Content-addressed checkpoints change for every required input class.
tmp_code <- tempfile(fileext = ".R")
writeLines("x <- 1", tmp_code)
on.exit(unlink(tmp_code), add = TRUE)
hash_case <- function(data = synthetic[1:5],
                      metadata_input = metadata,
                      rollup = metadata[, .(variable, rollup_path)],
                      media_scope = data.table(variable = "tv_support", spend_scope = "group_specific"),
                      baseline = shared_baseline,
                      transfer = list(curve_transfer_mode = "effectiveness_adstock_saturation"),
                      fit_args = list(a = 1),
                      seed = 1) {
  econ_seq_content_hash(
    data = data,
    metadata = metadata_input,
    rollup_map = rollup,
    media_scope_config = media_scope,
    baseline_spec = baseline,
    prior_transfer_settings = transfer,
    fit_args = fit_args,
    seed = seed,
    files = tmp_code
  )
}
base_hash <- hash_case()
stopifnot(base_hash != hash_case(data = synthetic[1:5][, kpi := kpi + 1]))
stopifnot(base_hash != hash_case(metadata_input = copy(metadata)[1, coef := 2]))
stopifnot(base_hash != hash_case(rollup = copy(metadata[, .(variable, rollup_path)])[1, rollup_path := "changed"]))
stopifnot(base_hash != hash_case(media_scope = data.table(variable = "tv_support", spend_scope = "national")))
stopifnot(base_hash != hash_case(baseline = utils::modifyList(shared_baseline, list(seasonal_period = 26))))
stopifnot(base_hash != hash_case(transfer = list(curve_transfer_mode = "effectiveness_only")))
stopifnot(base_hash != hash_case(fit_args = list(a = 2)))
stopifnot(base_hash != hash_case(seed = 2))
writeLines("x <- 2", tmp_code)
stopifnot(base_hash != hash_case())

# The opt-in validation script statically separates truth from primary fitting
# metadata and no longer uses a manual validation version string.
validation_path <- file.path(root_dir, "tests", "test_sequential_hierarchical_bayes_stan_validation.R")
if (file.exists(validation_path)) {
  validation_text <- paste(readLines(validation_path, warn = FALSE), collapse = "\n")
  stopifnot(grepl("truth_metadata", validation_text, fixed = TRUE))
  stopifnot(grepl("generic_fit_metadata", validation_text, fixed = TRUE))
  stopifnot(grepl("oracle_fit_metadata", validation_text, fixed = TRUE))
  stopifnot(!grepl("validation_version", validation_text, fixed = TRUE))
  stopifnot(grepl("checkpoint_hash", validation_text, fixed = TRUE))
  stopifnot(grepl("effectiveness_mae", validation_text, fixed = TRUE))
  stopifnot(grepl("normalized_saturation_mae", validation_text, fixed = TRUE))
  stopifnot(grepl("contribution_interval_coverage", validation_text, fixed = TRUE))
  stopifnot(grepl("holdout_rmse", validation_text, fixed = TRUE))
  stopifnot(grepl("ECONIMAP_SEQUENTIAL_VALIDATION_SEEDS", validation_text, fixed = TRUE))
}

cat("Sequential hierarchical Bayes hardening tests passed.\n")
