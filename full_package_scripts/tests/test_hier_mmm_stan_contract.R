#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

test_dir <- tryCatch({
  frames <- sys.frames()
  ofiles <- vapply(frames, function(f) if (!is.null(f$ofile)) as.character(f$ofile)[1] else NA_character_, character(1))
  ofiles <- ofiles[!is.na(ofiles) & nzchar(ofiles)]
  if (length(ofiles)) {
    dirname(normalizePath(ofiles[length(ofiles)], mustWork = FALSE))
  } else {
    file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)) else getwd()
  }
}, error = function(e) getwd())
bundle_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(bundle_dir, "hier_mmm.R"), chdir = TRUE)

results <- data.table(test = character(), status = character(), detail = character())
add_result <- function(test, ok, detail = "") {
  results <<- rbind(results, data.table(
    test = test,
    status = if (isTRUE(ok)) "PASS" else "FAIL",
    detail = as.character(detail)
  ), use.names = TRUE)
  if (!isTRUE(ok)) stop("FAILED: ", test, if (nzchar(detail)) paste0(" -- ", detail) else "")
  invisible(TRUE)
}

make_panel <- function(groups = c("G1", "G2"), n = 24L) {
  weeks <- seq.Date(as.Date("2024-01-07"), by = "week", length.out = n)
  dt <- CJ(week = weeks, geo = groups)
  dt[, entity := paste0("brand_", geo)]
  dt[, idx := match(week, weeks)]
  dt[, gidx := match(geo, groups)]
  dt[, tv := pmax(1, 80 + 10 * sin(idx / 4) + 8 * gidx)]
  dt[, national_tv := pmax(1, 90 + 8 * sin(idx / 5))]
  dt[, price := 1 + 0.03 * cos(idx / 6)]
  dt[, promo := as.numeric(idx %% 9L == 0L)]
  dt[, context_signal := 0.5 * sin(idx / 5) + 0.1 * gidx]
  dt[, lower_var := pmax(0.1, 5 + 0.2 * idx + gidx)]
  dt[, upper_var := pmax(0.1, 7 + 0.1 * idx - 0.2 * gidx)]
  dt[, bounded_var := pmax(0.1, 4 + sin(idx / 3) + 0.5 * gidx)]
  dt[, free_var := cos(idx / 8) + 0.1 * gidx]
  dt[, y := 1000 + 0.9 * tv - 20 * price + 4 * lower_var - 2 * upper_var + 3 * bounded_var + free_var]
  dt[]
}

make_meta <- function(vars,
                      curve = vars,
                      coef = rep(0.05, length(vars)),
                      coef_bound = rep("pos", length(vars)),
                      curve_type = rep("weibull", length(vars))) {
  data.table(
    variable = vars,
    source_entity = "GLOBAL",
    role = "media",
    curve_type = curve_type,
    rrate = ifelse(vars %in% curve, 0.25, 0),
    rrate_precision = 16,
    cvalue = ifelse(vars %in% curve, 0.80, 0),
    cvalue_precision = 16,
    dvalue = ifelse(vars %in% curve, 1.00, 0),
    dvalue_precision = 25,
    coef = coef,
    coef_precision = 25,
    coef_bound = coef_bound,
    coef_hierarchy_scale = 1
  )
}

prep_curve_only <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = "G1", n = 18L),
  metadata_input = make_meta("tv"),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  holdout_last_n = 3L,
  intercept_type = "flat",
  normalize_curve_x = TRUE,
  sample_curve_parameters = "never",
  sample_coef_hierarchy = "auto"
)
sd1 <- prep_curve_only$stan_data
add_result("curve-only one-group Stan data contract",
           sd1$J_curve == 1L && sd1$J_linear == 0L && sd1$K_extra == 0L &&
             sd1$G == 1L && sd1$N_train == sd1$N - 3L)
add_result("non-level baseline does not allocate unused state innovations",
           identical(as.integer(sd1$N_state_innov), 0L))
add_result("residual scale has finite Stan upper bound",
           is.finite(sd1$sigma_y_upper) && sd1$sigma_y_upper > sd1$sigma_y_floor)
init1 <- build_ucm_warm_start_init(prep_curve_only, chains = 2L, seed = 99)
finite_init <- all(vapply(init1, function(x) {
  vals <- unlist(x, recursive = TRUE, use.names = FALSE)
  all(is.finite(vals))
}, logical(1)))
add_result("curve-only warm-start init is finite", finite_init)
add_result("non-centered alpha mode uses one group intercept vector",
           sd1$alpha_parameterization == 1L &&
             sd1$G_alpha == sd1$G &&
             length(init1[[1]]$alpha_z) == sd1$G)
curve_helper_check <- media_transform_hier_mmm(
  x = prep_curve_only$stan_data$X[, prep_curve_only$stan_data$curve_idx[1]],
  rrate = prep_curve_only$curve_priors[variable == "tv", rrate_lower + (rrate_upper - rrate_lower) * plogis(rrate_raw_mu)][1],
  cvalue = prep_curve_only$curve_priors[variable == "tv", cvalue_lower + (cvalue_upper - cvalue_lower) * plogis(cvalue_raw_mu)][1],
  dvalue = prep_curve_only$curve_priors[variable == "tv", dvalue_lower + (dvalue_upper - dvalue_lower) * plogis(dvalue_raw_mu)][1],
  curve_type = "weibull",
  train_mask = prep_curve_only$stan_data$is_train == 1L,
  normalize_curve_x = TRUE
)
add_result("fixed-curve Stan input precomputes canonical transform",
           max(abs(sd1$X_curve_fixed[, 1] - curve_helper_check$transformed), na.rm = TRUE) < 1e-12 &&
             abs(sd1$X_curve_fixed_center[1, 1] - curve_helper_check$center_value) < 1e-12)

context_panel <- make_panel(groups = c("G1", "G2"), n = 18L)
context_meta <- make_meta("tv")
context_meta[, context := "(context_signal, 0.02, 0.03, +)"]
prep_context <- prepare_stan_data_hier_mmm(
  data = context_panel,
  metadata_input = context_meta,
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  holdout_last_n = 2L,
  intercept_type = "flat",
  normalize_curve_x = TRUE,
  sample_curve_parameters = "never",
  context_log_multiplier_bound = 1.5
)
sd_context <- prep_context$stan_data
context_train <- sd_context$is_train == 1L
add_result("context metadata tuples build Stan context contract",
           sd_context$K_context == 1L &&
             ncol(sd_context$X_context) == 1L &&
             sd_context$K_context_pos == 1L &&
             sd_context$K_context_free == 0L &&
             sd_context$context_log_multiplier_bound == 1.5 &&
             all(sd_context$context_variable_idx == prep_context$variable_lookup[variable == "tv", variable_idx][1]))
add_result("context drivers are standardized on training rows",
           max(abs(colMeans(sd_context$X_context[context_train, , drop = FALSE])), na.rm = TRUE) < 1e-12 &&
             max(abs(apply(sd_context$X_context[context_train, , drop = FALSE], 2, sd) - 1), na.rm = TRUE) < 1e-12 &&
             prep_context$context_effects[context_key == "context_signal", grepl("named data column", context_note)][1] &&
             all(c("context_risk_level", "prior_multiplier_min_train_range", "prior_multiplier_max_train_range") %in% names(prep_context$context_effects)))
context_init <- build_ucm_warm_start_init(prep_context, chains = 1L, seed = 123)
add_result("context warm-start init respects sign-constrained parameter blocks",
           length(context_init[[1]]$context_coef_pos) == 1L &&
             length(context_init[[1]]$context_coef_free) == 0L &&
             context_init[[1]]$context_coef_pos[1] > 0)
time_context_error <- tryCatch({
  z <- make_meta("tv")
  z[, context := "(time, 0, 0.10, +-)"]
  prepare_stan_data_hier_mmm(
    data = context_panel,
    metadata_input = z,
    dep_var_col = "y",
    group_col = "geo",
    time_col = "week",
    entity_col = "entity",
    intercept_type = "flat",
    sample_curve_parameters = "never"
  )
  ""
}, error = function(e) conditionMessage(e))
add_result("time context is blocked by default",
           grepl("disabled by default", time_context_error, fixed = TRUE))
self_context_error <- tryCatch({
  z <- make_meta("tv")
  z[, context := "(tv, 0, 0.10, +-)"]
  prepare_stan_data_hier_mmm(
    data = context_panel,
    metadata_input = z,
    dep_var_col = "y",
    group_col = "geo",
    time_col = "week",
    entity_col = "entity",
    intercept_type = "flat",
    sample_curve_parameters = "never"
  )
  ""
}, error = function(e) conditionMessage(e))
add_result("self-context is blocked",
           grepl("cannot use itself as a context modifier", self_context_error, fixed = TRUE))

generic_panel <- make_panel(groups = c("north", "south"), n = 10L)
generic_panel[, model_id := paste(entity, geo, sep = "|")]
prep_generic_group <- prepare_stan_data_hier_mmm(
  data = generic_panel,
  metadata_input = make_meta("tv"),
  dep_var_col = "y",
  group_col = "model_id",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  sample_curve_parameters = "never"
)
add_result("Stan accepts arbitrary composite model-id group columns",
           prep_generic_group$stan_data$G == 2L &&
             all(c("group_value", "target_entity", "group_idx") %in% names(prep_generic_group$group_lookup)) &&
             any(grepl("\\|", prep_generic_group$group_lookup$group_value)))

prep_shared_alpha <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c("G1", "G2"), n = 12L),
  metadata_input = make_meta("tv"),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  alpha_parameterization = "shared",
  sample_curve_parameters = "never"
)
init_shared_alpha <- build_ucm_warm_start_init(prep_shared_alpha, chains = 1L, seed = 101)
add_result("shared alpha mode drops unused group intercept vector",
           prep_shared_alpha$stan_data$alpha_parameterization == 3L &&
             prep_shared_alpha$stan_data$G_alpha == 0L &&
             length(init_shared_alpha[[1]]$alpha_z) == 0L)

prep_linear_only <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = "G1", n = 18L),
  metadata_input = make_meta("price", curve = character(), coef = -0.05, coef_bound = "neg"),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  normalize_curve_x = TRUE,
  sample_curve_parameters = "never"
)
sd2 <- prep_linear_only$stan_data
add_result("linear-only Stan data contract",
           sd2$J_curve == 0L && sd2$J_linear == 1L && sd2$J_bounded == 1L &&
             length(sd2$curve_idx) == 0L && length(sd2$linear_idx) == 1L)

prep_level <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c("G1", "G2"), n = 12L),
  metadata_input = make_meta("tv"),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "ucm",
  ucm_spec = list(level = TRUE, season = FALSE, cycle = FALSE),
  sample_curve_parameters = "never"
)
add_result("level baseline allocates expected state innovations",
           identical(as.integer(prep_level$stan_data$N_state_innov), prep_level$stan_data$N - prep_level$stan_data$G))

vars <- c("tv", "price", "lower_var", "upper_var", "bounded_var", "free_var")
meta_mixed <- make_meta(
  vars = vars,
  curve = "tv",
  coef = c(0.08, -0.04, 0.03, -0.02, 0.03, 0.00),
  coef_bound = c("pos", "neg", "(0.01,)", "(,-0.01)", "(0.01,0.08)", "free")
)
prep_mixed <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c("G1", "G2", "G3"), n = 22L),
  metadata_input = meta_mixed,
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  extra_control_cols = "promo",
  holdout_last_n = 2L,
  intercept_type = "fourier",
  normalize_curve_x = FALSE,
  center_predictors_for_sampling = TRUE,
  sample_curve_parameters = "always",
  sample_coef_hierarchy = "always",
  estimate_dvalue = TRUE,
  likelihood = "normal"
)
sd3 <- prep_mixed$stan_data
add_result("mixed coefficient blocks map correctly",
           sd3$J_pos == 0L && sd3$J_neg == 0L && sd3$J_lower == 0L &&
             sd3$J_upper == 0L && sd3$J_bounded == 5L && sd3$J_free == 1L)
add_result("extra controls and centered predictors contract",
           sd3$K_extra == 1L && sd3$center_predictors_for_sampling == 1L &&
             any(abs(sd3$X_center_mean) > 0))
add_result("normal likelihood and sampled dvalue flags propagate",
           sd3$likelihood_family == 1L && sd3$estimate_dvalue == 1L &&
             all(sd3$sample_curve_parameter == 1L))

holiday_panel <- make_panel(groups = "G1", n = 18L)
prep_holidays <- prepare_stan_data_hier_mmm(
  data = holiday_panel,
  metadata_input = make_meta("tv"),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  sample_curve_parameters = "never",
  holiday_config = list(
    calendar = "US_major",
    windows = c("week_before", "week_of", "week_after"),
    prefix = "us_holiday"
  )
)
add_result("Stan prep can generate built-in holiday controls as extra regressors",
           prep_holidays$stan_data$K_extra == 3L &&
             nrow(prep_holidays$holiday_control_audit) == 3L &&
             all(prep_holidays$holiday_control_audit$active_rows > 0) &&
             all(prep_holidays$holiday_control_audit$generated_col %in% names(prep_holidays$data)))
prep_custom_holiday <- prepare_stan_data_hier_mmm(
  data = holiday_panel,
  metadata_input = make_meta("tv"),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  sample_curve_parameters = "never",
  holiday_config = list(
    include_built_in = FALSE,
    custom_holidays = data.table(holiday_name = "launch_week", holiday_date = as.Date("2024-02-04")),
    windows = "week_of",
    mode = "separate",
    prefix = "custom_holiday"
  )
)
add_result("Stan prep can generate custom holiday controls",
           prep_custom_holiday$stan_data$K_extra == 1L &&
             prep_custom_holiday$holiday_control_audit$holiday_name[1] == "launch_week" &&
             prep_custom_holiday$holiday_control_audit$active_rows[1] == 1L)

prep_multi_fixed_curve <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c("G1", "G2"), n = 18L),
  metadata_input = make_meta(c("tv", "national_tv")),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  sample_curve_parameters = "never"
)
sd_multi_fixed <- prep_multi_fixed_curve$stan_data
add_result("fixed-curve mode passes one curve flag per curved variable",
           sd_multi_fixed$J_curve == 2L &&
             length(sd_multi_fixed$sample_curve_parameter) == 2L &&
             all(sd_multi_fixed$sample_curve_parameter == 0L))

prep_hill_curve <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = "G1", n = 18L),
  metadata_input = make_meta("tv", curve_type = "hill"),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  sample_curve_parameters = "never"
)
add_result("Hill curve type propagates to Stan data contract",
           prep_hill_curve$metadata[variable == "tv", curve_type][1] == "hill" &&
             identical(as.integer(prep_hill_curve$stan_data$curve_type[1]), 2L) &&
             length(prep_hill_curve$stan_data$curve_type) == prep_hill_curve$stan_data$J_curve)
mt_weibull <- media_transform_hier_mmm(1:10, rrate = 0.2, cvalue = 0.8, dvalue = 1.2, curve_type = "weibull")
mt_hill <- media_transform_hier_mmm(1:10, rrate = 0.2, cvalue = 0.8, dvalue = 1.2, curve_type = "hill")
add_result("Hill and Weibull transforms are both finite and distinct",
           all(is.finite(mt_weibull$transformed)) &&
             all(is.finite(mt_hill$transformed)) &&
             max(abs(mt_weibull$transformed - mt_hill$transformed), na.rm = TRUE) > 1e-6)

anchor_meta <- data.table(
  variable = c("tv", "national_tv"),
  source_entity = "GLOBAL",
  role = "media",
  curve_type = c("weibull", "hill"),
  rrate = 0.20,
  rrate_precision = 16,
  anchor_saturation = c(0.30, 0.65),
  dvalue = 1.00,
  dvalue_precision = 25,
  coef = 0.05,
  coef_precision = 25,
  coef_bound = "pos"
)
prep_anchor_direct <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = "G1", n = 18L),
  metadata_input = anchor_meta,
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  normalize_curve_x = TRUE,
  sample_curve_parameters = "never"
)
anchor_rates <- prep_anchor_direct$curve_priors[, cvalue_lower + (cvalue_upper - cvalue_lower) * plogis(cvalue_raw_mu)]
add_result("Stan direct metadata derives curve rate from anchor saturation without prior builder",
           prep_anchor_direct$metadata[, all(cvalue_from_anchor == TRUE)] &&
             prep_anchor_direct$curve_priors[, all(is.finite(anchor_saturation))] &&
             all(is.finite(anchor_rates) & anchor_rates > 0) &&
             abs(anchor_rates[1] - anchor_rates[2]) > 1e-6)

minimal_media_meta <- data.table(
  variable = "tv",
  source_entity = "GLOBAL",
  role = "media",
  curve_type = "hill",
  coef = 0.05,
  coef_precision = 25,
  coef_bound = "pos"
)
prep_minimal_media <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = "G1", n = 18L),
  metadata_input = minimal_media_meta,
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  normalize_curve_x = TRUE,
  sample_curve_parameters = "never"
)
add_result("minimal media metadata defaults to 50 percent median-support saturation",
           prep_minimal_media$metadata[variable == "tv", has_curve][1] == 1L &&
             abs(prep_minimal_media$metadata[variable == "tv", anchor_saturation][1] - 0.50) < 1e-12 &&
             prep_minimal_media$metadata[variable == "tv", cvalue_from_anchor][1] == TRUE &&
             prep_minimal_media$metadata[variable == "tv", rrate_defaulted][1] == TRUE &&
             prep_minimal_media$metadata[variable == "tv", dvalue_defaulted][1] == TRUE &&
             prep_minimal_media$metadata[variable == "tv", dvalue][1] > 0 &&
             prep_minimal_media$stan_data$J_curve == 1L)

bp_data <- make_panel(groups = "G1", n = 24L)
bp_data[, tv_spend := tv * 2]
bp_meta <- data.table(
  variable = "tv",
  source_entity = "GLOBAL",
  role = "media",
  curve_type = "weibull",
  anchor_saturation = 0.50,
  coef = 0.02,
  coef_precision = 4,
  coef_bound = "pos"
)
bp_direct <- data.table(variable = "tv", prior_metric = "coef", prior_mean = 0.08, prior_precision = 100)
bp_direct_update <- apply_business_priors_to_metadata_hier_mmm(
  data = bp_data,
  metadata_input = bp_meta,
  business_priors = bp_direct,
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  spend_map = data.table(variable = "tv", spend_col = "tv_spend")
)
add_result("Stan business priors accept direct coefficient precision",
           bp_direct_update$metadata[variable == "tv", abs(coef - 0.08) < 1e-12] &&
             bp_direct_update$metadata[variable == "tv", abs(coef_precision - 100) < 1e-12] &&
             bp_direct_update$business_prior_audit[variable == "tv", input_precision_preserved][1] == TRUE)

bp_cpkpi <- data.table(variable = "tv", prior_metric = "cpkpi", prior_mean = 10, prior_sd = 2)
bp_cpkpi_update <- apply_business_priors_to_metadata_hier_mmm(
  data = bp_data,
  metadata_input = bp_meta,
  business_priors = bp_cpkpi,
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  spend_map = data.table(variable = "tv", spend_col = "tv_spend"),
  holdout_last_n = 4L
)
bp_prep <- prepare_stan_data_hier_mmm(
  data = bp_data,
  metadata_input = bp_cpkpi_update$metadata,
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  sample_curve_parameters = "never"
)
add_result("Stan business priors convert CPKPI to coefficient prior auditably",
           nrow(bp_cpkpi_update$business_prior_audit[variable == "tv" & is.na(warning)]) == 1L &&
             bp_cpkpi_update$metadata[variable == "tv", business_prior_metric][1] == "cpkpi" &&
             is.finite(bp_cpkpi_update$metadata[variable == "tv", coef][1]) &&
             is.finite(bp_cpkpi_update$metadata[variable == "tv", coef_precision][1]) &&
             bp_prep$metadata[variable == "tv", business_prior_basis][1] == "average_metric_delta_method")

bp_data_perturbed <- copy(bp_data)
bp_data_perturbed[seq_len(.N) > .N - 4L, tv_spend := tv_spend * 1000]
bp_cpkpi_perturbed <- apply_business_priors_to_metadata_hier_mmm(
  data = bp_data_perturbed,
  metadata_input = bp_meta,
  business_priors = bp_cpkpi,
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  spend_map = data.table(variable = "tv", spend_col = "tv_spend"),
  holdout_last_n = 4L
)
add_result("Stan business-prior conversion ignores perturbed holdout spend",
           abs(bp_cpkpi_update$metadata[variable == "tv", coef][1] -
                 bp_cpkpi_perturbed$metadata[variable == "tv", coef][1]) < 1e-12 &&
             abs(bp_cpkpi_update$metadata[variable == "tv", coef_precision][1] -
                   bp_cpkpi_perturbed$metadata[variable == "tv", coef_precision][1]) < 1e-12)

add_result("fit_hier_mmm exposes direct business_priors front-door argument",
           all(c("business_priors", "kpi_value_per_outcome", "business_prior_default_relative_sd") %in% names(formals(fit_hier_mmm))))
add_result("fit_hier_mmm exposes holiday_config front-door argument",
           "holiday_config" %in% names(formals(fit_hier_mmm)))

prep_national_media <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c("G1", "G2", "G3"), n = 20L),
  metadata_input = make_meta("national_tv"),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  x_mean_index_scope = "global",
  sample_coef_hierarchy = "auto"
)
nat_flag <- prep_national_media$variable_lookup[variable == "national_tv", sample_coef_hierarchy_flag][1]
add_result("national-repeated media does not auto-sample group hierarchy",
           identical(as.integer(nat_flag), 0L))

prep_geo_media <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c("G1", "G2", "G3"), n = 20L),
  metadata_input = make_meta("tv"),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  x_mean_index_scope = "global",
  sample_coef_hierarchy = "auto",
  coef_hierarchy_auto_min_groups = 3L,
  coef_hierarchy_auto_min_geo_variation_share = 0.01
)
geo_flag <- prep_geo_media$variable_lookup[variable == "tv", sample_coef_hierarchy_flag][1]
add_result("geo-varying media can auto-sample group hierarchy",
           identical(as.integer(geo_flag), 1L))
prep_scope_none <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c("G1", "G2", "G3"), n = 20L),
  metadata_input = {
    z <- make_meta("tv")
    z[, coef_hierarchy_scope := "none"]
    z
  },
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  sample_coef_hierarchy = "always"
)
add_result("coef_hierarchy_scope none blocks group coefficient hierarchy even when requested globally",
           prep_scope_none$variable_lookup[variable == "tv", sample_coef_hierarchy_flag][1] == 0L &&
             prep_scope_none$metadata[variable == "tv", coef_hierarchy_scale][1] == 0 &&
             prep_scope_none$variable_lookup[variable == "tv", hierarchy_blocker_reason][1] == "coef_hierarchy_scope_none")
keyed_missing_parts_error <- tryCatch({
  prepare_stan_data_hier_mmm(
    data = make_panel(groups = c("G1", "G2", "G3"), n = 20L),
    metadata_input = {
      z <- make_meta("tv")
      z[, `:=`(coef_hierarchy_scope = "keyed", hierarchy_key = "snacks")]
      z
    },
    dep_var_col = "y",
    group_col = "geo",
    time_col = "week",
    entity_col = "entity",
    intercept_type = "flat",
    sample_coef_hierarchy = "always"
  )
  ""
}, error = function(e) conditionMessage(e))
add_result("keyed hierarchy requires group/model-id part indices",
           grepl("requires one-time coef_hierarchy_part_indices", keyed_missing_parts_error, fixed = TRUE))
prep_scope_keyed <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c(
    "east_walmart_chips",
    "east_target_chips",
    "west_walmart_chips",
    "east_walmart_pretzels",
    "west_target_pretzels"
  ), n = 20L),
  metadata_input = {
    z <- make_meta("tv")
    z[, `:=`(
      coef_hierarchy_scope = "keyed",
      hierarchy_key = "product"
    )]
    z
  },
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  sample_coef_hierarchy = "always",
  coef_hierarchy_part_indices = 3
)
add_result("keyed hierarchy maps group model-id parts into Stan pooling families",
           prep_scope_keyed$variable_lookup[variable == "tv", coef_hierarchy_scope][1] == "keyed" &&
             prep_scope_keyed$variable_lookup[variable == "tv", coef_hierarchy_mode][1] == 2L &&
             prep_scope_keyed$variable_lookup[variable == "tv", sample_coef_hierarchy_flag][1] == 1L &&
             prep_scope_keyed$stan_data$K_coef_hierarchy_keys == 2L &&
             all(sort(prep_scope_keyed$coef_hierarchy_key_lookup$coef_hierarchy_key_value) == c("chips", "pretzels")) &&
             length(prep_scope_keyed$stan_data$group_coef_hierarchy_key_id) == prep_scope_keyed$stan_data$G &&
             any(c(
               prep_scope_keyed$stan_data$coef_hierarchy_mode_pos,
               prep_scope_keyed$stan_data$coef_hierarchy_mode_neg,
               prep_scope_keyed$stan_data$coef_hierarchy_mode_lower,
               prep_scope_keyed$stan_data$coef_hierarchy_mode_upper,
               prep_scope_keyed$stan_data$coef_hierarchy_mode_bounded,
               prep_scope_keyed$stan_data$coef_hierarchy_mode_free
             ) == 2L))
prep_scope_keyed_single <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c(
    "east_walmart_chips",
    "east_target_chips",
    "west_walmart_chips"
  ), n = 20L),
  metadata_input = {
    z <- make_meta("tv")
    z[, `:=`(
      coef_hierarchy_scope = "keyed",
      hierarchy_key = "product"
    )]
    z
  },
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  sample_coef_hierarchy = "always",
  coef_hierarchy_part_indices = 3
)
single_modes <- unlist(prep_scope_keyed_single$stan_data[c(
  "coef_hierarchy_mode_pos",
  "coef_hierarchy_mode_neg",
  "coef_hierarchy_mode_lower",
  "coef_hierarchy_mode_upper",
  "coef_hierarchy_mode_bounded",
  "coef_hierarchy_mode_free"
)], use.names = FALSE)
add_result("single keyed family collapses to regular global hierarchy",
           prep_scope_keyed_single$stan_data$K_coef_hierarchy_keys == 1L &&
             prep_scope_keyed_single$variable_lookup[variable == "tv", sample_coef_hierarchy_flag][1] == 1L &&
             any(single_modes == 1L) &&
             !any(single_modes == 2L))
prep_scope_global <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c("G1", "G2", "G3"), n = 20L),
  metadata_input = {
    z <- make_meta("tv")
    z[, coef_hierarchy_scope := "global"]
    z
  },
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  sample_coef_hierarchy = "always"
)
add_result("global hierarchy scope keeps current explicit all-group coefficient pooling behavior",
           prep_scope_global$variable_lookup[variable == "tv", sample_coef_hierarchy_flag][1] == 1L)
add_result("Stan curve parameters remain shared variable-level values across groups",
           prep_scope_global$stan_data$J_curve == 1L &&
             prep_scope_global$stan_data$G == 3L &&
             nrow(prep_scope_global$curve_priors[variable == "tv"]) == 1L)
prep_small_geo_auto <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c("G1", "G2", "G3"), n = 20L),
  metadata_input = make_meta("tv"),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  x_mean_index_scope = "global",
  sample_coef_hierarchy = "auto",
  coef_hierarchy_auto_min_geo_variation_share = 0.01
)
small_geo_flag <- prep_small_geo_auto$variable_lookup[variable == "tv", sample_coef_hierarchy_flag][1]
add_result("small-geo auto mode pools coefficients instead of sampling weak hierarchy",
           identical(as.integer(small_geo_flag), 0L))

override <- data.table(geo = "G2", variable = "tv", coef = 0.12, coef_precision = 100)
prep_override <- prepare_stan_data_hier_mmm(
  data = make_panel(groups = c("G1", "G2"), n = 18L),
  metadata_input = make_meta("tv"),
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  intercept_type = "flat",
  coef_override_input = override
)
sd4 <- prep_override$stan_data
g2 <- prep_override$group_lookup[group_value == "G2", group_idx][1]
j_tv <- prep_override$variable_lookup[variable == "tv", variable_idx][1]
add_result("coef override maps to intended group and variable",
           sd4$use_coef_overrides == 1L &&
             abs(sd4$coef_override_mu[g2, j_tv] - 0.12) < 1e-12 &&
             abs(sd4$coef_override_sd[g2, j_tv] - 0.10) < 1e-12)

zero_train_error <- tryCatch({
  bad <- make_panel(groups = c("G1", "G2"), n = 8L)
  bad[geo == "G2", holdout := TRUE]
  bad[geo == "G1", holdout := FALSE]
  prepare_stan_data_hier_mmm(
    data = bad,
    metadata_input = make_meta("tv"),
    dep_var_col = "y",
    group_col = "geo",
    time_col = "week",
    entity_col = "entity",
    holdout_col = "holdout",
    intercept_type = "flat"
  )
  ""
}, error = function(e) conditionMessage(e))
add_result("groups with zero training rows fail clearly",
           grepl("zero training rows", zero_train_error, fixed = TRUE))
add_result("sampler diagnostic finite min/max helpers return NA on all-missing values",
           is.na(finite_min_or_na(c(NA_real_, Inf))) &&
             is.na(finite_max_or_na(c(NA_real_, -Inf))))
single_chain_readiness <- build_hier_mmm_model_readiness(list(
  sampler_overall = data.table(
    chains = 1L,
    iterations_total = 80L,
    divergences_total = 0,
    treedepth_hits_total = 0,
    mean_accept_stat = 0.9,
    mean_n_leapfrog = 10,
    min_bfmi = 0.8,
    max_rhat = 1.2,
    min_ess_bulk = 80,
    min_ess_tail = 80
  ),
  diagnostics_flags = data.table()
), iter_warmup = 300L, iter_sampling = 80L, max_treedepth = 12L)
add_result("single-chain readiness does not misuse R-hat",
           "single_chain_run" %in% single_chain_readiness$issues$issue &&
             !("rhat_over_1_05" %in% single_chain_readiness$issues$issue))

dir.create(file.path(bundle_dir, "test_outputs"), showWarnings = FALSE)
fwrite(results, file.path(bundle_dir, "test_outputs", "hier_mmm_stan_contract_results.csv"))
message("\nHier MMM Stan contract results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")
invisible(results)
