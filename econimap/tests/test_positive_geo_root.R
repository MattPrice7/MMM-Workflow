file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])
test_file <- if (length(file_arg) && nzchar(file_arg)) {
  normalizePath(file_arg, mustWork = FALSE)
} else tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
root_dir <- if (!is.na(test_file)) dirname(dirname(test_file)) else getwd()
if (!requireNamespace("data.table", quietly = TRUE)) stop("Positive geo-root test requires data.table.")
suppressPackageStartupMessages(library(data.table))
invisible(lapply(sort(list.files(file.path(root_dir, "R"), pattern = "[.]R$", full.names = TRUE)), source))

set.seed(602)
periods <- seq.Date(as.Date("2024-01-01"), by = "week", length.out = 72L)
geos <- paste0("geo_", seq_len(5L))
population <- c(0.8, 1.1, 1.7, 2.3, 3.2) * 1e6
effectiveness <- c(0.55, 0.65, 0.70, 0.75, 0.85)
panel <- rbindlist(lapply(seq_along(geos), function(ii) {
  n <- length(periods)
  spend <- population[ii] / 1e6 * pmax(10, 110 + 30 * sin(seq_len(n) / 5 + ii) + rnorm(n, 0, 13))
  macro <- sin(seq_len(n) / 12)
  data.table(
    period = periods,
    geo = geos[ii],
    entity = "brand",
    population = population[ii],
    paid_support = spend * 12,
    paid_spend = spend,
    macro = macro,
    kpi = population[ii] / 1e6 * (550 + 24 * macro) + effectiveness[ii] * spend + rnorm(n, 0, 5)
  )
}))
metadata <- data.table(
  variable = c("paid_support", "macro"),
  role = c("media", "control"),
  spend_col = c("paid_spend", NA_character_),
  rollup_path = c("total_paid_media > paid", "business_controls > macro"),
  coef = 0,
  coef_precision = 1,
  coef_bound = c("pos", "free")
)
fit <- fit_parsimonious_total_media_root(
  data = panel,
  metadata_input = metadata,
  dep_var_col = "kpi",
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  population_col = "population",
  root_scope = "hierarchical_panel",
  root_control_cols = "macro",
  root_pressure_scaling = "auto",
  root_media_transform = "linear",
  root_time_baseline = "knots",
  root_knot_n = 4L,
  root_geo_media_effect = "partially_pooled",
  root_bootstrap_reps = 0L
)
expected <- weighted.mean(effectiveness, population)
stopifnot(fit$root_summary$root_geo_media_effect_mode[1] == "partially_pooled_log_normal")
stopifnot(fit$root_summary$root_geo_media_effect_scale[1] == "log")
stopifnot(all(fit$root_geo_media_effects$root_media_beta > 0))
stopifnot(abs(fit$root_summary$root_effectiveness[1] - expected) < 0.15)

# A geo-panel root may need local linear baseline drift while retaining a
# shared total-media effect. The trend option is deliberately opt-in, and this
# regression guard confirms that it does not leak into a generic root fit.
set.seed(603)
trend_panel <- rbindlist(lapply(seq_along(geos), function(ii) {
  n <- length(periods)
  time_index <- seq_len(n)
  spend <- pmax(15, 130 + 22 * sin(time_index / 4 + ii) + rnorm(n, 0, 16))
  local_trend <- c(-2.1, -0.9, 0.4, 1.3, 2.5)[ii] * (time_index - mean(time_index))
  data.table(
    period = periods,
    geo = geos[ii],
    entity = "brand",
    population = 1e6,
    paid_support = spend * 9,
    paid_spend = spend,
    kpi = 900 + ii * 35 + local_trend + 0.68 * spend + rnorm(n, 0, 6)
  )
}))
trend_metadata <- metadata[variable != "macro"]
trend_fit <- fit_parsimonious_total_media_root(
  data = trend_panel,
  metadata_input = trend_metadata,
  dep_var_col = "kpi",
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  population_col = "population",
  root_scope = "hierarchical_panel",
  root_media_transform = "linear",
  root_time_baseline = "fourier",
  root_fourier_harmonics = 0L,
  root_trend_spec = "none",
  root_geo_trend = "fixed_linear",
  root_geo_media_effect = "shared",
  holdout_last_n = 8L,
  root_bootstrap_reps = 0L
)
stopifnot(trend_fit$root_summary$root_geo_trend[1] == "fixed_linear")
stopifnot(abs(trend_fit$root_summary$root_effectiveness[1] - 0.68) < 0.10)
stopifnot(trend_fit$root_scope_eligibility$decision[1] == "fit_hierarchical_root_with_partial_pooling")
stopifnot(trend_fit$root_holdout_validation$summary$root_holdout_status[1] == "available")
stopifnot(trend_fit$root_holdout_validation$summary$root_holdout_row_n[1] == length(geos) * 8L)
stopifnot(trend_fit$root_holdout_validation$summary$root_holdout_media_rmse_improvement[1] > 0)
stopifnot(trend_fit$root_summary$root_effectiveness_status[1] == "positive_transferable")

# A positive training estimate without incremental holdout value must never be
# passed to children as if it were transferable parent evidence.
nontransferable_root <- trend_fit
nontransferable_root$root_summary <- copy(trend_fit$root_summary)
nontransferable_root$root_summary[, root_effectiveness_status := "positive_in_sample_not_transferable_holdout"]
nontransferable_priors <- build_sequential_effectiveness_priors(
  root_fit = nontransferable_root,
  data = trend_panel,
  metadata_input = trend_metadata,
  time_col = "period",
  training_times = trend_fit$root_training_times,
  child_variables = "paid_support",
  child_spend_map = data.table(variable = "paid_support", spend_col = "paid_spend")
)
stopifnot(!nontransferable_priors$business_priors$parent_positive_effect_transferred[1])
stopifnot(nontransferable_priors$business_priors$prior_mean[1] == 0)

# Observed geo variation alone is not evidence of a positive parent effect.
# With no true media signal, the positive-constrained root must remain neutral
# and cannot generate a transferable child prior.
set.seed(604)
null_panel <- rbindlist(lapply(seq_along(geos), function(ii) {
  n <- length(periods)
  time_index <- seq_len(n)
  spend <- pmax(15, 120 + 27 * sin(time_index / 4 + ii) + rnorm(n, 0, 15))
  data.table(
    period = periods,
    geo = geos[ii],
    entity = "brand",
    population = 1e6,
    paid_support = spend * 11,
    paid_spend = spend,
    kpi = 850 + ii * 25 + c(-1.5, -0.7, 0.2, 1.1, 2.0)[ii] * (time_index - mean(time_index))
  )
}))
null_fit <- fit_parsimonious_total_media_root(
  data = null_panel,
  metadata_input = trend_metadata,
  dep_var_col = "kpi",
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  population_col = "population",
  root_scope = "hierarchical_panel",
  root_media_transform = "linear",
  root_time_baseline = "fourier",
  root_fourier_harmonics = 0L,
  root_trend_spec = "none",
  root_geo_trend = "fixed_linear",
  root_geo_media_effect = "shared",
  holdout_last_n = 8L,
  root_bootstrap_reps = 0L
)
stopifnot(null_fit$root_summary$root_effectiveness_status[1] != "positive_transferable")
stopifnot(!isTRUE(null_fit$root_summary$root_sign_boundary_active[1]) ||
          abs(null_fit$root_summary$root_effectiveness[1]) < 1e-8)
cat("Positive log-scale geo root test passed.\n")
