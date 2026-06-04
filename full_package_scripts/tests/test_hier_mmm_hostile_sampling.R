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

if (!requireNamespace("cmdstanr", quietly = TRUE) ||
    is.na(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
  add_result("hostile Stan sampling skipped", TRUE, "cmdstanr/CmdStan unavailable")
  message("\nHier MMM hostile sampling results")
  print(results)
  quit(save = "no", status = 0)
}

adstock_vec <- function(x, rrate) {
  out <- numeric(length(x))
  carry <- 0
  for (i in seq_along(x)) {
    carry <- x[i] + rrate * carry
    out[i] <- carry
  }
  out
}
sat_curve <- function(x, cvalue = 0.8, dvalue = 1) {
  z <- pmax(as.numeric(x), 0)
  1 - exp(-((pmax(z, 1e-12) * cvalue) ^ dvalue))
}

set.seed(20260526)
weeks <- seq.Date(as.Date("2024-01-07"), by = "week", length.out = 40L)
geos <- paste0("G", 1:4)
dt <- CJ(week = weeks, geo = geos)
dt[, entity := "brand"]
dt[, idx := match(week, weeks)]
dt[, gidx := match(geo, geos)]
dt[, season := sin(idx * 2 * pi / 26)]
dt[, trend := idx / max(idx)]
dt[, tv := pmax(1, 85 + 14 * season + 9 * gidx + stats::rnorm(.N, 0, 4))]
dt[, search := pmax(1, 35 + 0.55 * tv + 4 * cos(idx / 5) + stats::rnorm(.N, 0, 2))]
dt[, display := pmax(1, 50 + 7 * sin(idx / 6)), by = week]
dt[, promo := as.numeric(idx %% 8L %in% c(0L, 1L))]
dt[, tv_spend := tv * 10]
dt[, search_spend := search * 6]
dt[, display_spend := display * 4]
dt[, tv_ad := adstock_vec(tv, 0.35), by = geo]
dt[, search_ad := adstock_vec(search, 0.15), by = geo]
dt[, display_ad := adstock_vec(display, 0.25), by = geo]
for (cc in c("tv_ad", "search_ad", "display_ad")) {
  idx_col <- sub("_ad$", "_idx", cc)
  dt[, (idx_col) := get(cc) / mean(get(cc)), by = geo]
}
dt[, tv_contrib := 90 * sat_curve(tv_idx, 0.80)]
dt[, search_contrib := 65 * sat_curve(search_idx, 0.95)]
dt[, display_contrib := 35 * sat_curve(display_idx, 0.75)]
dt[, promo_contrib := 18 * promo]
dt[, baseline := 760 + 25 * gidx + 25 * trend + 22 * season]
dt[, y := baseline + tv_contrib + search_contrib + display_contrib + promo_contrib + stats::rnorm(.N, 0, 8)]

metadata <- data.table(
  variable = c("tv", "search", "display"),
  source_entity = "GLOBAL",
  role = "media",
  rrate = c(0.35, 0.15, 0.25),
  rrate_precision = c(25, 25, 25),
  cvalue = c(0.80, 0.95, 0.75),
  cvalue_precision = c(25, 25, 25),
  dvalue = 1,
  dvalue_precision = 100,
  coef = c(0.09, 0.06, 0.035),
  coef_precision = c(25, 25, 20),
  coef_bound = "pos",
  coef_hierarchy_scale = c(1, 1, 1)
)

fit_obj <- fit_hier_mmm(
  data = dt,
  metadata_input = metadata,
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  extra_control_cols = "promo",
  holdout_last_n = 4L,
  mean_index = TRUE,
  dep_mean_index_scope = "group",
  x_mean_index_scope = "global",
  intercept_type = "fourier",
  normalize_curve_x = TRUE,
  sample_curve_parameters = "never",
  sample_coef_hierarchy = "auto",
  coef_hierarchy_auto_min_geo_variation_share = 0.01,
  likelihood = "normal",
  init_strategy = "random",
  chains = 2,
  parallel_chains = 2,
  iter_warmup = 80,
  iter_sampling = 80,
  adapt_delta = 0.95,
  max_treedepth = 11,
  seed = 20260526,
  refresh = 0,
  output_dir = file.path(tempdir(), "mmm_hostile_stan_sampling"),
  output_prefix = "hostile",
  verbose = FALSE,
  output_variables = "lean"
)

add_result("hostile Stan fit returns row-level decompositions",
           nrow(fit_obj$wide_decomp) == nrow(dt) &&
             nrow(fit_obj$long_decomp) > nrow(dt))
add_result("hostile Stan fit retains train and holdout rows",
           all(c("train", "holdout") %in% unique(fit_obj$wide_decomp$sample)))

long_media <- fit_obj$long_decomp[variable != "residual", .(sum_contribution = sum(contribution, na.rm = TRUE)), by = row_id]
wide_check <- merge(fit_obj$wide_decomp[, .(row_id, y_actual, pred, residual)], long_media, by = "row_id")
add_result("hostile Stan decomposition reconciles to prediction",
           max(abs(wide_check$sum_contribution - wide_check$pred), na.rm = TRUE) < 1e-6)
add_result("hostile Stan prediction plus residual reconciles to actual",
           max(abs(wide_check$pred + wide_check$residual - wide_check$y_actual), na.rm = TRUE) < 1e-6)

fit_train <- fit_obj$diagnostics$fit_quality_overall[sample == "train"]
add_result("hostile Stan train fit quality is usable",
           nrow(fit_train) == 1L && is.finite(fit_train$r2) && fit_train$r2 > 0.50,
           paste0("r2=", signif(fit_train$r2[1], 4)))

coef_sign <- fit_obj$coef_long[variable %in% c("tv", "search", "display")]
add_result("hostile Stan media coefficients keep expected positive sign",
           nrow(coef_sign) > 0 && all(coef_sign$coef > 0))

display_hierarchy_flag <- fit_obj$variable_lookup[variable == "display", sample_coef_hierarchy_flag][1]
tv_hierarchy_flag <- fit_obj$variable_lookup[variable == "tv", sample_coef_hierarchy_flag][1]
add_result("national repeated media avoids automatic geo hierarchy",
           identical(as.integer(display_hierarchy_flag), 0L))
add_result("small-geo hostile fit pools weak geo hierarchy by default",
           identical(as.integer(tv_hierarchy_flag), 0L) &&
             identical(as.integer(fit_obj$coef_hierarchy_auto_min_groups), 5L))

roi <- build_roi_mroi_hier_mmm(
  fit_obj,
  spend_map = data.table(
    variable = c("tv", "search", "display"),
    spend_col = c("tv_spend", "search_spend", "display_spend")
  ),
  raw_data = dt,
  step_pct = 0.02
)
add_result("hostile Stan ROI/mROI outputs are finite for media",
           nrow(roi) == 3L && all(is.finite(roi$roi)) && all(is.finite(roi$mroi)))

sampler <- fit_obj$diagnostics$sampler_overall
add_result("hostile Stan sampler diagnostics are returned",
           nrow(sampler) == 1L && all(c("divergences_total", "treedepth_hits_total", "max_rhat") %in% names(sampler)))
if (nrow(sampler) && is.finite(sampler$treedepth_hits_total[1]) && sampler$treedepth_hits_total[1] > 0) {
  add_result("hostile Stan treedepth pressure is surfaced in recommendations",
             "max_treedepth_hits" %in% fit_obj$diagnostics$sampler_recommendations$issue,
             paste0("treedepth_hits_total=", sampler$treedepth_hits_total[1]))
} else {
  add_result("hostile Stan treedepth pressure is surfaced in recommendations", TRUE, "no treedepth hits")
}
add_result("hostile Stan prior-posterior diagnostics are returned",
           nrow(fit_obj$diagnostics$prior_posterior_coef) > 0 &&
             nrow(fit_obj$diagnostics$prior_posterior_curve) > 0)
add_result("hostile Stan model readiness summary is returned",
           nrow(fit_obj$diagnostics$model_readiness) == 1L &&
             nrow(fit_obj$diagnostics$model_readiness_issues) > 0 &&
             "short_warmup_smoke_run" %in% names(fit_obj$diagnostics$model_readiness) &&
             isTRUE(fit_obj$diagnostics$model_readiness$short_warmup_smoke_run[1]))

dir.create(file.path(bundle_dir, "test_outputs"), showWarnings = FALSE)
fwrite(results, file.path(bundle_dir, "test_outputs", "hier_mmm_hostile_sampling_results.csv"))
message("\nHier MMM hostile sampling results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")
invisible(results)
