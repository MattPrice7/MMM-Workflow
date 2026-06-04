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
  results <<- rbind(results, data.table(test = test, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = as.character(detail)))
  if (!isTRUE(ok)) stop("FAILED: ", test, if (nzchar(detail)) paste0(" -- ", detail) else "")
}

manual_stan_like_transform <- function(x, rrate, cvalue, dvalue, train_mask, normalize_curve_x) {
  x <- pmax(as.numeric(x), 0)
  x[!is.finite(x)] <- 0
  carry <- numeric(length(x))
  lagged <- 0
  for (ii in seq_along(x)) {
    lagged <- x[ii] + rrate * lagged
    carry[ii] <- lagged
  }
  train_mask <- as.logical(train_mask)
  carry_scale <- 1
  if (isTRUE(normalize_curve_x)) {
    carry_scale <- mean(carry[train_mask], na.rm = TRUE)
    if (!is.finite(carry_scale) || abs(carry_scale) <= 1e-8) carry_scale <- 1
  }
  sat <- 1 - exp(-((pmax(carry / carry_scale, 1e-12) * cvalue) ^ dvalue))
  if (!isTRUE(normalize_curve_x)) return(sat)
  sat_mean <- mean(sat[train_mask], na.rm = TRUE)
  if (!is.finite(sat_mean) || abs(sat_mean) <= 1e-8) sat_mean <- 1
  sat / sat_mean
}

make_fit_obj <- function(normalize_curve_x) {
  dt <- data.table(
    row_id = seq_len(12),
    geo = "G1",
    week = seq.Date(as.Date("2024-01-07"), by = "week", length.out = 12),
    group_idx = 1L,
    tv = c(10, 11, 12, 40, 42, 43, 18, 16, 15, 13, 12, 11),
    tv_spend = c(100, 110, 120, 400, 420, 430, 180, 160, 150, 130, 120, 110),
    is_holdout__ = c(rep(FALSE, 9), rep(TRUE, 3)),
    rescale_factor__ = 100
  )
  pm <- list(
    beta = matrix(0.08, nrow = 1, ncol = 1),
    rrate = 0.35,
    cvalue = 1.4,
    dvalue = 1
  )
  transformed <- manual_stan_like_transform(dt$tv, pm$rrate[1], pm$cvalue[1], pm$dvalue[1], !dt$is_holdout__, normalize_curve_x)
  decomp <- data.table(
    row_id = dt$row_id,
    geo = dt$geo,
    week = dt$week,
    variable = "tv",
    contribution = pm$beta[1, 1] * transformed * dt$rescale_factor__
  )
  list(
    data = dt,
    metadata = data.table(variable = "tv", role = "media", spend_col = "tv_spend"),
    variable_lookup = data.table(variable = "tv", variable_idx = 1L, has_curve = 1L, role = "media"),
    stan_data = list(curve_idx = 1L),
    posterior_means = pm,
    normalize_curve_x = normalize_curve_x,
    long_decomp = decomp,
    group_col = "geo",
    time_col = "week"
  )
}

for (norm in c(TRUE, FALSE)) {
  fit_obj <- make_fit_obj(norm)
  row_contrib <- variable_contribution_rows_hier_mmm(fit_obj, "tv", multiplier = 1)
  decomp <- fit_obj$long_decomp$contribution
  add_result(
    paste0("row transform consistency normalize_curve_x=", norm),
    max(abs(row_contrib - decomp), na.rm = TRUE) < 1e-10,
    paste0("max_delta=", signif(max(abs(row_contrib - decomp), na.rm = TRUE), 8))
  )
  add_result(
    paste0("sum transform consistency normalize_curve_x=", norm),
    abs(variable_contribution_sum_hier_mmm(fit_obj, "tv") - sum(decomp)) < 1e-10
  )
  add_result(
    paste0("single period transform consistency normalize_curve_x=", norm),
    abs(row_contrib[4] - decomp[4]) < 1e-10
  )
  roi <- build_roi_mroi_hier_mmm(
    fit_obj,
    spend_map = data.table(variable = "tv", spend_col = "tv_spend"),
    step_pct = 0.01
  )
  expected_mroi <- (variable_contribution_sum_hier_mmm(fit_obj, "tv", multiplier = 1.01) -
                      variable_contribution_sum_hier_mmm(fit_obj, "tv", multiplier = 1)) /
    (sum(fit_obj$data$tv_spend) * 0.01)
  add_result(
    paste0("ROI/mROI helper uses canonical transform normalize_curve_x=", norm),
    nrow(roi) == 1L && abs(roi$mroi[1] - expected_mroi) < 1e-10,
    paste0("mroi_delta=", signif(abs(roi$mroi[1] - expected_mroi), 8))
  )

  sd <- list(
    N = nrow(fit_obj$data), G = 1L, J = 1L,
    J_linear = 0L, J_curve = 1L,
    curve_idx = 1L,
    group_id = rep(1L, nrow(fit_obj$data)),
    start_idx = 1L, end_idx = nrow(fit_obj$data),
    X = matrix(fit_obj$data$tv, ncol = 1),
    is_train = as.integer(!fit_obj$data$is_holdout__),
    normalize_curve_x = as.integer(norm),
    center_predictors_for_sampling = 0L,
    rrate_lower = 0, rrate_upper = 0.999,
    cvalue_lower = 0, cvalue_upper = 5,
    dvalue_lower = 0.1, dvalue_upper = 3,
    rrate_raw_mu = inv_logit_bounded(0.35, 0, 0.999),
    cvalue_raw_mu = inv_logit_bounded(1.4, 0, 5),
    dvalue_raw_mu = inv_logit_bounded(1, 0.1, 3),
    J_pos = 0L, J_neg = 0L, J_lower = 0L, J_upper = 0L, J_bounded = 0L, J_free = 0L
  )
  beta <- matrix(0.08, nrow = 1, ncol = 1)
  prior_mu <- prior_non_intercept_mu(sd, beta = beta)
  expected_prior_mu <- manual_stan_like_transform(fit_obj$data$tv, 0.35, 1.4, 1, !fit_obj$data$is_holdout__, norm) * 0.08
  add_result(
    paste0("prior_non_intercept_mu uses canonical curve transform normalize_curve_x=", norm),
    max(abs(prior_mu - expected_prior_mu), na.rm = TRUE) < 1e-10,
    paste0("max_delta=", signif(max(abs(prior_mu - expected_prior_mu), na.rm = TRUE), 8))
  )
}

dir.create(file.path(bundle_dir, "test_outputs"), showWarnings = FALSE)
fwrite(results, file.path(bundle_dir, "test_outputs", "transform_consistency_results.csv"))
message("\nTransform consistency results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")
invisible(results)
