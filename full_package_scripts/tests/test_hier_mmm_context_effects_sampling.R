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
  add_result("context-effect Stan sampling skipped", TRUE, "cmdstanr/CmdStan unavailable")
  message("\nHier MMM context-effect sampling results")
  print(results)
  quit(save = "no", status = 0)
}

scale_train <- function(x, train = rep(TRUE, length(x))) {
  m <- mean(x[train], na.rm = TRUE)
  s <- stats::sd(x[train], na.rm = TRUE)
  if (!is.finite(s) || s <= 1e-8) s <- 1
  (x - m) / s
}

set.seed(20260611)
n <- 90L
dt <- data.table(
  week = seq.Date(as.Date("2024-01-07"), by = "week", length.out = n),
  geo = "G1",
  entity = "brand"
)
dt[, idx := seq_len(.N)]
dt[, tv_context := 75 + 24 * sin(idx / 6) + 8 * cos(idx / 13) + stats::rnorm(.N, 0, 3)]
dt[, macro_pressure := 100 + 12 * cos(idx / 9) + 0.25 * idx + stats::rnorm(.N, 0, 2)]
dt[, search := pmax(3, 22 + 5 * sin(idx / 4 + 0.4) + stats::rnorm(.N, 0, 2))]
dt[, price := pmax(0.75, 1.10 + 0.08 * sin(idx / 7) + stats::rnorm(.N, 0, 0.015))]
dt[, tv_context_z_true := scale_train(tv_context)]
dt[, macro_pressure_z_true := scale_train(macro_pressure)]
theta_search <- 0.45
theta_price <- 0.35
dt[, true_search_contribution := 5.5 * exp(theta_search * tv_context_z_true) * search]
dt[, true_price_contribution := -85 * exp(theta_price * macro_pressure_z_true) * price]
dt[, y := 330 + true_search_contribution + true_price_contribution + stats::rnorm(.N, 0, 2.5)]
dt[, row_id := .I]

metadata <- data.table(
  variable = c("search", "price"),
  source_entity = "GLOBAL",
  role = c("media", "control"),
  rrate = 0,
  rrate_precision = 1,
  cvalue = 0,
  cvalue_precision = 1,
  dvalue = 0,
  dvalue_precision = 1,
  coef = c(5.5, -85),
  coef_precision = c(4, 0.02),
  coef_bound = c("pos", "neg")
)
context_effects <- data.table(
  variable = c("search", "price"),
  context_col = c("tv_context", "macro_pressure"),
  context_coef_mean = c(0, 0),
  context_coef_sd = c(0.45, 0.45),
  context_sign = c("+", "+")
)

fit_obj <- fit_hier_mmm(
  data = dt,
  metadata_input = metadata,
  context_effects = context_effects,
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  mean_index = FALSE,
  intercept_type = "flat",
  sample_curve_parameters = "never",
  sample_coef_hierarchy = "never",
  likelihood = "normal",
  init_strategy = "random",
  chains = 2,
  parallel_chains = 2,
  iter_warmup = 150,
  iter_sampling = 150,
  adapt_delta = 0.95,
  max_treedepth = 11,
  seed = 20260611,
  refresh = 0,
  output_dir = file.path(tempdir(), "mmm_context_effect_sampling"),
  output_prefix = "context_truth",
  verbose = FALSE,
  create_response_curves = FALSE,
  output_variables = "lean"
)

context_diag <- fit_obj$diagnostics$prior_posterior_context
add_result("context-effect diagnostics are returned",
           nrow(context_diag) == 2L &&
             all(c("variable", "context_key", "posterior_mean", "posterior_q05", "posterior_q95") %in% names(context_diag)))

search_theta <- context_diag[variable == "search" & context_key == "tv_context", posterior_mean][1]
price_theta <- context_diag[variable == "price" & context_key == "macro_pressure", posterior_mean][1]
add_result("upper-funnel context makes search more effective in known-truth fit",
           is.finite(search_theta) && search_theta > 0.15,
           paste0("posterior_mean=", signif(search_theta, 4), ", true=", theta_search))
add_result("macro context increases price sensitivity magnitude in known-truth fit",
           is.finite(price_theta) && price_theta > 0.10,
           paste0("posterior_mean=", signif(price_theta, 4), ", true=", theta_price))

wd <- copy(fit_obj$wide_decomp)
ld <- copy(fit_obj$long_decomp)
fit_train <- fit_obj$diagnostics$fit_quality_overall[sample == "train"]
add_result("context-effect known-truth fit has strong fit quality",
           nrow(fit_train) == 1L && is.finite(fit_train$r2) && fit_train$r2 > 0.90,
           paste0("r2=", signif(fit_train$r2[1], 4)))
add_result("context-effect decomposition reconciles to prediction",
           max(abs(ld[variable != "residual", .(sum_contribution = sum(contribution)), by = row_id][
             wd[, .(row_id, pred)], on = "row_id"
           ][, sum_contribution - pred]), na.rm = TRUE) < 1e-6)

search_rows <- ld[variable == "search"]
price_rows <- ld[variable == "price"]
search_rows[dt, tv_context_z_true := i.tv_context_z_true, on = .(row_id)]
price_rows[dt, macro_pressure_z_true := i.macro_pressure_z_true, on = .(row_id)]
search_hi <- search_rows[tv_context_z_true >= stats::quantile(tv_context_z_true, 0.75), mean(contribution / dt$search[row_id])]
search_lo <- search_rows[tv_context_z_true <= stats::quantile(tv_context_z_true, 0.25), mean(contribution / dt$search[row_id])]
price_hi <- price_rows[macro_pressure_z_true >= stats::quantile(macro_pressure_z_true, 0.75), mean(abs(contribution / dt$price[row_id]))]
price_lo <- price_rows[macro_pressure_z_true <= stats::quantile(macro_pressure_z_true, 0.25), mean(abs(contribution / dt$price[row_id]))]
add_result("search effective contribution per support is higher when TV context is high",
           is.finite(search_hi) && is.finite(search_lo) && search_hi > search_lo,
           paste0("high=", signif(search_hi, 4), ", low=", signif(search_lo, 4)))
add_result("price effective negative contribution is larger when macro pressure is high",
           is.finite(price_hi) && is.finite(price_lo) && price_hi > price_lo,
           paste0("high=", signif(price_hi, 4), ", low=", signif(price_lo, 4)))

dir.create(file.path(bundle_dir, "test_outputs"), showWarnings = FALSE)
fwrite(results, file.path(bundle_dir, "test_outputs", "hier_mmm_context_effects_sampling_results.csv"))
message("\nHier MMM context-effect sampling results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")
invisible(results)
