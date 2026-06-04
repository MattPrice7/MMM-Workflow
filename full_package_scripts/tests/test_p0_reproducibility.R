#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

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

results <- data.table(test = character(), status = character(), detail = character())
add_result <- function(test, ok, detail = "") {
  results <<- rbind(
    results,
    data.table(test = test, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = as.character(detail)),
    use.names = TRUE
  )
  if (!isTRUE(ok)) stop("FAILED: ", test, if (nzchar(detail)) paste0(" -- ", detail) else "")
  invisible(TRUE)
}

run_clean_r <- function(expr) {
  code <- paste0("setwd(", shQuote(bundle_dir), "); ", expr)
  out <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", "-e", shQuote(code)), stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  list(ok = is.null(status) || identical(status, 0L), output = paste(out, collapse = "\n"))
}

clean_source <- run_clean_r("source('mmm_workflow.R'); stopifnot(is.function(run_mmm_workflow), is.function(build_mmm_priors), is.function(run_mmm_quasi_geo_test)); cat('workflow source ok\\n')")
add_result("clean session sources mmm_workflow.R", clean_source$ok, clean_source$output)

script_files <- c(
  "marketing_mix_diagnostic_builder_production_final.R",
  "semi_univariate_prior_builder_production_final.R",
  "prior_recovery_builder.R",
  "mmm_prior_workflow.R",
  "quasi_experimental_dose_response_analysis.R",
  "quasi_geo_test.R",
  "hier_mmm.R",
  "mmm_deck_output_builder.R",
  "pull_dma_population.R"
)
for (ff in script_files) {
  env <- new.env(parent = globalenv())
  ok <- tryCatch({
    source(file.path(bundle_dir, ff), local = env, chdir = TRUE)
    TRUE
  }, error = function(e) {
    add_result(paste("source", ff), FALSE, conditionMessage(e))
    FALSE
  })
  if (isTRUE(ok)) add_result(paste("source", ff), TRUE)
}

source(file.path(bundle_dir, "semi_univariate_prior_builder_production_final.R"), chdir = TRUE)
shrink_tbl <- data.table(variable = c("tv", "search"), coef_center_shrinkage = c(0.25, 0.75))
add_result("coef shrinkage table selects tv", identical(pb_coef_shrinkage_for(shrink_tbl, "tv"), 0.25))
add_result("coef shrinkage table selects search", identical(pb_coef_shrinkage_for(shrink_tbl, "search"), 0.75))
add_result("coef shrinkage table defaults missing variable", identical(pb_coef_shrinkage_for(shrink_tbl, "social", default = 1), 1))

set.seed(20260525)
n <- 52L
dt <- data.table(
  week = seq.Date(as.Date("2024-01-07"), by = "week", length.out = n),
  y = 100 + sin(seq_len(n) / 3) + rnorm(n, 0, 0.2),
  tv = pmax(1, 50 + 10 * sin(seq_len(n) / 4))
)
dt[, is_holdout := week > sort(unique(week))[n - 6L]]
dt_perturbed <- copy(dt)
dt_perturbed[is_holdout == TRUE, `:=`(y = y * 100, tv = tv * 50)]
vm <- data.table(variable = "tv", modeled_x_col = "tv", spend_col = "tv")
base_prior <- prior_builder(
  input_data = dt,
  date_col = "week",
  dep_var_col = "y",
  variable_map = vm,
  holdout_col = "is_holdout",
  estimate_cvalue_from_data = "auto",
  use_fourier = FALSE,
  use_holidays = FALSE,
  use_week_of_month = FALSE
)
pert_prior <- prior_builder(
  input_data = dt_perturbed,
  date_col = "week",
  dep_var_col = "y",
  variable_map = vm,
  holdout_col = "is_holdout",
  estimate_cvalue_from_data = "auto",
  use_fourier = FALSE,
  use_holidays = FALSE,
  use_week_of_month = FALSE
)
compare_cols <- intersect(c("rrate", "cvalue", "coef_prior_final", "coef_precision_final"), names(base_prior$priors))
base_vals <- as.numeric(unlist(base_prior$priors[1, ..compare_cols], use.names = FALSE))
pert_vals <- as.numeric(unlist(pert_prior$priors[1, ..compare_cols], use.names = FALSE))
max_delta <- max(abs(base_vals - pert_vals), na.rm = TRUE)
add_result("prior builder ignores perturbed holdout rows", is.finite(max_delta) && max_delta < 1e-10, paste0("max_delta=", signif(max_delta, 8)))
add_result("prior builder returns holdout audit", nrow(base_prior$holdout_audit) == 1L && base_prior$holdout_audit$holdout_row_n > 0)

source(file.path(bundle_dir, "quasi_geo_test.R"), chdir = TRUE)
q_weeks <- seq.Date(as.Date("2024-01-07"), by = "week", length.out = 36L)
q_dt <- CJ(week = q_weeks, geo = paste0("G", 1:5))
q_dt[, idx := match(week, q_weeks)]
q_dt[, tv := 50 + idx * 0.1 + as.integer(factor(geo))]
q_dt[, y := 1000 + 2 * idx + as.integer(factor(geo)) * 10]
q_dt[geo == "G1" & week >= q_weeks[18] & week < q_weeks[22], tv := tv + 40]
q_dt[geo == "G1" & week >= q_weeks[18] & week < q_weeks[22], y := y + 60]
q_dt[, is_holdout := week >= q_weeks[32]]
q_dt_perturbed <- copy(q_dt)
q_dt_perturbed[is_holdout == TRUE, `:=`(tv = tv * 100, y = y * 100)]
q_vm <- data.table(variable = "tv", modeled_x_col = "tv", spend_col = "tv")
q_base <- run_quasi_geo_test(
  q_dt, "week", "y", "geo",
  holdout_col = "is_holdout",
  variable_map = q_vm,
  normalize = "geo_mean_index",
  pre_weeks = 6,
  post_weeks = 4,
  rolling_window = 6,
  min_pct_change = 0.20,
  min_robust_z = 0.5,
  min_volume = 0.1,
  min_donors = 2
)
q_pert <- run_quasi_geo_test(
  q_dt_perturbed, "week", "y", "geo",
  holdout_col = "is_holdout",
  variable_map = q_vm,
  normalize = "geo_mean_index",
  pre_weeks = 6,
  post_weeks = 4,
  rolling_window = 6,
  min_pct_change = 0.20,
  min_robust_z = 0.5,
  min_volume = 0.1,
  min_donors = 2
)
q_cols <- intersect(c("incremental_outcome", "incremental_media", "incremental_spend", "marginal_response"), names(q_base$event_estimates))
q_delta <- if (nrow(q_base$event_estimates) && nrow(q_pert$event_estimates) && length(q_cols)) {
  max(abs(as.numeric(unlist(q_base$event_estimates[1, ..q_cols], use.names = FALSE)) -
            as.numeric(unlist(q_pert$event_estimates[1, ..q_cols], use.names = FALSE))), na.rm = TRUE)
} else {
  NA_real_
}
add_result("quasi geo ignores perturbed holdout rows", is.finite(q_delta) && q_delta < 1e-10, paste0("max_delta=", signif(q_delta, 8)))

env_dma <- new.env(parent = globalenv())
source(file.path(bundle_dir, "pull_dma_population.R"), local = env_dma, chdir = TRUE)
add_result("pull_dma_population source has no eager output objects", is.function(env_dma$pull_dma_population) && is.null(env_dma$dma_population))

if (tolower(Sys.getenv("RUN_STAN_TESTS", "false")) %in% c("true", "1", "yes")) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) stop("RUN_STAN_TESTS requested but cmdstanr is not installed.")
  mod <- cmdstanr::cmdstan_model(file.path(bundle_dir, "hier_mmm.stan"), force_recompile = FALSE)
  add_result("hier_mmm.stan compiles with cmdstanr", inherits(mod, "CmdStanModel"))
} else {
  add_result("hier_mmm.stan compile skipped", TRUE, "Set RUN_STAN_TESTS=true to compile.")
}

dir.create(file.path(bundle_dir, "test_outputs"), showWarnings = FALSE)
fwrite(results, file.path(bundle_dir, "test_outputs", "p0_reproducibility_results.csv"))
message("\nP0 reproducibility results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")

invisible(results)
