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
source(file.path(bundle_dir, "mmm_workflow.R"), chdir = TRUE)

results <- data.table(test = character(), status = character(), detail = character())
add_result <- function(test, ok, detail = "") {
  results <<- rbind(results, data.table(test = test, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = as.character(detail)))
  if (!isTRUE(ok)) stop("FAILED: ", test, if (nzchar(detail)) paste0(" -- ", detail) else "", call. = FALSE)
}

set.seed(11)
dt <- CJ(week = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 24), group = c("dma_a", "dma_b"))
dt[, population := fifelse(group == "dma_a", 1000000, 500000)]
dt[, tv_support := fifelse(group == "dma_a", 100, 55) * (1 + 0.25 * sin(seq_len(.N) / 4))]
dt[, tv_spend := tv_support * fifelse(group == "dma_a", 120, 130)]
dt[, search_support := fifelse(group == "dma_a", 80, 35) * (1 + 0.15 * cos(seq_len(.N) / 5))]
dt[, search_spend := search_support * fifelse(group == "dma_a", 40, 45)]
dt[, is_holdout := week > max(week) - 21]

vm <- data.table(
  variable = c("tv", "search"),
  support_col = c("tv_support", "search_support"),
  spend_col = c("tv_spend", "search_spend"),
  curve_type = c("hill", "weibull"),
  anchor_saturation = c(0.50, 0.35),
  dvalue = c(1.0, 1.2),
  rrate = c(0.35, 0.10),
  current_roi = c(0.20, NA),
  cost_per_kpi = c(NA, 8)
)

bau <- create_bau_response_curves(
  data = dt,
  variable_map = vm,
  group_col = "group",
  date_col = "week",
  population_col = "population",
  support_basis = "auto",
  curve_scope = "both",
  channel_curve_anchors = c(tv = 0.30),
  multiplier_grid = seq(0, 2, by = 0.25),
  holdout_col = "is_holdout"
)

add_result("main workflow exposes BAU response-curve helper",
           exists("run_mmm_bau_response_curves", mode = "function") &&
             identical(run_mmm_bau_response_curves, create_bau_response_curves))
add_result("BAU helper returns core tables",
           all(c("response_curves", "curve_metadata", "settings") %in% names(bau)) &&
             nrow(bau$response_curves) > 0 &&
             nrow(bau$curve_metadata) == 6L)
add_result("BAU curves include total and group scopes",
           identical(sort(unique(bau$response_curves$scope)), c("group", "total")) &&
             all(c("ALL", "dma_a", "dma_b") %in% unique(bau$response_curves$group)))
add_result("channel anchor override is applied",
           all(abs(bau$curve_metadata[variable == "tv", anchor_saturation] - 0.30) < 1e-8) &&
             all(abs(bau$curve_metadata[variable == "search", anchor_saturation] - 0.35) < 1e-8))
add_result("anchor saturation is reproduced by curve metadata",
           max(abs(bau$curve_metadata$saturation_at_anchor - bau$curve_metadata$anchor_saturation), na.rm = TRUE) < 1e-8)
add_result("population is used for group curves in auto support basis",
           all(bau$curve_metadata[scope == "group", support_basis] == "per_population_index") &&
             all(bau$curve_metadata[scope == "total", support_basis] == "raw"))
add_result("current multiplier is normalized to response index one",
           max(abs(bau$response_curves[abs(spend_multiplier - 1) < 1e-8, response_index] - 1), na.rm = TRUE) < 1e-8)
add_result("business scale makes BAU curves optimizer-ready",
           all(bau$response_curves$optimizer_ready) &&
             all(is.finite(bau$response_curves[spend_multiplier > 0, contribution])) &&
             all(is.finite(bau$response_curves[spend_multiplier > 0, roi])))

planner <- run_optimizer_scenario_planner(
  response_curves = bau$response_curves[scope == "total"],
  total_budget = bau$response_curves[scope == "total" & abs(spend_multiplier - 1) < 1e-8, sum(current_spend, na.rm = TRUE)],
  multiplier_grid = seq(0, 2, by = 0.25),
  optimizer_method = "grid",
  optimization_grid_step = 0.25,
  max_grid_combinations = 200L,
  scenario_multipliers = 1
)
add_result("BAU curves can feed optimizer scenario planner",
           nrow(planner$optimization_plan) == 2L &&
             is.finite(planner$optimization_summary$expected_contribution[1]))

no_scale <- create_bau_response_curves(
  data = dt,
  variable_map = vm[, .(variable, support_col, spend_col, curve_type, anchor_saturation)],
  group_col = "group",
  date_col = "week",
  multiplier_grid = c(0, 1, 2)
)
add_result("BAU shape-only curves are clearly not optimizer-ready",
           !any(no_scale$response_curves$optimizer_ready) &&
             all(is.na(no_scale$response_curves$contribution)))

no_dep_manual_rrate <- create_bau_response_curves(
  data = dt,
  variable_map = vm[, .(variable, support_col, spend_col, curve_type, anchor_saturation, rrate)],
  group_col = "group",
  date_col = "week",
  curve_scope = "both",
  multiplier_grid = c(0, 1, 2)
)
add_result("BAU does not estimate rrate without dependent variable",
           all(no_dep_manual_rrate$rrate_diagnostics$rrate_estimation_status == "not_estimated_supplied_rrate") &&
             all(abs(no_dep_manual_rrate$curve_metadata[variable == "tv", adstock_decay] - 0.35) < 1e-12) &&
             all(abs(no_dep_manual_rrate$curve_metadata[variable == "search", adstock_decay] - 0.10) < 1e-12))

rr_weeks <- seq.Date(as.Date("2023-01-02"), by = "week", length.out = 72)
rr_dt <- CJ(week = rr_weeks, group = c("dma_a", "dma_b"))
rr_dt[, pulse := as.numeric(week %in% rr_weeks[c(8, 9, 22, 23, 41, 42, 58, 59)])]
rr_dt[, tv_support := fifelse(group == "dma_a", 80, 45) + fifelse(group == "dma_a", 180, 120) * pulse]
rr_total <- rr_dt[, .(tv_total = sum(tv_support)), by = week][order(week)]
rr_total[, y_total := 500 + 4.5 * bau_adstock(tv_total, decay = 0.55)]
rr_dt[rr_total, y := i.y_total / 2, on = "week"]
rr_vm <- data.table(
  variable = "tv",
  support_col = "tv_support",
  spend_col = NA_character_,
  curve_type = "hill",
  anchor_saturation = 0.50,
  dvalue = 1.00,
  current_contribution = 100
)
rr_est <- create_bau_response_curves(
  data = rr_dt,
  variable_map = rr_vm,
  group_col = "group",
  date_col = "week",
  dep_var_col = "y",
  curve_scope = "both",
  estimate_rrate = TRUE,
  rrate_grid = seq(0, 0.80, by = 0.05),
  rrate_min_improvement = 0,
  multiplier_grid = c(0, 1, 2)
)
add_result("BAU can estimate one shared diagnostic rrate per variable when KPI is supplied",
           nrow(rr_est$rrate_diagnostics) == 1L &&
             rr_est$rrate_diagnostics$rrate_selected[1] > 0.20 &&
             all(abs(rr_est$curve_metadata$adstock_decay - rr_est$rrate_diagnostics$rrate_selected[1]) < 1e-12) &&
             length(unique(rr_est$curve_metadata$adstock_decay)) == 1L &&
             grepl("diagnostic", rr_est$rrate_diagnostics$rrate_source[1], fixed = TRUE))

data.table::fwrite(results, file.path(bundle_dir, "test_outputs", "bau_response_curves_results.csv"))
message("BAU response-curve tests passed.")
