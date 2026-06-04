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
source(file.path(bundle_dir, "semi_univariate_prior_builder_production_final.R"), chdir = TRUE)
source(file.path(bundle_dir, "mmm_prior_workflow.R"), chdir = TRUE)
source(file.path(bundle_dir, "mmm_deck_output_builder.R"), chdir = TRUE)
source(file.path(bundle_dir, "hier_mmm.R"), chdir = TRUE)
source(file.path(bundle_dir, "optimizer_scenario_planner.R"), chdir = TRUE)

results <- data.table(test = character(), status = character(), detail = character())
add_result <- function(test, ok, detail = "") {
  results <<- rbind(results, data.table(test = test, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = as.character(detail)))
  if (!isTRUE(ok)) stop("FAILED: ", test, if (nzchar(detail)) paste0(" -- ", detail) else "")
}

dt_market <- data.table(
  week = as.Date("2024-01-07") + 7 * 0:2,
  y = c(100, 110, 120),
  tv = c(10, 20, 30),
  population = c(1000, 0, NA_real_)
)
vm_market <- data.table(variable = "tv", modeled_x_col = "tv")
no_scale <- normalize_mmm_market_size_inputs(
  input_data = dt_market,
  variable_map = vm_market,
  dep_var_col = "y",
  population_col = "population",
  scale_media_by_market_size = FALSE,
  scale_dep_var_by_market_size = FALSE
)
add_result("bad market-size column does not fail when scaling is false", is.list(no_scale) && isFALSE(no_scale$scaled))

scale_error <- tryCatch({
  normalize_mmm_market_size_inputs(
    input_data = dt_market,
    variable_map = vm_market,
    dep_var_col = "y",
    population_col = "population",
    scale_media_by_market_size = TRUE
  )
  ""
}, error = function(e) conditionMessage(e))
add_result("bad market-size column fails when scaling is true", grepl("Market-size scaling requested", scale_error, fixed = TRUE))

long <- data.table(
  week = rep(as.Date("2024-01-07") + 7 * 0:1, each = 2),
  geo = "G1",
  entity = "brand",
  variable = rep(c("tv", "search"), times = 2),
  contribution = c(10, -2, 20, -3),
  y_actual = 100,
  pred = 95,
  residual = 5
)
raw <- data.table(
  week = as.Date("2024-01-07") + 7 * 0:2,
  geo = "G1",
  entity = "brand",
  tv = c(10, 20, 999),
  search = c(5, 6, 999),
  tv_spend = c(100, 200, 999),
  search_spend = c(50, 60, 999)
)
tables <- build_mmm_deck_tables(
  long_decomp = long,
  raw_data = raw,
  spend_map = data.table(variable = c("tv", "search"), spend_col = c("tv_spend", "search_spend")),
  media_variables = c("tv", "search"),
  time_col = "week",
  group_col = "geo",
  entity_col = "entity",
  period_granularity = "week"
)
tv_econ <- tables$kpi_economics[variable == "tv"]
search_econ <- tables$kpi_economics[variable == "search"]
add_result("raw spend aligns to long_decomp reporting rows", nrow(tv_econ) == 1L && abs(tv_econ$spend - 300) < 1e-8)
add_result("negative contribution economics are diagnostic not efficient", nrow(search_econ) == 1L &&
             search_econ$signed_economics_flag == "negative_contribution_diagnostic" &&
             !is.finite(search_econ$cost_per_outcome))
add_result("deck builder returns KPI decomposition funnel", is.data.table(tables$funnel_summary) &&
             all(c("stage", "metric_type", "value", "share_of_actual_kpi") %in% names(tables$funnel_summary)) &&
             all(c("Actual KPI", "Media contribution", "Media spend") %in% tables$funnel_summary$stage))

excel_week <- as.numeric(as.Date("2024-01-07") - as.Date("1899-12-30"))
long_excel <- data.table(
  week = c(excel_week, excel_week + 7),
  variable = "tv",
  contribution = c(10, 12)
)
tables_excel <- build_mmm_deck_tables(
  long_decomp = long_excel,
  media_variables = "tv",
  time_col = "week",
  period_granularity = "week"
)
add_result("deck builder parses Excel serial dates with 1899-12-30 origin",
           all(c("2024-01-07", "2024-01-14") %in% tables_excel$period_slicer_index$period_label))
add_result("deck builder handles long-only decomps without fit columns",
           is.data.table(tables_excel$period_kpi_change) &&
             all(c("actual", "fitted", "residual") %in% names(tables_excel$period_kpi_change)) &&
             all(is.na(tables_excel$period_kpi_change$actual)))

opt_m <- seq(0, 2, by = 0.25)
opt_curves <- rbindlist(list(
  data.table(variable = "tv", spend_multiplier = opt_m, current_spend = 300, contribution = 80 * (1 - exp(-1.3 * opt_m))),
  data.table(variable = "search", spend_multiplier = opt_m, current_spend = 110, contribution = 35 * (1 - exp(-0.8 * opt_m)))
))
opt_out <- run_optimizer_scenario_planner(
  response_curves = opt_curves,
  total_budget = 460,
  optimizer_method = "grid",
  optimization_grid_step = 0.25,
  max_grid_combinations = 200L,
  multiplier_grid = opt_m,
  max_multiplier = 2,
  scenario_multipliers = c(1, 1.2)
)
tables_opt <- build_mmm_deck_tables(
  long_decomp = long,
  raw_data = raw,
  spend_map = data.table(variable = c("tv", "search"), spend_col = c("tv_spend", "search_spend")),
  optimizer_output = opt_out,
  media_variables = c("tv", "search"),
  time_col = "week",
  group_col = "geo",
  entity_col = "entity",
  period_granularity = "week"
)
add_result("deck builder ingests optimizer output tables",
           nrow(tables_opt$optimizer_plan) == 2L &&
             nrow(tables_opt$optimizer_scenario_comparison) >= 2L &&
             nrow(tables_opt$optimizer_response_curves) > 0L)
add_result("deck builder exposes chart registry with optimizer charts",
           is.data.table(tables_opt$chart_registry) &&
             any(tables_opt$chart_registry$chart_id == "optimizer_response_curves" & tables_opt$chart_registry$available))
if (requireNamespace("ggplot2", quietly = TRUE)) {
  deck_out_dir <- file.path(bundle_dir, "test_outputs", "deck_optimizer_outputs")
  files_opt <- write_mmm_deck_outputs(
    report_tables = tables_opt,
    output_dir = deck_out_dir,
    prefix = "optimizer",
    write_charts = TRUE,
    write_html = TRUE,
    write_excel = FALSE,
    write_shiny = FALSE
  )
  add_result("deck builder writes optimizer charts when ggplot2 is available",
             any(grepl("optimizer_response_curves\\.png$", files_opt$chart_files)) &&
               any(grepl("optimizer_current_vs_recommended_spend\\.png$", files_opt$chart_files)) &&
               file.exists(files_opt$html_path))
} else {
  add_result("deck builder writes optimizer charts when ggplot2 is available", TRUE, "ggplot2 not installed; optional chart write skipped")
}

fake_fit <- list(
  variable_lookup = data.table(variable = c("tv", "search"), role = "media"),
  metadata = data.table(variable = c("tv", "search"), role = "media"),
  data = raw[1:2],
  long_decomp = copy(long)[, `:=`(row_id = rep(1:2, each = 2), sample = "train")],
  wide_decomp = data.table(
    row_id = 1:2,
    week = as.Date("2024-01-07") + 7 * 0:1,
    geo = "G1",
    entity = "brand",
    sample = "train",
    y_actual = c(100, 100),
    pred = c(95, 95),
    residual = c(5, 5),
    tv = c(10, 20),
    search = c(-2, -3)
  ),
  time_col = "week",
  group_col = "geo",
  entity_col = "entity"
)
fake_fit_spend <- attach_spend_to_hier_mmm_outputs(
  fake_fit,
  spend_map = data.table(variable = c("tv", "search"), spend_col = c("tv_spend", "search_spend")),
  raw_data = raw
)
add_result("Stan output spend/support attachment adds long and wide fields",
           "spend" %in% names(fake_fit_spend$long_decomp) &&
             "support" %in% names(fake_fit_spend$long_decomp) &&
             "total_spend" %in% names(fake_fit_spend$wide_decomp) &&
             "total_support" %in% names(fake_fit_spend$wide_decomp) &&
             abs(fake_fit_spend$long_decomp[variable == "tv", sum(spend, na.rm = TRUE)] - 300) < 1e-8 &&
             abs(fake_fit_spend$long_decomp[variable == "tv", sum(support, na.rm = TRUE)] - 0) > 1e-8 &&
             abs(fake_fit_spend$wide_decomp[, sum(total_spend, na.rm = TRUE)] - 410) < 1e-8 &&
             abs(fake_fit_spend$wide_decomp[, sum(total_support, na.rm = TRUE)] - 0) > 1e-8)

prior_input_data <- data.table(
  week = as.Date("2024-01-07") + 7 * 0:1,
  y = c(100, 100),
  tv_spend = c(50, 50),
  search_spend = c(50, 50),
  social_spend = c(50, 50),
  display_spend = c(50, 50),
  email_spend = c(50, 50)
)
prior_vars <- c("tv", "search", "social", "display", "email")
prior_output <- list(
  metadata_handoff = data.table(variable = prior_vars, spend_col = paste0(prior_vars, "_spend")),
  transformed_x_handoff = data.table(variable = rep(prior_vars, each = 2), x_handoff = 5)
)
business_priors <- data.table(
  variable = prior_vars,
  prior_metric = c("coef", "roi", "mroi", "ikpc", "cpkpi"),
  prior_mean = c(0.03, 2, 3, 0.20, 5),
  prior_precision = c(25, 100, NA, 400, 25),
  prior_sd = c(NA, NA, 0.30, NA, NA),
  prior_distribution = c("student_t", "normal", "lognormal", "normal", "normal"),
  kpi_value_per_outcome = c(NA, 10, 10, NA, NA)
)
coef_priors <- make_coef_benchmark_priors_from_kpi_economics(
  input_data = prior_input_data,
  prior_output = prior_output,
  kpi_priors = business_priors,
  dep_var_col = "y",
  max_precision = Inf
)
display_prior <- coef_priors[variable == "display"]
add_result("business prior helper accepts coef roi mroi ikpc and cpkpi inputs",
           nrow(coef_priors[!is.na(warning)]) == 0L &&
             all(prior_vars %in% coef_priors$variable) &&
             all(c("coef", "roi", "mroi", "ikpc", "cpkpi") %in% coef_priors$prior_metric) &&
             all(c("prior_distribution", "input_prior_precision", "coef_precision_uncapped") %in% names(coef_priors)))
add_result("business prior helper preserves true input precision on transformed coefficient scale",
           isTRUE(display_prior$input_precision_preserved[1]) &&
             abs(display_prior$input_prior_precision[1] - 400) < 1e-8 &&
             abs(display_prior$coef_precision_uncapped[1] - 40000) < 1e-6 &&
             abs(display_prior$coef_precision[1] - display_prior$coef_precision_uncapped[1]) < 1e-8)

base_md <- data.table(
  variable = prior_vars,
  coef = 0,
  coef_precision = 1,
  rrate = 0.2,
  rrate_precision = 1,
  cvalue = 1,
  cvalue_precision = 1,
  dvalue = 1,
  dvalue_precision = 1,
  coef_bound = "pos"
)
blended_md <- apply_benchmark_priors_to_metadata(base_md, coef_priors, max_precision = Inf)
tv_md <- blended_md[variable == "tv"]
add_result("business prior metadata blend preserves audit fields and precision",
           nrow(tv_md) == 1L &&
             abs(tv_md$coef_precision - 26) < 1e-8 &&
             identical(tv_md$benchmark_prior_distribution, "student_t") &&
             isTRUE(tv_md$benchmark_input_precision_preserved))

dir.create(file.path(bundle_dir, "test_outputs"), showWarnings = FALSE)
fwrite(results, file.path(bundle_dir, "test_outputs", "prior_deck_hardening_results.csv"))
message("\nPrior/deck hardening results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")
invisible(results)
