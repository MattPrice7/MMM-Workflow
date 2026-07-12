installed_mode <- requireNamespace("econimap", quietly = TRUE)
if (!requireNamespace("data.table", quietly = TRUE)) stop("Smoke test requires core dependency data.table.")
if (!"package:data.table" %in% search()) suppressPackageStartupMessages(library(data.table))
if (installed_mode) {
  library(econimap)
  local_script_path <- function(file) econimap_script_path(file)
} else {
  test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
  package_root <- if (is.na(test_file) || !nzchar(test_file)) {
    if (file.exists("DESCRIPTION")) normalizePath(".", mustWork = TRUE) else normalizePath("econimap", mustWork = TRUE)
  } else {
    dirname(dirname(test_file))
  }
  r_files <- sort(list.files(file.path(package_root, "R"), pattern = "[.]R$", full.names = TRUE))
  invisible(lapply(r_files, source))
  local_script_path <- function(file) file.path(package_root, "inst", "scripts", file)
}

stopifnot(file.exists(local_script_path("hier_mmm.stan")))
stopifnot(is.function(fit_hier_mmm))
stopifnot(is.function(prepare_stan_data_hier_mmm))
stopifnot(is.function(run_quasi_geo_test))
stopifnot(is.function(run_optimizer_scenario_planner))
stopifnot(is.function(run_mmm_deck_output_builder))
stopifnot(is.function(create_bau_response_curves))
stopifnot(is.function(fit_parsimonious_total_media_root))
stopifnot(is.function(build_sequential_effectiveness_priors))
stopifnot(is.function(run_sequential_hierarchical_bayes))

if (installed_mode) {
  stopifnot(is.function(econimap_get_function("run_quasi_geo_test")))
  stopifnot(is.function(econimap_get_function("fit_hier_mmm")))
  stopifnot(is.function(econimap_get_function("run_optimizer_scenario_planner")))
}

qgt_dates <- seq.Date(as.Date("2024-01-01"), by = "week", length.out = 20)
qgt_smoke_data <- do.call(rbind, lapply(c("geo_a", "geo_b"), function(g) {
  data.frame(
    period = qgt_dates,
    geo = g,
    kpi = 100 + seq_along(qgt_dates),
    tv = 10,
    tv_spend = 100
  )
}))
qgt_smoke <- run_quasi_geo_test(
  input_data = qgt_smoke_data,
  date_col = "period",
  dep_var_col = "kpi",
  geo_col = "geo",
  variable_map = data.frame(
    variable = "tv",
    modeled_x_col = "tv",
    spend_col = "tv_spend"
  ),
  normalize = "none",
  pre_weeks = 4,
  post_weeks = 2,
  rolling_window = 4,
  min_donors = 1,
  output_dir = NULL
)
stopifnot(all(c("candidate_events", "event_estimates_all", "dose_response_summary_all") %in% names(qgt_smoke)))
stopifnot(nrow(qgt_smoke$candidate_events) == 0)

curve_table <- data.frame(
  driver = rep(c("tv", "search"), each = 3),
  spend_multiplier = rep(c(0, 1, 2), times = 2),
  current_spend = rep(c(100, 50), each = 3),
  contribution = c(0, 20, 32, 0, 15, 23)
)
driver_constraints <- data.frame(
  driver = c("tv", "search"),
  min_multiplier = c(0.5, 0.5),
  max_multiplier = c(2, 2)
)
opt_smoke <- run_optimizer_scenario_planner(
  response_curve_table = curve_table,
  drivers = c("tv", "search"),
  driver_constraints = driver_constraints,
  optimizer_method = "hybrid",
  total_budget = 180,
  optimization_grid_step = 0.5,
  max_grid_combinations = 1000,
  scenario_multipliers = c(1, 1.2),
  uncertainty = "none"
)
stopifnot("package_info" %in% names(opt_smoke))
stopifnot(opt_smoke$package_info$workflow[1] == "run_optimizer_scenario_planner")
stopifnot(nrow(opt_smoke$current_plan) == 2)
stopifnot("driver" %in% names(opt_smoke$current_plan))
stopifnot("driver" %in% names(opt_smoke$optimization_plan))
stopifnot("input_alias_audit" %in% names(opt_smoke))
stopifnot(any(opt_smoke$input_alias_audit$alias_used))

bau_smoke <- create_bau_response_curves(
  data = data.frame(period = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 12), tv_support = c(rep(10, 6), rep(20, 6)), tv_spend = c(rep(100, 6), rep(200, 6))),
  variable_map = data.frame(variable = "tv", support_col = "tv_support", spend_col = "tv_spend", current_contribution = 50),
  date_col = "period",
  multiplier_grid = c(0, 1, 2),
  estimate_rrate = FALSE
)
stopifnot("package_info" %in% names(bau_smoke))
stopifnot(bau_smoke$package_info$workflow[1] == "create_bau_response_curves")
stopifnot(nrow(bau_smoke$response_curves) > 0)
stopifnot(all(c("curve_search_diagnostics", "curve_candidate_profile", "curve_export_table") %in% names(bau_smoke)))
stopifnot(bau_smoke$curve_search_diagnostics$curve_search_status[1] == "anchored_no_kpi")
stopifnot(all(c("response_index_q05", "response_index_q50", "response_index_q95") %in% names(bau_smoke$response_curves)))
stopifnot(all(c("median_to_mean_support", "active_median_support", "active_mean_support", "half_saturation_support", "saturation_at_mean_support", "marginal_response_at_mean_support", "curve_quality_flag") %in% names(bau_smoke$curve_metadata)))
stopifnot(all(c("group", "variable", "adstock_decay", "cvalue", "dvalue", "anchor_saturation", "anchor_x", "active_median_support", "active_mean_support", "half_saturation_support", "saturation_at_mean_support", "curve_quality_flag") %in% names(bau_smoke$curve_export_table)))
