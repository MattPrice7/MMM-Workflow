library(econimap)

stopifnot(file.exists(econimap_script_path("hier_mmm.stan")))
stopifnot(is.function(fit_hier_mmm))
stopifnot(is.function(prepare_stan_data_hier_mmm))
stopifnot(is.function(run_quasi_geo_test))
stopifnot(is.function(run_optimizer_scenario_planner))
stopifnot(is.function(run_mmm_deck_output_builder))
stopifnot(is.function(create_bau_response_curves))

stopifnot(is.function(econimap_get_function("run_quasi_geo_test")))
stopifnot(is.function(econimap_get_function("fit_hier_mmm")))
stopifnot(is.function(econimap_get_function("run_optimizer_scenario_planner")))

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
