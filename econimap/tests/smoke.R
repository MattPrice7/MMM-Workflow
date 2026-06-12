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
stopifnot(nrow(opt_smoke$current_plan) == 2)
stopifnot("driver" %in% names(opt_smoke$current_plan))
stopifnot("driver" %in% names(opt_smoke$optimization_plan))
stopifnot("input_alias_audit" %in% names(opt_smoke))
stopifnot(any(opt_smoke$input_alias_audit$alias_used))
