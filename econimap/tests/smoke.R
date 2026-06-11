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
