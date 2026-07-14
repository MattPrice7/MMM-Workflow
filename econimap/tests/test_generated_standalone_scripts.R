test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
candidate_roots <- unique(c(
  if (!is.na(test_file) && file.exists(test_file)) dirname(dirname(test_file)) else character(),
  getwd(), file.path(getwd(), ".."), Sys.getenv("R_PACKAGE_DIR")
))
source_root <- candidate_roots[vapply(candidate_roots, function(path) {
  file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))
}, logical(1))]
root_dir <- if (length(source_root)) source_root[1] else getwd()
if (!requireNamespace("data.table", quietly = TRUE)) stop("Standalone checks require data.table.")

if (length(source_root)) {
  source(file.path(root_dir, "tools", "generate_standalone_scripts.R"))
  status <- generate_econimap_standalone_scripts(root_dir, check = TRUE)
  stopifnot(all(status$status == "current"))
  script_dir <- file.path(root_dir, "inst", "scripts")
} else {
  library(econimap)
  script_dir <- econimap_script_dir()
}

expected_functions <- list(
  "hier_mmm.R" = "fit_hier_mmm",
  "quasi_geo_test.R" = "run_quasi_geo_test",
  "optimizer_scenario_planner.R" = "run_optimizer_scenario_planner",
  "mmm_deck_output_builder.R" = "run_mmm_deck_output_builder",
  "bau_response_curves.R" = "create_bau_response_curves",
  "sequential_hierarchical_bayes.R" = "run_sequential_hierarchical_bayes"
)
for (script in names(expected_functions)) {
  env <- new.env(parent = baseenv())
  sys.source(file.path(script_dir, script), envir = env)
  stopifnot(is.function(get(expected_functions[[script]], envir = env, inherits = FALSE)))
}

cat("Generated standalone scripts are current and source cleanly.\n")
