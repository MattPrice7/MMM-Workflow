.econimap_runtime_env <- new.env(parent = globalenv())
.econimap_state <- new.env(parent = emptyenv())
.econimap_state$runtime_loaded <- FALSE

.econimap_script_order <- c(
  "marketing_mix_diagnostic_builder_production_final.R",
  "semi_univariate_prior_builder_production_final.R",
  "prior_recovery_builder.R",
  "mmm_prior_workflow.R",
  "mmm_deck_output_builder.R",
  "quasi_experimental_dose_response_analysis.R",
  "quasi_geo_test.R",
  "hier_mmm.R",
  "bau_response_curves.R",
  "optimizer_scenario_planner.R",
  "synthetic_mmm_data_generators.R",
  "pull_dma_population.R"
)

econimap_script_dir <- function(must_work = TRUE) {
  script_dir <- system.file("scripts", package = "econimap", mustWork = FALSE)
  if (nzchar(script_dir) && dir.exists(script_dir)) return(normalizePath(script_dir, winslash = "/", mustWork = TRUE))

  local_dir <- file.path(dirname(dirname(normalizePath(getwd(), winslash = "/", mustWork = FALSE))), "inst", "scripts")
  if (dir.exists(local_dir)) return(normalizePath(local_dir, winslash = "/", mustWork = TRUE))

  if (isTRUE(must_work)) {
    stop("Could not locate econimap bundled scripts. Reinstall the package or use econimap_script_path(..., must_work = FALSE).", call. = FALSE)
  }
  character(0)
}

econimap_script_path <- function(file = NULL, must_work = TRUE) {
  script_dir <- econimap_script_dir(must_work = must_work)
  if (!length(script_dir)) return(character(0))
  if (is.null(file) || !nzchar(file)) return(script_dir)
  path <- file.path(script_dir, file)
  if (isTRUE(must_work) && !file.exists(path)) {
    stop(sprintf("Bundled econimap script not found: %s", file), call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = isTRUE(must_work))
}

econimap_available_scripts <- function(pattern = NULL) {
  files <- list.files(econimap_script_dir(), full.names = FALSE)
  if (!is.null(pattern)) files <- grep(pattern, files, value = TRUE)
  sort(files)
}

.econimap_source_one <- function(file, envir, quiet = TRUE) {
  path <- econimap_script_path(file)
  if (!isTRUE(quiet)) message("Sourcing ", path)
  sys.source(path, envir = envir, chdir = TRUE)
  invisible(path)
}

.econimap_attach_aliases <- function(envir) {
  if (!exists("run_mmm_workflow", envir = envir, inherits = FALSE) &&
      exists("run_mmm_prior_workflow", envir = envir, inherits = FALSE)) {
    assign("run_mmm_workflow", get("run_mmm_prior_workflow", envir = envir, inherits = FALSE), envir = envir)
  }
  invisible(TRUE)
}

.econimap_source_core <- function(envir, include_workflow = FALSE, quiet = TRUE) {
  files <- .econimap_script_order
  if (isTRUE(include_workflow)) files <- c("mmm_workflow.R", files)

  for (file in files) {
    .econimap_source_one(file, envir = envir, quiet = quiet)
  }
  .econimap_attach_aliases(envir)
  invisible(files)
}

load_econimap_scripts <- function(envir = globalenv(), include_workflow = FALSE, quiet = TRUE) {
  if (!is.environment(envir)) stop("`envir` must be an environment.", call. = FALSE)
  .econimap_source_core(envir = envir, include_workflow = include_workflow, quiet = quiet)
  invisible(envir)
}

.econimap_ensure_loaded <- function() {
  if (isTRUE(.econimap_state$runtime_loaded)) return(invisible(.econimap_runtime_env))
  .econimap_source_core(envir = .econimap_runtime_env, include_workflow = FALSE, quiet = TRUE)
  .econimap_state$runtime_loaded <- TRUE
  invisible(.econimap_runtime_env)
}

econimap_get_function <- function(name) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a single function name.", call. = FALSE)
  }
  .econimap_ensure_loaded()
  if (!exists(name, envir = .econimap_runtime_env, mode = "function", inherits = TRUE)) {
    stop(sprintf("Function not found in econimap runtime scripts: %s", name), call. = FALSE)
  }
  get(name, envir = .econimap_runtime_env, mode = "function", inherits = TRUE)
}

econimap_call <- function(name, ...) {
  fn <- econimap_get_function(name)
  fn(...)
}

fit_hier_mmm <- function(...) econimap_call("fit_hier_mmm", ...)

prepare_stan_data_hier_mmm <- function(...) econimap_call("prepare_stan_data_hier_mmm", ...)

variable_contribution_sum_hier_mmm <- function(...) econimap_call("variable_contribution_sum_hier_mmm", ...)

build_roi_mroi_hier_mmm <- function(...) econimap_call("build_roi_mroi_hier_mmm", ...)

run_hier_mmm_decisioning <- function(...) econimap_call("run_hier_mmm_decisioning", ...)

run_quasi_geo_test <- function(...) econimap_call("run_quasi_geo_test", ...)

run_optimizer_scenario_planner <- function(...) econimap_call("run_optimizer_scenario_planner", ...)

run_mmm_optimizer <- function(...) econimap_call("run_mmm_optimizer", ...)

run_mmm_scenario_planner <- function(...) econimap_call("run_mmm_scenario_planner", ...)

run_mmm_deck_output_builder <- function(...) econimap_call("run_mmm_deck_output_builder", ...)

build_mmm_deck_tables <- function(...) econimap_call("build_mmm_deck_tables", ...)

create_bau_response_curves <- function(...) econimap_call("create_bau_response_curves", ...)

build_bau_response_curves <- function(...) econimap_call("build_bau_response_curves", ...)

create_baseline_response_curves <- function(...) econimap_call("create_baseline_response_curves", ...)

econimap_dependency_manifest <- function() {
  required <- c("data.table")
  modeling <- c("cmdstanr", "posterior")
  optional <- c("ggplot2", "readxl", "openxlsx", "shiny", "plotly", "DT", "loo", "jsonlite")
  data.frame(
    package = c(required, modeling, optional),
    role = c(rep("required_core", length(required)), rep("required_for_stan_modeling", length(modeling)), rep("optional", length(optional))),
    installed = vapply(c(required, modeling, optional), requireNamespace, logical(1), quietly = TRUE),
    stringsAsFactors = FALSE
  )
}
