.econimap_script_order <- c(
  "mmm_workflow.R",
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

utils::globalVariables(c(
  ".", ":=", ".BY", ".I", ".N", ".SD", ".draw",
  "__mdo_entity_key", "__mdo_group_key", "__mdo_sample_key", "__mdo_time_key",
  "..donor_cols", "..econ_rollup_cols", "..extra_control_cols", "..id_cols",
  "..join_cols", "..keep", "..keep_cols", "..model_var_cols", "..num_cols",
  "..placebo_donors", "..vars", "..wide_keep"
))

econimap_script_dir <- function(must_work = TRUE) {
  script_dir <- system.file("scripts", package = "econimap", mustWork = FALSE)
  if (nzchar(script_dir) && dir.exists(script_dir)) {
    return(normalizePath(script_dir, winslash = "/", mustWork = TRUE))
  }
  if (isTRUE(must_work)) {
    stop("Could not locate econimap bundled scripts. Reinstall the package.", call. = FALSE)
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

econimap_stan_path <- function(file = "hier_mmm.stan", must_work = TRUE) {
  path <- system.file("stan", file, package = "econimap", mustWork = FALSE)
  if (nzchar(path) && file.exists(path)) {
    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }
  if (isTRUE(must_work)) {
    stop(sprintf("Bundled econimap Stan file not found: %s", file), call. = FALSE)
  }
  character(0)
}

econimap_available_scripts <- function(pattern = NULL) {
  files <- list.files(econimap_script_dir(), full.names = FALSE)
  if (!is.null(pattern)) files <- grep(pattern, files, value = TRUE)
  sort(files)
}

load_econimap_scripts <- function(envir = globalenv(), include_workflow = TRUE, quiet = TRUE) {
  if (!is.environment(envir)) stop("`envir` must be an environment.", call. = FALSE)
  files <- .econimap_script_order
  if (!isTRUE(include_workflow)) files <- setdiff(files, "mmm_workflow.R")
  for (file in files) {
    path <- econimap_script_path(file)
    if (!isTRUE(quiet)) message("Sourcing ", path)
    source(path, local = envir, chdir = TRUE)
  }
  invisible(envir)
}

econimap_dependency_manifest <- function() {
  required <- c("data.table")
  modeling <- c("cmdstanr", "posterior")
  optional <- c("ggplot2", "readxl", "openxlsx", "shiny", "plotly", "DT", "loo", "jsonlite")
  data.frame(
    package = c(required, modeling, optional),
    role = c(
      rep("required_core", length(required)),
      rep("required_for_stan_modeling", length(modeling)),
      rep("optional", length(optional))
    ),
    installed = vapply(c(required, modeling, optional), requireNamespace, logical(1), quietly = TRUE),
    stringsAsFactors = FALSE
  )
}

econimap_package_version <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("econimap")),
    error = function(e) NA_character_
  )
  if (!is.na(version) && nzchar(version)) return(version)
  desc_path <- file.path(getwd(), "econimap", "DESCRIPTION")
  if (!file.exists(desc_path)) desc_path <- file.path(getwd(), "DESCRIPTION")
  if (file.exists(desc_path)) {
    desc <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
    if (!is.null(desc) && "Version" %in% colnames(desc)) return(as.character(desc[1, "Version"]))
  }
  NA_character_
}

econimap_output_metadata <- function(workflow,
                                     surface = NA_character_,
                                     status = "ready") {
  data.table::data.table(
    package = "econimap",
    package_version = econimap_package_version(),
    workflow = as.character(workflow)[1],
    surface = as.character(surface)[1],
    status = as.character(status)[1],
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
}

econimap_get_function <- function(name) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a single function name.", call. = FALSE)
  }
  ns <- asNamespace("econimap")
  if (!exists(name, envir = ns, mode = "function", inherits = TRUE)) {
    stop(sprintf("Function not found in econimap namespace: %s", name), call. = FALSE)
  }
  get(name, envir = ns, mode = "function", inherits = TRUE)
}

econimap_call <- function(name, ...) {
  fn <- econimap_get_function(name)
  fn(...)
}
