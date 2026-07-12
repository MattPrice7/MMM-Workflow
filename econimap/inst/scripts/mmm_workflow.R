# mmm_workflow.R
#
# Main source file for the full MMM workflow bundle.
# Source this file first in new analyst workbooks; legacy script names remain valid.

.mmm_workflow_source_file <- tryCatch({
  frames <- sys.frames()
  ofiles <- vapply(frames, function(f) {
    if (!is.null(f$ofile)) as.character(f$ofile)[1] else NA_character_
  }, character(1))
  ofiles <- ofiles[!is.na(ofiles) & nzchar(ofiles)]
  if (length(ofiles)) {
    candidate <- ofiles[length(ofiles)]
    if (!file.exists(candidate) && file.exists(basename(candidate))) candidate <- basename(candidate)
    normalizePath(candidate, mustWork = FALSE)
  } else {
    file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(file_arg)) normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE) else NA_character_
  }
}, error = function(e) NA_character_)
.mmm_workflow_base_dir <- if (is.finite(nchar(.mmm_workflow_source_file)) && nzchar(.mmm_workflow_source_file)) {
  dirname(.mmm_workflow_source_file)
} else {
  getwd()
}

mmm_source_bundle_file <- function(file, local = globalenv()) {
  path <- file.path(.mmm_workflow_base_dir, file)
  if (!file.exists(path)) stop("Bundle file not found: ", path)
  source(path, local = local, chdir = TRUE)
  invisible(TRUE)
}

mmm_source_bundle_file("marketing_mix_diagnostic_builder_production_final.R")
mmm_source_bundle_file("prior_recovery_builder.R")
mmm_source_bundle_file("mmm_prior_workflow.R")
mmm_source_bundle_file("mmm_deck_output_builder.R")
mmm_source_bundle_file("quasi_experimental_dose_response_analysis.R")
mmm_source_bundle_file("quasi_geo_test.R")
mmm_source_bundle_file("hier_mmm.R")
mmm_source_bundle_file("bau_response_curves.R")
mmm_source_bundle_file("optimizer_scenario_planner.R")

run_mmm_workflow <- run_mmm_prior_workflow
build_mmm_priors <- prior_builder
build_mmm_prior_metadata <- make_hier_metadata_from_prior_output
run_mmm_quasi_geo_test <- run_quasi_geo_test
run_mmm_dose_response_analysis <- run_quasi_experimental_dose_response_analysis
run_mmm_reporting <- run_mmm_deck_output_builder
run_mmm_bau_response_curves <- create_bau_response_curves
run_mmm_optimizer_scenario_planner <- run_optimizer_scenario_planner
run_mmm_dma_population <- function(script_file = "pull_dma_population.R", quiet = TRUE, ...) {
  path <- file.path(.mmm_workflow_base_dir, script_file)
  if (!file.exists(path)) stop("DMA population script not found: ", path)
  env <- new.env(parent = globalenv())
  if (isTRUE(quiet)) {
    invisible(utils::capture.output(source(path, local = env)))
  } else {
    source(path, local = env)
  }
  pull_fun <- get0("pull_dma_population", envir = env, inherits = FALSE)
  if (!is.function(pull_fun)) stop("pull_dma_population() was not found after sourcing: ", path)
  pull_fun(...)
}

mmm_dependency_manifest <- function() {
  list(
    core_required = c("data.table"),
    modeling_required = c("cmdstanr", "posterior"),
    optional = c("readxl", "openxlsx", "ggplot2", "shiny", "plotly", "DT", "loo", "jsonlite"),
    notes = c(
      "Core scripts should source with data.table only.",
      "cmdstanr/posterior are required only when fitting or inspecting Stan models.",
      "Optional reporting and file-format packages are checked only by functions that need them."
    )
  )
}

mmm_required_packages <- function(include_optional = TRUE, include_stan = TRUE) {
  manifest <- mmm_dependency_manifest()
  core <- manifest$core_required
  optional <- if (isTRUE(include_optional)) manifest$optional else character()
  stan <- if (isTRUE(include_stan)) manifest$modeling_required else character()
  unique(c(core, optional, stan))
}
