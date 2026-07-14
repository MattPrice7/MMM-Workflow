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
  "sequential_hierarchical_bayes.R",
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
  "..placebo_donors", "..vars", "..wide_keep", "..child_variables", "..parent_node",
  "aggregate_child_response", "allowed", "anchor_saturation_prior_precision__",
  "adstock_child_noise_sd__", "adstock_tau_mean__", "adstock_tau_sd__",
  "base_prior_equivalent", "child_identification_score", "child_shape",
  "child_noise_sd", "child_noise_sd_logit", "curve_param_idx", "derivative__",
  "collective_sensitivity_status", "convergence", "draw", "fit_status",
  "i.anchor_saturation_prior_precision__", "i.child_identification_strength_0_1",
  "i.combination_mode", "i.combined_mean", "i.combined_precision",
  "i.measured_prior_dominance", "i.media_node", "i.mix_id", "i.parent_node",
  "i.parent_shape", "i.posterior_movement_prior_sd_units",
  "i.posterior_to_prior_sd_ratio", "i.shared_spend_group", "i.spend_bearing",
  "i.sufficient_mix_variation", "i.support_multiplier", "i.value__", "i.variable_idx",
  "i.variable_role_within_node", "index__", "label__",
  "measured_prior_dominance", "media_mix_churn", "media_node", "mix_id",
  "nonlinear_deviation", "nonlinear_deviation_q05", "nonlinear_deviation_q50",
  "nonlinear_deviation_q95", "nonlinear_deviation_share",
  "normalized_aggregate_shape_q05", "normalized_aggregate_shape_q50",
  "normalized_aggregate_shape_q95", "parent_draw", "parent_node",
  "parent_id", "parent_idx", "parent_mean", "parent_mean__", "parent_mu_logit",
  "parent_response_draw", "parent_sd", "parent_sd__", "parent_sd_logit",
  "parent_shape", "parent_shape_sd", "parent_support",
  "reconciliation_id", "reference_contribution", "root_half_saturation",
  "root_nonlinear_model_weight", "root_steepness", "saturation_evidence_class",
  "scenario_contribution", "sequential_parent_id", "sequential_saturation_prior_source",
  "shared_spend_group", "effect_tau_mean__", "effect_tau_sd__",
  "effect_aggregate_sd__", "effect_child_noise_sd__", "heterogeneity_sd__",
  "mix_sd__", "reference_spend", "tau_prior_mean", "tau_prior_mean_logit",
  "tau_prior_sd", "tau_prior_sd_logit", "value_n", "variable_idx",
  "spend_bearing", "spend_mechanically_allocated", "spend_national_layout",
  "spend_scope", "sufficient_mix_variation", "support_mechanically_allocated",
  "support_multiplier", "support_scope", "time_key__", "time_value",
  "variable_role_within_node", "x.anchor_saturation", "x.anchor_saturation_precision",
  "x.rrate", "x.rrate_precision"
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
