#!/usr/bin/env Rscript

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

run_test <- function(path) {
  message("\n== Running ", basename(path), " ==")
  system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(path)), stdout = TRUE, stderr = TRUE)
}

tests <- file.path(test_dir, c(
  "test_p0_reproducibility.R",
  "test_core_script_standalone_contract.R",
  "test_synthetic_data_generators.R",
  "test_transform_consistency.R",
  "test_hier_mmm_stan_contract.R",
  "test_quasi_geo_evidence_classes.R",
  "test_quasi_geo_to_stan_handoff.R",
  "test_prior_deck_hardening.R",
  "test_bau_response_curves.R",
  "test_optimizer_scenario_planner.R"
))

if (tolower(Sys.getenv("RUN_QUASI_GEO_TESTS", "true")) %in% c("true", "1", "yes")) {
  root_qgt <- file.path(bundle_dir, "test_quasi_geo_test.R")
  if (file.exists(root_qgt)) tests <- c(tests, root_qgt)
}

if (tolower(Sys.getenv("RUN_CORE_SYNTHETIC_TESTS", "false")) %in% c("true", "1", "yes")) {
  root_core <- file.path(bundle_dir, "test_prior_and_diagnostic_workflow.R")
  if (file.exists(root_core)) tests <- c(tests, root_core)
}

if (tolower(Sys.getenv("RUN_HOSTILE_TESTS", "false")) %in% c("true", "1", "yes")) {
  root_hostile <- file.path(bundle_dir, "test_hostile_mmm_scenarios.R")
  if (file.exists(root_hostile)) tests <- c(tests, root_hostile)
}

if (tolower(Sys.getenv("RUN_STAN_SMOKE_TESTS", "false")) %in% c("true", "1", "yes")) {
  stan_context <- file.path(test_dir, "test_hier_mmm_context_effects_sampling.R")
  if (file.exists(stan_context)) tests <- c(tests, stan_context)
  stan_hostile <- file.path(test_dir, "test_hier_mmm_hostile_sampling.R")
  if (file.exists(stan_hostile)) tests <- c(tests, stan_hostile)
  root_stan <- file.path(bundle_dir, "test_geo_sales_national_media_mean_indexing.R")
  if (file.exists(root_stan)) tests <- c(tests, root_stan)
}

if (tolower(Sys.getenv("RUN_DEEP_WORKFLOW_HARDENING", "false")) %in% c("true", "1", "yes")) {
  deep_workflow <- file.path(test_dir, "test_deep_workflow_hardening.R")
  if (file.exists(deep_workflow)) tests <- c(tests, deep_workflow)
}

failures <- character()
for (tt in tests) {
  out <- run_test(tt)
  status <- attr(out, "status")
  cat(paste(out, collapse = "\n"), "\n")
  if (!is.null(status) && !identical(status, 0L)) failures <- c(failures, basename(tt))
}

if (length(failures)) {
  stop("Bundle tests failed: ", paste(failures, collapse = ", "))
}
message("\nAll selected bundle tests passed.")
