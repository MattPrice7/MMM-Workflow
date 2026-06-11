# run_full_mmm_bundle_validation.R
# Source this file in RStudio from the rolling MMM scripts folder.
# Defaults are CI-safe: no package installs and no CmdStan install.

INSTALL_MISSING_PACKAGES <- FALSE
RUN_SOURCE_AND_TARGETED_TESTS <- TRUE
RUN_DEEP_WORKFLOW_HARDENING <- TRUE
RUN_STAN_COMPILE_TEST <- FALSE
RUN_STAN_SAMPLING_TEST <- FALSE

get_this_script_dir <- function() {
  frames <- sys.frames()
  ofiles <- vapply(frames, function(f) {
    if (!is.null(f$ofile)) as.character(f$ofile)[1] else NA_character_
  }, character(1))
  ofiles <- ofiles[!is.na(ofiles) & nzchar(ofiles)]
  if (length(ofiles)) return(dirname(normalizePath(ofiles[length(ofiles)], mustWork = FALSE)))
  getwd()
}

BUNDLE_DIR <- get_this_script_dir()
setwd(BUNDLE_DIR)
message("Working directory: ", normalizePath(BUNDLE_DIR, mustWork = FALSE))

required_files <- c(
  "mmm_workflow.R",
  "hier_mmm.stan",
  "tests/run_bundle_tests.R",
  "tests/test_deep_workflow_hardening.R"
)
missing_files <- required_files[!file.exists(file.path(BUNDLE_DIR, required_files))]
if (length(missing_files)) {
  stop("Missing required bundle files: ", paste(missing_files, collapse = ", "))
}

options(repos = c(CRAN = "https://cloud.r-project.org"))
install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) return(invisible(TRUE))
  if (!isTRUE(INSTALL_MISSING_PACKAGES)) {
    stop("Missing packages: ", paste(missing, collapse = ", "),
         ". Set INSTALL_MISSING_PACKAGES <- TRUE if you want this script to install them.")
  }
  install.packages(missing)
  invisible(TRUE)
}

install_if_missing("data.table")
if (isTRUE(RUN_STAN_COMPILE_TEST)) {
  install_if_missing(c("cmdstanr", "posterior"))
} else if (isTRUE(RUN_STAN_SAMPLING_TEST)) {
  install_if_missing(c("cmdstanr", "posterior"))
}

run_rscript <- function(path, env = character()) {
  out <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(path)),
    stdout = TRUE,
    stderr = TRUE,
    env = env
  )
  cat(paste(out, collapse = "\n"), "\n")
  status <- attr(out, "status")
  if (!is.null(status) && !identical(status, 0L)) stop("Script failed: ", path)
  invisible(TRUE)
}

if (isTRUE(RUN_SOURCE_AND_TARGETED_TESTS)) {
  message("\nRunning source and targeted bundle tests...")
  run_rscript(
    file.path(BUNDLE_DIR, "tests", "run_bundle_tests.R"),
    env = c(
      paste0("RUN_DEEP_WORKFLOW_HARDENING=", tolower(as.character(RUN_DEEP_WORKFLOW_HARDENING))),
      paste0("RUN_STAN_TESTS=", tolower(as.character(RUN_STAN_COMPILE_TEST))),
      paste0("RUN_STAN_SMOKE_TESTS=", tolower(as.character(RUN_STAN_SAMPLING_TEST)))
    )
  )
}

if (isTRUE(RUN_STAN_COMPILE_TEST)) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) stop("Package 'cmdstanr' is not installed.")
  message("\nCompiling Stan model...")
  mod <- cmdstanr::cmdstan_model(file.path(BUNDLE_DIR, "hier_mmm.stan"), force_recompile = FALSE)
  stopifnot(inherits(mod, "CmdStanModel"))
  message("Stan compile test passed.")
}

message("\nFull MMM bundle validation complete.")
