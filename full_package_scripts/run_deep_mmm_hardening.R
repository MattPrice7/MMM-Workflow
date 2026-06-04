# run_deep_mmm_hardening.R
# Source this file in RStudio from the rolling MMM scripts folder.
# It runs the heavier synthetic workflow checks without installing packages.

RUN_SOURCE_AND_TARGETED_TESTS <- TRUE
RUN_DEEP_WORKFLOW_HARDENING <- TRUE
RUN_STAN_TESTS <- FALSE

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
  "tests/run_bundle_tests.R",
  "tests/test_deep_workflow_hardening.R"
)
missing_files <- required_files[!file.exists(file.path(BUNDLE_DIR, required_files))]
if (length(missing_files)) {
  stop("Missing required files: ", paste(missing_files, collapse = ", "))
}

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required. Install it with install.packages('data.table').")
}

run_rscript <- function(path, env = character()) {
  cmd <- file.path(R.home("bin"), "Rscript")
  args <- c("--vanilla", shQuote(path))
  out <- system2(cmd, args, stdout = TRUE, stderr = TRUE, env = env)
  cat(paste(out, collapse = "\n"), "\n")
  status <- attr(out, "status")
  if (!is.null(status) && !identical(status, 0L)) {
    stop("Script failed: ", path)
  }
  invisible(TRUE)
}

if (isTRUE(RUN_SOURCE_AND_TARGETED_TESTS)) {
  message("\nRunning source and targeted tests...")
  run_rscript(
    file.path(BUNDLE_DIR, "tests", "run_bundle_tests.R"),
    env = c(
      "RUN_DEEP_WORKFLOW_HARDENING=false",
      paste0("RUN_STAN_TESTS=", tolower(as.character(RUN_STAN_TESTS))),
      paste0("RUN_STAN_SMOKE_TESTS=", tolower(as.character(RUN_STAN_TESTS)))
    )
  )
}

if (isTRUE(RUN_DEEP_WORKFLOW_HARDENING)) {
  message("\nRunning deep consultant-workflow hardening tests...")
  run_rscript(file.path(BUNDLE_DIR, "tests", "test_deep_workflow_hardening.R"))
}

message("\nDeep MMM hardening run complete.")
