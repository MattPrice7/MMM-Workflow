# Mandatory Econimap release gate. This intentionally requires CmdStan and
# never converts missing dependencies into a successful skip.

release_root <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE))) else getwd()
})

required <- c("data.table", "cmdstanr", "posterior")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Release validation requires: ", paste(missing, collapse = ", "), call. = FALSE)
cmdstan_path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
if (!nzchar(cmdstan_path) || !dir.exists(cmdstan_path)) {
  stop("Release validation requires an installed CmdStan toolchain.", call. = FALSE)
}

source(file.path(release_root, "tools", "generate_standalone_scripts.R"))
generate_econimap_standalone_scripts(release_root, check = TRUE)
source(file.path(release_root, "tests", "test_sequential_hierarchical_bayes.R"))
source(file.path(release_root, "tests", "test_collective_saturation_handoff.R"))

old_env <- Sys.getenv(c(
  "ECONIMAP_RUN_SEQUENTIAL_STAN_VALIDATION",
  "ECONIMAP_SEQUENTIAL_VALIDATION_MINIMAL_SMOKE",
  "ECONIMAP_SEQUENTIAL_VALIDATION_REGIMES",
  "ECONIMAP_SEQUENTIAL_VALIDATION_SEEDS",
  "ECONIMAP_SEQUENTIAL_VALIDATION_ORACLE",
  "ECONIMAP_SEQUENTIAL_VALIDATION_REQUIRE_SAMPLER_VALID",
  "ECONIMAP_RUN_COLLECTIVE_SHAPE_STAN"
), unset = NA_character_)
on.exit({
  for (name in names(old_env)) {
    if (is.na(old_env[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, stats::setNames(list(old_env[[name]]), name))
  }
}, add = TRUE)

Sys.setenv(
  ECONIMAP_RUN_SEQUENTIAL_STAN_VALIDATION = "1",
  ECONIMAP_SEQUENTIAL_VALIDATION_MINIMAL_SMOKE = "1",
  ECONIMAP_SEQUENTIAL_VALIDATION_REGIMES = "clean_separated",
  ECONIMAP_SEQUENTIAL_VALIDATION_SEEDS = "1",
  ECONIMAP_SEQUENTIAL_VALIDATION_ORACLE = "0",
  ECONIMAP_SEQUENTIAL_VALIDATION_REQUIRE_SAMPLER_VALID = "1",
  ECONIMAP_RUN_COLLECTIVE_SHAPE_STAN = "1"
)

# This one-seed gate fits the fair direct generic, direct Meridian-equivalent,
# root-to-leaf, and root-to-depth-1-to-leaf paths. It is the mandatory focused
# recovery contract; the broad multi-regime/multi-seed suite remains a separate
# evidence-building run.
source(file.path(release_root, "tests", "test_sequential_hierarchical_bayes_stan_validation.R"))
source(file.path(release_root, "tests", "test_collective_saturation_stan_recovery.R"))

cat("Econimap focused release validation passed.\n")
