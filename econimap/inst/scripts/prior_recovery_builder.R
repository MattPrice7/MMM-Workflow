# prior_recovery_builder.R
#
# Source file for the prior recovery module.
# The original implementation lives in semi_univariate_prior_builder_production_final.R
# for backward compatibility with earlier bundle scripts and saved notebooks.
# Source mmm_workflow.R when you want the full prior + geo model workflow.

prior_recovery_script_dir <- tryCatch({
  frames <- sys.frames()
  ofiles <- vapply(frames, function(f) if (!is.null(f$ofile)) as.character(f$ofile)[1] else NA_character_, character(1))
  ofiles <- ofiles[!is.na(ofiles) & nzchar(ofiles)]
  if (length(ofiles)) dirname(normalizePath(ofiles[length(ofiles)], mustWork = FALSE)) else getwd()
}, error = function(e) getwd())

source(file.path(prior_recovery_script_dir, "semi_univariate_prior_builder_production_final.R"))
build_prior_recovery_metadata <- make_hier_metadata_from_prior_output
