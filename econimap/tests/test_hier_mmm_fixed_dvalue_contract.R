test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
candidate_roots <- unique(c(
  if (!is.na(test_file) && file.exists(test_file)) dirname(dirname(test_file)) else character(),
  getwd(), file.path(getwd(), "econimap"), file.path(getwd(), ".."), Sys.getenv("R_PACKAGE_DIR")
))
root_dir <- candidate_roots[vapply(candidate_roots, function(path) {
  file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))
}, logical(1))]
if (!requireNamespace("data.table", quietly = TRUE)) stop("Fixed dvalue contract test requires data.table.")
suppressPackageStartupMessages(library(data.table))
if (length(root_dir)) {
  root_dir <- root_dir[1]
  invisible(lapply(sort(list.files(file.path(root_dir, "R"), pattern = "[.]R$", full.names = TRUE)), source))
} else {
  library(econimap)
  list2env(as.list(asNamespace("econimap"), all.names = TRUE), envir = .GlobalEnv)
}

panel <- CJ(geo = c("a", "b"), period = as.Date("2025-01-06") + 7 * 0:7)[, `:=`(
  entity = "brand",
  spend = 20 + seq_len(.N),
  support = 200 + 5 * seq_len(.N),
  kpi = 100 + seq_len(.N)
)]
metadata <- data.table(
  variable = "support", role = "media", spend_col = "spend", curve_type = "hill",
  rrate = .2, rrate_precision = 4, anchor_saturation = .5,
  anchor_saturation_precision = 4, cvalue_from_anchor = TRUE,
  dvalue = 1, dvalue_precision = 25, coef = .05, coef_precision = 4,
  coef_bound = "pos"
)

fixed <- prepare_stan_data_hier_mmm(
  data = panel, metadata_input = metadata, dep_var_col = "kpi", group_col = "geo",
  time_col = "period", entity_col = "entity", sample_curve_parameters = "always",
  estimate_dvalue = FALSE, sample_coef_hierarchy = "never"
)
stopifnot(fixed$stan_data$J_curve_sampled == 1L)
stopifnot(fixed$stan_data$J_dvalue_sampled == 0L)
stopifnot(length(fixed$stan_data$dvalue_sampled_pos) == 0L)
fixed_init <- build_ucm_warm_start_init(fixed, chains = 1L, seed = 1L)[[1]]
stopifnot(length(fixed_init$dvalue_raw) == 0L)

estimated <- prepare_stan_data_hier_mmm(
  data = panel, metadata_input = metadata, dep_var_col = "kpi", group_col = "geo",
  time_col = "period", entity_col = "entity", sample_curve_parameters = "always",
  estimate_dvalue = TRUE, sample_coef_hierarchy = "never"
)
stopifnot(estimated$stan_data$J_dvalue_sampled == estimated$stan_data$J_curve_sampled)
stopifnot(identical(estimated$stan_data$dvalue_sampled_pos, estimated$stan_data$curve_sampled_pos))
estimated_init <- build_ucm_warm_start_init(estimated, chains = 1L, seed = 1L)[[1]]
stopifnot(length(estimated_init$dvalue_raw) == estimated$stan_data$J_dvalue_sampled)

cat("Fixed dvalue Stan contract tests passed.\n")
