test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
candidate_roots <- unique(c(
  if (!is.na(test_file) && file.exists(test_file)) dirname(dirname(test_file)) else character(),
  getwd(), file.path(getwd(), "econimap"), file.path(getwd(), ".."), Sys.getenv("R_PACKAGE_DIR")
))
root_dir <- candidate_roots[vapply(candidate_roots, function(path) {
  file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))
}, logical(1))]
if (!requireNamespace("data.table", quietly = TRUE)) stop("Pressure scaling test requires data.table.")
suppressPackageStartupMessages(library(data.table))
if (length(root_dir)) {
  root_dir <- root_dir[1]
  invisible(lapply(sort(list.files(file.path(root_dir, "R"), pattern = "[.]R$", full.names = TRUE)), source))
} else {
  library(econimap)
  list2env(as.list(asNamespace("econimap"), all.names = TRUE), envir = .GlobalEnv)
}

panel <- CJ(
  geo = c("small", "large"),
  period = as.Date("2025-01-06") + 7 * 0:3
)[, `:=`(
  entity = "brand",
  population = fifelse(geo == "small", 100, 1000),
  support = 800 + 50 * (as.integer(period) - as.integer(min(period))),
  spend = 100,
  kpi = fifelse(geo == "small", 120, 1100)
)]
metadata <- data.table(
  variable = "support", role = "media", population_col = "population",
  spend_col = "spend", curve_type = "hill", rrate = .2, rrate_precision = 4,
  anchor_saturation = .5, anchor_saturation_precision = 4, cvalue_from_anchor = TRUE,
  dvalue = 1, dvalue_precision = 25, coef = .05, coef_precision = 4, coef_bound = "pos"
)

prep <- prepare_stan_data_hier_mmm(
  data = panel, metadata_input = metadata, dep_var_col = "kpi", group_col = "geo",
  time_col = "period", entity_col = "entity", mean_index = FALSE,
  sample_curve_parameters = "never", sample_coef_hierarchy = "never"
)
expected_pressure <- panel$support / panel$population
stopifnot(isTRUE(all.equal(as.numeric(prep$data$support), as.numeric(expected_pressure))))
audit <- as.data.table(prep$pressure_scaling_audit)
stopifnot(audit[variable == "support", pressure_scaling_applied])
stopifnot(audit[variable == "support", exposure_denominator_col] == "population")
stopifnot(all(is.finite(prep$data[[audit[variable == "support", raw_support_internal_col]]])))

metadata_none <- copy(metadata)[, exposure_scaling := "none"]
prep_none <- prepare_stan_data_hier_mmm(
  data = panel, metadata_input = metadata_none, dep_var_col = "kpi", group_col = "geo",
  time_col = "period", entity_col = "entity", mean_index = FALSE,
  sample_curve_parameters = "never", sample_coef_hierarchy = "never"
)
stopifnot(isTRUE(all.equal(as.numeric(prep_none$data$support), as.numeric(panel$support))))
stopifnot(!as.data.table(prep_none$pressure_scaling_audit)[variable == "support", pressure_scaling_applied])

bad_panel <- copy(panel)
bad_panel[geo == "large", population := 0]
bad <- try(prepare_stan_data_hier_mmm(
  data = bad_panel, metadata_input = metadata, dep_var_col = "kpi", group_col = "geo",
  time_col = "period", entity_col = "entity", mean_index = FALSE,
  sample_curve_parameters = "never", sample_coef_hierarchy = "never"
), silent = TRUE)
stopifnot(inherits(bad, "try-error"))
stopifnot(grepl("Exposure denominator 'population'", as.character(bad), fixed = TRUE))

cat("Hierarchical MMM exposure-pressure scaling tests passed.\n")
