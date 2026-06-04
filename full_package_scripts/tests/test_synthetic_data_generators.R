#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

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
source(file.path(bundle_dir, "synthetic_mmm_data_generators.R"), chdir = TRUE)

results <- data.table(test = character(), status = character(), detail = character())
add_result <- function(test, ok, detail = "") {
  results <<- rbind(results, data.table(test = test, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = as.character(detail)))
  if (!isTRUE(ok)) stop("FAILED: ", test, if (nzchar(detail)) paste0(" -- ", detail) else "")
}

panel <- make_synthetic_mmm_panel(seed = 1, n_weeks = 40, geos = paste0("G", 1:4), curve_type = "hill")
add_result("synthetic MMM panel returns required objects",
           all(c("data", "metadata", "truth", "spend_map", "variable_map", "channel_map") %in% names(panel)) &&
             is.data.table(panel$data) && is.data.table(panel$metadata) && is.data.table(panel$truth))
add_result("synthetic MMM panel has known-truth curves and holdout",
           all(c("anchor_saturation", "curve_rate", "coef", "adstock_rate") %in% names(panel$truth)) &&
             any(panel$data$is_holdout) &&
             panel$metadata[variable == "tv", curve_type][1] == "hill")
add_result("synthetic MMM panel has spend/support columns for media",
           all(panel$spend_map$spend_col %in% names(panel$data)) &&
             all(panel$variable_map$modeled_x_col %in% names(panel$data)))

qgt <- make_synthetic_quasi_geo_panel(seed = 2, event_type = "bundle")
add_result("synthetic quasi-geo panel returns event truth",
           all(c("data", "variable_map", "truth") %in% names(qgt)) &&
             qgt$truth$event_type[1] == "bundle" &&
             all(c("tv", "search", "social") %in% qgt$variable_map$variable))
add_result("synthetic quasi-geo bundle contains treated media movement",
           qgt$data[geo == qgt$truth$treated_geo[1] & week >= qgt$truth$event_start[1] & week <= qgt$truth$event_end[1],
                    sum(incremental_tv_true, na.rm = TRUE)] > 0 &&
             qgt$data[geo == qgt$truth$treated_geo[1] & week >= qgt$truth$event_start[1] & week <= qgt$truth$event_end[1],
                      sum(incremental_search_true, na.rm = TRUE)] > 0)

decomp <- make_synthetic_decomp_outputs(seed = 3, n_periods = 12)
add_result("synthetic decomposition outputs reconcile",
           all(c("long_decomp", "wide_decomp", "raw_data") %in% names(decomp)) &&
             is.data.table(decomp$long_decomp) &&
             is.data.table(decomp$wide_decomp) &&
             all(c("y_actual", "pred", "residual") %in% names(decomp$long_decomp)))

dir.create(file.path(bundle_dir, "test_outputs"), showWarnings = FALSE)
fwrite(results, file.path(bundle_dir, "test_outputs", "synthetic_data_generators_results.csv"))
message("\nSynthetic data generator results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")
invisible(results)
