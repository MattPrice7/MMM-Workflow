file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])
test_file <- if (length(file_arg) && nzchar(file_arg)) {
  normalizePath(file_arg, mustWork = FALSE)
} else tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
root_dir <- if (!is.na(test_file)) dirname(dirname(test_file)) else getwd()
if (!requireNamespace("data.table", quietly = TRUE)) stop("Positive geo-root test requires data.table.")
suppressPackageStartupMessages(library(data.table))
invisible(lapply(sort(list.files(file.path(root_dir, "R"), pattern = "[.]R$", full.names = TRUE)), source))

set.seed(602)
periods <- seq.Date(as.Date("2024-01-01"), by = "week", length.out = 72L)
geos <- paste0("geo_", seq_len(5L))
population <- c(0.8, 1.1, 1.7, 2.3, 3.2) * 1e6
effectiveness <- c(0.55, 0.65, 0.70, 0.75, 0.85)
panel <- rbindlist(lapply(seq_along(geos), function(ii) {
  n <- length(periods)
  spend <- population[ii] / 1e6 * pmax(10, 110 + 30 * sin(seq_len(n) / 5 + ii) + rnorm(n, 0, 13))
  macro <- sin(seq_len(n) / 12)
  data.table(
    period = periods,
    geo = geos[ii],
    entity = "brand",
    population = population[ii],
    paid_support = spend * 12,
    paid_spend = spend,
    macro = macro,
    kpi = population[ii] / 1e6 * (550 + 24 * macro) + effectiveness[ii] * spend + rnorm(n, 0, 5)
  )
}))
metadata <- data.table(
  variable = c("paid_support", "macro"),
  role = c("media", "control"),
  spend_col = c("paid_spend", NA_character_),
  rollup_path = c("total_paid_media > paid", "business_controls > macro"),
  coef = 0,
  coef_precision = 1,
  coef_bound = c("pos", "free")
)
fit <- fit_parsimonious_total_media_root(
  data = panel,
  metadata_input = metadata,
  dep_var_col = "kpi",
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  population_col = "population",
  root_scope = "hierarchical_panel",
  root_control_cols = "macro",
  root_pressure_scaling = "auto",
  root_media_transform = "linear",
  root_time_baseline = "knots",
  root_knot_n = 4L,
  root_geo_media_effect = "partially_pooled",
  root_bootstrap_reps = 0L
)
expected <- weighted.mean(effectiveness, population)
stopifnot(fit$root_summary$root_geo_media_effect_mode[1] == "partially_pooled_log_normal")
stopifnot(fit$root_summary$root_geo_media_effect_scale[1] == "log")
stopifnot(all(fit$root_geo_media_effects$root_media_beta > 0))
stopifnot(abs(fit$root_summary$root_effectiveness[1] - expected) < 0.15)
cat("Positive log-scale geo root test passed.\n")
