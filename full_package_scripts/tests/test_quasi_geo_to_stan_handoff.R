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
source(file.path(bundle_dir, "quasi_geo_test.R"), chdir = TRUE)
source(file.path(bundle_dir, "hier_mmm.R"), chdir = TRUE)

results <- data.table(test = character(), status = character(), detail = character())
add_result <- function(test, ok, detail = "") {
  results <<- rbind(results, data.table(
    test = test,
    status = if (isTRUE(ok)) "PASS" else "FAIL",
    detail = as.character(detail)
  ), use.names = TRUE)
  if (!isTRUE(ok)) stop("FAILED: ", test, if (nzchar(detail)) paste0(" -- ", detail) else "")
  invisible(TRUE)
}

make_panel <- function(n = 72L, geos = paste0("G", 1:7)) {
  weeks <- seq.Date(as.Date("2024-01-07"), by = "week", length.out = n)
  dt <- CJ(week = weeks, geo = geos)
  dt[, entity := "brand"]
  dt[, idx := match(week, weeks)]
  dt[, gidx := match(geo, geos)]
  dt[, season := sin(idx * 2 * pi / 52)]
  dt[, tv := pmax(80 + 3 * gidx + 5 * season, 1)]
  dt[, search := pmax(40 + 2 * gidx + 2 * season, 1)]
  dt[, tv_spend := tv * 10]
  dt[, search_spend := search * 6]
  dt[, y := 1000 + 20 * gidx + 25 * season]
  dt[]
}

vm <- data.table(
  variable = c("tv", "search"),
  modeled_x_col = c("tv", "search"),
  spend_col = c("tv_spend", "search_spend")
)

dt <- make_panel()
event_week <- sort(unique(dt$week))[34]
dt[geo == "G1" & week >= event_week & week < event_week + 28,
   `:=`(tv = tv + 55, tv_spend = tv_spend + 550, y = y + 80)]
dt[geo == "G2" & week >= event_week & week < event_week + 28,
   `:=`(search = search + 45, search_spend = search_spend + 270, y = y + 25)]

qgt <- run_quasi_geo_test(
  input_data = dt,
  date_col = "week",
  dep_var_col = "y",
  geo_col = "geo",
  variable_map = vm,
  normalize = "geo_mean_index",
  pre_weeks = 8,
  post_weeks = 4,
  rolling_window = 8,
  min_pct_change = 0.20,
  min_robust_z = 0.8,
  min_volume = 0.1,
  min_donors = 2,
  min_evidence_score_to_keep = 0
)

handoff <- qgt_build_stan_prior_handoff(qgt, min_evidence_score = 0)
add_result("quasi-geo produces Stan handoff rows",
           nrow(handoff) > 0 &&
             all(c("variable", "coef", "coef_precision", "quasi_geo_prior_source") %in% names(handoff)) &&
             all(handoff$coef > 0) &&
             all(handoff$coef_precision > 0))

qgt_bundle_like <- copy(qgt$prior_recommendations)
qgt_bundle_like[variable == "search", `:=`(
  coef_prior_mean = NA_real_,
  usable_positive_event_n = 0L,
  evidence_direction = "diagnostic_only"
)]
handoff_no_search <- qgt_build_stan_prior_handoff(qgt_bundle_like, min_evidence_score = 0)
add_result("diagnostic or bundle-confounded evidence stays out of Stan coef handoff",
           !"search" %in% handoff_no_search$variable)

base_metadata <- data.table(
  variable = c("tv", "search"),
  source_entity = "GLOBAL",
  role = "media",
  rrate = 0.20,
  rrate_precision = 16,
  cvalue = 0.80,
  cvalue_precision = 16,
  dvalue = 1,
  dvalue_precision = 100,
  coef = 0.01,
  coef_precision = 4,
  coef_bound = "pos",
  coef_hierarchy_scale = 1
)
md_qgt <- qgt_apply_stan_prior_handoff(base_metadata, handoff, overwrite_existing = TRUE)
add_result("quasi-geo handoff updates metadata explicitly",
           all(c("quasi_geo_prior_source", "quasi_geo_max_evidence_score") %in% names(md_qgt)) &&
             any(md_qgt$coef > base_metadata$coef[1]) &&
             all(md_qgt[variable %in% handoff$variable]$quasi_geo_prior_source == "quasi_geo_channel_specific_marginal_response"))

prep <- prepare_stan_data_hier_mmm(
  data = dt,
  metadata_input = md_qgt,
  dep_var_col = "y",
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  mean_index = TRUE,
  dep_mean_index_scope = "group",
  x_mean_index_scope = "global",
  intercept_type = "flat",
  normalize_curve_x = TRUE,
  sample_curve_parameters = "never",
  sample_coef_hierarchy = "auto",
  coef_hierarchy_auto_min_geo_variation_share = 0.01,
  holdout_last_n = 4L
)
beta0 <- initial_beta_from_stan_data(prep$stan_data)
tv_idx <- prep$variable_lookup[variable == "tv", variable_idx][1]
tv_handoff <- handoff[variable == "tv"][1]
add_result("Stan data contract consumes quasi-geo coefficient prior",
           nrow(tv_handoff) == 1L &&
             abs(beta0[1, tv_idx] - tv_handoff$coef) < 1e-8)
add_result("Stan prep preserves quasi-geo audit fields in metadata",
           "quasi_geo_prior_source" %in% names(prep$metadata) &&
             prep$metadata[variable == "tv", quasi_geo_prior_source][1] == "quasi_geo_channel_specific_marginal_response")
add_result("quasi-geo Stan handoff does not create curve priors by itself",
           prep$curve_priors[variable == "tv", use_observed_cvalue_prior][1] == 0L)

dir.create(file.path(bundle_dir, "test_outputs"), showWarnings = FALSE)
fwrite(results, file.path(bundle_dir, "test_outputs", "quasi_geo_to_stan_handoff_results.csv"))
message("\nQuasi-geo to Stan handoff results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")
invisible(results)
