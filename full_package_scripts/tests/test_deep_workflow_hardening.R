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
source(file.path(bundle_dir, "mmm_workflow.R"), chdir = TRUE)

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

adstock_vec <- function(x, rrate) {
  out <- numeric(length(x))
  x <- as.numeric(x)
  for (i in seq_along(x)) out[i] <- x[i] + if (i == 1L) 0 else rrate * out[i - 1L]
  out
}

sat_curve <- function(x, cvalue = 0.85, dvalue = 1) {
  x <- pmax(as.numeric(x), 0)
  x^dvalue / (x^dvalue + cvalue^dvalue)
}

make_panel <- function(seed = 20260526, n_weeks = 104L, geos = paste0("G", 1:8)) {
  set.seed(seed)
  weeks <- seq.Date(as.Date("2024-01-07"), by = "week", length.out = n_weeks)
  dt <- CJ(week = weeks, geo = geos)
  dt[, entity := "brand_a"]
  dt[, idx := match(week, weeks)]
  dt[, gidx := match(geo, geos)]
  dt[, season := sin(idx * 2 * pi / 52)]
  dt[, trend := idx / max(idx)]
  dt[, geo_effect := 20 * gidx]

  dt[, tv := pmax(0, 85 + 18 * season + 3.5 * gidx + stats::rnorm(.N, 0, 4))]
  dt[, search := pmax(0, 45 + 0.35 * tv + 5 * cos(idx / 6) + stats::rnorm(.N, 0, 3))]
  dt[, social := pmax(0, 40 + 10 * cos(idx / 8) + 2 * gidx + stats::rnorm(.N, 0, 4))]
  dt[, audio := pmax(0, 30 + 6 * sin(idx / 7))]

  shock_week <- weeks[54]
  bundle_week <- weeks[64]
  down_week <- weeks[74]
  national_week <- weeks[84]
  dt[geo == "G1" & week >= shock_week & week < shock_week + 28, tv := tv + 85]
  dt[geo == "G2" & week >= bundle_week & week < bundle_week + 28, `:=`(
    search = search + 55,
    social = social + 42
  )]
  dt[geo == "G3" & week >= down_week & week < down_week + 28, tv := pmax(tv - 55, 0)]
  dt[week >= national_week & week < national_week + 28, audio := audio + 70]

  dt[, tv_spend := tv * 11]
  dt[, search_spend := search * 7]
  dt[, social_spend := social * 5]
  dt[, audio_spend := audio * 4]

  dt[, tv_ad := adstock_vec(tv, 0.35), by = geo]
  dt[, search_ad := adstock_vec(search, 0.10), by = geo]
  dt[, social_ad := adstock_vec(social, 0.20), by = geo]
  dt[, audio_ad := adstock_vec(audio, 0.25), by = geo]
  for (cc in c("tv_ad", "search_ad", "social_ad", "audio_ad")) {
    idx_col <- sub("_ad$", "_idx", cc)
    dt[, (idx_col) := get(cc) / mean(get(cc), na.rm = TRUE), by = geo]
  }

  dt[, tv_contrib := 180 * sat_curve(tv_idx, 0.90)]
  dt[, search_contrib := 130 * sat_curve(search_idx, 0.70)]
  dt[, social_contrib := 80 * sat_curve(social_idx, 0.80)]
  dt[, audio_contrib := 45 * sat_curve(audio_idx, 0.85)]
  dt[, baseline_contrib := 700 + geo_effect + 30 * trend + 45 * season]
  dt[, pred := baseline_contrib + tv_contrib + search_contrib + social_contrib + audio_contrib]
  dt[, subscriptions := pred + stats::rnorm(.N, 0, 10)]
  dt[, is_holdout := week > weeks[n_weeks - 8L]]
  dt[]
}

dt <- make_panel()
media_vars <- c("tv", "search", "social", "audio")
vm <- data.table(
  variable = media_vars,
  modeled_x_col = media_vars,
  spend_col = paste0(media_vars, "_spend")
)

national <- dt[, .(
  subscriptions = sum(subscriptions),
  tv = sum(tv),
  search = sum(search),
  social = sum(social),
  audio = sum(audio),
  tv_spend = sum(tv_spend),
  search_spend = sum(search_spend),
  social_spend = sum(social_spend),
  audio_spend = sum(audio_spend),
  is_holdout = all(is_holdout)
), by = week]

workflow <- run_mmm_prior_workflow(
  input_data = national,
  date_col = "week",
  dep_var_col = "subscriptions",
  variable_map = vm,
  holdout_col = "is_holdout",
  prior_args = list(
    use_fourier = TRUE,
    use_holidays = FALSE,
    use_week_of_month = FALSE,
    estimate_cvalue_from_data = "auto",
    rrate_grid_n = 11L,
    cvalue_grid_n = 13L,
    cvalue_min_ramp_points = 4L,
    cvalue_min_ramp_share = 0.03,
    multivariate_coef_prior_mode = "auto",
    verbose = FALSE
  ),
  response_curve_multipliers = c(0, 0.5, 1, 1.5, 2)
)

add_result("prior workflow returns metadata for all variables",
           nrow(workflow$metadata) >= length(media_vars) && all(media_vars %in% workflow$metadata$variable))
add_result("prior workflow keeps holdout audit",
           nrow(workflow$holdout_audit) == 1L && workflow$holdout_audit$holdout_row_n > 0)
add_result("prior workflow returns response curves",
           is.data.table(workflow$response_curves) && nrow(workflow$response_curves) > 0)
add_result("prior audit carries defensibility actions",
           is.data.table(workflow$prior_audit) && "analyst_action" %in% names(workflow$prior_audit))

qgt <- run_quasi_geo_test(
  input_data = dt,
  date_col = "week",
  dep_var_col = "subscriptions",
  geo_col = "geo",
  variable_map = vm,
  holdout_col = "is_holdout",
  normalize = "geo_mean_index",
  pre_weeks = 8,
  post_weeks = 4,
  rolling_window = 8,
  min_pct_change = 0.20,
  min_robust_z = 0.5,
  min_volume = 0.1,
  min_donors = 3,
  other_media_contamination_pct = 0.20,
  min_evidence_score_to_keep = 0
)

tv_clean <- qgt$event_estimates_all[variable == "tv" & geo == "G1" & estimand_level == "channel"]
add_result("quasi geo finds clean hidden single-channel shock",
           nrow(tv_clean) > 0 && any(tv_clean$recommended_use %in% c("calibration", "directional_prior", "diagnostic_only")))
tv_down <- qgt$event_estimates_all[variable == "tv" & geo == "G3" & ramp_direction %in% c("down_ramp", "turn_off")]
add_result("quasi geo preserves signed down-ramp evidence",
           nrow(tv_down) > 0 && any(tv_down$incremental_media < 0, na.rm = TRUE))
bundle_rows <- qgt$event_estimates_all[estimand_level == "bundle" & grepl("search", moved_channels) & grepl("social", moved_channels)]
add_result("quasi geo identifies co-moving bundle shock",
           nrow(bundle_rows) > 0 && all(bundle_rows$channel_specific_usable == FALSE))
add_result("bundle evidence stays separate from variable priors",
           nrow(qgt$bundle_prior_recommendations) > 0 &&
             all(!is.finite(qgt$prior_recommendations[variable %in% c("search", "social")]$coef_prior_mean) |
                   qgt$prior_recommendations[variable %in% c("search", "social")]$usable_positive_event_n == 0L))
national_diag <- qgt$event_estimates_all[variable == "audio" & event_type == "national_repeated_media"]
add_result("national repeated media is diagnostic only",
           nrow(national_diag) == 1L &&
             national_diag$recommended_use == "diagnostic_only" &&
             national_diag$channel_specific_usable == FALSE)
add_result("quasi geo exposes industry-style diagnostics",
           all(c("pre_fit_quality", "placebo_strength", "mde_power", "donor_quality",
                 "donor_contamination", "other_media_contamination", "recommended_use",
                 "confidence_band") %in% names(qgt$event_estimates_all)))

long_decomp <- rbindlist(lapply(c("tv", "search", "social", "audio", "baseline"), function(v) {
  contrib_col <- paste0(v, "_contrib")
  if (v == "baseline") contrib_col <- "baseline_contrib"
  dt[is_holdout == FALSE, .(
    week,
    geo,
    entity,
    variable = v,
    contribution = get(contrib_col),
    y_actual = subscriptions,
    pred = pred,
    residual = subscriptions - pred
  )]
}), use.names = TRUE)

deck <- build_mmm_deck_tables(
  long_decomp = long_decomp,
  raw_data = dt[is_holdout == FALSE],
  spend_map = vm[, .(variable, spend_col)],
  media_variables = media_vars,
  time_col = "week",
  group_col = "geo",
  entity_col = "entity",
  period_granularity = "week"
)

add_result("deck builder creates KPI economics",
           is.data.table(deck$kpi_economics) && nrow(deck$kpi_economics[variable %in% media_vars]) == length(media_vars))
add_result("deck builder keeps raw spend on reporting domain",
           abs(deck$kpi_economics[variable == "tv"]$spend - dt[is_holdout == FALSE, sum(tv_spend)]) < 1e-6)
add_result("deck builder creates period change table",
           is.data.table(deck$period_kpi_change) && nrow(deck$period_kpi_change) > 0)

out_dir <- file.path(bundle_dir, "test_outputs")
dir.create(out_dir, showWarnings = FALSE)
fwrite(results, file.path(out_dir, "deep_workflow_hardening_results.csv"))
message("\nDeep workflow hardening results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")
invisible(results)
