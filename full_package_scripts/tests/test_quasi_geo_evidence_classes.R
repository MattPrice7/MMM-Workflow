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

results <- data.table(test = character(), status = character(), detail = character())
add_result <- function(test, ok, detail = "") {
  results <<- rbind(results, data.table(test = test, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = as.character(detail)))
  if (!isTRUE(ok)) stop("FAILED: ", test, if (nzchar(detail)) paste0(" -- ", detail) else "")
}

make_panel <- function(n = 72, geos = paste0("G", 1:7)) {
  weeks <- seq.Date(as.Date("2024-01-07"), by = "week", length.out = n)
  dt <- CJ(week = weeks, geo = geos)
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

vm_two <- data.table(
  variable = c("tv", "search"),
  modeled_x_col = c("tv", "search"),
  spend_col = c("tv_spend", "search_spend"),
  channel = c("TV", "Search"),
  rollup_path = c("Media/TV", "Media/Search")
)

vm_blank_spend <- data.table(variable = "tv", modeled_x_col = "tv", spend_col = "")
blank_spend <- run_quasi_geo_test(make_panel(), "week", "y", "geo", variable_map = vm_blank_spend,
                                  normalize = "none", pre_weeks = 8, post_weeks = 4,
                                  rolling_window = 8, min_pct_change = 0.2, min_robust_z = 0,
                                  min_volume = 0.1, min_donors = 2)
add_result("blank spend_col defaults to modeled_x_col", is.list(blank_spend) && "candidate_events" %in% names(blank_spend))
bad_vm_error <- tryCatch({
  run_quasi_geo_test(make_panel(), "week", "y", "geo", variable_map = data.table(variable = "missing_media", modeled_x_col = "", spend_col = ""),
                     normalize = "none")
  ""
}, error = function(e) conditionMessage(e))
add_result("unrecoverable modeled_x_col fails clearly", grepl("unrecoverable modeled_x_col", bad_vm_error, fixed = TRUE))
dup_vm_error <- tryCatch({
  run_quasi_geo_test(make_panel(), "week", "y", "geo", variable_map = data.table(variable = c("tv", "tv"), modeled_x_col = c("tv", "tv"), spend_col = c("tv_spend", "tv_spend")),
                     normalize = "none")
  ""
}, error = function(e) conditionMessage(e))
add_result("duplicate variable_map keys fail clearly", grepl("duplicate variable keys", dup_vm_error, fixed = TRUE))
bad_tier_error <- tryCatch({
  run_quasi_geo_test(make_panel(), "week", "y", "geo", variable_map = vm_two, min_evidence_tier_to_keep = "bad_tier")
  ""
}, error = function(e) conditionMessage(e))
add_result("unknown min_evidence_tier_to_keep errors clearly", grepl("Unknown min_evidence_tier_to_keep", bad_tier_error, fixed = TRUE))

set.seed(42)
ridge_y <- rnorm(5)
ridge_x <- matrix(rnorm(5 * 10), nrow = 5)
ridge_x[, 10] <- 1
ridge_w <- qgt_ridge_synth_weights(ridge_y, ridge_x, lambda_grid = c(0, 0.01, 0.1, 1))
add_result("ridge allows more donors than pre-period rows", length(ridge_w) == 10L && all(is.finite(ridge_w)) &&
             abs(sum(ridge_w) - 1) < 1e-8 && ridge_w[10] == 0)

dt_up <- make_panel()
event_week <- sort(unique(dt_up$week))[34]
dt_up[geo == "G1" & week >= event_week & week < event_week + 28, `:=`(tv = tv + 55, tv_spend = tv_spend + 550, y = y + 80)]
up <- run_quasi_geo_test(dt_up, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                         pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.2,
                         min_robust_z = 0.8, min_volume = 0.1, min_donors = 2)
up_tv <- up$event_estimates_all[variable == "tv" & is.finite(incremental_outcome)]
add_result("clean single-channel up-ramp is channel estimand", nrow(up_tv) > 0 && any(up_tv$estimand_level == "channel"))
add_result("clean single-channel up-ramp has usable prior/calibration class", any(up_tv$recommended_use %in% c("calibration", "directional_prior", "diagnostic_only")))
add_result("quasi geo returns analyst evidence summaries", nrow(up$evidence_summary) == 1L &&
             nrow(up$variable_evidence_summary) >= 1L &&
             nrow(up$estimand_evidence_summary) >= 1L &&
             all(c("recommended_analyst_action", "best_recommended_use", "max_evidence_score") %in% names(up$variable_evidence_summary)))
add_result("quasi geo carries rollup metadata for reporting",
           all(c("channel", "rollup_path", "rollup_root", "rollup_leaf") %in% names(up$event_estimates_all)) &&
             all(c("channel", "rollup_path") %in% names(up$prior_recommendations)) &&
             nrow(up$rollup_evidence_summary[rollup_root == "Media"]) >= 1L)
add_result("quasi geo returns donor placebo and leave-one-donor-out diagnostics", nrow(up_tv) > 0 &&
             all(c("donor_placebo_p_value", "donor_placebo_strength", "leave_one_donor_out_stability_score") %in% names(up_tv)) &&
             any(is.finite(up_tv$donor_placebo_p_value)) &&
             any(is.finite(up_tv$leave_one_donor_out_stability_score)))
tbr_dt <- make_panel(geos = paste0("G", 1:4))
tbr_week <- sort(unique(tbr_dt$week))[34]
tbr_dt[geo == "G1" & week >= tbr_week & week < tbr_week + 28, `:=`(tv = tv + 55, tv_spend = tv_spend + 550, y = y + 80)]
tbr_fallback <- run_quasi_geo_test(tbr_dt, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                                   pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.2,
                                   min_robust_z = 0.8, min_volume = 0.1, min_donors = 5)
tbr_row <- tbr_fallback$event_estimates_all[variable == "tv" & geo == "G1" & is.finite(incremental_outcome)][1]
add_result("TBR/DiD fallback is selected when donor support is weak", nrow(tbr_row) == 1L &&
             tbr_row$selected_counterfactual_method %in% c("time_based_regression", "difference_in_differences") &&
             tbr_row$counterfactual_method == tbr_row$selected_counterfactual_method &&
             grepl("few_donors|donor_contamination_or_shortage", tbr_row$counterfactual_fallback_reason))
add_result("TBR/DiD fallback downgrades synthetic failure instead of hard blocking", nrow(tbr_row) == 1L &&
             isTRUE(tbr_row$counterfactual_fallback_success) &&
             !grepl("synthetic_control_weights_failed", tbr_row$blocker_reasons, fixed = TRUE) &&
             tbr_row$recommended_use %in% c("calibration", "directional_prior", "diagnostic_only"))
raw_row <- up_tv[is.finite(cost_per_incremental_outcome) & is.finite(roi_like)][1]
add_result("raw-scale ROI and cost per outcome are arithmetic", nrow(raw_row) == 1L &&
             "incremental_outcome_raw" %in% names(raw_row) &&
             abs(raw_row$cost_per_incremental_outcome - raw_row$incremental_spend / raw_row$incremental_outcome_raw) < 1e-8 &&
             abs(raw_row$roi_like - raw_row$incremental_outcome_raw / raw_row$incremental_spend) < 1e-8)
add_result("geo mean-index keeps raw outcome for ROI scale", nrow(raw_row) == 1L &&
             is.finite(raw_row$incremental_outcome_raw) &&
             abs(raw_row$incremental_outcome_raw - raw_row$incremental_outcome) > 1e-6)

dt_missing_post <- copy(dt_up)
dt_missing_post[geo == "G2" & week == event_week, y := NA_real_]
missing_post <- run_quasi_geo_test(dt_missing_post, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                                   pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.2,
                                   min_robust_z = 0.8, min_volume = 0.1, min_donors = 2)
missing_post_tv <- missing_post$event_estimates_all[variable == "tv" & grepl("incomplete_post_synthetic_rows", diagnostic_reason)][1]
add_result("missing donor synthetic post rows are diagnosed and aligned", nrow(missing_post_tv) == 1L &&
             missing_post_tv$post_complete_row_n < missing_post_tv$post_outcome_row_n &&
             is.finite(missing_post_tv$incremental_media))

dt_raw_na <- copy(dt_up)
dt_raw_na[, raw_y := NA_real_]
ev_raw_na <- qgt_event_scan_one(dt_raw_na, "week", "geo", "tv", "tv", pre_weeks = 8, post_weeks = 4,
                                min_abs_change = 0, min_pct_change = 0.2, min_robust_z = 0.8,
                                min_volume = 0.1, media_cutoff_pct = 0.10, rolling_window = 8)[1]
raw_na_est <- qgt_estimate_event(dt_raw_na, ev_raw_na, "week", "y", raw_dep_col = "raw_y", "geo",
                                 media_col = "tv", spend_col = "tv_spend",
                                 all_media_cols = vm_two$modeled_x_col,
                                 all_spend_cols = stats::setNames(vm_two$spend_col, vm_two$modeled_x_col),
                                 pre_weeks = 8, post_weeks = 4, min_volume = 0.1, min_donors = 2)
add_result("all-NA raw synthetic lift stays NA", nrow(raw_na_est) == 1L &&
             !is.finite(raw_na_est$incremental_outcome_raw) &&
             !is.finite(raw_na_est$roi_like))

dt_down <- make_panel()
down_week <- sort(unique(dt_down$week))[34]
dt_down[geo == "G1" & week >= down_week & week < down_week + 28, `:=`(tv = tv - 45, tv_spend = tv_spend - 450, y = y - 70)]
down <- run_quasi_geo_test(dt_down, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                           pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.15,
                           min_robust_z = 0.8, min_volume = 0.1, min_donors = 2)
down_tv <- down$event_estimates_all[variable == "tv" & ramp_direction == "down_ramp" & is.finite(marginal_response)]
add_result("clean single-channel down-ramp supports positive media economics", nrow(down_tv) > 0 && median(down_tv$marginal_response, na.rm = TRUE) > 0)
add_result("down-ramp marginal response CI is ordered", nrow(down_tv) > 0 &&
             all(down_tv$marginal_response_ci_low <= down_tv$marginal_response_ci_high, na.rm = TRUE))

dt_turn_on <- make_panel()
turn_on_week <- sort(unique(dt_turn_on$week))[34]
dt_turn_on[geo == "G1" & week < turn_on_week, `:=`(tv = 0, tv_spend = 0)]
dt_turn_on[geo == "G1" & week >= turn_on_week & week < turn_on_week + 28, `:=`(tv = 65, tv_spend = 650, y = y + 75)]
turn_on <- run_quasi_geo_test(dt_turn_on, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                              pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.15,
                              min_robust_z = 0, min_volume = 0.1, min_donors = 2)
add_result("turn-on event is detected", nrow(turn_on$event_estimates_all[variable == "tv" & event_type == "turn_on"]) > 0)

dt_tiny_primary <- make_panel()
tiny_primary_week <- sort(unique(dt_tiny_primary$week))[34]
dt_tiny_primary[geo == "G1", `:=`(tv = 0, tv_spend = 0)]
dt_tiny_primary[geo == "G1" & week >= tiny_primary_week & week < tiny_primary_week + 28, `:=`(tv = 0.001, tv_spend = 0.01)]
tiny_primary <- run_quasi_geo_test(dt_tiny_primary, "week", "y", "geo", variable_map = vm_two, normalize = "none",
                                   pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_abs_change = 0,
                                   min_pct_change = 0.15, min_robust_z = 0, min_volume = 0.1, min_donors = 2)
add_result("tiny primary media movement does not create turn-on event", nrow(tiny_primary$candidate_events[variable == "tv" & event_type == "turn_on"]) == 0L)

dt_turn_off <- make_panel()
turn_off_week <- sort(unique(dt_turn_off$week))[34]
dt_turn_off[geo == "G1" & week < turn_off_week, `:=`(tv = 65, tv_spend = 650)]
dt_turn_off[geo == "G1" & week >= turn_off_week & week < turn_off_week + 28, `:=`(tv = 0, tv_spend = 0, y = y - 75)]
turn_off <- run_quasi_geo_test(dt_turn_off, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                               pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.15,
                               min_robust_z = 0, min_volume = 0.1, min_donors = 2)
add_result("turn-off event is detected", nrow(turn_off$event_estimates_all[variable == "tv" & event_type == "turn_off"]) > 0)

dt_bundle <- make_panel()
bundle_week <- sort(unique(dt_bundle$week))[34]
dt_bundle[geo == "G1" & week >= bundle_week & week < bundle_week + 28, `:=`(
  tv = tv + 45, tv_spend = tv_spend + 450,
  search = search + 25, search_spend = search_spend + 150,
  y = y + 100
)]
bundle <- run_quasi_geo_test(dt_bundle, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                             pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.15,
                             min_robust_z = 0.8, min_volume = 0.1, min_donors = 2,
                             other_media_contamination_pct = 0.15)
bundle_rows <- bundle$event_estimates_all[estimand_level == "bundle"]
add_result("multi-channel shock becomes bundle estimand", nrow(bundle_rows) > 0 && all(bundle_rows$channel_specific_usable == FALSE))
add_result("bundle shock returns bundle economics", nrow(bundle_rows[is.finite(bundle_incremental_outcome) & is.finite(bundle_incremental_spend) &
                                                                      abs(bundle_incremental_spend) > abs(incremental_spend)]) > 0)
add_result("moved channel list is clean channel names", nrow(bundle_rows) > 0 &&
             !any(grepl(":", bundle_rows$moved_channels, fixed = TRUE)))
add_result("bundle events stay out of channel coef priors", nrow(bundle$prior_recommendations) > 0 &&
             all(bundle$prior_recommendations$usable_positive_event_n == 0L | !is.finite(bundle$prior_recommendations$coef_prior_mean)) &&
             all(!is.finite(bundle$prior_recommendations$contribution_prior_mean)) &&
             all(!is.finite(bundle$prior_recommendations$roi_like_prior_mean)) &&
             all(!is.finite(bundle$prior_recommendations$cost_per_outcome_prior_mean)))
add_result("bundle events get bundle-level recommendations", nrow(bundle$bundle_prior_recommendations) > 0)
add_result("bundle evidence summary recommends bundle-only action", nrow(bundle$estimand_evidence_summary[estimand_level == "bundle"]) > 0 &&
             any(bundle$estimand_evidence_summary[estimand_level == "bundle"]$recommended_analyst_action %in% c("bundle_calibration_candidate", "bundle_or_campaign_prior_only", "diagnostic_review")))
add_result("two-channel bundle key is normalized once in recommendations", nrow(bundle$bundle_prior_recommendations) == uniqueN(bundle$bundle_prior_recommendations$bundle_name) &&
             all(!grepl("tv\\|search.*search\\|tv|search\\|tv.*tv\\|search", bundle$bundle_prior_recommendations$moved_channels)))
add_result("bundle shock does not create variable curve direction", nrow(bundle$dose_response_summary[variable %in% c("tv", "search") & !curve_prior_direction %in% c("inconclusive", "inconclusive_negative_or_confounded")]) == 0L)

bundle_filtered <- run_quasi_geo_test(dt_bundle, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                                      pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.15,
                                      min_robust_z = 0.8, min_volume = 0.1, min_donors = 2,
                                      other_media_contamination_pct = 0.15,
                                      min_evidence_score_to_keep = 1000)
add_result("reporting evidence filter does not change prior recommendations", isTRUE(all.equal(bundle$prior_recommendations, bundle_filtered$prior_recommendations, check.attributes = FALSE)) &&
             isTRUE(all.equal(bundle$bundle_prior_recommendations, bundle_filtered$bundle_prior_recommendations, check.attributes = FALSE)))

dt_opposite <- make_panel()
opposite_week <- sort(unique(dt_opposite$week))[34]
dt_opposite[geo == "G1" & week >= opposite_week & week < opposite_week + 28, `:=`(
  tv = tv + 45, tv_spend = tv_spend + 450,
  search = pmax(search - 25, 0), search_spend = pmax(search_spend - 150, 0),
  y = y + 60
)]
opposite <- run_quasi_geo_test(dt_opposite, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                               pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.15,
                               min_robust_z = 0.8, min_volume = 0.1, min_donors = 2,
                               other_media_contamination_pct = 0.15)
opposite_bundle <- opposite$event_estimates_all[estimand_level == "bundle" & co_moving_opposite_direction_n > 0]
add_result("opposite-direction bundle is diagnostic not usable", nrow(opposite_bundle) > 0 &&
             all(opposite_bundle$bundle_usable == FALSE) &&
             all(opposite_bundle$recommended_use == "diagnostic_only"))
add_result("opposite-direction bundle has diagnostic read not usable prior", nrow(opposite$bundle_prior_recommendations) > 0 &&
             all(opposite$bundle_prior_recommendations$usable_bundle_prior == FALSE) &&
             all(!is.finite(opposite$bundle_prior_recommendations$bundle_return_prior_mean)) &&
             any(is.finite(opposite$bundle_prior_recommendations$diagnostic_bundle_return_read)))

dt_contam <- copy(dt_up)
dt_contam[geo != "G1" & week >= event_week & week < event_week + 28, `:=`(tv = tv + 35, tv_spend = tv_spend + 350)]
contam <- run_quasi_geo_test(dt_contam, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                             pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.15,
                             min_robust_z = 0.8, min_volume = 0.1, min_donors = 2)
add_result("contaminated shock is downgraded not deleted", nrow(contam$event_estimates_all) > 0 &&
             any(grepl("donor_media_contamination|diagnostic", contam$event_estimates_all$diagnostic_reason)))

dt_donor_turn_on <- copy(dt_up)
dt_donor_turn_on[geo != "G1" & week < event_week, `:=`(tv = 0, tv_spend = 0)]
dt_donor_turn_on[geo != "G1" & week >= event_week & week < event_week + 28, `:=`(tv = 50, tv_spend = 500)]
donor_turn_on <- run_quasi_geo_test(dt_donor_turn_on, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                                    pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.15,
                                    min_robust_z = 0, min_volume = 0.1, min_donors = 2)
add_result("donor media turn-on contamination is flagged", any(grepl("donor_turn_on|donor_media_contamination", donor_turn_on$event_estimates_all$diagnostic_reason)))

dt_other_turn_on <- copy(dt_up)
dt_other_turn_on[geo == "G1" & week < event_week, `:=`(search = 0, search_spend = 0)]
dt_other_turn_on[geo == "G1" & week >= event_week & week < event_week + 28, `:=`(search = 35, search_spend = 210)]
other_turn_on <- run_quasi_geo_test(dt_other_turn_on, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                                    pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.15,
                                    min_robust_z = 0, min_volume = 0.1, min_donors = 2,
                                    other_media_contamination_pct = 0.15)
add_result("other media turn-on contamination is flagged", any(grepl("other_media_turn_on", other_turn_on$event_estimates_all$diagnostic_reason)))

dt_tiny_other <- copy(dt_up)
dt_tiny_other[geo == "G1", `:=`(search = 0, search_spend = 0)]
dt_tiny_other[geo == "G1" & week >= event_week & week < event_week + 28, `:=`(search = 0.001, search_spend = 0.006)]
tiny_other <- run_quasi_geo_test(dt_tiny_other, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                                 pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.15,
                                 min_robust_z = 0, min_volume = 0.1, min_donors = 2,
                                 other_media_contamination_pct = 0.15)
add_result("tiny other-media movement is not turn-on contamination", !any(grepl("other_media_turn_on", tiny_other$event_estimates_all[variable == "tv"]$diagnostic_reason)))

dt_na_post <- copy(dt_up)
dt_na_post[geo == "G1" & week == event_week + 7, tv := NA_real_]
na_post <- run_quasi_geo_test(dt_na_post, "week", "y", "geo", variable_map = vm_two, normalize = "none",
                              pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.2,
                              min_robust_z = 0, min_volume = 0.1, min_donors = 2)
add_result("post-window media NA does not hide strong ramp", nrow(na_post$candidate_events[variable == "tv"]) > 0)

dt_shifted_week <- copy(dt_up)
dt_shifted_week[, week := week + 2]
shifted <- run_quasi_geo_test(dt_shifted_week, "week", "y", "geo", variable_map = vm_two, normalize = "none",
                              pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.2,
                              min_robust_z = 0.8, min_volume = 0.1, min_donors = 2, period_days = 7)
add_result("non-Sunday weekly dates preserve seven-day event windows", nrow(shifted$candidate_events) > 0 &&
             all(as.integer(shifted$candidate_events$event_end - shifted$candidate_events$event_start) == 21L))

dt_nat <- make_panel()
dt_nat[, `:=`(tv = 100, tv_spend = 1000)]
nat <- run_quasi_geo_test(dt_nat, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                          pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.15,
                          min_robust_z = 0.8, min_volume = 0.1, min_donors = 2)
add_result("national repeated media is diagnostic only", nrow(nat$diagnostic_events[variable == "tv" & grepl("national_repeated_media", diagnostic_reason)]) > 0)
add_result("all and filtered dose summaries are returned", is.data.table(nat$dose_response_summary_all) && is.data.table(nat$dose_response_summary))
add_result("national repeated media evidence summary is blocked", nrow(nat$variable_evidence_summary[variable == "tv"]) > 0 &&
             "blocked_or_not_geo_identifiable" %in% nat$variable_evidence_summary[variable == "tv"]$recommended_analyst_action)

dt_sync <- make_panel()
dt_sync[week >= event_week & week < event_week + 28, `:=`(
  tv = tv * 1.8,
  tv_spend = tv_spend * 1.8,
  y = y + 60
)]
sync <- run_quasi_geo_test(dt_sync, "week", "y", "geo", variable_map = vm_two, normalize = "geo_mean_index",
                           pre_weeks = 8, post_weeks = 4, rolling_window = 8, min_pct_change = 0.20,
                           min_robust_z = 0.8, min_volume = 0.1, min_donors = 2)
sync_tv <- sync$event_estimates_all[variable == "tv" & grepl("no_unaffected_donor_geos_same_channel", diagnostic_reason, fixed = TRUE)]
add_result("synchronized all-geo media shock is not geo-identifiable", nrow(sync_tv) > 0 &&
             all(sync_tv$recommended_use == "diagnostic_only") &&
             all(sync_tv$channel_specific_usable == FALSE))
add_result("synchronized all-geo media shock stays out of priors", nrow(sync$prior_recommendations[variable == "tv"]) > 0 &&
             all(sync$prior_recommendations[variable == "tv", usable_positive_event_n] == 0L) &&
             all(!is.finite(sync$prior_recommendations[variable == "tv", coef_prior_mean])))

dir.create(file.path(bundle_dir, "test_outputs"), showWarnings = FALSE)
fwrite(results, file.path(bundle_dir, "test_outputs", "quasi_geo_evidence_classes_results.csv"))
message("\nQuasi geo evidence class results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")
invisible(results)
