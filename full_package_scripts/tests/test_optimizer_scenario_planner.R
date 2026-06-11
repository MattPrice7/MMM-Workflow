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
source(file.path(bundle_dir, "hier_mmm.R"), chdir = TRUE)
source(file.path(bundle_dir, "optimizer_scenario_planner.R"), chdir = TRUE)

results <- data.table(test = character(), status = character(), detail = character())
add_result <- function(test, ok, detail = "") {
  results <<- rbind(results, data.table(test = test, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = as.character(detail)))
  if (!isTRUE(ok)) stop("FAILED: ", test, if (nzchar(detail)) paste0(" -- ", detail) else "")
}

m <- seq(0, 2, by = 0.25)
response_curves <- rbindlist(list(
  data.table(variable = "tv", spend_multiplier = m, current_spend = 100, contribution = 60 * (1 - exp(-1.5 * m))),
  data.table(variable = "search", spend_multiplier = m, current_spend = 100, contribution = 95 * (1 - exp(-0.45 * m)))
))
response_curves[variable == "tv", `:=`(
  evidence_score_0_100 = 88,
  confidence_band = "very_strong",
  recommended_use = "calibration",
  response_curve_basis = "stan_posterior_draws"
)]
response_curves[variable == "search", `:=`(
  evidence_score_0_100 = 42,
  confidence_band = "diagnostic",
  recommended_use = "diagnostic_only",
  response_curve_basis = "bau_or_observational_diagnostic"
)]
out_rc <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  total_budget = 200,
  multiplier_grid = seq(0, 2, by = 0.25),
  budget_step_frac = 0.02,
  max_multiplier = 2,
  scenario_multipliers = c(1, 1.2),
  scenario_plan = data.table(
    scenario = "custom_tv_down_search_up",
    variable = c("tv", "search"),
    spend_multiplier = c(0.8, 1.2)
  )
)
add_result("response-curve-only planner returns core tables",
           all(c("current_plan", "response_curves", "saturation_headroom", "scenario_summary", "optimization_plan", "optimization_summary") %in% names(out_rc)) &&
             nrow(out_rc$current_plan) == 2L &&
             nrow(out_rc$response_curves) > 4L)
add_result("optimizer preserves curve evidence metadata in decision tables",
           all(c("curve_evidence_level", "curve_evidence_score", "curve_recommended_use", "response_curve_basis") %in% names(out_rc$current_plan)) &&
             out_rc$current_plan[variable == "tv", curve_evidence_level][1] == "very_strong" &&
             out_rc$current_plan[variable == "search", curve_evidence_level][1] == "diagnostic" &&
             out_rc$scenario_summary[scenario == "all_channels_1x", weak_curve_count][1] == 1L &&
             out_rc$optimization_summary$weak_curve_count[1] == 1L)
add_result("response-curve-only optimizer respects fixed budget",
           abs(out_rc$optimization_summary$recommended_spend[1] - 200) < 1e-6)
out_grid <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  total_budget = 200,
  multiplier_grid = seq(0, 2, by = 0.25),
  optimizer_method = "grid",
  optimization_grid_step = 0.25,
  max_grid_combinations = 200L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("grid optimizer respects fixed budget and reports grid basis",
           abs(out_grid$optimization_summary$recommended_spend[1] - 200) < 1e-6 &&
             grepl("_grid_search_point_estimate$", out_grid$optimization_summary$optimizer_basis[1]) &&
             out_grid$allocation_history$optimizer_method[1] == "grid")
variable_group_map <- data.table(
  variable = c("tv", "search"),
  planning_group = c("upper_funnel", "lower_funnel")
)
out_group_cap <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  total_budget = 200,
  variable_group_map = variable_group_map,
  group_constraints = data.table(planning_group = "upper_funnel", max_spend = 75),
  multiplier_grid = seq(0, 2, by = 0.25),
  optimizer_method = "grid",
  optimization_grid_step = 0.25,
  max_grid_combinations = 200L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("grid optimizer enforces planning-group max-spend constraint",
           out_group_cap$optimization_plan[variable == "tv", recommended_spend][1] <= 75 + 1e-8 &&
             nrow(out_group_cap$optimization_group_rollup[planning_group == "upper_funnel"]) == 1L &&
             isTRUE(out_group_cap$optimization_group_rollup[planning_group == "upper_funnel", group_constraint_ok][1]))

rollup_curves <- rbindlist(list(
  data.table(variable = "meta_campaign_1", spend_multiplier = m, current_spend = 50, contribution = 35 * (1 - exp(-1.0 * m))),
  data.table(variable = "meta_campaign_2", spend_multiplier = m, current_spend = 50, contribution = 30 * (1 - exp(-0.9 * m))),
  data.table(variable = "tiktok_campaign_1", spend_multiplier = m, current_spend = 50, contribution = 25 * (1 - exp(-0.8 * m)))
))
rollup_variable_map <- data.table(
  variable = c("meta_campaign_1", "meta_campaign_2", "tiktok_campaign_1"),
  rollup_path = c(
    "total_media > paid_social > meta > meta_campaign_1",
    "total_media > paid_social > meta > meta_campaign_2",
    "total_media > paid_social > tiktok > tiktok_campaign_1"
  )
)
out_rollup_group <- run_optimizer_scenario_planner(
  response_curves = rollup_curves,
  total_budget = 120,
  variable_group_map = rollup_variable_map,
  group_constraints = data.table(planning_group = "paid_social", max_spend = 120),
  multiplier_grid = seq(0, 2, by = 0.25),
  optimizer_method = "grid",
  optimization_grid_step = 0.25,
  max_grid_combinations = 1000L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("optimizer infers planning groups from arbitrary-depth rollup_path metadata",
           nrow(out_rollup_group$optimization_group_rollup) == 1L &&
             out_rollup_group$optimization_group_rollup$planning_group[1] == "paid_social" &&
             out_rollup_group$optimization_group_rollup$variable_count[1] == 3L &&
             out_rollup_group$optimization_group_rollup$recommended_spend[1] <= 120 + 1e-8)

out_group_share <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  total_budget = 200,
  variable_group_map = variable_group_map,
  group_constraints = data.table(planning_group = "upper_funnel", max_share = 0.375),
  multiplier_grid = seq(0, 2, by = 0.25),
  optimizer_method = "grid",
  optimization_grid_step = 0.25,
  max_grid_combinations = 200L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("grid optimizer converts planning-group share constraints to budget-scaled spend caps",
           out_group_share$optimization_group_rollup[planning_group == "upper_funnel", recommended_spend][1] <= 75 + 1e-8 &&
             out_group_share$settings$group_constraint_count[1] == 1L)

greedy_group_error <- tryCatch({
  run_optimizer_scenario_planner(
    response_curves = response_curves,
    total_budget = 200,
    variable_group_map = variable_group_map,
    group_constraints = data.table(planning_group = "upper_funnel", max_spend = 75),
    optimizer_method = "greedy",
    scenario_multipliers = 1
  )
  FALSE
}, error = function(e) grepl("group_constraints require optimizer_method", conditionMessage(e), fixed = TRUE))
add_result("greedy optimizer refuses group constraints instead of ignoring them",
           isTRUE(greedy_group_error))

out_hybrid <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  total_budget = 200,
  multiplier_grid = seq(0, 2, by = 0.25),
  optimizer_method = "hybrid",
  optimization_grid_step = 0.25,
  hybrid_refine_max_iter = 20L,
  max_grid_combinations = 200L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("hybrid optimizer runs coarse grid then continuous local refinement",
           grepl("_hybrid_grid_refine_point_estimate$", out_hybrid$optimization_summary$optimizer_basis[1]) &&
             any(out_hybrid$allocation_history$optimizer_phase == "coarse_grid", na.rm = TRUE) &&
             any(out_hybrid$allocation_history$optimizer_phase == "continuous_refine", na.rm = TRUE))
out_hybrid_group_cap <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  total_budget = 200,
  variable_group_map = variable_group_map,
  group_constraints = data.table(planning_group = "upper_funnel", max_spend = 75),
  multiplier_grid = seq(0, 2, by = 0.25),
  optimizer_method = "hybrid",
  optimization_grid_step = 0.25,
  hybrid_refine_max_iter = 20L,
  max_grid_combinations = 200L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("hybrid optimizer keeps continuous refinement inside planning-group constraints",
           out_hybrid_group_cap$optimization_plan[variable == "tv", recommended_spend][1] <= 75 + 1e-6 &&
             isTRUE(out_hybrid_group_cap$optimization_group_rollup[planning_group == "upper_funnel", group_constraint_ok][1]))
draw_curves <- rbindlist(lapply(1:5, function(draw_id) {
  scale <- c(0.80, 0.90, 1.00, 1.10, 1.20)[draw_id]
  rbindlist(list(
    data.table(.draw = draw_id, variable = "tv", spend_multiplier = m, current_spend = 100,
               contribution = scale * 60 * (1 - exp(-1.5 * m))),
    data.table(.draw = draw_id, variable = "search", spend_multiplier = m, current_spend = 100,
               contribution = scale * 95 * (1 - exp(-0.45 * m)))
  ))
}))
out_uncertainty <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  response_curve_draws = draw_curves,
  total_budget = 200,
  multiplier_grid = seq(0, 2, by = 0.25),
  max_multiplier = 2,
  scenario_multipliers = c(1, 1.2),
  value_per_kpi = 5,
  uncertainty_quantile = 0.35
)
add_result("optimizer reports draw-based scenario uncertainty when supplied",
           nrow(out_uncertainty$scenario_uncertainty_summary) == 2L &&
             all(c("contribution_q05", "contribution_q50", "contribution_q95") %in% names(out_uncertainty$scenario_uncertainty_summary)) &&
             out_uncertainty$scenario_uncertainty_summary[scenario == "all_channels_1x", contribution_q05][1] <
               out_uncertainty$scenario_uncertainty_summary[scenario == "all_channels_1x", contribution_q95][1])
add_result("optimizer reports custom uncertainty quantile, profit, and probability metrics",
           all(c("custom_quantile", "q05_incremental_contribution", "mean_incremental_contribution",
                 "q05_incremental_roi", "expected_profit", "q05_profit",
                 "probability_profit_positive", "probability_incremental_contribution_positive") %in%
                 names(out_uncertainty$scenario_uncertainty_summary)) &&
             abs(out_uncertainty$scenario_uncertainty_summary$custom_quantile[1] - 0.35) < 1e-8 &&
             is.finite(out_uncertainty$scenario_uncertainty_summary[scenario == "all_channels_1.2x", expected_profit][1]) &&
             out_uncertainty$scenario_uncertainty_summary[scenario == "all_channels_1.2x", probability_profit_positive][1] > 0 &&
             out_uncertainty$scenario_uncertainty_summary[scenario == "all_channels_1.2x", probability_incremental_contribution_positive][1] == 1)
add_result("optimizer reports draw-based optimized-plan uncertainty when supplied",
           nrow(out_uncertainty$optimization_uncertainty_summary) == 1L &&
             out_uncertainty$optimization_uncertainty_summary$draw_n[1] == 5L &&
             nrow(out_uncertainty$optimization_uncertainty_by_variable) == 2L)
out_robust <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  response_curve_draws = draw_curves,
  total_budget = 200,
  optimizer_method = "robust_grid",
  robust_objective = "q05_contribution",
  optimization_grid_step = 0.25,
  max_grid_combinations = 200L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("robust-grid optimizer selects plan from draw-based objective",
           grepl("_robust_grid_q05_contribution$", out_robust$optimization_summary$optimizer_basis[1]) &&
             out_robust$allocation_history$optimizer_method[1] == "robust_grid" &&
             "robust_score" %in% names(out_robust$allocation_history) &&
             grepl("robust_grid", out_robust$uncertainty_diagnostics$uncertainty_note[1]) &&
             nrow(out_robust$optimization_uncertainty_summary) == 1L)
out_robust_profit <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  response_curve_draws = draw_curves,
  total_budget = 200,
  optimizer_method = "robust_grid",
  robust_objective = "expected_profit",
  value_per_kpi = 5,
  optimization_grid_step = 0.25,
  max_grid_combinations = 200L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("robust optimizer can select plans by expected profit",
           grepl("_robust_grid_expected_profit$", out_robust_profit$optimization_summary$optimizer_basis[1]) &&
             "expected_profit" %in% names(out_robust_profit$allocation_history) &&
             is.finite(out_robust_profit$allocation_history$expected_profit[1]))
out_robust_hybrid <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  response_curve_draws = draw_curves,
  total_budget = 200,
  optimizer_method = "robust_hybrid",
  robust_objective = "q05_contribution",
  optimization_grid_step = 0.25,
  hybrid_refine_max_iter = 20L,
  max_grid_combinations = 200L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("robust-hybrid optimizer runs posterior objective with local refinement",
           grepl("_robust_hybrid_q05_contribution$", out_robust_hybrid$optimization_summary$optimizer_basis[1]) &&
             any(out_robust_hybrid$allocation_history$optimizer_phase == "coarse_grid", na.rm = TRUE) &&
             any(out_robust_hybrid$allocation_history$optimizer_phase == "continuous_refine", na.rm = TRUE) &&
             grepl("robust_hybrid", out_robust_hybrid$uncertainty_diagnostics$uncertainty_note[1]))
add_result("custom scenario planner evaluates variable-level multipliers",
           nrow(out_rc$scenario_detail[scenario == "custom_tv_down_search_up"]) == 2L &&
             abs(out_rc$scenario_detail[scenario == "custom_tv_down_search_up" & variable == "tv", spend_multiplier][1] - 0.8) < 1e-8)
add_result("response curves include ROI and mROI economics",
           all(c("roi", "mroi", "cost_per_kpi") %in% names(out_rc$response_curves)) &&
             any(is.finite(out_rc$response_curves$mroi)))
add_result("optimizer returns grid-based saturation and headroom summary",
           all(c("variable", "current_spend", "current_contribution", "peak_grid_contribution",
                 "contribution_headroom_to_peak_grid", "pct_of_peak_grid_contribution",
                 "saturation_band", "interpretation_note") %in% names(out_rc$saturation_headroom)) &&
             nrow(out_rc$saturation_headroom) == 2L &&
             all(out_rc$saturation_headroom$pct_of_peak_grid_contribution >= 0 &
                   out_rc$saturation_headroom$pct_of_peak_grid_contribution <= 1 + 1e-8, na.rm = TRUE) &&
             out_rc$saturation_headroom[variable == "tv", saturation_band][1] == "moderate_saturation_grid" &&
             out_rc$saturation_headroom[variable == "search", saturation_band][1] == "low_saturation_grid")

out_target <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  target_contribution = 90,
  target_cost_per_kpi = 2.8,
  budget_step_frac = 0.02,
  max_multiplier = 2,
  scenario_multipliers = 1,
  target_search_iter = 12
)
add_result("target-response planner finds minimum budget for KPI target",
           nrow(out_target$target_plan_summary[target_type == "target_contribution"]) == 1L &&
             isTRUE(out_target$target_plan_summary[target_type == "target_contribution", target_met][1]) &&
             out_target$target_plan_summary[target_type == "target_contribution", expected_contribution][1] >= 90 - 1e-6)
add_result("target-efficiency planner finds max budget within cost-per-KPI target",
           nrow(out_target$target_plan_summary[target_type == "target_cost_per_kpi"]) == 1L &&
             isTRUE(out_target$target_plan_summary[target_type == "target_cost_per_kpi", target_met][1]) &&
             out_target$target_plan_summary[target_type == "target_cost_per_kpi", expected_cost_per_kpi][1] <= 2.8 + 1e-6)

group_only_curves <- rbindlist(list(
  data.table(scope = "group", group = "G1", variable = "tv", spend_multiplier = m, current_spend = 60, contribution = 40 * (1 - exp(-1.3 * m))),
  data.table(scope = "group", group = "G2", variable = "tv", spend_multiplier = m, current_spend = 40, contribution = 20 * (1 - exp(-1.0 * m))),
  data.table(scope = "group", group = "G1", variable = "search", spend_multiplier = m, current_spend = 50, contribution = 45 * (1 - exp(-0.5 * m))),
  data.table(scope = "group", group = "G2", variable = "search", spend_multiplier = m, current_spend = 50, contribution = 35 * (1 - exp(-0.4 * m)))
))
out_group_curves <- run_optimizer_scenario_planner(
  response_curves = group_only_curves,
  total_budget = 200,
  multiplier_grid = c(0, 1, 2),
  budget_step_frac = 0.02,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("optimizer aggregates direct geo/product-geo response curves to variable level",
           nrow(out_group_curves$current_plan) == 2L &&
             abs(out_group_curves$current_plan[variable == "tv", current_spend][1] - 100) < 1e-8 &&
             out_group_curves$inputs_used$engine_mode[1] == "response_curve_table")

support_curves <- data.table(
  variable = "video",
  spend_multiplier = m,
  current_spend = 120,
  spend = 120 * m^1.05,
  current_support = 1000,
  support = 1000 * m,
  contribution = 80 * (1 - exp(-0.8 * m))
)
out_support_curves <- run_optimizer_scenario_planner(
  response_curves = support_curves,
  total_budget = 120,
  multiplier_grid = c(0, 1, 2),
  budget_step_frac = 0.02,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("optimizer preserves explicit spend and support from response curves",
           all(c("support", "current_support") %in% names(out_support_curves$response_curves)) &&
             abs(out_support_curves$current_plan$current_support[1] - 1000) < 1e-8 &&
             abs(out_support_curves$response_curves[variable == "video" & spend_multiplier == 2, support][1] - 2000) < 1e-8 &&
             abs(out_support_curves$response_curves[variable == "video" & spend_multiplier == 2, spend][1] - 120 * 2^1.05) < 1e-8 &&
             "current_support" %in% names(out_support_curves$saturation_headroom))

support_only_curves <- data.table(
  variable = "display",
  support = c(0, 1000000, 1500000, 2000000),
  current_support = 1000000,
  contribution = c(0, 40, 55, 65)
)
support_cost_map <- data.table(variable = "display", cpm = 10, support_unit = "impressions")
out_support_cost <- run_optimizer_scenario_planner(
  response_curves = support_only_curves,
  support_cost_map = support_cost_map,
  total_budget = 15000,
  scenario_plan = data.table(scenario = "support_plan", variable = "display", planned_support = 1500000),
  multiplier_grid = c(0, 1, 1.5, 2),
  optimizer_method = "grid",
  optimization_grid_step = 0.5,
  max_grid_combinations = 20L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("support-only response curves can be priced through CPM support_cost_map",
           abs(out_support_cost$current_plan$current_spend[1] - 10000) < 1e-8 &&
             abs(out_support_cost$response_curves[variable == "display" & spend_multiplier == 2, spend][1] - 20000) < 1e-8 &&
             out_support_cost$response_curves[variable == "display", support_unit][1] == "impressions")
add_result("custom scenario plan accepts planned support units",
           abs(out_support_cost$scenario_detail[scenario == "support_plan", spend_multiplier][1] - 1.5) < 1e-8 &&
             abs(out_support_cost$scenario_summary[scenario == "support_plan", spend][1] - 15000) < 1e-8)

out_support_cap <- run_optimizer_scenario_planner(
  response_curves = support_curves,
  total_budget = 240,
  constraints = data.table(variable = "video", max_spend = 130),
  multiplier_grid = c(0, 1, 2),
  optimizer_method = "grid",
  optimization_grid_step = 1,
  max_grid_combinations = 20L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("optimizer enforces max_spend against explicit nonlinear spend curve",
           out_support_cap$optimization_plan$recommended_spend[1] <= 130 + 1e-8 &&
             out_support_cap$allocation_history$channel_constraint_filtered_combinations[1] > 0)

launch_curves <- rbindlist(list(
  data.table(variable = "launch_video", spend_multiplier = c(0, 1, 2), current_spend = 0,
             spend = c(0, 50, 100), contribution = c(0, 35, 55)),
  data.table(variable = "search", spend_multiplier = c(0, 1, 2), current_spend = 100,
             spend = c(0, 100, 200), contribution = c(0, 30, 45))
))
out_launch <- run_optimizer_scenario_planner(
  response_curves = launch_curves,
  total_budget = 150,
  constraints = data.table(variable = "launch_video", min_spend = 50),
  multiplier_grid = c(0, 1, 2),
  optimizer_method = "grid",
  optimization_grid_step = 1,
  max_grid_combinations = 20L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("optimizer enforces min_spend for zero-current launch channel when explicit spend curve is supplied",
           out_launch$optimization_plan[variable == "launch_video", recommended_spend][1] >= 50 - 1e-8 &&
             any(out_launch$current_plan$variable == "launch_video" & out_launch$current_plan$current_spend == 0))

infeasible_spend_error <- tryCatch({
  run_optimizer_scenario_planner(
    response_curves = launch_curves,
    total_budget = 150,
    constraints = data.table(variable = "launch_video", min_spend = 150),
    multiplier_grid = c(0, 1, 2),
    optimizer_method = "grid",
    optimization_grid_step = 1,
    max_grid_combinations = 20L,
    max_multiplier = 2,
    scenario_multipliers = 1
  )
  ""
}, error = function(e) conditionMessage(e))
add_result("optimizer errors clearly when actual-spend constraints leave no feasible candidate",
           grepl("No feasible grid allocation satisfies budget, channel constraints, and group constraints", infeasible_spend_error, fixed = TRUE))

missing_spend_error <- tryCatch({
  run_optimizer_scenario_planner(
    response_curves = data.table(variable = "x", spend_multiplier = c(0, 1), contribution = c(0, 10)),
    scenario_multipliers = 1
  )
  ""
}, error = function(e) conditionMessage(e))
add_result("response-curve planner fails clearly when spend/current_spend is missing",
           grepl("Could not infer current_spend", missing_spend_error, fixed = TRUE))

negative_curves <- data.table(
  variable = "bad_media",
  spend_multiplier = c(0, 1, 2),
  current_spend = 100,
  contribution = c(0, -10, -25)
)
out_negative <- run_optimizer_scenario_planner(
  response_curves = negative_curves,
  total_budget = 100,
  optimizer_method = "grid",
  optimization_grid_step = 1,
  max_grid_combinations = 10L,
  max_multiplier = 2,
  scenario_multipliers = 1
)
negative_flags <- if (length(out_negative$diagnostics$flags)) out_negative$diagnostics$flags[[1]] else data.table()
add_result("negative contribution economics are flagged for review",
           any(negative_flags$flag %in% c("non_positive_current_contribution", "non_positive_expected_contribution")) &&
             all(!is.finite(out_negative$optimization_plan$expected_cost_per_kpi)))

dt <- data.table(
  week = as.Date("2024-01-07") + 7 * 0:3,
  geo = "G1",
  entity = "brand",
  tv = c(10, 20, 30, 40),
  search = c(30, 30, 30, 30),
  tv_spend = c(100, 100, 100, 100),
  search_spend = c(80, 80, 80, 80),
  is_holdout__ = FALSE,
  group_idx = 1L,
  rescale_factor__ = 1
)
fake_fit <- list(
  data = copy(dt),
  metadata = data.table(variable = c("tv", "search"), role = "media"),
  variable_lookup = data.table(
    variable = c("tv", "search"),
    variable_idx = c(1L, 2L),
    has_curve = c(0L, 0L),
    role = "media",
    curve_type = "weibull"
  ),
  posterior_means = list(
    beta = matrix(c(2.0, 1.0), nrow = 1),
    rrate = c(0, 0),
    cvalue = c(0, 0),
    dvalue = c(0, 0)
  ),
  long_decomp = data.table(
    week = rep(dt$week, each = 2),
    geo = "G1",
    entity = "brand",
    variable = rep(c("tv", "search"), times = 4),
    contribution = c(rbind(2 * dt$tv, dt$search))
  ),
  wide_decomp = data.table(),
  group_col = "geo",
  time_col = "week",
  entity_col = "entity",
  normalize_curve_x = TRUE,
  stan_data = list(curve_idx = integer())
)
out_fit <- run_optimizer_scenario_planner(
  fit_obj = fake_fit,
  spend_map = data.table(variable = c("tv", "search"), spend_col = c("tv_spend", "search_spend")),
  raw_data = dt,
  total_budget = 720,
  budget_step_frac = 0.05,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("MMM-fit planner uses Stan contribution helper path",
           out_fit$inputs_used$engine_mode[1] == "stan_mmm_fit" &&
             abs(out_fit$current_plan[variable == "tv", current_contribution][1] - sum(2 * dt$tv)) < 1e-8)
add_result("MMM-fit optimizer respects total budget",
           abs(out_fit$optimization_summary$recommended_spend[1] - 720) < 1e-6)

stan_curves <- build_response_curves_hier_mmm(
  fake_fit,
  spend_map = data.table(variable = c("tv", "search"), spend_col = c("tv_spend", "search_spend")),
  raw_data = dt,
  multiplier_grid = c(0, 1, 2)
)
add_result("Stan fit can output variable-level response curve sheet when only one group exists",
           is.data.table(stan_curves) &&
             all(c("scope", "group", "variable", "spend_multiplier", "spend", "support", "contribution", "mroi") %in% names(stan_curves)) &&
             nrow(stan_curves[scope == "total" & group == "ALL" & variable == "tv"]) == 3L &&
             nrow(stan_curves[scope == "group"]) == 0L)

stan_curves_both <- build_response_curves_hier_mmm(
  fake_fit,
  spend_map = data.table(variable = c("tv", "search"), spend_col = c("tv_spend", "search_spend")),
  raw_data = dt,
  multiplier_grid = c(0, 1, 2),
  response_curve_scope = "both"
)
add_result("Stan response curve sheet can include market/group rows when requested",
           nrow(stan_curves_both[scope == "group" & group == "G1" & variable == "tv"]) == 3L &&
             nrow(stan_curves_both[scope == "total" & group == "ALL" & variable == "tv"]) == 3L)

fake_fit_draws <- fake_fit
fake_fit_draws$posterior_draw_params <- list(params = list(
  list(draw_id = "draw_low", beta = matrix(c(1.8, 0.9), nrow = 1), rrate = c(0, 0), cvalue = c(0, 0), dvalue = c(1, 1)),
  list(draw_id = "draw_high", beta = matrix(c(2.2, 1.1), nrow = 1), rrate = c(0, 0), cvalue = c(0, 0), dvalue = c(1, 1))
))
stan_draw_curves <- build_response_curves_draws_hier_mmm(
  fake_fit_draws,
  spend_map = data.table(variable = c("tv", "search"), spend_col = c("tv_spend", "search_spend")),
  raw_data = dt,
  multiplier_grid = c(0, 1, 2)
)
add_result("Stan fit can output posterior-draw response curve sheet when draw params are available",
           is.data.table(stan_draw_curves) &&
             all(c(".draw", "scope", "group", "variable", "spend_multiplier", "contribution") %in% names(stan_draw_curves)) &&
             nrow(stan_draw_curves[.draw == "draw_low" & scope == "total" & variable == "tv"]) == 3L &&
             stan_draw_curves[.draw == "draw_low" & scope == "total" & variable == "tv" & spend_multiplier == 1, contribution][1] <
               stan_draw_curves[.draw == "draw_high" & scope == "total" & variable == "tv" & spend_multiplier == 1, contribution][1])

fake_fit_with_curves <- fake_fit
fake_fit_with_curves$response_curves <- stan_curves
out_fit_curves <- run_optimizer_scenario_planner(
  fit_obj = fake_fit_with_curves,
  total_budget = 720,
  budget_step_frac = 0.05,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("optimizer uses Stan response curve sheet when available",
           out_fit_curves$inputs_used$engine_mode[1] == "stan_response_curve_sheet" &&
             abs(out_fit_curves$optimization_summary$recommended_spend[1] - 720) < 1e-6)

out_locked <- run_optimizer_scenario_planner(
  response_curves = response_curves,
  total_budget = 200,
  constraints = data.table(variable = "tv", locked = TRUE),
  budget_step_frac = 0.02,
  max_multiplier = 2,
  scenario_multipliers = 1
)
add_result("optimizer respects locked-channel constraint",
           abs(out_locked$optimization_plan[variable == "tv", recommended_multiplier][1] - 1) < 1e-8)

dir.create(file.path(bundle_dir, "test_outputs"), showWarnings = FALSE)
fwrite(results, file.path(bundle_dir, "test_outputs", "optimizer_scenario_planner_results.csv"))
message("\nOptimizer scenario planner results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")
invisible(results)
