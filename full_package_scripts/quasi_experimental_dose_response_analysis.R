#!/usr/bin/env Rscript

# Analyst-facing wrapper for observational quasi-experimental dose-response checks.
# This is intentionally a thin layer over prior_builder(): the same guarded ramp,
# pooled geo/segment, placebo, and spend-support logic feeds both this standalone
# analysis and the prior-estimator workflow.

suppressPackageStartupMessages({
  library(data.table)
})

qedr_script_dir <- tryCatch({
  frames <- sys.frames()
  ofiles <- vapply(frames, function(f) if (!is.null(f$ofile)) as.character(f$ofile)[1] else NA_character_, character(1))
  ofiles <- ofiles[!is.na(ofiles) & nzchar(ofiles)]
  if (length(ofiles)) dirname(normalizePath(ofiles[length(ofiles)], mustWork = FALSE)) else getwd()
}, error = function(e) getwd())

source(file.path(qedr_script_dir, "semi_univariate_prior_builder_production_final.R"), chdir = TRUE)

run_quasi_experimental_dose_response_analysis <- function(input_data,
                                                          date_col,
                                                          dep_var_col,
                                                          variable_map,
                                                          coef_bounds = NULL,
                                                          fixed_rrate_by_var = NULL,
                                                          geo_col = NULL,
                                                          group_col = NULL,
                                                          segment_col = NULL,
                                                          holdout_col = NULL,
                                                          holdout_value = TRUE,
                                                          holdout_last_n = 0L,
                                                          base_cols = character(),
                                                          control_cols = character(),
                                                          output_dir = NULL,
                                                          prefix = "dose_response",
                                                          estimate_cvalue_from_data = "auto",
                                                          cvalue_anchor_method = "industry_hybrid",
                                                          use_fourier = TRUE,
                                                          use_holidays = TRUE,
                                                          use_week_of_month = TRUE,
                                                          ...) {
  if (missing(input_data) || is.null(input_data)) stop("input_data is required.")
  if (missing(variable_map) || is.null(variable_map)) stop("variable_map is required.")

  candidate_group_cols <- unique(as.character(c(geo_col, group_col, segment_col)))
  candidate_group_cols <- candidate_group_cols[!is.na(candidate_group_cols) & nzchar(candidate_group_cols)]
  input_names <- names(as.data.table(input_data))
  pooled_hit <- candidate_group_cols[candidate_group_cols %in% input_names]
  pooled_col <- if (length(pooled_hit)) pooled_hit[1] else NULL

  prior_out <- prior_builder(
    input_data = input_data,
    date_col = date_col,
    dep_var_col = dep_var_col,
    variable_map = variable_map,
    coef_bounds = coef_bounds,
    fixed_rrate_by_var = fixed_rrate_by_var,
    holdout_col = holdout_col,
    holdout_value = holdout_value,
    holdout_last_n = holdout_last_n,
    base_cols = base_cols,
    control_cols = control_cols,
    estimate_cvalue_from_data = estimate_cvalue_from_data,
    cvalue_anchor_method = cvalue_anchor_method,
    observed_diminishing_returns = TRUE,
    flatten_when_no_observed_diminishing = TRUE,
    future_spend_placebo_guard = TRUE,
    pooled_ramp_group_col = pooled_col,
    use_fourier = use_fourier,
    use_holidays = use_holidays,
    use_week_of_month = use_week_of_month,
    ...
  )

  p <- copy(prior_out$priors)
  wanted <- intersect(c(
    "variable", "modeled_x_col", "spend_col", "support_col",
    "cvalue", "cvalue_anchor", "cvalue_data_driven", "cvalue_final_source",
    "cvalue_data_reason", "cvalue_data_improvement", "ramp_period_n",
    "ramp_period_share", "observed_curve_evidence_class",
    "observed_marginal_slope_low", "observed_marginal_slope_high",
    "observed_marginal_slope_ratio", "observed_slope_cvalue",
    "pooled_ramp_group_col", "pooled_ramp_usable_groups",
    "pooled_ramp_flat_negative_groups",
    "pooled_ramp_cvalue", "pooled_ramp_reliability",
    "pooled_ramp_evidence_class", "future_spend_placebo_class",
    "future_spend_placebo_ratio", "spend_level_class",
    "under_spend_flag", "over_spend_flag", "coef_prior_final",
    "coef_precision_final", "recommended_prior_strategy",
    "identification_class", "stan_observed_cvalue",
    "stan_observed_cvalue_source", "stan_observed_cvalue_reliability"
  ), names(p))

  summary <- p[, ..wanted]
  summary[, analysis_frame := "observational_quasi_experimental_dose_response"]
  summary[, geo_or_segment_level := !is.null(pooled_col)]

  if (!is.null(output_dir) && nzchar(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    fwrite(summary, file.path(output_dir, paste0(prefix, "_summary.csv")))
    fwrite(prior_out$evidence_stack, file.path(output_dir, paste0(prefix, "_evidence.csv")))
    if (nrow(prior_out$pooled_group_ramp_details)) {
      fwrite(prior_out$pooled_group_ramp_details, file.path(output_dir, paste0(prefix, "_geo_segment_details.csv")))
    }
    if (nrow(prior_out$transformed_x_handoff)) {
      fwrite(prior_out$transformed_x_handoff, file.path(output_dir, paste0(prefix, "_transformed_series.csv")))
    }
  }

  list(
    summary = summary[],
    evidence = prior_out$evidence_stack,
    geo_segment_details = prior_out$pooled_group_ramp_details,
    transformed_series = prior_out$transformed_x_handoff,
    metadata_handoff = prior_out$metadata_handoff,
    holdout_audit = prior_out$holdout_audit,
    prior_output = prior_out,
    notes = c(
      "This is observational quasi-experimental dose-response evidence, not randomized lift calibration.",
      "Use higher confidence when ramps are material, repeated, directionally consistent, and pass future-spend placebo checks.",
      "Use lower confidence when spend movement is weak, future spend predicts current KPI, or geo/segment ramp signals disagree."
    )
  )
}
