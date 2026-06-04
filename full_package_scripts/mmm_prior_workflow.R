# mmm_prior_workflow.R
# Analyst-facing orchestration helpers for the MMM diagnostic + prior workflow.

if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required.")

`%||%` <- function(x, y) if (is.null(x)) y else x

mpw_as_dt <- function(x, label = "input") {
  if (is.null(x)) stop(label, " is NULL.")
  data.table::as.data.table(data.table::copy(x))
}

mpw_require_functions <- function(fns) {
  missing <- fns[!vapply(fns, exists, logical(1), mode = "function")]
  if (length(missing)) {
    stop(
      "Missing required functions: ", paste(missing, collapse = ", "),
      ". Source marketing_mix_diagnostic_builder_production_final.R and ",
      "semi_univariate_prior_builder_production_final.R before mmm_prior_workflow.R."
    )
  }
  invisible(TRUE)
}

mpw_first_existing_col <- function(dt, candidates) {
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  hit <- candidates[candidates %in% names(dt)]
  if (length(hit)) hit[1] else NA_character_
}

mpw_parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  out <- suppressWarnings(as.Date(x))
  if (all(is.na(out))) out <- suppressWarnings(as.Date(as.numeric(x), origin = "1899-12-30"))
  out
}

mpw_apply_holdout_filter <- function(dt,
                                     date_col,
                                     holdout_col = NULL,
                                     holdout_value = TRUE,
                                     holdout_last_n = 0L) {
  out <- mpw_as_dt(dt, "input_data")
  is_holdout <- rep(FALSE, nrow(out))
  if (!is.null(holdout_col) && nzchar(as.character(holdout_col)[1])) {
    holdout_col <- as.character(holdout_col)[1]
    if (!holdout_col %in% names(out)) stop("holdout_col not found: ", holdout_col)
    hv <- out[[holdout_col]]
    if (is.logical(hv) && identical(holdout_value, TRUE)) {
      is_holdout <- is_holdout | (hv %in% TRUE)
    } else {
      is_holdout <- is_holdout | (as.character(hv) %in% as.character(holdout_value))
    }
  }
  holdout_last_n <- as.integer(holdout_last_n %||% 0L)[1]
  if (is.finite(holdout_last_n) && holdout_last_n > 0L) {
    d <- mpw_parse_date(out[[date_col]])
    holdout_dates <- utils::tail(sort(unique(d[!is.na(d)])), holdout_last_n)
    is_holdout <- is_holdout | (d %in% holdout_dates)
  }
  out[, is_prior_training__ := !is_holdout]
  if (!any(out$is_prior_training__)) stop("All rows are holdout. At least one training row is required.")
  list(
    training_data = out[is_prior_training__ == TRUE][, is_prior_training__ := NULL][],
    holdout_data = out[is_prior_training__ == FALSE][, is_prior_training__ := NULL][],
    audit = data.table::data.table(
      input_row_n = nrow(out),
      training_row_n = sum(out$is_prior_training__),
      holdout_row_n = sum(!out$is_prior_training__),
      holdout_col = as.character(holdout_col %||% NA_character_),
      holdout_value = paste(as.character(holdout_value), collapse = "|"),
      holdout_last_n = as.integer(holdout_last_n %||% 0L)
    )
  )
}

mpw_adstock <- function(x, rrate) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- 0
  rrate <- min(max(as.numeric(rrate)[1], 0), 0.99)
  out <- numeric(length(x))
  for (i in seq_along(x)) out[i] <- x[i] + if (i == 1L) 0 else rrate * out[i - 1L]
  out
}

mpw_saturation <- function(x, cvalue, dvalue = 1) {
  1 - exp(-((pmax(as.numeric(x), 0) * as.numeric(cvalue)[1]) ^ as.numeric(dvalue)[1]))
}

normalize_mmm_market_size_inputs <- function(input_data,
                                             variable_map,
                                             dep_var_col,
                                             variables = NULL,
                                             market_size_col = NULL,
                                             population_col = NULL,
                                             households_col = NULL,
                                             scale_media_by_market_size = FALSE,
                                             scale_dep_var_by_market_size = FALSE,
                                             scale_factor = 1000,
                                             scaled_suffix = "_per_market_size",
                                             keep_original_cols = TRUE) {
  dt <- mpw_as_dt(input_data, "input_data")
  vm <- mpw_as_dt(variable_map, "variable_map")
  if (!"variable" %in% names(vm)) {
    if ("channel" %in% names(vm)) vm[, variable := as.character(channel)] else stop("variable_map must contain variable or channel.")
  }
  for (nm in c("modeled_x_col", "modeled_x_basis")) if (!nm %in% names(vm)) vm[, (nm) := NA_character_]
  if (!dep_var_col %in% names(dt)) stop("dep_var_col not found: ", dep_var_col)

  size_col <- mpw_first_existing_col(dt, c(market_size_col, population_col, households_col))
  if (is.na(size_col)) {
    if (isTRUE(scale_media_by_market_size) || isTRUE(scale_dep_var_by_market_size)) {
      stop("Market-size scaling requested but no market_size_col/population_col/households_col was found in input_data.")
    }
    return(list(
      data = dt,
      variable_map = vm,
      dep_var_col = dep_var_col,
      scaled = FALSE,
      scale_metadata = data.table::data.table(
        scaled = FALSE,
        market_size_col = NA_character_,
        scale_factor = as.numeric(scale_factor),
        scaled_media_variables = "",
        scaled_dep_var_col = NA_character_,
        note = "No market-size scaling requested or available."
      )
    ))
  }

  scaling_requested <- isTRUE(scale_media_by_market_size) || isTRUE(scale_dep_var_by_market_size)
  size <- suppressWarnings(as.numeric(dt[[size_col]]))
  if (isTRUE(scaling_requested) && any(!is.finite(size) | size <= 0, na.rm = FALSE)) {
    bad_n <- sum(!is.finite(size) | size <= 0, na.rm = TRUE)
    stop("Market-size scaling requested but column '", size_col, "' contains ", bad_n, " non-positive or non-finite rows.")
  }
  scale_factor <- as.numeric(scale_factor)[1]
  if (!is.finite(scale_factor) || scale_factor <= 0) stop("scale_factor must be positive.")

  if (is.null(variables)) variables <- vm$variable
  variables <- intersect(as.character(variables), as.character(vm$variable))
  scaled_vars <- character()

  if (isTRUE(scale_media_by_market_size) && length(variables)) {
    for (v in variables) {
      row_i <- match(v, vm$variable)
      mx <- as.character(vm$modeled_x_col[row_i])
      if (is.na(mx) || !nzchar(mx) || !(mx %in% names(dt))) stop("modeled_x_col for variable '", v, "' is missing from input_data.")
      new_col <- paste0(mx, scaled_suffix)
      if (new_col %in% names(dt) && !isTRUE(keep_original_cols)) stop("Scaled column already exists: ", new_col)
      dt[, (new_col) := suppressWarnings(as.numeric(get(mx))) / suppressWarnings(as.numeric(get(size_col))) * scale_factor]
      basis <- as.character(vm$modeled_x_basis[row_i])
      if (is.na(basis) || !nzchar(basis)) basis <- "media"
      vm[row_i, modeled_x_col := new_col]
      vm[row_i, modeled_x_basis := paste0(basis, "_per_", size_col)]
      scaled_vars <- c(scaled_vars, v)
    }
  }

  dep_out <- dep_var_col
  if (isTRUE(scale_dep_var_by_market_size)) {
    dep_out <- paste0(dep_var_col, scaled_suffix)
    dt[, (dep_out) := suppressWarnings(as.numeric(get(dep_var_col))) / suppressWarnings(as.numeric(get(size_col))) * scale_factor]
  }

  list(
    data = dt,
    variable_map = vm,
    dep_var_col = dep_out,
    scaled = length(scaled_vars) > 0 || isTRUE(scale_dep_var_by_market_size),
    scale_metadata = data.table::data.table(
      scaled = length(scaled_vars) > 0 || isTRUE(scale_dep_var_by_market_size),
      market_size_col = size_col,
      scale_factor = scale_factor,
      scaled_media_variables = paste(scaled_vars, collapse = "|"),
      scaled_dep_var_col = if (isTRUE(scale_dep_var_by_market_size)) dep_out else NA_character_,
      note = "Scaled values are per scale_factor units of market size; spend_col remains unscaled for cost and budget accounting."
    )
  )
}

make_mmm_prior_audit <- function(prior_output,
                                 granularity_audit = NULL,
                                 mix_diagnostic = NULL) {
  if (!is.list(prior_output) || is.null(prior_output$metadata_handoff)) {
    stop("prior_output must be returned by prior_builder().")
  }
  md <- mpw_as_dt(prior_output$metadata_handoff, "prior_output$metadata_handoff")
  priors <- if (!is.null(prior_output$priors)) mpw_as_dt(prior_output$priors, "prior_output$priors") else data.table::data.table(variable = md$variable)
  keep_prior <- intersect(c(
    "variable", "status", "coef_prior_handoff_tier", "coef_prior_handoff_reason",
    "use_as_production_prior", "use_as_directional_starting_value",
    "recommended_prior_strategy", "identification_class",
    "observed_curve_evidence_class", "pooled_ramp_evidence_class",
    "future_spend_placebo_class", "spend_level_class", "under_spend_flag",
    "over_spend_flag", "missing_data_class", "missing_data_action",
    "max_missing_share_before", "max_missing_share_after",
    "missing_data_risk_multiplier", "sanity_bound_class",
    "sanity_bound_flags", "sanity_bound_risk_multiplier",
    "implied_contribution_share", "implied_elasticity",
    "implied_cost_per_outcome", "implied_outcome_per_cost",
    "coef_prior_source", "coef_prior_pre_multivariate",
    "multivariate_coef_scan_class", "multivariate_ridge_coef",
    "multivariate_ridge_max_abs_corr", "multivariate_ridge_condition_number",
    "multivariate_ridge_to_univariate_ratio"
  ), names(priors))
  audit <- unique(priors[, ..keep_prior])[md, on = "variable"]

  if (!is.null(granularity_audit) && !is.null(granularity_audit$variable_granularity)) {
    vg <- mpw_as_dt(granularity_audit$variable_granularity, "granularity_audit$variable_granularity")
    keep_vg <- intersect(c(
      "variable", "media_granularity_class", "modeled_x_geo_variation_week_share",
      "spend_geo_variation_week_share", "coef_hierarchy_scale_recommended"
    ), names(vg))
    audit <- unique(vg[, ..keep_vg])[audit, on = "variable"]
  }

  if (!is.null(mix_diagnostic)) {
    mix_obj <- if (is.list(mix_diagnostic) && !is.null(mix_diagnostic$out)) mix_diagnostic$out else mix_diagnostic
    if (!is.null(mix_obj$curve_prior_inputs)) {
      cpi <- mpw_as_dt(mix_obj$curve_prior_inputs, "mix_diagnostic$curve_prior_inputs")
      keep_cpi <- intersect(c(
        "variable", "reliability", "anchor_actionability_tier",
        "anchor_should_drive_curve_prior", "spend_support_mismatch_flag",
        "anchor_defensibility_note"
      ), names(cpi))
      audit <- unique(cpi[, ..keep_cpi])[audit, on = "variable"]
    }
  }

  for (nm in c("use_as_production_prior", "use_as_directional_starting_value", "requires_external_prior")) {
    if (!(nm %in% names(audit))) audit[, (nm) := FALSE]
    audit[is.na(get(nm)), (nm) := FALSE]
  }
  if (!"collinearity_class" %in% names(audit)) audit[, collinearity_class := NA_character_]
  if (!"media_granularity_class" %in% names(audit)) audit[, media_granularity_class := NA_character_]
  if (!"anchor_actionability_tier" %in% names(audit)) audit[, anchor_actionability_tier := NA_character_]
  if (!"sanity_bound_class" %in% names(audit)) audit[, sanity_bound_class := NA_character_]
  if (!"missing_data_class" %in% names(audit)) audit[, missing_data_class := NA_character_]
  if (!"spend_level_class" %in% names(audit)) audit[, spend_level_class := NA_character_]

  audit[, curve_confidence := data.table::fifelse(
    requires_external_prior == TRUE |
      sanity_bound_class == "outside_sanity_bounds" |
      grepl("_high", missing_data_class %||% "") |
      spend_level_class %in% c("under_spent_extrapolation", "over_spent_high_saturation"),
    "low_needs_external_evidence_or_review",
    data.table::fifelse(
      use_as_production_prior == TRUE &
        collinearity_class %in% c("low_multicollinearity", "not_enough_variables", NA_character_) &
        anchor_actionability_tier %in% c("actionable", "directional", NA_character_),
      "moderate",
      data.table::fifelse(use_as_directional_starting_value == TRUE, "directional", "manual_review")
    )
  )]

  audit[, analyst_action := data.table::fifelse(
    requires_external_prior == TRUE,
    "aggregate_or_add_external_prior_before_tight_curve",
    data.table::fifelse(
      sanity_bound_class == "outside_sanity_bounds",
      "review_kpi_economics_contribution_bounds_before_use",
      data.table::fifelse(
        grepl("_high", missing_data_class %||% ""),
        "repair_or_document_missing_data_before_tight_curve",
        data.table::fifelse(
      use_as_production_prior == TRUE,
      "use_as_prior_curve_with_sensitivity",
      data.table::fifelse(
        use_as_directional_starting_value == TRUE,
        "directional_curve_only",
        "manual_review_before_use"
      )
        )
      )
    )
  )]

  reason_one <- function(i) {
    parts <- c()
    if ("status" %in% names(audit) && nzchar(as.character(audit$status[i] %||% ""))) parts <- c(parts, paste0("status=", audit$status[i]))
    if (nzchar(as.character(audit$collinearity_class[i] %||% ""))) parts <- c(parts, paste0("collinearity=", audit$collinearity_class[i]))
    if (nzchar(as.character(audit$media_granularity_class[i] %||% ""))) parts <- c(parts, paste0("granularity=", audit$media_granularity_class[i]))
    if (nzchar(as.character(audit$anchor_actionability_tier[i] %||% ""))) parts <- c(parts, paste0("anchor=", audit$anchor_actionability_tier[i]))
    if (nzchar(as.character(audit$observed_curve_evidence_class[i] %||% ""))) parts <- c(parts, paste0("curve_evidence=", audit$observed_curve_evidence_class[i]))
    if (nzchar(as.character(audit$pooled_ramp_evidence_class[i] %||% ""))) parts <- c(parts, paste0("pooled_ramp=", audit$pooled_ramp_evidence_class[i]))
    if (nzchar(as.character(audit$future_spend_placebo_class[i] %||% ""))) parts <- c(parts, paste0("placebo=", audit$future_spend_placebo_class[i]))
    if (nzchar(as.character(audit$spend_level_class[i] %||% ""))) parts <- c(parts, paste0("spend_level=", audit$spend_level_class[i]))
    if (nzchar(as.character(audit$coef_prior_source[i] %||% ""))) parts <- c(parts, paste0("coef_source=", audit$coef_prior_source[i]))
    if (nzchar(as.character(audit$multivariate_coef_scan_class[i] %||% ""))) parts <- c(parts, paste0("ridge_scan=", audit$multivariate_coef_scan_class[i]))
    if (nzchar(as.character(audit$missing_data_class[i] %||% ""))) parts <- c(parts, paste0("missing=", audit$missing_data_class[i]))
    if (nzchar(as.character(audit$sanity_bound_class[i] %||% "")) && !identical(as.character(audit$sanity_bound_class[i]), "within_sanity_bounds")) {
      parts <- c(parts, paste0("sanity=", audit$sanity_bound_class[i]))
    }
    if ("spend_support_mismatch_flag" %in% names(audit) && isTRUE(audit$spend_support_mismatch_flag[i])) parts <- c(parts, "spend_support_mismatch")
    paste(unique(parts), collapse = "; ")
  }
  audit[, audit_reason := vapply(seq_len(.N), reason_one, character(1))]

  front <- intersect(c(
    "variable", "analyst_action", "curve_confidence", "audit_reason",
    "coef", "coef_precision", "coef_bound", "rrate", "cvalue", "dvalue",
    "rrate_precision", "cvalue_precision", "dvalue_precision",
    "collinearity_class", "max_abs_correlation", "max_vif",
    "media_granularity_class", "coef_hierarchy_scale",
    "anchor_actionability_tier", "reliability",
    "observed_curve_evidence_class", "pooled_ramp_evidence_class",
    "future_spend_placebo_class", "spend_level_class",
    "coef_prior_source", "multivariate_coef_scan_class",
    "multivariate_ridge_coef", "multivariate_ridge_max_abs_corr",
    "missing_data_class", "max_missing_share_before",
    "sanity_bound_class", "sanity_bound_flags",
    "implied_contribution_share", "implied_elasticity"
  ), names(audit))
  audit[, c(front, setdiff(names(audit), front)), with = FALSE][]
}

summarize_mmm_prior_evidence_layers <- function(prior_output,
                                                prior_audit = NULL) {
  if (!is.list(prior_output) || is.null(prior_output$priors)) {
    stop("prior_output must be returned by prior_builder().")
  }
  priors <- mpw_as_dt(prior_output$priors, "prior_output$priors")
  if (!"variable" %in% names(priors)) stop("prior_output$priors must contain variable.")
  get_col <- function(dt, col, default = NA_character_) {
    if (col %in% names(dt)) dt[[col]] else rep(default, nrow(dt))
  }
  get_num <- function(dt, col, default = NA_real_) suppressWarnings(as.numeric(get_col(dt, col, default)))
  get_log <- function(dt, col, default = FALSE) {
    x <- get_col(dt, col, default)
    x[is.na(x)] <- default
    as.logical(x)
  }
  evidence <- data.table::data.table(
    variable = as.character(priors$variable),
    final_coef = get_num(priors, "coef_prior_final"),
    final_rrate = get_num(priors, "rrate"),
    final_cvalue = get_num(priors, "cvalue"),
    final_dvalue = get_num(priors, "dvalue"),
    coef_prior_source = as.character(get_col(priors, "coef_prior_source")),
    curve_source = as.character(get_col(priors, "cvalue_final_source")),
    observed_curve_evidence = as.character(get_col(priors, "observed_curve_evidence_class")),
    pooled_geo_or_segment_ramp_evidence = as.character(get_col(priors, "pooled_ramp_evidence_class")),
    future_spend_placebo = as.character(get_col(priors, "future_spend_placebo_class")),
    multivariate_ridge_recovery = as.character(get_col(priors, "multivariate_coef_scan_class")),
    collinearity = as.character(get_col(priors, "collinearity_class")),
    missing_data = as.character(get_col(priors, "missing_data_class")),
    spend_level = as.character(get_col(priors, "spend_level_class")),
    sanity_bounds = as.character(get_col(priors, "sanity_bound_class")),
    granularity = as.character(get_col(priors, "media_granularity_class")),
    use_as_production_prior = get_log(priors, "use_as_production_prior"),
    use_as_directional_starting_value = get_log(priors, "use_as_directional_starting_value"),
    requires_external_prior = get_log(priors, "requires_external_prior"),
    recommended_prior_strategy = as.character(get_col(priors, "recommended_prior_strategy"))
  )
  evidence[, best_curve_evidence_layer := data.table::fcase(
    nzchar(pooled_geo_or_segment_ramp_evidence) & !pooled_geo_or_segment_ramp_evidence %in% c("no_group_evidence", "insufficient_group_evidence", NA_character_),
    "pooled_geo_or_segment_ramp",
    nzchar(observed_curve_evidence) & !observed_curve_evidence %in% c("no_observed_diminishing_evidence", "insufficient_evidence", NA_character_),
    "historical_ramp_or_partial_residual",
    nzchar(curve_source),
    curve_source,
    default = "default_anchor"
  )]
  evidence[, analyst_use_level := data.table::fcase(
    requires_external_prior == TRUE, "needs_external_evidence_or_aggregation",
    use_as_production_prior == TRUE, "usable_with_sensitivity",
    use_as_directional_starting_value == TRUE, "directional_only",
    default = "manual_review"
  )]

  if (!is.null(prior_audit)) {
    audit <- mpw_as_dt(prior_audit, "prior_audit")
    keep <- intersect(c("variable", "analyst_action", "curve_confidence", "audit_reason"), names(audit))
    if (length(keep) > 1L) evidence <- unique(audit[, ..keep])[evidence, on = "variable"]
  }
  front <- intersect(c(
    "variable", "analyst_use_level", "analyst_action", "curve_confidence",
    "best_curve_evidence_layer", "coef_prior_source", "curve_source",
    "observed_curve_evidence", "pooled_geo_or_segment_ramp_evidence",
    "multivariate_ridge_recovery", "collinearity", "granularity",
    "missing_data", "spend_level", "sanity_bounds", "future_spend_placebo",
    "recommended_prior_strategy", "audit_reason"
  ), names(evidence))
  evidence[, c(front, setdiff(names(evidence), front)), with = FALSE][]
}

write_mmm_prior_workflow_outputs <- function(workflow_output,
                                             output_dir,
                                             prefix = "") {
  if (is.null(output_dir) || !nzchar(output_dir)) return(invisible(NULL))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  pfx <- if (nzchar(prefix)) paste0(prefix, "_") else ""
  write_dt <- function(x, name) {
    if (!is.null(x) && (data.table::is.data.table(x) || is.data.frame(x))) {
      data.table::fwrite(data.table::as.data.table(x), file.path(output_dir, paste0(pfx, name, ".csv")))
    }
  }
  write_dt(workflow_output$prior_audit, "prior_audit")
  write_dt(workflow_output$evidence_summary, "prior_evidence_summary")
  write_dt(workflow_output$response_curves, "response_curves")
  write_dt(workflow_output$metadata, "metadata_handoff")
  write_dt(workflow_output$holdout_audit, "holdout_audit")
  write_dt(workflow_output$prior_output$priors, "priors")
  write_dt(workflow_output$prior_output$evidence_stack, "evidence_stack")
  write_dt(workflow_output$prior_output$identification_diagnostics, "identification_diagnostics")
  write_dt(workflow_output$prior_output$collinearity_pairs, "collinearity_pairs")
  if (!is.null(workflow_output$granularity)) {
    write_dt(workflow_output$granularity$summary, "granularity_summary")
    write_dt(workflow_output$granularity$variable_granularity, "granularity_by_variable")
  }
  if (!is.null(workflow_output$mix_diagnostic)) {
    mix_obj <- if (is.list(workflow_output$mix_diagnostic) && !is.null(workflow_output$mix_diagnostic$out)) workflow_output$mix_diagnostic$out else workflow_output$mix_diagnostic
    write_dt(mix_obj$total_media_diagnostic, "mix_total_media_diagnostic")
    write_dt(mix_obj$channel_diagnostic, "mix_channel_diagnostic")
    write_dt(mix_obj$curve_prior_inputs, "mix_curve_prior_inputs")
    if (is.list(workflow_output$mix_diagnostic) && !is.null(workflow_output$mix_diagnostic$validation)) {
      write_dt(workflow_output$mix_diagnostic$validation, "mix_validation")
    }
  }
  write_dt(workflow_output$market_size_scaling$scale_metadata, "market_size_scaling")
  invisible(output_dir)
}

run_mmm_prior_workflow <- function(input_data,
                                   date_col,
                                   dep_var_col,
                                   variable_map,
                                   channel_map = NULL,
                                   geo_col = NULL,
                                   market_size_col = NULL,
                                   population_col = NULL,
                                   households_col = NULL,
                                   scale_media_by_market_size = FALSE,
                                   scale_dep_var_by_market_size = FALSE,
                                   market_size_scale_factor = 1000,
                                   coef_bounds = NULL,
                                   fixed_rrate_by_var = NULL,
                                   benchmark_priors = NULL,
                                   benchmark_precision_weight = 1,
                                   diagnostic_args = list(),
                                   prior_args = list(),
                                   holdout_col = NULL,
                                   holdout_value = TRUE,
                                   holdout_last_n = 0L,
                                   response_curve_multipliers = seq(0, 2, by = 0.05),
                                   output_dir = NULL,
                                   output_prefix = "") {
  mpw_require_functions(c(
    "diagnose_mmm_data_granularity", "prior_builder", "make_hier_metadata_from_prior_output",
    "apply_data_granularity_adjustments_to_prior_output", "make_curve_anchors_from_diagnostic",
    "apply_benchmark_priors_to_metadata"
  ))
  if (!is.null(channel_map)) mpw_require_functions("diagnose_marketing_mix")

  holdout_split <- mpw_apply_holdout_filter(
    dt = input_data,
    date_col = date_col,
    holdout_col = holdout_col,
    holdout_value = holdout_value,
    holdout_last_n = holdout_last_n
  )

  scaled <- normalize_mmm_market_size_inputs(
    input_data = input_data,
    variable_map = variable_map,
    dep_var_col = dep_var_col,
    market_size_col = market_size_col,
    population_col = population_col,
    households_col = households_col,
    scale_media_by_market_size = scale_media_by_market_size,
    scale_dep_var_by_market_size = scale_dep_var_by_market_size,
    scale_factor = market_size_scale_factor
  )
  training_scaled <- normalize_mmm_market_size_inputs(
    input_data = holdout_split$training_data,
    variable_map = variable_map,
    dep_var_col = dep_var_col,
    market_size_col = market_size_col,
    population_col = population_col,
    households_col = households_col,
    scale_media_by_market_size = scale_media_by_market_size,
    scale_dep_var_by_market_size = scale_dep_var_by_market_size,
    scale_factor = market_size_scale_factor
  )

  granularity <- diagnose_mmm_data_granularity(
    input_data = training_scaled$data,
    date_col = date_col,
    dep_var_col = training_scaled$dep_var_col,
    variable_map = training_scaled$variable_map,
    geo_col = geo_col
  )

  mix_diag <- NULL
  curve_anchors <- NULL
  if (!is.null(channel_map)) {
    diag_call <- modifyList(list(
      input_data = holdout_split$training_data,
      week_col = date_col,
      sales_col = dep_var_col,
      channel_map = channel_map
    ), diagnostic_args)
    mix_diag <- do.call(diagnose_marketing_mix, diag_call)
    curve_anchors <- make_curve_anchors_from_diagnostic(mix_diag)
  }

  pb_call <- modifyList(list(
    input_data = training_scaled$data,
    date_col = date_col,
    dep_var_col = training_scaled$dep_var_col,
    variable_map = training_scaled$variable_map,
    coef_bounds = coef_bounds,
    fixed_rrate_by_var = fixed_rrate_by_var,
    curve_anchors = curve_anchors,
    diagnose_collinearity = TRUE,
    adjust_priors_for_collinearity = TRUE
  ), prior_args)
  if (!is.null(geo_col) && nzchar(as.character(geo_col)[1]) &&
      as.character(geo_col)[1] %in% names(training_scaled$data) &&
      !"pooled_ramp_group_col" %in% names(pb_call)) {
    pb_call$pooled_ramp_group_col <- as.character(geo_col)[1]
  }
  prior_out <- do.call(prior_builder, pb_call)
  prior_out <- apply_data_granularity_adjustments_to_prior_output(prior_out, granularity)

  metadata <- make_hier_metadata_from_prior_output(prior_out)
  if (!is.null(benchmark_priors)) {
    metadata <- apply_benchmark_priors_to_metadata(
      metadata,
      benchmark_priors = benchmark_priors,
      benchmark_precision_weight = benchmark_precision_weight
    )
    prior_out$metadata_handoff_benchmark_blended <- metadata
  }

  audit <- make_mmm_prior_audit(
    prior_output = prior_out,
    granularity_audit = granularity,
    mix_diagnostic = mix_diag
  )
  evidence_summary <- summarize_mmm_prior_evidence_layers(prior_out, audit)
  response_curves <- build_prior_response_curves(
    prior_output = prior_out,
    multipliers = response_curve_multipliers,
    dep_mean = mean(suppressWarnings(as.numeric(training_scaled$data[[training_scaled$dep_var_col]])), na.rm = TRUE),
    raw_data = training_scaled$data
  )

  out <- list(
    data = scaled$data,
    training_data = training_scaled$data,
    holdout_data = holdout_split$holdout_data,
    holdout_audit = holdout_split$audit,
    variable_map = scaled$variable_map,
    dep_var_col = scaled$dep_var_col,
    market_size_scaling = scaled,
    granularity = granularity,
    mix_diagnostic = mix_diag,
    curve_anchors = curve_anchors,
    prior_output = prior_out,
    metadata = metadata,
    prior_audit = audit,
    evidence_summary = evidence_summary,
    response_curves = response_curves,
    recommended_next_step = unique(audit$analyst_action)
  )
  write_mmm_prior_workflow_outputs(out, output_dir = output_dir, prefix = output_prefix)
  out
}

make_mmm_calibration_template <- function(variables,
                                          include_geo = TRUE,
                                          geo_col = "geo",
                                          include_group = TRUE,
                                          group_col = "mod_id") {
  variables <- as.character(variables)
  out <- data.table::data.table(
    calibration_id = paste0("calib_", seq_along(variables)),
    variable = variables,
    start_period = as.Date(NA),
    end_period = as.Date(NA),
    observed_lift = NA_real_,
    observed_lift_sd = NA_real_,
    observed_lift_precision = NA_real_,
    evidence_source = NA_character_,
    evidence_notes = NA_character_
  )
  if (isTRUE(include_geo)) out[, (geo_col) := NA_character_]
  if (isTRUE(include_group)) out[, (group_col) := NA_character_]
  out[]
}

make_mmm_business_prior_template <- function(variables) {
  variables <- as.character(variables)
  data.table::data.table(
    variable = variables,
    prior_metric = NA_character_,       # coef, roi, mroi, ikpc, cpkpi, outcome_per_cost, cost_per_outcome
    prior_mean = NA_real_,
    prior_sd = NA_real_,
    prior_precision = NA_real_,
    prior_distribution = "normal",
    kpi_value_per_outcome = NA_real_,
    evidence_source = NA_character_,
    evidence_notes = NA_character_
  )
}

mpw_metric_alias <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  out <- x
  out[x %in% c("coefficient", "beta", "model_coef", "coef_prior")] <- "coef"
  out[x %in% c("incremental_kpi_per_cost", "incremental_outcome_per_cost", "incremental_kpi_per_dollar", "ikpc", "kpi_per_cost", "kpi_per_dollar", "outcome_per_dollar")] <- "ikpc"
  out[x %in% c("cost_per_kpi", "cost_per_outcome", "cost_per_conversion", "cost_per_subscriber", "cost_per_lead", "cpa", "cpkpi")] <- "cpkpi"
  out[x %in% c("outcome_per_cost", "outcomes_per_cost")] <- "outcome_per_cost"
  out[x %in% c("roi", "return_on_investment")] <- "roi"
  out[x %in% c("mroi", "marginal_roi", "marginal_return_on_investment")] <- "mroi"
  out
}

mpw_prior_distribution_for_row <- function(kp, i) {
  dist_cols <- intersect(c("prior_distribution", "distribution", "prior_family", "prior_function", "distribution_function"), names(kp))
  if (!length(dist_cols)) return("normal")
  out <- as.character(kp[[dist_cols[1]]][i])
  if (is.na(out) || !nzchar(trimws(out))) "normal" else trimws(out)
}

mpw_row_num <- function(kp, i, candidates) {
  candidates <- candidates[candidates %in% names(kp)]
  for (cc in candidates) {
    val <- suppressWarnings(as.numeric(kp[[cc]][i]))
    if (is.finite(val)) return(val)
  }
  NA_real_
}

mpw_metric_candidates <- function(metric) {
  switch(
    metric,
    coef = c("prior_mean", "coef", "coefficient", "beta", "model_coef", "coef_prior"),
    ikpc = c("prior_mean", "ikpc", "incremental_kpi_per_cost", "incremental_outcome_per_cost", "kpi_per_cost", "kpi_per_dollar"),
    cpkpi = c("prior_mean", "cpkpi", "cost_per_kpi", "cost_per_outcome", "cost_per_subscriber", "cost_per_lead", "cpa"),
    outcome_per_cost = c("prior_mean", "outcome_per_cost", "kpi_per_cost", "ikpc"),
    roi = c("prior_mean", "roi"),
    mroi = c("prior_mean", "mroi", "marginal_roi"),
    c("prior_mean", metric)
  )
}

mpw_metric_sd_candidates <- function(metric) {
  base <- mpw_metric_candidates(metric)
  unique(c("prior_sd", paste0(base, "_sd"), paste0(base, "_se")))
}

mpw_metric_precision_candidates <- function(metric) {
  base <- mpw_metric_candidates(metric)
  unique(c("prior_precision", paste0(base, "_precision")))
}

mpw_detect_prior_metric <- function(kp, i) {
  if ("prior_metric" %in% names(kp)) {
    pm <- mpw_metric_alias(kp$prior_metric[i])
    if (nzchar(pm) && !is.na(pm)) return(pm)
  }
  if ("metric" %in% names(kp)) {
    pm <- mpw_metric_alias(kp$metric[i])
    if (nzchar(pm) && !is.na(pm)) return(pm)
  }
  candidates <- c("coef", "ikpc", "outcome_per_cost", "cpkpi", "roi", "mroi")
  for (metric in candidates) {
    if (is.finite(mpw_row_num(kp, i, mpw_metric_candidates(metric)))) return(metric)
  }
  NA_character_
}

mpw_kpi_value_for_row <- function(kpi_value_per_outcome, kp, i, variable) {
  if ("kpi_value_per_outcome" %in% names(kp)) {
    val <- suppressWarnings(as.numeric(kp$kpi_value_per_outcome[i]))
    if (is.finite(val) && val > 0) return(val)
  }
  if (length(kpi_value_per_outcome) > 1L && !is.null(names(kpi_value_per_outcome))) {
    val <- suppressWarnings(as.numeric(kpi_value_per_outcome[[as.character(variable)]]))
    if (is.finite(val) && val > 0) return(val)
  }
  val <- suppressWarnings(as.numeric(kpi_value_per_outcome)[1])
  if (!is.finite(val) || val <= 0) stop("kpi_value_per_outcome must be positive.")
  val
}

make_coef_benchmark_priors_from_kpi_economics <- function(input_data,
                                                          prior_output,
                                                          kpi_priors,
                                                          dep_var_col,
                                                          spend_map = NULL,
                                                          kpi_value_per_outcome = 1,
                                                          default_relative_sd = 0.50,
                                                          min_outcome_per_cost_sd = 1e-8,
                                                          min_coef_sd = 1e-6,
                                                          max_precision = Inf) {
  if (!is.list(prior_output) || is.null(prior_output$metadata_handoff) || is.null(prior_output$transformed_x_handoff)) {
    stop("prior_output must be returned by prior_builder() and include transformed_x_handoff.")
  }
  dt <- mpw_as_dt(input_data, "input_data")
  md <- mpw_as_dt(prior_output$metadata_handoff, "prior_output$metadata_handoff")
  tx <- mpw_as_dt(prior_output$transformed_x_handoff, "prior_output$transformed_x_handoff")
  kp <- mpw_as_dt(kpi_priors, "kpi_priors")
  if (!dep_var_col %in% names(dt)) stop("dep_var_col not found in input_data: ", dep_var_col)
  if (!"variable" %in% names(kp)) stop("kpi_priors must contain variable.")
  detected_metrics <- vapply(seq_len(nrow(kp)), function(i) mpw_detect_prior_metric(kp, i), character(1))
  if (!any(!is.na(detected_metrics) & nzchar(detected_metrics))) {
    stop("kpi_priors must contain prior_metric/prior_mean or one of: coef, roi, mroi, ikpc, cpkpi, outcome_per_cost, cost_per_outcome.")
  }
  kp[, variable := as.character(variable)]
  if (anyDuplicated(kp$variable)) stop("kpi_priors contains duplicate variable rows: ", paste(unique(kp$variable[duplicated(kp$variable)]), collapse = ", "))
  default_relative_sd <- as.numeric(default_relative_sd)[1]
  if (!is.finite(default_relative_sd) || default_relative_sd <= 0) default_relative_sd <- 0.50
  max_precision <- suppressWarnings(as.numeric(max_precision)[1])
  if (!is.finite(max_precision) || max_precision <= 0) max_precision <- Inf

  if (is.null(spend_map)) {
    spend_map <- md[, .(variable, spend_col = if ("spend_col" %in% names(md)) as.character(spend_col) else NA_character_)]
  } else {
    spend_map <- mpw_as_dt(spend_map, "spend_map")
    if (!all(c("variable", "spend_col") %in% names(spend_map))) stop("spend_map must contain variable and spend_col.")
  }
  spend_map[, variable := as.character(variable)]

  y_mean <- mean(suppressWarnings(as.numeric(dt[[dep_var_col]])), na.rm = TRUE)
  if (!is.finite(y_mean) || abs(y_mean) <= 1e-8) stop("dep_var_col has invalid mean; cannot convert KPI economics to coefficient prior.")

  rows <- lapply(seq_len(nrow(kp)), function(i) {
    v <- kp$variable[i]
    prior_metric <- mpw_detect_prior_metric(kp, i)
    prior_distribution <- mpw_prior_distribution_for_row(kp, i)
    input_prior_mean <- mpw_row_num(kp, i, mpw_metric_candidates(prior_metric))
    input_prior_sd <- mpw_row_num(kp, i, mpw_metric_sd_candidates(prior_metric))
    input_prior_precision <- mpw_row_num(kp, i, mpw_metric_precision_candidates(prior_metric))
    if ((!is.finite(input_prior_sd) || input_prior_sd <= 0) && is.finite(input_prior_precision) && input_prior_precision > 0) {
      input_prior_sd <- 1 / sqrt(input_prior_precision)
    }
    if (!is.finite(input_prior_mean)) {
      return(data.table::data.table(variable = v, warning = "invalid_prior_metric_mean"))
    }
    if (identical(prior_metric, "coef")) {
      coef <- input_prior_mean
      coef_sd <- input_prior_sd
      if (!is.finite(coef_sd) || coef_sd <= 0) coef_sd <- max(abs(coef) * default_relative_sd, min_coef_sd)
      coef_sd <- max(coef_sd, min_coef_sd)
      coef_precision_uncapped <- 1 / (coef_sd ^ 2)
      coef_precision <- min(coef_precision_uncapped, max_precision)
      return(data.table::data.table(
        variable = v,
        coef = coef,
        coef_sd = coef_sd,
        coef_precision = coef_precision,
        coef_precision_uncapped = coef_precision_uncapped,
        coef_precision_was_capped = is.finite(max_precision) && coef_precision_uncapped > max_precision,
        prior_metric = prior_metric,
        input_prior_metric = prior_metric,
        prior_distribution = prior_distribution,
        stan_prior_distribution = "normal_on_coefficient",
        input_prior_mean = input_prior_mean,
        input_prior_sd = coef_sd,
        input_prior_precision = if (is.finite(input_prior_precision)) input_prior_precision else coef_precision_uncapped,
        input_precision_preserved = is.finite(input_prior_precision) && input_prior_precision > 0,
        outcome_per_cost = NA_real_,
        outcome_per_cost_sd = NA_real_,
        cost_per_outcome = NA_real_,
        cost_per_outcome_sd = NA_real_,
        roi = NA_real_,
        roi_sd = NA_real_,
        mroi = NA_real_,
        mroi_sd = NA_real_,
        kpi_value_per_outcome = NA_real_,
        spend_col = NA_character_,
        spend_total = NA_real_,
        dep_mean = y_mean,
        transformed_x_sum = NA_real_,
        warning = NA_character_,
        prior_source = "direct_coef_prior"
      ))
    }

    sc <- spend_map[variable == v, spend_col][1]
    if (is.na(sc) || !nzchar(sc) || !(sc %in% names(dt))) {
      return(data.table::data.table(variable = v, warning = "missing_spend_col_for_kpi_economics_conversion"))
    }
    spend_total <- sum(pmax(suppressWarnings(as.numeric(dt[[sc]])), 0), na.rm = TRUE)
    x_sum <- tx[variable == v, sum(as.numeric(x_handoff), na.rm = TRUE)]
    if (!is.finite(spend_total) || spend_total <= 0 || !is.finite(x_sum) || x_sum <= 0) {
      return(data.table::data.table(variable = v, warning = "invalid_spend_or_transformed_x_for_kpi_economics_conversion"))
    }

    kpi_value_i <- mpw_kpi_value_for_row(kpi_value_per_outcome, kp, i, v)
    outcome_per_cost <- NA_real_
    outcome_per_cost_sd <- NA_real_
    cost_per_outcome <- NA_real_
    cost_per_outcome_sd <- NA_real_
    roi <- NA_real_
    roi_sd <- NA_real_
    mroi <- NA_real_
    mroi_sd <- NA_real_

    if (prior_metric %in% c("ikpc", "outcome_per_cost")) {
      outcome_per_cost <- input_prior_mean
      outcome_per_cost_sd <- input_prior_sd
    } else if (identical(prior_metric, "cpkpi")) {
      cost_per_outcome <- input_prior_mean
      cost_per_outcome_sd <- input_prior_sd
      outcome_per_cost <- 1 / cost_per_outcome
      if (is.finite(cost_per_outcome_sd) && cost_per_outcome_sd > 0) {
        outcome_per_cost_sd <- cost_per_outcome_sd / (cost_per_outcome ^ 2)
      }
    } else if (identical(prior_metric, "roi")) {
      roi <- input_prior_mean
      roi_sd <- input_prior_sd
      outcome_per_cost <- roi / kpi_value_i
      if (is.finite(roi_sd) && roi_sd > 0) outcome_per_cost_sd <- roi_sd / kpi_value_i
    } else if (identical(prior_metric, "mroi")) {
      mroi <- input_prior_mean
      mroi_sd <- input_prior_sd
      outcome_per_cost <- mroi / kpi_value_i
      if (is.finite(mroi_sd) && mroi_sd > 0) outcome_per_cost_sd <- mroi_sd / kpi_value_i
    }

    if (!is.finite(outcome_per_cost) || outcome_per_cost <= 0 || prior_metric %in% c(NA_character_, "")) {
      return(data.table::data.table(variable = v, warning = "invalid_kpi_economics_prior"))
    }
    if (!is.finite(outcome_per_cost_sd) || outcome_per_cost_sd <= 0) {
      outcome_per_cost_sd <- max(abs(outcome_per_cost) * default_relative_sd, min_outcome_per_cost_sd)
    }

    coef <- (outcome_per_cost * spend_total) / (y_mean * x_sum)
    coef_sd <- max((outcome_per_cost_sd * spend_total) / (y_mean * x_sum), min_coef_sd)
    coef_precision_uncapped <- 1 / (coef_sd ^ 2)
    coef_precision <- min(coef_precision_uncapped, max_precision)
    data.table::data.table(
      variable = v,
      coef = coef,
      coef_sd = coef_sd,
      coef_precision = coef_precision,
      coef_precision_uncapped = coef_precision_uncapped,
      coef_precision_was_capped = is.finite(max_precision) && coef_precision_uncapped > max_precision,
      prior_metric = prior_metric,
      input_prior_metric = prior_metric,
      prior_distribution = prior_distribution,
      stan_prior_distribution = "normal_delta_method_on_coefficient",
      input_prior_mean = input_prior_mean,
      input_prior_sd = input_prior_sd,
      input_prior_precision = input_prior_precision,
      input_precision_preserved = is.finite(input_prior_precision) && input_prior_precision > 0,
      outcome_per_cost = outcome_per_cost,
      outcome_per_cost_sd = outcome_per_cost_sd,
      cost_per_outcome = if (is.finite(cost_per_outcome)) cost_per_outcome else 1 / outcome_per_cost,
      cost_per_outcome_sd = cost_per_outcome_sd,
      roi = if (is.finite(roi)) roi else outcome_per_cost * kpi_value_i,
      roi_sd = if (is.finite(roi_sd)) roi_sd else outcome_per_cost_sd * kpi_value_i,
      mroi = mroi,
      mroi_sd = mroi_sd,
      kpi_value_per_outcome = kpi_value_i,
      spend_col = sc,
      spend_total = spend_total,
      dep_mean = y_mean,
      transformed_x_sum = x_sum,
      economic_prior_basis = if (identical(prior_metric, "mroi")) "marginal_metric_delta_method" else "average_metric_delta_method",
      warning = NA_character_,
      prior_source = "kpi_economics_to_coef_conversion"
    )
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)[]
}

make_coef_benchmark_priors_from_roi <- function(input_data,
                                                prior_output,
                                                roi_priors,
                                                dep_var_col,
                                                revenue_per_kpi = 1,
                                                spend_map = NULL,
                                                min_coef_sd = 1e-6,
                                                max_precision = Inf) {
  make_coef_benchmark_priors_from_kpi_economics(
    input_data = input_data,
    prior_output = prior_output,
    kpi_priors = roi_priors,
    dep_var_col = dep_var_col,
    spend_map = spend_map,
    kpi_value_per_outcome = revenue_per_kpi,
    min_coef_sd = min_coef_sd,
    max_precision = max_precision
  )
}

make_coef_benchmark_priors_from_business_priors <- function(input_data,
                                                            prior_output,
                                                            business_priors,
                                                            dep_var_col,
                                                            spend_map = NULL,
                                                            kpi_value_per_outcome = 1,
                                                            default_relative_sd = 0.50,
                                                            min_coef_sd = 1e-6,
                                                            max_precision = Inf) {
  make_coef_benchmark_priors_from_kpi_economics(
    input_data = input_data,
    prior_output = prior_output,
    kpi_priors = business_priors,
    dep_var_col = dep_var_col,
    spend_map = spend_map,
    kpi_value_per_outcome = kpi_value_per_outcome,
    default_relative_sd = default_relative_sd,
    min_coef_sd = min_coef_sd,
    max_precision = max_precision
  )
}

build_prior_response_curves <- function(prior_output,
                                        multipliers = seq(0, 2, by = 0.05),
                                        dep_mean = 1,
                                        raw_data = NULL,
                                        spend_map = NULL) {
  if (!is.list(prior_output) || is.null(prior_output$metadata_handoff) || is.null(prior_output$transformed_x_handoff)) {
    stop("prior_output must be returned by prior_builder() and include transformed_x_handoff.")
  }
  md <- mpw_as_dt(prior_output$metadata_handoff, "prior_output$metadata_handoff")
  tx <- mpw_as_dt(prior_output$transformed_x_handoff, "prior_output$transformed_x_handoff")
  multipliers <- sort(unique(as.numeric(multipliers)))
  multipliers <- multipliers[is.finite(multipliers) & multipliers >= 0]
  if (!length(multipliers)) stop("multipliers must contain non-negative numeric values.")
  dep_mean <- as.numeric(dep_mean)[1]
  if (!is.finite(dep_mean) || dep_mean <= 0) dep_mean <- 1

  spend_dt <- data.table::data.table(variable = md$variable, spend_total_base = NA_real_)
  if (!is.null(raw_data)) {
    raw <- mpw_as_dt(raw_data, "raw_data")
    if (is.null(spend_map)) {
      spend_map <- md[, .(variable, spend_col = if ("spend_col" %in% names(md)) as.character(spend_col) else NA_character_)]
    } else {
      spend_map <- mpw_as_dt(spend_map, "spend_map")
    }
    if (all(c("variable", "spend_col") %in% names(spend_map))) {
      spend_rows <- lapply(seq_len(nrow(spend_map)), function(i) {
        sc <- as.character(spend_map$spend_col[i])
        if (!is.na(sc) && nzchar(sc) && sc %in% names(raw)) {
          data.table::data.table(variable = as.character(spend_map$variable[i]), spend_total_base = sum(pmax(as.numeric(raw[[sc]]), 0), na.rm = TRUE))
        } else {
          data.table::data.table(variable = as.character(spend_map$variable[i]), spend_total_base = NA_real_)
        }
      })
      spend_dt <- data.table::rbindlist(spend_rows, use.names = TRUE, fill = TRUE)
    }
  }

  rows <- lapply(seq_len(nrow(md)), function(i) {
    v <- md$variable[i]
    x <- tx[variable == v, as.numeric(raw_x)]
    if (!length(x)) return(NULL)
    coef <- as.numeric(md$coef[i])
    rr <- as.numeric(md$rrate[i])
    cv <- as.numeric(md$cvalue[i])
    dv <- as.numeric(md$dvalue[i])
    has_curve <- isTRUE(as.logical(md$has_curve[i]))
    base_spend <- spend_dt[variable == v, spend_total_base][1]

    if (!is.finite(coef)) return(NULL)
    if (!has_curve) {
      base_x <- x
      base_den <- mean(base_x, na.rm = TRUE)
      if (!is.finite(base_den) || abs(base_den) <= 1e-8) base_den <- 1
      curve <- lapply(multipliers, function(m) {
        tr <- (base_x * m) / base_den
        avg_idx <- mean(tr, na.rm = TRUE)
        data.table::data.table(variable = v, multiplier = m, mean_transformed_index = avg_idx)
      })
    } else {
      base_ad <- mpw_adstock(x, rr)
      base_ad_mean <- mean(base_ad, na.rm = TRUE)
      if (!is.finite(base_ad_mean) || base_ad_mean <= 1e-8) base_ad_mean <- 1
      base_sat <- mpw_saturation(base_ad / base_ad_mean, cv, dv)
      base_sat_mean <- mean(base_sat, na.rm = TRUE)
      if (!is.finite(base_sat_mean) || base_sat_mean <= 1e-8) base_sat_mean <- 1
      curve <- lapply(multipliers, function(m) {
        tr <- mpw_saturation(mpw_adstock(x * m, rr) / base_ad_mean, cv, dv) / base_sat_mean
        data.table::data.table(variable = v, multiplier = m, mean_transformed_index = mean(tr, na.rm = TRUE))
      })
    }
    out <- data.table::rbindlist(curve, use.names = TRUE, fill = TRUE)
    out[, `:=`(
      coef = coef,
      avg_contribution_index = coef * mean_transformed_index,
      total_contribution_units = coef * mean_transformed_index * dep_mean * length(x),
      spend_total = if (is.finite(base_spend)) base_spend * multiplier else NA_real_
    )]
    out[, incremental_contribution_units_vs_zero := total_contribution_units - total_contribution_units[multiplier == min(multiplier)][1]]
    out[, outcome_per_cost := data.table::fifelse(is.finite(spend_total) & spend_total > 0, total_contribution_units / spend_total, NA_real_)]
    out[, cost_per_outcome := data.table::fifelse(is.finite(total_contribution_units) & total_contribution_units > 0, spend_total / total_contribution_units, NA_real_)]
    out[]
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)[]
}
