#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

script_dir <- tryCatch({
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
pilot_dir <- normalizePath(script_dir, mustWork = TRUE)
bundle_dir <- normalizePath(file.path(pilot_dir, "..", "mmm_latest_scripts"), mustWork = TRUE)
output_dir <- file.path(pilot_dir, "outputs")
output_suffix <- Sys.getenv("PILOT_OUTPUT_SUFFIX", "")
if (nzchar(output_suffix)) output_dir <- paste0(output_dir, "_", gsub("[^A-Za-z0-9_.-]+", "_", output_suffix))
data_dir <- file.path(pilot_dir, "data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(bundle_dir, "mmm_workflow.R"), chdir = TRUE)

run_stan <- tolower(Sys.getenv("RUN_STAN", "true")) %in% c("1", "true", "yes", "y")
seed <- as.integer(Sys.getenv("PILOT_SEED", "123"))
geo_n <- as.integer(Sys.getenv("PILOT_GEO_N", "12"))
week_n <- as.integer(Sys.getenv("PILOT_WEEK_N", "104"))
channel_n <- as.integer(Sys.getenv("PILOT_CHANNEL_N", "3"))
pilot_likelihood <- Sys.getenv("PILOT_LIKELIHOOD", "normal")
pilot_init_strategy <- Sys.getenv("PILOT_INIT_STRATEGY", "pathfinder")
pilot_use_pathfinder <- tolower(Sys.getenv("PILOT_USE_PATHFINDER", ifelse(pilot_init_strategy == "pathfinder", "true", "false"))) %in% c("1", "true", "yes", "y")
pilot_use_warm_start <- tolower(Sys.getenv("PILOT_USE_WARM_START", "true")) %in% c("1", "true", "yes", "y")
pilot_intercept_type <- Sys.getenv("PILOT_INTERCEPT_TYPE", "fourier")
pilot_coef_parameterization <- Sys.getenv("PILOT_COEF_PARAMETERIZATION", "noncentered")
pilot_alpha_parameterization <- Sys.getenv("PILOT_ALPHA_PARAMETERIZATION", "noncentered")
pilot_ucm_parameterization <- Sys.getenv("PILOT_UCM_PARAMETERIZATION", "noncentered")
pilot_center_predictors <- tolower(Sys.getenv("PILOT_CENTER_PREDICTORS", "true")) %in% c("1", "true", "yes", "y")
pilot_sample_coef_hierarchy <- Sys.getenv("PILOT_SAMPLE_COEF_HIERARCHY", "never")
pilot_sample_curve_parameters <- Sys.getenv("PILOT_SAMPLE_CURVE_PARAMETERS", "never")
pilot_run_quasi_stan <- tolower(Sys.getenv("PILOT_RUN_QUASI_STAN", "false")) %in% c("1", "true", "yes", "y")
pilot_bound_one_sided <- tolower(Sys.getenv("PILOT_BOUND_ONE_SIDED", "true")) %in% c("1", "true", "yes", "y")
pilot_max_treedepth <- as.integer(Sys.getenv("PILOT_MAX_TREEDEPTH", "10"))
pilot_metric <- Sys.getenv("PILOT_METRIC", "diag_e")
pilot_adapt_delta <- as.numeric(Sys.getenv("PILOT_ADAPT_DELTA", "0.90"))
iter_warmup <- as.integer(Sys.getenv("PILOT_ITER_WARMUP", "150"))
iter_sampling <- as.integer(Sys.getenv("PILOT_ITER_SAMPLING", "150"))
set.seed(seed)

data_url <- "https://raw.githubusercontent.com/google/meridian/refs/heads/main/meridian/data/simulated_data/csv/geo_all_channels.csv"
data_path <- file.path(data_dir, "google_meridian_geo_all_channels.csv")
if (!file.exists(data_path)) {
  download.file(data_url, data_path, mode = "wb", quiet = TRUE)
}

raw <- fread(data_path)
raw[, week := as.Date(time)]
raw[, entity := "public_meridian_sim"]
setorder(raw, geo, week)

channel_n <- max(1L, min(channel_n, 5L))
media_vars <- paste0("Channel", seq_len(channel_n) - 1L, "_impression")
spend_cols <- paste0("Channel", seq_len(channel_n) - 1L, "_spend")
control_vars <- c("competitor_sales_control", "sentiment_score_control", "Promo")
needed <- c("geo", "week", "entity", "conversions", media_vars, spend_cols, control_vars, "population")
miss <- setdiff(needed, names(raw))
if (length(miss)) stop("Missing expected columns in public dataset: ", paste(miss, collapse = ", "))

geo_keep <- sort(unique(raw$geo))[seq_len(min(geo_n, uniqueN(raw$geo)))]
week_keep <- sort(unique(raw$week))[seq_len(min(week_n, uniqueN(raw$week)))]
dt <- raw[geo %in% geo_keep & week %in% week_keep, ..needed]
dt[, holdout := week %in% tail(sort(unique(week)), 12)]
fwrite(dt, file.path(output_dir, "pilot_model_data.csv"))

variable_map <- data.table(
  variable = media_vars,
  modeled_x_col = media_vars,
  spend_col = spend_cols
)

qgt <- run_quasi_geo_test(
  input_data = dt,
  date_col = "week",
  dep_var_col = "conversions",
  geo_col = "geo",
  holdout_col = "holdout",
  variable_map = variable_map,
  control_cols = control_vars,
  normalize = "geo_mean_index",
  pre_weeks = 8,
  post_weeks = 4,
  min_pct_change = 0.35,
  min_robust_z = 1.5,
  min_donors = 4,
  output_dir = file.path(output_dir, "quasi_geo"),
  prefix = "meridian_public"
)

handoff <- qgt_build_stan_prior_handoff(qgt, min_evidence_score = 35)
fwrite(handoff, file.path(output_dir, "quasi_geo_stan_prior_handoff.csv"))

base_metadata <- rbindlist(list(
  data.table(
    variable = media_vars,
    role = "media",
    source_entity = "GLOBAL",
    curve_type = "weibull",
    rrate = rep(c(0.20, 0.25, 0.30, 0.18, 0.22), length.out = length(media_vars)),
    rrate_precision = 9,
    cvalue = 1.0,
    cvalue_precision = 4,
    dvalue = 1.0,
    dvalue_precision = 25,
    coef = 0.03,
    coef_precision = 4,
    coef_bound = "pos",
    coef_hierarchy_scale = 0.10,
    spend_col = spend_cols
  ),
  data.table(
    variable = control_vars,
    role = "control",
    source_entity = "GLOBAL",
    curve_type = "weibull",
    rrate = 0,
    rrate_precision = 1,
    cvalue = 0,
    cvalue_precision = 1,
    dvalue = 0,
    dvalue_precision = 1,
    coef = 0,
    coef_precision = 1,
    coef_bound = "",
    coef_hierarchy_scale = 0,
    spend_col = NA_character_
  )
), fill = TRUE)

qgt_metadata <- if (nrow(handoff)) {
  qgt_apply_stan_prior_handoff(base_metadata, handoff, overwrite_existing = TRUE)
} else {
  copy(base_metadata)
}
fwrite(base_metadata, file.path(output_dir, "stan_metadata_baseline.csv"))
fwrite(qgt_metadata, file.path(output_dir, "stan_metadata_quasi_geo.csv"))

summarize_qgt <- function(x) {
  all_events <- as.data.table(x$event_estimates_all)
  data.table(
    candidate_event_n = nrow(all_events),
    estimable_event_n = all_events[is.finite(incremental_outcome), .N],
    calibration_event_n = nrow(x$calibration_events),
    directional_prior_event_n = nrow(x$directional_prior_events),
    diagnostic_event_n = nrow(x$diagnostic_events),
    stan_handoff_n = nrow(handoff),
    max_evidence_score = if (nrow(all_events)) max(all_events$evidence_score_0_100, na.rm = TRUE) else NA_real_
  )
}
qgt_summary <- summarize_qgt(qgt)
fwrite(qgt_summary, file.path(output_dir, "quasi_geo_summary.csv"))

fit_summary <- function(fit_obj, label) {
  wd <- as.data.table(fit_obj$wide_decomp)
  if (!"sample" %in% names(wd)) wd[, sample := ifelse(row_id %in% fit_obj$holdout_idx, "holdout", "train")]
  wd[, .(
    model = label,
    row_n = .N,
    rmse = sqrt(mean((y_actual - pred)^2, na.rm = TRUE)),
    mae = mean(abs(y_actual - pred), na.rm = TRUE),
    mape = mean(abs((y_actual - pred) / pmax(abs(y_actual), 1e-8)), na.rm = TRUE),
    r2 = 1 - sum((y_actual - pred)^2, na.rm = TRUE) / pmax(sum((y_actual - mean(y_actual, na.rm = TRUE))^2, na.rm = TRUE), 1e-8)
  ), by = sample]
}

if (isTRUE(run_stan)) {
  common_args <- list(
    data = dt,
    dep_var_col = "conversions",
    group_col = "geo",
    time_col = "week",
    entity_col = "entity",
    spend_map = variable_map[, .(variable, spend_col)],
    raw_output_data = dt,
    holdout_col = "holdout",
    intercept_type = pilot_intercept_type,
    likelihood = pilot_likelihood,
    estimate_dvalue = FALSE,
    sample_curve_parameters = pilot_sample_curve_parameters,
    sample_coef_hierarchy = pilot_sample_coef_hierarchy,
    bound_one_sided_coef_defaults = pilot_bound_one_sided,
    coef_parameterization = pilot_coef_parameterization,
    alpha_parameterization = pilot_alpha_parameterization,
    ucm_parameterization = pilot_ucm_parameterization,
    center_predictors_for_sampling = pilot_center_predictors,
    chains = 1,
    parallel_chains = 1,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = pilot_adapt_delta,
    max_treedepth = pilot_max_treedepth,
    metric = pilot_metric,
    init_strategy = pilot_init_strategy,
    use_pathfinder_init = pilot_use_pathfinder,
    use_ucm_warm_start_init = pilot_use_warm_start,
    refresh = 0,
    seed = seed,
    verbose = FALSE,
    output_variables = "lean"
  )

  fit_safely <- function(label, metadata, out_subdir, prefix) {
    tryCatch({
      fit <- do.call(fit_hier_mmm, c(common_args, list(
        metadata_input = metadata,
        output_dir = file.path(output_dir, out_subdir),
        output_prefix = prefix
      )))
      list(ok = TRUE, fit = fit, error = NA_character_)
    }, error = function(e) {
      fwrite(data.table(model = label, error = conditionMessage(e)), file.path(output_dir, paste0(label, "_stan_error.csv")))
      list(ok = FALSE, fit = NULL, error = conditionMessage(e))
    })
  }

  base_res <- fit_safely("baseline", base_metadata, "stan_baseline", "baseline")
  qgt_res <- if (isTRUE(pilot_run_quasi_stan)) {
    fit_safely("quasi_geo_prior", qgt_metadata, "stan_quasi_geo", "quasi_geo")
  } else {
    list(ok = FALSE, fit = NULL, error = "PILOT_RUN_QUASI_STAN=false")
  }

  comparison <- rbindlist(list(
    if (isTRUE(base_res$ok)) fit_summary(base_res$fit, "baseline") else data.table(model = "baseline", sample = NA_character_, error = base_res$error),
    if (isTRUE(qgt_res$ok)) fit_summary(qgt_res$fit, "quasi_geo_prior") else data.table(model = "quasi_geo_prior", sample = NA_character_, error = qgt_res$error)
  ), fill = TRUE)
  fwrite(comparison, file.path(output_dir, "stan_ab_fit_comparison.csv"))
  if (isTRUE(base_res$ok)) fwrite(as.data.table(base_res$fit$diagnostics$sampler_overall)[, model := "baseline"], file.path(output_dir, "stan_baseline_sampler_overall.csv"))
  if (isTRUE(qgt_res$ok)) fwrite(as.data.table(qgt_res$fit$diagnostics$sampler_overall)[, model := "quasi_geo_prior"], file.path(output_dir, "stan_quasi_geo_sampler_overall.csv"))

  deck_fit <- if (isTRUE(qgt_res$ok)) qgt_res$fit else if (isTRUE(base_res$ok)) base_res$fit else NULL
  if (!is.null(deck_fit)) {
    deck <- run_mmm_deck_output_builder(
      long_decomp = deck_fit$long_decomp,
      wide_decomp = deck_fit$wide_decomp,
      raw_data = dt,
      spend_map = variable_map[, .(variable, spend_col)],
      channel_map = data.table(variable = media_vars, channel = paste("Channel", 0:2), role = "media"),
      output_dir = file.path(output_dir, "deck_quasi_geo"),
      prefix = "meridian_public",
      media_variables = media_vars,
      time_col = "week",
      group_col = "geo",
      entity_col = "entity",
      period_granularity = "month",
      write_html = TRUE,
      write_charts = TRUE,
      write_excel = FALSE,
      write_shiny = FALSE
    )
    invisible(deck)
  }
} else {
  fwrite(data.table(note = "RUN_STAN=false, Stan A/B and deck outputs skipped."), file.path(output_dir, "stan_skipped.csv"))
}

message("Public Meridian pilot complete. Outputs: ", output_dir)
print(qgt_summary)
if (file.exists(file.path(output_dir, "stan_ab_fit_comparison.csv"))) {
  print(fread(file.path(output_dir, "stan_ab_fit_comparison.csv")))
}
