# Opt-in known-truth validation for the staged empirical-Bayes workflow.
# It is intentionally excluded from routine tests because it fits nine Stan models.

test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
candidate_roots <- unique(c(
  if (!is.na(test_file) && file.exists(test_file)) dirname(dirname(test_file)) else character(),
  getwd(), file.path(getwd(), ".."), Sys.getenv("R_PACKAGE_DIR")
))
source_root <- candidate_roots[vapply(candidate_roots, function(path) {
  file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))
}, logical(1))]

if (Sys.getenv("ECONIMAP_RUN_SEQUENTIAL_STAN_VALIDATION", "0") != "1") {
  cat("Sequential Stan validation skipped. Set ECONIMAP_RUN_SEQUENTIAL_STAN_VALIDATION=1 to run.\n")
} else if (!requireNamespace("cmdstanr", quietly = TRUE) || !requireNamespace("posterior", quietly = TRUE)) {
  cat("Sequential Stan validation skipped: cmdstanr/posterior unavailable.\n")
} else if (inherits(try(cmdstanr::cmdstan_path(), silent = TRUE), "try-error")) {
  cat("Sequential Stan validation skipped: CmdStan is not installed.\n")
} else {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Sequential Stan validation requires data.table.")
  if (!"package:data.table" %in% search()) suppressPackageStartupMessages(library(data.table))
  if (length(source_root)) {
    invisible(lapply(sort(list.files(file.path(source_root[1], "R"), pattern = "[.]R$", full.names = TRUE)), source))
  } else {
    library(econimap)
  }

  sample_curve_mode <- match.arg(
    Sys.getenv("ECONIMAP_SEQUENTIAL_VALIDATION_CURVE_MODE", "always"),
    c("always", "never")
  )
  default_checkpoint_root <- if (length(source_root)) {
    file.path(dirname(normalizePath(source_root[1])), "econimap_validation_outputs")
  } else {
    file.path(getwd(), "econimap_validation_outputs")
  }
  checkpoint_root <- Sys.getenv("ECONIMAP_SEQUENTIAL_VALIDATION_OUTPUT_DIR", default_checkpoint_root)
  dir.create(checkpoint_root, recursive = TRUE, showWarnings = FALSE)
  resume_checkpoints <- Sys.getenv("ECONIMAP_SEQUENTIAL_VALIDATION_RESUME", "1") != "0"
  load_or_run <- function(path, run, manifest) {
    manifest_path <- sub("[.]rds$", "_manifest.rds", path)
    if (isTRUE(resume_checkpoints) && file.exists(path)) {
      if (!file.exists(manifest_path)) stop("Checkpoint exists without its content manifest: ", path, call. = FALSE)
      saved_manifest <- readRDS(manifest_path)
      if (!identical(saved_manifest$checkpoint_hash, manifest$checkpoint_hash)) {
        stop("Checkpoint manifest hash does not match its content-addressed path.", call. = FALSE)
      }
      cat("Resuming checkpoint: ", path, "\n", sep = "")
      return(readRDS(path))
    }
    value <- run()
    saveRDS(value, path)
    saveRDS(manifest, manifest_path)
    value
  }

  media_vars <- c("tv_support", "meta_support", "tiktok_support")
  spend_cols <- sub("_support$", "_spend", media_vars)
  truth_cols <- sub("_support$", "_truth", media_vars)

  make_panel <- function(regime, seed) {
    set.seed(seed)
    periods <- seq.Date(as.Date("2023-01-02"), by = "week", length.out = 104L)
    geos <- paste0("geo_", seq_len(6L))
    curve_spec <- data.table(
      variable = media_vars,
      rrate = c(0.25, 0.10, 0.05),
      cvalue = c(1.10, 1.30, 1.55),
      dvalue = 1,
      raw_beta = c(118, 76, 44)
    )
    out <- rbindlist(lapply(seq_along(geos), function(g) {
      n <- length(periods)
      week <- seq_len(n)
      common <- 1 + 0.24 * sin(week / 7) + 0.12 * cos(week / 15) + rnorm(n, sd = 0.06)
      geo_scale <- 0.75 + 0.11 * g
      if (regime == "clean_separated") {
        tv_spend <- pmax(8, geo_scale * (82 + 31 * sin(week / 5) + rnorm(n, sd = 10)))
        meta_spend <- pmax(6, geo_scale * (48 + 21 * cos(week / 8) + rnorm(n, sd = 8)))
        tiktok_spend <- pmax(4, geo_scale * (29 + 16 * sin(week / 11 + 1) + rnorm(n, sd = 6)))
        noise_sd <- 20
      } else if (regime == "collinear_mix_shift") {
        tv_spend <- pmax(8, geo_scale * (78 * common + 10 * sin(week / 4) + rnorm(n, sd = 5)))
        social_total <- pmax(8, geo_scale * (86 * common + rnorm(n, sd = 5)))
        mix <- plogis(-0.5 + 0.9 * sin(week / 10 + g / 3) + rnorm(n, sd = 0.18))
        meta_spend <- pmax(3, social_total * mix)
        tiktok_spend <- pmax(3, social_total * (1 - mix))
        noise_sd <- 28
      } else if (regime == "weak_common_trend") {
        shared <- pmax(8, geo_scale * (70 * common + rnorm(n, sd = 1.2)))
        tv_spend <- shared
        meta_spend <- pmax(3, 0.68 * shared + rnorm(n, sd = 0.7))
        tiktok_spend <- pmax(3, 0.40 * shared + rnorm(n, sd = 0.5))
        noise_sd <- 38
      } else {
        stop("Unknown regime: ", regime, call. = FALSE)
      }
      macro <- as.numeric(scale(0.55 * sin(week / 13) + rnorm(n, sd = 0.75)))
      dt <- data.table(
        period = periods,
        geo = geos[g],
        entity = "brand",
        tv_spend = tv_spend,
        meta_spend = meta_spend,
        tiktok_spend = tiktok_spend,
        tv_support = tv_spend,
        meta_support = meta_spend,
        tiktok_support = tiktok_spend,
        macro = macro
      )
      for (j in seq_len(nrow(curve_spec))) {
        spec <- curve_spec[j]
        transformed <- media_transform_hier_mmm(
          x = dt[[spec$variable]],
          rrate = spec$rrate,
          cvalue = spec$cvalue,
          dvalue = spec$dvalue,
          curve_type = "hill",
          normalize_curve_x = TRUE
        )$transformed
        dt[[truth_cols[j]]] <- spec$raw_beta * transformed
      }
      dt[, kpi := 820 + 19 * g + 24 * macro + rowSums(as.matrix(.SD)) + rnorm(.N, sd = noise_sd), .SDcols = truth_cols]
      dt
    }))
    truth_metadata <- data.table(
      variable = media_vars,
      true_rrate = curve_spec$rrate,
      true_cvalue = curve_spec$cvalue,
      true_dvalue = curve_spec$dvalue,
      true_raw_beta = curve_spec$raw_beta
    )
    generic_fit_metadata <- data.table(
      variable = c(media_vars, "macro"),
      role = c(rep("media", length(media_vars)), "control"),
      spend_col = c(spend_cols, NA_character_),
      rollup_path = c(
        "total_paid_media > video > tv",
        "total_paid_media > social > meta",
        "total_paid_media > social > tiktok",
        "business_controls > macro"
      ),
      has_curve = c(rep(1L, length(media_vars)), 0L),
      curve_type = c(rep("hill", length(media_vars)), NA_character_),
      rrate = c(rep(0.20, length(media_vars)), 0),
      rrate_precision = c(rep(16, length(media_vars)), 1),
      anchor_saturation = c(rep(0.50, length(media_vars)), NA_real_),
      anchor_saturation_precision = c(rep(4, length(media_vars)), NA_real_),
      cvalue_from_anchor = c(rep(TRUE, length(media_vars)), FALSE),
      dvalue = c(rep(1, length(media_vars)), 0),
      dvalue_precision = c(rep(25, length(media_vars)), 1),
      coef = c(rep(0.05, length(media_vars)), 0),
      coef_precision = c(rep(4, length(media_vars)), 1),
      coef_bound = c(rep("pos", length(media_vars)), "free")
    )
    oracle_fit_metadata <- data.table(
        variable = c(media_vars, "macro"),
        role = c(rep("media", length(media_vars)), "control"),
        spend_col = c(spend_cols, NA_character_),
        rollup_path = c(
          "total_paid_media > video > tv",
          "total_paid_media > social > meta",
          "total_paid_media > social > tiktok",
          "business_controls > macro"
        ),
        has_curve = c(rep(1L, length(media_vars)), 0L),
        curve_type = c(rep("hill", length(media_vars)), NA_character_),
        rrate = c(curve_spec$rrate, 0),
        cvalue = c(curve_spec$cvalue, 0),
        dvalue = c(curve_spec$dvalue, 0),
        coef = c(curve_spec$raw_beta, 0),
        coef_precision = c(rep(400, length(media_vars)), 4),
        coef_bound = c(rep("pos", length(media_vars)), "free")
      )
    list(
      data = out[],
      curve_spec = curve_spec,
      truth_metadata = truth_metadata[],
      generic_fit_metadata = generic_fit_metadata[],
      oracle_fit_metadata = oracle_fit_metadata[]
    )
  }

  sampler_summary <- function(fit_obj) {
    x <- as.data.table(fit_obj$diagnostics$sampler_overall %||% data.table())
    if (!nrow(x)) return(data.table())
    x[, .(
      divergences_total = as.numeric(divergences_total),
      treedepth_hits_total = as.numeric(treedepth_hits_total),
      min_bfmi = as.numeric(min_bfmi),
      max_rhat = as.numeric(max_rhat),
      min_ess_bulk = as.numeric(min_ess_bulk),
      model_readiness = as.character(fit_obj$diagnostics$model_readiness$model_readiness[1] %||% NA_character_)
    )]
  }

  recovery_summary <- function(fit_obj, panel, curve_spec, label, fitted_variable_map = NULL) {
    truth_source <- data.table(
      truth_variable = media_vars,
      true_contribution = vapply(truth_cols, function(x) sum(panel[[x]]), numeric(1)),
      true_spend = vapply(spend_cols, function(x) sum(panel[[x]]), numeric(1)),
      true_rrate = curve_spec$rrate,
      true_cvalue = curve_spec$cvalue,
      true_dvalue = curve_spec$dvalue,
      true_normalized_saturation = mapply(
        function(cvalue, dvalue) saturate_media_hier_mmm(1, cvalue, dvalue, "hill"),
        curve_spec$cvalue, curve_spec$dvalue
      )
    )
    if (is.null(fitted_variable_map)) {
      truth <- truth_source[, .(
        variable = truth_variable, true_contribution, true_spend, true_rrate,
        true_cvalue, true_dvalue, true_normalized_saturation
      )]
    } else {
      vm <- unique(as.data.table(fitted_variable_map)[, .(
        truth_variable = as.character(variable),
        variable = as.character(generated_variable)
      )])
      truth <- merge(vm, truth_source, by = "truth_variable", all.x = TRUE, sort = FALSE)[,
        .(
          true_contribution = sum(true_contribution),
          true_spend = sum(true_spend),
          true_rrate = mean(true_rrate),
          true_cvalue = mean(true_cvalue),
          true_dvalue = mean(true_dvalue),
          true_normalized_saturation = mean(true_normalized_saturation)
        ), by = variable]
    }
    truth[, true_effectiveness := true_contribution / true_spend]
    est <- as.data.table(fit_obj$long_decomp)[variable %in% truth$variable,
      .(estimated_contribution = sum(contribution, na.rm = TRUE)), by = variable]
    rows <- merge(truth, est, by = "variable", all.x = TRUE, sort = FALSE)
    pm <- fit_obj$posterior_means %||% extract_pm_from_fit_obj_hier_mmm(fit_obj)
    vl <- as.data.table(fit_obj$variable_lookup)[variable %in% truth$variable, .(variable, variable_idx)]
    curve_est <- vl[, .(
      variable,
      estimated_rrate = as.numeric(pm$rrate[variable_idx]),
      estimated_cvalue = as.numeric(pm$cvalue[variable_idx]),
      estimated_dvalue = as.numeric(pm$dvalue[variable_idx])
    )]
    curve_est[, estimated_normalized_saturation := mapply(
      function(cvalue, dvalue) saturate_media_hier_mmm(1, cvalue, dvalue, "hill"),
      estimated_cvalue, estimated_dvalue
    )]
    rows <- merge(rows, curve_est, by = "variable", all.x = TRUE, sort = FALSE)

    curve_draws <- as.data.table(fit_obj$response_curves_draws %||% data.table())
    contribution_intervals <- data.table(
      variable = character(), contribution_q05 = numeric(), contribution_q50 = numeric(),
      contribution_q95 = numeric(), posterior_contribution_width = numeric()
    )
    if (nrow(curve_draws) && all(c(".draw", "variable", "contribution") %in% names(curve_draws))) {
      if ("scope" %in% names(curve_draws)) curve_draws <- curve_draws[scope == "total"]
      if ("spend_multiplier" %in% names(curve_draws)) curve_draws <- curve_draws[abs(spend_multiplier - 1) < 1e-8]
      by_draw <- curve_draws[variable %in% truth$variable,
        .(contribution = sum(contribution, na.rm = TRUE)), by = .(variable, .draw)]
      contribution_intervals <- by_draw[, .(
        contribution_q05 = as.numeric(quantile(contribution, 0.05, na.rm = TRUE)),
        contribution_q50 = as.numeric(quantile(contribution, 0.50, na.rm = TRUE)),
        contribution_q95 = as.numeric(quantile(contribution, 0.95, na.rm = TRUE))
      ), by = variable]
      contribution_intervals[, posterior_contribution_width := contribution_q95 - contribution_q05]
    }
    rows <- merge(rows, contribution_intervals, by = "variable", all.x = TRUE, sort = FALSE)
    rows[, `:=`(
      estimated_effectiveness = estimated_contribution / true_spend,
      effectiveness_q05 = contribution_q05 / true_spend,
      effectiveness_q50 = contribution_q50 / true_spend,
      effectiveness_q95 = contribution_q95 / true_spend,
      contribution_interval_covered = true_contribution >= contribution_q05 & true_contribution <= contribution_q95,
      absolute_rrate_error = abs(estimated_rrate - true_rrate),
      absolute_saturation_error = abs(estimated_normalized_saturation - true_normalized_saturation),
      absolute_effectiveness_error = abs(estimated_effectiveness - true_effectiveness)
    )]

    movement <- as.data.table(fit_obj$sequential_prior_posterior_audit %||% data.table())
    if (nrow(movement)) {
      movement <- movement[, .(
        variable,
        prior_to_posterior_movement_sd = posterior_movement_prior_sd_units,
        prior_dominance_classification
      )]
    } else {
      movement <- as.data.table(fit_obj$diagnostics$prior_posterior_coef %||% data.table())
      if (nrow(movement)) {
        movement <- movement[variable %in% truth$variable, .(
          prior_to_posterior_movement_sd = mean(posterior_z_vs_prior, na.rm = TRUE),
          prior_dominance_classification = "generic_direct_prior_audit"
        ), by = variable]
      }
    }
    if (nrow(movement)) rows <- merge(rows, movement, by = "variable", all.x = TRUE, sort = FALSE)
    if (!("prior_to_posterior_movement_sd" %in% names(rows))) rows[, prior_to_posterior_movement_sd := NA_real_]
    if (!("prior_dominance_classification" %in% names(rows))) rows[, prior_dominance_classification := NA_character_]
    rows[, `:=`(
      true_share = true_contribution / sum(true_contribution),
      estimated_share = estimated_contribution / sum(estimated_contribution)
    )]
    rows[, `:=`(
      absolute_share_error = abs(estimated_share - true_share),
      model = label
    )]
    total_true <- sum(rows$true_contribution)
    total_est <- sum(rows$estimated_contribution)
    sampler <- sampler_summary(fit_obj)
    holdout <- as.data.table(fit_obj$diagnostics$fit_quality_overall %||% data.table())
    holdout <- if (nrow(holdout) && "sample" %in% names(holdout)) holdout[sample == "holdout"] else data.table()
    list(
      by_variable = rows[],
      overall = data.table(
        model = label,
        true_total_contribution = total_true,
        estimated_total_contribution = total_est,
        total_relative_error = abs(total_est - total_true) / total_true,
        share_mae = mean(rows$absolute_share_error),
        effectiveness_mae = mean(rows$absolute_effectiveness_error, na.rm = TRUE),
        rrate_mae = mean(rows$absolute_rrate_error, na.rm = TRUE),
        normalized_saturation_mae = mean(rows$absolute_saturation_error, na.rm = TRUE),
        contribution_interval_coverage = mean(rows$contribution_interval_covered, na.rm = TRUE),
        mean_posterior_contribution_width = mean(rows$posterior_contribution_width, na.rm = TRUE),
        mean_abs_prior_to_posterior_movement_sd = mean(abs(rows$prior_to_posterior_movement_sd), na.rm = TRUE),
        holdout_rmse = holdout$rmse[1],
        holdout_mae = holdout$mae[1],
        holdout_r2 = holdout$r2[1],
        divergences_total = sampler$divergences_total[1],
        treedepth_hits_total = sampler$treedepth_hits_total[1],
        min_bfmi = sampler$min_bfmi[1],
        max_rhat = sampler$max_rhat[1],
        min_ess_bulk = sampler$min_ess_bulk[1],
        model_readiness = sampler$model_readiness[1]
      )
    )
  }

  fit_args <- list(
    chains = 4L,
    parallel_chains = 4L,
    iter_warmup = 400L,
    iter_sampling = 300L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    metric = "dense_e",
    seed = 20260711L,
    refresh = 0L,
    verbose = FALSE,
    output_dir = NULL,
    init_strategy = "random",
    use_pathfinder_init = FALSE,
    sample_curve_parameters = sample_curve_mode,
    sample_coef_hierarchy = "auto",
    coef_parameterization = "noncentered",
    center_predictors_for_sampling = TRUE,
    likelihood = "student_t",
    intercept_type = "flat",
    ucm_spec = list(level = FALSE, season = FALSE, cycle = FALSE),
    create_frequency_response_curves = FALSE,
    create_response_curve_draws = TRUE,
    response_curve_draw_count = 80L,
    response_curve_multipliers = 1,
    response_curve_scope = "total"
    ,holdout_last_n = 13L
  )
  if (length(source_root)) fit_args$stan_file <- file.path(source_root[1], "inst", "stan", "hier_mmm.stan")
  validation_files <- if (length(source_root)) {
    c(
      sort(list.files(file.path(source_root[1], "R"), pattern = "[.]R$", full.names = TRUE)),
      file.path(source_root[1], "inst", "stan", "hier_mmm.stan"),
      test_file
    )
  } else test_file
  validation_files <- unique(validation_files[!is.na(validation_files) & file.exists(validation_files)])
  run_oracle <- Sys.getenv("ECONIMAP_SEQUENTIAL_VALIDATION_ORACLE", "0") == "1"

  all_overall <- list()
  all_by_variable <- list()
  all_gates <- list()
  regimes <- c("clean_separated", "collinear_mix_shift", "weak_common_trend")
  requested_regimes <- trimws(strsplit(Sys.getenv("ECONIMAP_SEQUENTIAL_VALIDATION_REGIMES", ""), ",", fixed = TRUE)[[1]])
  requested_regimes <- requested_regimes[nzchar(requested_regimes)]
  if (length(requested_regimes)) {
    unknown_regimes <- setdiff(requested_regimes, regimes)
    if (length(unknown_regimes)) stop("Unknown validation regime(s): ", paste(unknown_regimes, collapse = ", "), call. = FALSE)
    regimes <- requested_regimes
  }
  validation_seeds <- suppressWarnings(as.integer(trimws(strsplit(
    Sys.getenv("ECONIMAP_SEQUENTIAL_VALIDATION_SEEDS", "1,2,3"), ",", fixed = TRUE
  )[[1]])))
  validation_seeds <- unique(validation_seeds[is.finite(validation_seeds)])
  if (!length(validation_seeds)) stop("ECONIMAP_SEQUENTIAL_VALIDATION_SEEDS must contain at least one integer.", call. = FALSE)
  validation_jobs <- data.table::CJ(regime = regimes, validation_seed = validation_seeds, unique = TRUE)
  for (i in seq_len(nrow(validation_jobs))) {
    regime <- validation_jobs$regime[i]
    validation_seed <- validation_jobs$validation_seed[i]
    regime_index <- match(regime, c("clean_separated", "collinear_mix_shift", "weak_common_trend"))
    simulation_seed <- 1100L + 100L * regime_index + validation_seed
    sampling_seed <- 20260711L + validation_seed
    fit_args_i <- modifyList(fit_args, list(seed = sampling_seed))
    cat("\nRunning sequential known-truth validation: ", regime, ", seed=", validation_seed, "\n", sep = "")
    sim <- make_panel(regime, seed = simulation_seed)
    spend_map <- sim$generic_fit_metadata[role == "media", .(variable, spend_col)]
    validation_config <- list(
      regime = regime,
      validation_seed = validation_seed,
      simulation_seed = simulation_seed,
      sampling_seed = sampling_seed,
      root_seed = 2100L + validation_seed,
      parent_draw_seed = 3100L + validation_seed,
      fit_args = fit_args_i,
      sample_curve_mode = sample_curve_mode,
      root_fourier_harmonics = 0L,
      baseline_spec = econ_seq_baseline_contract(
        root_trend_spec = "none", root_fourier_harmonics = 0L,
        root_season_period = 52L, control_cols = "macro"
      ),
      media_scope_config = NULL,
      root_bootstrap_reps = 60L,
      root_block_length = 4L,
      sequential_target_layer = "leaf",
      prior_transfer_settings = c(
        "effectiveness_only",
        "effectiveness_adstock",
        "effectiveness_adstock_saturation"
      ),
      run_oracle = run_oracle
    )
    checkpoint_hash <- econ_seq_content_hash(
      data = sim$data,
      truth_metadata = sim$truth_metadata,
      generic_fit_metadata = sim$generic_fit_metadata,
      oracle_fit_metadata = sim$oracle_fit_metadata,
      rollup_map = sim$generic_fit_metadata[, .(variable, rollup_path)],
      media_scope_config = validation_config$media_scope_config,
      baseline_spec = validation_config$baseline_spec,
      prior_transfer_settings = validation_config$prior_transfer_settings,
      validation_config = validation_config,
      files = validation_files
    )
    manifest <- list(
      checkpoint_hash = checkpoint_hash,
      validation_config = validation_config,
      component_hashes = list(
        data = econ_seq_content_hash(sim$data),
        truth_metadata = econ_seq_content_hash(sim$truth_metadata),
        generic_fit_metadata = econ_seq_content_hash(sim$generic_fit_metadata),
        oracle_fit_metadata = econ_seq_content_hash(sim$oracle_fit_metadata),
        rollup_map = econ_seq_content_hash(sim$generic_fit_metadata[, .(variable, rollup_path)]),
        media_scope_config = econ_seq_content_hash(validation_config$media_scope_config),
        baseline_spec = econ_seq_content_hash(validation_config$baseline_spec),
        prior_transfer_settings = econ_seq_content_hash(validation_config$prior_transfer_settings),
        source_files = econ_seq_content_hash(files = validation_files)
      ),
      files = data.table(path = normalizePath(validation_files, winslash = "/", mustWork = TRUE),
                         md5 = unname(tools::md5sum(validation_files)))
    )
    regime_dir <- file.path(checkpoint_root, regime, paste0("seed_", validation_seed), checkpoint_hash)
    dir.create(regime_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(manifest, file.path(regime_dir, "validation_manifest.rds"))

    direct <- load_or_run(file.path(regime_dir, "direct_leaf_fit.rds"), function() {
      do.call(fit_hier_mmm, modifyList(fit_args_i, list(
        data = sim$data,
        metadata_input = sim$generic_fit_metadata,
        dep_var_col = "kpi",
        group_col = "geo",
        time_col = "period",
        entity_col = "entity",
        spend_map = spend_map,
        output_dir = file.path(regime_dir, "direct_leaf_cmdstan"),
        output_prefix = "direct_leaf"
      )))
    }, manifest)
    direct_eval <- recovery_summary(direct, sim$data, sim$curve_spec, "direct_leaf")
    fwrite(direct_eval$overall, file.path(regime_dir, "direct_leaf_recovery_overall.csv"))
    fwrite(direct_eval$by_variable, file.path(regime_dir, "direct_leaf_recovery_by_variable.csv"))

    oracle_eval <- NULL
    if (run_oracle) {
      oracle <- load_or_run(file.path(regime_dir, "direct_oracle_fit.rds"), function() {
        do.call(fit_hier_mmm, modifyList(fit_args_i, list(
          data = sim$data,
          metadata_input = sim$oracle_fit_metadata,
          dep_var_col = "kpi",
          group_col = "geo",
          time_col = "period",
          entity_col = "entity",
          spend_map = spend_map,
          output_dir = file.path(regime_dir, "direct_oracle_cmdstan"),
          output_prefix = "direct_oracle"
        )))
      }, manifest)
      oracle_eval <- recovery_summary(oracle, sim$data, sim$curve_spec, "direct_oracle_upper_bound")
      fwrite(oracle_eval$overall, file.path(regime_dir, "direct_oracle_recovery_overall.csv"))
      fwrite(oracle_eval$by_variable, file.path(regime_dir, "direct_oracle_recovery_by_variable.csv"))
    }

    sequential_modes <- validation_config$prior_transfer_settings
    staged_evals <- list()
    staged_gates <- list()
    staged_root_audit <- list()
    for (mode_i in seq_along(sequential_modes)) {
      transfer_mode <- sequential_modes[mode_i]
      model_label <- paste0("sequential_", transfer_mode)
      stage <- load_or_run(file.path(regime_dir, paste0(model_label, "_leaf_stage.rds")), function() {
        stage_fit_args <- modifyList(fit_args_i, list(
          output_dir = file.path(regime_dir, paste0(model_label, "_cmdstan")),
          output_prefix = model_label
        ))
        run_sequential_hierarchical_bayes(
          data = sim$data,
          metadata_input = sim$generic_fit_metadata,
          dep_var_col = "kpi",
          group_col = "geo",
          time_col = "period",
          entity_col = "entity",
          spend_map = spend_map,
          media_scope_config = validation_config$media_scope_config,
          root_control_cols = "macro",
          root_trend_spec = "none",
          root_fourier_harmonics = 0L,
          baseline_spec = validation_config$baseline_spec,
          holdout_last_n = 13L,
          root_bootstrap_reps = 60L,
          root_block_length = 4L,
          rollup_depth = "leaf",
          curve_transfer_mode = transfer_mode,
          fit_child = TRUE,
          child_fit_args = stage_fit_args,
          output_dir = file.path(regime_dir, paste0(model_label, "_audit")),
          seed = 2100L + validation_seed
        )
      }, manifest)
      staged_evals[[mode_i]] <- recovery_summary(stage$child_fit, sim$data, sim$curve_spec, model_label)
      fwrite(staged_evals[[mode_i]]$overall, file.path(regime_dir, paste0(model_label, "_recovery_overall.csv")))
      fwrite(staged_evals[[mode_i]]$by_variable, file.path(regime_dir, paste0(model_label, "_recovery_by_variable.csv")))
      staged_gates[[mode_i]] <- copy(stage$child_identification$by_variable)[, `:=`(
        regime = regime,
        model = model_label
      )]
      staged_root_audit[[mode_i]] <- data.table(
        model = model_label,
        root_curve_type = stage$root_fit$root_summary$root_curve_type[1],
        root_effectiveness_status = stage$root_fit$root_summary$root_effectiveness_status[1],
        depth_gate = stage$depth_gate$identification_recommendation[1]
      )
    }
    primary_overall <- rbindlist(c(list(direct_eval$overall), lapply(staged_evals, `[[`, "overall")), fill = TRUE)
    overall <- if (is.null(oracle_eval)) primary_overall else rbindlist(list(primary_overall, oracle_eval$overall), fill = TRUE)
    root_audit <- rbindlist(staged_root_audit, fill = TRUE)
    overall[root_audit, on = "model", `:=`(
      root_curve_type = i.root_curve_type,
      root_effectiveness_status = i.root_effectiveness_status,
      depth_gate = i.depth_gate
    )]
    overall[, `:=`(
      regime = regime,
      validation_seed = validation_seed
    )]
    overall[, validation_status := fifelse(
      divergences_total > 0 | (!is.na(max_rhat) & max_rhat > 1.05) | (!is.na(min_bfmi) & min_bfmi < 0.3),
      "sampler_review",
      fifelse(regime == "clean_separated" & (total_relative_error > 0.35 | share_mae > 0.18), "recovery_review", "measured")
    )]
    primary_by_variable <- rbindlist(c(list(direct_eval$by_variable), lapply(staged_evals, `[[`, "by_variable")), fill = TRUE)
    by_variable <- if (is.null(oracle_eval)) primary_by_variable else rbindlist(list(primary_by_variable, oracle_eval$by_variable), fill = TRUE)
    by_variable[, `:=`(regime = regime, validation_seed = validation_seed)]
    all_overall[[i]] <- overall
    all_by_variable[[i]] <- by_variable
    all_gates[[i]] <- rbindlist(staged_gates, fill = TRUE)[, validation_seed := validation_seed]
    fwrite(overall, file.path(regime_dir, "comparison_overall.csv"))
    fwrite(by_variable, file.path(regime_dir, "comparison_by_variable.csv"))
    fwrite(all_gates[[i]], file.path(regime_dir, "depth_gate_by_variable.csv"))
  }

  overall <- rbindlist(all_overall, fill = TRUE)
  by_variable <- rbindlist(all_by_variable, fill = TRUE)
  gates <- rbindlist(all_gates, fill = TRUE)
  fwrite(overall, file.path(checkpoint_root, "validation_overall.csv"))
  fwrite(by_variable, file.path(checkpoint_root, "validation_by_variable.csv"))
  fwrite(gates, file.path(checkpoint_root, "validation_depth_gates.csv"))
  print(overall)
  cat("\nVariable-level contribution recovery:\n")
  print(by_variable[, .(
    regime, validation_seed, model, variable,
    true_share, estimated_share, absolute_share_error,
    true_effectiveness, estimated_effectiveness, absolute_effectiveness_error,
    true_rrate, estimated_rrate, absolute_rrate_error,
    true_normalized_saturation, estimated_normalized_saturation, absolute_saturation_error,
    contribution_interval_covered, posterior_contribution_width,
    prior_to_posterior_movement_sd, prior_dominance_classification
  )])
  cat("\nSequential depth-gating diagnostics:\n")
  print(gates)

  if ("clean_separated" %in% regimes && any(overall$model == "direct_leaf") &&
      all(paste0("sequential_", c("effectiveness_only", "effectiveness_adstock", "effectiveness_adstock_saturation")) %in% overall$model)) {
    clean <- overall[regime == "clean_separated" & model != "direct_oracle_upper_bound"]
    stopifnot(all(clean[, .N, by = validation_seed]$N == 4L))
    stopifnot(all(is.finite(clean$total_relative_error)))
  }
  cat("Sequential hierarchical Bayes known-truth validation completed. Review validation_status; this is a measurement run, not a pass-through test.\n")
}
