test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
candidate_roots <- unique(c(
  if (!is.na(test_file) && file.exists(test_file)) dirname(dirname(test_file)) else character(),
  getwd(), file.path(getwd(), ".."), Sys.getenv("R_PACKAGE_DIR")
))
source_root <- candidate_roots[vapply(candidate_roots, function(path) {
  file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))
}, logical(1))]

if (Sys.getenv("ECONIMAP_RUN_STAN_SMOKE", "0") != "1") {
  cat("Sequential Stan smoke skipped. Set ECONIMAP_RUN_STAN_SMOKE=1 to run.\n")
} else if (!requireNamespace("cmdstanr", quietly = TRUE) || !requireNamespace("posterior", quietly = TRUE)) {
  cat("Sequential Stan smoke skipped: cmdstanr/posterior unavailable.\n")
} else if (inherits(try(cmdstanr::cmdstan_path(), silent = TRUE), "try-error")) {
  cat("Sequential Stan smoke skipped: CmdStan is not installed.\n")
} else {
  smoke_sentinel <- Sys.getenv("ECONIMAP_SMOKE_SENTINEL", "")
  if (nzchar(smoke_sentinel)) writeLines("started", smoke_sentinel)
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Sequential Stan smoke requires data.table.")
  if (!"package:data.table" %in% search()) suppressPackageStartupMessages(library(data.table))
  if (length(source_root)) {
    invisible(lapply(sort(list.files(file.path(source_root[1], "R"), pattern = "[.]R$", full.names = TRUE)), source))
  } else {
    library(econimap)
  }

  set.seed(902L)
  periods <- seq.Date(as.Date("2024-01-01"), by = "week", length.out = 52L)
  groups <- c("geo_a", "geo_b")
  panel <- rbindlist(lapply(seq_along(groups), function(group_i) {
    n <- length(periods)
    tv_spend <- pmax(4, 34 + 12 * sin(seq_len(n) / 4) + rnorm(n, sd = 4))
    meta_spend <- pmax(3, 24 + 9 * cos(seq_len(n) / 5) + rnorm(n, sd = 3))
    tiktok_spend <- pmax(2, 0.68 * meta_spend + rnorm(n, sd = 0.8))
    macro <- rnorm(n)
    out <- data.table(
      period = periods,
      geo = groups[group_i],
      entity = "brand",
      tv_spend = tv_spend,
      meta_spend = meta_spend,
      tiktok_spend = tiktok_spend,
      tv_support = tv_spend * 11,
      meta_support = meta_spend * 18,
      tiktok_support = tiktok_spend * 15,
      macro = macro
    )
    tv_curve <- media_transform_hier_mmm(out$tv_support, 0.25, 1.05, 1, "hill", normalize_curve_x = TRUE)$transformed
    meta_curve <- media_transform_hier_mmm(out$meta_support, 0.12, 1.30, 1, "hill", normalize_curve_x = TRUE)$transformed
    tiktok_curve <- media_transform_hier_mmm(out$tiktok_support, 0.08, 1.55, 1, "hill", normalize_curve_x = TRUE)$transformed
    out[, kpi := 700 + 25 * group_i + 120 * tv_curve + 72 * meta_curve +
      18 * tiktok_curve + 9 * macro + rnorm(n, sd = 10)]
    out
  }))

  metadata <- data.table(
    variable = c("tv_support", "meta_support", "tiktok_support", "macro"),
    role = c("media", "media", "media", "control"),
    spend_col = c("tv_spend", "meta_spend", "tiktok_spend", NA_character_),
    rollup_path = c(
      "total_paid_media > video > tv",
      "total_paid_media > social > meta",
      "total_paid_media > social > tiktok",
      "business_controls > macro"
    ),
    coef = c(0, 0, 0, 0),
    coef_precision = c(1, 1, 1, 1),
    coef_bound = c("pos", "pos", "pos", "free")
  )
  spend_map <- metadata[role == "media", .(variable, spend_col)]
  smoke_fit_args <- list(
    chains = 1L,
    parallel_chains = 1L,
    iter_warmup = 60L,
    iter_sampling = 30L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 0L,
    verbose = FALSE,
    output_dir = NULL,
    init_strategy = "random",
    use_pathfinder_init = FALSE,
    sample_curve_parameters = "always",
    sample_coef_hierarchy = "never",
    likelihood = "normal",
    intercept_type = "flat",
    ucm_spec = list(level = FALSE, season = FALSE, cycle = FALSE),
    create_frequency_response_curves = FALSE,
    create_response_curve_draws = TRUE,
    response_curve_draw_count = 20L,
    response_curve_multipliers = 1,
    response_curve_scope = "total"
  )
  if (length(source_root)) {
    smoke_fit_args$stan_file <- file.path(source_root[1], "inst", "stan", "hier_mmm.stan")
  }

  stage_one <- run_sequential_hierarchical_bayes(
    data = panel,
    metadata_input = metadata,
    dep_var_col = "kpi",
    group_col = "geo",
    time_col = "period",
    entity_col = "entity",
    spend_map = spend_map,
    root_control_cols = "macro",
    root_trend_spec = "none",
    root_fourier_harmonics = 0L,
    root_bootstrap_reps = 12L,
    root_block_length = 4L,
    rollup_depth = "leaf",
    holdout_last_n = 8L,
    fit_child = TRUE,
    child_fit_args = smoke_fit_args,
    seed = 903L
  )
  stopifnot(!is.null(stage_one$child_fit$fit))
  stopifnot(nrow(stage_one$child_fit$response_curves_draws) > 0L)
  stopifnot(data.table::uniqueN(stage_one$child_fit$response_curves_draws$contribution) > 1L)
  stopifnot(nrow(stage_one$child_business_priors) == 3L)
  stopifnot(nrow(stage_one$child_reference_calibration_input) == 3L)
  stopifnot(all(stage_one$child_reference_calibration_input$sequential_prior_application == "joint_reference_spend_calibration"))
  stopifnot(!any(stage_one$branch_decisions$branch_action %in% c("stop", "prune", "require_prior")))
  stopifnot(stage_one$root_fit$root_training_panel[, uniqueN(root_time__)] == 44L)
  stage_one_curve_draws <- as.matrix(stage_one$child_fit$fit$draws(variables = c("rrate", "cvalue"), format = "matrix"))
  stopifnot(ncol(stage_one_curve_draws) >= 6L)
  stopifnot(all(vapply(seq_len(ncol(stage_one_curve_draws)), function(ii) stats::sd(stage_one_curve_draws[, ii]) > 0, logical(1))))
  curve_audit <- as.data.table(stage_one$child_fit$diagnostics$prior_posterior_curve)
  stopifnot(all(c("rrate", "curve_rate") %in% curve_audit$curve_parameter))
  stopifnot(all(is.finite(curve_audit[curve_parameter %in% c("rrate", "curve_rate"), posterior_sd])))
  stopifnot(any(abs(curve_audit[curve_parameter %in% c("rrate", "curve_rate"), posterior_minus_prior]) > 1e-8))
  seq_audit <- as.data.table(stage_one$prior_posterior_audit)
  stopifnot(nrow(seq_audit) == 3L)
  stopifnot(all(is.finite(seq_audit$posterior_effectiveness)))
  stopifnot(any(abs(seq_audit$posterior_movement_prior_sd_units) > 0.01))
  id_audit <- as.data.table(stage_one$child_identification$by_variable)
  stopifnot(max(id_audit$parent_shrinkage_multiplier) >= min(id_audit$parent_shrinkage_multiplier))
  weak_var <- id_audit[which.min(identification_strength_0_1), variable]
  weak_move <- abs(seq_audit[variable == weak_var, posterior_movement_prior_sd_units])
  stopifnot(length(weak_move) == 1L, is.finite(weak_move), weak_move < 4)

  # The posterior-to-next-level mapping is unit-tested with deterministic
  # response-curve draws. Keeping this execution test to one child stage makes
  # it a true low-iteration contract check rather than a convergence exercise.
  if (nzchar(smoke_sentinel)) writeLines("passed", smoke_sentinel)
  cat("Sequential hierarchical Bayes Stan execution smoke passed (not a convergence test).\n")
}
