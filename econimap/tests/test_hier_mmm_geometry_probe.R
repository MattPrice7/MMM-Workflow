# Focused, opt-in geometry probe for the direct hierarchical MMM.
# This deliberately does not run sequential handoffs or a recovery suite.

if (Sys.getenv("ECONIMAP_RUN_GEOMETRY_PROBE", "0") != "1") {
  cat("Geometry probe skipped. Set ECONIMAP_RUN_GEOMETRY_PROBE=1 to run.\n")
} else if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Geometry probe requires data.table.", call. = FALSE)
} else {
  test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
  candidate_roots <- unique(c(
    if (!is.na(test_file) && file.exists(test_file)) dirname(dirname(test_file)) else character(),
    getwd(), file.path(getwd(), "econimap"), file.path(getwd(), ".."), Sys.getenv("R_PACKAGE_DIR")
  ))
  root_dir <- candidate_roots[vapply(candidate_roots, function(path) {
    file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))
  }, logical(1))]
  if (!length(root_dir)) stop("Could not locate the econimap package root.", call. = FALSE)
  root_dir <- root_dir[1]
  suppressPackageStartupMessages(library(data.table))
  invisible(lapply(sort(list.files(file.path(root_dir, "R"), pattern = "[.]R$", full.names = TRUE)), source))

  # This mirrors the direct validation structure: six geographies, three
  # media variables, an observed control, and no latent baseline component.
  set.seed(1101L)
  periods <- seq.Date(as.Date("2023-01-02"), by = "week", length.out = 104L)
  media_vars <- c("tv_support", "meta_support", "tiktok_support")
  panel <- rbindlist(lapply(seq_len(6L), function(g) {
    week <- seq_along(periods)
    geo_scale <- 0.75 + 0.11 * g
    tv <- pmax(8, geo_scale * (82 + 31 * sin(week / 5) + rnorm(length(week), sd = 10)))
    meta <- pmax(6, geo_scale * (48 + 21 * cos(week / 8) + rnorm(length(week), sd = 8)))
    tiktok <- pmax(4, geo_scale * (29 + 16 * sin(week / 11 + 1) + rnorm(length(week), sd = 6)))
    macro <- as.numeric(scale(0.55 * sin(week / 13) + rnorm(length(week), sd = 0.75)))
    out <- data.table(
      period = periods, geo = paste0("geo_", g), entity = "brand",
      tv_support = tv, meta_support = meta, tiktok_support = tiktok,
      tv_spend = tv, meta_spend = meta, tiktok_spend = tiktok, macro = macro
    )
    out[, kpi := 820 + 19 * g + 24 * macro +
      118 * media_transform_hier_mmm(tv_support, .25, 1.10, curve_type = "hill")$transformed +
      76 * media_transform_hier_mmm(meta_support, .10, 1.30, curve_type = "hill")$transformed +
      44 * media_transform_hier_mmm(tiktok_support, .05, 1.55, curve_type = "hill")$transformed +
      rnorm(.N, sd = 20)]
    out
  }))
  metadata <- data.table(
    variable = c(media_vars, "macro"), role = c(rep("media", 3L), "control"),
    spend_col = c("tv_spend", "meta_spend", "tiktok_spend", NA_character_),
    curve_type = c(rep("hill", 3L), NA_character_), has_curve = c(rep(1L, 3L), 0L),
    rrate = c(rep(.20, 3L), 0), rrate_precision = c(rep(16, 3L), 1),
    anchor_saturation = c(rep(.50, 3L), NA_real_),
    anchor_saturation_precision = c(rep(4, 3L), NA_real_),
    cvalue_from_anchor = c(rep(TRUE, 3L), FALSE),
    dvalue = c(rep(1, 3L), 0), dvalue_precision = c(rep(25, 3L), 1),
    coef = c(rep(.05, 3L), 0), coef_precision = c(rep(4, 3L), 1),
    coef_bound = c(rep("pos", 3L), "free")
  )

  prep <- prepare_stan_data_hier_mmm(
    data = panel, metadata_input = metadata, dep_var_col = "kpi", group_col = "geo",
    time_col = "period", entity_col = "entity", holdout_last_n = 13L,
    sample_curve_parameters = "always", sample_coef_hierarchy = "auto",
    coef_parameterization = "auto", likelihood = "normal", intercept_type = "flat",
    ucm_spec = list(level = FALSE, season = FALSE, cycle = FALSE)
  )
  lookup <- as.data.table(prep$variable_lookup)
  audit <- lookup[, .(
    variable, role, has_curve, sample_coef_hierarchy_flag, coef_hierarchy_mode,
    coef_hierarchy_scale, geo_variation_week_share, hierarchy_blocker_reason
  )]
  cat("Direct geometry probe: active model blocks\n")
  print(audit)
  cat("\nStan dimensions:\n")
  print(data.table(
    N = prep$stan_data$N, N_train = prep$stan_data$N_train, G = prep$stan_data$G,
    J = prep$stan_data$J, J_curve = prep$stan_data$J_curve,
    J_curve_sampled = prep$stan_data$J_curve_sampled,
    J_dvalue_sampled = prep$stan_data$J_dvalue_sampled,
    J_pos_hier = prep$stan_data$J_pos_hier, J_neg_hier = prep$stan_data$J_neg_hier,
    J_lower_hier = prep$stan_data$J_lower_hier, J_upper_hier = prep$stan_data$J_upper_hier,
    J_bounded_hier = prep$stan_data$J_bounded_hier, J_free_hier = prep$stan_data$J_free_hier,
    N_state_innov = prep$stan_data$N_state_innov, K_season = prep$stan_data$K_season
  ))

  # An explicitly requested one-chain ablation. It is deliberately a geometry
  # probe, not a convergence claim. Profiles change one source of flexibility
  # at a time from the stable fixed/no-hierarchy baseline.
  if (Sys.getenv("ECONIMAP_GEOMETRY_PROBE_SAMPLE", "0") == "1") {
    profile <- match.arg(
      Sys.getenv("ECONIMAP_GEOMETRY_PROBE_PROFILE", "fixed_no_hierarchy"),
      c("fixed_no_hierarchy", "free_curves_no_hierarchy", "fixed_curves_hierarchy", "production_direct")
    )
    sample_curves <- profile %in% c("free_curves_no_hierarchy", "production_direct")
    sample_hierarchy <- profile %in% c("fixed_curves_hierarchy", "production_direct")
    coef_parameterization <- match.arg(
      Sys.getenv("ECONIMAP_GEOMETRY_PROBE_COEF_PARAMETERIZATION", "auto"),
      c("auto", "centered", "noncentered")
    )
    fit <- fit_hier_mmm(
      data = panel, metadata_input = metadata, dep_var_col = "kpi", group_col = "geo",
      time_col = "period", entity_col = "entity", holdout_last_n = 13L,
      sample_curve_parameters = if (sample_curves) "always" else "never",
      sample_coef_hierarchy = if (sample_hierarchy) "auto" else "never",
      coef_parameterization = coef_parameterization,
      likelihood = "normal", intercept_type = "flat",
      ucm_spec = list(level = FALSE, season = FALSE, cycle = FALSE),
      chains = 1L, parallel_chains = 1L, iter_warmup = 150L, iter_sampling = 50L,
      adapt_delta = .95, max_treedepth = 12L, metric = "diag_e",
      init_strategy = "pathfinder", use_pathfinder_init = TRUE,
      create_frequency_response_curves = FALSE, create_response_curve_draws = FALSE,
      output_dir = NULL, refresh = 0L, verbose = FALSE,
      stan_file = file.path(root_dir, "inst", "stan", "hier_mmm.stan")
    )
    cat("\nGeometry profile: ", profile, "\n", sep = "")
    result <- as.data.table(fit$diagnostics$sampler_overall)
    result[, `:=`(
      profile = profile,
      coef_parameterization = coef_parameterization,
      j_curve_sampled = prep$stan_data$J_curve_sampled,
      j_dvalue_sampled = prep$stan_data$J_dvalue_sampled
    )]
    print(result)
    output_file <- Sys.getenv("ECONIMAP_GEOMETRY_PROBE_OUTPUT", "")
    if (nzchar(output_file)) {
      dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
      fwrite(result, output_file)
      cat("Saved geometry diagnostics to: ", output_file, "\n", sep = "")
    }
  }
}
