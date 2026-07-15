# Opt-in, paired geometry ablation for sequential transfer modes.
if (Sys.getenv("ECONIMAP_RUN_SEQUENTIAL_GEOMETRY_ABLATION", "0") != "1") {
  cat("Sequential geometry ablation skipped. Set ECONIMAP_RUN_SEQUENTIAL_GEOMETRY_ABLATION=1 to run.\n")
} else {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Requires data.table.", call. = FALSE)
  test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
  candidate_roots <- unique(c(
    if (!is.na(test_file) && file.exists(test_file)) dirname(dirname(test_file)) else character(),
    getwd(), file.path(getwd(), "econimap"), Sys.getenv("R_PACKAGE_DIR")
  ))
  root_dir <- candidate_roots[vapply(candidate_roots, function(path) {
    file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))
  }, logical(1))]
  if (!length(root_dir)) stop("Could not locate the econimap package root.", call. = FALSE)
  root_dir <- root_dir[1]
  invisible(lapply(sort(list.files(file.path(root_dir, "R"), pattern = "[.]R$", full.names = TRUE)), source))
  library(data.table)

  direct_fit_file <- Sys.getenv("ECONIMAP_SEQUENTIAL_GEOMETRY_DIRECT_FIT", "")
  if (!nzchar(direct_fit_file) || !file.exists(direct_fit_file)) {
    stop("Set ECONIMAP_SEQUENTIAL_GEOMETRY_DIRECT_FIT to a saved direct_leaf_fit.rds.", call. = FALSE)
  }
  mode <- match.arg(
    Sys.getenv("ECONIMAP_SEQUENTIAL_GEOMETRY_TRANSFER_MODE", "effectiveness_only"),
    c("effectiveness_only", "effectiveness_adstock", "effectiveness_adstock_saturation")
  )
  saved_direct <- readRDS(direct_fit_file)
  output_dir <- Sys.getenv("ECONIMAP_SEQUENTIAL_GEOMETRY_OUTPUT_DIR", file.path(tempdir(), paste0("econimap_seq_geometry_", mode)))
  bootstrap_reps <- max(2L, as.integer(Sys.getenv("ECONIMAP_SEQUENTIAL_GEOMETRY_BOOTSTRAP_REPS", "8")))
  result <- run_sequential_hierarchical_bayes(
    data = saved_direct$data,
    metadata_input = saved_direct$metadata,
    dep_var_col = saved_direct$dep_var_col,
    group_col = saved_direct$group_col,
    time_col = saved_direct$time_col,
    entity_col = saved_direct$entity_col,
    # The saved model panel retains the modeled support columns, not the
    # synthetic raw spend aliases. In this paired geometry probe support and
    # spend were generated identically, so use the modeled support basis.
    spend_map = saved_direct$metadata[role == "media", .(variable, spend_col = variable)],
    root_control_cols = "macro",
    root_trend_spec = "none",
    root_fourier_harmonics = 0L,
    holdout_last_n = saved_direct$holdout_last_n,
    root_bootstrap_reps = bootstrap_reps,
    root_block_length = 4L,
    rollup_depth = "leaf",
    curve_transfer_mode = mode,
    saturation_handoff = "generic_child_prior",
    fit_child = TRUE,
    child_fit_args = list(
      chains = 1L, parallel_chains = 1L, iter_warmup = 150L, iter_sampling = 50L,
      adapt_delta = .95, max_treedepth = 12L, metric = "diag_e", seed = 20260712L,
      likelihood = "normal", intercept_type = "flat", ucm_spec = list(level = FALSE, season = FALSE, cycle = FALSE),
      sample_curve_parameters = "always", sample_coef_hierarchy = "auto", coef_parameterization = "noncentered",
      init_strategy = "pathfinder", use_pathfinder_init = TRUE,
      create_frequency_response_curves = FALSE, create_response_curve_draws = FALSE,
      output_dir = output_dir, output_prefix = paste0("sequential_", mode), refresh = 0L, verbose = FALSE,
      stan_file = file.path(root_dir, "inst", "stan", "hier_mmm.stan")
    ),
    output_dir = output_dir,
    output_prefix = paste0("sequential_", mode),
    seed = 20260712L
  )
  sampler <- as.data.table(result$child_fit$diagnostics$sampler_overall)
  sampler[, transfer_mode := mode]
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  fwrite(sampler, file.path(output_dir, "sampler_overall.csv"))
  print(sampler)
}
