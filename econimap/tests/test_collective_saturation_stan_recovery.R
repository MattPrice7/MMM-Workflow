test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
root_dir <- if (!is.na(test_file) && file.exists(test_file)) dirname(dirname(test_file)) else getwd()

if (Sys.getenv("ECONIMAP_RUN_COLLECTIVE_SHAPE_STAN", "0") != "1") {
  cat("Collective shape Stan recovery test skipped. Set ECONIMAP_RUN_COLLECTIVE_SHAPE_STAN=1 to run.\n")
} else if (!requireNamespace("cmdstanr", quietly = TRUE) || !requireNamespace("posterior", quietly = TRUE) ||
           inherits(try(cmdstanr::cmdstan_path(), silent = TRUE), "try-error")) {
  cat("Collective shape Stan recovery test skipped: CmdStan dependencies unavailable.\n")
} else {
  suppressPackageStartupMessages(library(data.table))
  invisible(lapply(sort(list.files(file.path(root_dir, "R"), pattern = "[.]R$", full.names = TRUE)), source))

  # One visibly saturated, independently varying child and one weak, nearly
  # linear sibling. This is deliberately small: it is a feature recovery test,
  # not a convergence certification or the broad validation suite.
  set.seed(260713L)
  weeks <- seq.Date(as.Date("2024-01-01"), by = "week", length.out = 72L)
  geos <- c("east", "west")
  panel <- rbindlist(lapply(seq_along(geos), function(g) {
    t <- seq_along(weeks)
    a <- pmax(0, 26 + 19 * sin(t / 4 + g / 3) + rnorm(length(t), 0, 7))
    # Deliberately low-variation and low-effect: identifiable volume, weak
    # saturation information. It should not inherit child A's curvature.
    b <- pmax(0, 9 + 1.5 * sin(t / 8) + rnorm(length(t), 0, .8))
    data.table(period = weeks, geo = geos[g], entity = "brand", child_a = a, child_b = b,
               child_a_spend = a * 2, child_b_spend = b * 2)
  }))
  make_truth_contribution <- function(x, rrate, cvalue, beta) {
    out <- numeric(length(x))
    for (gg in unique(panel$geo)) {
      ix <- which(panel$geo == gg)
      tr <- media_transform_hier_mmm(x[ix], rrate = rrate, cvalue = cvalue, dvalue = 1,
                                     curve_type = "hill", normalize_curve_x = TRUE)$transformed
      out[ix] <- beta * tr
    }
    out
  }
  truth_a <- make_truth_contribution(panel$child_a, .18, 1.8, 52)
  truth_b <- make_truth_contribution(panel$child_b, .05, .12, 7)
  panel[, kpi := 350 + truth_a + truth_b + rnorm(.N, 0, 5)]
  panel[, parent_support := child_a + child_b]

  common_fit <- list(
    chains = 1L, parallel_chains = 1L, iter_warmup = 80L, iter_sampling = 60L,
    adapt_delta = .95, max_treedepth = 12L, refresh = 0L, verbose = FALSE,
    output_dir = NULL, init_strategy = "random", use_pathfinder_init = FALSE,
    sample_coef_hierarchy = "never", likelihood = "normal", intercept_type = "flat",
    ucm_spec = list(level = FALSE, season = FALSE, cycle = FALSE),
    create_frequency_response_curves = FALSE, create_response_curve_draws = FALSE,
    stan_file = file.path(root_dir, "inst", "stan", "hier_mmm.stan")
  )
  parent_md <- data.table(variable = "parent_support", role = "media", curve_type = "hill",
                          coef = 0, coef_precision = 1, coef_bound = "pos", rrate = .15,
                          rrate_precision = 1, anchor_saturation = .5,
                          anchor_saturation_precision = 1, cvalue_from_anchor = TRUE)
  parent_fit <- do.call(fit_hier_mmm, c(common_fit, list(
    data = panel, metadata_input = parent_md, dep_var_col = "kpi", group_col = "geo",
    time_col = "period", entity_col = "entity", spend_map = data.table(variable = "parent_support", spend_col = NA_character_),
    sample_curve_parameters = "always"
  )))

  child_md <- data.table(
    variable = c("child_a", "child_b"), role = "media", curve_type = "hill",
    coef = 0, coef_precision = 1, coef_bound = "pos", rrate = c(.12, .12),
    rrate_precision = c(1, 1), anchor_saturation = c(.5, .5),
    anchor_saturation_precision = c(1, 1), cvalue_from_anchor = TRUE,
    spend_col = c("child_a_spend", "child_b_spend")
  )
  child_base_args <- c(common_fit, list(
    data = panel, metadata_input = child_md, dep_var_col = "kpi", group_col = "geo",
    time_col = "period", entity_col = "entity",
    spend_map = child_md[, .(variable, spend_col)], sample_curve_parameters = "always"
  ))
  generic_fit <- do.call(fit_hier_mmm, child_base_args)

  target_time <- weeks[58]
  parent_draws <- extract_posterior_draw_params_hier_mmm(parent_fit, max_draws = 50L, seed = 260714L)$params
  mults <- c(.5, 1.5, 2)
  target_draws <- lapply(mults, function(mult) {
    pairs <- lapply(parent_draws, function(pm) {
      pm$draw_id <- NULL
      ref <- econ_seq_parent_shape_response_draw(parent_fit, "parent_support", target_time, 1, pm)
      val <- econ_seq_parent_shape_response_draw(parent_fit, "parent_support", target_time, mult, pm)
      c(reference = ref, shape = if (is.finite(ref) && ref > 1e-6 && is.finite(val)) val / ref else NA_real_)
    })
    do.call(rbind, pairs)
  })
  valid <- Reduce(`&`, lapply(target_draws, function(x) is.finite(x[, "reference"]) & x[, "reference"] > 1e-6 & is.finite(x[, "shape"])))
  stopifnot(sum(valid) >= 10L)
  shape_matrix <- do.call(cbind, lapply(target_draws, function(x) x[valid, "shape"]))
  ref_mean <- vapply(target_draws, function(x) mean(x[valid, "reference"]), numeric(1))
  shape_cov <- stats::cov(shape_matrix)
  response_cov <- diag(ref_mean, length(ref_mean)) %*% shape_cov %*% diag(ref_mean, length(ref_mean)) + diag(rep(4, length(mults)))
  shape_input <- list(
    scenarios = data.table(
      reconciliation_id = paste0("parent_shape_", mults), parent_node = "parent_support",
      mix_id = "observed_mix", support_multiplier = mults,
      parent_shape = colMeans(shape_matrix), parent_reference_response = ref_mean
    ),
    members = data.table(reconciliation_id = rep(paste0("parent_shape_", mults), each = 2L),
                         variable = rep(c("child_a", "child_b"), length(mults)),
                         time_value = target_time, multiplier = rep(mults, each = 2L)),
    parent_shape_cov = response_cov,
    mix_selection = data.table(parent_node = "parent_support", mix_id = "observed_mix", sufficient_mix_variation = TRUE)
  )
  collective_fit <- do.call(fit_hier_mmm, c(child_base_args, list(
    collective_saturation_shape_reconciliation_input = shape_input
  )))

  curve_summary <- function(fit) {
    draw <- as.matrix(fit$fit$draws(variables = c("cvalue", "rrate"), format = "matrix"))
    data.table(
      variable = c("child_a", "child_b"),
      cvalue_q50 = apply(draw[, grep("^cvalue", colnames(draw)), drop = FALSE], 2, stats::median),
      rrate_q50 = apply(draw[, grep("^rrate", colnames(draw)), drop = FALSE], 2, stats::median)
    )
  }
  collective_curve <- curve_summary(collective_fit)
  # High cvalue means more rapidly saturating under this parameterization.
  stopifnot(collective_curve[variable == "child_a", cvalue_q50] > collective_curve[variable == "child_b", cvalue_q50])
  sampler <- sampler_diagnostics_hier_mmm(collective_fit)
  cat("Focused collective-shape Stan recovery (one chain; not convergence-certified):\n")
  print(collective_curve)
  print(sampler[, .(divergences_total, treedepth_hits_total, max_rhat, min_ess_bulk)])
  cat("Generic and collective child fits completed; collective constraint uses actual Stan sampling.\n")
}
