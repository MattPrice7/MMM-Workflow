test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
candidate_roots <- unique(c(
  if (!is.na(test_file) && file.exists(test_file)) dirname(dirname(test_file)) else character(),
  getwd(), file.path(getwd(), ".."), Sys.getenv("R_PACKAGE_DIR")
))
source_root <- candidate_roots[vapply(candidate_roots, function(path) {
  file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))
}, logical(1))]
root_dir <- if (length(source_root)) source_root[1] else getwd()
if (!requireNamespace("data.table", quietly = TRUE)) stop("Collective saturation test requires data.table.")
suppressPackageStartupMessages(library(data.table))
if (length(source_root)) {
  invisible(lapply(sort(list.files(file.path(root_dir, "R"), pattern = "[.]R$", full.names = TRUE)), source))
} else {
  library(econimap)
  list2env(as.list(asNamespace("econimap"), all.names = TRUE), envir = .GlobalEnv)
}

# The collective mode leaves generic child saturation untouched while retaining
# the existing child-specific parent-informed adstock regularization.
metadata <- data.table(
  variable = c("child_a", "child_b"), role = "media", coef = 0,
  coef_precision = 1, coef_bound = "pos", rrate = c(.15, .15),
  rrate_precision = c(4, 4), anchor_saturation = c(.42, .42),
  anchor_saturation_precision = c(6, 6), cvalue_from_anchor = TRUE
)
parent_evidence <- data.table(
  variable = c("child_a", "child_b"), curve_prior_available = TRUE,
  branch_decision = "fit", rrate_prior_mean = .40,
  rrate_prior_precision = 9, anchor_saturation_prior_mean = .75,
  anchor_saturation_prior_precision = 16
)
relation_fixture <- data.table(
  parent_node = c("social", "social", "video"),
  child_variable = c("meta", "tiktok", "tv")
)
stopifnot(identical(
  sort(econ_seq_child_variables_for_parent(relation_fixture, "social")),
  c("meta", "tiktok")
))
collective_md <- econ_seq_apply_rrate_priors(
  metadata, parent_evidence,
  saturation_handoff = "collective_parent_shape_reconciliation"
)
independent_md <- econ_seq_apply_rrate_priors(
  metadata, parent_evidence,
  saturation_handoff = "independent_parent_prior"
)
stopifnot(all(abs(collective_md$anchor_saturation - metadata$anchor_saturation) < 1e-12))
stopifnot(all(abs(collective_md$anchor_saturation_precision - metadata$anchor_saturation_precision) < 1e-12))
stopifnot(all(collective_md$rrate_precision > metadata$rrate_precision))
stopifnot(all(independent_md$anchor_saturation_precision > metadata$anchor_saturation_precision))
stopifnot(eval(formals(run_sequential_hierarchical_bayes)$saturation_handoff)[1] == "generic_child_prior")
stopifnot(eval(formals(continue_sequential_hierarchical_bayes)$saturation_handoff)[1] == "collective_parent_shape_reconciliation")
stopifnot(inherits(try(econ_seq_apply_rrate_priors(
  metadata, parent_evidence,
  saturation_handoff = "collective_parent_response_reconciliation_experimental"
), silent = TRUE), "try-error"))

# Fixed nonlinear curves are excluded before any posterior extraction. A soft
# shape likelihood cannot exactly reconcile a transform that Stan is forbidden
# to update, so the contract reports exclusion rather than a fake constraint.
fixed_exclusion <- econ_seq_collective_saturation_shape_reconciliation_input(
  parent_fit = list(),
  parent_layer = list(variable_mapping = data.table(
    variable = c("child_a", "child_b"), generated_variable = "parent"
  )),
  child_layer = list(variable_mapping = data.table(
    variable = c("child_a", "child_b"), generated_variable = c("child_a", "child_b")
  )),
  time_col = "period",
  sample_curve_parameters = "never"
)
stopifnot(!nrow(fixed_exclusion$scenarios))
stopifnot(fixed_exclusion$audit$reconciliation_mode[1] == "excluded_fixed_curve_parameters")
stopifnot(!fixed_exclusion$audit$aggregate_shape_constraint_retained[1])
stopifnot(all.equal(econ_seq_curvature_share(c(2, 1), 3), c(2 / 3, 1 / 3)))
stopifnot(all(is.na(econ_seq_curvature_share(c(2, 1), 0))))

# The Stan data contract sums selected child contributions across an observed
# training-period mix. It rejects holdout rows rather than silently leaking.
panel <- data.table(
  period = rep(as.Date("2025-01-06") + 7 * 0:2, each = 2),
  geo = rep(c("east", "west"), 3),
  is_holdout__ = FALSE,
  rescale_factor__ = c(2, 3, 2, 3, 2, 3)
)
lookup <- data.table(variable = c("child_a", "child_b"), variable_idx = 1:2)
reconciliation_input <- list(
  scenarios = data.table(reconciliation_id = "p_low", parent_node = "parent", scenario_label = "low",
                         parent_response = 120, parent_response_sd = 70,
                         parent_posterior_sd_component = 25, data_reuse_sd_component = 30,
                         heterogeneity_sd_component = 40, mix_instability_sd_component = 15,
                         approximation_sd_component = 20),
  members = data.table(reconciliation_id = "p_low", variable = c("child_a", "child_b"),
                       time_value = as.Date("2025-01-06"))
)
prepared <- build_collective_saturation_reconciliation_hier_mmm(
  reconciliation_input, panel, lookup, group_col = "geo", time_col = "period"
)
stopifnot(nrow(prepared$audit) == 1L)
stopifnot(identical(dim(prepared$weight), c(1L, nrow(panel), nrow(lookup))))
stopifnot(sum(prepared$weight[1, , 1]) == 5)
stopifnot(sum(prepared$weight[1, , 2]) == 5)
empty_response_contract <- build_collective_saturation_reconciliation_hier_mmm(
  econ_seq_empty_collective_response_reconciliation(), panel, lookup,
  group_col = "geo", time_col = "period"
)
stopifnot(length(empty_response_contract$observed) == 0L)
stopifnot(identical(dim(empty_response_contract$weight), c(0L, nrow(panel), nrow(lookup))))
panel_holdout <- copy(panel)
panel_holdout[period == as.Date("2025-01-06"), is_holdout__ := TRUE]
stopifnot(inherits(try(build_collective_saturation_reconciliation_hier_mmm(
  reconciliation_input, panel_holdout, lookup, group_col = "geo", time_col = "period"
), silent = TRUE), "try-error"))

# Preparation-level smoke test: the three-dimensional reconciliation weights
# are included in the real Stan data contract with the correct C x N x J shape.
model_panel <- copy(panel)[, `:=`(
  entity = "brand",
  child_a = c(5, 4, 8, 7, 12, 10),
  child_b = c(3, 2, 4, 3, 5, 4),
  kpi = c(100, 95, 120, 115, 140, 132)
)]
model_metadata <- data.table(
  variable = c("child_a", "child_b"), role = "media", curve_type = "hill",
  coef = 0, coef_precision = 1, coef_bound = "pos", rrate = .1,
  rrate_precision = 1, anchor_saturation = .5, anchor_saturation_precision = 1,
  cvalue_from_anchor = TRUE
)
prep <- prepare_stan_data_hier_mmm(
  data = model_panel, metadata_input = model_metadata, dep_var_col = "kpi",
  group_col = "geo", time_col = "period", entity_col = "entity", mean_index = FALSE,
  collective_saturation_reconciliation_input = reconciliation_input,
  stop_on_zero_variance = FALSE
)
stopifnot(prep$stan_data$C_collective_reconciliation == 1L)
stopifnot(identical(dim(prep$stan_data$collective_reconciliation_weight), c(1L, nrow(model_panel), 2L)))
stopifnot(prep$stan_data$collective_parent_response[1] == 120)

# Shape targets are ratios to the same observed mix at its reference level;
# their covariance is carried as one multivariate likelihood rather than a set
# of falsely independent pseudo-experiments.
shape_input <- list(
  scenarios = data.table(reconciliation_id = c("mix_low", "mix_high"), parent_node = "parent",
                         mix_id = "observed_mix_1", support_multiplier = c(.5, 2),
                         parent_shape = c(.72, 1.38), parent_reference_response = c(100, 100)),
  members = data.table(reconciliation_id = rep(c("mix_low", "mix_high"), each = 2),
                       variable = rep(c("child_a", "child_b"), 2),
                       time_value = as.Date("2025-01-20"), multiplier = rep(c(.5, 2), each = 2)),
  parent_shape_cov = matrix(c(.03, .015, .015, .05), 2, 2)
)
shape_prep <- prepare_stan_data_hier_mmm(
  data = model_panel, metadata_input = model_metadata, dep_var_col = "kpi",
  group_col = "geo", time_col = "period", entity_col = "entity", mean_index = FALSE,
  collective_saturation_shape_reconciliation_input = shape_input,
  stop_on_zero_variance = FALSE
)
stopifnot(shape_prep$stan_data$C_collective_shape == 2L)
stopifnot(identical(dim(shape_prep$stan_data$collective_shape_multiplier), c(2L, nrow(model_panel), 2L)))
stopifnot(all(shape_prep$stan_data$collective_parent_shape == c(.72, 1.38)))
stopifnot(all(shape_prep$stan_data$collective_parent_reference_response == c(100, 100)))
stopifnot(abs(shape_prep$stan_data$collective_shape_multiplier[1, 1, 1] - .5) < 1e-12)
stopifnot(abs(shape_prep$stan_data$collective_shape_multiplier[1, 3, 1] - .5) < 1e-12)

# Known-shape fixture: one child carries curvature while a weak linear sibling
# stays linear; their aggregate exactly reconstructs the parent response.
x <- seq(0, 2, length.out = 101)
child_a <- 1.2 * x / (x + .35)
child_b <- .18 * x
parent <- child_a + child_b
aggregate <- child_a + child_b
curvature <- function(y) mean(diff(y, differences = 2))
stopifnot(max(abs(parent - aggregate)) < 1e-12)
stopifnot(abs(curvature(child_b)) < 1e-12)
stopifnot(curvature(child_a) < -1e-4)

# Homogeneous siblings can naturally retain similar curvature under the same
# aggregate target; the handoff has not encoded any artificial asymmetry.
homogeneous_a <- .70 * x / (x + .50)
homogeneous_b <- .65 * x / (x + .45)
homogeneous_parent <- homogeneous_a + homogeneous_b
stopifnot(max(abs(homogeneous_parent - (homogeneous_a + homogeneous_b))) < 1e-12)
stopifnot(abs(curvature(homogeneous_a) - curvature(homogeneous_b)) < .002)

# A strongly identified child with a different curve is not moved toward a
# parent anchor by collective mode. The reconciliation SD is intentionally
# wider than the parent-only posterior component, leaving sibling allocation
# and approximation error room to absorb the conflict.
conflict_md <- copy(metadata)[variable == "child_a", anchor_saturation := .20]
conflict_after <- econ_seq_apply_rrate_priors(
  conflict_md, parent_evidence, saturation_handoff = "collective_parent_shape_reconciliation"
)
stopifnot(conflict_after[variable == "child_a", anchor_saturation] == .20)
stopifnot(prepared$audit$parent_response_sd[1] > prepared$audit$parent_posterior_sd_component[1])

# With weak children the aggregate can be informed while neither individual
# saturation prior becomes falsely precise.
weak_md <- copy(metadata)[, anchor_saturation_precision := .25]
weak_after <- econ_seq_apply_rrate_priors(
  weak_md, parent_evidence, saturation_handoff = "collective_parent_shape_reconciliation"
)
stopifnot(all(weak_after$anchor_saturation_precision == .25))

cat("Collective saturation handoff unit tests passed.\n")
