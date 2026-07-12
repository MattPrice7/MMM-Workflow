test_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE), error = function(e) NA_character_)
candidate_roots <- unique(c(
  if (!is.na(test_file) && file.exists(test_file)) dirname(dirname(test_file)) else character(),
  getwd(), file.path(getwd(), ".."), Sys.getenv("R_PACKAGE_DIR")
))
source_root <- candidate_roots[vapply(candidate_roots, function(path) {
  file.exists(file.path(path, "DESCRIPTION")) && dir.exists(file.path(path, "R"))
}, logical(1))]
root <- if (length(source_root)) source_root[1] else getwd()
if (!requireNamespace("data.table", quietly = TRUE)) stop("Sequential allocation test requires data.table.")
suppressPackageStartupMessages(library(data.table))
if (length(source_root)) {
  invisible(lapply(sort(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)), source))
} else {
  library(econimap)
  list2env(as.list(asNamespace("econimap"), all.names = TRUE), envir = .GlobalEnv)
}

periods <- seq.Date(as.Date("2024-01-01"), by = "week", length.out = 16L)
geos <- paste0("g", 1:4)
panel <- CJ(period = periods, geo = geos)
panel[, entity := "brand"]
panel[, population := c(100, 200, 300, 400)[match(geo, geos)]]
panel[, national_tv_spend := 1000 + 50 * as.integer(factor(period))]
panel[, national_tv_frequency := 2.5]
panel[, local_search_spend := 20 * match(geo, geos) + as.integer(factor(period))]
panel[, kpi := 100 + 0.2 * local_search_spend]

metadata <- data.table(
  variable = c("national_tv_spend", "national_tv_frequency", "local_search_spend"),
  role = c("media", "reach_frequency", "media"),
  spend_col = c("national_tv_spend", "national_tv_spend", "local_search_spend"),
  coef = 0,
  coef_precision = 1,
  rollup_path = c(
    "total_paid_media > video > tv",
    "total_paid_media > video > tv_frequency",
    "total_paid_media > search > local_search"
  )
)

# Additive repeated national media is counted once, population allocated, and
# never promoted to genuine geo identification.
tv_metadata <- metadata[variable == "national_tv_spend"]
tv_config <- data.table(
  variable = "national_tv_spend",
  spend_scope = "national",
  spend_national_layout = "repeated_by_group",
  support_scope = "national",
  support_national_layout = "repeated_by_group",
  spend_allocation_basis = "population",
  support_allocation_basis = "population",
  population_col = "population",
  support_semantics = "additive_total"
)
tv <- canonicalize_sequential_media_panel(
  data = panel,
  metadata_input = tv_metadata,
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = data.table(variable = "national_tv_spend", spend_col = "national_tv_spend"),
  media_scope_config = tv_config,
  population_col = "population"
)
expected_national <- unique(panel[, .(period, expected = national_tv_spend)])
allocated <- tv$data[, .(allocated = sum(national_tv_spend)), by = period]
stopifnot(max(abs(allocated[expected_national, on = "period"]$allocated - expected_national$expected)) < 1e-10)
stopifnot(nrow(tv$national_spend) == length(periods))
stopifnot(max(abs(tv$allocation_audit$allocation_reconciliation_error), na.rm = TRUE) < 1e-10)
stopifnot(all(tv$media_config$mechanically_allocated))
stopifnot(!any(tv$media_config$hierarchical_variation_eligible))
stopifnot(!any(tv$media_config$spend_hierarchical_variation_eligible))
stopifnot(!any(tv$media_config$support_hierarchical_variation_eligible))
stopifnot(all(tv$metadata[variable == "national_tv_spend", coef_hierarchy_scope] == "none"))
first_period <- tv$data[period == periods[1]][order(geo)]
stopifnot(max(abs(first_period$national_tv_spend / sum(first_period$national_tv_spend) - c(0.1, 0.2, 0.3, 0.4))) < 1e-10)
stopifnot(all(first_period[[tv$media_config$raw_spend_col[1]]] == expected_national$expected[1]))

# Automatic equal-share fallback preserves the total. Explicit population
# allocation errors on invalid data unless the analyst deliberately opts into
# an equal-share override.
equal_panel <- copy(panel)[, population := NULL]
equal_config <- copy(tv_config)
equal_config[, `:=`(
  spend_allocation_basis = "auto",
  support_allocation_basis = "auto",
  population_col = NA_character_
)]
equal <- canonicalize_sequential_media_panel(
  data = equal_panel,
  metadata_input = tv_metadata,
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = data.table(variable = "national_tv_spend", spend_col = "national_tv_spend"),
  media_scope_config = equal_config
)
stopifnot(max(abs(equal$data[, sum(national_tv_spend), by = period]$V1 - expected_national$expected)) < 1e-10)
stopifnot(any(grepl("equal_shares", equal$allocation_audit$fallback_used, fixed = TRUE)))
partial_panel <- copy(panel)
partial_panel[geo == "g4", population := NA_real_]
partial_error <- try(canonicalize_sequential_media_panel(
  data = partial_panel,
  metadata_input = tv_metadata,
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = data.table(variable = "national_tv_spend", spend_col = "national_tv_spend"),
  media_scope_config = tv_config,
  population_col = "population"
), silent = TRUE)
stopifnot(inherits(partial_error, "try-error"))
partial <- canonicalize_sequential_media_panel(
  data = partial_panel,
  metadata_input = tv_metadata,
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = data.table(variable = "national_tv_spend", spend_col = "national_tv_spend"),
  media_scope_config = tv_config,
  population_col = "population",
  invalid_allocation_fallback = "equal"
)
stopifnot(max(abs(partial$data[, sum(national_tv_spend), by = period]$V1 - expected_national$expected)) < 1e-10)
stopifnot(any(grepl("explicit_invalid_weight_override", partial$allocation_audit$fallback_used, fixed = TRUE)))

# Genuinely group-specific media remain unchanged and qualify for hierarchy.
local_metadata <- metadata[variable == "local_search_spend"]
local <- canonicalize_sequential_media_panel(
  data = panel,
  metadata_input = local_metadata,
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = data.table(variable = "local_search_spend", spend_col = "local_search_spend"),
  media_scope_config = data.table(
    variable = "local_search_spend",
    media_scope = "group_specific",
    national_data_layout = "already_allocated",
    support_semantics = "additive_total"
  )
)
stopifnot(max(abs(local$data$local_search_spend - panel$local_search_spend)) < 1e-12)
stopifnot(all(local$media_config$hierarchical_variation_eligible))

# Spend and modeled support have independent scopes. National spend must not
# erase genuine geo impression variation.
cross_panel <- copy(panel)
cross_panel[, geo_impressions := national_tv_spend * population / 10 + match(geo, geos) * as.integer(factor(period))]
cross_metadata <- data.table(
  variable = "geo_impressions",
  role = "media",
  spend_col = "national_tv_spend",
  coef = 0,
  coef_precision = 1,
  rollup_path = "total_paid_media > video > geo_impressions"
)
cross_scope <- canonicalize_sequential_media_panel(
  data = cross_panel,
  metadata_input = cross_metadata,
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = cross_metadata[, .(variable, spend_col)],
  media_scope_config = data.table(
    variable = "geo_impressions",
    spend_scope = "national",
    spend_national_layout = "repeated_by_group",
    spend_allocation_basis = "population",
    support_scope = "group_specific",
    support_national_layout = "already_allocated",
    support_semantics = "impressions",
    population_col = "population"
  ),
  population_col = "population"
)
stopifnot(max(abs(cross_scope$data$geo_impressions - cross_panel$geo_impressions)) < 1e-10)
stopifnot(!cross_scope$media_config$spend_hierarchical_variation_eligible[1])
stopifnot(cross_scope$media_config$support_hierarchical_variation_eligible[1])
stopifnot(cross_scope$metadata[variable == "geo_impressions", coef_hierarchy_scope] != "none")

# Conversely, observed geo spend can coexist with national GRP support. GRPs
# are converted to gross impressions, and the audit reconstructs the source
# rating points instead of pretending the units reconcile directly.
cross_panel[, national_grp := 65 + as.integer(factor(period))]
grp_metadata <- data.table(
  variable = "national_grp",
  role = "media",
  spend_col = "local_search_spend",
  coef = 0,
  coef_precision = 1,
  rollup_path = "total_paid_media > video > national_grp"
)
grp_scope <- canonicalize_sequential_media_panel(
  data = cross_panel,
  metadata_input = grp_metadata,
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = grp_metadata[, .(variable, spend_col)],
  media_scope_config = data.table(
    variable = "national_grp",
    spend_scope = "group_specific",
    spend_national_layout = "already_allocated",
    support_scope = "national",
    support_national_layout = "repeated_by_group",
    support_semantics = "grp",
    population_col = "population"
  ),
  population_col = "population"
)
stopifnot(grp_scope$media_config$spend_hierarchical_variation_eligible[1])
stopifnot(!grp_scope$media_config$support_hierarchical_variation_eligible[1])
grp_audit <- grp_scope$allocation_audit[measure == "support"]
stopifnot(all(is.na(grp_audit$raw_unit_reconciliation_error)))
stopifnot(max(abs(grp_audit$reconstructed_source_error), na.rm = TRUE) < 1e-10)
stopifnot(all(grp_audit$allocated_metric == "gross_impressions"))

# Reach-percent conversion follows the same unit-safe reconstruction contract.
cross_panel[, national_reach_pct := 40 + 0.5 * as.integer(factor(period))]
reach_metadata <- copy(grp_metadata)
reach_metadata[, `:=`(variable = "national_reach_pct", rollup_path = "total_paid_media > video > national_reach")]
reach_scope <- canonicalize_sequential_media_panel(
  data = cross_panel,
  metadata_input = reach_metadata,
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = reach_metadata[, .(variable, spend_col)],
  media_scope_config = data.table(
    variable = "national_reach_pct",
    spend_scope = "group_specific",
    support_scope = "national",
    support_national_layout = "repeated_by_group",
    support_semantics = "reach_percent",
    population_col = "population"
  ),
  population_col = "population"
)
reach_audit <- reach_scope$allocation_audit[measure == "support"]
stopifnot(all(is.na(reach_audit$raw_unit_reconciliation_error)))
stopifnot(max(abs(reach_audit$reconstructed_source_error), na.rm = TRUE) < 1e-10)
stopifnot(all(reach_audit$allocated_metric == "reach_count"))

# National intensive support is shared, not population-divided. Its additive
# spend is still allocated and audited independently.
frequency_metadata <- metadata[variable == "national_tv_frequency"]
frequency <- canonicalize_sequential_media_panel(
  data = panel,
  metadata_input = frequency_metadata,
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = data.table(variable = "national_tv_frequency", spend_col = "national_tv_spend"),
  media_scope_config = data.table(
    variable = "national_tv_frequency",
    spend_scope = "national",
    spend_national_layout = "repeated_by_group",
    support_scope = "national",
    support_national_layout = "repeated_by_group",
    support_semantics = "frequency",
    population_col = "population"
  ),
  population_col = "population"
)
stopifnot(all(frequency$data$national_tv_frequency == 2.5))
stopifnot(any(frequency$allocation_audit$fallback_used == "kept_as_shared_national_intensive_measure"))
stopifnot(!any(frequency$media_config$hierarchical_variation_eligible))

# Mixed scope totals reconcile and the national root uses every national
# variable-period total exactly once.
mixed_metadata <- metadata[variable %in% c("national_tv_spend", "local_search_spend")]
mixed_config <- rbindlist(list(
  tv_config,
  data.table(
    variable = "local_search_spend",
    media_scope = "group_specific",
    national_data_layout = "already_allocated",
    support_semantics = "additive_total"
  )
), fill = TRUE)
mixed <- canonicalize_sequential_media_panel(
  data = panel,
  metadata_input = mixed_metadata,
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = mixed_metadata[, .(variable, spend_col)],
  media_scope_config = mixed_config,
  population_col = "population"
)
expected_mixed <- panel[, .(
  expected = national_tv_spend[1] + sum(local_search_spend)
), by = period]
actual_mixed <- mixed$data[, .(actual = sum(national_tv_spend) + sum(local_search_spend)), by = period]
stopifnot(max(abs(actual_mixed$actual - expected_mixed$expected)) < 1e-10)
root_panel <- econ_seq_root_panel(
  data = mixed$data,
  metadata_input = mixed$metadata,
  dep_var_col = "kpi",
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = mixed$spend_map,
  national_spend = mixed$national_spend,
  root_scope = "national",
  root_control_mode = "none"
)
stopifnot(max(abs(root_panel$data$root_total_paid_spend__ - expected_mixed$expected)) < 1e-10)

# Missing spend is not silently converted to zero.
missing_panel <- copy(panel)
missing_panel[period == periods[2] & geo == "g2", local_search_spend := NA_real_]
missing_canonical <- canonicalize_sequential_media_panel(
  data = missing_panel,
  metadata_input = local_metadata,
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = data.table(variable = "local_search_spend", spend_col = "local_search_spend"),
  media_scope_config = data.table(variable = "local_search_spend", media_scope = "group_specific")
)
stopifnot(anyNA(missing_canonical$data$local_search_spend))
stopifnot(any(missing_canonical$allocation_audit$incomplete_reason == "incomplete_additive_rows", na.rm = TRUE))
missing_error <- try(econ_seq_root_panel(
  data = missing_canonical$data,
  metadata_input = missing_canonical$metadata,
  dep_var_col = "kpi",
  group_col = "geo",
  time_col = "period",
  entity_col = "entity",
  spend_map = missing_canonical$spend_map,
  national_spend = missing_canonical$national_spend,
  root_scope = "national",
  root_control_mode = "none",
  incomplete_period_action = "error"
), silent = TRUE)
stopifnot(inherits(missing_error, "try-error"))

cat("Sequential national media allocation tests passed.\n")
