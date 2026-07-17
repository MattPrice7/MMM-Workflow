# Sequential hierarchical Bayes, phase 1.
#
# The root is deliberately frequentist by default.  Its blocked-bootstrap
# sampling distribution is treated as data-derived evidence, then widened
# before it becomes a child-model prior.  This is staged empirical Bayes, not
# a claim that the parent and child KPI fits are independent experiments.

econ_seq_with_seed <- function(seed = NULL, code) {
  if (is.null(seed) || !is.finite(as.numeric(seed)[1])) return(force(code))
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed)[1])
  force(code)
}

econ_seq_content_hash <- function(..., files = character()) {
  objects <- list(...)
  files <- unique(as.character(files))
  files <- files[nzchar(files)]
  if (length(files) && any(!file.exists(files))) {
    stop("Checkpoint hash file(s) do not exist: ", paste(files[!file.exists(files)], collapse = ", "), call. = FALSE)
  }
  file_manifest <- if (length(files)) {
    data.table::data.table(
      path = normalizePath(files, winslash = "/", mustWork = TRUE),
      md5 = unname(tools::md5sum(files))
    )[order(path)]
  } else data.table::data.table(path = character(), md5 = character())
  payload <- list(objects = objects, files = file_manifest)
  path <- tempfile("econimap-sequential-hash-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(payload, path, version = 3, compress = FALSE)
  unname(tools::md5sum(path))
}

econ_seq_checkpoint_manifest <- function(..., files = character()) {
  objects <- list(...)
  hash <- do.call(econ_seq_content_hash, c(objects, list(files = files)))
  data.table::data.table(
    checkpoint_hash = hash,
    created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    file_count = length(unique(files)),
    object_names = paste(names(objects), collapse = " | "),
    file_paths = paste(sort(normalizePath(files, winslash = "/", mustWork = FALSE)), collapse = " | ")
  )
}

econ_seq_input_table <- function(x, label) {
  if (is.null(x)) return(data.table::data.table())
  out <- read_input_table(x)
  if (!inherits(out, "data.table")) out <- data.table::as.data.table(out)
  data.table::copy(out)
}

econ_seq_media_spend_map <- function(data,
                                     metadata_input,
                                     spend_map = NULL,
                                     media_variables = NULL) {
  dt <- econ_seq_input_table(data, "data")
  md <- prepare_metadata_shell(read_input_table(metadata_input))
  if (!"variable" %in% names(md)) stop("metadata_input must include variable.", call. = FALSE)
  md <- md[!is_ucm_metadata_row(md)]
  md[, `:=`(variable = as.character(variable), role = standardize_role(role))]
  media_roles <- c("media", "reach_frequency")
  candidate <- md[role %in% media_roles, variable]
  if (!is.null(media_variables)) candidate <- intersect(candidate, as.character(media_variables))
  if (!length(candidate)) stop("No paid-media variables were found in metadata/spend_map.", call. = FALSE)

  if (!is.null(spend_map)) {
    sm <- econ_seq_input_table(spend_map, "spend_map")
    if (!"variable" %in% names(sm)) stop("spend_map must include variable.", call. = FALSE)
    if (!"spend_col" %in% names(sm)) {
      alt <- intersect(c("cost_col", "spend", "cost", "spend_column"), names(sm))
      if (!length(alt)) stop("spend_map must include spend_col or cost_col.", call. = FALSE)
      data.table::setnames(sm, alt[1], "spend_col")
    }
    sm[, `:=`(variable = as.character(variable), spend_col = as.character(spend_col))]
  } else {
    spend_col <- rep(NA_character_, nrow(md))
    for (cc in c("spend_col", "cost_col", "rf_spend_col")) {
      if (cc %in% names(md)) {
        candidate_col <- trimws(as.character(md[[cc]]))
        take <- (is.na(spend_col) | !nzchar(spend_col)) & !is.na(candidate_col) & nzchar(candidate_col)
        spend_col[take] <- candidate_col[take]
      }
    }
    sm <- data.table::data.table(variable = md$variable, spend_col = spend_col)
  }
  sm <- unique(sm[variable %in% candidate & !is.na(spend_col) & nzchar(trimws(spend_col))], by = "variable")
  node_fields <- data.table::copy(md[, .(variable, role)])
  for (cc in c("media_node", "shared_spend_group", "variable_role_within_node", "spend_bearing")) {
    node_fields[, (cc) := if (cc %in% names(md)) md[[cc]] else NA]
  }
  sm[node_fields, on = "variable", `:=`(
    role = i.role,
    media_node = as.character(i.media_node),
    shared_spend_group = as.character(i.shared_spend_group),
    variable_role_within_node = as.character(i.variable_role_within_node),
    spend_bearing = as.logical(i.spend_bearing)
  )]
  sm[is.na(spend_bearing), spend_bearing := role != "reach_frequency"]
  sm[is.na(variable_role_within_node) | !nzchar(variable_role_within_node),
     variable_role_within_node := data.table::fifelse(role == "reach_frequency", "auxiliary_execution", "response_support")]
  sm[is.na(shared_spend_group) | !nzchar(shared_spend_group), shared_spend_group := media_node]
  if (anyDuplicated(sm$variable)) stop("spend_map has duplicate paid-media variable rows.", call. = FALSE)
  duplicate_spend <- sm[, .N, by = spend_col][N > 1L, spend_col]
  allowed_shared_spend <- sm[, .(
    allowed = data.table::uniqueN(shared_spend_group) == 1L &&
      !is.na(shared_spend_group[1]) && nzchar(shared_spend_group[1]) &&
      sum(spend_bearing %in% TRUE) == 1L &&
      any(role == "reach_frequency")
  ), by = spend_col]
  invalid_duplicate_spend <- allowed_shared_spend[spend_col %in% duplicate_spend & allowed == FALSE, spend_col]
  if (length(invalid_duplicate_spend)) {
    stop(
      "Duplicate observed spend mapping(s) require one declared spend-bearing media node plus reach/frequency auxiliary variables. Invalid mapping(s): ",
      paste(invalid_duplicate_spend, collapse = ", "),
      call. = FALSE
    )
  }
  missing_vars <- setdiff(candidate, sm$variable)
  if (length(missing_vars)) {
    stop("Missing spend mapping for paid-media variable(s): ", paste(missing_vars, collapse = ", "),
         ". Provide spend_map or metadata spend_col/cost_col.", call. = FALSE)
  }
  missing_cols <- setdiff(sm$spend_col, names(dt))
  if (length(missing_cols)) stop("Spend column(s) missing from data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  sm[order(variable)]
}

econ_seq_normalize_scope_value <- function(x, allowed, default = "auto", label = "value") {
  out <- tolower(trimws(as.character(x %||% default)[1]))
  if (is.na(out) || !nzchar(out)) out <- default
  out <- gsub("[ -]+", "_", out)
  if (!(out %in% allowed)) {
    stop(label, " must be one of: ", paste(allowed, collapse = ", "), ".", call. = FALSE)
  }
  out
}

econ_seq_safe_internal_col <- function(prefix, value) {
  paste0(prefix, gsub("_+", "_", gsub("[^A-Za-z0-9]+", "_", as.character(value))))
}

econ_seq_config_value <- function(variable, metadata, config, candidates, default = NA_character_) {
  variable_name <- as.character(variable)[1]
  find_value <- function(dt) {
    if (is.null(dt) || !nrow(dt) || !("variable" %in% names(dt))) return(NULL)
    row <- dt[as.character(dt[["variable"]]) == variable_name]
    if (!nrow(row)) return(NULL)
    for (cc in candidates) {
      if (!(cc %in% names(row))) next
      value <- row[[cc]][1]
      if (!is.null(value) && length(value) && !is.na(value) && nzchar(trimws(as.character(value)))) return(value)
    }
    NULL
  }
  explicit <- find_value(config)
  if (!is.null(explicit)) return(explicit)
  declared <- find_value(metadata)
  if (!is.null(declared)) return(declared)
  default
}

econ_seq_infer_support_semantics <- function(variable) {
  key <- tolower(as.character(variable)[1])
  if (grepl("(^|_)(grp|trp)s?($|_)", key)) return("grp")
  if (grepl("reach.*(pct|percent|percentage)|(^|_)reach_pct($|_)", key)) return("reach_percent")
  if (grepl("(^|_)(frequency|freq)($|_)", key)) return("frequency")
  if (grepl("(^|_)(cpm|cpc|cpp|rate|index)($|_)", key)) return("intensive_shared")
  "additive_total"
}

econ_seq_infer_media_scope <- function(data, value_col, time_col, tolerance = 1e-8) {
  dt <- data.table::as.data.table(data)
  by_time <- dt[, {
    value <- suppressWarnings(as.numeric(get(value_col)))
    finite <- value[is.finite(value)]
    repeated <- length(finite) >= 2L &&
      (max(finite) - min(finite)) <= tolerance * max(1, abs(mean(finite)))
    list(nonmissing_n = length(finite), repeated = repeated)
  }, by = time_col]
  repeated_share <- if (nrow(by_time)) mean(by_time$repeated & by_time$nonmissing_n >= 2L) else 0
  if (is.finite(repeated_share) && repeated_share >= 0.80) "national" else "group_specific"
}

econ_seq_infer_national_layout <- function(data, value_col, time_col, tolerance = 1e-8) {
  dt <- data.table::as.data.table(data)
  by_time <- dt[, {
    value <- suppressWarnings(as.numeric(get(value_col)))
    finite <- value[is.finite(value)]
    repeated <- length(finite) >= 2L &&
      (max(finite) - min(finite)) <= tolerance * max(1, abs(mean(finite)))
    list(nonmissing_n = length(finite), repeated = repeated)
  }, by = time_col]
  if (!nrow(by_time)) return("single_national_total")
  if (mean(by_time$nonmissing_n == 1L) >= 0.80) return("single_national_total")
  if (mean(by_time$repeated & by_time$nonmissing_n >= 2L) >= 0.80) return("repeated_by_group")
  "already_allocated"
}

econ_seq_infer_scope_layout <- function(data, value_col, time_col, tolerance = 1e-8) {
  dt <- data.table::as.data.table(data)
  by_time <- dt[, {
    value <- suppressWarnings(as.numeric(get(value_col)))
    finite <- value[is.finite(value)]
    repeated <- length(finite) >= 2L &&
      (max(finite) - min(finite)) <= tolerance * max(1, abs(mean(finite)))
    list(nonmissing_n = length(finite), repeated = repeated)
  }, by = time_col]
  if (!nrow(by_time)) return(list(scope = "national", layout = "single_national_total"))
  single_share <- mean(by_time$nonmissing_n == 1L)
  repeated_share <- mean(by_time$repeated & by_time$nonmissing_n >= 2L)
  if (is.finite(single_share) && single_share >= 0.80) {
    return(list(scope = "national", layout = "single_national_total"))
  }
  if (is.finite(repeated_share) && repeated_share >= 0.80) {
    return(list(scope = "national", layout = "repeated_by_group"))
  }
  list(scope = "group_specific", layout = "already_allocated")
}

econ_seq_extract_period_total <- function(data,
                                          value_col,
                                          time_col,
                                          media_scope,
                                          national_data_layout,
                                          tolerance = 1e-8) {
  dt <- data.table::as.data.table(data)
  out <- dt[, {
    value <- suppressWarnings(as.numeric(get(value_col)))
    finite <- value[is.finite(value)]
    missing_n <- sum(!is.finite(value))
    consistent <- TRUE
    reason <- NA_character_
    total <- NA_real_
    if (identical(media_scope, "group_specific") || identical(media_scope, "already_allocated") ||
        identical(national_data_layout, "already_allocated")) {
      if (missing_n == 0L) total <- sum(finite) else reason <- "incomplete_additive_rows"
    } else if (identical(national_data_layout, "repeated_by_group")) {
      if (!length(finite)) {
        consistent <- FALSE
        reason <- "all_values_missing"
      } else if ((max(finite) - min(finite)) > tolerance * max(1, abs(mean(finite)))) {
        consistent <- FALSE
        reason <- "repeated_national_values_disagree"
      } else {
        total <- finite[1]
        if (missing_n > 0L) reason <- "repeated_national_rows_partially_missing"
      }
    } else if (identical(national_data_layout, "single_national_total")) {
      if (length(finite) == 1L) {
        total <- finite[1]
      } else {
        consistent <- FALSE
        reason <- if (!length(finite)) "national_total_missing" else "multiple_values_for_single_national_total"
      }
    }
    list(
      original_national_total = total,
      source_nonmissing_n = length(finite),
      source_missing_n = missing_n,
      layout_consistent = consistent,
      incomplete_reason = reason
    )
  }, by = time_col]
  out[]
}

econ_seq_allocation_weights <- function(data,
                                        time_col,
                                        group_col,
                                        allocation_basis = "auto",
                                        allocation_weight_col = NA_character_,
                                        target_population_col = NA_character_,
                                        population_col = NA_character_,
                                        invalid_allocation_fallback = c("error", "equal")) {
  dt <- data.table::copy(data.table::as.data.table(data))
  dt[, allocation_row_id__ := .I]
  invalid_allocation_fallback <- match.arg(invalid_allocation_fallback)
  allowed_basis <- c("auto", "user_weight", "target_population", "households", "population", "equal")
  allocation_basis <- econ_seq_normalize_scope_value(allocation_basis, allowed_basis, "auto", "allocation_basis")
  candidates <- switch(allocation_basis,
    user_weight = c(allocation_weight_col),
    target_population = c(target_population_col),
    households = c(target_population_col),
    population = c(population_col),
    equal = character(),
    auto = c(allocation_weight_col, target_population_col, population_col)
  )
  declared_candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])
  candidates <- declared_candidates[declared_candidates %in% names(dt)]
  explicit_basis <- !(allocation_basis %in% c("auto", "equal"))
  if (explicit_basis && !length(candidates)) {
    stop("allocation_basis = '", allocation_basis, "' requires a valid declared allocation column.", call. = FALSE)
  }
  rows <- dt[, {
    selected_col <- NA_character_
    raw_weight <- rep(NA_real_, .N)
    fallback <- NA_character_
    if (length(candidates)) {
      for (cc in candidates) {
        z <- suppressWarnings(as.numeric(get(cc)))
        if (all(is.finite(z)) && all(z >= 0) && sum(z) > 0) {
          selected_col <- cc
          raw_weight <- z
          break
        }
      }
    }
    if (is.na(selected_col) && explicit_basis && identical(invalid_allocation_fallback, "error")) {
      stop(
        "Explicit allocation_basis = '", allocation_basis,
        "' has missing, negative, or zero-sum weights for period ", as.character(get(time_col)[1]),
        ". Set invalid_allocation_fallback = 'equal' only when that fallback is analytically intended.",
        call. = FALSE
      )
    }
    if (is.na(selected_col)) {
      raw_weight <- rep(1, .N)
      fallback <- if (explicit_basis) {
        "equal_shares_explicit_invalid_weight_override"
      } else if (length(candidates)) {
        "equal_shares_due_to_missing_or_invalid_auto_weights"
      } else {
        "equal_shares_no_usable_auto_denominator"
      }
    }
    list(
      allocation_row_id__ = allocation_row_id__,
      allocation_weight = raw_weight / sum(raw_weight),
      allocation_basis_used = if (is.na(selected_col)) "equal" else if (identical(selected_col, allocation_weight_col)) "user_weight" else if (identical(selected_col, target_population_col)) "target_population" else "population",
      allocation_weight_col_used = selected_col,
      fallback_used = fallback,
      active_group_n = .N
    )
  }, by = time_col]
  rows[]
}

#' Convert media inputs to one auditable group-by-time representation.
#'
#' Explicit `media_scope_config` values override matching metadata fields.
#' Automatic scope/layout inference is diagnostic fallback only and is recorded
#' in the returned audit.
canonicalize_sequential_media_panel <- function(data,
                                                metadata_input,
                                                group_col,
                                                time_col,
                                                entity_col,
                                                spend_map = NULL,
                                                media_variables = NULL,
                                                media_scope_config = NULL,
                                                population_col = NULL,
                                                market_size_col = NULL,
                                                target_population_col = NULL,
                                                invalid_allocation_fallback = c("error", "equal"),
                                                tolerance = 1e-8) {
  invalid_allocation_fallback <- match.arg(invalid_allocation_fallback)
  dt <- econ_seq_input_table(data, "data")
  required <- c(group_col, time_col, entity_col)
  missing <- setdiff(required, names(dt))
  if (length(missing)) stop("Media canonicalization is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  if (dt[, anyDuplicated(data.table::data.table(group = get(group_col), period = get(time_col))) > 0L]) {
    stop("Media canonicalization requires unique group_col + time_col rows.", call. = FALSE)
  }
  md <- prepare_metadata_shell(read_input_table(metadata_input))
  md <- md[!is_ucm_metadata_row(md)]
  md[, `:=`(variable = as.character(variable), role = standardize_role(role))]
  cfg <- econ_seq_input_table(media_scope_config, "media_scope_config")
  if (nrow(cfg)) {
    if (!("variable" %in% names(cfg))) stop("media_scope_config must include variable.", call. = FALSE)
    cfg[, variable := as.character(variable)]
    if (anyDuplicated(cfg$variable)) stop("media_scope_config has duplicate variable rows.", call. = FALSE)
  }
  sm <- econ_seq_media_spend_map(dt, md, spend_map = spend_map, media_variables = media_variables)
  dt[, sequential_row_id__ := .I]
  audit_rows <- list()
  config_rows <- list()
  spend_rows <- list()

  for (ii in seq_len(nrow(sm))) {
    variable <- sm$variable[ii]
    spend_col <- sm$spend_col[ii]
    legacy_scope <- econ_seq_config_value(variable, md, cfg, c("media_scope"), "auto")
    legacy_layout <- econ_seq_config_value(variable, md, cfg, c("national_data_layout", "national_layout"), "auto")
    common_allocation_basis <- econ_seq_config_value(variable, md, cfg, c("allocation_basis"), "auto")
    common_allocation_weight_col <- as.character(econ_seq_config_value(
      variable, md, cfg, c("allocation_weight_col", "weight_col"), NA_character_
    ))[1]
    variable_target_population_col <- as.character(econ_seq_config_value(
      variable, md, cfg, c("target_population_col", "households_col", "target_households_col"), target_population_col %||% market_size_col %||% NA_character_
    ))[1]
    variable_population_col <- as.character(econ_seq_config_value(
      variable, md, cfg, c("population_col", "market_size_col"), population_col %||% market_size_col %||% NA_character_
    ))[1]
    semantics_raw <- econ_seq_config_value(variable, md, cfg, c("support_semantics", "metric_semantics", "support_type"), NA_character_)
    semantics_source <- if (!is.na(semantics_raw) && nzchar(as.character(semantics_raw))) "declared" else "diagnostic_inference"
    support_semantics <- if (identical(semantics_source, "declared")) {
      econ_seq_normalize_scope_value(
        semantics_raw,
        c("additive_total", "spend", "impressions", "clicks", "views", "reach_count", "grp", "trp", "reach_percent", "frequency", "intensive_shared", "rate", "index", "cpm", "cpc"),
        "additive_total", "support_semantics"
      )
    } else econ_seq_infer_support_semantics(variable)
    if (support_semantics %in% c("spend", "impressions", "clicks", "views", "reach_count")) support_semantics <- "additive_total"
    if (support_semantics %in% c("trp")) support_semantics <- "grp"
    if (support_semantics %in% c("rate", "index", "cpm", "cpc")) support_semantics <- "intensive_shared"
    variable_role <- md[as.character(md[["variable"]]) == variable, role][1]
    if (support_semantics %in% c("frequency", "intensive_shared") && !identical(variable_role, "reach_frequency")) {
      stop(
        "Support variable '", variable, "' is ", support_semantics,
        ", not additive response-curve support. Declare a valid reach/frequency specification or provide additive support.",
        call. = FALSE
      )
    }
    resolve_state <- function(kind, value_col) {
      scope_value <- econ_seq_config_value(
        variable, md, cfg, c(paste0(kind, "_scope")), legacy_scope
      )
      layout_value <- econ_seq_config_value(
        variable, md, cfg, c(paste0(kind, "_national_layout")), legacy_layout
      )
      exposure_value <- econ_seq_config_value(
        variable, md, cfg,
        c(paste0(kind, "_observed_or_imputed"), paste0(kind, "_exposure_status"), "allocated_exposure_status", "exposure_status"),
        "auto"
      )
      scope <- econ_seq_normalize_scope_value(
        scope_value, c("auto", "group_specific", "national", "already_allocated"), "auto", paste0(kind, "_scope")
      )
      layout <- econ_seq_normalize_scope_value(
        layout_value, c("auto", "repeated_by_group", "single_national_total", "already_allocated"),
        "auto", paste0(kind, "_national_layout")
      )
      scope_source <- if (identical(scope, "auto")) "diagnostic_inference" else "declared"
      layout_source <- if (identical(layout, "auto")) "diagnostic_inference" else "declared"
      if (identical(scope, "auto") && identical(layout, "auto")) {
        inferred <- econ_seq_infer_scope_layout(dt, value_col, time_col, tolerance)
        scope <- inferred$scope
        layout <- inferred$layout
      } else {
        if (identical(scope, "auto")) scope <- econ_seq_infer_media_scope(dt, value_col, time_col, tolerance)
        if (identical(layout, "auto")) layout <- econ_seq_infer_national_layout(dt, value_col, time_col, tolerance)
      }
      if (scope %in% c("group_specific", "already_allocated")) layout <- "already_allocated"
      observed <- econ_seq_normalize_scope_value(
        exposure_value, c("auto", "observed", "imputed"), "auto", paste0(kind, "_observed_or_imputed")
      )
      if (identical(observed, "auto")) observed <- if (scope %in% c("group_specific", "already_allocated")) "observed" else "imputed"
      list(
        scope = scope,
        scope_source = scope_source,
        layout = layout,
        layout_source = layout_source,
        observed = observed,
        mechanically_allocated = identical(scope, "national"),
        hierarchical_variation_eligible = scope %in% c("group_specific", "already_allocated") && identical(observed, "observed")
      )
    }
    spend_state <- resolve_state("spend", spend_col)
    support_state <- resolve_state("support", variable)
    if (identical(variable, spend_col) &&
        (!identical(spend_state$scope, support_state$scope) || !identical(spend_state$layout, support_state$layout))) {
      stop("Variable '", variable, "' uses one column for spend and support but declares incompatible spend/support scopes.", call. = FALSE)
    }
    spend_allocation_basis <- econ_seq_config_value(
      variable, md, cfg, c("spend_allocation_basis"), common_allocation_basis
    )
    support_allocation_basis <- econ_seq_config_value(
      variable, md, cfg, c("support_allocation_basis"), common_allocation_basis
    )
    spend_allocation_weight_col <- as.character(econ_seq_config_value(
      variable, md, cfg, c("spend_allocation_weight_col"), common_allocation_weight_col
    ))[1]
    support_allocation_weight_col <- as.character(econ_seq_config_value(
      variable, md, cfg, c("support_allocation_weight_col"), common_allocation_weight_col
    ))[1]
    variable_invalid_fallback <- econ_seq_normalize_scope_value(
      econ_seq_config_value(variable, md, cfg, c("invalid_allocation_fallback"), invalid_allocation_fallback),
      c("error", "equal"), invalid_allocation_fallback, "invalid_allocation_fallback"
    )

    raw_spend_col <- econ_seq_safe_internal_col("seq_raw_spend__", spend_col)
    allocated_spend_col <- econ_seq_safe_internal_col("seq_alloc_spend__", spend_col)
    raw_support_col <- econ_seq_safe_internal_col("seq_raw_support__", variable)
    allocated_support_col <- econ_seq_safe_internal_col("seq_alloc_support__", variable)
    if (!(raw_spend_col %in% names(dt))) dt[, (raw_spend_col) := get(spend_col)]
    if (!(raw_support_col %in% names(dt))) dt[, (raw_support_col) := get(variable)]

    process_additive <- function(value_col, raw_value_col, measure, state, allocation_basis, allocation_weight_col) {
      extracted <- econ_seq_extract_period_total(
        data = dt, value_col = value_col, time_col = time_col,
        media_scope = state$scope, national_data_layout = state$layout,
        tolerance = tolerance
      )
      row_values <- merge(
        dt[, .(allocation_row_id__ = sequential_row_id__, time_value__ = get(time_col))],
        data.table::copy(extracted)[, time_value__ := get(time_col)][, (time_col) := NULL],
        by = "time_value__", all.x = TRUE, sort = FALSE
      )
      if (identical(state$scope, "national")) {
        weights <- econ_seq_allocation_weights(
          data = dt,
          time_col = time_col,
          group_col = group_col,
          allocation_basis = allocation_basis,
          allocation_weight_col = allocation_weight_col,
          target_population_col = variable_target_population_col,
          population_col = variable_population_col,
          invalid_allocation_fallback = variable_invalid_fallback
        )
        weight_lookup <- weights[, .(
          allocation_row_id__ = allocation_row_id__, allocation_weight,
          allocation_basis_used, allocation_weight_col_used, fallback_used, active_group_n
        )]
        row_values <- merge(row_values, weight_lookup, by = "allocation_row_id__", all.x = TRUE, sort = FALSE)
      } else {
        row_values[, `:=`(
          allocation_weight = 1,
          allocation_basis_used = "not_applicable_observed_group_specific",
          allocation_weight_col_used = NA_character_,
          fallback_used = NA_character_,
          active_group_n = data.table::uniqueN(dt[[group_col]])
        )]
      }
      data.table::setorder(row_values, allocation_row_id__)
      raw <- suppressWarnings(as.numeric(dt[[raw_value_col]]))
      allocated <- if (state$scope %in% c("group_specific", "already_allocated")) raw else row_values$original_national_total * row_values$allocation_weight
      row_values[, allocated_value__ := allocated]
      audit <- row_values[, .(
        variable = variable,
        measure = measure,
        source_col = value_col,
        media_scope = state$scope,
        media_scope_source = state$scope_source,
        national_data_layout = state$layout,
        national_data_layout_source = state$layout_source,
        allocation_basis = unique(allocation_basis_used)[1],
        allocation_weight_col = unique(allocation_weight_col_used)[1],
        support_semantics = if (identical(measure, "spend")) "additive_total" else support_semantics,
        support_semantics_source = if (identical(measure, "spend")) "known_additive_spend" else semantics_source,
        observed_or_imputed = state$observed,
        mechanically_allocated = state$mechanically_allocated,
        original_national_total,
        allocated_total = if (all(is.finite(allocated_value__))) sum(allocated_value__) else NA_real_,
        allocation_reconciliation_error = if (all(is.finite(allocated_value__))) sum(allocated_value__) - original_national_total[1] else NA_real_,
        source_metric = if (identical(measure, "spend")) "spend" else support_semantics,
        source_unit = if (identical(measure, "spend")) "cost_units" else "additive_support_units",
        allocated_metric = if (identical(measure, "spend")) "spend" else support_semantics,
        allocated_unit = if (identical(measure, "spend")) "cost_units" else "additive_support_units",
        raw_unit_reconciliation_error = if (all(is.finite(allocated_value__))) sum(allocated_value__) - original_national_total[1] else NA_real_,
        reconstructed_source_value = sum(allocated_value__),
        reconstructed_source_error = if (all(is.finite(allocated_value__))) sum(allocated_value__) - original_national_total[1] else NA_real_,
        source_nonmissing_n = source_nonmissing_n[1],
        source_missing_n = source_missing_n[1],
        layout_consistent = layout_consistent[1],
        incomplete_reason = incomplete_reason[1],
        fallback_used = paste(unique(fallback_used[!is.na(fallback_used) & nzchar(fallback_used)]), collapse = " | "),
        active_group_n = active_group_n[1],
        hierarchical_variation_eligible = state$hierarchical_variation_eligible
      ), by = time_value__]
      list(values = allocated, audit = audit, extracted = extracted)
    }

    spend_result <- process_additive(
      spend_col, raw_spend_col, "spend", spend_state,
      spend_allocation_basis, spend_allocation_weight_col
    )
    dt[, (allocated_spend_col) := spend_result$values]
    dt[, (spend_col) := get(allocated_spend_col)]
    audit_rows[[length(audit_rows) + 1L]] <- spend_result$audit

    if (identical(variable, spend_col)) {
      support_values <- spend_result$values
      support_audit <- data.table::copy(spend_result$audit)
      support_audit[, `:=`(measure = "support", source_col = variable, support_semantics = "additive_total",
                           support_semantics_source = "same_as_spend")]
    } else if (support_state$scope %in% c("group_specific", "already_allocated")) {
      support_result <- process_additive(
        variable, raw_support_col, "support", support_state,
        support_allocation_basis, support_allocation_weight_col
      )
      support_values <- support_result$values
      support_audit <- support_result$audit
    } else if (identical(support_semantics, "additive_total")) {
      support_result <- process_additive(
        variable, raw_support_col, "support", support_state,
        support_allocation_basis, support_allocation_weight_col
      )
      support_values <- support_result$values
      support_audit <- support_result$audit
    } else if (support_semantics %in% c("grp", "reach_percent")) {
      population_use_col <- c(variable_target_population_col, variable_population_col)
      population_use_col <- population_use_col[!is.na(population_use_col) & nzchar(population_use_col) & population_use_col %in% names(dt)][1]
      if (is.na(population_use_col) || !nzchar(population_use_col)) {
        stop("National ", support_semantics, " support for '", variable, "' requires population/households to convert to additive exposure.", call. = FALSE)
      }
      population_values <- suppressWarnings(as.numeric(dt[[population_use_col]]))
      if (any(!is.finite(population_values)) || any(population_values < 0)) {
        stop("Population/households are incomplete for national ", support_semantics, " support variable '", variable, "'.", call. = FALSE)
      }
      metric <- econ_seq_extract_period_total(dt, raw_support_col, time_col, support_state$scope, support_state$layout, tolerance)
      metric_rows <- merge(
        dt[, .(allocation_row_id__ = sequential_row_id__, time_value__ = get(time_col))],
        data.table::copy(metric)[, time_value__ := get(time_col)][, (time_col) := NULL],
        by = "time_value__", all.x = TRUE, sort = FALSE
      )
      data.table::setorder(metric_rows, allocation_row_id__)
      support_values <- metric_rows$original_national_total / 100 * population_values
      metric_rows[, support_value__ := support_values]
      metric_rows[, population_value__ := population_values]
      support_audit <- metric_rows[, .(
        variable = variable, measure = "support", source_col = variable,
        media_scope = support_state$scope, media_scope_source = support_state$scope_source,
        national_data_layout = support_state$layout, national_data_layout_source = support_state$layout_source,
        allocation_basis = "population_conversion", allocation_weight_col = population_use_col,
        support_semantics = if (identical(support_semantics, "grp")) "gross_impressions_from_grp" else "reach_count_from_percent",
        support_semantics_source = semantics_source,
        observed_or_imputed = "imputed", mechanically_allocated = TRUE,
        original_national_total = original_national_total[1],
        allocated_total = sum(support_value__),
        allocation_reconciliation_error = NA_real_,
        source_metric = support_semantics,
        source_unit = if (identical(support_semantics, "grp")) "rating_points" else "percent",
        allocated_metric = if (identical(support_semantics, "grp")) "gross_impressions" else "reach_count",
        allocated_unit = if (identical(support_semantics, "grp")) "impressions" else "people",
        raw_unit_reconciliation_error = NA_real_,
        reconstructed_source_value = 100 * sum(support_value__) / sum(population_value__),
        reconstructed_source_error = 100 * sum(support_value__) / sum(population_value__) - original_national_total[1],
        source_nonmissing_n = source_nonmissing_n[1], source_missing_n = source_missing_n[1],
        layout_consistent = layout_consistent[1], incomplete_reason = incomplete_reason[1],
        fallback_used = NA_character_, active_group_n = .N,
        hierarchical_variation_eligible = FALSE
      ), by = time_value__]
      support_state$mechanically_allocated <- TRUE
      support_state$hierarchical_variation_eligible <- FALSE
    } else {
      variable_role <- md[as.character(md[["variable"]]) == variable, role][1]
      if (!identical(variable_role, "reach_frequency")) {
        stop(
          "Support variable '", variable, "' is ", support_semantics,
          ", which is not a normal additive media exposure. Use role = 'reach_frequency' with its specialized model or provide additive support.",
          call. = FALSE
        )
      }
      metric <- econ_seq_extract_period_total(dt, raw_support_col, time_col, support_state$scope, support_state$layout, tolerance)
      metric_rows <- merge(
        dt[, .(allocation_row_id__ = sequential_row_id__, time_value__ = get(time_col))],
        data.table::copy(metric)[, time_value__ := get(time_col)][, (time_col) := NULL],
        by = "time_value__", all.x = TRUE, sort = FALSE
      )
      data.table::setorder(metric_rows, allocation_row_id__)
      support_values <- metric_rows$original_national_total
      support_audit <- metric_rows[, .(
        variable = variable, measure = "support", source_col = variable,
        media_scope = support_state$scope, media_scope_source = support_state$scope_source,
        national_data_layout = support_state$layout, national_data_layout_source = support_state$layout_source,
        allocation_basis = "shared_national_intensive_measure", allocation_weight_col = NA_character_,
        support_semantics = support_semantics, support_semantics_source = semantics_source,
        observed_or_imputed = "imputed", mechanically_allocated = TRUE,
        original_national_total = original_national_total[1], allocated_total = NA_real_,
        allocation_reconciliation_error = NA_real_,
        source_metric = support_semantics, source_unit = "intensive_metric",
        allocated_metric = support_semantics, allocated_unit = "shared_national_intensive_metric",
        raw_unit_reconciliation_error = NA_real_, reconstructed_source_value = original_national_total[1],
        reconstructed_source_error = 0,
        source_nonmissing_n = source_nonmissing_n[1], source_missing_n = source_missing_n[1],
        layout_consistent = layout_consistent[1], incomplete_reason = incomplete_reason[1],
        fallback_used = "kept_as_shared_national_intensive_measure", active_group_n = .N,
        hierarchical_variation_eligible = FALSE
      ), by = time_value__]
      support_state$mechanically_allocated <- TRUE
      support_state$hierarchical_variation_eligible <- FALSE
    }
    dt[, (allocated_support_col) := support_values]
    dt[, (variable) := get(allocated_support_col)]
    audit_rows[[length(audit_rows) + 1L]] <- support_audit

    config_rows[[ii]] <- data.table::data.table(
      variable = variable,
      spend_col = spend_col,
      model_support_col = variable,
      spend_scope = spend_state$scope,
      spend_scope_source = spend_state$scope_source,
      spend_national_layout = spend_state$layout,
      spend_national_layout_source = spend_state$layout_source,
      spend_observed_or_imputed = spend_state$observed,
      spend_mechanically_allocated = spend_state$mechanically_allocated,
      spend_hierarchical_variation_eligible = spend_state$hierarchical_variation_eligible,
      support_scope = support_state$scope,
      support_scope_source = support_state$scope_source,
      support_national_layout = support_state$layout,
      support_national_layout_source = support_state$layout_source,
      support_observed_or_imputed = support_state$observed,
      support_mechanically_allocated = support_state$mechanically_allocated,
      support_hierarchical_variation_eligible = support_state$hierarchical_variation_eligible,
      media_scope = support_state$scope,
      media_scope_source = support_state$scope_source,
      national_data_layout = support_state$layout,
      national_data_layout_source = support_state$layout_source,
      allocation_basis = spend_allocation_basis,
      allocation_weight_col = spend_allocation_weight_col,
      target_population_col = variable_target_population_col,
      population_col = variable_population_col,
      support_semantics = support_semantics,
      support_semantics_source = semantics_source,
      observed_or_imputed = support_state$observed,
      mechanically_allocated = support_state$mechanically_allocated,
      hierarchical_variation_eligible = support_state$hierarchical_variation_eligible,
      invalid_allocation_fallback = variable_invalid_fallback,
      raw_support_col = raw_support_col,
      allocated_support_col = allocated_support_col,
      raw_spend_col = raw_spend_col,
      allocated_spend_col = allocated_spend_col
    )
    spend_rows[[ii]] <- data.table::copy(spend_result$audit)[, .(
      national_spend = unique(original_national_total)[1],
      national_spend_complete = all(layout_consistent) && is.finite(unique(original_national_total)[1]),
      national_spend_incomplete_reason = paste(unique(incomplete_reason[!is.na(incomplete_reason) & nzchar(incomplete_reason)]), collapse = " | ")
    ), by = .(variable, time_value__)]
  }

  config_table <- data.table::rbindlist(config_rows, fill = TRUE)
  allocation_audit <- data.table::rbindlist(audit_rows, fill = TRUE)
  national_spend <- data.table::rbindlist(spend_rows, fill = TRUE)
  data.table::setnames(allocation_audit, "time_value__", time_col)
  data.table::setnames(national_spend, "time_value__", time_col)
  allocation_audit[config_table, on = "variable", `:=`(
    spend_scope = i.spend_scope,
    spend_national_layout = i.spend_national_layout,
    spend_observed_or_imputed = i.spend_observed_or_imputed,
    spend_mechanically_allocated = i.spend_mechanically_allocated,
    spend_hierarchical_variation_eligible = i.spend_hierarchical_variation_eligible,
    support_scope = i.support_scope,
    support_national_layout = i.support_national_layout,
    support_observed_or_imputed = i.support_observed_or_imputed,
    support_mechanically_allocated = i.support_mechanically_allocated,
    support_hierarchical_variation_eligible = i.support_hierarchical_variation_eligible
  )]
  md[config_table, on = "variable", `:=`(
    model_support_col = i.model_support_col,
    spend_scope = i.spend_scope,
    spend_national_layout = i.spend_national_layout,
    spend_observed_or_imputed = i.spend_observed_or_imputed,
    spend_mechanically_allocated = i.spend_mechanically_allocated,
    spend_hierarchical_variation_eligible = i.spend_hierarchical_variation_eligible,
    support_scope = i.support_scope,
    support_national_layout = i.support_national_layout,
    support_observed_or_imputed = i.support_observed_or_imputed,
    support_mechanically_allocated = i.support_mechanically_allocated,
    support_hierarchical_variation_eligible = i.support_hierarchical_variation_eligible,
    media_scope = i.media_scope,
    national_data_layout = i.national_data_layout,
    allocation_basis = i.allocation_basis,
    allocation_weight_col = i.allocation_weight_col,
    support_semantics = i.support_semantics,
    allocated_exposure_status = i.observed_or_imputed,
    mechanically_allocated = i.mechanically_allocated,
    hierarchical_variation_eligible = i.hierarchical_variation_eligible,
    sequential_raw_support_col = i.raw_support_col,
    sequential_allocated_support_col = i.allocated_support_col,
    sequential_raw_spend_col = i.raw_spend_col,
    sequential_allocated_spend_col = i.allocated_spend_col
  )]
  if (!("coef_hierarchy_scope" %in% names(md))) md[, coef_hierarchy_scope := "auto"]
  if (!("coef_hierarchy_scale" %in% names(md))) md[, coef_hierarchy_scale := 1]
  md[variable %in% config_table[support_hierarchical_variation_eligible == FALSE, variable], `:=`(
    coef_hierarchy_scope = "none",
    coef_hierarchy_scale = 0
  )]
  spend_map_out <- merge(sm, config_table, by = c("variable", "spend_col"), all.x = TRUE, sort = FALSE)
  dt[, sequential_row_id__ := NULL]
  list(
    package_info = econimap_output_metadata("canonicalize_sequential_media_panel", surface = "sequential_media_allocation"),
    data = dt[],
    metadata = md[],
    spend_map = spend_map_out[],
    media_config = config_table[],
    allocation_audit = allocation_audit[],
    national_spend = national_spend[],
    interpretation = "Raw media are preserved in sequential_raw_* columns; modeled media/spend use audited canonical group-by-time values. Mechanical allocation never creates geo identification."
  )
}

econ_seq_normalize_aggregation_rule <- function(x, default = "mean") {
  out <- tolower(trimws(as.character(x %||% default)[1]))
  if (is.na(out) || !nzchar(out)) out <- default
  out <- gsub("[ -]+", "_", out)
  if (out %in% c("deduplicate", "first", "dedupe")) out <- "deduplicate_first"
  if (out %in% c("weighted_mean", "population_weighted", "pop_weighted_mean")) out <- "population_weighted_mean"
  if (out %in% c("sales_weighted", "kpi_weighted_mean")) out <- "sales_weighted_mean"
  allowed <- c("sum", "mean", "population_weighted_mean", "sales_weighted_mean", "deduplicate_first", "user_defined")
  if (!(out %in% allowed)) stop("Unsupported national aggregation rule: ", out, ".", call. = FALSE)
  out
}

econ_seq_aggregate_national_series <- function(data,
                                               value_col,
                                               time_col,
                                               rule = "mean",
                                               weight_col = NULL,
                                               user_function = NULL,
                                               tolerance = 1e-8) {
  dt <- data.table::as.data.table(data)
  rule <- econ_seq_normalize_aggregation_rule(rule)
  if (!(value_col %in% names(dt))) stop("National aggregation column not found: ", value_col, call. = FALSE)
  if (rule %in% c("population_weighted_mean", "sales_weighted_mean") &&
      (is.null(weight_col) || is.na(weight_col) || !nzchar(weight_col) || !(weight_col %in% names(dt)))) {
    stop("Aggregation rule '", rule, "' for '", value_col, "' requires a valid weight_col.", call. = FALSE)
  }
  if (identical(rule, "user_defined") && !is.function(user_function)) {
    stop("user_defined aggregation for '", value_col, "' requires a function.", call. = FALSE)
  }
  result <- dt[, {
    value <- suppressWarnings(as.numeric(get(value_col)))
    finite <- is.finite(value)
    missing_n <- sum(!finite)
    aggregate <- NA_real_
    reason <- NA_character_
    if (identical(rule, "sum")) {
      if (all(finite)) aggregate <- sum(value) else reason <- "missing_values_prevent_sum"
    } else if (identical(rule, "mean")) {
      if (all(finite)) aggregate <- mean(value) else reason <- "missing_values_prevent_mean"
    } else if (identical(rule, "deduplicate_first")) {
      observed <- value[finite]
      if (!length(observed)) {
        reason <- "all_values_missing"
      } else if ((max(observed) - min(observed)) > tolerance * max(1, abs(mean(observed)))) {
        reason <- "deduplicated_values_disagree"
      } else {
        aggregate <- observed[1]
        if (missing_n > 0L) reason <- "deduplicated_rows_partially_missing"
      }
    } else if (rule %in% c("population_weighted_mean", "sales_weighted_mean")) {
      weight <- suppressWarnings(as.numeric(get(weight_col)))
      if (all(finite) && all(is.finite(weight)) && all(weight >= 0) && sum(weight) > 0) {
        aggregate <- sum(value * weight) / sum(weight)
      } else reason <- "missing_or_invalid_weighted_mean_inputs"
    } else {
      aggregate <- tryCatch(as.numeric(user_function(.SD))[1], error = function(e) NA_real_)
      if (!is.finite(aggregate)) reason <- "user_defined_aggregation_failed"
    }
    list(
      aggregated_value = aggregate,
      source_row_n = .N,
      source_nonmissing_n = sum(finite),
      source_missing_n = missing_n,
      aggregation_complete = is.finite(aggregate) && (missing_n == 0L || identical(rule, "user_defined")),
      incomplete_reason = reason
    )
  }, by = time_col]
  result[, `:=`(
    variable = value_col,
    aggregation_rule = rule,
    aggregation_weight_col = as.character(weight_col %||% NA_character_)
  )]
  result[]
}

econ_seq_control_aggregation_specs <- function(metadata_input,
                                               controls,
                                               root_control_aggregation = NULL) {
  md <- econ_seq_input_table(metadata_input, "metadata_input")
  md[, variable := as.character(variable)]
  cfg <- econ_seq_input_table(root_control_aggregation, "root_control_aggregation")
  if (nrow(cfg)) {
    if (!("variable" %in% names(cfg))) stop("root_control_aggregation must include variable.", call. = FALSE)
    cfg[, variable := as.character(variable)]
    if (anyDuplicated(cfg$variable)) stop("root_control_aggregation has duplicate variable rows.", call. = FALSE)
  }
  data.table::rbindlist(lapply(controls, function(variable) {
    rule <- econ_seq_config_value(
      variable, md, cfg,
      c("aggregation_rule", "root_aggregation_rule", "national_aggregation_rule"),
      "mean"
    )
    weight_col <- econ_seq_config_value(
      variable, md, cfg,
      c("aggregation_weight_col", "root_aggregation_weight_col", "weight_col"),
      NA_character_
    )
    data.table::data.table(
      variable = variable,
      aggregation_rule = econ_seq_normalize_aggregation_rule(rule, "mean"),
      aggregation_weight_col = as.character(weight_col)[1]
    )
  }), fill = TRUE)
}

econ_seq_root_controls <- function(data,
                                   metadata_input,
                                   media_variables,
                                   root_control_cols = NULL,
                                   root_control_mode = c("declared_controls", "all_nonmedia", "none")) {
  root_control_mode <- match.arg(root_control_mode)
  if (!is.null(root_control_cols)) return(unique(intersect(as.character(root_control_cols), names(data))))
  if (identical(root_control_mode, "none")) return(character())
  md <- prepare_metadata_shell(read_input_table(metadata_input))
  md <- md[!is_ucm_metadata_row(md)]
  md[, `:=`(variable = as.character(variable), role = standardize_role(role))]
  roles <- if (identical(root_control_mode, "declared_controls")) {
    c("control", "macro", "comp")
  } else {
    setdiff(unique(md$role), c("media", "reach_frequency", "organic_media", "organic_reach_frequency", "ucm"))
  }
  unique(intersect(md[role %in% roles & !(variable %in% media_variables), variable], names(data)))
}

econ_seq_root_panel <- function(data,
                                metadata_input,
                                dep_var_col,
                                group_col,
                                time_col,
                                entity_col,
                                spend_map = NULL,
                                media_variables = NULL,
                                national_spend = NULL,
                                root_control_cols = NULL,
                                root_control_mode = c("declared_controls", "all_nonmedia", "none"),
                                root_scope = c("national", "hierarchical_panel"),
                                kpi_aggregation_rule = "sum",
                                kpi_weight_col = NULL,
                                root_control_aggregation = NULL,
                                root_aggregation_functions = list(),
                                incomplete_period_action = c("error", "drop"),
                                root_pressure_scaling = c("auto", "none", "per_capita"),
                                root_pressure_col = NULL) {
  root_control_mode <- match.arg(root_control_mode)
  root_scope <- match.arg(root_scope)
  incomplete_period_action <- match.arg(incomplete_period_action)
  root_pressure_scaling <- match.arg(root_pressure_scaling)
  if (!is.list(root_aggregation_functions)) stop("root_aggregation_functions must be a named list.", call. = FALSE)
  dt <- econ_seq_input_table(data, "data")
  required <- c(dep_var_col, group_col, time_col, entity_col)
  missing <- setdiff(required, names(dt))
  if (length(missing)) stop("data is missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  if (dt[, anyDuplicated(data.table::data.table(group = get(group_col), period = get(time_col))) > 0L]) {
    stop("Sequential root data must have unique group_col + time_col rows.", call. = FALSE)
  }
  sm <- econ_seq_media_spend_map(dt, metadata_input, spend_map = spend_map, media_variables = media_variables)
  controls <- econ_seq_root_controls(
    data = dt,
    metadata_input = metadata_input,
    media_variables = sm$variable,
    root_control_cols = root_control_cols,
    root_control_mode = root_control_mode
  )
  root_pressure_col <- as.character(root_pressure_col %||% "")[1]
  if (!nzchar(root_pressure_col) || is.na(root_pressure_col)) root_pressure_col <- NULL
  if (!is.null(root_pressure_col) && !(root_pressure_col %in% names(dt))) {
    stop("root_pressure_col is not present in data: ", root_pressure_col, call. = FALSE)
  }
  if (identical(root_pressure_scaling, "per_capita") && is.null(root_pressure_col)) {
    stop("root_pressure_scaling = 'per_capita' requires a valid population, household, or other exposure column.", call. = FALSE)
  }
  input_cols <- unique(c(group_col, time_col, entity_col, dep_var_col, sm$spend_col, controls, root_pressure_col))
  out <- dt[, ..input_cols]
  data.table::setnames(out, c(group_col, time_col, entity_col, dep_var_col), c("root_group__", "root_time__", "root_entity__", "root_y__"))
  out[, root_y__ := suppressWarnings(as.numeric(root_y__))]
  spend_cols <- sm$spend_col
  spend_matrix <- as.matrix(out[, ..spend_cols])
  storage.mode(spend_matrix) <- "double"
  if (any(spend_matrix < 0, na.rm = TRUE)) stop("Sequential root spend must be non-negative.", call. = FALSE)
  row_spend_complete <- apply(spend_matrix, 1L, function(z) all(is.finite(z)))
  out[, root_total_paid_spend__ := NA_real_]
  out[row_spend_complete, root_total_paid_spend__ := rowSums(spend_matrix[row_spend_complete, , drop = FALSE])]
  hierarchical_scope_eligibility <- NULL
  aggregation_audit <- data.table::data.table()
  if (identical(root_scope, "hierarchical_panel")) {
    if (!("spend_hierarchical_variation_eligible" %in% names(sm))) {
      legacy_spend_eligible <- if ("hierarchical_variation_eligible" %in% names(sm)) {
        as.logical(sm$hierarchical_variation_eligible)
      } else rep(TRUE, nrow(sm))
      sm[, spend_hierarchical_variation_eligible := legacy_spend_eligible]
    }
    eligible_spend_cols <- sm[spend_hierarchical_variation_eligible %in% TRUE, spend_col]
    if (!length(eligible_spend_cols)) {
      stop("root_scope = 'hierarchical_panel' has no genuinely observed group-varying media. Population-allocated national media do not qualify.", call. = FALSE)
    }
    eligible_matrix <- as.matrix(out[, ..eligible_spend_cols])
    storage.mode(eligible_matrix) <- "double"
    eligible_complete <- apply(eligible_matrix, 1L, function(z) all(is.finite(z)))
    out[, root_identifiable_paid_spend__ := NA_real_]
    out[eligible_complete, root_identifiable_paid_spend__ := rowSums(eligible_matrix[eligible_complete, , drop = FALSE])]
    root_id <- econ_seq_layer_identification_diagnostics(
      data = out,
      spend_map = data.table::data.table(
        variable = "identifiable_total_paid_media",
        spend_col = "root_identifiable_paid_spend__",
        hierarchical_variation_eligible = TRUE
      ),
      group_col = "root_group__",
      time_col = "root_time__",
      layer_label = "hierarchical_total_paid_media_root"
    )
    group_variation <- out[, .(
      group_spend_sd = stats::sd(root_identifiable_paid_spend__, na.rm = TRUE),
      group_row_n = .N
    ), by = root_group__]
    between_group_variation <- out[, .(
      between_group_spend_sd = stats::sd(root_identifiable_paid_spend__, na.rm = TRUE)
    ), by = root_time__]
    eligible <- data.table::uniqueN(out$root_group__) > 1L &&
      sum(is.finite(group_variation$group_spend_sd) & group_variation$group_spend_sd > 1e-10) >= 2L &&
      sum(is.finite(between_group_variation$between_group_spend_sd) & between_group_variation$between_group_spend_sd > 1e-10) >= 4L &&
      identical(root_id$overall$identification_recommendation[1], "fit")
    hierarchical_scope_eligibility <- data.table::data.table(
      root_scope = "hierarchical_panel",
      group_n = data.table::uniqueN(out$root_group__),
      groups_with_media_variation_n = sum(is.finite(group_variation$group_spend_sd) & group_variation$group_spend_sd > 1e-10),
      periods_with_between_group_media_variation_n = sum(is.finite(between_group_variation$between_group_spend_sd) & between_group_variation$between_group_spend_sd > 1e-10),
      identification_recommendation = root_id$overall$identification_recommendation[1],
      eligible = eligible,
      decision = if (eligible) "fit_hierarchical_root" else "use_national_root",
      note = "A hierarchical root is opt-in only when genuinely observed total paid-media exposure has identifiable residual group variation. Mechanically allocated national media are excluded from this screen."
    )
    if (!eligible) {
      stop("root_scope = 'hierarchical_panel' requires identifiable cross-group total-media variation. Use the default root_scope = 'national' otherwise.", call. = FALSE)
    }
    incomplete <- !is.finite(out$root_y__) | !is.finite(out$root_total_paid_spend__)
    if (length(controls)) {
      control_matrix <- as.matrix(out[, ..controls])
      storage.mode(control_matrix) <- "double"
      incomplete <- incomplete | !apply(control_matrix, 1L, function(z) all(is.finite(z)))
    }
    aggregation_audit <- out[, .(
      root_group__, root_time__,
      aggregation_complete = !incomplete,
      incomplete_reason = data.table::fifelse(!is.finite(root_y__), "missing_kpi",
        data.table::fifelse(!is.finite(root_total_paid_spend__), "missing_media_spend",
          data.table::fifelse(incomplete, "missing_control", NA_character_)))
    )]
    if (any(incomplete)) {
      if (identical(incomplete_period_action, "error")) {
        stop("Hierarchical root has incomplete KPI, spend, or control rows. Review root national_aggregation_audit or use incomplete_period_action = 'drop'.", call. = FALSE)
      }
      out <- out[!incomplete]
    }
  }
  if (identical(root_scope, "national")) {
    kpi_rule <- econ_seq_normalize_aggregation_rule(kpi_aggregation_rule, "sum")
    kpi_function <- root_aggregation_functions[[dep_var_col]] %||% NULL
    kpi <- econ_seq_aggregate_national_series(
      data = dt,
      value_col = dep_var_col,
      time_col = time_col,
      rule = kpi_rule,
      weight_col = kpi_weight_col,
      user_function = kpi_function
    )
    data.table::setnames(kpi, c(time_col, "aggregated_value"), c("root_time__", "root_y__"))

    ns <- econ_seq_input_table(national_spend, "national_spend")
    if (!nrow(ns)) {
      ns <- data.table::rbindlist(lapply(seq_len(nrow(sm)), function(ii) {
        extracted <- econ_seq_extract_period_total(
          data = dt,
          value_col = sm$spend_col[ii],
          time_col = time_col,
          media_scope = as.character(sm$media_scope[ii] %||% "group_specific"),
          national_data_layout = as.character(sm$national_data_layout[ii] %||% "already_allocated")
        )
        extracted[, .(
          variable = sm$variable[ii],
          time_value__ = get(time_col),
          national_spend = original_national_total,
          national_spend_complete = layout_consistent & is.finite(original_national_total),
          national_spend_incomplete_reason = incomplete_reason
        )]
      }), fill = TRUE)
      data.table::setnames(ns, "time_value__", time_col)
    }
    required_ns <- c(time_col, "variable", "national_spend", "national_spend_complete")
    if (!all(required_ns %in% names(ns))) stop("national_spend is missing required columns.", call. = FALSE)
    media <- ns[, .(
      root_total_paid_spend__ = if (all(national_spend_complete %in% TRUE) && all(is.finite(national_spend))) sum(national_spend) else NA_real_,
      media_aggregation_complete = all(national_spend_complete %in% TRUE) && all(is.finite(national_spend)),
      media_incomplete_reason = paste(unique(national_spend_incomplete_reason[!is.na(national_spend_incomplete_reason) & nzchar(national_spend_incomplete_reason)]), collapse = " | ")
    ), by = time_col]
    data.table::setnames(media, time_col, "root_time__")
    national <- merge(
      kpi[, .(root_time__, root_y__, kpi_aggregation_complete = aggregation_complete,
              kpi_incomplete_reason = incomplete_reason)],
      media,
      by = "root_time__", all = TRUE, sort = FALSE
    )

    control_specs <- econ_seq_control_aggregation_specs(metadata_input, controls, root_control_aggregation)
    control_audits <- list()
    if (nrow(control_specs)) {
      for (ii in seq_len(nrow(control_specs))) {
        cc <- control_specs$variable[ii]
        control_function <- root_aggregation_functions[[cc]] %||% NULL
        ag <- econ_seq_aggregate_national_series(
          data = dt,
          value_col = cc,
          time_col = time_col,
          rule = control_specs$aggregation_rule[ii],
          weight_col = control_specs$aggregation_weight_col[ii],
          user_function = control_function
        )
        data.table::setnames(ag, c(time_col, "aggregated_value"), c("root_time__", cc))
        national <- merge(national, ag[, c("root_time__", cc), with = FALSE], by = "root_time__", all.x = TRUE, sort = FALSE)
        control_audits[[ii]] <- ag[, .(
          root_time__, component = "control", variable = cc,
          aggregation_rule, aggregation_weight_col,
          aggregation_complete, source_row_n, source_nonmissing_n, source_missing_n,
          incomplete_reason
        )]
      }
    }
    kpi_audit <- kpi[, .(
      root_time__, component = "kpi", variable = dep_var_col,
      aggregation_rule, aggregation_weight_col,
      aggregation_complete, source_row_n, source_nonmissing_n, source_missing_n,
      incomplete_reason
    )]
    media_audit <- media[, .(
      root_time__, component = "media_spend", variable = "total_paid_media",
      aggregation_rule = "scope_aware_sum", aggregation_weight_col = NA_character_,
      aggregation_complete = media_aggregation_complete,
      source_row_n = sm[, .N], source_nonmissing_n = NA_integer_, source_missing_n = NA_integer_,
      incomplete_reason = media_incomplete_reason
    )]
    aggregation_audit <- data.table::rbindlist(c(list(kpi_audit, media_audit), control_audits), fill = TRUE)
    incomplete_times <- aggregation_audit[aggregation_complete != TRUE | is.na(aggregation_complete), unique(root_time__)]
    if (length(incomplete_times)) {
      if (identical(incomplete_period_action, "error")) {
        stop("National root has incomplete KPI, media, or control periods: ",
             paste(head(as.character(incomplete_times), 8L), collapse = ", "),
             if (length(incomplete_times) > 8L) " ..." else "",
             ". Use incomplete_period_action = 'drop' only after reviewing national_aggregation_audit.", call. = FALSE)
      }
      national <- national[!(root_time__ %in% incomplete_times)]
    }
    national[, `:=`(root_group__ = "national", root_entity__ = "ALL")]
    out <- national[]
  }
  # Media pressure and the KPI must use the same exposure denominator. This
  # preserves raw-scale effectiveness while making curve pressure comparable
  # across differently sized groups, following Meridian's scaling principle.
  use_pressure_scaling <- !identical(root_pressure_scaling, "none") && !is.null(root_pressure_col)
  if (use_pressure_scaling && identical(root_scope, "national")) {
    exposure <- dt[, .(root_exposure__ = sum(suppressWarnings(as.numeric(get(root_pressure_col))), na.rm = TRUE)), by = time_col]
    data.table::setnames(exposure, time_col, "root_time__")
    out <- merge(out, exposure, by = "root_time__", all.x = TRUE, sort = FALSE)
  } else if (use_pressure_scaling) {
    out[, root_exposure__ := suppressWarnings(as.numeric(get(root_pressure_col)))]
  }
  if (use_pressure_scaling) {
    invalid_exposure <- !is.finite(out$root_exposure__) | out$root_exposure__ <= 0
    if (any(invalid_exposure)) {
      stop("root_pressure_scaling requires finite, positive exposure values on every usable root row.", call. = FALSE)
    }
    out[, `:=`(
      root_media_pressure__ = root_total_paid_spend__ / root_exposure__,
      root_y_model__ = root_y__ / root_exposure__,
      root_outcome_multiplier__ = root_exposure__
    )]
    pressure_mode_used <- "per_capita"
  } else {
    out[, `:=`(
      root_exposure__ = 1,
      root_media_pressure__ = root_total_paid_spend__,
      root_y_model__ = root_y__,
      root_outcome_multiplier__ = 1
    )]
    pressure_mode_used <- "none"
  }
  ordered_time <- sort(unique(out$root_time__))
  out[, root_time_index__ := match(root_time__, ordered_time)]
  out[, root_adstock_order__ := root_time_index__]
  for (cc in controls) {
    values <- suppressWarnings(as.numeric(out[[cc]]))
    finite <- values[is.finite(values)]
    if (!length(finite) || stats::sd(finite) <= 1e-12) {
      out[, (cc) := NULL]
      controls <- setdiff(controls, cc)
    } else out[, (cc) := values]
  }
  usable <- is.finite(out$root_y_model__) & is.finite(out$root_media_pressure__) & is.finite(out$root_total_paid_spend__)
  out <- out[usable]
  if (nrow(out) < 12L) stop("Sequential root has fewer than 12 usable panel rows.", call. = FALSE)
  if (stats::sd(out$root_media_pressure__) <= 1e-12) stop("Total paid-media pressure has no usable variation for the sequential root.", call. = FALSE)
  list(
    data = out[],
    spend_map = sm[],
    control_cols = controls,
    time_values = ordered_time,
    media_variables = sm$variable,
    root_scope = root_scope,
    root_pressure_scaling = pressure_mode_used,
    root_pressure_col = if (use_pressure_scaling) root_pressure_col else NA_character_,
    hierarchical_scope_eligibility = hierarchical_scope_eligibility,
    national_aggregation_audit = aggregation_audit[]
  )
}

econ_seq_root_curve_spec <- function(type = c("linear", "adstock_hill"),
                                     rrate = 0,
                                     anchor_saturation = 0.50,
                                     half_saturation = NULL,
                                     steepness = 1) {
  type <- match.arg(type)
  rrate <- min(max(suppressWarnings(as.numeric(rrate)[1]), 0), 0.95)
  anchor_saturation <- suppressWarnings(as.numeric(anchor_saturation)[1])
  if (!is.finite(anchor_saturation) || anchor_saturation <= 0 || anchor_saturation >= 1) anchor_saturation <- 0.50
  half_saturation <- suppressWarnings(as.numeric(half_saturation)[1])
  if (!is.finite(half_saturation) || half_saturation <= 0) half_saturation <- NA_real_
  steepness <- suppressWarnings(as.numeric(steepness)[1])
  if (!is.finite(steepness) || steepness <= 0) steepness <- 1
  list(
    type = type,
    rrate = if (identical(type, "linear")) 0 else rrate,
    anchor_saturation = if (identical(type, "linear")) NA_real_ else anchor_saturation,
    half_saturation = if (identical(type, "linear")) NA_real_ else half_saturation,
    steepness = if (identical(type, "linear")) NA_real_ else steepness,
    curve_parameter_n = if (identical(type, "linear")) 0L else 3L
  )
}

econ_seq_root_media_feature <- function(root_data, curve_spec) {
  dt <- data.table::copy(root_data)
  raw_spend <- pmax(suppressWarnings(as.numeric(dt$root_total_paid_spend__)), 0)
  raw_spend[!is.finite(raw_spend)] <- 0
  pressure <- if ("root_media_pressure__" %in% names(dt)) {
    pmax(suppressWarnings(as.numeric(dt$root_media_pressure__)), 0)
  } else raw_spend
  pressure[!is.finite(pressure)] <- 0
  outcome_multiplier <- if ("root_outcome_multiplier__" %in% names(dt)) {
    suppressWarnings(as.numeric(dt$root_outcome_multiplier__))
  } else rep(1, nrow(dt))
  outcome_multiplier[!is.finite(outcome_multiplier) | outcome_multiplier <= 0] <- 1
  spec <- econ_seq_root_curve_spec(
    type = curve_spec$type %||% "linear",
    rrate = curve_spec$rrate %||% 0,
    anchor_saturation = curve_spec$anchor_saturation %||% 0.50,
    half_saturation = curve_spec$half_saturation %||% NA_real_,
    steepness = curve_spec$steepness %||% 1
  )
  if (identical(spec$type, "linear")) {
    scale <- mean(pressure[pressure > 0], na.rm = TRUE)
    if (!is.finite(scale) || scale <= 1e-8) scale <- 1
    feature <- pressure / scale
    conversion <- sum(feature * outcome_multiplier) / sum(raw_spend)
    if (!is.finite(conversion) || conversion <= 1e-12) conversion <- 1e-12
    return(list(
      feature = feature,
      raw_spend = raw_spend,
      pressure = pressure,
      outcome_multiplier = outcome_multiplier,
      feature_scale = scale,
      effect_conversion = conversion,
      curve_spec = spec,
      half_saturation = NA_real_,
      adstock = raw,
      saturation = feature
    ))
  }

  order_col <- if ("root_adstock_order__" %in% names(dt)) "root_adstock_order__" else "root_time_index__"
  adstock <- numeric(nrow(dt))
  for (group_value in unique(as.character(dt$root_group__))) {
    idx <- which(as.character(dt$root_group__) == group_value)
    idx <- idx[order(as.numeric(dt[[order_col]][idx]), idx)]
    carry <- 0
    for (row_i in idx) {
      carry <- pressure[row_i] + spec$rrate * carry
      adstock[row_i] <- carry
    }
  }
  active_adstock <- adstock[is.finite(adstock) & adstock > 0]
  anchor_x <- if (length(active_adstock)) stats::median(active_adstock) else 1
  if (!is.finite(anchor_x) || anchor_x <= 1e-8) anchor_x <- 1
  half_saturation <- spec$half_saturation
  if (!is.finite(half_saturation) || half_saturation <= 1e-8) {
    half_saturation <- anchor_x * ((1 - spec$anchor_saturation) / spec$anchor_saturation) ^ (1 / spec$steepness)
  }
  z <- (pmax(adstock, 0) / pmax(half_saturation, 1e-12)) ^ spec$steepness
  saturation <- z / (1 + z)
  saturation[!is.finite(saturation)] <- 0
  scale <- mean(saturation[pressure > 0], na.rm = TRUE)
  if (!is.finite(scale) || scale <= 1e-8) scale <- 1
  feature <- saturation / scale
  conversion <- sum(feature * outcome_multiplier) / sum(raw_spend)
  if (!is.finite(conversion) || conversion <= 1e-12) conversion <- 1e-12
  spec$half_saturation <- half_saturation
  spec$anchor_saturation <- anchor_x ^ spec$steepness /
    (anchor_x ^ spec$steepness + half_saturation ^ spec$steepness)
  list(
    feature = feature,
    raw_spend = raw_spend,
    pressure = pressure,
    outcome_multiplier = outcome_multiplier,
    feature_scale = scale,
    effect_conversion = conversion,
    curve_spec = spec,
    half_saturation = half_saturation,
    adstock = adstock,
    saturation = saturation
  )
}

econ_seq_resolve_root_time_baseline <- function(root_time_baseline, root_data) {
  root_time_baseline <- match.arg(root_time_baseline, c("auto", "fourier", "knots"))
  if (!identical(root_time_baseline, "auto")) return(root_time_baseline)
  if (data.table::uniqueN(root_data$root_group__) > 1L) "knots" else "fourier"
}

econ_seq_root_time_basis <- function(time_index,
                                     root_time_baseline,
                                     root_fourier_harmonics = 2L,
                                     root_season_period = 52L,
                                     root_knot_n = 6L) {
  time_index <- suppressWarnings(as.numeric(time_index))
  unique_time_n <- data.table::uniqueN(time_index[is.finite(time_index)])
  if (identical(root_time_baseline, "knots")) {
    # A small shared natural-spline basis is a constrained knot baseline for
    # the frequentist root. It is deliberately common across groups; it is not
    # represented as Meridian's Bayesian random-walk intercept.
    df <- min(max(2L, as.integer(root_knot_n)[1]), max(2L, unique_time_n - 2L))
    basis <- tryCatch(splines::ns(time_index, df = df, intercept = FALSE), error = function(e) NULL)
    if (is.null(basis)) return(list(columns = list(), names = character(), type = "knots", effective_n = 0L))
    columns <- lapply(seq_len(ncol(basis)), function(ii) as.numeric(basis[, ii]))
    names(columns) <- paste0("time_knot_", seq_along(columns))
    return(list(columns = columns, names = names(columns), type = "knots", effective_n = length(columns)))
  }
  root_fourier_harmonics <- max(0L, as.integer(root_fourier_harmonics)[1])
  root_season_period <- as.numeric(root_season_period)[1]
  columns <- list()
  if (root_fourier_harmonics > 0L && is.finite(root_season_period) && root_season_period > 1) {
    for (kk in seq_len(root_fourier_harmonics)) {
      columns[[paste0("season_sin_", kk)]] <- sin(2 * pi * kk * time_index / root_season_period)
      columns[[paste0("season_cos_", kk)]] <- cos(2 * pi * kk * time_index / root_season_period)
    }
  }
  list(columns = columns, names = names(columns), type = "fourier", effective_n = length(columns))
}

econ_seq_build_root_design <- function(root_data,
                                       control_cols = character(),
                                       root_trend_spec = c("none", "linear"),
                                       root_fourier_harmonics = 2L,
                                       root_season_period = 52L,
                                       root_time_baseline = c("auto", "fourier", "knots"),
                                       root_knot_n = 6L,
                                       root_curve_spec = econ_seq_root_curve_spec()) {
  dt <- data.table::copy(root_data)
  root_trend_spec <- match.arg(root_trend_spec)
  root_time_baseline <- econ_seq_resolve_root_time_baseline(root_time_baseline, dt)
  media <- econ_seq_root_media_feature(dt, root_curve_spec)
  cols <- list(`(Intercept)` = rep(1, nrow(dt)), total_paid_media_scaled = media$feature)

  groups <- as.character(dt$root_group__)
  group_levels <- sort(unique(groups))
  if (length(group_levels) > 1L) {
    for (gg in group_levels[-1]) cols[[paste0("group__", make.names(gg))]] <- as.numeric(groups == gg)
  }
  time_index <- as.numeric(dt$root_time_index__)
  if (identical(root_trend_spec, "linear") && length(unique(time_index)) > 2L) {
    trend <- time_index - mean(time_index)
    trend_sd <- stats::sd(trend)
    cols$trend__ <- if (is.finite(trend_sd) && trend_sd > 1e-8) trend / trend_sd else trend
  }
  time_basis <- econ_seq_root_time_basis(
    time_index = time_index,
    root_time_baseline = root_time_baseline,
    root_fourier_harmonics = root_fourier_harmonics,
    root_season_period = root_season_period,
    root_knot_n = root_knot_n
  )
  cols <- c(cols, time_basis$columns)
  for (cc in control_cols) {
    z <- as.numeric(dt[[cc]])
    z <- z - mean(z, na.rm = TRUE)
    z_sd <- stats::sd(z, na.rm = TRUE)
    if (is.finite(z_sd) && z_sd > 1e-8) cols[[paste0("control__", cc)]] <- z / z_sd
  }
  X <- do.call(cbind, cols)
  storage.mode(X) <- "double"
  list(
    X = X,
    y = as.numeric(if ("root_y_model__" %in% names(dt)) dt$root_y_model__ else dt$root_y__),
    effect_scale = 1 / media$effect_conversion,
    effect_conversion = media$effect_conversion,
    media = media,
    root_time_baseline = time_basis$type,
    root_time_basis_cols = time_basis$names,
    root_time_basis_n = time_basis$effective_n
  )
}

econ_seq_parse_root_effect_prior <- function(root_effect_prior = NULL) {
  if (is.null(root_effect_prior)) return(list(active = FALSE, mean = NA_real_, sd = NA_real_, source = NA_character_))
  if (is.numeric(root_effect_prior)) {
    if (length(root_effect_prior) < 2L) stop("root_effect_prior numeric input must be c(mean, sd).", call. = FALSE)
    mean_value <- as.numeric(root_effect_prior[1])
    sd_value <- as.numeric(root_effect_prior[2])
    source <- "user_calibration_penalty"
  } else {
    mean_value <- suppressWarnings(as.numeric(root_effect_prior$mean %||% root_effect_prior$prior_mean))[1]
    sd_value <- suppressWarnings(as.numeric(root_effect_prior$sd %||% root_effect_prior$prior_sd))[1]
    source <- as.character(root_effect_prior$source %||% "user_calibration_penalty")[1]
  }
  if (!is.finite(mean_value) || !is.finite(sd_value) || sd_value <= 0) {
    stop("root_effect_prior needs finite mean and positive sd on outcome-per-cost scale.", call. = FALSE)
  }
  list(active = TRUE, mean = mean_value, sd = sd_value, source = source)
}

econ_seq_fit_root_pooled_lme <- function(design,
                                         root_data,
                                         root_effect_sign,
                                         root_curve_spec) {
  if (!requireNamespace("nlme", quietly = TRUE)) return(NULL)
  groups <- as.character(root_data$root_group__)
  if (data.table::uniqueN(groups) < 3L) return(NULL)
  X <- design$X
  if (!all(is.finite(X)) || any(!is.finite(design$y))) return(NULL)
  original_names <- colnames(X)
  safe_names <- make.names(original_names, unique = TRUE)
  colnames(X) <- safe_names
  media_col <- safe_names[match("total_paid_media_scaled", original_names)]
  if (!length(media_col) || is.na(media_col)) return(NULL)
  fit_data <- as.data.frame(X)
  fit_data$root_y__ <- design$y
  fit_data$root_group__ <- factor(groups)
  fixed_formula <- stats::as.formula(paste("root_y__ ~ 0 +", paste(safe_names, collapse = " + ")))
  random_formula <- stats::as.formula(paste0("~ 0 + ", media_col, " | root_group__"))
  mixed_fit <- tryCatch(
    nlme::lme(
      fixed = fixed_formula,
      random = random_formula,
      data = fit_data,
      method = "ML",
      na.action = stats::na.fail,
      control = nlme::lmeControl(returnObject = TRUE, msMaxIter = 100L, msVerbose = FALSE)
    ),
    error = function(e) NULL
  )
  if (is.null(mixed_fit)) return(NULL)
  fixed_beta <- nlme::fixef(mixed_fit)
  if (!(media_col %in% names(fixed_beta))) return(NULL)
  random_effects <- nlme::ranef(mixed_fit)
  random_by_group <- setNames(rep(0, nlevels(fit_data$root_group__)), levels(fit_data$root_group__))
  if (media_col %in% names(random_effects)) {
    random_by_group[rownames(random_effects)] <- as.numeric(random_effects[[media_col]])
  }
  beta_common <- as.numeric(fixed_beta[[media_col]])
  beta_by_row <- beta_common + unname(random_by_group[groups])
  fitted <- as.numeric(X %*% fixed_beta[safe_names] + (beta_by_row - beta_common) * X[, media_col])
  resid <- design$y - fitted
  raw_spend <- design$media$raw_spend
  weighted_feature <- design$media$feature * design$media$outcome_multiplier
  spend_total <- sum(raw_spend)
  if (!is.finite(spend_total) || spend_total <= 0) return(NULL)
  effect <- sum(beta_by_row * weighted_feature) / spend_total
  fixed_cov <- tryCatch(as.matrix(stats::vcov(mixed_fit)), error = function(e) matrix(NA_real_, 0L, 0L))
  fixed_media_sd <- if (media_col %in% rownames(fixed_cov)) sqrt(pmax(fixed_cov[media_col, media_col], 0)) else NA_real_
  random_variance <- tryCatch(suppressWarnings(as.numeric(nlme::VarCorr(mixed_fit)[1, "Variance"])), error = function(e) NA_real_)
  group_weight <- tapply(weighted_feature, groups, sum) / spend_total
  effect_sd <- sqrt(pmax(
    (fixed_media_sd * sum(weighted_feature) / spend_total)^2 +
      pmax(random_variance, 0) * sum(group_weight^2),
    0
  ))
  log_likelihood <- as.numeric(stats::logLik(mixed_fit))
  effective_k <- attr(stats::logLik(mixed_fit), "df") + root_curve_spec$curve_parameter_n
  root_aic <- -2 * log_likelihood + 2 * effective_k
  root_aicc <- if (length(resid) > effective_k + 1L) {
    root_aic + (2 * effective_k * (effective_k + 1L)) / (length(resid) - effective_k - 1L)
  } else Inf
  if ((identical(root_effect_sign, "positive") && effect < 0) ||
      (identical(root_effect_sign, "negative") && effect > 0)) return(NULL)
  slopes <- data.table::data.table(
    root_group__ = names(random_by_group),
    root_media_beta = beta_common + as.numeric(random_by_group),
    root_media_beta_deviation = as.numeric(random_by_group),
    root_effect_scale = sum(weighted_feature) / spend_total
  )
  out <- data.table::data.table(
    root_effectiveness = effect,
    root_effectiveness_analytic_sd = effect_sd,
    root_r_squared = if (stats::var(design$y) > 1e-12) 1 - sum(resid^2) / sum((design$y - mean(design$y))^2) else NA_real_,
    root_rmse = sqrt(mean(resid^2)),
    root_row_n = length(resid),
    root_design_rank = qr(X)$rank + data.table::uniqueN(groups),
    root_effect_scale = 1 / design$effect_conversion,
    root_effect_conversion = design$effect_conversion,
    root_media_beta = beta_common,
    root_sse = sum(resid^2),
    root_profile_objective = -2 * log_likelihood,
    root_aicc = root_aicc,
    root_curve_type = root_curve_spec$type,
    root_rrate = root_curve_spec$rrate,
    root_anchor_saturation = root_curve_spec$anchor_saturation,
    root_half_saturation = design$media$half_saturation,
    root_steepness = root_curve_spec$steepness,
    root_curve_parameter_n = root_curve_spec$curve_parameter_n,
    root_prior_active = FALSE,
    root_prior_mean = NA_real_,
    root_prior_sd = NA_real_,
    root_prior_source = NA_character_,
    root_effect_sign = root_effect_sign,
    root_sign_boundary_active = FALSE,
    root_fit_method = "frequentist_mixed_effects_profile_ml",
    root_geo_media_effect_mode = "partially_pooled_normal",
    root_geo_media_effect_group_n = data.table::uniqueN(groups),
    root_geo_media_effect_tau = sqrt(pmax(random_variance, 0)),
    root_time_baseline = design$root_time_baseline,
    root_time_basis_n = design$root_time_basis_n,
    root_time_basis_penalty_applied = FALSE
  )
  attr(out, "root_fitted_values") <- fitted
  attr(out, "root_residuals") <- resid
  attr(out, "root_media_timeline") <- design$media$pressure
  attr(out, "root_geo_media_effects") <- slopes
  out
}

econ_seq_fit_root_lm <- function(root_data,
                                 control_cols = character(),
                                 root_trend_spec = c("none", "linear"),
                                 root_fourier_harmonics = 2L,
                                 root_season_period = 52L,
                                 root_effect_prior = NULL,
                                 root_effect_sign = c("positive", "unconstrained", "negative"),
                                 root_curve_spec = econ_seq_root_curve_spec(),
                                 root_time_baseline = c("auto", "fourier", "knots"),
                                 root_knot_n = 6L,
                                 root_knot_penalty = 1,
                                 root_geo_media_effect = c("shared", "partially_pooled")) {
  root_trend_spec <- match.arg(root_trend_spec)
  root_effect_sign <- match.arg(root_effect_sign)
  root_geo_media_effect <- match.arg(root_geo_media_effect)
  design <- econ_seq_build_root_design(
    root_data = root_data,
    control_cols = control_cols,
    root_trend_spec = root_trend_spec,
    root_fourier_harmonics = root_fourier_harmonics,
    root_season_period = root_season_period,
    root_time_baseline = root_time_baseline,
    root_knot_n = root_knot_n,
    root_curve_spec = root_curve_spec
  )
  prior <- econ_seq_parse_root_effect_prior(root_effect_prior)
  if (identical(root_geo_media_effect, "partially_pooled") && !prior$active) {
    pooled <- econ_seq_fit_root_pooled_lme(
      design = design,
      root_data = root_data,
      root_effect_sign = root_effect_sign,
      root_curve_spec = design$media$curve_spec
    )
    if (!is.null(pooled)) return(pooled)
  }
  X <- design$X
  y <- design$y
  p <- ncol(X)
  effect_idx <- match("total_paid_media_scaled", colnames(X))
  cross_x <- crossprod(X)
  cross_y <- crossprod(X, y)
  base_beta <- tryCatch(qr.solve(X, y), error = function(e) rep(0, ncol(X)))
  base_resid <- y - as.numeric(X %*% base_beta)
  base_sigma2 <- sum(base_resid^2) / max(length(y) - qr(X)$rank, 1L)
  if (!is.finite(base_sigma2) || base_sigma2 <= 0) base_sigma2 <- 1
  diagonal_scale <- max(diag(cross_x), 1)
  penalty <- rep(diagonal_scale * 1e-10, p)
  prior_target <- rep(0, p)
  time_idx <- match(design$root_time_basis_cols, colnames(X), nomatch = 0L)
  time_idx <- time_idx[time_idx > 0L]
  root_knot_penalty <- suppressWarnings(as.numeric(root_knot_penalty)[1])
  if (identical(design$root_time_baseline, "knots") && length(time_idx) && is.finite(root_knot_penalty) && root_knot_penalty > 0) {
    penalty[time_idx] <- penalty[time_idx] + diagonal_scale * root_knot_penalty
  }
  if (isTRUE(prior$active)) {
    beta_sd <- prior$sd / abs(design$effect_conversion)
    beta_target <- prior$mean / design$effect_conversion
    prior_precision_scaled <- base_sigma2 / beta_sd^2
    penalty[effect_idx] <- penalty[effect_idx] + prior_precision_scaled
    prior_target[effect_idx] <- beta_target * prior_precision_scaled
  }
  system <- cross_x + diag(penalty, nrow = p)
  beta <- tryCatch(solve(system, cross_y + prior_target), error = function(e) qr.solve(system, cross_y + prior_target))
  beta <- as.numeric(beta)
  sign_boundary_active <- (identical(root_effect_sign, "positive") && beta[effect_idx] < 0) ||
    (identical(root_effect_sign, "negative") && beta[effect_idx] > 0)
  if (isTRUE(sign_boundary_active)) {
    other_idx <- setdiff(seq_len(p), effect_idx)
    beta[] <- 0
    if (length(other_idx)) {
      other_x <- X[, other_idx, drop = FALSE]
      beta[other_idx] <- tryCatch(qr.solve(other_x, y), error = function(e) rep(0, length(other_idx)))
    }
  }
  fitted <- as.numeric(X %*% beta)
  resid <- y - fitted
  rank_x <- qr(X)$rank
  sigma2 <- sum(resid^2) / max(length(y) - rank_x, 1L)
  cov_beta <- tryCatch(sigma2 * solve(system), error = function(e) matrix(NA_real_, nrow = p, ncol = p))
  effect <- sum(beta[effect_idx] * design$media$feature * design$media$outcome_multiplier) /
    sum(design$media$raw_spend)
  effect_sd <- sqrt(pmax(cov_beta[effect_idx, effect_idx], 0)) * abs(design$effect_conversion)
  sse <- sum(resid^2)
  root_profile_objective <- length(y) * log(pmax(sse / length(y), 1e-12))
  if (isTRUE(prior$active)) {
    root_profile_objective <- root_profile_objective + ((effect - prior$mean) / prior$sd)^2
  }
  effective_k <- rank_x + design$media$curve_spec$curve_parameter_n
  root_aic <- length(y) * log(pmax(sse / length(y), 1e-12)) + 2 * effective_k
  root_aicc <- if (length(y) > effective_k + 1L) {
    root_aic + (2 * effective_k * (effective_k + 1L)) / (length(y) - effective_k - 1L)
  } else {
    Inf
  }
  out <- data.table::data.table(
    root_effectiveness = effect,
    root_effectiveness_analytic_sd = effect_sd,
    root_r_squared = if (stats::var(y) > 1e-12) 1 - sum(resid^2) / sum((y - mean(y))^2) else NA_real_,
    root_rmse = sqrt(mean(resid^2)),
    root_row_n = length(y),
    root_design_rank = rank_x,
    root_effect_scale = design$effect_scale,
    root_effect_conversion = design$effect_conversion,
    root_media_beta = beta[effect_idx],
    root_sse = sse,
    root_profile_objective = root_profile_objective,
    root_aicc = root_aicc,
    root_curve_type = design$media$curve_spec$type,
    root_rrate = design$media$curve_spec$rrate,
    root_anchor_saturation = design$media$curve_spec$anchor_saturation,
    root_half_saturation = design$media$half_saturation,
    root_steepness = design$media$curve_spec$steepness,
    root_curve_parameter_n = design$media$curve_spec$curve_parameter_n,
    root_prior_active = prior$active,
    root_prior_mean = prior$mean,
    root_prior_sd = prior$sd,
    root_prior_source = prior$source,
    root_effect_sign = root_effect_sign,
    root_sign_boundary_active = sign_boundary_active,
    root_fit_method = if (prior$active) "frequentist_profile_penalized_likelihood" else "frequentist_profile_gaussian_mle",
    root_geo_media_effect_mode = if (identical(root_geo_media_effect, "partially_pooled") && prior$active) "shared_effect_prior_compatibility_fallback" else "shared",
    root_geo_media_effect_group_n = data.table::uniqueN(root_data$root_group__),
    root_geo_media_effect_tau = NA_real_,
    root_time_baseline = design$root_time_baseline,
    root_time_basis_n = design$root_time_basis_n,
    root_time_basis_penalty_applied = identical(design$root_time_baseline, "knots") && length(time_idx) > 0L
  )
  attr(out, "root_fitted_values") <- fitted
  attr(out, "root_residuals") <- resid
  attr(out, "root_media_timeline") <- design$media$pressure
  attr(out, "root_geo_media_effects") <- data.table::data.table(
    root_group__ = sort(unique(as.character(root_data$root_group__))),
    root_media_beta = beta[effect_idx],
    root_media_beta_deviation = 0,
    root_effect_scale = design$effect_conversion
  )
  out
}

econ_seq_root_curve_candidates <- function(root_media_transform = c("adstock_hill", "linear"),
                                           root_rrate_grid = c(0, 0.25, 0.50, 0.70),
                                           root_anchor_saturation_grid = c(0.30, 0.50, 0.70)) {
  root_media_transform <- match.arg(root_media_transform)
  linear <- list(econ_seq_root_curve_spec("linear"))
  if (identical(root_media_transform, "linear")) return(linear)
  rrate_grid <- sort(unique(pmin(pmax(suppressWarnings(as.numeric(root_rrate_grid)), 0), 0.95)))
  anchor_grid <- sort(unique(suppressWarnings(as.numeric(root_anchor_saturation_grid))))
  rrate_grid <- rrate_grid[is.finite(rrate_grid)]
  anchor_grid <- anchor_grid[is.finite(anchor_grid) & anchor_grid > 0.05 & anchor_grid < 0.95]
  if (!length(rrate_grid)) rrate_grid <- c(0, 0.50)
  if (!length(anchor_grid)) anchor_grid <- c(0.30, 0.50, 0.70)
  c(linear, unlist(lapply(rrate_grid, function(rrate) {
    lapply(anchor_grid, function(anchor) econ_seq_root_curve_spec("adstock_hill", rrate, anchor))
  }), recursive = FALSE))
}

# Deterministic three-dimensional Sobol sequence for optimizer initialization.
# These points are design points only: they are not priors and are never
# retained as a curve ensemble.
econ_seq_sobol_points <- function(n, dimension = 3L, skip = 8L, bits = 29L) {
  n <- max(1L, as.integer(n)[1]); dimension <- as.integer(dimension)[1]
  if (dimension < 1L || dimension > 3L) stop("The internal Sobol generator supports one to three dimensions.", call. = FALSE)
  bits <- max(8L, min(29L, as.integer(bits)[1]))
  directions <- matrix(0L, nrow = bits, ncol = dimension)
  directions[, 1L] <- as.integer(2 ^ (bits - seq_len(bits)))
  params <- list(
    list(s = 1L, a = 0L, m = 1L),
    list(s = 2L, a = 1L, m = c(1L, 3L))
  )
  if (dimension >= 2L) {
    for (dd in 2:dimension) {
      par <- params[[dd - 1L]]; s <- par$s
      directions[seq_len(s), dd] <- as.integer(par$m * 2 ^ (bits - seq_len(s)))
      if (bits > s) for (jj in (s + 1L):bits) {
        value <- bitwXor(directions[jj - s, dd], bitwShiftR(directions[jj - s, dd], s))
        if (s > 1L) for (kk in seq_len(s - 1L)) {
          if (bitwAnd(par$a, bitwShiftL(1L, s - 1L - kk)) != 0L) value <- bitwXor(value, directions[jj - kk, dd])
        }
        directions[jj, dd] <- value
      }
    }
  }
  indices <- seq.int(from = max(0L, as.integer(skip)[1]), length.out = n)
  out <- matrix(0, nrow = n, ncol = dimension)
  for (ii in seq_along(indices)) {
    gray <- bitwXor(indices[ii], bitwShiftR(indices[ii], 1L))
    for (dd in seq_len(dimension)) {
      value <- 0L
      for (jj in seq_len(bits)) if (bitwAnd(gray, bitwShiftL(1L, jj - 1L)) != 0L) {
        value <- bitwXor(value, directions[jj, dd])
      }
      out[ii, dd] <- as.numeric(value) / 2 ^ bits
    }
  }
  pmin(pmax(out, 1e-6), 1 - 1e-6)
}

econ_seq_root_nonlinear_bounds <- function(root_data,
                                           rrate_bounds = c(0, 0.95),
                                           half_saturation_multiple_bounds = c(0.05, 10),
                                           steepness_bounds = c(0.25, 5)) {
  pressure_col <- if ("root_media_pressure__" %in% names(root_data)) "root_media_pressure__" else "root_total_paid_spend__"
  active <- pmax(suppressWarnings(as.numeric(root_data[[pressure_col]])), 0)
  active <- active[is.finite(active) & active > 0]
  if (!length(active)) stop("Root nonlinear bounds require positive training-period media support.", call. = FALSE)
  ref <- stats::median(active)
  rr <- sort(as.numeric(rrate_bounds)[1:2]); mult <- sort(as.numeric(half_saturation_multiple_bounds)[1:2])
  shape <- sort(as.numeric(steepness_bounds)[1:2])
  if (any(!is.finite(rr)) || rr[1] < 0 || rr[2] >= 1 || rr[1] >= rr[2]) stop("rrate_bounds must satisfy 0 <= lower < upper < 1.", call. = FALSE)
  if (any(!is.finite(mult)) || mult[1] <= 0 || mult[1] >= mult[2]) stop("half_saturation_multiple_bounds must be positive and increasing.", call. = FALSE)
  if (any(!is.finite(shape)) || shape[1] <= 0 || shape[1] >= shape[2]) stop("steepness_bounds must be positive and increasing.", call. = FALSE)
  list(
    lower = c(rrate = rr[1], log_half_saturation = log(ref * mult[1]), log_steepness = log(shape[1])),
    upper = c(rrate = rr[2], log_half_saturation = log(ref * mult[2]), log_steepness = log(shape[2])),
    active_support_reference = ref,
    rrate_bounds = rr,
    half_saturation_bounds = ref * mult,
    steepness_bounds = shape
  )
}

econ_seq_fit_root_multistart <- function(root_data,
                                         control_cols,
                                         root_trend_spec,
                                         root_fourier_harmonics,
                                         root_season_period,
                                         root_effect_prior,
                                         root_effect_sign,
                                         root_nonlinear_starts = 24L,
                                         root_rrate_bounds = c(0, 0.95),
                                         root_half_saturation_multiple_bounds = c(0.05, 10),
                                         root_steepness_bounds = c(0.25, 5),
                                         root_optimizer_maxit = 300L,
                                         root_time_baseline = c("auto", "fourier", "knots"),
                                         root_knot_n = 6L,
                                         root_knot_penalty = 1,
                                         root_geo_media_effect = c("shared", "partially_pooled")) {
  bounds <- econ_seq_root_nonlinear_bounds(
    root_data, root_rrate_bounds, root_half_saturation_multiple_bounds, root_steepness_bounds
  )
  starts_u <- econ_seq_sobol_points(max(4L, as.integer(root_nonlinear_starts)[1]), 3L)
  starts <- sweep(starts_u, 2L, bounds$upper - bounds$lower, `*`)
  starts <- sweep(starts, 2L, bounds$lower, `+`)
  objective <- function(theta) {
    spec <- econ_seq_root_curve_spec(
      "adstock_hill", rrate = theta[1], half_saturation = exp(theta[2]), steepness = exp(theta[3])
    )
    fit <- tryCatch(econ_seq_fit_root_lm(
      root_data, control_cols, root_trend_spec, root_fourier_harmonics, root_season_period,
      root_effect_prior, root_effect_sign, spec,
      root_time_baseline = root_time_baseline, root_knot_n = root_knot_n,
      root_knot_penalty = root_knot_penalty, root_geo_media_effect = root_geo_media_effect
    ), error = function(e) NULL)
    if (is.null(fit) || !is.finite(fit$root_profile_objective[1])) return(1e30)
    fit$root_profile_objective[1]
  }
  runs <- lapply(seq_len(nrow(starts)), function(ii) {
    opt <- tryCatch(stats::optim(
      starts[ii, ], objective, method = "L-BFGS-B", lower = bounds$lower, upper = bounds$upper,
      control = list(maxit = max(50L, as.integer(root_optimizer_maxit)[1]), factr = 1e7)
    ), error = function(e) NULL)
    if (is.null(opt)) return(data.table::data.table(start_id = ii, convergence = 999L, objective = Inf,
      root_rrate = NA_real_, root_half_saturation = NA_real_, root_steepness = NA_real_, optimizer_message = "optimizer_error"))
    tol <- pmax((bounds$upper - bounds$lower) * 1e-4, 1e-6)
    at_bound <- any(abs(opt$par - bounds$lower) <= tol | abs(opt$par - bounds$upper) <= tol)
    data.table::data.table(
      start_id = ii, convergence = as.integer(opt$convergence), objective = as.numeric(opt$value),
      root_rrate = opt$par[1], root_half_saturation = exp(opt$par[2]), root_steepness = exp(opt$par[3]),
      optimizer_at_bound = at_bound, optimizer_message = as.character(opt$message %||% "")
    )
  })
  run_table <- data.table::rbindlist(runs, fill = TRUE)
  usable <- run_table[convergence == 0L & is.finite(objective)]
  if (!nrow(usable)) return(list(fit = NULL, runs = run_table[], bounds = bounds, diagnostics = data.table::data.table(
    nonlinear_fit_stable = FALSE, fallback_recommended = TRUE, fallback_reason = "no_converged_multistart_solution"
  )))
  best <- usable[which.min(objective)]
  best_spec <- econ_seq_root_curve_spec(
    "adstock_hill", rrate = best$root_rrate, half_saturation = best$root_half_saturation, steepness = best$root_steepness
  )
  best_fit <- econ_seq_fit_root_lm(
    root_data, control_cols, root_trend_spec, root_fourier_harmonics, root_season_period,
    root_effect_prior, root_effect_sign, best_spec,
    root_time_baseline = root_time_baseline, root_knot_n = root_knot_n,
    root_knot_penalty = root_knot_penalty, root_geo_media_effect = root_geo_media_effect
  )
  near <- usable[objective <= min(objective) + 2]
  spread <- if (nrow(near) > 1L) c(
    rrate = diff(range(near$root_rrate)) / diff(bounds$rrate_bounds),
    half = diff(range(log(near$root_half_saturation))) / diff(log(bounds$half_saturation_bounds)),
    steepness = diff(range(log(near$root_steepness))) / diff(log(bounds$steepness_bounds))
  ) else c(rrate = 0, half = 0, steepness = 0)
  flat <- any(is.finite(spread) & spread >= .25)
  repeated_bounds <- mean(usable$optimizer_at_bound %in% TRUE) >= .50
  unstable <- nrow(usable) < max(3L, ceiling(.25 * nrow(run_table)))
  reasons <- c(if (flat) "flat_profile_likelihood", if (repeated_bounds) "solutions_repeatedly_at_bounds", if (unstable) "low_multistart_convergence_rate")
  diagnostics <- data.table::data.table(
    nonlinear_start_design = "deterministic_sobol", nonlinear_start_n = nrow(run_table),
    nonlinear_converged_n = nrow(usable), nonlinear_near_optimum_n = nrow(near),
    profile_flat = flat, repeated_bound_solutions = repeated_bounds, solution_instability = unstable,
    nonlinear_fit_stable = !length(reasons), fallback_recommended = length(reasons) > 0L,
    fallback_reason = paste(reasons, collapse = " | ")
  )
  list(fit = best_fit[], spec = best_spec, runs = run_table[], bounds = bounds, diagnostics = diagnostics[])
}

econ_seq_select_root_curve <- function(root_data,
                                       control_cols,
                                       root_trend_spec,
                                       root_fourier_harmonics,
                                       root_season_period,
                                       root_effect_prior,
                                       root_media_transform = c("adstock_hill", "linear"),
                                       root_rrate_grid = c(0, 0.25, 0.50, 0.70),
                                       root_anchor_saturation_grid = c(0.30, 0.50, 0.70),
                                       root_curve_min_delta_aicc = 2,
                                       root_effect_sign = c("positive", "unconstrained", "negative"),
                                       root_nonlinear_starts = 24L,
                                       root_rrate_bounds = c(0, 0.95),
                                       root_half_saturation_multiple_bounds = c(0.05, 10),
                                       root_steepness_bounds = c(0.25, 5),
                                       root_optimizer_maxit = 300L,
                                       root_time_baseline = c("auto", "fourier", "knots"),
                                       root_knot_n = 6L,
                                       root_knot_penalty = 1,
                                       root_geo_media_effect = c("shared", "partially_pooled")) {
  root_media_transform <- match.arg(root_media_transform)
  root_effect_sign <- match.arg(root_effect_sign)
  minimum_delta <- max(0, suppressWarnings(as.numeric(root_curve_min_delta_aicc)[1]))
  linear_fit <- econ_seq_fit_root_lm(
    root_data, control_cols, root_trend_spec, root_fourier_harmonics, root_season_period,
    root_effect_prior, root_effect_sign, econ_seq_root_curve_spec("linear"),
    root_time_baseline = root_time_baseline, root_knot_n = root_knot_n,
    root_knot_penalty = root_knot_penalty, root_geo_media_effect = root_geo_media_effect
  )
  linear_fit[, `:=`(candidate_fit_ok = TRUE, candidate_role = "primary_linear_limit")]
  multistart <- if (identical(root_media_transform, "adstock_hill")) econ_seq_fit_root_multistart(
    root_data, control_cols, root_trend_spec, root_fourier_harmonics, root_season_period,
    root_effect_prior, root_effect_sign, root_nonlinear_starts, root_rrate_bounds,
    root_half_saturation_multiple_bounds, root_steepness_bounds, root_optimizer_maxit,
    root_time_baseline = root_time_baseline, root_knot_n = root_knot_n,
    root_knot_penalty = root_knot_penalty, root_geo_media_effect = root_geo_media_effect
  ) else list(fit = NULL, runs = data.table::data.table(), diagnostics = data.table::data.table(
    nonlinear_fit_stable = NA, fallback_recommended = FALSE, fallback_reason = "forced_linear"
  ))
  nonlinear_fit <- multistart$fit
  if (!is.null(nonlinear_fit)) nonlinear_fit[, `:=`(candidate_fit_ok = TRUE, candidate_role = "primary_multistart_mle")]

  # Retain the old coarse grid strictly as a diagnostic comparison.
  coarse_specs <- econ_seq_root_curve_candidates(
    root_media_transform = root_media_transform,
    root_rrate_grid = root_rrate_grid,
    root_anchor_saturation_grid = root_anchor_saturation_grid
  )
  coarse_specs <- Filter(function(x) !identical(x$type, "linear"), coarse_specs)
  coarse_fits <- lapply(coarse_specs, function(spec) {
    fit <- tryCatch(
      econ_seq_fit_root_lm(
        root_data = root_data,
        control_cols = control_cols,
        root_trend_spec = root_trend_spec,
        root_fourier_harmonics = root_fourier_harmonics,
        root_season_period = root_season_period,
        root_effect_prior = root_effect_prior,
        root_effect_sign = root_effect_sign,
        root_curve_spec = spec,
        root_time_baseline = root_time_baseline,
        root_knot_n = root_knot_n,
        root_knot_penalty = root_knot_penalty,
        root_geo_media_effect = root_geo_media_effect
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) return(data.table::data.table(
      root_curve_type = spec$type,
      root_rrate = spec$rrate,
      root_anchor_saturation = spec$anchor_saturation,
      root_aicc = Inf,
      candidate_fit_ok = FALSE
    ))
    fit[, `:=`(candidate_fit_ok = TRUE, candidate_role = "coarse_grid_diagnostic")]
    fit
  })
  candidate_table <- data.table::rbindlist(c(list(linear_fit), if (!is.null(nonlinear_fit)) list(nonlinear_fit) else list(), coarse_fits), fill = TRUE)
  selected_idx <- 1L
  if (!is.null(nonlinear_fit)) {
    improvement <- linear_fit$root_aicc[1] - nonlinear_fit$root_aicc[1]
    if (is.finite(improvement) && improvement >= minimum_delta) selected_idx <- 2L
  }
  selected <- candidate_table[selected_idx]
  selected_spec <- econ_seq_root_curve_spec(
    type = selected$root_curve_type[1],
    rrate = selected$root_rrate[1],
    anchor_saturation = selected$root_anchor_saturation[1],
    half_saturation = selected$root_half_saturation[1],
    steepness = selected$root_steepness[1]
  )
  candidate_table[, `:=`(
    root_curve_selected = .I == selected_idx,
    root_curve_delta_aicc = root_aicc - selected$root_aicc[1],
    root_curve_min_delta_aicc = minimum_delta,
    root_curve_selection_method = if (identical(root_media_transform, "linear")) "forced_linear" else "sobol_multistart_profile_mle_with_linear_guardrail"
  )]
  selected <- candidate_table[selected_idx]
  list(selected = selected[], selected_spec = selected_spec, candidates = candidate_table[],
       optimizer_runs = multistart$runs %||% data.table::data.table(),
       nonlinear_diagnostics = multistart$diagnostics %||% data.table::data.table())
}

econ_seq_bootstrap_time_indices <- function(n_time, block_length) {
  block_length <- max(1L, as.integer(block_length)[1])
  out <- integer()
  while (length(out) < n_time) {
    start <- sample.int(n_time, 1L)
    out <- c(out, ((start - 1L + seq_len(block_length) - 1L) %% n_time) + 1L)
  }
  out[seq_len(n_time)]
}

econ_seq_resample_root_residuals <- function(root_data, residuals, block_length) {
  dt <- data.table::copy(data.table::as.data.table(root_data))
  residuals <- suppressWarnings(as.numeric(residuals))
  if (length(residuals) != nrow(dt)) {
    stop("Root residual bootstrap received residuals that do not align to root_data.", call. = FALSE)
  }
  required <- c("root_time_index__", "root_group__")
  if (!all(required %in% names(dt))) {
    stop("Root residual bootstrap requires root_time_index__ and root_group__.", call. = FALSE)
  }
  dt[, root_residual__ := residuals]
  dt[, root_residual__ := root_residual__ - mean(root_residual__, na.rm = TRUE), by = root_group__]
  source <- dt[, .(root_residual__ = mean(root_residual__, na.rm = TRUE)),
               by = .(root_group__, root_time_index__)]
  time_ids <- sort(unique(dt$root_time_index__))
  source_position <- econ_seq_bootstrap_time_indices(length(time_ids), block_length)
  time_map <- data.table::data.table(
    root_time_index__ = time_ids,
    source_time_index__ = time_ids[source_position],
    bootstrap_source_position__ = source_position
  )
  # Joins can reorder rows when a panel is not already sorted. Restore the
  # exact input order before the synthetic outcome is constructed.
  lookup <- data.table::copy(dt)[, root_row_id__ := .I]
  mapped <- merge(
    lookup[, .(root_row_id__, root_group__, root_time_index__)],
    time_map,
    by = "root_time_index__",
    all.x = TRUE,
    sort = FALSE
  )
  mapped <- merge(
    mapped,
    source,
    by.x = c("root_group__", "source_time_index__"),
    by.y = c("root_group__", "root_time_index__"),
    all.x = TRUE,
    sort = FALSE
  )
  data.table::setorder(mapped, root_row_id__)
  list(
    residuals = mapped$root_residual__,
    source_time_map = time_map[],
    complete = all(is.finite(mapped$root_residual__))
  )
}

econ_seq_block_bootstrap_root <- function(root_data,
                                          control_cols,
                                          root_trend_spec,
                                          root_fourier_harmonics,
                                          root_season_period,
                                          root_effect_prior,
                                          root_curve_spec = econ_seq_root_curve_spec(),
                                          root_media_transform = c("adstock_hill", "linear"),
                                          root_rrate_grid = c(0, 0.25, 0.50, 0.70),
                                          root_anchor_saturation_grid = c(0.30, 0.50, 0.70),
                                          root_curve_min_delta_aicc = 2,
                                          root_effect_sign = c("positive", "unconstrained", "negative"),
                                          root_nonlinear_starts = 24L,
                                          root_rrate_bounds = c(0, 0.95),
                                          root_half_saturation_multiple_bounds = c(0.05, 10),
                                          root_steepness_bounds = c(0.25, 5),
                                          root_optimizer_maxit = 300L,
                                          root_time_baseline = c("auto", "fourier", "knots"),
                                          root_knot_n = 6L,
                                          root_knot_penalty = 1,
                                          root_geo_media_effect = c("shared", "partially_pooled"),
                                          reselect_curve = TRUE,
                                          reps = 200L,
                                          block_length = 4L,
                                          seed = 123L) {
  root_media_transform <- match.arg(root_media_transform)
  root_effect_sign <- match.arg(root_effect_sign)
  reps <- max(0L, as.integer(reps)[1])
  empty <- function() data.table::data.table(
    draw = integer(), root_effectiveness = numeric(), fit_ok = logical(),
    root_curve_type = character(), root_rrate = numeric(), root_anchor_saturation = numeric(),
    root_half_saturation = numeric(), root_steepness = numeric(),
    bootstrap_method = character(), original_media_timeline_preserved = logical(),
    curve_selection_repeated = logical()
  )
  if (!reps) return(empty())
  time_ids <- sort(unique(root_data$root_time_index__))
  if (length(time_ids) < 4L) return(empty())
  base_fit <- tryCatch(econ_seq_fit_root_lm(
    root_data = root_data,
    control_cols = control_cols,
    root_trend_spec = root_trend_spec,
    root_fourier_harmonics = root_fourier_harmonics,
    root_season_period = root_season_period,
    root_effect_prior = root_effect_prior,
    root_effect_sign = root_effect_sign,
    root_curve_spec = root_curve_spec,
    root_time_baseline = root_time_baseline,
    root_knot_n = root_knot_n,
    root_knot_penalty = root_knot_penalty,
    root_geo_media_effect = root_geo_media_effect
  ), error = function(e) NULL)
  if (is.null(base_fit)) return(empty())
  base_fitted <- attr(base_fit, "root_fitted_values")
  base_residuals <- attr(base_fit, "root_residuals")
  if (length(base_fitted) != nrow(root_data) || length(base_residuals) != nrow(root_data)) return(empty())
  econ_seq_with_seed(seed, {
    rows <- lapply(seq_len(reps), function(ii) {
      residual_sample <- econ_seq_resample_root_residuals(
        root_data = root_data,
        residuals = base_residuals,
        block_length = block_length
      )
      if (!isTRUE(residual_sample$complete)) {
        return(data.table::data.table(
          draw = ii, root_effectiveness = NA_real_, fit_ok = FALSE,
          root_curve_type = NA_character_, root_rrate = NA_real_, root_anchor_saturation = NA_real_,
          root_half_saturation = NA_real_, root_steepness = NA_real_,
          bootstrap_method = "moving_block_residual_original_media_timeline",
          original_media_timeline_preserved = TRUE,
          curve_selection_repeated = isTRUE(reselect_curve)
        ))
      }
      boot <- data.table::copy(root_data)
      boot[, root_y__ := base_fitted + residual_sample$residuals]
      selected_spec <- root_curve_spec
      if (isTRUE(reselect_curve)) {
        selected_spec <- tryCatch(
          econ_seq_select_root_curve(
            root_data = boot,
            control_cols = control_cols,
            root_trend_spec = root_trend_spec,
            root_fourier_harmonics = root_fourier_harmonics,
            root_season_period = root_season_period,
            root_effect_prior = root_effect_prior,
            root_media_transform = root_media_transform,
            root_rrate_grid = root_rrate_grid,
            root_anchor_saturation_grid = root_anchor_saturation_grid,
            root_curve_min_delta_aicc = root_curve_min_delta_aicc,
            root_effect_sign = root_effect_sign,
            root_nonlinear_starts = root_nonlinear_starts,
            root_rrate_bounds = root_rrate_bounds,
            root_half_saturation_multiple_bounds = root_half_saturation_multiple_bounds,
            root_steepness_bounds = root_steepness_bounds,
            root_optimizer_maxit = root_optimizer_maxit,
            root_time_baseline = root_time_baseline,
            root_knot_n = root_knot_n,
            root_knot_penalty = root_knot_penalty,
            root_geo_media_effect = root_geo_media_effect
          )$selected_spec,
          error = function(e) NULL
        )
      }
      if (is.null(selected_spec)) {
        return(data.table::data.table(draw = ii, root_effectiveness = NA_real_, fit_ok = FALSE,
                                      root_curve_type = NA_character_, root_rrate = NA_real_, root_anchor_saturation = NA_real_,
                                      root_half_saturation = NA_real_, root_steepness = NA_real_,
                                      bootstrap_method = "moving_block_residual_original_media_timeline",
                                      original_media_timeline_preserved = TRUE,
                                      curve_selection_repeated = isTRUE(reselect_curve)))
      }
      fit <- tryCatch(econ_seq_fit_root_lm(
        root_data = boot,
        control_cols = control_cols,
        root_trend_spec = root_trend_spec,
        root_fourier_harmonics = root_fourier_harmonics,
        root_season_period = root_season_period,
        root_effect_prior = root_effect_prior,
        root_effect_sign = root_effect_sign,
        root_curve_spec = selected_spec,
        root_time_baseline = root_time_baseline,
        root_knot_n = root_knot_n,
        root_knot_penalty = root_knot_penalty,
        root_geo_media_effect = root_geo_media_effect
      ), error = function(e) NULL)
      if (is.null(fit)) return(data.table::data.table(draw = ii, root_effectiveness = NA_real_, fit_ok = FALSE,
                                                       root_curve_type = selected_spec$type, root_rrate = selected_spec$rrate,
                                                       root_anchor_saturation = selected_spec$anchor_saturation,
                                                       root_half_saturation = selected_spec$half_saturation,
                                                       root_steepness = selected_spec$steepness,
                                                       bootstrap_method = "moving_block_residual_original_media_timeline",
                                                       original_media_timeline_preserved = TRUE,
                                                       curve_selection_repeated = isTRUE(reselect_curve)))
      data.table::data.table(
        draw = ii,
        root_effectiveness = fit$root_effectiveness[1],
        fit_ok = is.finite(fit$root_effectiveness[1]),
        root_curve_type = selected_spec$type,
        root_rrate = selected_spec$rrate,
        root_anchor_saturation = selected_spec$anchor_saturation,
        root_half_saturation = selected_spec$half_saturation,
        root_steepness = selected_spec$steepness,
        bootstrap_method = "moving_block_residual_original_media_timeline",
        original_media_timeline_preserved = TRUE,
        curve_selection_repeated = isTRUE(reselect_curve)
      )
    })
    data.table::rbindlist(rows, fill = TRUE)
  })
}

econ_seq_mix_diagnostics <- function(data, spend_map, time_col) {
  dt <- econ_seq_input_table(data, "data")
  rows <- lapply(seq_len(nrow(spend_map)), function(ii) {
    sc <- spend_map$spend_col[ii]
    out <- dt[, .(spend = sum(pmax(suppressWarnings(as.numeric(get(sc))), 0), na.rm = TRUE)), by = time_col]
    out[, variable := spend_map$variable[ii]]
    out
  })
  long <- data.table::rbindlist(rows, fill = TRUE)
  if (!nrow(long)) return(list(summary = data.table::data.table(), by_variable = data.table::data.table()))
  data.table::setorderv(long, c(time_col, "variable"))
  long[, total_spend := sum(spend), by = time_col]
  long[, spend_share := data.table::fifelse(total_spend > 0, spend / total_spend, 0)]
  wide <- data.table::dcast(long, stats::as.formula(paste(time_col, "~ variable")), value.var = "spend_share", fill = 0)
  share_cols <- setdiff(names(wide), time_col)
  churn <- if (nrow(wide) > 1L && length(share_cols)) {
    shares <- as.matrix(wide[, ..share_cols])
    mean(rowSums(abs(shares[-1, , drop = FALSE] - shares[-nrow(shares), , drop = FALSE])) / 2, na.rm = TRUE)
  } else 0
  by_variable <- long[, .(
    spend_total = sum(spend),
    mean_spend_share = mean(spend_share),
    spend_share_sd = stats::sd(spend_share),
    active_period_n = sum(spend > 0)
  ), by = variable]
  list(
    summary = data.table::data.table(
      media_mix_churn = churn,
      period_n = data.table::uniqueN(long[[time_col]]),
      variable_n = data.table::uniqueN(long$variable),
      interpretation = "Average total-variation change in the paid-media spend mix between adjacent observed periods."
    ),
    by_variable = by_variable[]
  )
}

econ_seq_rollup_map <- function(metadata_input, variables, rollup_map = NULL) {
  if (!is.null(rollup_map)) {
    rm <- econ_seq_input_table(rollup_map, "rollup_map")
  } else {
    rm <- econ_seq_input_table(metadata_input, "metadata_input")
  }
  if (!"variable" %in% names(rm)) rm[, variable := character()]
  if (!"rollup_path" %in% names(rm)) rm[, rollup_path := variable]
  rm <- rm[, .(variable = as.character(variable), rollup_path = as.character(rollup_path))]
  duplicate_variables <- unique(rm$variable[duplicated(rm$variable)])
  if (length(duplicate_variables)) {
    stop("rollup_map has duplicate variable rows: ", paste(duplicate_variables, collapse = ", "), call. = FALSE)
  }
  out <- data.table::data.table(variable = as.character(variables))
  out[rm, rollup_path := i.rollup_path, on = "variable"]
  out[is.na(rollup_path) | !nzchar(trimws(rollup_path)), rollup_path := variable]
  parse_path <- function(path) {
    # Only explicit hierarchy delimiters split a path. A slash is common in
    # analyst-facing labels (for example "CTV/OLV") and must remain literal.
    nodes <- trimws(unlist(strsplit(as.character(path), "\\s*(?:>|\\|)\\s*", perl = TRUE), use.names = FALSE))
    nodes <- nodes[nzchar(nodes)]
    if (!length(nodes)) nodes <- as.character(path)
    list(root = nodes[1], parent = if (length(nodes) > 1L) nodes[length(nodes) - 1L] else nodes[1], leaf = nodes[length(nodes)])
  }
  parsed <- lapply(out$rollup_path, parse_path)
  out[, `:=`(
    rollup_root = vapply(parsed, `[[`, character(1), "root"),
    rollup_parent = vapply(parsed, `[[`, character(1), "parent"),
    rollup_leaf = vapply(parsed, `[[`, character(1), "leaf")
  )]
  out[]
}

econ_seq_rollup_node_key <- function(x) {
  gsub("[^a-z0-9]+", "_", tolower(trimws(as.character(x))))
}

econ_seq_is_total_media_node <- function(x) {
  econ_seq_rollup_node_key(x) %in% c(
    "total", "total_media", "total_paid_media", "paid_media", "media", "all_media", "all_paid_media"
  )
}

econ_seq_split_rollup_path <- function(path) {
  nodes <- trimws(unlist(strsplit(as.character(path), "\\s*(?:>|\\|)\\s*", perl = TRUE), use.names = FALSE))
  nodes[nzchar(nodes)]
}

econ_seq_media_rollup_paths <- function(metadata_input, variables, rollup_map = NULL) {
  lookup <- econ_seq_rollup_map(metadata_input, variables, rollup_map = rollup_map)
  lookup[, media_nodes__ := lapply(rollup_path, function(path) {
    nodes <- econ_seq_split_rollup_path(path)
    if (length(nodes) && econ_seq_is_total_media_node(nodes[1])) nodes <- nodes[-1L]
    if (!length(nodes)) nodes <- character()
    nodes
  })]

  # A repeated terminal path normally represents a modeled child below the
  # declared reporting node (for example Meta campaign 1 and Meta campaign 2).
  # Add that modeled leaf so deeper optional layers remain distinguishable.
  terminal_key <- vapply(lookup$media_nodes__, function(nodes) {
    if (!length(nodes)) "" else paste(econ_seq_rollup_node_key(nodes), collapse = ">")
  }, character(1))
  repeated_terminal <- terminal_key %in% terminal_key[duplicated(terminal_key) | duplicated(terminal_key, fromLast = TRUE)] & nzchar(terminal_key)
  for (ii in which(repeated_terminal)) {
    nodes <- lookup$media_nodes__[[ii]]
    if (!identical(econ_seq_rollup_node_key(nodes[length(nodes)]), econ_seq_rollup_node_key(lookup$variable[ii]))) {
      lookup$media_nodes__[[ii]] <- c(nodes, lookup$variable[ii])
    }
  }
  lookup[, media_depth__ := lengths(media_nodes__)]
  lookup[]
}

econ_seq_parse_rollup_depths <- function(rollup_depths) {
  if (is.null(rollup_depths) || !length(rollup_depths)) rollup_depths <- c(0L, "leaf")
  parsed <- lapply(rollup_depths, function(x) {
    if (is.character(x)) {
      key <- tolower(trimws(x[1]))
      if (key %in% c("root", "total", "total_paid_media", "0")) return(list(key = "root", depth = 0L, label = "total_paid_media"))
      if (key %in% c("leaf", "leaves", "variable", "variables")) return(list(key = "leaf", depth = NA_integer_, label = "leaf"))
      value <- suppressWarnings(as.numeric(key))
      if (!is.finite(value) || value != floor(value) || value < 0) {
        stop("rollup_depths must use 0/root, positive media depths, or leaf.", call. = FALSE)
      }
      if (value == 0) return(list(key = "root", depth = 0L, label = "total_paid_media"))
      return(list(key = paste0("depth_", as.integer(value)), depth = as.integer(value), label = paste0("depth_", as.integer(value))))
    }
    value <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(value) || value != floor(value) || value < 0) {
      stop("rollup_depths must use 0/root, positive media depths, or leaf.", call. = FALSE)
    }
    if (value == 0) return(list(key = "root", depth = 0L, label = "total_paid_media"))
    list(key = paste0("depth_", as.integer(value)), depth = as.integer(value), label = paste0("depth_", as.integer(value)))
  })
  keys <- vapply(parsed, `[[`, character(1), "key")
  if (anyDuplicated(keys)) stop("rollup_depths contains duplicate layers.", call. = FALSE)
  ordering <- vapply(parsed, function(x) if (identical(x$key, "leaf")) Inf else x$depth, numeric(1))
  if (is.unsorted(ordering, strictly = TRUE)) {
    stop("rollup_depths must be ordered from total paid media toward the leaf level.", call. = FALSE)
  }
  data.table::rbindlist(lapply(parsed, as.data.frame), fill = TRUE)
}

econ_seq_target_rollup_nodes <- function(path_lookup, depth, short_path_action = c("leaf", "error")) {
  short_path_action <- match.arg(short_path_action)
  if (identical(depth$key[1], "root")) {
    return(data.table::data.table(
      variable = path_lookup$variable,
      rollup_depth = 0L,
      rollup_node = "total_paid_media",
      rollup_node_path = "total_paid_media",
      source_path_depth = path_lookup$media_depth__
    ))
  }
  if (identical(depth$key[1], "leaf")) {
    return(data.table::data.table(
      variable = path_lookup$variable,
      rollup_depth = NA_integer_,
      rollup_node = path_lookup$variable,
      rollup_node_path = paste0("leaf > ", path_lookup$variable),
      source_path_depth = path_lookup$media_depth__
    ))
  }
  requested_depth <- as.integer(depth$depth[1])
  short <- path_lookup$media_depth__ < requested_depth
  if (any(short) && identical(short_path_action, "error")) {
    stop(
      "rollup_depth ", requested_depth, " is deeper than the declared rollup_path for: ",
      paste(path_lookup$variable[short], collapse = ", "),
      call. = FALSE
    )
  }
  nodes <- lapply(seq_len(nrow(path_lookup)), function(ii) {
    path <- path_lookup$media_nodes__[[ii]]
    if (length(path) >= requested_depth) path[seq_len(requested_depth)] else character()
  })
  node <- vapply(seq_len(nrow(path_lookup)), function(ii) {
    if (length(nodes[[ii]])) nodes[[ii]][length(nodes[[ii]])] else path_lookup$variable[ii]
  }, character(1))
  node_path <- vapply(seq_len(nrow(path_lookup)), function(ii) {
    if (length(nodes[[ii]])) paste(nodes[[ii]], collapse = " > ") else paste0("leaf > ", path_lookup$variable[ii])
  }, character(1))
  data.table::data.table(
    variable = path_lookup$variable,
    rollup_depth = requested_depth,
    rollup_node = node,
    rollup_node_path = node_path,
    source_path_depth = path_lookup$media_depth__
  )
}

#' Build an optional media-rollup plan for sequential MMM.
#'
#' Numeric depths are counted below an optional total-paid-media path node:
#' depth 1 is the first media family, depth 2 its child, and so on.  Use
#' `leaf` for the modeled variables themselves.  Analysts may omit any
#' intermediate depth, for example `c(0, 3, "leaf")`.
build_sequential_rollup_plan <- function(metadata_input,
                                         media_variables = NULL,
                                         rollup_map = NULL,
                                         rollup_depths = c(0L, "leaf"),
                                         short_path_action = c("leaf", "error")) {
  short_path_action <- match.arg(short_path_action)
  md <- prepare_metadata_shell(read_input_table(metadata_input))
  md <- md[!is_ucm_metadata_row(md)]
  md[, `:=`(variable = as.character(variable), role = standardize_role(role))]
  available <- md[role %in% c("media", "reach_frequency"), variable]
  if (!is.null(media_variables)) available <- intersect(available, as.character(media_variables))
  if (!length(available)) stop("No paid-media variables available for rollup planning.", call. = FALSE)
  depth_spec <- econ_seq_parse_rollup_depths(rollup_depths)
  path_lookup <- econ_seq_media_rollup_paths(metadata_input, available, rollup_map = rollup_map)
  mappings <- lapply(seq_len(nrow(depth_spec)), function(ii) {
    target <- econ_seq_target_rollup_nodes(path_lookup, depth_spec[ii], short_path_action = short_path_action)
    target[, `:=`(layer_id = depth_spec$key[ii], layer_label = depth_spec$label[ii])]
    target
  })
  variable_mapping <- data.table::rbindlist(mappings, use.names = TRUE, fill = TRUE)
  layer_plan <- variable_mapping[, .(
    media_variable_n = data.table::uniqueN(variable),
    modeled_node_n = data.table::uniqueN(rollup_node_path),
    source_path_depth_min = min(source_path_depth),
    source_path_depth_max = max(source_path_depth),
    short_path_variable_n = sum(source_path_depth < rollup_depth[1])
  ), by = .(layer_id, layer_label, rollup_depth)]
  layer_plan[is.infinite(rollup_depth), rollup_depth := NA_integer_]
  list(
    package_info = econimap_output_metadata("build_sequential_rollup_plan", surface = "sequential_empirical_bayes"),
    layer_plan = layer_plan[],
    variable_mapping = variable_mapping[],
    path_lookup = path_lookup[, c("variable", "rollup_path", "media_nodes__", "media_depth__"), with = FALSE][],
    short_path_action = short_path_action,
    interpretation = "Numeric depths are counted below total paid media. Each selected depth is independently legal; omitted levels are not required."
  )
}

econ_seq_safe_generated_names <- function(node_paths, existing_names) {
  base <- paste0("seq_media__", gsub("_+", "_", gsub("[^a-z0-9]+", "_", tolower(node_paths))))
  base <- sub("_$", "", base)
  used <- as.character(existing_names)
  out <- character(length(base))
  for (ii in seq_along(base)) {
    candidate <- base[ii]
    suffix <- 1L
    while (candidate %in% c(used, out[seq_len(max(ii - 1L, 0L))])) {
      suffix <- suffix + 1L
      candidate <- paste0(base[ii], "__", suffix)
    }
    out[ii] <- candidate
  }
  out
}

#' Build one spend-based aggregate media layer for sequential MMM.
#'
#' The returned metadata replaces the source paid-media variables with the
#' selected rollup nodes.  Only observed spend is aggregated across media;
#' heterogeneous raw support measures are intentionally not summed.
build_sequential_rollup_layer <- function(data,
                                          metadata_input,
                                          spend_map = NULL,
                                          rollup_map = NULL,
                                          rollup_depth,
                                          media_variables = NULL,
                                          layer_metric = c("spend"),
                                          short_path_action = c("leaf", "error"),
                                          curve_type_default = c("hill", "weibull")) {
  layer_metric <- match.arg(layer_metric)
  short_path_action <- match.arg(short_path_action)
  curve_type_default <- match.arg(curve_type_default)
  depth_spec <- econ_seq_parse_rollup_depths(rollup_depth)
  if (nrow(depth_spec) != 1L || depth_spec$key[1] %in% c("root", "leaf")) {
    stop("build_sequential_rollup_layer() requires one positive numeric rollup_depth below total paid media.", call. = FALSE)
  }
  dt <- econ_seq_input_table(data, "data")
  sm <- econ_seq_media_spend_map(dt, metadata_input, spend_map = spend_map, media_variables = media_variables)
  if (!"model_support_col" %in% names(sm)) sm[, model_support_col := variable]
  if (!"hierarchical_variation_eligible" %in% names(sm)) sm[, hierarchical_variation_eligible := TRUE]
  if (!"support_hierarchical_variation_eligible" %in% names(sm)) {
    sm[, support_hierarchical_variation_eligible := as.logical(hierarchical_variation_eligible)]
  }
  if (!"mechanically_allocated" %in% names(sm)) sm[, mechanically_allocated := FALSE]
  if (!"spend_mechanically_allocated" %in% names(sm)) sm[, spend_mechanically_allocated := mechanically_allocated]
  if (!"spend_hierarchical_variation_eligible" %in% names(sm)) {
    sm[, spend_hierarchical_variation_eligible := as.logical(hierarchical_variation_eligible)]
  }
  if (!"spend_scope" %in% names(sm)) sm[, spend_scope := "group_specific"]
  if (!"spend_national_layout" %in% names(sm)) sm[, spend_national_layout := "already_allocated"]
  plan <- build_sequential_rollup_plan(
    metadata_input = metadata_input,
    media_variables = sm$variable,
    rollup_map = rollup_map,
    rollup_depths = rollup_depth,
    short_path_action = short_path_action
  )
  mapping <- plan$variable_mapping
  mapping <- merge(mapping, sm, by = "variable", all.x = TRUE, sort = FALSE)
  if (any(is.na(mapping$spend_col) | !nzchar(mapping$spend_col))) stop("Sequential rollup layer is missing a source spend mapping.", call. = FALSE)

  node_map <- mapping[, .(
    source_variables = paste(sort(variable), collapse = " | "),
    source_spend_columns = paste(sort(spend_col), collapse = " | "),
    source_variable_n = .N,
    spend_scope = if (data.table::uniqueN(spend_scope) == 1L) spend_scope[1] else "mixed_spend_scope",
    spend_national_layout = if (data.table::uniqueN(spend_national_layout) == 1L) spend_national_layout[1] else "mixed_spend_layout",
    spend_mechanically_allocated = any(spend_mechanically_allocated %in% TRUE),
    spend_hierarchical_variation_eligible = all(spend_hierarchical_variation_eligible %in% TRUE),
    # This node is modeled on summed spend. Do not inherit hierarchy eligibility
    # from heterogeneous leaf execution support such as impressions or clicks.
    support_scope = if (data.table::uniqueN(spend_scope) == 1L) spend_scope[1] else "generated_from_mixed_spend_scope",
    support_mechanically_allocated = any(spend_mechanically_allocated %in% TRUE),
    support_hierarchical_variation_eligible = all(spend_hierarchical_variation_eligible %in% TRUE),
    hierarchical_variation_eligible = all(spend_hierarchical_variation_eligible %in% TRUE),
    mechanically_allocated = any(spend_mechanically_allocated %in% TRUE)
  ), by = .(rollup_depth, rollup_node, rollup_node_path)]
  data.table::setorderv(node_map, c("rollup_node_path", "rollup_node"))
  node_map[, generated_variable := econ_seq_safe_generated_names(rollup_node_path, names(dt))]
  mapping[node_map, generated_variable := i.generated_variable, on = c("rollup_depth", "rollup_node", "rollup_node_path")]

  missing_source <- vapply(mapping$spend_col, function(cc) {
    z <- suppressWarnings(as.numeric(dt[[cc]]))
    sum(!is.finite(z) | is.na(z))
  }, integer(1))
  if (any(missing_source > 0L)) {
    bad <- paste0(mapping$variable[missing_source > 0L], " (", missing_source[missing_source > 0L], " missing)")
    stop("Sequential spend rollups require complete numeric observed spend. Missing/non-numeric values in: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  negative_source <- vapply(mapping$spend_col, function(cc) sum(as.numeric(dt[[cc]]) < 0, na.rm = TRUE), integer(1))
  if (any(negative_source > 0L)) {
    bad <- paste0(mapping$variable[negative_source > 0L], " (", negative_source[negative_source > 0L], " negative)")
    stop("Sequential spend rollups require non-negative observed spend. Negative values in: ", paste(bad, collapse = ", "), call. = FALSE)
  }

  out_data <- data.table::copy(dt)
  for (ii in seq_len(nrow(node_map))) {
    sources <- mapping[generated_variable == node_map$generated_variable[ii], spend_col]
    values <- as.matrix(out_data[, ..sources])
    storage.mode(values) <- "double"
    out_data[, (node_map$generated_variable[ii]) := rowSums(values)]
  }
  node_map[, `:=`(
    generated_spend_total = vapply(generated_variable, function(cc) sum(out_data[[cc]], na.rm = TRUE), numeric(1)),
    layer_metric = layer_metric,
    aggregation_basis = "observed_spend",
    source_media_replaced = TRUE
  )]

  # Normalize retained rows before binding generated rollup rows. Otherwise a
  # source metadata column can exist for only some rows and create numeric NAs
  # through rbind(fill = TRUE), which later invalidates the child Stan handoff.
  raw_metadata <- clean_metadata(metadata_input, curve_type_default = curve_type_default)
  raw_metadata[, `:=`(variable = as.character(variable), role = standardize_role(role))]
  source_variables <- sm$variable
  source_pressure <- raw_metadata[variable %in% source_variables, .(
    variable,
    source_exposure_scaling = as.character(exposure_scaling),
    source_exposure_denominator_col = as.character(exposure_denominator_col)
  )]
  mapping <- merge(mapping, source_pressure, by = "variable", all.x = TRUE, sort = FALSE)
  pressure_by_node <- mapping[, {
    # A generated spend node can retain a physical pressure interpretation only
    # when every child declares the same exposure denominator.  Mixed or absent
    # denominators deliberately fall back to the conservative relative-support path.
    same_pressure_basis <- isTRUE(.N > 0L &&
      all(!is.na(source_exposure_scaling) & source_exposure_scaling == "per_denominator") &&
      data.table::uniqueN(source_exposure_denominator_col[!is.na(source_exposure_denominator_col) & nzchar(source_exposure_denominator_col)]) == 1L &&
      !is.na(source_exposure_denominator_col[1]) && nzchar(source_exposure_denominator_col[1]))
    list(
      exposure_scaling = if (same_pressure_basis) "per_denominator" else "none",
      exposure_denominator_col = if (same_pressure_basis) source_exposure_denominator_col[1] else NA_character_,
      pressure_handoff_note = if (same_pressure_basis) {
        "All source media share one declared pressure denominator; aggregate spend is modeled per denominator."
      } else {
        "Aggregate spend is not pressure-scaled because source media lack a common declared denominator."
      }
    )
  }, by = generated_variable]
  node_map[pressure_by_node, `:=`(
    exposure_scaling = i.exposure_scaling,
    exposure_denominator_col = i.exposure_denominator_col,
    pressure_handoff_note = i.pressure_handoff_note
  ), on = "generated_variable"]
  retained_metadata <- raw_metadata[!(role %in% c("media", "reach_frequency") & variable %in% source_variables)]
  aggregate_metadata <- node_map[, .(
    variable = generated_variable,
    role = "media",
    effect_type = "media",
    spend_col = generated_variable,
    cost_col = generated_variable,
    source_entity = "GLOBAL",
    curve_type = curve_type_default,
    anchor_saturation = 0.50,
    rrate = 0,
    rrate_precision = 1,
    cvalue = 1,
    cvalue_precision = 1,
    dvalue = 1,
    dvalue_precision = 1,
    cvalue_from_anchor = TRUE,
    coef = 0,
    coef_precision = 1,
    coef_bound = "pos",
    rollup_path = paste0("total_paid_media > ", rollup_node_path),
    sequential_rollup_depth = rollup_depth,
    sequential_rollup_node = rollup_node,
    sequential_rollup_source_n = source_variable_n,
    sequential_aggregation_basis = "observed_spend",
    spend_scope = spend_scope,
    spend_national_layout = spend_national_layout,
    spend_mechanically_allocated = spend_mechanically_allocated,
    spend_hierarchical_variation_eligible = spend_hierarchical_variation_eligible,
    support_scope = support_scope,
    mechanically_allocated = mechanically_allocated,
    support_mechanically_allocated = support_mechanically_allocated,
    support_hierarchical_variation_eligible = support_hierarchical_variation_eligible,
    hierarchical_variation_eligible = hierarchical_variation_eligible,
    exposure_scaling = exposure_scaling,
    exposure_denominator_col = exposure_denominator_col,
    pressure_handoff_note = pressure_handoff_note,
    coef_hierarchy_scope = data.table::fifelse(hierarchical_variation_eligible, "auto", "none"),
    coef_hierarchy_scale = data.table::fifelse(hierarchical_variation_eligible, 1, 0)
  )]
  out_metadata <- data.table::rbindlist(list(retained_metadata, aggregate_metadata), use.names = TRUE, fill = TRUE)
  if (anyDuplicated(out_metadata$variable)) stop("Sequential rollup metadata has duplicate variables after source replacement.", call. = FALSE)
  out_spend_map <- node_map[, .(
    variable = generated_variable,
    spend_col = generated_variable,
    model_support_col = generated_variable,
    mechanically_allocated,
    spend_scope,
    spend_national_layout,
    spend_mechanically_allocated,
    spend_hierarchical_variation_eligible,
    support_scope,
    support_mechanically_allocated,
    support_hierarchical_variation_eligible,
    hierarchical_variation_eligible
  )]
  rollup_output_map <- aggregate_metadata[, .(variable, rollup_path)]
  list(
    package_info = econimap_output_metadata("build_sequential_rollup_layer", surface = "sequential_empirical_bayes"),
    data = out_data[],
    metadata = out_metadata[],
    spend_map = out_spend_map[],
    rollup_map = rollup_output_map[],
    node_map = node_map[],
    variable_mapping = mapping[],
    path_lookup = plan$path_lookup,
    aggregation_audit = node_map[],
    rollup_depth = as.integer(depth_spec$depth[1]),
    layer_metric = layer_metric,
    interpretation = "Source paid-media terms are replaced by spend-based aggregate nodes. Raw support is not aggregated across heterogeneous media."
  )
}

econ_seq_build_leaf_layer <- function(data,
                                      metadata_input,
                                      spend_map = NULL,
                                      rollup_map = NULL,
                                      media_variables = NULL,
                                      short_path_action = c("leaf", "error"),
                                      curve_type_default = c("hill", "weibull")) {
  short_path_action <- match.arg(short_path_action)
  curve_type_default <- match.arg(curve_type_default)
  dt <- econ_seq_input_table(data, "data")
  sm <- econ_seq_media_spend_map(dt, metadata_input, spend_map = spend_map, media_variables = media_variables)
  if (!"model_support_col" %in% names(sm)) sm[, model_support_col := variable]
  plan <- build_sequential_rollup_plan(
    metadata_input = metadata_input,
    media_variables = sm$variable,
    rollup_map = rollup_map,
    rollup_depths = "leaf",
    short_path_action = short_path_action
  )
  mapping <- data.table::copy(plan$variable_mapping[layer_id == "leaf"])
  mapping[, generated_variable := variable]
  mapping <- merge(mapping, sm, by = "variable", all.x = TRUE, sort = FALSE)
  md <- clean_metadata(metadata_input, curve_type_default = curve_type_default)
  md[, `:=`(variable = as.character(variable), role = standardize_role(role))]
  node_map <- mapping[, .(
    rollup_depth = NA_integer_,
    rollup_node,
    rollup_node_path,
    generated_variable,
    source_variables = variable,
    source_spend_columns = spend_col,
    source_variable_n = 1L,
    is_leaf_node = TRUE,
    decomposition_eligible = TRUE
  )]
  list(
    package_info = econimap_output_metadata("econ_seq_build_leaf_layer", surface = "sequential_empirical_bayes"),
    data = dt[],
    metadata = md[],
    spend_map = sm[],
    rollup_map = econ_seq_rollup_map(metadata_input, sm$variable, rollup_map = rollup_map)[, .(variable, rollup_path)],
    node_map = node_map[],
    variable_mapping = mapping[],
    path_lookup = plan$path_lookup,
    aggregation_audit = data.table::data.table(
      variable = sm$variable,
      source_spend_col = sm$spend_col,
      aggregation_basis = "original_leaf_support_and_observed_spend",
      source_media_replaced = FALSE
    ),
    rollup_depth = NA_integer_,
    layer_key = "leaf",
    is_leaf_layer = TRUE,
    interpretation = "Final sequential layer restores original modeled support variables and their observed spend mappings."
  )
}

econ_seq_apply_child_prior_overrides <- function(out, child_prior_overrides = NULL) {
  if (is.null(child_prior_overrides)) return(out[])
  ov <- econ_seq_input_table(child_prior_overrides, "child_prior_overrides")
  if (!"variable" %in% names(ov)) stop("child_prior_overrides must include variable.", call. = FALSE)
  if (anyDuplicated(ov$variable)) stop("child_prior_overrides has duplicate variable rows.", call. = FALSE)
  ov[, variable := as.character(variable)]
  for (cc in c("prior_mean", "prior_sd", "prior_precision", "transfer_scale")) {
    if (!(cc %in% names(ov))) ov[, (cc) := NA_real_]
    ov[, (cc) := suppressWarnings(as.numeric(get(cc)))]
  }
  out[ov, on = "variable", `:=`(
    override_prior_mean__ = i.prior_mean,
    override_prior_sd__ = i.prior_sd,
    override_prior_precision__ = i.prior_precision,
    override_transfer_scale__ = i.transfer_scale
  )]
  out[is.finite(override_prior_mean__), prior_mean := override_prior_mean__]
  out[is.finite(override_prior_sd__) & override_prior_sd__ > 0, prior_sd := override_prior_sd__]
  out[is.finite(override_prior_precision__) & override_prior_precision__ > 0,
      prior_sd := 1 / sqrt(override_prior_precision__)]
  out[is.finite(override_transfer_scale__) & override_transfer_scale__ > 0,
      prior_sd := prior_sd * override_transfer_scale__]
  out[is.finite(override_prior_mean__) |
        (is.finite(override_prior_sd__) & override_prior_sd__ > 0) |
        (is.finite(override_prior_precision__) & override_prior_precision__ > 0),
      prior_evidence_mode := "user_adjusted_parent_regularized"]
  out[, c("override_prior_mean__", "override_prior_sd__", "override_prior_precision__", "override_transfer_scale__") := NULL]
  out[]
}

econ_seq_classify_effectiveness <- function(mean_value,
                                            sd_value,
                                            minimum_signal_z = 1,
                                            near_zero_abs = 1e-8) {
  mean_value <- suppressWarnings(as.numeric(mean_value))
  sd_value <- suppressWarnings(as.numeric(sd_value))
  z <- mean_value / pmax(sd_value, near_zero_abs)
  out <- rep("near_zero_or_inconclusive", length(mean_value))
  out[is.finite(mean_value) & is.finite(sd_value) & sd_value > 0 &
        mean_value > near_zero_abs & z >= minimum_signal_z] <- "positive_transferable"
  out[is.finite(mean_value) & is.finite(sd_value) & sd_value > 0 &
        mean_value < -near_zero_abs & z <= -minimum_signal_z] <- "negative_not_transferable"
  out
}

econ_seq_root_rrate_distribution <- function(root_fit) {
  bootstrap <- data.table::as.data.table(root_fit$bootstrap_draws %||% data.table::data.table())
  bootstrap <- bootstrap[fit_ok %in% TRUE & root_curve_type %in% c("linear", "adstock_hill")]
  bootstrap_hill <- bootstrap[
    root_curve_type == "adstock_hill" & is.finite(root_rrate) & is.finite(root_anchor_saturation)
  ]
  candidates <- data.table::as.data.table(root_fit$root_curve_candidates %||% data.table::data.table())
  candidates <- candidates[candidate_fit_ok %in% TRUE & is.finite(root_aicc)]
  hill_candidates <- candidates[
    root_curve_type == "adstock_hill" & is.finite(root_rrate) & is.finite(root_anchor_saturation)
  ]

  nonlinear_weight <- NA_real_
  weight_source <- NA_character_
  if (nrow(bootstrap) >= 10L) {
    nonlinear_weight <- mean(bootstrap$root_curve_type == "adstock_hill")
    weight_source <- "root_residual_block_bootstrap_selection_frequency"
  } else if (nrow(hill_candidates)) {
    best_hill <- min(hill_candidates$root_aicc)
    linear <- candidates[root_curve_type == "linear"]
    if (nrow(linear)) {
      best_linear <- min(linear$root_aicc)
      weights <- exp(-0.5 * (c(best_linear, best_hill) - min(best_linear, best_hill)))
      nonlinear_weight <- weights[2] / sum(weights)
    } else {
      nonlinear_weight <- 1
    }
    weight_source <- "best_linear_vs_best_hill_aicc_weight"
  }
  if (!is.finite(nonlinear_weight)) nonlinear_weight <- 0
  nonlinear_weight <- pmin(pmax(nonlinear_weight, 0), 1)
  parent_curve_evidence_available <- nonlinear_weight > 0 &&
    (nrow(bootstrap_hill) >= 2L || nrow(hill_candidates) >= 1L)

  center <- spread <- anchor_center <- anchor_sd <- NA_real_
  rrate_source <- anchor_source <- "generic_default_with_linear_model_probability"
  if (nrow(bootstrap_hill) >= 2L) {
    center <- mean(bootstrap_hill$root_rrate)
    spread <- stats::sd(bootstrap_hill$root_rrate)
    anchor_center <- mean(bootstrap_hill$root_anchor_saturation)
    anchor_sd <- stats::sd(bootstrap_hill$root_anchor_saturation)
    rrate_source <- anchor_source <- "sequential_root_residual_block_bootstrap_hill_conditional"
  } else if (nrow(hill_candidates)) {
    hill_candidates[, hill_weight__ := exp(-0.5 * pmin(root_aicc - min(root_aicc), 50))]
    hill_candidates[, hill_weight__ := hill_weight__ / sum(hill_weight__)]
    center <- sum(hill_candidates$hill_weight__ * hill_candidates$root_rrate)
    spread <- sqrt(sum(hill_candidates$hill_weight__ * (hill_candidates$root_rrate - center)^2))
    anchor_center <- sum(hill_candidates$hill_weight__ * hill_candidates$root_anchor_saturation)
    anchor_sd <- sqrt(sum(hill_candidates$hill_weight__ * (hill_candidates$root_anchor_saturation - anchor_center)^2))
    rrate_source <- anchor_source <- "sequential_root_hill_conditional_aicc_candidates"
  }
  # This object carries parent-only nonlinear evidence. The child model's
  # generic curve prior is deliberately absent here and is combined exactly
  # once in econ_seq_apply_rrate_priors().
  parent_rrate_mean <- if (is.finite(center)) pmin(pmax(center, 0), 0.95) else NA_real_
  parent_rrate_sd <- if (is.finite(spread)) max(spread, 0.10) else NA_real_
  parent_anchor_mean <- if (is.finite(anchor_center)) pmin(pmax(anchor_center, 0.01), 0.99) else NA_real_
  parent_anchor_sd <- if (is.finite(anchor_sd)) max(anchor_sd, 0.10) else NA_real_
  evidence_weight <- if (isTRUE(parent_curve_evidence_available)) nonlinear_weight else 0
  curve_mode <- if (evidence_weight >= 0.67) {
    "predominantly_parent_nonlinear_evidence"
  } else if (evidence_weight > 0.10) {
    "attenuated_parent_nonlinear_evidence"
  } else {
    "no_parent_nonlinear_curve_evidence"
  }
  if (evidence_weight <= 0) rrate_source <- anchor_source <- "no_parent_nonlinear_curve_evidence"
  out <- data.table::data.table(
    curve_prior_available = evidence_weight > 0,
    parent_curve_evidence_available = isTRUE(parent_curve_evidence_available),
    curve_prior_mode = curve_mode,
    root_nonlinear_model_weight = nonlinear_weight,
    root_nonlinear_model_weight_source = weight_source,
    rrate_prior_mean = parent_rrate_mean,
    rrate_prior_sd = parent_rrate_sd,
    rrate_prior_source = rrate_source,
    anchor_saturation_prior_mean = parent_anchor_mean,
    anchor_saturation_prior_sd = parent_anchor_sd,
    anchor_saturation_prior_source = anchor_source
  )
  out[, `:=`(
    rrate_prior_precision = data.table::fifelse(is.finite(rrate_prior_sd) & rrate_prior_sd > 0,
                                                 root_nonlinear_model_weight / rrate_prior_sd^2, NA_real_),
    anchor_saturation_prior_precision = data.table::fifelse(
      is.finite(anchor_saturation_prior_sd) & anchor_saturation_prior_sd > 0,
      root_nonlinear_model_weight / anchor_saturation_prior_sd^2,
      NA_real_
    )
  )]
  out[]
}

econ_seq_valid_child_prior_overrides <- function(child_prior_overrides = NULL) {
  empty <- data.table::data.table(
    variable = character(),
    valid_effectiveness_override = logical(),
    override_validation_reason = character()
  )
  if (is.null(child_prior_overrides) || !nrow(child_prior_overrides)) return(empty)
  ov <- econ_seq_input_table(child_prior_overrides, "child_prior_overrides")
  if (!"variable" %in% names(ov)) stop("child_prior_overrides must include variable.", call. = FALSE)
  if (anyDuplicated(ov$variable)) stop("child_prior_overrides has duplicate variable rows.", call. = FALSE)
  for (cc in c("prior_mean", "prior_sd", "prior_precision")) {
    if (!cc %in% names(ov)) ov[, (cc) := NA_real_]
    ov[, (cc) := suppressWarnings(as.numeric(get(cc)))]
  }
  ov[, valid_effectiveness_override := is.finite(prior_mean) &
       ((is.finite(prior_sd) & prior_sd > 0) | (is.finite(prior_precision) & prior_precision > 0))]
  ov[, override_validation_reason := data.table::fcase(
    valid_effectiveness_override, "finite_mean_and_positive_sd_or_precision",
    !is.finite(prior_mean), "missing_finite_prior_mean",
    default = "missing_positive_prior_sd_or_precision"
  )]
  ov[, .(variable = as.character(variable), valid_effectiveness_override, override_validation_reason)]
}

econ_seq_identification_calibration <- function() {
  list(
    version = "sequential_identification_synthetic_v1",
    predominantly_prior_driven_max = 0.45,
    data_driven_min = 0.75,
    active_support_cv_scale = 0.10,
    active_period_scale = 26,
    minimum_active_rows = 8L,
    calibration_regimes = c(
      "independent_clean", "moderately_correlated", "near_collinear",
      "sparse_flighting", "near_constant_support", "national_repeated_support"
    ),
    interpretation = paste(
      "Thresholds classify a continuous observational identification score.",
      "They were calibrated against the package's labeled synthetic contracts",
      "and must not be interpreted as universal statistical cutoffs."
    )
  )
}

econ_seq_apply_branch_diagnostics <- function(prior_table,
                                              child_identification = NULL,
                                              child_prior_overrides = NULL,
                                              strong_child_prior_relaxation = 1.20,
                                              identification_calibration = econ_seq_identification_calibration()) {
  out <- data.table::copy(data.table::as.data.table(prior_table))
  strong_child_prior_relaxation <- max(as.numeric(strong_child_prior_relaxation)[1], 1)
  lower_threshold <- identification_calibration$predominantly_prior_driven_max
  upper_threshold <- identification_calibration$data_driven_min
  out[, `:=`(
    child_identification_recommendation = "fit",
    child_active_row_n = NA_integer_,
    child_identification_strength_0_1 = 1,
    child_identification_pooling_multiplier = 1 / strong_child_prior_relaxation^2,
    parent_shrinkage_multiplier = 1 / strong_child_prior_relaxation^2,
    child_prior_relaxation = strong_child_prior_relaxation,
    user_prior_override_present = FALSE,
    user_prior_override_valid = FALSE,
    override_validation_reason = "not_supplied"
  )]
  if (!is.null(child_identification) && nrow(child_identification)) {
    id <- econ_seq_input_table(child_identification, "child_identification")
    if (!"variable" %in% names(id)) stop("child_identification must include variable.", call. = FALSE)
    if (!"identification_recommendation" %in% names(id)) id[, identification_recommendation := "fit"]
    if (!"active_row_n" %in% names(id)) id[, active_row_n := NA_integer_]
    if (!"identification_strength_0_1" %in% names(id)) id[, identification_strength_0_1 := NA_real_]
    if (!"parent_shrinkage_multiplier" %in% names(id)) id[, parent_shrinkage_multiplier := NA_real_]
    id <- unique(id[, .(
      variable = as.character(variable),
      child_identification_recommendation__ = as.character(identification_recommendation),
      child_active_row_n__ = as.integer(active_row_n),
      child_identification_strength__ = as.numeric(identification_strength_0_1),
      parent_shrinkage_multiplier__ = as.numeric(parent_shrinkage_multiplier)
    )], by = "variable")
    out[id, on = "variable", `:=`(
      child_identification_recommendation = i.child_identification_recommendation__,
      child_active_row_n = i.child_active_row_n__,
      child_identification_strength_0_1 = i.child_identification_strength__,
      parent_shrinkage_multiplier = i.parent_shrinkage_multiplier__
    )]
  }
  overrides <- econ_seq_valid_child_prior_overrides(child_prior_overrides)
  if (nrow(overrides)) {
    out[overrides, on = "variable", `:=`(
      user_prior_override_present = TRUE,
      user_prior_override_valid = i.valid_effectiveness_override,
      override_validation_reason = i.override_validation_reason
    )]
  }
  out[!is.finite(parent_shrinkage_multiplier) | parent_shrinkage_multiplier <= 0,
      parent_shrinkage_multiplier := 1 / strong_child_prior_relaxation^2]
  out[, child_identification_pooling_multiplier := parent_shrinkage_multiplier]
  out[, branch_decision := data.table::fcase(
    user_prior_override_valid, "fit",
    !parent_positive_effect_transferred, "fit",
    parent_positive_effect_transferred & child_identification_recommendation == "fit", "fit",
    default = "strong_parent_shrinkage"
  )]
  # `branch_decision` remains an internal compatibility field for structural
  # remainder handling. Analyst-facing status never stops a valid weak branch.
  out[, fit_status := data.table::fcase(
    user_prior_override_valid, "fit_user_prior_dominant",
    !parent_positive_effect_transferred, "fit_default_prior_dominant",
    child_identification_strength_0_1 >= upper_threshold, "fit_data_dominant",
    child_identification_strength_0_1 >= lower_threshold, "fit_parent_regularized",
    default = "fit_strongly_regularized"
  )]
  out[, prior_dominance_classification := data.table::fcase(
    user_prior_override_valid, "user_prior_driven",
    !parent_positive_effect_transferred, "default_prior_driven",
    child_identification_strength_0_1 >= upper_threshold, "data_driven",
    child_identification_strength_0_1 >= lower_threshold, "parent_prior_and_data_blended",
    default = "parent_prior_driven"
  )]
  out[, `:=`(
    identification_calibration_version = identification_calibration$version,
    identification_prior_driven_max = lower_threshold,
    identification_data_driven_min = upper_threshold
  )]
  out[, branch_decision_reason := data.table::fcase(
    user_prior_override_valid, "valid_analyst_effectiveness_prior_supplied",
    !parent_positive_effect_transferred, "weak_parent_evidence_uses_broad_validated_model_default_without_stopping",
    branch_decision == "fit" & parent_positive_effect_transferred, "identified_child_relaxes_parent_inherited_shrinkage",
    default = "weak_or_moderate_child_identification_strengthens_shrinkage_without_stopping_the_branch"
  )]
  out[]
}

econ_seq_reference_effectiveness_calibration <- function(prior_table,
                                                         calibration_prefix = "sequential_effectiveness") {
  priors <- data.table::as.data.table(prior_table)
  required <- c("variable", "prior_mean", "prior_sd", "child_spend_total")
  if (!all(required %in% names(priors))) {
    stop("Sequential reference calibration needs variable, prior_mean, prior_sd, and child_spend_total.", call. = FALSE)
  }
  if (!"user_prior_override_valid" %in% names(priors)) priors[, user_prior_override_valid := FALSE]
  priors <- priors[
    branch_decision %in% c("fit", "strong_parent_shrinkage", "parent_retained", "parent_remainder") &
      (parent_positive_effect_transferred %in% TRUE | user_prior_override_valid %in% TRUE)
  ]
  if (!nrow(priors)) return(data.table::data.table())
  observed_lift <- priors$prior_mean * priors$child_spend_total
  observed_sd <- pmax(abs(priors$prior_sd * priors$child_spend_total), 1e-8)
  data.table::data.table(
    calibration_id = paste0(calibration_prefix, "_", priors$variable),
    variable = priors$variable,
    observed_lift = observed_lift,
    observed_lift_sd = observed_sd,
    sequential_reference_spend = priors$child_spend_total,
    sequential_reference_effectiveness = priors$prior_mean,
    sequential_reference_effectiveness_sd = priors$prior_sd,
    sequential_prior_application = "joint_reference_spend_calibration",
    evidence_source = priors$evidence_source,
    evidence_notes = "Joint prior on modeled contribution at observed child spend/support. It remains interpretable as effectiveness when the child curve updates."
  )
}

econ_seq_hierarchical_transfer_input <- function(prior_table,
                                                 tau_overrides = NULL,
                                                 include_effectiveness = TRUE,
                                                 include_adstock = TRUE,
                                                 effectiveness_tau_mode = c("learned", "fixed")) {
  effectiveness_tau_mode <- match.arg(effectiveness_tau_mode)
  x <- data.table::copy(data.table::as.data.table(prior_table))
  if (!nrow(x)) return(list(effectiveness = data.table::data.table(), adstock = data.table::data.table()))
  value_from <- function(candidates, default = NA_real_) {
    hit <- candidates[candidates %in% names(x)]
    if (!length(hit)) return(rep(default, nrow(x)))
    out <- rep(NA_real_, nrow(x))
    for (cc in hit) {
      z <- suppressWarnings(as.numeric(x[[cc]]))
      fill <- !is.finite(out) & is.finite(z)
      out[fill] <- z[fill]
    }
    out[!is.finite(out)] <- default
    out
  }
  if (!"sequential_parent_id" %in% names(x)) x[, sequential_parent_id := "total_paid_media"]
  x[, `:=`(
    parent_mean__ = value_from(c("sequential_parent_mean", "sequential_root_mean", "prior_mean")),
    parent_sd__ = value_from(c("sequential_parent_sd_component", "sequential_root_sd_component", "sequential_parent_sd", "sequential_root_sd", "prior_sd")),
    heterogeneity_sd__ = value_from(c("sequential_child_heterogeneity_sd_component"), NA_real_),
    mix_sd__ = value_from(c("sequential_mix_sd_component"), 0)
  )]
  x[!is.finite(heterogeneity_sd__) | heterogeneity_sd__ <= 0,
    heterogeneity_sd__ := pmax(abs(parent_mean__) * 0.50, parent_sd__, 1e-6)]
  x[, `:=`(
    effect_tau_mean__ = 0,
    effect_tau_sd__ = pmax(heterogeneity_sd__, 1e-6),
    effect_aggregate_sd__ = pmax(abs(parent_mean__) * 0.20, mix_sd__, parent_sd__ * 0.25, 1e-6),
    effect_child_noise_sd__ = pmax(abs(parent_mean__) * 0.05, parent_sd__ * 0.05, 1e-6),
    adstock_tau_mean__ = 0,
    adstock_tau_sd__ = pmax(value_from(c("rrate_prior_sd"), 0.10), 0.05),
    adstock_child_noise_sd__ = pmax(value_from(c("rrate_prior_sd"), 0.05) * 0.10, 0.01)
  )]
  # tau is a layer-level dispersion parameter. Parent centers remain distinct,
  # but all sibling sets in this transition inform one shared deviation scale.
  x[, `:=`(
    effect_tau_mean__ = stats::median(effect_tau_mean__, na.rm = TRUE),
    effect_tau_sd__ = stats::median(effect_tau_sd__, na.rm = TRUE),
    adstock_tau_mean__ = stats::median(adstock_tau_mean__, na.rm = TRUE),
    adstock_tau_sd__ = stats::median(adstock_tau_sd__, na.rm = TRUE)
  )]

  if (!is.null(tau_overrides) && nrow(data.table::as.data.table(tau_overrides))) {
    ov <- econ_seq_input_table(tau_overrides, "sequential_tau_overrides")
    if (!"parent_id" %in% names(ov)) stop("sequential_tau_overrides must include parent_id.", call. = FALSE)
    if (anyDuplicated(ov$parent_id)) stop("sequential_tau_overrides has duplicate parent_id rows.", call. = FALSE)
    layer_mapping <- c(
      effectiveness_tau_mean = "effect_tau_mean__",
      effectiveness_tau_sd = "effect_tau_sd__",
      adstock_tau_mean = "adstock_tau_mean__",
      adstock_tau_sd = "adstock_tau_sd__"
    )
    parent_mapping <- c(
      effectiveness_aggregate_sd = "effect_aggregate_sd__",
      effectiveness_child_noise_sd = "effect_child_noise_sd__",
      adstock_child_noise_sd = "adstock_child_noise_sd__"
    )
    ov[, parent_id := as.character(parent_id)]
    for (src in names(layer_mapping)) {
      if (!src %in% names(ov)) next
      vals <- suppressWarnings(as.numeric(ov[[src]]))
      vals <- vals[is.finite(vals)]
      if (!length(vals)) next
      if (data.table::uniqueN(round(vals, 12)) > 1L) {
        stop(src, " is a shared layer override and must have one value.", call. = FALSE)
      }
      x[, (layer_mapping[[src]]) := vals[1]]
    }
    for (src in names(parent_mapping)) {
      if (!src %in% names(ov)) next
      dest <- parent_mapping[[src]]
      vals <- suppressWarnings(as.numeric(ov[[src]]))
      tmp <- data.table::data.table(parent_id = ov$parent_id, value__ = vals)
      global <- tmp[parent_id == "*" & is.finite(value__)]$value__
      if (length(global)) x[, (dest) := global[1]]
      x[tmp[parent_id != "*" & is.finite(value__)], on = c("sequential_parent_id" = "parent_id"), (dest) := i.value__]
    }
  }

  effect <- data.table::data.table()
  if (isTRUE(include_effectiveness)) {
    effect <- x[
      parent_positive_effect_transferred %in% TRUE & is.finite(child_spend_total) & child_spend_total > 0,
      .(
        variable, parent_id = sequential_parent_id,
        parent_mean = parent_mean__, parent_sd = pmax(parent_sd__, 1e-6),
        reference_spend = child_spend_total,
        tau_prior_mean = pmax(effect_tau_mean__, 0), tau_prior_sd = pmax(effect_tau_sd__, 1e-6),
        tau_mode = effectiveness_tau_mode,
        fixed_tau = pmax(effect_tau_sd__, 1e-6),
        aggregate_sd = pmax(effect_aggregate_sd__, 1e-6),
        child_noise_sd = pmax(effect_child_noise_sd__, 1e-6)
      )
    ]
  }
  adstock <- data.table::data.table()
  if (isTRUE(include_adstock)) {
    adstock <- x[
      parent_positive_effect_transferred %in% TRUE & curve_prior_available %in% TRUE &
        is.finite(rrate_prior_mean) & rrate_prior_mean > 0 & rrate_prior_mean < 1 &
        is.finite(rrate_prior_sd) & rrate_prior_sd > 0,
      .(
        variable, parent_id = sequential_parent_id,
        parent_mean = rrate_prior_mean, parent_sd = pmax(rrate_prior_sd, 1e-6),
        tau_prior_mean = pmax(adstock_tau_mean__, 0), tau_prior_sd = pmax(adstock_tau_sd__, 1e-6),
        child_noise_sd = pmax(adstock_child_noise_sd__, 1e-6)
      )
    ]
  }
  list(effectiveness = effect[], adstock = adstock[])
}

econ_seq_hierarchical_transfer_posterior_audit <- function(fit_obj, transfer_input) {
  empty <- function() list(
    effectiveness_parents = data.table::data.table(),
    effectiveness_children = data.table::data.table(),
    effectiveness_layer = data.table::data.table(),
    adstock_parents = data.table::data.table(),
    adstock_layer = data.table::data.table()
  )
  if (is.null(fit_obj$fit) || !is.function(fit_obj$fit$summary)) return(empty())
  summarize <- function(variable, labels, value_name) {
    if (!length(labels)) return(data.table::data.table())
    sm <- tryCatch(data.table::as.data.table(fit_obj$fit$summary(variables = variable)), error = function(e) data.table::data.table())
    if (!nrow(sm)) return(data.table::data.table())
    sm[, index__ := suppressWarnings(as.integer(sub("^.*\\[([0-9]+)\\]$", "\\1", variable)))]
    sm[get("variable") == variable, index__ := 1L]
    sm <- sm[is.finite(index__) & index__ <= length(labels)]
    sm[, label__ := labels[index__]]
    keep <- intersect(c("mean", "median", "sd", "q5", "q95", "rhat", "ess_bulk", "ess_tail"), names(sm))
    out <- sm[, c(list(label = label__), mget(keep))]
    data.table::setnames(out, keep, paste0(value_name, "_", keep))
    out[]
  }
  eff <- data.table::as.data.table(transfer_input$effectiveness %||% data.table::data.table())
  ad <- data.table::as.data.table(transfer_input$adstock %||% data.table::data.table())
  # Independent child-adstock priors intentionally have no latent parent/tau
  # table. Keep the posterior audit optional rather than letting that empty
  # schema discard an otherwise completed child fit.
  for (cc in c("parent_id", "variable", "reference_spend")) {
    if (!cc %in% names(eff)) eff[, (cc) := if (identical(cc, "reference_spend")) NA_real_ else NA_character_]
  }
  if (!"parent_id" %in% names(ad)) ad[, parent_id := character()]
  eff_parent <- unique(eff$parent_id)
  ad_parent <- unique(ad$parent_id)
  parent_effect <- summarize("seq_effect_parent", eff_parent, "parent_effectiveness")
  effect_layer <- summarize("seq_effect_tau", "layer", "tau_effectiveness")
  parent_aggregate <- summarize("seq_effect_aggregate", eff_parent, "aggregate_child_effectiveness")
  effect_parents <- Reduce(
    function(a, b) merge(a, b, by = "label", all = TRUE, sort = FALSE),
    Filter(nrow, list(parent_effect, parent_aggregate)),
    init = data.table::data.table(label = eff_parent)
  )
  if (nrow(effect_layer) && nrow(effect_parents)) {
    tau_cols <- setdiff(names(effect_layer), "label")
    for (cc in tau_cols) effect_parents[, (cc) := effect_layer[[cc]][1]]
  }
  child_effect <- summarize("seq_effectiveness", eff$variable, "child_effectiveness")
  if (nrow(child_effect)) {
    child_effect[eff[, .(label = variable, parent_id, reference_spend)], on = "label", `:=`(
      parent_id = i.parent_id,
      reference_spend = i.reference_spend
    )]
  }
  ad_parent_effect <- summarize("seq_adstock_parent_logit", ad_parent, "parent_adstock_logit")
  adstock_layer <- summarize("seq_adstock_tau_logit", "layer", "tau_adstock_logit")
  adstock_parents <- merge(data.table::data.table(label = ad_parent), ad_parent_effect, by = "label", all = TRUE, sort = FALSE)
  if (nrow(adstock_layer) && nrow(adstock_parents)) {
    tau_cols <- setdiff(names(adstock_layer), "label")
    for (cc in tau_cols) adstock_parents[, (cc) := adstock_layer[[cc]][1]]
  }
  list(
    effectiveness_parents = effect_parents[],
    effectiveness_children = child_effect[],
    effectiveness_layer = effect_layer[],
    adstock_parents = adstock_parents[],
    adstock_layer = adstock_layer[]
  )
}

econ_seq_merge_calibration_inputs <- function(existing, inherited) {
  if (is.null(existing) || !nrow(existing)) return(data.table::as.data.table(inherited))
  if (is.null(inherited) || !nrow(inherited)) return(data.table::as.data.table(existing))
  out <- data.table::rbindlist(list(data.table::as.data.table(existing), data.table::as.data.table(inherited)), fill = TRUE)
  if (anyDuplicated(out$calibration_id)) stop("Sequential and analyst calibration_input rows have duplicate calibration_id values.", call. = FALSE)
  out[]
}

econ_seq_enforce_branch_decisions <- function(data,
                                               metadata_input,
                                               spend_map,
                                               prior_table,
                                               parent_layer = NULL,
                                               curve_type_default = "hill") {
  dt <- econ_seq_input_table(data, "data")
  md <- clean_metadata(metadata_input, curve_type_default = curve_type_default)
  md[, `:=`(variable = as.character(variable), role = standardize_role(role))]
  sm <- econ_seq_input_table(spend_map, "spend_map")
  if (!all(c("variable", "spend_col") %in% names(sm))) {
    stop("Branch enforcement requires spend_map variable and spend_col.", call. = FALSE)
  }
  sm[, `:=`(variable = as.character(variable), spend_col = as.character(spend_col))]
  if (!"model_support_col" %in% names(sm)) sm[, model_support_col := variable]
  if (!"hierarchical_variation_eligible" %in% names(sm)) sm[, hierarchical_variation_eligible := TRUE]
  if (!"support_hierarchical_variation_eligible" %in% names(sm)) {
    sm[, support_hierarchical_variation_eligible := as.logical(hierarchical_variation_eligible)]
  }
  if (!"mechanically_allocated" %in% names(sm)) sm[, mechanically_allocated := FALSE]
  if (!"spend_mechanically_allocated" %in% names(sm)) sm[, spend_mechanically_allocated := mechanically_allocated]
  if (!"spend_hierarchical_variation_eligible" %in% names(sm)) {
    sm[, spend_hierarchical_variation_eligible := as.logical(hierarchical_variation_eligible)]
  }
  if (!"spend_scope" %in% names(sm)) sm[, spend_scope := "group_specific"]
  sm <- unique(sm, by = "variable")
  priors <- data.table::copy(data.table::as.data.table(prior_table))
  required_prior <- c("variable", "branch_decision", "sequential_parent_id", "child_spend_total")
  missing_prior <- setdiff(required_prior, names(priors))
  if (length(missing_prior)) {
    stop("Branch enforcement prior table is missing: ", paste(missing_prior, collapse = ", "), call. = FALSE)
  }
  priors[, variable := as.character(variable)]
  missing_decisions <- setdiff(sm$variable, priors$variable)
  if (length(missing_decisions)) {
    stop("No branch decision is available for modeled media: ", paste(missing_decisions, collapse = ", "), call. = FALSE)
  }
  # Earlier prototypes used stop/prune/require-prior as identification exits.
  # Identification weakness now changes regularization only, so normalize an
  # old in-memory prior table rather than silently terminating a branch.
  legacy_decisions <- priors$branch_decision %in% c("prune", "require_prior", "stop")
  if (any(legacy_decisions)) {
    old_reason <- if ("branch_decision_reason" %in% names(priors)) as.character(priors$branch_decision_reason) else rep("", nrow(priors))
    priors[legacy_decisions, `:=`(
      branch_decision = "strong_parent_shrinkage",
      branch_decision_reason = paste0(
        "legacy_identification_exit_normalized_to_strong_parent_shrinkage",
        ifelse(nzchar(old_reason[legacy_decisions]), paste0(": ", old_reason[legacy_decisions]), "")
      )
    )]
  }
  unknown_decisions <- setdiff(unique(priors$branch_decision), c("fit", "strong_parent_shrinkage"))
  if (length(unknown_decisions)) {
    stop("Unknown sequential branch decision(s): ", paste(unknown_decisions, collapse = ", "), call. = FALSE)
  }
  if (any(!sm$spend_col %in% names(dt))) {
    stop("Branch enforcement spend columns are missing from data.", call. = FALSE)
  }

  active_variables <- priors[branch_decision %in% c("fit", "strong_parent_shrinkage"), variable]
  generated_data <- list()
  generated_metadata <- list()
  generated_spend <- list()
  generated_priors <- list()
  audit_rows <- list()
  parent_ids <- unique(priors$sequential_parent_id)

  for (parent_id in parent_ids) {
    parent_rows <- priors[sequential_parent_id == parent_id]
    # A structural failure only retains the full parent when every child under
    # that parent is unusable. One invalid child becomes a spend-preserving
    # remainder while valid siblings continue independently.
    stop_parent <- !is.null(parent_layer) && all(parent_rows$branch_decision == "stop")
    unresolved <- if (stop_parent) parent_rows$variable else parent_rows[
      branch_decision %in% c("prune", "require_prior", "stop"), variable
    ]
    if (stop_parent) active_variables <- setdiff(active_variables, parent_rows$variable)
    if (!length(unresolved)) next

    all_parent_unresolved <- setequal(unresolved, parent_rows$variable)
    node_action <- if (stop_parent || all_parent_unresolved) "parent_retained" else "parent_remainder"
    source_vars <- if (stop_parent) parent_rows$variable else unresolved
    source_map <- sm[match(source_vars, variable)]
    if (anyNA(source_map$spend_col)) {
      stop("Could not preserve spend for unresolved branch under parent '", parent_id, "'.", call. = FALSE)
    }
    values <- as.matrix(dt[, source_map$spend_col, with = FALSE])
    storage.mode(values) <- "double"
    if (any(!is.finite(values))) {
      stop("Unresolved branch spend is incomplete under parent '", parent_id, "'.", call. = FALSE)
    }
    generated_name <- if (identical(node_action, "parent_retained") &&
                           !identical(parent_id, "total_paid_media") &&
                           !(parent_id %in% sm$variable)) {
      as.character(parent_id)
    } else {
      econ_seq_safe_generated_names(paste(node_action, parent_id, sep = " > "), c(names(dt), active_variables))[1]
    }
    generated_values <- rowSums(values)
    dt[, (generated_name) := generated_values]

    source_prior <- parent_rows[variable %in% source_vars]
    source_weight <- pmax(suppressWarnings(as.numeric(source_prior$child_spend_total)), 0)
    if (!any(is.finite(source_weight) & source_weight > 0)) source_weight <- rep(1, nrow(source_prior))
    template_prior <- data.table::copy(source_prior[1])
    weighted_mean <- function(col, default = NA_real_) {
      if (!(col %in% names(source_prior))) return(default)
      z <- suppressWarnings(as.numeric(source_prior[[col]]))
      ok <- is.finite(z) & is.finite(source_weight) & source_weight > 0
      if (!any(ok)) default else stats::weighted.mean(z[ok], source_weight[ok])
    }
    template_prior[, `:=`(
      variable = generated_name,
      branch_decision = node_action,
      branch_decision_reason = if (identical(node_action, "parent_retained"))
        "weak_branch_stopped_at_fitted_parent_grain" else
        "unresolved_children_collapsed_to_auditable_parent_remainder",
      child_spend_total = sum(generated_values),
      child_spend_share = NA_real_,
      sequential_spend_col = generated_name,
      prior_mean = weighted_mean("prior_mean", 0),
      prior_sd = max(suppressWarnings(as.numeric(source_prior$prior_sd)), na.rm = TRUE),
      user_prior_override_present = FALSE,
      user_prior_override_valid = FALSE,
      override_validation_reason = "generated_parent_preservation_node"
    )]
    if (!is.finite(template_prior$prior_sd[1]) || template_prior$prior_sd[1] <= 0) template_prior[, prior_sd := 1]
    template_prior[, implied_child_contribution_mean := prior_mean * child_spend_total]
    if ("rrate_prior_mean" %in% names(template_prior)) template_prior[, rrate_prior_mean := weighted_mean("rrate_prior_mean")]
    if ("anchor_saturation_prior_mean" %in% names(template_prior)) {
      template_prior[, anchor_saturation_prior_mean := weighted_mean("anchor_saturation_prior_mean")]
    }
    generated_priors[[length(generated_priors) + 1L]] <- template_prior

    parent_md <- data.table::data.table()
    if (!is.null(parent_layer) && !is.null(parent_layer$metadata)) {
      parent_md <- data.table::as.data.table(parent_layer$metadata)[variable == parent_id]
    }
    template_md <- if (nrow(parent_md)) data.table::copy(parent_md[1]) else data.table::copy(md[variable == source_vars[1]][1])
    if (!nrow(template_md)) stop("Could not construct metadata for retained parent branch.", call. = FALSE)
    template_md[, `:=`(
      variable = generated_name,
      role = "media",
      effect_type = "media",
      spend_col = generated_name,
      model_support_col = generated_name,
      cost_col = generated_name,
      rollup_path = paste0("total_paid_media > ", node_action, " > ", parent_id),
      sequential_branch_action = node_action,
      sequential_parent_id = parent_id,
      spend_scope = if (data.table::uniqueN(source_map$spend_scope) == 1L) source_map$spend_scope[1] else "generated_from_mixed_spend_scope",
      spend_mechanically_allocated = any(source_map$spend_mechanically_allocated %in% TRUE),
      spend_hierarchical_variation_eligible = all(source_map$spend_hierarchical_variation_eligible %in% TRUE),
      mechanically_allocated = any(source_map$spend_mechanically_allocated %in% TRUE),
      support_mechanically_allocated = any(source_map$spend_mechanically_allocated %in% TRUE),
      support_hierarchical_variation_eligible = all(source_map$spend_hierarchical_variation_eligible %in% TRUE),
      hierarchical_variation_eligible = all(source_map$spend_hierarchical_variation_eligible %in% TRUE)
    )]
    if ("coef_hierarchy_scope" %in% names(template_md) && !template_md$hierarchical_variation_eligible[1]) {
      template_md[, `:=`(coef_hierarchy_scope = "none", coef_hierarchy_scale = 0)]
    }
    generated_metadata[[length(generated_metadata) + 1L]] <- template_md
    generated_spend[[length(generated_spend) + 1L]] <- data.table::data.table(
      variable = generated_name,
      spend_col = generated_name,
      model_support_col = generated_name,
      spend_scope = if (data.table::uniqueN(source_map$spend_scope) == 1L) source_map$spend_scope[1] else "generated_from_mixed_spend_scope",
      spend_mechanically_allocated = any(source_map$spend_mechanically_allocated %in% TRUE),
      spend_hierarchical_variation_eligible = all(source_map$spend_hierarchical_variation_eligible %in% TRUE),
      mechanically_allocated = any(source_map$spend_mechanically_allocated %in% TRUE),
      support_mechanically_allocated = any(source_map$spend_mechanically_allocated %in% TRUE),
      support_hierarchical_variation_eligible = all(source_map$spend_hierarchical_variation_eligible %in% TRUE),
      hierarchical_variation_eligible = all(source_map$spend_hierarchical_variation_eligible %in% TRUE)
    )
    audit_rows[[length(audit_rows) + 1L]] <- source_prior[, .(
      variable,
      original_branch_decision = branch_decision,
      enforced_action = node_action,
      modeled_as = generated_name,
      sequential_parent_id,
      independent_child_retained = FALSE,
      spend_preserved = TRUE
    )]
  }

  active_priors <- priors[variable %in% active_variables]
  active_map <- sm[variable %in% active_variables]
  inactive_variables <- setdiff(sm$variable, active_variables)
  out_metadata <- md[!(variable %in% inactive_variables)]
  if (length(generated_metadata)) {
    out_metadata <- data.table::rbindlist(c(list(out_metadata), generated_metadata), use.names = TRUE, fill = TRUE)
  }
  out_spend <- active_map
  if (length(generated_spend)) out_spend <- data.table::rbindlist(c(list(out_spend), generated_spend), use.names = TRUE, fill = TRUE)
  out_priors <- active_priors
  if (length(generated_priors)) out_priors <- data.table::rbindlist(c(list(out_priors), generated_priors), use.names = TRUE, fill = TRUE)
  if (nrow(out_priors)) {
    total_spend <- sum(out_priors$child_spend_total, na.rm = TRUE)
    out_priors[, child_spend_share := if (total_spend > 0) child_spend_total / total_spend else NA_real_]
  }
  active_audit <- priors[variable %in% active_variables, .(
    variable,
    original_branch_decision = branch_decision,
    enforced_action = branch_decision,
    modeled_as = variable,
    sequential_parent_id,
    independent_child_retained = TRUE,
    spend_preserved = TRUE
  )]
  action_audit <- data.table::rbindlist(c(list(active_audit), audit_rows), use.names = TRUE, fill = TRUE)
  if (anyDuplicated(out_spend$variable) || anyDuplicated(out_metadata$variable) || anyDuplicated(out_priors$variable)) {
    stop("Branch enforcement produced duplicate modeled variables.", call. = FALSE)
  }
  original_total <- rowSums(as.matrix(dt[, sm$spend_col, with = FALSE]))
  modeled_total <- rowSums(as.matrix(dt[, out_spend$spend_col, with = FALSE]))
  reconciliation <- data.table::data.table(
    max_abs_row_spend_reconciliation_error = max(abs(original_total - modeled_total)),
    original_spend_total = sum(original_total),
    modeled_spend_total = sum(modeled_total),
    branch_actions_enforced = TRUE
  )
  list(
    data = dt[],
    metadata = out_metadata[],
    spend_map = out_spend[],
    prior_table = out_priors[],
    action_audit = action_audit[],
    reconciliation = reconciliation[]
  )
}

econ_seq_update_layer_mapping_after_enforcement <- function(layer, enforcement) {
  if (is.null(layer) || is.null(layer$variable_mapping) || !nrow(layer$variable_mapping)) return(layer)
  out <- layer
  mapping <- data.table::copy(data.table::as.data.table(out$variable_mapping))
  audit <- data.table::as.data.table(enforcement$action_audit)
  if (!"decomposition_eligible" %in% names(mapping)) mapping[, decomposition_eligible := TRUE]
  mapping[audit, on = .(generated_variable = variable), `:=`(
    generated_variable = i.modeled_as,
    decomposition_eligible = decomposition_eligible & i.independent_child_retained
  )]
  spend <- data.table::as.data.table(enforcement$spend_map)
  node_map <- mapping[, .(
    rollup_depth = suppressWarnings(as.integer(rollup_depth[1])),
    rollup_node = paste(sort(unique(rollup_node)), collapse = " | "),
    rollup_node_path = paste(sort(unique(rollup_node_path)), collapse = " | "),
    source_variables = paste(sort(unique(variable)), collapse = " | "),
    source_variable_n = data.table::uniqueN(variable),
    decomposition_eligible = all(decomposition_eligible %in% TRUE)
  ), by = generated_variable]
  node_map[spend, on = .(generated_variable = variable), `:=`(
    spend_col = i.spend_col,
    model_support_col = i.model_support_col,
    mechanically_allocated = i.mechanically_allocated,
    hierarchical_variation_eligible = i.hierarchical_variation_eligible,
    support_hierarchical_variation_eligible = i.support_hierarchical_variation_eligible
  )]
  out$variable_mapping <- mapping[]
  out$node_map <- node_map[]
  out$branch_action_audit <- enforcement$action_audit
  out$branch_action_reconciliation <- enforcement$reconciliation
  out
}

econ_seq_carry_stopped_parent_nodes <- function(parent_layer, child_layer) {
  parent_mapping <- data.table::copy(data.table::as.data.table(parent_layer$variable_mapping))
  if (!"decomposition_eligible" %in% names(parent_mapping)) return(child_layer)
  blocked_mapping <- parent_mapping[decomposition_eligible %in% FALSE]
  if (!nrow(blocked_mapping)) return(child_layer)
  child_mapping <- data.table::copy(data.table::as.data.table(child_layer$variable_mapping))
  blocked_variables <- unique(blocked_mapping$variable)
  blocked_child_nodes <- unique(child_mapping[variable %in% blocked_variables, generated_variable])
  mixed_nodes <- child_mapping[generated_variable %in% blocked_child_nodes,
                               .(blocked_n = sum(variable %in% blocked_variables), total_n = .N),
                               by = generated_variable][blocked_n != total_n]
  if (nrow(mixed_nodes)) {
    stop("A deeper rollup node mixes stopped and active parent branches. Choose a depth that preserves the declared parent partition.", call. = FALSE)
  }
  if (nrow(parent_layer$data) != nrow(child_layer$data)) {
    stop("Stopped parent branches cannot be carried because parent and child rows do not align.", call. = FALSE)
  }
  out <- child_layer
  out$data <- data.table::copy(data.table::as.data.table(out$data))
  out$metadata <- data.table::copy(data.table::as.data.table(out$metadata))
  out$spend_map <- data.table::copy(data.table::as.data.table(out$spend_map))
  out$metadata <- out$metadata[!(variable %in% blocked_child_nodes)]
  out$spend_map <- out$spend_map[!(variable %in% blocked_child_nodes)]
  child_mapping <- child_mapping[!(variable %in% blocked_variables)]
  carried_ids <- unique(blocked_mapping$generated_variable)
  carry_audit <- lapply(carried_ids, function(parent_id) {
    parent_spend <- data.table::as.data.table(parent_layer$spend_map)[variable == parent_id]
    parent_md <- data.table::as.data.table(parent_layer$metadata)[variable == parent_id]
    if (!nrow(parent_spend) || !nrow(parent_md)) {
      stop("Stopped parent node '", parent_id, "' is missing its fitted metadata/spend mapping.", call. = FALSE)
    }
    source_col <- parent_spend$spend_col[1]
    if (!(source_col %in% names(parent_layer$data))) {
      stop("Stopped parent node '", parent_id, "' is missing its fitted spend series.", call. = FALSE)
    }
    out$data[, (parent_id) := suppressWarnings(as.numeric(parent_layer$data[[source_col]]))]
    out$metadata <- data.table::rbindlist(
      list(out$metadata[variable != parent_id], data.table::copy(parent_md[1])[, variable := parent_id]),
      use.names = TRUE,
      fill = TRUE
    )
    carry_spend <- data.table::copy(parent_spend[1])
    carry_spend[, `:=`(variable = parent_id, spend_col = parent_id)]
    out$spend_map <- data.table::rbindlist(
      list(out$spend_map[variable != parent_id], carry_spend),
      use.names = TRUE,
      fill = TRUE
    )
    originals <- blocked_mapping[generated_variable == parent_id]
    carry_mapping <- originals[, .(
      variable,
      rollup_depth,
      rollup_node,
      rollup_node_path,
      source_path_depth,
      layer_id = as.character(out$layer_key %||% "mixed_depth"),
      layer_label = "carried_stopped_parent",
      generated_variable = parent_id,
      decomposition_eligible = FALSE,
      carried_parent_node = TRUE
    )]
    child_mapping <<- data.table::rbindlist(list(child_mapping, carry_mapping), use.names = TRUE, fill = TRUE)
    data.table::data.table(
      sequential_parent_id = parent_id,
      carried_to_child_layer = TRUE,
      decomposition_eligible = FALSE,
      source_variable_n = nrow(originals)
    )
  })
  if (!"carried_parent_node" %in% names(child_mapping)) child_mapping[, carried_parent_node := FALSE]
  child_mapping[is.na(carried_parent_node), carried_parent_node := FALSE]
  out$variable_mapping <- child_mapping[]
  out$carried_parent_nodes <- data.table::rbindlist(carry_audit, fill = TRUE)
  out
}

econ_seq_base_prior_specification <- function(metadata_input,
                                              variables = NULL,
                                              baseline_spec = NULL) {
  raw <- econ_seq_input_table(metadata_input, "metadata_input")
  # Validate only the modeled child rows. A raw input table may legitimately
  # carry blank media-only curve fields on controls before model preparation.
  if (!is.null(variables)) raw <- raw[variable %in% as.character(variables)]
  md <- clean_metadata(raw)
  md[, `:=`(variable = as.character(variable), role = standardize_role(role))]
  keep <- intersect(c(
    "variable", "role", "curve_type", "rrate", "rrate_precision",
    "anchor_saturation", "anchor_saturation_precision", "cvalue", "cvalue_precision",
    "dvalue", "dvalue_precision", "coef", "coef_precision", "coef_bound",
    "coef_lower", "coef_upper", "cvalue_from_anchor"
  ), names(md))
  out <- md[, ..keep]
  data.table::setorderv(out, "variable")
  attr(out, "baseline_spec") <- baseline_spec
  out[]
}

econ_seq_assert_base_prior_equivalence <- function(reference_spec,
                                                    candidate_spec,
                                                    context = "sequential child handoff",
                                                    reference_context = list(),
                                                    candidate_context = list()) {
  ref <- data.table::copy(data.table::as.data.table(reference_spec))
  cand <- data.table::copy(data.table::as.data.table(candidate_spec))
  if (!"variable" %in% names(ref) || !"variable" %in% names(cand)) {
    stop("Base-prior equivalence audit requires a variable column.", call. = FALSE)
  }
  columns <- sort(unique(c(names(ref), names(cand))))
  columns <- setdiff(columns, "variable")
  for (cc in setdiff(columns, names(ref))) ref[, (cc) := NA]
  for (cc in setdiff(columns, names(cand))) cand[, (cc) := NA]
  data.table::setcolorder(ref, c("variable", columns))
  data.table::setcolorder(cand, c("variable", columns))
  audit <- merge(ref, cand, by = "variable", all = TRUE, suffixes = c("_reference", "_candidate"), sort = TRUE)
  mismatch <- rep(FALSE, nrow(audit))
  mismatch_fields <- rep("", nrow(audit))
  for (cc in columns) {
    left <- audit[[paste0(cc, "_reference")]]
    right <- audit[[paste0(cc, "_candidate")]]
    same <- if (is.numeric(left) || is.integer(left)) {
      (is.na(left) & is.na(right)) | (!is.na(left) & !is.na(right) & abs(as.numeric(left) - as.numeric(right)) <= 1e-12)
    } else {
      (is.na(left) & is.na(right)) | (!is.na(left) & !is.na(right) & as.character(left) == as.character(right))
    }
    bad <- !same
    mismatch <- mismatch | bad
    mismatch_fields[bad] <- paste0(mismatch_fields[bad], ifelse(nzchar(mismatch_fields[bad]), " | ", ""), cc)
  }
  audit[, `:=`(base_prior_equivalent = !mismatch, mismatch_fields = mismatch_fields, audit_context = context)]
  context_fields <- c("baseline_spec", "controls", "holdout_contract", "fit_args")
  for (field in context_fields) {
    ref_hash <- econ_seq_content_hash(reference_context[[field]] %||% NULL)
    cand_hash <- econ_seq_content_hash(candidate_context[[field]] %||% NULL)
    audit[, (paste0(field, "_equivalent")) := identical(ref_hash, cand_hash)]
    if (!identical(ref_hash, cand_hash)) {
      audit[, `:=`(
        base_prior_equivalent = FALSE,
        mismatch_fields = paste0(mismatch_fields, ifelse(nzchar(mismatch_fields), " | ", ""), field)
      )]
    }
  }
  if (any(!audit$base_prior_equivalent)) {
    bad <- audit[base_prior_equivalent == FALSE, paste0(variable, " [", mismatch_fields, "]")]
    stop("Base-prior equivalence audit failed for ", context, ": ", paste(bad, collapse = "; "), call. = FALSE)
  }
  audit[]
}

econ_seq_combine_normal_priors <- function(base_mean,
                                           base_precision,
                                           evidence_mean,
                                           evidence_precision) {
  base_mean <- suppressWarnings(as.numeric(base_mean))
  base_precision <- suppressWarnings(as.numeric(base_precision))
  evidence_mean <- suppressWarnings(as.numeric(evidence_mean))
  evidence_precision <- suppressWarnings(as.numeric(evidence_precision))
  use_base <- is.finite(base_mean) & is.finite(base_precision) & base_precision > 0
  use_evidence <- is.finite(evidence_mean) & is.finite(evidence_precision) & evidence_precision > 0
  combined_precision <- ifelse(use_base, base_precision, 0) + ifelse(use_evidence, evidence_precision, 0)
  combined_mean <- rep(NA_real_, length(combined_precision))
  both <- use_base & use_evidence
  combined_mean[both] <- (base_mean[both] * base_precision[both] + evidence_mean[both] * evidence_precision[both]) / combined_precision[both]
  combined_mean[use_base & !use_evidence] <- base_mean[use_base & !use_evidence]
  combined_mean[!use_base & use_evidence] <- evidence_mean[!use_base & use_evidence]
  data.table::data.table(
    prior_mean = combined_mean,
    prior_precision = combined_precision,
    combination_mode = data.table::fcase(
      both, "generic_plus_parent_evidence",
      use_evidence, "parent_evidence_no_generic_prior",
      use_base, "generic_metadata_preserved",
      default = "no_usable_prior"
    )
  )
}

econ_seq_apply_rrate_priors <- function(metadata_input,
                                        prior_table,
                                        curve_transfer_mode = c("effectiveness_adstock_saturation", "effectiveness_adstock", "effectiveness_only"),
                                        saturation_handoff = c("generic_child_prior", "collective_parent_shape_reconciliation", "independent_parent_prior"),
                                        rrate_prior_sd_multiplier = 1,
                                        saturation_prior_precision_multiplier = 1) {
  curve_transfer_mode <- match.arg(curve_transfer_mode)
  saturation_handoff <- match.arg(saturation_handoff)
  md <- econ_seq_input_table(metadata_input, "metadata_input")
  if (identical(curve_transfer_mode, "effectiveness_only")) return(md[])
  priors <- data.table::copy(data.table::as.data.table(prior_table))
  required <- c("variable", "rrate_prior_mean", "rrate_prior_precision")
  if (!all(required %in% names(priors))) return(md[])
  if (!"curve_prior_available" %in% names(priors)) {
    priors[, curve_prior_available := is.finite(rrate_prior_mean) & is.finite(rrate_prior_precision) & rrate_prior_precision > 0]
  }
  if (!"rrate_prior_source" %in% names(priors)) priors[, rrate_prior_source := NA_character_]
  if (!"anchor_saturation_prior_source" %in% names(priors)) priors[, anchor_saturation_prior_source := NA_character_]
  if (!"branch_decision" %in% names(priors)) priors[, branch_decision := "fit"]
  priors <- priors[
    curve_prior_available %in% TRUE &
      branch_decision %in% c("fit", "strong_parent_shrinkage", "parent_retained", "parent_remainder") &
      is.finite(rrate_prior_mean) & is.finite(rrate_prior_precision) & rrate_prior_precision > 0
  ]
  if (!nrow(priors)) return(md[])
  rrate_prior_sd_multiplier <- max(as.numeric(rrate_prior_sd_multiplier)[1], 1e-6)
  saturation_prior_precision_multiplier <- max(as.numeric(saturation_prior_precision_multiplier)[1], 1e-6)
  priors[, rrate_prior_precision__ := rrate_prior_precision / rrate_prior_sd_multiplier^2]
  if (!"rrate" %in% names(md)) md[, rrate := NA_real_]
  if (!"rrate_precision" %in% names(md)) md[, rrate_precision := NA_real_]
  rrate_base <- md[priors, on = "variable", .(
    variable = i.variable,
    base_mean = x.rrate,
    base_precision = x.rrate_precision,
    parent_mean = i.rrate_prior_mean,
    parent_precision = i.rrate_prior_precision__,
    parent_source = i.rrate_prior_source
  )]
  rrate_combined <- econ_seq_combine_normal_priors(
    rrate_base$base_mean, rrate_base$base_precision,
    rrate_base$parent_mean, rrate_base$parent_precision
  )
  rrate_base[, `:=`(
    combined_mean = rrate_combined$prior_mean,
    combined_precision = rrate_combined$prior_precision,
    combination_mode = rrate_combined$combination_mode
  )]
  md[rrate_base, on = "variable", `:=`(
    rrate = i.combined_mean,
    rrate_precision = i.combined_precision,
    sequential_rrate_prior_source = i.combination_mode
  )]
  saturation_required <- c("anchor_saturation_prior_mean", "anchor_saturation_prior_precision")
  if (identical(curve_transfer_mode, "effectiveness_adstock_saturation") &&
      identical(saturation_handoff, "independent_parent_prior") && all(saturation_required %in% names(priors))) {
    saturation_priors <- priors[
      is.finite(anchor_saturation_prior_mean) &
        is.finite(anchor_saturation_prior_precision) & anchor_saturation_prior_precision > 0
    ]
    if (!nrow(saturation_priors)) return(md[])
    if (!"anchor_saturation" %in% names(md)) md[, anchor_saturation := NA_real_]
    if (!"anchor_saturation_precision" %in% names(md)) md[, anchor_saturation_precision := NA_real_]
    if (!"cvalue_from_anchor" %in% names(md)) md[, cvalue_from_anchor := FALSE]
    saturation_priors[, anchor_saturation_prior_precision__ :=
                        anchor_saturation_prior_precision * saturation_prior_precision_multiplier]
    saturation_base <- md[saturation_priors, on = "variable", .(
      variable = i.variable,
      base_mean = x.anchor_saturation,
      base_precision = x.anchor_saturation_precision,
      parent_mean = i.anchor_saturation_prior_mean,
      parent_precision = i.anchor_saturation_prior_precision__,
      parent_source = i.anchor_saturation_prior_source
    )]
    saturation_combined <- econ_seq_combine_normal_priors(
      saturation_base$base_mean, saturation_base$base_precision,
      saturation_base$parent_mean, saturation_base$parent_precision
    )
    saturation_base[, `:=`(
      combined_mean = saturation_combined$prior_mean,
      combined_precision = saturation_combined$prior_precision,
      combination_mode = saturation_combined$combination_mode
    )]
    md[saturation_base, on = "variable", `:=`(
      anchor_saturation = i.combined_mean,
      anchor_saturation_precision = i.combined_precision,
      cvalue_from_anchor = TRUE,
      sequential_saturation_prior_source = i.combination_mode
    )]
  }
  if (identical(curve_transfer_mode, "effectiveness_adstock_saturation") && !identical(saturation_handoff, "independent_parent_prior")) {
    md[, sequential_saturation_prior_source := if (identical(saturation_handoff, "collective_parent_shape_reconciliation")) {
      "generic_child_saturation_plus_collective_parent_shape_reconciliation"
    } else {
      "generic_child_saturation_no_parent_anchor_transfer"
    }]
  }
  md[]
}

econ_seq_scale_handoff_effectiveness <- function(handoff, multiplier = 1) {
  multiplier <- max(as.numeric(multiplier)[1], 1e-6)
  for (nm in c("business_priors", "prior_ledger")) {
    if (is.null(handoff[[nm]]) || !nrow(handoff[[nm]])) next
    handoff[[nm]][, prior_sd := prior_sd * multiplier]
    if ("prior_precision" %in% names(handoff[[nm]])) handoff[[nm]][, prior_precision := 1 / prior_sd^2]
    handoff[[nm]][, effectiveness_prior_sd_multiplier := multiplier]
  }
  handoff
}

econ_seq_layer_identification_diagnostics <- function(data,
                                                       spend_map,
                                                       group_col,
                                                       time_col,
                                                       dep_var_col = NULL,
                                                       control_cols = character(),
                                                       baseline_trend_spec = c("none", "linear"),
                                                       baseline_fourier_harmonics = 0L,
                                                       season_period = 52L,
                                                       layer_label = "child_layer",
                                                       identification_calibration = econ_seq_identification_calibration()) {
  dt <- econ_seq_input_table(data, "data")
  sm <- econ_seq_input_table(spend_map, "spend_map")
  baseline_trend_spec <- match.arg(baseline_trend_spec)
  lower_threshold <- identification_calibration$predominantly_prior_driven_max
  upper_threshold <- identification_calibration$data_driven_min
  variation_scale <- identification_calibration$active_support_cv_scale
  active_period_scale <- identification_calibration$active_period_scale
  minimum_active_rows <- identification_calibration$minimum_active_rows
  if (!all(c("variable", "spend_col") %in% names(sm))) {
    stop("spend_map must include variable and spend_col for sequential identification diagnostics.", call. = FALSE)
  }
  if (!"model_support_col" %in% names(sm)) {
    sm[, model_support_col := data.table::fifelse(variable %in% names(dt), as.character(variable), as.character(spend_col))]
  }
  required <- c(group_col, time_col, sm$spend_col, sm$model_support_col, control_cols)
  if (!is.null(dep_var_col)) required <- c(required, dep_var_col)
  missing <- setdiff(required, names(dt))
  if (length(missing)) stop("Identification diagnostics missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  if (!"support_hierarchical_variation_eligible" %in% names(sm)) {
    if ("hierarchical_variation_eligible" %in% names(sm)) {
      sm[, support_hierarchical_variation_eligible := as.logical(hierarchical_variation_eligible)]
    } else sm[, support_hierarchical_variation_eligible := TRUE]
  }
  sm <- unique(sm[, .(
    variable = as.character(variable),
    spend_col = as.character(spend_col),
    model_support_col = as.character(model_support_col),
    support_hierarchical_variation_eligible = as.logical(support_hierarchical_variation_eligible)
  )], by = "variable")
  support_matrix <- as.matrix(dt[, sm$model_support_col, with = FALSE])
  storage.mode(support_matrix) <- "double"
  support_matrix[!is.finite(support_matrix)] <- NA_real_
  spend_matrix <- as.matrix(dt[, sm$spend_col, with = FALSE])
  storage.mode(spend_matrix) <- "double"
  spend_matrix[!is.finite(spend_matrix)] <- NA_real_
  groups <- as.character(dt[[group_col]])
  time_index <- as.numeric(factor(dt[[time_col]], levels = sort(unique(dt[[time_col]]))))
  base_x <- if (data.table::uniqueN(groups) > 1L) {
    stats::model.matrix(~ 0 + groups)
  } else {
    cbind(`(Intercept)` = rep(1, nrow(dt)))
  }
  if (identical(baseline_trend_spec, "linear")) base_x <- cbind(base_x, time_index = time_index)
  baseline_fourier_harmonics <- max(0L, as.integer(baseline_fourier_harmonics)[1])
  season_period <- suppressWarnings(as.numeric(season_period)[1])
  if (baseline_fourier_harmonics > 0L && is.finite(season_period) && season_period > 1) {
    for (kk in seq_len(baseline_fourier_harmonics)) {
      base_x <- cbind(
        base_x,
        sin(2 * pi * kk * time_index / season_period),
        cos(2 * pi * kk * time_index / season_period)
      )
    }
  }
  for (cc in unique(as.character(control_cols))) {
    z <- suppressWarnings(as.numeric(dt[[cc]]))
    z <- z - mean(z, na.rm = TRUE)
    z_sd <- stats::sd(z, na.rm = TRUE)
    if (is.finite(z_sd) && z_sd > 1e-10) base_x <- cbind(base_x, z / z_sd)
  }
  complete_support <- stats::complete.cases(support_matrix)
  condition_index <- NA_real_
  if (sum(complete_support) >= max(4L, ncol(support_matrix) + 1L)) {
    z <- scale(support_matrix[complete_support, , drop = FALSE])
    z <- z[, apply(z, 2, function(v) all(is.finite(v)) && stats::sd(v) > 1e-10), drop = FALSE]
    if (ncol(z) >= 2L) {
      singular <- svd(z, nu = 0, nv = 0)$d
      positive <- singular[is.finite(singular) & singular > max(singular, na.rm = TRUE) * 1e-10]
      if (length(positive) >= 2L) condition_index <- max(positive) / min(positive)
    }
  }
  outcome <- if (!is.null(dep_var_col)) suppressWarnings(as.numeric(dt[[dep_var_col]])) else rep(NA_real_, nrow(dt))
  rows <- lapply(seq_len(nrow(sm)), function(ii) {
    x <- support_matrix[, ii]
    spend <- spend_matrix[, ii]
    finite <- is.finite(x)
    active <- finite & x > 0
    active_x <- x[active]
    active_spend <- spend[is.finite(spend) & spend > 0]
    raw_sd <- stats::sd(x[finite])
    active_cv <- if (length(active_x) >= 2L && mean(active_x) > 1e-8) stats::sd(active_x) / mean(active_x) else 0
    active_spend_cv <- if (length(active_spend) >= 2L && mean(active_spend) > 1e-8) stats::sd(active_spend) / mean(active_spend) else 0
    residual_ratio <- NA_real_
    if (sum(finite) > ncol(base_x) + 2L && is.finite(raw_sd) && raw_sd > 1e-10) {
      bx <- base_x[finite, , drop = FALSE]
      beta <- tryCatch(qr.solve(bx, x[finite]), error = function(e) NULL)
      if (!is.null(beta)) residual_ratio <- stats::sd(x[finite] - as.numeric(bx %*% beta)) / raw_sd
    }
    other <- setdiff(seq_len(ncol(support_matrix)), ii)
    max_abs_corr <- NA_real_
    if (length(other)) {
      cors <- vapply(other, function(jj) {
        ok <- finite & is.finite(support_matrix[, jj])
        if (sum(ok) < 4L || stats::sd(x[ok]) <= 1e-10 || stats::sd(support_matrix[ok, jj]) <= 1e-10) return(NA_real_)
        suppressWarnings(abs(stats::cor(x[ok], support_matrix[ok, jj])))
      }, numeric(1))
      if (any(is.finite(cors))) max_abs_corr <- max(cors, na.rm = TRUE)
    }
    independent_ratio <- residual_ratio
    if (length(other) && sum(finite & apply(is.finite(support_matrix[, other, drop = FALSE]), 1L, all)) > ncol(base_x) + length(other) + 2L &&
        is.finite(raw_sd) && raw_sd > 1e-10) {
      ok <- finite & apply(is.finite(support_matrix[, other, drop = FALSE]), 1L, all)
      full_x <- cbind(base_x[ok, , drop = FALSE], support_matrix[ok, other, drop = FALSE])
      beta <- tryCatch(qr.solve(full_x, x[ok]), error = function(e) NULL)
      if (!is.null(beta)) independent_ratio <- stats::sd(x[ok] - as.numeric(full_x %*% beta)) / raw_sd
    }
    coefficient_stability <- NA_real_
    if (!is.null(dep_var_col)) {
      period_levels <- sort(unique(time_index[finite & is.finite(outcome)]))
      if (length(period_levels) >= 8L) {
        midpoint <- stats::median(period_levels)
        split_coef <- vapply(c(FALSE, TRUE), function(second_half) {
          ok <- finite & is.finite(outcome) & ((time_index > midpoint) == second_half)
          if (sum(ok) <= ncol(base_x) + 3L || stats::sd(x[ok]) <= 1e-10) return(NA_real_)
          design <- cbind(base_x[ok, , drop = FALSE], media__ = x[ok])
          beta <- tryCatch(qr.solve(design, outcome[ok]), error = function(e) NULL)
          if (is.null(beta)) NA_real_ else as.numeric(tail(beta, 1L))
        }, numeric(1))
        if (all(is.finite(split_coef))) {
          coefficient_stability <- exp(-abs(diff(split_coef)) / (mean(abs(split_coef)) + 1e-8))
        }
      }
    }
    group_active_n <- tapply(active, groups, sum)
    active_period_n <- data.table::uniqueN(dt[[time_col]][active])
    support_score <- pmin(active_period_n / 26, 1) * sqrt(sum(active) / pmax(sum(finite), 1))
    variation_score <- pmin(pmax(log1p(active_cv) / log(2), 0), 1)
    residual_score <- if (is.finite(residual_ratio)) pmin(pmax(residual_ratio / 0.50, 0), 1) else 0.50
    independent_score <- if (is.finite(independent_ratio)) pmin(pmax(independent_ratio / 0.50, 0), 1) else 0.50
    collinearity_score <- if (is.finite(max_abs_corr)) pmin(pmax(1 - max_abs_corr^2, 0), 1) else 0.50
    stability_score <- if (is.finite(coefficient_stability)) pmin(pmax(coefficient_stability, 0), 1) else 0.50
    raw_strength <- 0.25 * support_score + 0.15 * variation_score + 0.20 * residual_score +
      0.20 * independent_score + 0.10 * collinearity_score + 0.10 * stability_score
    # Smooth gates prevent abundant but nearly constant support, or a handful
    # of isolated active periods, from looking identified merely because its
    # residualized series is numerically independent of sibling media.
    variation_gate <- 1 - exp(-pmax(active_cv, active_spend_cv, na.rm = TRUE) / variation_scale)
    active_period_gate <- 1 - exp(-active_period_n / active_period_scale)
    strength <- raw_strength * sqrt(pmax(variation_gate * active_period_gate, 0))
    safety_floor_failed <- sum(active) < minimum_active_rows || !is.finite(raw_sd) || raw_sd <= 1e-10
    if (safety_floor_failed || !is.finite(strength)) strength <- 0
    strength <- pmin(pmax(strength, 0), 1)
    status <- if (strength < lower_threshold) {
      "strong_parent_shrinkage"
    } else if (strength < upper_threshold) {
      "moderate_parent_shrinkage"
    } else {
      "fit"
    }
    shrinkage_multiplier <- 0.70 + 3.30 * (1 - strength)^2
    if (safety_floor_failed) shrinkage_multiplier <- 4
    data.table::data.table(
      layer_label = layer_label,
      variable = sm$variable[ii],
      spend_col = sm$spend_col[ii],
      model_support_col = sm$model_support_col[ii],
      row_n = sum(finite),
      active_row_n = sum(active),
      active_group_n = sum(group_active_n > 0),
      active_period_n = active_period_n,
      sparsity_rate = 1 - sum(active) / pmax(sum(finite), 1),
      active_support_cv = active_cv,
      active_spend_cv = active_spend_cv,
      residualized_variation_ratio = residual_ratio,
      sibling_independent_variation_ratio = independent_ratio,
      max_abs_media_correlation = max_abs_corr,
      media_condition_index = condition_index,
      coefficient_stability_score = coefficient_stability,
      spend_total = sum(spend[is.finite(spend)], na.rm = TRUE),
      spend_nonmissing_rate = mean(is.finite(spend)),
      zero_spend_nonzero_support_n = sum(is.finite(spend) & spend <= 0 & active),
      nonzero_spend_zero_support_n = sum(is.finite(spend) & spend > 0 & finite & x <= 0),
      support_hierarchical_variation_eligible = isTRUE(sm$support_hierarchical_variation_eligible[ii]),
      hierarchical_variation_eligible = isTRUE(sm$support_hierarchical_variation_eligible[ii]),
      identification_strength_0_1 = strength,
      identification_raw_strength_0_1 = raw_strength,
      identification_variation_gate_0_1 = variation_gate,
      identification_active_period_gate_0_1 = active_period_gate,
      identification_recommendation = status,
      identification_evidence_band = data.table::fcase(
        strength >= upper_threshold, "data_driven",
        strength >= lower_threshold, "blended_parent_and_data",
        default = "predominantly_prior_driven"
      ),
      parent_shrinkage_multiplier = shrinkage_multiplier,
      prior_width_multiplier = 1,
      safety_floor_failed = safety_floor_failed,
      thresholds_calibrated = TRUE,
      identification_calibration_version = identification_calibration$version,
      identification_prior_driven_max = lower_threshold,
      identification_data_driven_min = upper_threshold,
      diagnostic_note = paste(
        "Continuous observational identification screen calibrated against included synthetic regimes.",
        "Weak child evidence increases shrinkage but never stops a valid Bayesian branch.",
        "Mechanical geo allocation is reported separately and does not establish geo heterogeneity."
      )
    )
  })
  by_variable <- data.table::rbindlist(rows, fill = TRUE)
  unique_status <- unique(by_variable$identification_recommendation)
  overall_status <- if (length(unique_status) == 1L) unique_status else "mixed_branch_decisions"
  list(
    by_variable = by_variable[],
    overall = data.table::data.table(
      layer_label = layer_label,
      media_variable_n = nrow(by_variable),
      identification_recommendation = overall_status,
      mean_identification_strength_0_1 = mean(by_variable$identification_strength_0_1, na.rm = TRUE),
      max_parent_shrinkage_multiplier = max(by_variable$parent_shrinkage_multiplier, na.rm = TRUE),
      prior_width_multiplier = 1,
      thresholds_calibrated = TRUE,
      identification_calibration_version = identification_calibration$version,
      identification_prior_driven_max = lower_threshold,
      identification_data_driven_min = upper_threshold,
      note = paste(
        "Branch-level actions are enforced independently. Identification weakness changes inherited shrinkage;",
        "parent uncertainty is handled separately through the parent distribution. Thresholds classify the",
        "continuous score and are calibrated to the package synthetic contracts, not universal cutoffs."
      )
    )
  )
}

econ_seq_baseline_contract <- function(root_trend_spec = "none",
                                       root_fourier_harmonics = 0L,
                                       root_season_period = 52L,
                                       control_cols = character(),
                                       kpi_aggregation_rule = "sum",
                                       control_aggregation = NULL,
                                       baseline_spec = NULL) {
  defaults <- list(
    type = if (as.integer(root_fourier_harmonics)[1] > 0L) "fourier" else "flat",
    trend_spec = as.character(root_trend_spec)[1],
    fourier_harmonics = max(0L, as.integer(root_fourier_harmonics)[1]),
    seasonal_period = as.numeric(root_season_period)[1],
    ucm_components = list(level = FALSE, season = as.integer(root_fourier_harmonics)[1] > 0L, cycle = FALSE),
    controls = unique(as.character(control_cols)),
    control_transformations = "center_and_scale_continuous_controls",
    kpi_aggregation_rule = as.character(kpi_aggregation_rule)[1],
    control_aggregation = control_aggregation,
    deliberate_override = FALSE
  )
  contract <- utils::modifyList(defaults, baseline_spec %||% list())
  contract$type <- tolower(trimws(as.character(contract$type)[1]))
  contract$trend_spec <- match.arg(as.character(contract$trend_spec)[1], c("none", "linear"))
  if (!(contract$type %in% c("flat", "fourier"))) {
    stop("Sequential shared baseline currently supports type = 'flat' or 'fourier'. A UCM root approximation would not be the same baseline contract.", call. = FALSE)
  }
  contract$fourier_harmonics <- max(0L, as.integer(contract$fourier_harmonics)[1])
  contract$seasonal_period <- as.numeric(contract$seasonal_period)[1]
  if (identical(contract$type, "flat")) contract$fourier_harmonics <- 0L
  if (identical(contract$type, "fourier") && contract$fourier_harmonics < 1L) {
    stop("A fourier sequential baseline requires at least one harmonic.", call. = FALSE)
  }
  if (identical(contract$trend_spec, "linear")) {
    stop("A shared linear-trend child control is not yet implemented. Use trend_spec = 'none' so root and Bayesian child baselines remain identical.", call. = FALSE)
  }
  contract$intercept_type <- if (identical(contract$type, "fourier")) "fourier" else "flat"
  contract$ucm_spec <- list(
    level = FALSE,
    season = identical(contract$type, "fourier"),
    cycle = FALSE,
    season_period = if (identical(contract$type, "fourier")) as.integer(contract$seasonal_period) else 52L,
    season_harmonics = if (identical(contract$type, "fourier")) contract$fourier_harmonics else 2L,
    cycle_period = 104L,
    cycle_harmonics = 1L
  )
  contract
}

econ_seq_apply_baseline_contract <- function(fit_args,
                                             baseline_contract,
                                             allow_baseline_override = FALSE) {
  if (!is.list(fit_args)) stop("child_fit_args must be a list.", call. = FALSE)
  expected_type <- baseline_contract$intercept_type
  expected_ucm <- baseline_contract$ucm_spec
  type_conflict <- !is.null(fit_args$intercept_type) &&
    !identical(tolower(as.character(fit_args$intercept_type)[1]), expected_type)
  supplied_ucm <- if (is.null(fit_args$ucm_spec)) expected_ucm else utils::modifyList(expected_ucm, fit_args$ucm_spec)
  ucm_conflict <- !identical(supplied_ucm, expected_ucm)
  if ((type_conflict || ucm_conflict) && !isTRUE(allow_baseline_override)) {
    stop("child_fit_args baseline conflicts with the shared sequential baseline contract. Set allow_baseline_override = TRUE only for a deliberate audited difference.", call. = FALSE)
  }
  if (isTRUE(allow_baseline_override) && (type_conflict || ucm_conflict)) {
    baseline_contract$deliberate_override <- TRUE
    baseline_contract$override_intercept_type <- fit_args$intercept_type %||% expected_type
    baseline_contract$override_ucm_spec <- fit_args$ucm_spec %||% expected_ucm
    return(list(fit_args = fit_args, baseline_contract = baseline_contract))
  }
  fit_args$intercept_type <- expected_type
  fit_args$ucm_spec <- expected_ucm
  list(fit_args = fit_args, baseline_contract = baseline_contract)
}

econ_seq_training_time_values <- function(data,
                                          group_col,
                                          time_col,
                                          holdout_col = NULL,
                                          holdout_value = TRUE,
                                          holdout_last_n = 0L) {
  dt <- data.table::copy(data.table::as.data.table(data))
  required <- c(group_col, time_col)
  missing <- setdiff(required, names(dt))
  if (length(missing)) stop("Training-scope data are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!is.null(holdout_col) && !(holdout_col %in% names(dt))) {
    stop("holdout_col not found in sequential data: ", holdout_col, call. = FALSE)
  }
  holdout_last_n <- suppressWarnings(as.integer(holdout_last_n %||% 0L)[1])
  if (!is.finite(holdout_last_n) || holdout_last_n < 0L) {
    stop("holdout_last_n must be a non-negative integer.", call. = FALSE)
  }
  data.table::setorderv(dt, c(group_col, time_col))
  dt[, is_holdout__ := FALSE]
  if (!is.null(holdout_col)) {
    if (is.logical(holdout_value) && length(holdout_value) == 1L) {
      raw <- tolower(trimws(as.character(dt[[holdout_col]])))
      raw[is.na(raw)] <- ""
      truthy <- c("true", "t", "yes", "y", "1", "on", "holdout", "test")
      falsy <- c("false", "f", "no", "n", "0", "off", "train", "training")
      dt[, is_holdout__ := raw %in% if (isTRUE(holdout_value)) truthy else falsy]
    } else {
      dt[, is_holdout__ := as.character(get(holdout_col)) %in% as.character(holdout_value)]
    }
  }
  if (holdout_last_n > 0L) {
    dt[, is_holdout__ := is_holdout__ | (seq_len(.N) > pmax(.N - holdout_last_n, 0L)), by = group_col]
  }
  if (!dt[, any(!is_holdout__)]) stop("All sequential rows are holdout; root training requires observations.", call. = FALSE)
  time_status <- dt[, .(holdout_states = data.table::uniqueN(is_holdout__)), by = time_col]
  if (time_status[holdout_states > 1L, .N]) {
    stop("Sequential national-root holdout status must be consistent across groups within each period.", call. = FALSE)
  }
  unique(dt[is_holdout__ == FALSE, get(time_col)])
}

#' Fit the default parsimonious total-paid-media root.
#'
#' The default is a nationally aggregated, frequentist total-paid-media root.
#' When genuinely observed group-level paid-media variation is identifiable,
#' `root_scope = "hierarchical_panel"` retains the geo-time panel and can fit
#' partially pooled geo total-media effects. Exposure scaling applies to both
#' the KPI and media pressure while outcome-per-cost reporting remains on raw
#' KPI/spend totals. The root supports a shared low-dimensional Fourier or knot
#' time basis, guarded linear versus concave adstock/Hill selection, and a
#' blocked time bootstrap. An optional Gaussian calibration penalty is
#' available for the shared-effect fit; it is not described as a Bayesian prior.
fit_parsimonious_total_media_root <- function(data,
                                              metadata_input,
                                              dep_var_col,
                                              group_col,
                                              time_col,
                                              entity_col,
                                              spend_map = NULL,
                                              media_variables = NULL,
                                              media_scope_config = NULL,
                                              population_col = NULL,
                                              market_size_col = NULL,
                                              target_population_col = NULL,
                                              invalid_allocation_fallback = c("error", "equal"),
                                              root_pressure_scaling = c("auto", "none", "per_capita"),
                                              root_pressure_col = NULL,
                                              root_control_cols = NULL,
                                              root_control_mode = c("declared_controls", "all_nonmedia", "none"),
                                              root_scope = c("national", "hierarchical_panel"),
                                              kpi_aggregation_rule = "sum",
                                              kpi_weight_col = NULL,
                                              root_control_aggregation = NULL,
                                              root_aggregation_functions = list(),
                                              incomplete_period_action = c("error", "drop"),
                                              root_trend_spec = c("none", "linear"),
                                              root_fourier_harmonics = 2L,
                                              root_season_period = 52L,
                                              root_time_baseline = c("auto", "fourier", "knots"),
                                              root_knot_n = 6L,
                                              root_knot_penalty = 1,
                                              root_geo_media_effect = c("shared", "partially_pooled"),
                                              root_media_transform = c("adstock_hill", "linear"),
                                              root_rrate_grid = c(0, 0.25, 0.50, 0.70),
                                              root_anchor_saturation_grid = c(0.30, 0.50, 0.70),
                                              root_curve_min_delta_aicc = 2,
                                              root_effect_sign = c("positive", "unconstrained", "negative"),
                                              root_nonlinear_starts = 24L,
                                              root_rrate_bounds = c(0, 0.95),
                                              root_half_saturation_multiple_bounds = c(0.05, 10),
                                              root_steepness_bounds = c(0.25, 5),
                                              root_optimizer_maxit = 300L,
                                              root_bootstrap_reps = 200L,
                                              root_block_length = 4L,
                                              root_effect_prior = NULL,
                                              holdout_col = NULL,
                                              holdout_value = TRUE,
                                              holdout_last_n = 0L,
                                              seed = 123L) {
  root_control_mode <- match.arg(root_control_mode)
  root_scope <- match.arg(root_scope)
  root_media_transform <- match.arg(root_media_transform)
  root_effect_sign <- match.arg(root_effect_sign)
  incomplete_period_action <- match.arg(incomplete_period_action)
  invalid_allocation_fallback <- match.arg(invalid_allocation_fallback)
  root_trend_spec <- match.arg(root_trend_spec)
  root_pressure_scaling <- match.arg(root_pressure_scaling)
  root_geo_media_effect <- match.arg(root_geo_media_effect)
  root_time_baseline <- match.arg(root_time_baseline)
  if (is.null(root_pressure_col)) root_pressure_col <- population_col %||% market_size_col
  canonical <- canonicalize_sequential_media_panel(
    data = data,
    metadata_input = metadata_input,
    group_col = group_col,
    time_col = time_col,
    entity_col = entity_col,
    spend_map = spend_map,
    media_variables = media_variables,
    media_scope_config = media_scope_config,
    population_col = population_col,
    market_size_col = market_size_col,
    target_population_col = target_population_col,
    invalid_allocation_fallback = invalid_allocation_fallback
  )
  modeled_spend_map <- data.table::copy(canonical$spend_map)
  if ("spend_bearing" %in% names(modeled_spend_map)) {
    modeled_spend_map <- modeled_spend_map[spend_bearing %in% TRUE]
  }
  if (!nrow(modeled_spend_map)) {
    stop("No spend-bearing media nodes remain after excluding reach/frequency auxiliary execution variables.", call. = FALSE)
  }
  panel <- econ_seq_root_panel(
    data = canonical$data,
    metadata_input = canonical$metadata,
    dep_var_col = dep_var_col,
    group_col = group_col,
    time_col = time_col,
    entity_col = entity_col,
    spend_map = modeled_spend_map,
    media_variables = media_variables,
    national_spend = canonical$national_spend,
    root_control_cols = root_control_cols,
    root_control_mode = root_control_mode,
    root_scope = root_scope,
    kpi_aggregation_rule = kpi_aggregation_rule,
    kpi_weight_col = kpi_weight_col,
    root_control_aggregation = root_control_aggregation,
    root_aggregation_functions = root_aggregation_functions,
    incomplete_period_action = incomplete_period_action,
    root_pressure_scaling = root_pressure_scaling,
    root_pressure_col = root_pressure_col
  )
  training_times <- econ_seq_training_time_values(
    data = canonical$data,
    group_col = group_col,
    time_col = time_col,
    holdout_col = holdout_col,
    holdout_value = holdout_value,
    holdout_last_n = holdout_last_n
  )
  root_model_data <- panel$data[root_time__ %in% training_times]
  if (!nrow(root_model_data)) stop("No national-root training periods remain after holdout selection.", call. = FALSE)
  curve_selection <- econ_seq_select_root_curve(
    root_data = root_model_data,
    control_cols = panel$control_cols,
    root_trend_spec = root_trend_spec,
    root_fourier_harmonics = root_fourier_harmonics,
    root_season_period = root_season_period,
    root_time_baseline = root_time_baseline,
    root_knot_n = root_knot_n,
    root_knot_penalty = root_knot_penalty,
    root_geo_media_effect = root_geo_media_effect,
    root_effect_prior = root_effect_prior,
    root_media_transform = root_media_transform,
    root_rrate_grid = root_rrate_grid,
    root_anchor_saturation_grid = root_anchor_saturation_grid,
    root_curve_min_delta_aicc = root_curve_min_delta_aicc,
    root_effect_sign = root_effect_sign,
    root_nonlinear_starts = root_nonlinear_starts,
    root_rrate_bounds = root_rrate_bounds,
    root_half_saturation_multiple_bounds = root_half_saturation_multiple_bounds,
    root_steepness_bounds = root_steepness_bounds,
    root_optimizer_maxit = root_optimizer_maxit
  )
  point <- curve_selection$selected
  point_fit <- econ_seq_fit_root_lm(
    root_data = root_model_data,
    control_cols = panel$control_cols,
    root_trend_spec = root_trend_spec,
    root_fourier_harmonics = root_fourier_harmonics,
    root_season_period = root_season_period,
    root_effect_prior = root_effect_prior,
    root_effect_sign = root_effect_sign,
    root_curve_spec = curve_selection$selected_spec,
    root_time_baseline = root_time_baseline,
    root_knot_n = root_knot_n,
    root_knot_penalty = root_knot_penalty,
    root_geo_media_effect = root_geo_media_effect
  )
  root_geo_media_effects <- attr(point_fit, "root_geo_media_effects") %||% data.table::data.table()
  draws <- econ_seq_block_bootstrap_root(
    root_data = root_model_data,
    control_cols = panel$control_cols,
    root_trend_spec = root_trend_spec,
    root_fourier_harmonics = root_fourier_harmonics,
    root_season_period = root_season_period,
    root_effect_prior = root_effect_prior,
    root_curve_spec = curve_selection$selected_spec,
    root_media_transform = root_media_transform,
    root_rrate_grid = root_rrate_grid,
    root_anchor_saturation_grid = root_anchor_saturation_grid,
    root_curve_min_delta_aicc = root_curve_min_delta_aicc,
    root_effect_sign = root_effect_sign,
    root_nonlinear_starts = root_nonlinear_starts,
    root_rrate_bounds = root_rrate_bounds,
    root_half_saturation_multiple_bounds = root_half_saturation_multiple_bounds,
    root_steepness_bounds = root_steepness_bounds,
    root_optimizer_maxit = root_optimizer_maxit,
    root_time_baseline = root_time_baseline,
    root_knot_n = root_knot_n,
    root_knot_penalty = root_knot_penalty,
    root_geo_media_effect = root_geo_media_effect,
    reselect_curve = TRUE,
    reps = root_bootstrap_reps,
    block_length = root_block_length,
    seed = seed
  )
  usable_draws <- draws[fit_ok == TRUE & is.finite(root_effectiveness), root_effectiveness]
  usable_bootstrap <- draws[fit_ok == TRUE]
  nonlinear_bootstrap <- usable_bootstrap[
    root_curve_type == "adstock_hill" & is.finite(root_rrate) &
      is.finite(root_half_saturation) & is.finite(root_steepness)
  ]
  root_curve_quantile <- function(column, probability) {
    values <- nonlinear_bootstrap[[column]]
    values <- values[is.finite(values)]
    if (!length(values)) return(NA_real_)
    as.numeric(stats::quantile(values, probability, names = FALSE, na.rm = TRUE))
  }
  nonlinear_selection_rate <- if (nrow(usable_bootstrap)) {
    mean(usable_bootstrap$root_curve_type == "adstock_hill", na.rm = TRUE)
  } else NA_real_
  root_sd <- if (length(usable_draws) >= 10L) stats::sd(usable_draws) else point$root_effectiveness_analytic_sd[1]
  if (!is.finite(root_sd) || root_sd <= 0) root_sd <- max(abs(point$root_effectiveness[1]) * 0.50, 1e-8)
  root_status <- econ_seq_classify_effectiveness(point$root_effectiveness[1], root_sd)
  nonlinear_diagnostics <- data.table::as.data.table(curve_selection$nonlinear_diagnostics %||% data.table::data.table())
  fallback_recommended <- nrow(nonlinear_diagnostics) && isTRUE(nonlinear_diagnostics$fallback_recommended[1])
  fallback_reason <- if (nrow(nonlinear_diagnostics)) nonlinear_diagnostics$fallback_reason[1] else ""
  root_id <- econ_seq_layer_identification_diagnostics(
    data = root_model_data,
    spend_map = data.table::data.table(variable = "total_paid_media", spend_col = "root_total_paid_spend__"),
    group_col = "root_group__",
    time_col = "root_time_index__",
    dep_var_col = "root_y__",
    control_cols = panel$control_cols,
    baseline_trend_spec = root_trend_spec,
    baseline_fourier_harmonics = root_fourier_harmonics,
    season_period = root_season_period,
    layer_label = "total_paid_media_root"
  )
  point[, `:=`(
    root_effectiveness_sd = root_sd,
    root_effectiveness_q05 = if (length(usable_draws)) as.numeric(stats::quantile(usable_draws, 0.05, na.rm = TRUE)) else NA_real_,
    root_effectiveness_q50 = if (length(usable_draws)) as.numeric(stats::quantile(usable_draws, 0.50, na.rm = TRUE)) else NA_real_,
    root_effectiveness_q95 = if (length(usable_draws)) as.numeric(stats::quantile(usable_draws, 0.95, na.rm = TRUE)) else NA_real_,
    root_nonlinear_selection_rate = nonlinear_selection_rate,
    root_rrate_q05 = root_curve_quantile("root_rrate", 0.05),
    root_rrate_q50 = root_curve_quantile("root_rrate", 0.50),
    root_rrate_q95 = root_curve_quantile("root_rrate", 0.95),
    root_half_saturation_q05 = root_curve_quantile("root_half_saturation", 0.05),
    root_half_saturation_q50 = root_curve_quantile("root_half_saturation", 0.50),
    root_half_saturation_q95 = root_curve_quantile("root_half_saturation", 0.95),
    root_steepness_q05 = root_curve_quantile("root_steepness", 0.05),
    root_steepness_q50 = root_curve_quantile("root_steepness", 0.50),
    root_steepness_q95 = root_curve_quantile("root_steepness", 0.95),
    root_bootstrap_requested = as.integer(root_bootstrap_reps),
    root_bootstrap_successful = length(usable_draws),
    root_block_length = as.integer(root_block_length),
    root_bootstrap_seasonality = "Moving-block residuals generate KPI outcomes on the unchanged media timeline. Calendar phase and adstock history remain original, and each replicate reselects the root transform.",
    root_bootstrap_method = "moving_block_residual_original_media_timeline",
    root_bootstrap_original_media_timeline_preserved = TRUE,
    root_bootstrap_curve_selection_repeated = TRUE,
    root_effectiveness_status = root_status,
    root_bayesian_fallback_recommended = fallback_recommended,
    root_bayesian_fallback_reason = fallback_reason,
    root_bayesian_fallback_policy = "invoke_only_for_flat_unstable_or_bound_hitting_nonlinear_likelihood",
    root_scope = root_scope,
    root_pressure_scaling = panel$root_pressure_scaling,
    root_pressure_col = panel$root_pressure_col,
    root_identification_recommendation = root_id$overall$identification_recommendation[1],
    root_prior_width_multiplier = root_id$overall$prior_width_multiplier[1],
    root_evidence_type = "frequentist_moving_block_residual_bootstrap_sampling_distribution",
    root_interpretation = "Portfolio average incremental KPI per paid-media cost unit; do not interpret as a channel-specific causal estimate."
  )]
  mix <- econ_seq_mix_diagnostics(canonical$data[get(time_col) %in% training_times], panel$spend_map, time_col = time_col)
  list(
    package_info = econimap_output_metadata("fit_parsimonious_total_media_root", surface = "sequential_root"),
    root_summary = point[],
    bootstrap_draws = draws[],
    spend_map = panel$spend_map[],
    root_control_cols = panel$control_cols,
    root_panel = panel$data[],
    root_training_panel = root_model_data[],
    root_training_times = training_times,
    root_holdout_col = holdout_col,
    root_holdout_value = holdout_value,
    root_holdout_last_n = holdout_last_n,
    root_scope = root_scope,
    root_pressure_scaling = panel$root_pressure_scaling,
    root_pressure_col = panel$root_pressure_col,
    root_time_baseline = point$root_time_baseline[1],
    root_knot_n = as.integer(root_knot_n),
    root_geo_media_effect = point$root_geo_media_effect_mode[1],
    root_geo_media_effects = data.table::as.data.table(root_geo_media_effects),
    canonical_data = canonical$data[],
    canonical_metadata = canonical$metadata[],
    canonical_spend_map = modeled_spend_map[],
    media_scope_config = canonical$media_config[],
    media_allocation_audit = canonical$allocation_audit[],
    national_spend = canonical$national_spend[],
    national_aggregation_audit = panel$national_aggregation_audit[],
    root_scope_eligibility = panel$hierarchical_scope_eligibility %||% data.table::data.table(
      root_scope = "national", eligible = TRUE, decision = "fit_national_root",
      note = "National aggregation is the default sequential root."
    ),
    root_curve_candidates = curve_selection$candidates[],
    root_nonlinear_optimizer_runs = curve_selection$optimizer_runs[],
    root_nonlinear_diagnostics = nonlinear_diagnostics[],
    root_identification = root_id$by_variable[],
    root_identification_overall = root_id$overall[],
    mix_diagnostics = mix$summary[],
    mix_by_variable = mix$by_variable[]
  )
}

#' Create equal-effectiveness child priors from a total-media root.
#'
#' The returned `reference_calibration_input` is an explicit compatibility
#' handoff to `fit_hier_mmm(calibration_input = ...)`; the sequential runner
#' defaults to a joint aggregate-effectiveness transfer with learned sibling
#' dispersion. `business_priors` remains an auditable compatibility
#' representation. The transfer width deliberately includes root sampling
#' uncertainty, same-data reuse inflation, a child heterogeneity allowance,
#' and observed spend-mix instability.
build_sequential_effectiveness_priors <- function(root_fit,
                                                   data,
                                                   metadata_input,
                                                   time_col,
                                                   training_times = NULL,
                                                   child_variables = NULL,
                                                   child_spend_map = NULL,
                                                   rollup_map = NULL,
                                                   child_prior_overrides = NULL,
                                                   data_reuse_inflation = 1.5,
                                                   child_heterogeneity_relative_sd = 0.50,
                                                   mix_transfer_scale = 1,
                                                   minimum_relative_sd = 0.35,
                                                   child_identification = NULL,
                                                   strong_child_prior_relaxation = 1.20,
                                                   warn_partial_handoff = TRUE) {
  if (is.null(root_fit$root_summary) || !nrow(root_fit$root_summary)) stop("root_fit must be returned by fit_parsimonious_total_media_root().", call. = FALSE)
  root_summary <- data.table::as.data.table(root_fit$root_summary)[1]
  root_mean <- as.numeric(root_summary$root_effectiveness)
  root_sd <- as.numeric(root_summary$root_effectiveness_sd)
  if (!is.finite(root_mean) || !is.finite(root_sd) || root_sd <= 0) stop("root_fit has no usable effectiveness distribution.", call. = FALSE)
  data_reuse_inflation <- max(1, as.numeric(data_reuse_inflation)[1])
  child_heterogeneity_relative_sd <- max(0, as.numeric(child_heterogeneity_relative_sd)[1])
  mix_transfer_scale <- max(0, as.numeric(mix_transfer_scale)[1])
  minimum_relative_sd <- max(0, as.numeric(minimum_relative_sd)[1])
  root_status <- as.character(root_summary$root_effectiveness_status %||% econ_seq_classify_effectiveness(root_mean, root_sd))[1]
  if (is.null(child_spend_map)) {
    sm <- data.table::as.data.table(root_fit$spend_map)
  } else {
    sm <- econ_seq_input_table(child_spend_map, "child_spend_map")
    if (!"variable" %in% names(sm)) stop("child_spend_map must include variable.", call. = FALSE)
    if (!"spend_col" %in% names(sm)) {
      if ("cost_col" %in% names(sm)) data.table::setnames(sm, "cost_col", "spend_col")
      else stop("child_spend_map must include spend_col or cost_col.", call. = FALSE)
    }
    sm <- unique(sm[, .(variable = as.character(variable), spend_col = as.character(spend_col))], by = "variable")
    if (anyDuplicated(sm$variable)) stop("child_spend_map has duplicate variable rows.", call. = FALSE)
  }
  available_child_variables <- sm$variable
  if (!is.null(child_variables)) sm <- sm[variable %in% as.character(child_variables)]
  if (!nrow(sm)) stop("No sequential child variables remain after filtering.", call. = FALSE)
  partial_handoff <- !setequal(sm$variable, available_child_variables)
  excluded_child_variables <- setdiff(available_child_variables, sm$variable)
  if (isTRUE(partial_handoff) && isTRUE(warn_partial_handoff)) {
    warning(
      "Partial sequential child handoff: ", paste(excluded_child_variables, collapse = ", "),
      " receive no inherited parent prior. Parent reconciliation is scoped only to selected children.",
      call. = FALSE
    )
  }
  dt <- econ_seq_input_table(data, "data")
  if (!is.null(training_times)) {
    if (!(time_col %in% names(dt))) stop("time_col is required to apply the sequential training scope.", call. = FALSE)
    dt <- dt[get(time_col) %in% training_times]
  }
  if (!nrow(dt)) stop("No training rows remain for sequential child-prior construction.", call. = FALSE)
  missing_child_spend <- setdiff(sm$spend_col, names(dt))
  if (length(missing_child_spend)) {
    stop("Child spend column(s) missing from data: ", paste(missing_child_spend, collapse = ", "), call. = FALSE)
  }
  total_spend <- vapply(seq_len(nrow(sm)), function(ii) {
    z <- pmax(suppressWarnings(as.numeric(dt[[sm$spend_col[ii]]])), 0)
    sum(z[is.finite(z)], na.rm = TRUE)
  }, numeric(1))
  mix_churn <- if (!is.null(root_fit$mix_diagnostics) && nrow(root_fit$mix_diagnostics)) {
    as.numeric(root_fit$mix_diagnostics$media_mix_churn[1])
  } else 0
  mix_churn <- if (is.finite(mix_churn)) max(mix_churn, 0) else 0
  root_component_sd <- root_sd * data_reuse_inflation
  heterogeneity_sd <- abs(root_mean) * child_heterogeneity_relative_sd
  mix_sd <- abs(root_mean) * mix_churn * mix_transfer_scale
  prior_sd <- sqrt(root_component_sd^2 + heterogeneity_sd^2 + mix_sd^2)
  prior_sd <- max(prior_sd, abs(root_mean) * minimum_relative_sd, 1e-8)
  transferable <- identical(root_status, "positive_transferable")
  transfer_mean <- if (transferable) root_mean else 0
  root_rrate_prior <- econ_seq_root_rrate_distribution(root_fit)
  if (!transferable) {
    # A negative or near-zero root cannot be transferred as a negative prior
    # into a positive paid-media child model. Retain it only as wide, neutral
    # evidence rather than manufacturing a positive effect.
    prior_sd <- max(prior_sd, root_component_sd * 2, abs(root_mean), 1e-8)
  }
  out <- data.table::data.table(
    variable = sm$variable,
    prior_metric = "ikpc",
    prior_mean = transfer_mean,
    prior_sd = prior_sd,
    prior_uncertainty_basis = "sd",
    prior_distribution = "normal",
    evidence_source = if (transferable) "sequential_total_paid_media_root" else "sequential_total_paid_media_root_weak_neutral",
    evidence_notes = if (transferable) "Equal-effectiveness transfer from total paid media; contribution is implied by observed child spend, not allocated equally." else "Total-media evidence is negative or near zero/inconclusive for a positive paid-media effect. Child priors are neutral and deliberately wide.",
    sequential_parent_id = "total_paid_media",
    sequential_transfer_mode = if (transferable) "equal_effectiveness" else "weak_neutral_no_positive_transfer",
    sequential_root_mean = root_mean,
    sequential_root_sd = root_sd,
    sequential_root_effectiveness_status = root_status,
    parent_positive_effect_transferred = transferable,
    # Generic root defaults are not nonlinear parent evidence. Preserve the
    # child metadata untouched unless a root Hill/adstock candidate carries
    # actual model-supported nonlinear information.
    curve_prior_available = transferable && isTRUE(root_rrate_prior$parent_curve_evidence_available[1]),
    curve_prior_mode = root_rrate_prior$curve_prior_mode[1],
    root_nonlinear_model_weight = root_rrate_prior$root_nonlinear_model_weight[1],
    root_nonlinear_model_weight_source = root_rrate_prior$root_nonlinear_model_weight_source[1],
    rrate_prior_mean = root_rrate_prior$rrate_prior_mean[1],
    rrate_prior_sd = root_rrate_prior$rrate_prior_sd[1],
    rrate_prior_precision = root_rrate_prior$rrate_prior_precision[1],
    rrate_prior_source = root_rrate_prior$rrate_prior_source[1],
    anchor_saturation_prior_mean = root_rrate_prior$anchor_saturation_prior_mean[1],
    anchor_saturation_prior_sd = root_rrate_prior$anchor_saturation_prior_sd[1],
    anchor_saturation_prior_precision = root_rrate_prior$anchor_saturation_prior_precision[1],
    anchor_saturation_prior_source = root_rrate_prior$anchor_saturation_prior_source[1],
    rrate_pooling_mode = "shared_parent_regularization_no_latent_sibling_pool",
    sequential_root_data_reuse_inflation = data_reuse_inflation,
    sequential_root_sd_component = root_component_sd,
    sequential_child_heterogeneity_sd_component = heterogeneity_sd,
    sequential_mix_sd_component = mix_sd,
    sequential_media_mix_churn = mix_churn,
    sequential_spend_col = sm$spend_col,
    sequential_training_period_n = data.table::uniqueN(dt[[time_col]]),
    child_spend_total = total_spend,
    child_spend_share = total_spend / sum(total_spend),
    implied_child_contribution_mean = transfer_mean * total_spend,
    prior_evidence_mode = if (transferable) "parent_regularized" else "weak_neutral_parent_evidence"
  )
  rollup <- econ_seq_rollup_map(metadata_input, out$variable, rollup_map = rollup_map)
  out[rollup, on = "variable", `:=`(
    rollup_path = i.rollup_path,
    rollup_root = i.rollup_root,
    rollup_parent = i.rollup_parent,
    rollup_leaf = i.rollup_leaf
  )]
  out[!is.finite(child_spend_share), child_spend_share := NA_real_]
  root_width_multiplier <- suppressWarnings(as.numeric(root_summary$root_prior_width_multiplier %||% 1))[1]
  if (!is.finite(root_width_multiplier) || root_width_multiplier < 1) root_width_multiplier <- 1
  out <- econ_seq_apply_branch_diagnostics(
    prior_table = out,
    child_identification = child_identification,
    child_prior_overrides = child_prior_overrides,
    strong_child_prior_relaxation = strong_child_prior_relaxation
  )
  out[, `:=`(
    parent_uncertainty_width_multiplier = root_width_multiplier,
    diagnostic_prior_width_multiplier = root_width_multiplier,
    sequential_prior_application = "joint_reference_spend_calibration"
  )]
  out[, prior_sd := prior_sd * parent_uncertainty_width_multiplier / sqrt(child_identification_pooling_multiplier)]
  out[parent_positive_effect_transferred == FALSE,
      prior_sd := pmax(prior_sd, sequential_root_sd_component, abs(sequential_root_mean), 1e-8)]
  out[, `:=`(
    rrate_prior_precision = data.table::fifelse(
      curve_prior_available & is.finite(rrate_prior_precision),
      rrate_prior_precision * child_identification_pooling_multiplier,
      NA_real_
    ),
    anchor_saturation_prior_precision = data.table::fifelse(
      curve_prior_available & is.finite(anchor_saturation_prior_precision),
      anchor_saturation_prior_precision * child_identification_pooling_multiplier,
      NA_real_
    )
  )]
  out <- econ_seq_apply_child_prior_overrides(out, child_prior_overrides)
  ledger <- data.table::copy(out)
  ledger[, `:=`(
    generated_prior = TRUE,
    parent_evidence_type = root_summary$root_evidence_type,
    parent_fit_method = root_summary$root_fit_method,
    parent_bootstrap_successful = root_summary$root_bootstrap_successful,
    prior_precision = 1 / prior_sd^2,
    prior_audit_note = "Staged empirical-Bayes handoff. The effectiveness prior is applied through a joint reference-spend calibration likelihood, while inherited curve priors remain estimable."
  )]
  list(
    business_priors = out[],
    prior_ledger = ledger[],
    reference_calibration_input = econ_seq_reference_effectiveness_calibration(out),
    branch_decisions = out[, .(variable, fit_status, branch_decision, branch_decision_reason,
                                child_identification_recommendation, child_identification_pooling_multiplier,
                                child_identification_strength_0_1, parent_shrinkage_multiplier,
                                user_prior_override_present, user_prior_override_valid,
                                override_validation_reason, prior_dominance_classification)],
    handoff_scope = data.table::data.table(
      partial_child_handoff = partial_handoff,
      selected_child_variable_n = length(sm$variable),
      available_child_variable_n = length(available_child_variables),
      excluded_child_variables = paste(excluded_child_variables, collapse = " | "),
      parent_effectiveness_status = root_status,
      parent_positive_effect_transferred = transferable
    )
  )
}

econ_seq_parent_rrate_summary <- function(parent_fit, parent_variables, max_draws = 200L) {
  if (is.null(parent_fit$fit) || !is.function(parent_fit$fit$draws)) return(data.table::data.table())
  draw_matrix <- function(variable) {
    out <- tryCatch(as.matrix(parent_fit$fit$draws(variables = variable, format = "matrix")), error = function(e) NULL)
    if (is.null(out) || !nrow(out) || !ncol(out)) NULL else out
  }
  rrate_mat <- draw_matrix("rrate")
  cvalue_mat <- draw_matrix("cvalue")
  dvalue_mat <- draw_matrix("dvalue")
  if (is.null(rrate_mat)) return(data.table::data.table())
  max_draws <- max(1L, as.integer(max_draws)[1])
  keep <- unique(pmax(1L, pmin(nrow(rrate_mat), round(seq(1, nrow(rrate_mat), length.out = min(max_draws, nrow(rrate_mat)))))))
  rrate_mat <- rrate_mat[keep, , drop = FALSE]
  if (!is.null(cvalue_mat)) cvalue_mat <- cvalue_mat[keep, , drop = FALSE]
  if (!is.null(dvalue_mat)) dvalue_mat <- dvalue_mat[keep, , drop = FALSE]
  vl <- data.table::as.data.table(parent_fit$variable_lookup)[has_curve == 1L][order(curve_param_idx)]
  vl <- vl[variable %in% parent_variables]
  data.table::rbindlist(lapply(seq_len(nrow(vl)), function(ii) {
    col <- paste0("rrate[", vl$curve_param_idx[ii], "]")
    if (!(col %in% colnames(rrate_mat))) return(NULL)
    x <- as.numeric(rrate_mat[, col])
    sd_value <- stats::sd(x)
    if (!is.finite(sd_value) || sd_value < 0.05) sd_value <- 0.05
    c_col <- paste0("cvalue[", vl$curve_param_idx[ii], "]")
    d_col <- paste0("dvalue[", vl$curve_param_idx[ii], "]")
    c_draws <- if (!is.null(cvalue_mat) && c_col %in% colnames(cvalue_mat)) as.numeric(cvalue_mat[, c_col]) else numeric()
    d_draws <- if (!is.null(dvalue_mat) && d_col %in% colnames(dvalue_mat)) as.numeric(dvalue_mat[, d_col]) else rep(1, length(c_draws))
    curve_type <- normalize_curve_type_hier_mmm(vl$curve_type[ii] %||% "hill")[1]
    anchor_draws <- if (length(c_draws)) {
      vapply(seq_along(c_draws), function(jj) {
        if (!is.finite(c_draws[jj]) || c_draws[jj] <= 0) return(NA_real_)
        saturate_media_hier_mmm(1, cvalue = c_draws[jj], dvalue = d_draws[jj], curve_type = curve_type)
      }, numeric(1))
    } else numeric()
    anchor_draws <- anchor_draws[is.finite(anchor_draws)]
    anchor_mean <- if (length(anchor_draws)) mean(anchor_draws) else NA_real_
    anchor_sd <- if (length(anchor_draws) >= 2L) stats::sd(anchor_draws) else NA_real_
    if (is.finite(anchor_sd)) anchor_sd <- max(anchor_sd, 0.05)
    data.table::data.table(
      variable = vl$variable[ii],
      rrate_prior_mean = mean(x),
      rrate_prior_sd = sd_value,
      rrate_prior_precision = 1 / sd_value^2,
      rrate_prior_source = "sequential_parent_stan_posterior",
      anchor_saturation_prior_mean = anchor_mean,
      anchor_saturation_prior_sd = anchor_sd,
      anchor_saturation_prior_precision = if (is.finite(anchor_sd) && anchor_sd > 0) 1 / anchor_sd^2 else NA_real_,
      anchor_saturation_prior_source = if (length(anchor_draws)) "sequential_parent_stan_posterior_normalized_reference" else NA_character_
    )
  }), fill = TRUE)
}

econ_seq_parent_effectiveness_draws <- function(parent_fit,
                                                parent_layer,
                                                training_times = NULL,
                                                max_draws = 200L,
                                                seed = 123L) {
  if (is.null(parent_layer$spend_map) || !nrow(parent_layer$spend_map)) {
    stop("parent_layer must be returned by build_sequential_rollup_layer() or a sequential stage.", call. = FALSE)
  }
  parent_variables <- as.character(parent_layer$spend_map$variable)
  parent_data <- econ_seq_input_table(parent_layer$data, "parent_layer$data")
  if (!is.null(training_times)) {
    time_col <- parent_fit$time_col %||% NULL
    if (is.null(time_col) || !(time_col %in% names(parent_data))) {
      stop("Parent fit needs a valid time_col to rebuild training-scope response curves.", call. = FALSE)
    }
    parent_data <- parent_data[get(time_col) %in% training_times]
  }
  curves <- if (is.null(training_times)) parent_fit$response_curves_draws %||% data.table::data.table() else data.table::data.table()
  curves <- data.table::as.data.table(curves)
  if (!nrow(curves)) {
    if (is.null(parent_fit$fit) || !is.function(parent_fit$fit$draws)) {
      stop("parent_fit needs response_curves_draws or a CmdStan posterior fit to transfer posterior parent evidence.", call. = FALSE)
    }
    curves <- build_response_curves_draws_hier_mmm(
      fit_obj = parent_fit,
      spend_map = parent_layer$spend_map,
      raw_data = parent_data,
      variables = parent_variables,
      multiplier_grid = 1,
      response_curve_scope = "total",
      max_draws = max_draws,
      seed = seed
    )
  }
  required <- c(".draw", "scope", "variable", "spend_multiplier", "roi", "contribution", "current_spend")
  missing <- setdiff(required, names(curves))
  if (length(missing)) {
    stop("Parent response-curve draws are missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  curves[, `:=`(
    parent_draw__ = as.character(get(".draw")),
    parent_roi__ = suppressWarnings(as.numeric(roi)),
    parent_contribution__ = suppressWarnings(as.numeric(contribution)),
    parent_spend__ = suppressWarnings(as.numeric(current_spend)),
    parent_multiplier__ = suppressWarnings(as.numeric(spend_multiplier))
  )]
  usable <- curves[
    variable %in% parent_variables & scope == "total" &
      is.finite(parent_multiplier__) & abs(parent_multiplier__ - 1) <= 1e-8 &
      is.finite(parent_contribution__) & is.finite(parent_spend__) & parent_spend__ > 0
  ]
  if (!nrow(usable)) {
    stop("Parent response-curve draws contain no finite total-scope ROI values at the current spend multiplier.", call. = FALSE)
  }
  usable <- usable[, .(
    parent_contribution_draw = sum(parent_contribution__),
    parent_spend_draw = sum(parent_spend__),
    reported_roi_weighted__ = sum(parent_roi__ * parent_spend__) / sum(parent_spend__)
  ), by = .(variable, parent_draw__)]
  usable[, parent_effectiveness_draw := parent_contribution_draw / parent_spend_draw]
  usable[, reported_vs_recomputed_roi_gap := abs(reported_roi_weighted__ - parent_effectiveness_draw)]
  summary <- usable[, .(
    parent_effectiveness = mean(parent_effectiveness_draw),
    parent_effectiveness_sd = stats::sd(parent_effectiveness_draw),
    parent_draw_n = data.table::uniqueN(parent_draw__),
    parent_current_contribution = mean(parent_contribution_draw),
    parent_current_spend = mean(parent_spend_draw),
    parent_max_reported_vs_recomputed_roi_gap = max(reported_vs_recomputed_roi_gap, na.rm = TRUE)
  ), by = variable]
  if (any(summary$parent_draw_n < 10L)) {
    bad <- summary[parent_draw_n < 10L, variable]
    stop("At least 10 posterior draws are required for sequential parent transfer. Insufficient draws for: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  summary[!is.finite(parent_effectiveness_sd) | parent_effectiveness_sd <= 0,
          parent_effectiveness_sd := pmax(abs(parent_effectiveness) * 0.50, 1e-6)]
  summary[, parent_effectiveness_status := econ_seq_classify_effectiveness(parent_effectiveness, parent_effectiveness_sd)]
  rrate_summary <- econ_seq_parent_rrate_summary(parent_fit, parent_variables, max_draws = max_draws)
  if (nrow(rrate_summary)) summary <- merge(summary, rrate_summary, by = "variable", all.x = TRUE, sort = FALSE)
  audit <- summary[, .(
    variable,
    aggregation_method = "total_scope_contribution_divided_by_total_scope_spend_per_draw",
    parent_draw_n,
    max_abs_reported_vs_recomputed_roi_gap = parent_max_reported_vs_recomputed_roi_gap,
    reported_roi_used_for_transfer = FALSE,
    transfer_status = parent_effectiveness_status
  )]
  if (any(summary$parent_max_reported_vs_recomputed_roi_gap > 1e-8, na.rm = TRUE)) {
    warning("Parent response-curve ROI differed from contribution/spend; sequential transfer uses the recomputed aggregate ratio.", call. = FALSE)
  }
  list(draws = usable[], summary = summary[], roi_aggregation_audit = audit[])
}

econ_seq_child_variables_for_parent <- function(relation, parent_node_value) {
  relation <- data.table::as.data.table(relation)
  if (!all(c("parent_node", "child_variable") %in% names(relation))) return(character())
  unique(as.character(relation$child_variable[as.character(relation$parent_node) == as.character(parent_node_value)[1]]))
}

econ_seq_empty_collective_response_reconciliation <- function() {
  list(
    scenarios = data.table::data.table(
      reconciliation_id = character(),
      parent_response = numeric(),
      parent_response_sd = numeric()
    ),
    members = data.table::data.table(
      reconciliation_id = character(),
      variable = character(),
      time_value = character()
    ),
    audit = data.table::data.table()
  )
}

# Build soft collective saturation evidence from observed training-period
# mixes. It constrains the sum of child responses and never makes a child
# saturation parameter an independent copy of its parent.
econ_seq_collective_saturation_reconciliation_input <- function(parent_fit,
                                                                 parent_layer,
                                                                 child_layer,
                                                                 time_col,
                                                                 training_times = NULL,
                                                                 max_draws = 200L,
                                                                 seed = 123L,
                                                                 data_reuse_inflation = 1.5,
                                                                 child_heterogeneity_relative_sd = 0.50,
                                                                 mix_transfer_scale = 1,
                                                                 approximation_relative_sd = 0.20,
                                                                 minimum_relative_sd = 0.50) {
  empty <- function() list(scenarios = data.table::data.table(), members = data.table::data.table(), audit = data.table::data.table())
  parent_map <- data.table::as.data.table(parent_layer$variable_mapping %||% data.table::data.table())
  child_map <- data.table::as.data.table(child_layer$variable_mapping %||% data.table::data.table())
  if (!all(c("variable", "generated_variable") %in% names(parent_map)) || !all(c("variable", "generated_variable") %in% names(child_map))) return(empty())
  parent_map <- unique(parent_map[, .(variable, parent_node = generated_variable)], by = "variable")
  child_map <- child_map[, .(variable, child_variable = generated_variable)]
  relation <- unique(merge(child_map, parent_map, by = "variable", all = FALSE, sort = FALSE)[child_variable != parent_node], by = c("parent_node", "child_variable"))
  if (!nrow(relation)) return(empty())
  child_data <- econ_seq_input_table(child_layer$data, "child_layer$data")
  if (!is.null(training_times)) child_data <- child_data[get(time_col) %in% training_times]
  parent_data <- data.table::as.data.table(parent_fit$data %||% data.table::data.table())
  if (!is.null(training_times)) parent_data <- parent_data[get(time_col) %in% training_times]
  if (!nrow(child_data) || !nrow(parent_data) || !(time_col %in% names(parent_data))) return(empty())
  child_mix <- econ_seq_mix_diagnostics(child_data, data.table::as.data.table(child_layer$spend_map), time_col = time_col)$summary
  mix_churn <- if (nrow(child_mix)) as.numeric(child_mix$media_mix_churn[1]) else 0
  if (!is.finite(mix_churn) || mix_churn < 0) mix_churn <- 0
  draw_params <- extract_posterior_draw_params_hier_mmm(parent_fit, max_draws = max_draws, seed = seed)$params
  if (!length(draw_params)) return(empty())
  scenarios <- list(); members <- list(); audit <- list()
  scenario_labels <- c("low", "lower_mid", "median", "upper_mid", "high")
  for (parent_node in unique(relation$parent_node)) {
    parent_node_value <- parent_node
    if (!(parent_node %in% names(parent_data))) next
    support <- parent_data[, .(parent_support = sum(pmax(as.numeric(get(parent_node)), 0), na.rm = TRUE)), by = time_col]
    support <- support[is.finite(parent_support)]
    if (nrow(support) < 3L) next
    targets <- stats::quantile(support$parent_support, c(.10, .25, .50, .75, .90), na.rm = TRUE, names = FALSE)
    selected <- support[unique(vapply(targets, function(x) which.min(abs(support$parent_support - x)), integer(1)))]
    draw_totals <- data.table::rbindlist(lapply(draw_params, function(pm) {
      draw_id <- as.character(pm$draw_id %||% "")
      pm$draw_id <- NULL
      x <- tryCatch(variable_contribution_rows_hier_mmm(parent_fit, parent_node, posterior_params = pm), error = function(e) NULL)
      if (is.null(x)) return(data.table::data.table())
      data.table::data.table(time_value = as.character(parent_fit$data[[time_col]]), contribution = as.numeric(x))[
        , .(parent_response_draw = sum(contribution, na.rm = TRUE)), by = time_value][, parent_draw := draw_id][]
    }), fill = TRUE)
    if (!nrow(draw_totals)) next
    parent_curve <- econ_seq_parent_rrate_summary(parent_fit, parent_node, max_draws = max_draws)
    parent_anchor <- if (nrow(parent_curve)) parent_curve$anchor_saturation_prior_mean[1] else NA_real_
    parent_anchor_sd <- if (nrow(parent_curve)) parent_curve$anchor_saturation_prior_sd[1] else NA_real_
    selected[, time_key__ := as.character(get(time_col))]
    draw_summary <- draw_totals[selected, on = c("time_value" = "time_key__"), nomatch = 0L][
      , .(parent_response = mean(parent_response_draw), parent_posterior_sd_component = stats::sd(parent_response_draw), parent_draw_n = data.table::uniqueN(parent_draw)), by = time_value
    ]
    if (!nrow(draw_summary)) next
    child_variables <- econ_seq_child_variables_for_parent(relation, parent_node_value)
    for (ii in seq_len(nrow(draw_summary))) {
      response <- draw_summary$parent_response[ii]
      parent_sd <- draw_summary$parent_posterior_sd_component[ii]
      if (!is.finite(parent_sd) || parent_sd <= 0) parent_sd <- max(abs(response) * .25, 1e-6)
      components <- c(
        parent_posterior = parent_sd,
        data_reuse = parent_sd * max(1, data_reuse_inflation),
        heterogeneity = abs(response) * max(0, child_heterogeneity_relative_sd),
        mix_instability = abs(response) * mix_churn * max(0, mix_transfer_scale),
        approximation = abs(response) * max(0, approximation_relative_sd)
      )
      response_sd <- max(sqrt(sum(components^2)), abs(response) * max(0, minimum_relative_sd), 1e-6)
      sid <- paste0("collective_sat_", gsub("[^A-Za-z0-9]+", "_", parent_node), "_", ii)
      scenarios[[length(scenarios) + 1L]] <- data.table::data.table(
        reconciliation_id = sid, parent_node = parent_node,
        scenario_label = paste0("observed_support_", scenario_labels[min(ii, length(scenario_labels))]),
        parent_response = response, parent_response_sd = response_sd,
        parent_posterior_sd_component = components[["parent_posterior"]],
        data_reuse_sd_component = components[["data_reuse"]],
        heterogeneity_sd_component = components[["heterogeneity"]],
        mix_instability_sd_component = components[["mix_instability"]],
        approximation_sd_component = components[["approximation"]],
        parent_draw_n = draw_summary$parent_draw_n[ii],
        parent_anchor_saturation = parent_anchor,
        parent_anchor_saturation_sd = parent_anchor_sd
      )
      members[[length(members) + 1L]] <- data.table::data.table(reconciliation_id = sid, variable = child_variables, time_value = draw_summary$time_value[ii])
    }
    audit[[length(audit) + 1L]] <- data.table::data.table(parent_node = parent_node, child_n = length(child_variables), selected_scenario_n = nrow(draw_summary), media_mix_churn = mix_churn, reconciliation_mode = "soft_collective_observed_training_mix")
  }
  list(scenarios = data.table::rbindlist(scenarios, fill = TRUE), members = data.table::rbindlist(members, fill = TRUE), audit = data.table::rbindlist(audit, fill = TRUE))
}

econ_seq_collective_saturation_reconciliation_audit <- function(child_fit, reconciliation_input) {
  empty <- function() list(scenarios = data.table::data.table(), children = data.table::data.table())
  if (is.null(reconciliation_input) || !nrow(reconciliation_input$scenarios %||% data.table::data.table())) return(empty())
  scenarios <- data.table::as.data.table(reconciliation_input$scenarios)
  members <- data.table::as.data.table(reconciliation_input$members)
  data <- data.table::as.data.table(child_fit$data)
  time_col <- child_fit$time_col
  pm <- extract_pm_from_fit_obj_hier_mmm(child_fit)
  lookup <- data.table::as.data.table(child_fit$variable_lookup)
  curve_draw_summary <- function(parameter, curve_param_idx, fallback) {
    mat <- tryCatch(as.matrix(child_fit$fit$draws(parameter, format = "matrix")), error = function(e) NULL)
    col <- paste0(parameter, "[", curve_param_idx, "]")
    x <- if (!is.null(mat) && col %in% colnames(mat)) as.numeric(mat[, col]) else numeric()
    if (!length(x) || !any(is.finite(x))) return(c(q05 = fallback, q50 = fallback, q95 = fallback))
    as.numeric(stats::quantile(x[is.finite(x)], c(.05, .50, .95), names = FALSE))
  }
  branch <- data.table::as.data.table(child_fit$sequential_branch_decisions %||% data.table::data.table())
  out <- lapply(seq_len(nrow(scenarios)), function(ii) {
    sc <- scenarios[ii]
    mm <- members[reconciliation_id == sc$reconciliation_id]
    children <- data.table::rbindlist(lapply(mm$variable, function(v) {
      x <- variable_contribution_rows_hier_mmm(child_fit, v)
      idx <- which(as.character(data[[time_col]]) == as.character(mm[variable == v, time_value][1]))
      k <- lookup[variable == v, curve_param_idx][1]
      j <- lookup[variable == v, variable_idx][1]
      rr <- curve_draw_summary("rrate", k, pm$rrate[j])
      cv <- curve_draw_summary("cvalue", k, pm$cvalue[j])
      dv <- curve_draw_summary("dvalue", k, pm$dvalue[j])
      decision <- if (nrow(branch) && v %in% branch$variable) branch[variable == v, branch_decision][1] else "fit"
      evidence_class <- if (decision %in% c("strong_parent_shrinkage", "parent_retained", "parent_remainder")) {
        "identified_mainly_through_collective_constraint_or_prior"
      } else {
        "child_data_and_collective_constraint"
      }
      data.table::data.table(
        variable = v,
        contribution = sum(x[idx], na.rm = TRUE),
        posterior_rrate_q05 = rr[1], posterior_rrate_q50 = rr[2], posterior_rrate_q95 = rr[3],
        posterior_cvalue_q05 = cv[1], posterior_cvalue_q50 = cv[2], posterior_cvalue_q95 = cv[3],
        posterior_dvalue_q05 = dv[1], posterior_dvalue_q50 = dv[2], posterior_dvalue_q95 = dv[3],
        saturation_evidence_class = evidence_class
      )
    }))
    list(
      scenario = data.table::data.table(reconciliation_id = sc$reconciliation_id, parent_node = sc$parent_node, scenario_label = sc$scenario_label, parent_response = sc$parent_response, parent_response_sd = sc$parent_response_sd, parent_anchor_saturation = sc$parent_anchor_saturation %||% NA_real_, parent_anchor_saturation_sd = sc$parent_anchor_saturation_sd %||% NA_real_, aggregate_child_response = sum(children$contribution), reconciliation_error = sum(children$contribution) - sc$parent_response, reconciliation_z = (sum(children$contribution) - sc$parent_response) / sc$parent_response_sd),
      children = children[, `:=`(reconciliation_id = sc$reconciliation_id, parent_node = sc$parent_node)]
    )
  })
  list(scenarios = data.table::rbindlist(lapply(out, `[[`, "scenario"), fill = TRUE), children = data.table::rbindlist(lapply(out, `[[`, "children"), fill = TRUE))
}

econ_seq_parent_shape_response_draw <- function(parent_fit, variable, time_value, multiplier, posterior_params) {
  tmp <- parent_fit
  tmp$data <- data.table::copy(data.table::as.data.table(parent_fit$data))
  idx <- which(as.character(tmp$data[[parent_fit$time_col]]) == as.character(time_value))
  if (!length(idx) || !(variable %in% names(tmp$data))) return(NA_real_)
  # Proportionally perturb the complete pre-scenario history. Perturbing only
  # the target row would change current pulse versus carryover and confound the
  # shape target with adstock dynamics.
  time_levels <- unique(as.character(tmp$data[[parent_fit$time_col]]))
  target_period <- match(as.character(time_value), time_levels)
  history_idx <- which(match(as.character(tmp$data[[parent_fit$time_col]]), time_levels) <= target_period)
  tmp$data[history_idx, (variable) := pmax(as.numeric(get(variable)) * multiplier, 0)]
  response <- tryCatch(variable_contribution_rows_hier_mmm(tmp, variable, posterior_params = posterior_params), error = function(e) NULL)
  if (is.null(response)) return(NA_real_)
  sum(response[idx], na.rm = TRUE)
}

# Construct parent/child shape evidence at several support multipliers while
# retaining each observed sibling mix and its fixed row-level context. The
# target is the parent response shape relative to the same mix at multiplier
# one. Stan receives the equivalent cross-multiplied residual in response units
# so no near-zero ratio is ever evaluated in the model.
econ_seq_collective_saturation_shape_reconciliation_input <- function(parent_fit,
                                                                       parent_layer,
                                                                       child_layer,
                                                                       time_col,
                                                                       training_times = NULL,
                                                                       max_draws = 200L,
                                                                       seed = 123L,
                                                                       support_multipliers = c(0.50, 1.50, 2.00),
                                                                       data_reuse_inflation = 1.5,
                                                                       child_heterogeneity_relative_sd = 0.50,
                                                                       mix_transfer_scale = 1,
                                                                       approximation_relative_sd = 0.15,
                                                                       sample_curve_parameters = c("always", "never")) {
  sample_curve_parameters <- match.arg(sample_curve_parameters)
  empty <- function() list(scenarios = data.table::data.table(), members = data.table::data.table(), parent_shape_cov = matrix(0, 0, 0), audit = data.table::data.table(), mix_selection = data.table::data.table())
  parent_map <- data.table::as.data.table(parent_layer$variable_mapping %||% data.table::data.table())
  child_map <- data.table::as.data.table(child_layer$variable_mapping %||% data.table::data.table())
  if (!all(c("variable", "generated_variable") %in% names(parent_map)) || !all(c("variable", "generated_variable") %in% names(child_map))) return(empty())
  relation <- merge(child_map[, .(variable, child_variable = generated_variable)], parent_map[, .(variable, parent_node = generated_variable)], by = "variable", all = FALSE, sort = FALSE)
  relation <- unique(relation[child_variable != parent_node], by = c("parent_node", "child_variable"))
  if (!nrow(relation)) return(empty())
  if (identical(sample_curve_parameters, "never")) {
    return(list(
      scenarios = data.table::data.table(),
      members = data.table::data.table(),
      parent_shape_cov = matrix(0, 0, 0),
      audit = unique(relation[, .(
        parent_node,
        reconciliation_mode = "excluded_fixed_curve_parameters",
        aggregate_shape_constraint_retained = FALSE,
        child_allocation_status = "not_applicable_fixed_curve"
      )], by = "parent_node"),
      mix_selection = data.table::data.table()
    ))
  }
  parent_data <- data.table::as.data.table(parent_fit$data)
  child_data <- econ_seq_input_table(child_layer$data, "child_layer$data")
  if (!is.null(training_times)) {
    parent_data <- parent_data[get(time_col) %in% training_times]
    child_data <- child_data[get(time_col) %in% training_times]
  }
  if (!nrow(parent_data) || !nrow(child_data)) return(empty())
  multipliers <- sort(unique(as.numeric(support_multipliers)))
  multipliers <- multipliers[is.finite(multipliers) & multipliers > 0 & abs(multipliers - 1) > 1e-8]
  if (!length(multipliers)) return(empty())
  draw_params <- extract_posterior_draw_params_hier_mmm(parent_fit, max_draws = max_draws, seed = seed)$params
  if (length(draw_params) < 10L) return(empty())
  scenarios <- list(); members <- list(); audit <- list(); mix_selection <- list()
  for (parent_node in unique(relation$parent_node)) {
    parent_node_value <- parent_node
    if (!(parent_node %in% names(parent_data))) next
    child_variables <- econ_seq_child_variables_for_parent(relation, parent_node_value)
    child_variables <- child_variables[child_variables %in% names(child_data)]
    child_meta <- data.table::as.data.table(child_layer$metadata)[variable %in% child_variables]
    rf_child <- child_meta[role %in% c("reach_frequency", "organic_reach_frequency"), variable]
    if (length(rf_child)) {
      audit[[length(audit) + 1L]] <- data.table::data.table(parent_node = parent_node, reconciliation_mode = "excluded_fixed_or_reach_frequency_curve", excluded_variable_n = length(rf_child), excluded_variables = paste(rf_child, collapse = " | "))
      next
    }
    if (length(child_variables) < 2L) next
    mix_panel <- child_data[, lapply(.SD, function(x) sum(pmax(as.numeric(x), 0), na.rm = TRUE)), by = time_col, .SDcols = child_variables]
    data.table::setnames(mix_panel, time_col, "time_value__")
    mix_mat <- as.matrix(mix_panel[, ..child_variables])
    total_support <- rowSums(mix_mat)
    keep_mix <- is.finite(total_support) & total_support > 1e-8
    mix_panel <- mix_panel[keep_mix]; mix_mat <- mix_mat[keep_mix, , drop = FALSE]; total_support <- total_support[keep_mix]
    if (nrow(mix_panel) < 3L) next
    shares <- mix_mat / pmax(total_support, 1e-8)
    support_rank <- rank(total_support, ties.method = "average") / (length(total_support) + 1)
    distance <- as.matrix(stats::dist(shares, method = "euclidean"))
    selected_idx <- integer()
    targets <- c(.50, .25, .75)
    for (target in targets) {
      candidates <- setdiff(seq_len(nrow(mix_panel)), selected_idx)
      support_score <- 1 - abs(support_rank[candidates] - target)
      diversity_score <- if (!length(selected_idx)) rep(0, length(candidates)) else apply(distance[candidates, selected_idx, drop = FALSE], 1, min)
      selected_idx <- c(selected_idx, candidates[which.max(.35 * support_score + .65 * diversity_score)])
    }
    mix_rows <- mix_panel[selected_idx]
    pairwise_distance <- if (length(selected_idx) > 1L) mean(distance[selected_idx, selected_idx][upper.tri(distance[selected_idx, selected_idx])]) else 0
    mix_churn <- if (nrow(shares) > 1L) mean(sqrt(rowSums((shares[-1, , drop = FALSE] - shares[-nrow(shares), , drop = FALSE])^2)), na.rm = TRUE) else 0
    if (!is.finite(mix_churn)) mix_churn <- 0
    sufficient_mix_variation <- is.finite(pairwise_distance) && pairwise_distance >= .05
    mix_selection[[length(mix_selection) + 1L]] <- data.table::data.table(parent_node = parent_node, mix_id = paste0(parent_node, "_mix", seq_along(selected_idx)), time_value = mix_rows$time_value__, total_support = total_support[selected_idx], sibling_shares = vapply(seq_along(selected_idx), function(ii) paste(paste0(child_variables, "=", format(round(shares[selected_idx[ii], ], 4), nsmall = 4)), collapse = " | "), character(1)), pairwise_mix_distance = pairwise_distance, sufficient_mix_variation = sufficient_mix_variation)
    parent_support <- parent_data[, .(support = sum(pmax(as.numeric(get(parent_node)), 0), na.rm = TRUE)), by = time_col]
    for (mm in seq_len(nrow(mix_rows))) {
      time_value <- mix_rows$time_value__[mm]
      draw_response <- sapply(draw_params, function(pm) {
        draw_id <- pm$draw_id; pm$draw_id <- NULL
        ref <- econ_seq_parent_shape_response_draw(parent_fit, parent_node, time_value, 1, pm)
        vals <- vapply(multipliers, function(mult) {
          val <- econ_seq_parent_shape_response_draw(parent_fit, parent_node, time_value, mult, pm)
          if (!is.finite(ref) || ref <= 1e-6 || !is.finite(val)) NA_real_ else val / ref
        }, numeric(1))
        c(reference_response = ref, vals)
      })
      if (is.null(dim(draw_response))) draw_response <- matrix(draw_response, ncol = 1L)
      reference_draws <- as.numeric(draw_response[1, ])
      draw_ratio <- draw_response[-1, , drop = FALSE]
      for (ss in seq_along(multipliers)) {
        raw_ratios <- as.numeric(draw_ratio[ss, ])
        valid <- is.finite(raw_ratios) & is.finite(reference_draws) & reference_draws > 1e-6
        ratios <- raw_ratios[valid]
        references <- reference_draws[valid]
        # A near-zero parent response cannot identify a stable shape target.
        if (length(ratios) < 10L || median(references) <= 1e-6) next
        sid <- paste0("collective_shape_", gsub("[^A-Za-z0-9]+", "_", parent_node), "_mix", mm, "_x", format(multipliers[ss], trim = TRUE))
        scenarios[[length(scenarios) + 1L]] <- data.table::data.table(
          reconciliation_id = sid, parent_node = parent_node,
          mix_id = paste0(parent_node, "_mix", mm), support_multiplier = multipliers[ss],
          parent_shape = mean(ratios), parent_shape_draw_n = length(ratios),
          parent_reference_response = mean(references),
          child_allocation_usable = sufficient_mix_variation,
          collective_constraint_scope = "aggregate_shape_only",
          parent_shape_draws__ = list(raw_ratios),
          parent_reference_draws__ = list(reference_draws)
        )
        members[[length(members) + 1L]] <- data.table::data.table(reconciliation_id = sid, variable = child_variables, time_value = time_value, multiplier = multipliers[ss])
      }
    }
    audit[[length(audit) + 1L]] <- data.table::data.table(
      parent_node = parent_node, observed_mix_n = nrow(mix_rows), support_multiplier_n = length(multipliers),
      media_mix_churn = mix_churn, pairwise_mix_distance = pairwise_distance,
      sufficient_mix_variation = sufficient_mix_variation,
      aggregate_shape_constraint_retained = TRUE,
      child_allocation_status = if (sufficient_mix_variation) "allocation_supported" else "unresolved_allocation",
      reconciliation_mode = "shape_ratio_observed_mix_scaled_support"
    )
  }
  scenario_dt <- data.table::rbindlist(scenarios, fill = TRUE)
  member_dt <- data.table::rbindlist(members, fill = TRUE)
  if (!nrow(scenario_dt)) return(empty())
  # Parent draws are common within a mix, so preserve their covariance. Extra
  # sequential uncertainty enters only the diagonal as softening, not as fake
  # independent evidence.
  draw_list <- scenario_dt$parent_shape_draws__
  reference_list <- scenario_dt$parent_reference_draws__
  draw_matrix <- do.call(cbind, draw_list)
  reference_matrix <- do.call(cbind, reference_list)
  complete_draws <- stats::complete.cases(draw_matrix) & stats::complete.cases(reference_matrix) &
    apply(reference_matrix > 1e-6, 1L, all)
  draw_matrix <- draw_matrix[complete_draws, , drop = FALSE]
  if (nrow(draw_matrix) < 5L) return(empty())
  cov_shape <- stats::cov(draw_matrix)
  if (is.null(dim(cov_shape))) cov_shape <- matrix(cov_shape, 1L, 1L)
  # Reuse inflates the full parent covariance, retaining its within-parent
  # correlation. Heterogeneity/mix/approximation are independent softening
  # components added after that covariance inflation.
  cov_shape <- cov_shape * max(1, data_reuse_inflation)^2
  mix_churn_by_parent <- data.table::rbindlist(audit, fill = TRUE)[match(scenario_dt$parent_node, parent_node), media_mix_churn]
  mix_churn_by_parent[!is.finite(mix_churn_by_parent)] <- 0
  # Convert ratio uncertainty into the response-unit residual used by Stan.
  # This preserves the shared-draw covariance without treating scenarios as
  # independent evidence.
  response_scale <- pmax(abs(scenario_dt$parent_reference_response), 1e-6)
  cov <- diag(response_scale, nrow(cov_shape)) %*% cov_shape %*% diag(response_scale, nrow(cov_shape))
  extra_sd <- response_scale * sqrt(
    max(0, child_heterogeneity_relative_sd)^2 +
      (pmax(mix_churn_by_parent, 0) * max(0, mix_transfer_scale))^2 +
      max(0, approximation_relative_sd)^2
  )
  cov <- cov + diag(pmax(extra_sd, .02)^2, nrow(cov))
  scenario_dt[, c("parent_shape_draws__", "parent_reference_draws__") := NULL]
  list(scenarios = scenario_dt[], members = member_dt[], parent_shape_cov = cov,
       audit = data.table::rbindlist(audit, fill = TRUE), mix_selection = data.table::rbindlist(mix_selection, fill = TRUE))
}

econ_seq_curvature_share <- function(child_deviation, aggregate_deviation, tolerance = 1e-8) {
  child_deviation <- as.numeric(child_deviation)
  aggregate_deviation <- as.numeric(aggregate_deviation)[1]
  if (!is.finite(aggregate_deviation) || abs(aggregate_deviation) <= tolerance) {
    return(rep(NA_real_, length(child_deviation)))
  }
  child_deviation / aggregate_deviation
}

econ_seq_collective_saturation_shape_reconciliation_audit <- function(child_fit,
                                                                       reconciliation_input,
                                                                       max_draws = 200L,
                                                                       seed = 123L) {
  empty <- function() list(scenarios = data.table::data.table(), children = data.table::data.table())
  if (is.null(reconciliation_input) || !nrow(reconciliation_input$scenarios %||% data.table::data.table())) return(empty())
  scenarios <- data.table::as.data.table(reconciliation_input$scenarios)
  members <- data.table::as.data.table(reconciliation_input$members)
  fit_data <- data.table::as.data.table(child_fit$data)
  time_col <- child_fit$time_col
  holdout_row <- if ("is_holdout__" %in% names(fit_data)) as.logical(fit_data$is_holdout__) else rep(FALSE, nrow(fit_data))
  holdout_row[is.na(holdout_row)] <- FALSE
  draw_params <- extract_posterior_draw_params_hier_mmm(child_fit, max_draws = max_draws, seed = seed)$params
  if (!length(draw_params)) return(empty())
  results <- lapply(seq_len(nrow(scenarios)), function(ii) {
    sc <- scenarios[ii]
    mm <- members[reconciliation_id == sc$reconciliation_id]
    if (!nrow(mm)) return(list(aggregate = data.table::data.table(), children = data.table::data.table()))
    per_draw <- lapply(draw_params, function(pm) {
      draw_id <- as.character(pm$draw_id %||% ""); pm$draw_id <- NULL
      child_vals <- lapply(seq_len(nrow(mm)), function(rr) {
        v <- mm$variable[rr]
        idx <- which(as.character(fit_data[[time_col]]) == as.character(mm$time_value[rr]) & !holdout_row)
        time_levels <- unique(as.character(fit_data[[time_col]]))
        target_period <- match(as.character(mm$time_value[rr]), time_levels)
        history_idx <- which(match(as.character(fit_data[[time_col]]), time_levels) <= target_period)
        tmp <- child_fit; tmp$data <- data.table::copy(fit_data)
        # Same complete-history perturbation as the Stan shape likelihood.
        tmp$data[history_idx, (v) := pmax(as.numeric(get(v)) * as.numeric(mm$multiplier[rr]), 0)]
        scenario_x <- variable_contribution_rows_hier_mmm(tmp, v, posterior_params = pm)
        reference_x <- variable_contribution_rows_hier_mmm(child_fit, v, posterior_params = pm)
        data.table::data.table(variable = v, scenario_contribution = sum(scenario_x[idx], na.rm = TRUE), reference_contribution = sum(reference_x[idx], na.rm = TRUE))
      })
      child_dt <- data.table::rbindlist(child_vals)
      reference <- sum(child_dt$reference_contribution)
      scenario <- sum(child_dt$scenario_contribution)
      child_dt[, `:=`(
        reconciliation_id = sc$reconciliation_id, parent_node = sc$parent_node,
        mix_id = sc$mix_id, support_multiplier = sc$support_multiplier, draw = draw_id,
        nonlinear_deviation = scenario_contribution - as.numeric(sc$support_multiplier) * reference_contribution,
        aggregate_nonlinear_deviation = scenario - as.numeric(sc$support_multiplier) * reference,
        nonlinear_deviation_share = econ_seq_curvature_share(
          scenario_contribution - as.numeric(sc$support_multiplier) * reference_contribution,
          scenario - as.numeric(sc$support_multiplier) * reference
        )
      )]
      list(
        aggregate = data.table::data.table(reconciliation_id = sc$reconciliation_id, draw = draw_id,
          child_shape = if (is.finite(reference) && abs(reference) > 1e-8) scenario / reference else NA_real_,
          aggregate_child_response = scenario, aggregate_reference_response = reference),
        children = child_dt
      )
    })
    list(aggregate = data.table::rbindlist(lapply(per_draw, `[[`, "aggregate"), fill = TRUE),
         children = data.table::rbindlist(lapply(per_draw, `[[`, "children"), fill = TRUE))
  })
  draws <- data.table::rbindlist(lapply(results, `[[`, "aggregate"), fill = TRUE)
  child_draws <- data.table::rbindlist(lapply(results, `[[`, "children"), fill = TRUE)
  if (!nrow(draws)) return(empty())
  summary <- draws[, .(
    aggregate_child_response_q05 = stats::quantile(aggregate_child_response, .05, na.rm = TRUE),
    aggregate_child_response_q50 = stats::quantile(aggregate_child_response, .50, na.rm = TRUE),
    aggregate_child_response_q95 = stats::quantile(aggregate_child_response, .95, na.rm = TRUE),
    normalized_aggregate_shape_q05 = stats::quantile(child_shape, .05, na.rm = TRUE),
    normalized_aggregate_shape_q50 = stats::quantile(child_shape, .50, na.rm = TRUE),
    normalized_aggregate_shape_q95 = stats::quantile(child_shape, .95, na.rm = TRUE),
    posterior_draw_n = data.table::uniqueN(draw)
  ), by = reconciliation_id]
  summary[scenarios, on = "reconciliation_id", `:=`(
    parent_node = i.parent_node, mix_id = i.mix_id, support_multiplier = i.support_multiplier,
    parent_shape = i.parent_shape
  )]
  # Parent covariance is supplied separately; keep its diagonal auditable.
  summary[, parent_shape_sd := sqrt(diag(reconciliation_input$parent_shape_cov)[match(reconciliation_id, scenarios$reconciliation_id)])]
  summary[, `:=`(
    parent_vs_child_shape_error = normalized_aggregate_shape_q50 - parent_shape,
    absolute_shape_error = abs(normalized_aggregate_shape_q50 - parent_shape),
    relative_shape_error = abs(normalized_aggregate_shape_q50 - parent_shape) / pmax(abs(parent_shape), 1e-8),
    reconciliation_covered = parent_shape >= normalized_aggregate_shape_q05 & parent_shape <= normalized_aggregate_shape_q95
  )]
  child_summary <- child_draws[, .(
    reference_contribution_q05 = stats::quantile(reference_contribution, .05, na.rm = TRUE),
    reference_contribution_q50 = stats::quantile(reference_contribution, .50, na.rm = TRUE),
    reference_contribution_q95 = stats::quantile(reference_contribution, .95, na.rm = TRUE),
    scenario_contribution_q05 = stats::quantile(scenario_contribution, .05, na.rm = TRUE),
    scenario_contribution_q50 = stats::quantile(scenario_contribution, .50, na.rm = TRUE),
    scenario_contribution_q95 = stats::quantile(scenario_contribution, .95, na.rm = TRUE),
    nonlinear_deviation_q05 = stats::quantile(nonlinear_deviation, .05, na.rm = TRUE),
    nonlinear_deviation_q50 = stats::quantile(nonlinear_deviation, .50, na.rm = TRUE),
    nonlinear_deviation_q95 = stats::quantile(nonlinear_deviation, .95, na.rm = TRUE),
    aggregate_nonlinear_curvature_share_q05 = stats::quantile(nonlinear_deviation_share, .05, na.rm = TRUE),
    aggregate_nonlinear_curvature_share_q50 = stats::quantile(nonlinear_deviation_share, .50, na.rm = TRUE),
    aggregate_nonlinear_curvature_share_q95 = stats::quantile(nonlinear_deviation_share, .95, na.rm = TRUE)
  ), by = .(reconciliation_id, parent_node, mix_id, support_multiplier, variable)]
  prior_audit <- data.table::as.data.table(child_fit$sequential_prior_posterior_audit %||% data.table::data.table())
  if (nrow(prior_audit) && "variable" %in% names(prior_audit)) {
    for (cc in c("prior_dominance_classification", "child_identification_score",
                 "posterior_movement_prior_sd_units", "posterior_to_prior_sd_ratio")) {
      if (!cc %in% names(prior_audit)) prior_audit[, (cc) := NA]
    }
    pa <- prior_audit[, .(
      variable,
      measured_prior_dominance = as.character(prior_dominance_classification),
      child_identification_strength_0_1 = as.numeric(child_identification_score),
      posterior_movement_prior_sd_units = as.numeric(posterior_movement_prior_sd_units),
      posterior_to_prior_sd_ratio = as.numeric(posterior_to_prior_sd_ratio)
    )]
    child_summary[pa, on = "variable", `:=`(
      measured_prior_dominance = i.measured_prior_dominance,
      child_identification_strength_0_1 = i.child_identification_strength_0_1,
      posterior_movement_prior_sd_units = i.posterior_movement_prior_sd_units,
      posterior_to_prior_sd_ratio = i.posterior_to_prior_sd_ratio
    )]
  } else {
    child_summary[, `:=`(measured_prior_dominance = "not_available", child_identification_strength_0_1 = NA_real_,
                         posterior_movement_prior_sd_units = NA_real_, posterior_to_prior_sd_ratio = NA_real_)]
  }
  mix_selection <- data.table::as.data.table(reconciliation_input$mix_selection %||% data.table::data.table())
  if (nrow(mix_selection) && all(c("parent_node", "mix_id", "sufficient_mix_variation") %in% names(mix_selection))) {
    mix_status <- unique(mix_selection[, .(parent_node, mix_id, sufficient_mix_variation)])
    child_summary[mix_status, on = c("parent_node", "mix_id"), sufficient_mix_variation := i.sufficient_mix_variation]
  }
  if (!"sufficient_mix_variation" %in% names(child_summary)) child_summary[, sufficient_mix_variation := FALSE]
  child_summary[is.na(sufficient_mix_variation), sufficient_mix_variation := FALSE]
  child_summary[, collective_sensitivity_status := "not_measured_requires_no_collective_refit"]
  identification_data_driven_min <- econ_seq_identification_calibration()$data_driven_min
  child_summary[, saturation_evidence_class := data.table::fcase(
    !sufficient_mix_variation, "unresolved_allocation",
    grepl("prior_driven", measured_prior_dominance), "generic_prior_dominant",
    is.finite(child_identification_strength_0_1) & child_identification_strength_0_1 >= identification_data_driven_min &
      is.finite(posterior_movement_prior_sd_units) & abs(posterior_movement_prior_sd_units) >= .75 &
      is.finite(posterior_to_prior_sd_ratio) & posterior_to_prior_sd_ratio <= .80 &
      is.finite(nonlinear_deviation_q05) & is.finite(nonlinear_deviation_q95) &
      abs(nonlinear_deviation_q50) > .5 * (nonlinear_deviation_q95 - nonlinear_deviation_q05), "child_data_dominant",
    is.finite(nonlinear_deviation_q05) & is.finite(nonlinear_deviation_q95), "collective_constraint_informed",
    default = "unresolved_allocation"
  )]
  list(scenarios = summary[], children = child_summary[])
}

econ_seq_sequential_prior_posterior_audit <- function(prior_table,
                                                       fit_obj = NULL,
                                                       layer = NULL,
                                                       training_times = NULL,
                                                       max_draws = 200L,
                                                       seed = 123L) {
  priors <- data.table::copy(data.table::as.data.table(prior_table))
  if (!nrow(priors)) return(data.table::data.table())
  for (cc in c(
    "sequential_parent_sd", "sequential_root_sd", "sequential_root_data_reuse_inflation",
    "sequential_child_heterogeneity_sd_component", "child_identification_strength_0_1",
    "parent_shrinkage_multiplier"
  )) if (!cc %in% names(priors)) priors[, (cc) := NA_real_]
  if (!"prior_dominance_classification" %in% names(priors)) {
    priors[, prior_dominance_classification := "not_classified"]
  }
  priors[, parent_uncertainty__ := data.table::fcoalesce(sequential_parent_sd, sequential_root_sd)]
  out <- priors[, .(
    variable,
    parent_prior_center = prior_mean,
    parent_uncertainty = parent_uncertainty__,
    data_reuse_inflation = sequential_root_data_reuse_inflation,
    child_heterogeneity_allowance = sequential_child_heterogeneity_sd_component,
    child_identification_score = child_identification_strength_0_1,
    final_shrinkage_multiplier = parent_shrinkage_multiplier,
    final_prior_sd = prior_sd,
    final_prior_precision = 1 / prior_sd^2,
    prior_dominance_classification,
    posterior_effectiveness = NA_real_,
    posterior_effectiveness_sd = NA_real_,
    posterior_movement_away_from_prior = NA_real_,
    posterior_movement_prior_sd_units = NA_real_,
    posterior_to_prior_sd_ratio = NA_real_
  )]
  if (is.null(fit_obj) || is.null(layer)) return(out[])
  posterior <- tryCatch(
    econ_seq_parent_effectiveness_draws(
      fit_obj, layer, training_times = training_times, max_draws = max_draws, seed = seed
    )$summary,
    error = function(e) data.table::data.table()
  )
  if (!nrow(posterior)) return(out[])
  posterior <- posterior[, .(
    variable,
    posterior_effectiveness__ = parent_effectiveness,
    posterior_effectiveness_sd__ = parent_effectiveness_sd
  )]
  out[posterior, on = "variable", `:=`(
    posterior_effectiveness = i.posterior_effectiveness__,
    posterior_effectiveness_sd = i.posterior_effectiveness_sd__
  )]
  out[, `:=`(
    posterior_movement_away_from_prior = posterior_effectiveness - parent_prior_center,
    posterior_movement_prior_sd_units = (posterior_effectiveness - parent_prior_center) / pmax(final_prior_sd, 1e-8),
    posterior_to_prior_sd_ratio = posterior_effectiveness_sd / pmax(final_prior_sd, 1e-8)
  )]
  out[is.finite(posterior_movement_prior_sd_units), prior_dominance_classification := data.table::fcase(
    grepl("user_prior", prior_dominance_classification), "user_prior_driven",
    abs(posterior_movement_prior_sd_units) <= 0.25 & posterior_to_prior_sd_ratio >= 0.75,
      data.table::fifelse(grepl("default", prior_dominance_classification), "default_prior_driven", "parent_prior_driven"),
    abs(posterior_movement_prior_sd_units) >= 0.75, "data_updated_away_from_prior",
    default = "parent_prior_and_data_blended"
  )]
  out[]
}

#' Build child effectiveness priors from a fitted sequential parent layer.
#'
#' This is the posterior-to-prior handoff used after a fitted intermediate
#' rollup layer. Parent effectiveness comes from total-scope response-curve
#' posterior draws at the observed spend point, then is widened for data reuse,
#' child heterogeneity, and media-mix transfer risk.
build_sequential_effectiveness_priors_from_parent_fit <- function(parent_fit,
                                                                   parent_layer,
                                                                   child_layer,
                                                                   time_col,
                                                                   training_times = NULL,
                                                                   child_prior_overrides = NULL,
                                                                   data_reuse_inflation = 1.5,
                                                                   child_heterogeneity_relative_sd = 0.50,
                                                                   mix_transfer_scale = 1,
                                                                   minimum_relative_sd = 0.35,
                                                                   child_identification = NULL,
                                                                   strong_child_prior_relaxation = 1.20,
                                                                   parent_draw_count = 200L,
                                                                   parent_draw_seed = 123L) {
  parent_depth <- suppressWarnings(as.integer(parent_layer$rollup_depth)[1])
  child_depth <- suppressWarnings(as.integer(child_layer$rollup_depth)[1])
  child_is_leaf <- isTRUE(child_layer$is_leaf_layer) || identical(as.character(child_layer$layer_key %||% ""), "leaf")
  if (!is.finite(parent_depth) || parent_depth < 1L) stop("parent_layer must be a positive numeric rollup depth.", call. = FALSE)
  if (!child_is_leaf && (!is.finite(child_depth) || child_depth <= parent_depth)) {
    stop("child_layer must be deeper than parent_layer for posterior sequential transfer.", call. = FALSE)
  }
  parent_mapping <- data.table::as.data.table(parent_layer$variable_mapping)
  child_mapping <- data.table::as.data.table(child_layer$variable_mapping)
  required_parent <- c("variable", "generated_variable")
  required_child <- c("variable", "rollup_node_path", "generated_variable")
  if (!all(required_parent %in% names(parent_mapping)) || !all(required_child %in% names(child_mapping))) {
    stop("parent_layer/child_layer do not contain the sequential rollup contract required for a posterior handoff.", call. = FALSE)
  }
  if (!"decomposition_eligible" %in% names(parent_mapping)) parent_mapping[, decomposition_eligible := TRUE]
  parent_map <- unique(parent_mapping[, .(
    variable,
    sequential_parent_id = generated_variable,
    parent_decomposition_eligible = decomposition_eligible
  )], by = "variable")
  child_map <- child_mapping[, .(variable, child_rollup_node_path = rollup_node_path, child_variable = generated_variable)]
  relationship <- merge(child_map, parent_map, by = "variable", all.x = TRUE, sort = FALSE)
  if (any(is.na(relationship$sequential_parent_id))) {
    bad <- unique(relationship[is.na(sequential_parent_id), variable])
    stop("Could not map child media to a fitted parent node for: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  child_parent <- relationship[, .(parent_n = data.table::uniqueN(sequential_parent_id),
                                   sequential_parent_id = unique(sequential_parent_id)[1],
                                   parent_decomposition_eligible = all(parent_decomposition_eligible %in% TRUE)),
                               by = .(child_variable, child_rollup_node_path)]
  if (any(child_parent$parent_n > 1L)) {
    bad <- child_parent[parent_n > 1L, child_variable]
    stop("A child rollup node crosses multiple fitted parents: ", paste(bad, collapse = ", "),
         ". Define the media graph so each child has one parent at the selected depth.", call. = FALSE)
  }
  parent_draw_evidence <- econ_seq_parent_effectiveness_draws(
    parent_fit = parent_fit,
    parent_layer = parent_layer,
    training_times = training_times,
    max_draws = parent_draw_count,
    seed = parent_draw_seed
  )
  parent_evidence <- parent_draw_evidence$summary
  data.table::setnames(parent_evidence, "variable", "sequential_parent_id")
  child_parent <- merge(child_parent, parent_evidence, by = "sequential_parent_id", all.x = TRUE, sort = FALSE)
  numeric_curve_cols <- c(
    "rrate_prior_mean", "rrate_prior_sd", "rrate_prior_precision",
    "anchor_saturation_prior_mean", "anchor_saturation_prior_sd", "anchor_saturation_prior_precision"
  )
  for (cc in numeric_curve_cols) if (!cc %in% names(child_parent)) child_parent[, (cc) := NA_real_]
  for (cc in c("rrate_prior_source", "anchor_saturation_prior_source")) {
    if (!cc %in% names(child_parent)) child_parent[, (cc) := NA_character_]
  }
  child_parent[, curve_prior_available :=
                 is.finite(rrate_prior_mean) & is.finite(rrate_prior_precision) & rrate_prior_precision > 0]
  child_parent[, saturation_prior_available :=
                 is.finite(anchor_saturation_prior_mean) &
                   is.finite(anchor_saturation_prior_precision) & anchor_saturation_prior_precision > 0]
  child_parent[, curve_prior_mode := data.table::fifelse(
    curve_prior_available,
    "sequential_parent_stan_posterior",
    "generic_default_no_parent_curve_draws"
  )]
  child_parent[curve_prior_available == FALSE, rrate_prior_source := "no_parent_curve_evidence_generic_metadata_preserved"]
  child_parent[saturation_prior_available == FALSE, anchor_saturation_prior_source := "no_parent_saturation_evidence_generic_metadata_preserved"]
  if (any(!is.finite(child_parent$parent_effectiveness))) {
    bad <- child_parent[!is.finite(parent_effectiveness), child_variable]
    stop("No posterior effectiveness evidence is available for parent of: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  child_spend_map <- data.table::as.data.table(child_layer$spend_map)
  child_parent <- merge(child_parent, child_spend_map[, .(child_variable = variable, child_spend_col = spend_col)],
                        by = "child_variable", all.x = TRUE, sort = FALSE)
  if (any(is.na(child_parent$child_spend_col) | !nzchar(child_parent$child_spend_col))) {
    stop("Child rollup layer is missing an observed spend mapping.", call. = FALSE)
  }
  child_data <- econ_seq_input_table(child_layer$data, "child_layer$data")
  if (!is.null(training_times)) child_data <- child_data[get(time_col) %in% training_times]
  if (!nrow(child_data)) stop("No training rows remain for sequential continuation priors.", call. = FALSE)
  child_parent[, child_spend_total := vapply(child_spend_col, function(cc) {
    z <- suppressWarnings(as.numeric(child_data[[cc]]))
    sum(z[is.finite(z)], na.rm = TRUE)
  }, numeric(1))]
  data_reuse_inflation <- max(1, as.numeric(data_reuse_inflation)[1])
  child_heterogeneity_relative_sd <- max(0, as.numeric(child_heterogeneity_relative_sd)[1])
  mix_transfer_scale <- max(0, as.numeric(mix_transfer_scale)[1])
  minimum_relative_sd <- max(0, as.numeric(minimum_relative_sd)[1])
  child_mix <- econ_seq_mix_diagnostics(child_data, child_spend_map, time_col = time_col)$summary
  mix_churn <- if (nrow(child_mix)) as.numeric(child_mix$media_mix_churn[1]) else 0
  if (!is.finite(mix_churn) || mix_churn < 0) mix_churn <- 0
  child_parent[, `:=`(
    sequential_parent_sd_component = parent_effectiveness_sd * data_reuse_inflation,
    sequential_child_heterogeneity_sd_component = abs(parent_effectiveness) * child_heterogeneity_relative_sd,
    sequential_mix_sd_component = abs(parent_effectiveness) * mix_churn * mix_transfer_scale
  )]
  child_parent[, prior_sd := sqrt(
    sequential_parent_sd_component^2 +
      sequential_child_heterogeneity_sd_component^2 +
      sequential_mix_sd_component^2
  )]
  child_parent[, prior_sd := pmax(prior_sd, abs(parent_effectiveness) * minimum_relative_sd, 1e-8)]
  child_parent[, parent_positive_effect_transferred := parent_effectiveness_status == "positive_transferable"]
  child_parent[parent_positive_effect_transferred == FALSE,
               prior_sd := pmax(prior_sd, sequential_parent_sd_component * 2, abs(parent_effectiveness), 1e-8)]
  total_child_spend <- sum(child_parent$child_spend_total)
  out <- child_parent[, .(
    variable = child_variable,
    prior_metric = "ikpc",
    prior_mean = ifelse(parent_positive_effect_transferred, parent_effectiveness, 0),
    prior_sd = prior_sd,
    prior_uncertainty_basis = "sd",
    prior_distribution = "normal",
    evidence_source = ifelse(parent_positive_effect_transferred, "sequential_parent_posterior_response_curve", "sequential_parent_posterior_weak_neutral"),
    evidence_notes = ifelse(parent_positive_effect_transferred,
                            "Parent effectiveness is the posterior outcome-per-cost at the observed spend point; uncertainty is widened before child transfer.",
                            "Parent effect is negative or near zero/inconclusive for a positive paid-media child effect. The child prior is neutral and deliberately wide."),
    sequential_parent_id,
    sequential_transfer_mode = ifelse(parent_positive_effect_transferred, "parent_posterior_effectiveness", "weak_neutral_no_positive_transfer"),
    sequential_parent_rollup_depth = parent_depth,
    sequential_child_rollup_depth = if (child_is_leaf) NA_integer_ else child_depth,
    sequential_child_layer = if (child_is_leaf) "leaf" else paste0("depth_", child_depth),
    sequential_parent_mean = parent_effectiveness,
    sequential_parent_sd = parent_effectiveness_sd,
    sequential_parent_effectiveness_status = parent_effectiveness_status,
    parent_decomposition_eligible,
    curve_prior_available,
    saturation_prior_available,
    curve_prior_mode,
    rrate_prior_mean,
    rrate_prior_sd,
    rrate_prior_precision,
    rrate_prior_source,
    anchor_saturation_prior_mean,
    anchor_saturation_prior_sd,
    anchor_saturation_prior_precision,
    anchor_saturation_prior_source,
    rrate_pooling_mode = "shared_parent_regularization_no_latent_sibling_pool",
    parent_positive_effect_transferred,
    sequential_parent_draw_n = parent_draw_n,
    sequential_parent_current_contribution = parent_current_contribution,
    sequential_parent_current_spend = parent_current_spend,
    sequential_parent_sd_component,
    sequential_child_heterogeneity_sd_component,
    sequential_mix_sd_component,
    sequential_media_mix_churn = mix_churn,
    sequential_spend_col = child_spend_col,
    child_spend_total,
    child_spend_share = child_spend_total / total_child_spend,
    implied_child_contribution_mean = ifelse(parent_positive_effect_transferred, parent_effectiveness, 0) * child_spend_total,
    prior_evidence_mode = ifelse(parent_positive_effect_transferred, "parent_posterior_regularized", "weak_neutral_parent_evidence")
  )]
  out[parent_positive_effect_transferred == FALSE, `:=`(
    curve_prior_available = FALSE,
    saturation_prior_available = FALSE,
    curve_prior_mode = "generic_default_weak_or_negative_parent_effect",
    rrate_prior_precision = NA_real_,
    anchor_saturation_prior_precision = NA_real_
  )]
  rollup <- econ_seq_rollup_map(child_layer$metadata, out$variable, rollup_map = child_layer$rollup_map)
  out[rollup, on = "variable", `:=`(
    rollup_path = i.rollup_path,
    rollup_root = i.rollup_root,
    rollup_parent = i.rollup_parent,
    rollup_leaf = i.rollup_leaf
  )]
  out[!is.finite(child_spend_share), child_spend_share := NA_real_]
  out <- econ_seq_apply_branch_diagnostics(
    prior_table = out,
    child_identification = child_identification,
    child_prior_overrides = child_prior_overrides,
    strong_child_prior_relaxation = strong_child_prior_relaxation
  )
  out[, `:=`(
    parent_uncertainty_width_multiplier = 1,
    diagnostic_prior_width_multiplier = 1,
    sequential_prior_application = "joint_reference_spend_calibration"
  )]
  out[, prior_sd := prior_sd * parent_uncertainty_width_multiplier / sqrt(child_identification_pooling_multiplier)]
  out[parent_positive_effect_transferred == FALSE,
      prior_sd := pmax(prior_sd, sequential_parent_sd_component, abs(sequential_parent_mean), 1e-8)]
  out[, `:=`(
    rrate_prior_precision = data.table::fifelse(
      curve_prior_available & is.finite(rrate_prior_precision),
      rrate_prior_precision * child_identification_pooling_multiplier,
      NA_real_
    ),
    anchor_saturation_prior_precision = data.table::fifelse(
      saturation_prior_available & is.finite(anchor_saturation_prior_precision),
      anchor_saturation_prior_precision * child_identification_pooling_multiplier,
      NA_real_
    )
  )]
  out <- econ_seq_apply_child_prior_overrides(out, child_prior_overrides)
  parent_reconciliation <- out[, .(
    parent_current_spend = unique(sequential_parent_current_spend)[1],
    parent_current_contribution = unique(sequential_parent_current_contribution)[1],
    child_spend_total = sum(child_spend_total),
    child_prior_implied_contribution = sum(implied_child_contribution_mean),
    reconciliation_difference = unique(sequential_parent_current_contribution)[1] - sum(implied_child_contribution_mean),
    parent_positive_effect_transferred = unique(parent_positive_effect_transferred)[1]
  ), by = sequential_parent_id]
  ledger <- data.table::copy(out)
  ledger[, `:=`(
    generated_prior = TRUE,
    parent_evidence_type = "stan_posterior_response_curve_draws",
    parent_fit_method = "joint_stan_mmm",
    prior_precision = 1 / prior_sd^2,
    prior_audit_note = "Staged empirical-Bayes handoff from fitted parent posterior draws. Effectiveness is applied through a joint reference-spend calibration likelihood; adstock and pooled saturation remain estimable."
  )]
  list(
    business_priors = out[],
    prior_ledger = ledger[],
    reference_calibration_input = econ_seq_reference_effectiveness_calibration(out, calibration_prefix = "sequential_parent_effectiveness"),
    branch_decisions = out[, .(variable, fit_status, branch_decision, branch_decision_reason,
                                child_identification_recommendation, child_identification_pooling_multiplier,
                                child_identification_strength_0_1, parent_shrinkage_multiplier,
                                user_prior_override_present, user_prior_override_valid,
                                override_validation_reason, prior_dominance_classification)],
    parent_effectiveness = parent_evidence[],
    parent_roi_aggregation_audit = parent_draw_evidence$roi_aggregation_audit[],
    parent_child_mapping = relationship[],
    reconciliation_audit = parent_reconciliation[]
  )
}

#' Run the opt-in sequential hierarchical MMM workflow.
#'
#' Phase 1 fits the total-paid-media frequentist root, creates auditable
#' equal-effectiveness priors for either media leaves or one selected declared
#' spend-rollup depth, and can pass them directly to the existing joint Stan
#' child model. Intermediate graph depths are optional. A fitted numeric stage
#' can be continued with continue_sequential_hierarchical_bayes(), which uses
#' parent response-curve posterior draws for the deeper handoff. Effectiveness
#' is applied through a soft, spend-weighted aggregate effectiveness constraint
#' by default, with learned sibling dispersion. Inherited adstock and
#' saturation remain estimable. `reference_calibration` and a fixed-dispersion
#' aggregate reconciliation remain explicit alternative modes.
run_sequential_hierarchical_bayes <- function(data,
                                              metadata_input,
                                              dep_var_col,
                                              group_col,
                                              time_col,
                                              entity_col,
                                              spend_map = NULL,
                                              media_scope_config = NULL,
                                              population_col = NULL,
                                              market_size_col = NULL,
                                              target_population_col = NULL,
                                              invalid_allocation_fallback = c("error", "equal"),
                                              root_pressure_scaling = c("auto", "none", "per_capita"),
                                              root_pressure_col = NULL,
                                              child_variables = NULL,
                                              rollup_map = NULL,
                                              rollup_depth = "leaf",
                                              rollup_short_path_action = c("leaf", "error"),
                                              curve_type_default = c("hill", "weibull"),
                                              root_control_cols = NULL,
                                              root_control_mode = c("declared_controls", "all_nonmedia", "none"),
                                              root_scope = c("national", "hierarchical_panel"),
                                              kpi_aggregation_rule = "sum",
                                              kpi_weight_col = NULL,
                                              root_control_aggregation = NULL,
                                              root_aggregation_functions = list(),
                                              incomplete_period_action = c("error", "drop"),
                                              root_trend_spec = c("none", "linear"),
                                              root_fourier_harmonics = 2L,
                                              root_season_period = 52L,
                                              root_time_baseline = c("fourier", "knots", "auto"),
                                              root_knot_n = 6L,
                                              root_knot_penalty = 1,
                                              root_geo_media_effect = c("shared", "partially_pooled"),
                                              root_media_transform = c("adstock_hill", "linear"),
                                              root_rrate_grid = c(0, 0.25, 0.50, 0.70),
                                              root_anchor_saturation_grid = c(0.30, 0.50, 0.70),
                                              root_curve_min_delta_aicc = 2,
                                              root_effect_sign = c("positive", "unconstrained", "negative"),
                                              root_nonlinear_starts = 24L,
                                              root_rrate_bounds = c(0, 0.95),
                                              root_half_saturation_multiple_bounds = c(0.05, 10),
                                              root_steepness_bounds = c(0.25, 5),
                                              root_optimizer_maxit = 300L,
                                              root_bootstrap_reps = 200L,
                                              root_block_length = 4L,
                                              root_effect_prior = NULL,
                                              baseline_spec = NULL,
                                              allow_baseline_override = FALSE,
                                              holdout_col = NULL,
                                              holdout_value = TRUE,
                                              holdout_last_n = 0L,
                                              child_prior_overrides = NULL,
                                              data_reuse_inflation = 1.5,
                                              # Root total-media is the broadest handoff; retain a wider
                                              # prior scale for sibling deviation than later parent-child splits.
                                              child_heterogeneity_relative_sd = 0.75,
                                              mix_transfer_scale = 1,
                                              minimum_relative_sd = 0.50,
                                              strong_child_prior_relaxation = 1.20,
                                              curve_transfer_mode = c("effectiveness_adstock_saturation", "effectiveness_adstock", "effectiveness_only"),
                                              saturation_handoff = c("generic_child_prior", "collective_parent_shape_reconciliation", "independent_parent_prior"),
                                              # Learned tau is the default: sibling dispersion is estimated
                                              # hierarchically.  The fixed option is a transparent sensitivity
                                              # mode for difficult geometry or a deliberately fixed transfer width.
                                              sequential_effectiveness_application = c("hierarchical_tau", "fixed_aggregate_reconciliation", "reference_calibration", "coefficient_approximation"),
                                              # Parent-informed independent priors preserve child-specific
                                              # learning without adding a shared-tau funnel to the child fit.
                                              sequential_adstock_application = c("independent_prior", "hierarchical_tau"),
                                              sequential_tau_overrides = NULL,
                                              fit_child = FALSE,
                                              child_fit_args = list(),
                                              output_dir = NULL,
                                              output_prefix = "sequential",
                                              seed = 123L) {
  root_control_mode <- match.arg(root_control_mode)
  root_scope <- match.arg(root_scope)
  rollup_short_path_action <- match.arg(rollup_short_path_action)
  curve_type_default <- match.arg(curve_type_default)
  root_media_transform <- match.arg(root_media_transform)
  root_effect_sign <- match.arg(root_effect_sign)
  incomplete_period_action <- match.arg(incomplete_period_action)
  invalid_allocation_fallback <- match.arg(invalid_allocation_fallback)
  root_trend_spec <- match.arg(root_trend_spec)
  root_pressure_scaling <- match.arg(root_pressure_scaling)
  root_time_baseline <- match.arg(root_time_baseline)
  root_geo_media_effect <- match.arg(root_geo_media_effect)
  if (isTRUE(fit_child) && identical(root_time_baseline, "knots") && !isTRUE(allow_baseline_override)) {
    stop(
      "root_time_baseline = 'knots' is currently a root-only frequentist basis and cannot be represented by the shared Stan child baseline. Use root_time_baseline = 'fourier' for a matched sequential child fit, or set allow_baseline_override = TRUE for an explicitly audited difference.",
      call. = FALSE
    )
  }
  if (isTRUE(fit_child) && identical(root_time_baseline, "auto") && identical(root_scope, "hierarchical_panel") && !isTRUE(allow_baseline_override)) {
    stop(
      "root_time_baseline = 'auto' selects knots for a geo-panel root, which is not yet the shared Stan child baseline. Use 'fourier' for a matched sequential child fit, or set allow_baseline_override = TRUE for an explicitly audited difference.",
      call. = FALSE
    )
  }
  sequential_effectiveness_application <- match.arg(sequential_effectiveness_application)
  sequential_adstock_application <- match.arg(sequential_adstock_application)
  curve_transfer_mode <- match.arg(curve_transfer_mode)
  saturation_handoff <- match.arg(saturation_handoff)
  if (identical(sequential_adstock_application, "hierarchical_tau") && identical(saturation_handoff, "independent_parent_prior")) {
    stop("hierarchical_tau adstock cannot be combined with independent_parent_prior saturation; use generic_child_prior or collective_parent_shape_reconciliation.", call. = FALSE)
  }
  if (!is.list(child_fit_args)) stop("child_fit_args must be a list.", call. = FALSE)
  holdout_contract <- list(
    holdout_col = holdout_col,
    holdout_value = holdout_value,
    holdout_last_n = as.integer(holdout_last_n %||% 0L)
  )
  for (nm in names(holdout_contract)) {
    if (!is.null(child_fit_args[[nm]]) && !identical(child_fit_args[[nm]], holdout_contract[[nm]])) {
      stop("child_fit_args$", nm, " conflicts with the sequential holdout contract.", call. = FALSE)
    }
    child_fit_args[[nm]] <- holdout_contract[[nm]]
  }
  baseline_contract <- econ_seq_baseline_contract(
    root_trend_spec = root_trend_spec,
    root_fourier_harmonics = root_fourier_harmonics,
    root_season_period = root_season_period,
    control_cols = root_control_cols %||% character(),
    kpi_aggregation_rule = kpi_aggregation_rule,
    control_aggregation = root_control_aggregation,
    baseline_spec = baseline_spec
  )
  root_trend_spec <- baseline_contract$trend_spec
  root_fourier_harmonics <- baseline_contract$fourier_harmonics
  root_season_period <- baseline_contract$seasonal_period
  target_depth <- econ_seq_parse_rollup_depths(rollup_depth)
  if (nrow(target_depth) != 1L || identical(target_depth$key[1], "root")) {
    stop("rollup_depth must be leaf or one positive media depth below total paid media.", call. = FALSE)
  }
  root_fit <- fit_parsimonious_total_media_root(
    data = data,
    metadata_input = metadata_input,
    dep_var_col = dep_var_col,
    group_col = group_col,
    time_col = time_col,
    entity_col = entity_col,
    spend_map = spend_map,
    media_variables = NULL,
    media_scope_config = media_scope_config,
    population_col = population_col,
    market_size_col = market_size_col,
    target_population_col = target_population_col,
    invalid_allocation_fallback = invalid_allocation_fallback,
    root_pressure_scaling = root_pressure_scaling,
    root_pressure_col = root_pressure_col,
    root_control_cols = root_control_cols,
    root_control_mode = root_control_mode,
    root_scope = root_scope,
    kpi_aggregation_rule = kpi_aggregation_rule,
    kpi_weight_col = kpi_weight_col,
    root_control_aggregation = root_control_aggregation,
    root_aggregation_functions = root_aggregation_functions,
    incomplete_period_action = incomplete_period_action,
    root_trend_spec = root_trend_spec,
    root_fourier_harmonics = root_fourier_harmonics,
    root_season_period = root_season_period,
    root_time_baseline = root_time_baseline,
    root_knot_n = root_knot_n,
    root_knot_penalty = root_knot_penalty,
    root_geo_media_effect = root_geo_media_effect,
    root_media_transform = root_media_transform,
    root_rrate_grid = root_rrate_grid,
    root_anchor_saturation_grid = root_anchor_saturation_grid,
    root_curve_min_delta_aicc = root_curve_min_delta_aicc,
    root_effect_sign = root_effect_sign,
    root_nonlinear_starts = root_nonlinear_starts,
    root_rrate_bounds = root_rrate_bounds,
    root_half_saturation_multiple_bounds = root_half_saturation_multiple_bounds,
    root_steepness_bounds = root_steepness_bounds,
    root_optimizer_maxit = root_optimizer_maxit,
    root_bootstrap_reps = root_bootstrap_reps,
    root_block_length = root_block_length,
    root_effect_prior = root_effect_prior,
    holdout_col = holdout_col,
    holdout_value = holdout_value,
    holdout_last_n = holdout_last_n,
    seed = seed
  )
  training_times <- root_fit$root_training_times
  baseline_contract$controls <- root_fit$root_control_cols
  baseline_applied <- econ_seq_apply_baseline_contract(
    fit_args = child_fit_args,
    baseline_contract = baseline_contract,
    allow_baseline_override = allow_baseline_override
  )
  child_fit_args <- baseline_applied$fit_args
  baseline_contract <- baseline_applied$baseline_contract
  layer_plan <- build_sequential_rollup_plan(
    metadata_input = root_fit$canonical_metadata,
    media_variables = root_fit$spend_map$variable,
    rollup_map = rollup_map,
    rollup_depths = c(0L, rollup_depth),
    short_path_action = rollup_short_path_action
  )
  rollup_layer <- NULL
  child_data <- root_fit$canonical_data
  child_metadata <- root_fit$canonical_metadata
  child_spend_map <- NULL
  child_rollup_map <- rollup_map
  handoff_child_variables <- child_variables
  if (!identical(target_depth$key[1], "leaf")) {
    if (!is.null(child_variables)) {
      stop(
        "child_variables cannot be combined with numeric rollup_depth yet. A rollup layer must replace the complete paid-media set to preserve the total-media reconciliation.",
        call. = FALSE
      )
    }
    rollup_layer <- build_sequential_rollup_layer(
      data = root_fit$canonical_data,
      metadata_input = root_fit$canonical_metadata,
      spend_map = root_fit$canonical_spend_map,
      rollup_map = rollup_map,
      rollup_depth = target_depth$depth[1],
      media_variables = root_fit$canonical_spend_map$variable,
      short_path_action = rollup_short_path_action,
      curve_type_default = curve_type_default
    )
    child_data <- rollup_layer$data
    child_metadata <- rollup_layer$metadata
    child_spend_map <- rollup_layer$spend_map
    child_rollup_map <- rollup_layer$rollup_map
    handoff_child_variables <- NULL
  }
  diagnostic_spend_map <- child_spend_map %||% root_fit$spend_map
  if (!is.null(handoff_child_variables)) {
    diagnostic_spend_map <- data.table::as.data.table(diagnostic_spend_map)[variable %in% handoff_child_variables]
  }
  identification_controls <- econ_seq_root_controls(
    data = child_data,
    metadata_input = child_metadata,
    media_variables = diagnostic_spend_map$variable,
    root_control_cols = root_control_cols,
    root_control_mode = root_control_mode
  )
  child_identification <- econ_seq_layer_identification_diagnostics(
    data = child_data,
    spend_map = diagnostic_spend_map,
    group_col = group_col,
    time_col = time_col,
    dep_var_col = dep_var_col,
    control_cols = identification_controls,
    baseline_trend_spec = root_trend_spec,
    baseline_fourier_harmonics = root_fourier_harmonics,
    season_period = root_season_period,
    layer_label = if (is.null(rollup_layer)) "modeled_media_leaves" else paste0("rollup_depth_", target_depth$depth[1])
  )
  handoff <- build_sequential_effectiveness_priors(
    root_fit = root_fit,
    data = child_data,
    metadata_input = child_metadata,
    time_col = time_col,
    training_times = training_times,
    child_variables = handoff_child_variables,
    child_spend_map = child_spend_map,
    rollup_map = child_rollup_map,
    child_prior_overrides = child_prior_overrides,
    data_reuse_inflation = data_reuse_inflation,
    child_heterogeneity_relative_sd = child_heterogeneity_relative_sd,
    mix_transfer_scale = mix_transfer_scale,
    minimum_relative_sd = minimum_relative_sd,
    child_identification = child_identification$by_variable,
    strong_child_prior_relaxation = strong_child_prior_relaxation
  )
  model_spend_map <- data.table::as.data.table(child_spend_map %||% root_fit$spend_map)
  enforcement <- econ_seq_enforce_branch_decisions(
    data = child_data,
    metadata_input = child_metadata,
    spend_map = diagnostic_spend_map,
    prior_table = handoff$business_priors,
    parent_layer = NULL,
    curve_type_default = curve_type_default
  )
  untouched_spend <- model_spend_map[!(variable %in% diagnostic_spend_map$variable)]
  child_data <- enforcement$data
  child_metadata <- enforcement$metadata
  child_spend_map <- data.table::rbindlist(
    list(enforcement$spend_map, untouched_spend),
    use.names = TRUE,
    fill = TRUE
  )
  handoff$business_priors <- enforcement$prior_table
  handoff$prior_ledger <- data.table::copy(enforcement$prior_table)
  handoff$prior_ledger[, `:=`(
    generated_prior = TRUE,
    parent_evidence_type = root_fit$root_summary$root_evidence_type[1],
    parent_fit_method = root_fit$root_summary$root_fit_method[1],
    parent_bootstrap_successful = root_fit$root_summary$root_bootstrap_successful[1],
    prior_precision = 1 / prior_sd^2,
    prior_audit_note = "Enforced staged empirical-Bayes handoff. Unresolved branches are retained at an auditable parent/remainder grain rather than fitted independently."
  )]
  handoff$reference_calibration_input <- econ_seq_reference_effectiveness_calibration(enforcement$prior_table)
  handoff$branch_decisions_pre_enforcement <- handoff$branch_decisions
  handoff$branch_decisions <- enforcement$action_audit
  handoff$branch_action_reconciliation <- enforcement$reconciliation
  sequential_transfer_input <- econ_seq_hierarchical_transfer_input(
    enforcement$prior_table,
    tau_overrides = sequential_tau_overrides,
    include_effectiveness = sequential_effectiveness_application %in% c("hierarchical_tau", "fixed_aggregate_reconciliation"),
    effectiveness_tau_mode = if (identical(sequential_effectiveness_application, "fixed_aggregate_reconciliation")) "fixed" else "learned",
    include_adstock = identical(sequential_adstock_application, "hierarchical_tau") &&
      !identical(curve_transfer_mode, "effectiveness_only")
  )
  if (!is.null(rollup_layer)) {
    rollup_layer <- econ_seq_update_layer_mapping_after_enforcement(rollup_layer, enforcement)
    rollup_layer$data <- child_data
    rollup_layer$metadata <- child_metadata
    rollup_layer$spend_map <- child_spend_map
    rollup_layer$branch_action_audit <- enforcement$action_audit
    rollup_layer$branch_action_reconciliation <- enforcement$reconciliation
  }
  # This is the unmodified generic child specification. It is retained so a
  # direct leaf benchmark can prove that sequential differences arise only
  # after parent evidence is applied.
  child_base_prior_specification <- econ_seq_base_prior_specification(
    child_metadata,
    variables = child_spend_map$variable,
    baseline_spec = baseline_contract
  )
  if (identical(sequential_adstock_application, "independent_prior")) {
    child_metadata <- econ_seq_apply_rrate_priors(
      child_metadata,
      handoff$business_priors,
      curve_transfer_mode = curve_transfer_mode,
      # The frequentist root has no joint child likelihood. Its nonlinear
      # evidence regularizes child adstock only; child saturation stays generic.
      saturation_handoff = if (identical(saturation_handoff, "collective_parent_shape_reconciliation")) "generic_child_prior" else saturation_handoff
    )
  } else {
    child_metadata[, sequential_rrate_prior_source := "generic_child_prior_plus_hierarchical_parent_tau"]
    child_metadata[, sequential_saturation_prior_source := "generic_child_saturation_no_individual_parent_anchor"]
  }
  if (!is.null(rollup_layer)) rollup_layer$metadata <- child_metadata
  child_fit <- NULL
  prior_posterior_audit <- econ_seq_sequential_prior_posterior_audit(handoff$business_priors)
  if (isTRUE(fit_child)) {
    inherited_calibration <- if (identical(sequential_effectiveness_application, "reference_calibration")) {
      econ_seq_merge_calibration_inputs(child_fit_args$calibration_input %||% NULL, handoff$reference_calibration_input)
    } else child_fit_args$calibration_input %||% NULL
    inherited_business_priors <- if (identical(sequential_effectiveness_application, "coefficient_approximation")) {
      handoff$business_priors
    } else child_fit_args$business_priors %||% NULL
    child_args <- modifyList(child_fit_args, list(
      data = child_data,
      metadata_input = child_metadata,
      dep_var_col = dep_var_col,
      group_col = group_col,
      time_col = time_col,
      entity_col = entity_col,
      spend_map = child_spend_map,
      business_priors = inherited_business_priors,
      calibration_input = inherited_calibration,
      sequential_transfer_input = sequential_transfer_input
    ))
    child_fit <- do.call(fit_hier_mmm, child_args)
    child_fit$sequential_prior_ledger <- handoff$prior_ledger
    child_fit$sequential_reference_calibration_input <- handoff$reference_calibration_input
    child_fit$sequential_transfer_input <- sequential_transfer_input
    child_fit$sequential_transfer_posterior_audit <- econ_seq_hierarchical_transfer_posterior_audit(
      child_fit, sequential_transfer_input
    )
    child_fit$sequential_branch_decisions <- handoff$branch_decisions
    child_fit$sequential_root_summary <- root_fit$root_summary
    child_fit$sequential_rollup_layer <- rollup_layer
    child_fit$sequential_baseline_spec <- baseline_contract
    child_fit$sequential_holdout_spec <- holdout_contract
    audit_layer <- rollup_layer %||% list(spend_map = child_spend_map, data = child_data)
    prior_posterior_audit <- econ_seq_sequential_prior_posterior_audit(
      handoff$business_priors,
      fit_obj = child_fit,
      layer = audit_layer,
      training_times = training_times,
      seed = seed
    )
    child_fit$sequential_prior_posterior_audit <- prior_posterior_audit
  }
  reconciliation <- data.table::data.table(
    sequential_parent_id = "total_paid_media",
    target_layer_id = target_depth$key[1],
    target_rollup_depth = target_depth$depth[1],
    parent_observational_effectiveness = root_fit$root_summary$root_effectiveness[1],
    parent_prior_effectiveness = root_fit$root_summary$root_effectiveness[1],
    parent_effectiveness_status = root_fit$root_summary$root_effectiveness_status[1],
    parent_implied_contribution = root_fit$root_summary$root_effectiveness[1] * sum(handoff$business_priors$child_spend_total),
    child_prior_implied_contribution = sum(handoff$business_priors$implied_child_contribution_mean),
    reconciliation_difference = root_fit$root_summary$root_effectiveness[1] * sum(handoff$business_priors$child_spend_total) -
      sum(handoff$business_priors$implied_child_contribution_mean),
    reconciliation_basis = "equal_effectiveness_times_observed_child_spend",
    note = "Prior reconciliation only. The fitted child MMM is intentionally allowed to disagree with the parent evidence."
  )
  if (!is.null(output_dir) && nzchar(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    pfx <- if (nzchar(output_prefix %||% "")) paste0(output_prefix, "_") else ""
    data.table::fwrite(root_fit$root_summary, file.path(output_dir, paste0(pfx, "root_summary.csv")))
    data.table::fwrite(root_fit$root_curve_candidates, file.path(output_dir, paste0(pfx, "root_curve_candidates.csv")))
    data.table::fwrite(root_fit$root_identification, file.path(output_dir, paste0(pfx, "root_identification.csv")))
    data.table::fwrite(root_fit$bootstrap_draws, file.path(output_dir, paste0(pfx, "root_bootstrap_draws.csv")))
    data.table::fwrite(handoff$business_priors, file.path(output_dir, paste0(pfx, "child_business_priors.csv")))
    data.table::fwrite(handoff$prior_ledger, file.path(output_dir, paste0(pfx, "prior_ledger.csv")))
    data.table::fwrite(handoff$reference_calibration_input, file.path(output_dir, paste0(pfx, "reference_calibration_input.csv")))
    data.table::fwrite(sequential_transfer_input$effectiveness, file.path(output_dir, paste0(pfx, "hierarchical_effectiveness_transfer.csv")))
    data.table::fwrite(sequential_transfer_input$adstock, file.path(output_dir, paste0(pfx, "hierarchical_adstock_transfer.csv")))
    data.table::fwrite(handoff$branch_decisions, file.path(output_dir, paste0(pfx, "branch_decisions.csv")))
    data.table::fwrite(handoff$branch_action_reconciliation, file.path(output_dir, paste0(pfx, "branch_action_reconciliation.csv")))
    data.table::fwrite(reconciliation, file.path(output_dir, paste0(pfx, "reconciliation_audit.csv")))
    data.table::fwrite(child_identification$by_variable, file.path(output_dir, paste0(pfx, "child_identification.csv")))
    data.table::fwrite(child_identification$overall, file.path(output_dir, paste0(pfx, "depth_gate.csv")))
    data.table::fwrite(handoff$handoff_scope, file.path(output_dir, paste0(pfx, "handoff_scope.csv")))
    data.table::fwrite(layer_plan$layer_plan, file.path(output_dir, paste0(pfx, "layer_plan.csv")))
    if (!is.null(rollup_layer)) {
      data.table::fwrite(rollup_layer$aggregation_audit, file.path(output_dir, paste0(pfx, "rollup_aggregation_audit.csv")))
      data.table::fwrite(rollup_layer$variable_mapping, file.path(output_dir, paste0(pfx, "rollup_variable_mapping.csv")))
    }
  }
  list(
    package_info = econimap_output_metadata("run_sequential_hierarchical_bayes", surface = "sequential_empirical_bayes"),
    method = if (is.null(rollup_layer)) "staged_empirical_bayes_total_media_to_leaves" else "staged_empirical_bayes_total_media_to_selected_rollup_depth",
    root_fit = root_fit,
    source_rollup_map = rollup_map,
    layer_plan = layer_plan,
    rollup_layer = rollup_layer,
    child_data = child_data,
    child_spend_map = child_spend_map,
    child_identification = child_identification,
    depth_gate = child_identification$overall,
    child_business_priors = handoff$business_priors,
    child_reference_calibration_input = handoff$reference_calibration_input,
    child_sequential_transfer_input = sequential_transfer_input,
    branch_decisions = handoff$branch_decisions,
    branch_decisions_pre_enforcement = handoff$branch_decisions_pre_enforcement,
    branch_action_reconciliation = handoff$branch_action_reconciliation,
    child_metadata = child_metadata,
    child_base_prior_specification = child_base_prior_specification,
    prior_ledger = handoff$prior_ledger,
    prior_posterior_audit = prior_posterior_audit,
    holdout_spec = holdout_contract,
    training_times = training_times,
    handoff_scope = handoff$handoff_scope,
    reconciliation_audit = reconciliation,
    baseline_spec = baseline_contract,
    curve_transfer_mode = curve_transfer_mode,
    sequential_effectiveness_application = sequential_effectiveness_application,
    sequential_adstock_application = sequential_adstock_application,
    saturation_handoff = saturation_handoff,
    child_fit = child_fit,
    sequential_transfer_posterior_audit = if (!is.null(child_fit)) child_fit$sequential_transfer_posterior_audit else econ_seq_hierarchical_transfer_posterior_audit(list(), sequential_transfer_input),
    limitations = data.table::data.table(
      limitation = c("same_kpi_data_reuse", "phase_1_child_prior_dependence"),
      treatment = c(
        "Inherited parent uncertainty is tempered before one spend-weighted aggregate-effectiveness constraint; sibling dispersion is estimated through tau rather than identification-score precision multipliers.",
        "The root stage transfers an uncertain aggregate effectiveness and, when supported, an uncertain adstock center. Child effects and retention rates remain estimable before deeper continuation."
      )
    )
  )
}

#' Continue a fitted sequential MMM stage to a deeper optional media layer.
#'
#' `parent_stage` must be the result of a prior sequential stage with
#' `fit_child = TRUE` at a positive numeric `rollup_depth`. The continuation
#' transfers parent response-curve posterior draws, not the original
#' total-media root estimate.
continue_sequential_hierarchical_bayes <- function(parent_stage,
                                                   data,
                                                   metadata_input,
                                                   dep_var_col,
                                                   group_col,
                                                   time_col,
                                                   entity_col,
                                                   spend_map = NULL,
                                                   rollup_map = NULL,
                                                   rollup_depth,
                                                   rollup_short_path_action = c("leaf", "error"),
                                                   curve_type_default = c("hill", "weibull"),
                                                   child_prior_overrides = NULL,
                                                   data_reuse_inflation = 1.5,
                                                   child_heterogeneity_relative_sd = 0.50,
                                                   mix_transfer_scale = 1,
                                                   minimum_relative_sd = 0.35,
                                                   strong_child_prior_relaxation = 1.20,
                                                   curve_transfer_mode = c("effectiveness_adstock_saturation", "effectiveness_adstock", "effectiveness_only"),
                                                   # Below the root, an eligible complete partition receives a
                                                   # soft aggregate shape constraint; individual child curves
                                                   # remain estimable and are never parent-centered directly.
                                                   saturation_handoff = c("collective_parent_shape_reconciliation", "generic_child_prior", "independent_parent_prior"),
                                                   # Keep learned sibling dispersion as the default continuation
                                                   # mode; use fixed aggregate reconciliation as a sensitivity mode.
                                                   sequential_effectiveness_application = c("hierarchical_tau", "fixed_aggregate_reconciliation", "reference_calibration", "coefficient_approximation"),
                                                   sequential_adstock_application = c("independent_prior", "hierarchical_tau"),
                                                   sequential_tau_overrides = NULL,
                                                   parent_draw_count = 200L,
                                                   parent_draw_seed = 123L,
                                                   allow_baseline_override = FALSE,
                                                   fit_child = FALSE,
                                                   child_fit_args = list(),
                                                   output_dir = NULL,
                                                   output_prefix = "sequential_continue") {
  rollup_short_path_action <- match.arg(rollup_short_path_action)
  curve_type_default <- match.arg(curve_type_default)
  sequential_effectiveness_application <- match.arg(sequential_effectiveness_application)
  sequential_adstock_application <- match.arg(sequential_adstock_application)
  curve_transfer_mode <- match.arg(curve_transfer_mode)
  saturation_handoff <- match.arg(saturation_handoff)
  if (identical(sequential_adstock_application, "hierarchical_tau") && identical(saturation_handoff, "independent_parent_prior")) {
    stop("hierarchical_tau adstock cannot be combined with independent_parent_prior saturation; use generic_child_prior or collective_parent_shape_reconciliation.", call. = FALSE)
  }
  if (!is.list(parent_stage) || is.null(parent_stage$child_fit) || is.null(parent_stage$rollup_layer)) {
    stop("parent_stage must be a prior sequential stage with fit_child = TRUE and a positive numeric rollup_depth.", call. = FALSE)
  }
  if (!is.list(child_fit_args)) stop("child_fit_args must be a list.", call. = FALSE)
  baseline_contract <- parent_stage$baseline_spec %||% econ_seq_baseline_contract(
    root_trend_spec = "none",
    root_fourier_harmonics = 0L,
    root_season_period = 52L
  )
  baseline_applied <- econ_seq_apply_baseline_contract(
    fit_args = child_fit_args,
    baseline_contract = baseline_contract,
    allow_baseline_override = allow_baseline_override
  )
  child_fit_args <- baseline_applied$fit_args
  baseline_contract <- baseline_applied$baseline_contract
  holdout_contract <- parent_stage$holdout_spec %||% parent_stage$child_fit$sequential_holdout_spec %||% list()
  training_times <- parent_stage$training_times %||% parent_stage$root_fit$root_training_times
  if (is.null(training_times) || !length(training_times)) {
    # Compatibility for legacy in-memory stages created before holdout support:
    # only an all-row contract is possible because no holdout declaration exists.
    legacy_data <- parent_stage$root_fit$canonical_data %||% data
    if (!(time_col %in% names(legacy_data))) {
      stop("parent_stage is missing the canonical sequential training-period contract.", call. = FALSE)
    }
    training_times <- sort(unique(legacy_data[[time_col]]))
    holdout_contract <- c(holdout_contract, list(legacy_all_rows_training_contract = TRUE))
  }
  for (nm in c("holdout_col", "holdout_value", "holdout_last_n")) {
    if (!is.null(holdout_contract[[nm]])) {
      if (!is.null(child_fit_args[[nm]]) && !identical(child_fit_args[[nm]], holdout_contract[[nm]])) {
        stop("child_fit_args$", nm, " conflicts with the parent sequential holdout contract.", call. = FALSE)
      }
      child_fit_args[[nm]] <- holdout_contract[[nm]]
    }
  }
  parent_layer <- parent_stage$rollup_layer
  parent_depth <- suppressWarnings(as.integer(parent_layer$rollup_depth)[1])
  target_depth <- econ_seq_parse_rollup_depths(rollup_depth)
  target_is_leaf <- nrow(target_depth) == 1L && identical(target_depth$key[1], "leaf")
  if (nrow(target_depth) != 1L || identical(target_depth$key[1], "root") ||
      (!target_is_leaf && target_depth$depth[1] <= parent_depth)) {
    stop("rollup_depth must be a deeper positive numeric depth or the final leaf layer.", call. = FALSE)
  }
  source_rollup_map <- rollup_map %||% parent_stage$source_rollup_map
  source_data <- parent_stage$root_fit$canonical_data %||% data
  source_metadata <- parent_stage$root_fit$canonical_metadata %||% metadata_input
  source_spend_map <- spend_map %||% parent_stage$root_fit$canonical_spend_map %||% parent_stage$root_fit$spend_map
  if (is.null(source_spend_map) || !nrow(source_spend_map)) {
    source_spend_map <- econ_seq_media_spend_map(source_data, source_metadata)
  }
  child_layer <- if (target_is_leaf) {
    econ_seq_build_leaf_layer(
      data = source_data,
      metadata_input = source_metadata,
      spend_map = source_spend_map,
      rollup_map = source_rollup_map,
      media_variables = data.table::as.data.table(source_spend_map)$variable,
      short_path_action = rollup_short_path_action,
      curve_type_default = curve_type_default
    )
  } else {
    build_sequential_rollup_layer(
      data = source_data,
      metadata_input = source_metadata,
      spend_map = source_spend_map,
      rollup_map = source_rollup_map,
      rollup_depth = target_depth$depth[1],
      media_variables = data.table::as.data.table(source_spend_map)$variable,
      short_path_action = rollup_short_path_action,
      curve_type_default = curve_type_default
    )
  }
  child_layer <- econ_seq_carry_stopped_parent_nodes(parent_layer, child_layer)
  identification_controls <- econ_seq_root_controls(
    data = child_layer$data,
    metadata_input = child_layer$metadata,
    media_variables = child_layer$spend_map$variable,
    root_control_mode = "declared_controls"
  )
  inherited_baseline <- baseline_contract
  child_identification <- econ_seq_layer_identification_diagnostics(
    data = child_layer$data,
    spend_map = child_layer$spend_map,
    group_col = group_col,
    time_col = time_col,
    dep_var_col = dep_var_col,
    control_cols = identification_controls,
    baseline_trend_spec = inherited_baseline$trend_spec %||% "none",
    baseline_fourier_harmonics = inherited_baseline$fourier_harmonics %||% 0L,
    season_period = inherited_baseline$seasonal_period %||% 52L,
    layer_label = if (target_is_leaf) "modeled_media_leaves" else paste0("rollup_depth_", target_depth$depth[1])
  )
  if (!is.null(child_layer$carried_parent_nodes) && nrow(child_layer$carried_parent_nodes)) {
    carried <- child_layer$carried_parent_nodes$sequential_parent_id
    child_identification$by_variable[variable %in% carried, `:=`(
      identification_recommendation = "strong_parent_shrinkage",
      parent_shrinkage_multiplier = 4,
      prior_width_multiplier = 1,
      diagnostic_note = "Previously stopped parent branch is carried at its fitted grain with strong posterior-informed shrinkage; it is not decomposed further."
    )]
  }
  handoff <- build_sequential_effectiveness_priors_from_parent_fit(
    parent_fit = parent_stage$child_fit,
    parent_layer = parent_layer,
    child_layer = child_layer,
    time_col = time_col,
    training_times = if (isTRUE(holdout_contract$legacy_all_rows_training_contract)) NULL else training_times,
    child_prior_overrides = child_prior_overrides,
    data_reuse_inflation = data_reuse_inflation,
    child_heterogeneity_relative_sd = child_heterogeneity_relative_sd,
    mix_transfer_scale = mix_transfer_scale,
    minimum_relative_sd = minimum_relative_sd,
    child_identification = child_identification$by_variable,
    strong_child_prior_relaxation = strong_child_prior_relaxation,
    parent_draw_count = parent_draw_count,
    parent_draw_seed = parent_draw_seed
  )
  enforcement <- econ_seq_enforce_branch_decisions(
    data = child_layer$data,
    metadata_input = child_layer$metadata,
    spend_map = child_layer$spend_map,
    prior_table = handoff$business_priors,
    parent_layer = parent_layer,
    curve_type_default = curve_type_default
  )
  child_layer$data <- enforcement$data
  child_layer$metadata <- enforcement$metadata
  child_layer$spend_map <- enforcement$spend_map
  child_layer$branch_action_audit <- enforcement$action_audit
  child_layer$branch_action_reconciliation <- enforcement$reconciliation
  handoff$business_priors <- enforcement$prior_table
  handoff$prior_ledger <- data.table::copy(enforcement$prior_table)
  handoff$prior_ledger[, `:=`(
    generated_prior = TRUE,
    parent_evidence_type = "stan_posterior_response_curve_draws",
    parent_fit_method = "joint_stan_mmm",
    prior_precision = 1 / prior_sd^2,
    prior_audit_note = "Enforced posterior-to-prior branch handoff with unresolved branches retained at parent/remainder grain."
  )]
  handoff$reference_calibration_input <- econ_seq_reference_effectiveness_calibration(
    enforcement$prior_table,
    calibration_prefix = "sequential_parent_effectiveness"
  )
  handoff$branch_decisions_pre_enforcement <- handoff$branch_decisions
  handoff$branch_decisions <- enforcement$action_audit
  handoff$branch_action_reconciliation <- enforcement$reconciliation
  child_layer <- econ_seq_update_layer_mapping_after_enforcement(child_layer, enforcement)
  sequential_transfer_input <- econ_seq_hierarchical_transfer_input(
    enforcement$prior_table,
    tau_overrides = sequential_tau_overrides,
    include_effectiveness = sequential_effectiveness_application %in% c("hierarchical_tau", "fixed_aggregate_reconciliation"),
    effectiveness_tau_mode = if (identical(sequential_effectiveness_application, "fixed_aggregate_reconciliation")) "fixed" else "learned",
    include_adstock = identical(sequential_adstock_application, "hierarchical_tau") &&
      !identical(curve_transfer_mode, "effectiveness_only")
  )
  child_base_prior_specification <- econ_seq_base_prior_specification(
    child_layer$metadata,
    variables = child_layer$spend_map$variable,
    baseline_spec = baseline_contract
  )
  if (identical(sequential_adstock_application, "independent_prior")) {
    child_layer$metadata <- econ_seq_apply_rrate_priors(
      child_layer$metadata,
      handoff$business_priors,
      curve_transfer_mode = curve_transfer_mode,
      saturation_handoff = saturation_handoff
    )
  } else {
    child_layer$metadata[, sequential_rrate_prior_source := "generic_child_prior_plus_hierarchical_parent_tau"]
    child_layer$metadata[, sequential_saturation_prior_source := "generic_child_saturation_no_individual_parent_anchor"]
  }
  # The former response-level experimental handoff double-counted level
  # evidence already supplied through effectiveness calibration and had an
  # unsupported call contract. It is intentionally no longer selectable.
  collective_saturation_reconciliation_input <- econ_seq_empty_collective_response_reconciliation()
  collective_saturation_shape_reconciliation_input <- if (identical(saturation_handoff, "collective_parent_shape_reconciliation") &&
                                                          identical(curve_transfer_mode, "effectiveness_adstock_saturation")) {
    econ_seq_collective_saturation_shape_reconciliation_input(
      parent_fit = parent_stage$child_fit,
      parent_layer = parent_layer,
      child_layer = child_layer,
      time_col = time_col,
      training_times = if (isTRUE(holdout_contract$legacy_all_rows_training_contract)) NULL else training_times,
      max_draws = parent_draw_count,
      seed = parent_draw_seed,
      data_reuse_inflation = data_reuse_inflation,
      child_heterogeneity_relative_sd = child_heterogeneity_relative_sd,
      mix_transfer_scale = mix_transfer_scale,
      sample_curve_parameters = child_fit_args$sample_curve_parameters %||% "always"
    )
  } else {
    list(scenarios = data.table::data.table(), members = data.table::data.table(), parent_shape_cov = matrix(0, 0, 0), audit = data.table::data.table())
  }
  child_fit <- NULL
  prior_posterior_audit <- econ_seq_sequential_prior_posterior_audit(handoff$business_priors)
  if (isTRUE(fit_child)) {
    inherited_calibration <- if (identical(sequential_effectiveness_application, "reference_calibration")) {
      econ_seq_merge_calibration_inputs(child_fit_args$calibration_input %||% NULL, handoff$reference_calibration_input)
    } else child_fit_args$calibration_input %||% NULL
    inherited_business_priors <- if (identical(sequential_effectiveness_application, "coefficient_approximation")) {
      handoff$business_priors
    } else child_fit_args$business_priors %||% NULL
    child_args <- modifyList(child_fit_args, list(
      data = child_layer$data,
      metadata_input = child_layer$metadata,
      dep_var_col = dep_var_col,
      group_col = group_col,
      time_col = time_col,
      entity_col = entity_col,
      spend_map = child_layer$spend_map,
      business_priors = inherited_business_priors,
      calibration_input = inherited_calibration,
      sequential_transfer_input = sequential_transfer_input,
      collective_saturation_reconciliation_input = collective_saturation_reconciliation_input,
      collective_saturation_shape_reconciliation_input = collective_saturation_shape_reconciliation_input
    ))
    child_fit <- do.call(fit_hier_mmm, child_args)
    child_fit$sequential_prior_ledger <- handoff$prior_ledger
    child_fit$sequential_parent_effectiveness <- handoff$parent_effectiveness
    child_fit$sequential_reference_calibration_input <- handoff$reference_calibration_input
    child_fit$sequential_transfer_input <- sequential_transfer_input
    child_fit$sequential_transfer_posterior_audit <- econ_seq_hierarchical_transfer_posterior_audit(
      child_fit, sequential_transfer_input
    )
    child_fit$sequential_branch_decisions <- handoff$branch_decisions
    child_fit$sequential_rollup_layer <- child_layer
    child_fit$sequential_baseline_spec <- baseline_contract
    child_fit$collective_saturation_reconciliation_input <- collective_saturation_reconciliation_input
    child_fit$collective_saturation_shape_reconciliation_input <- collective_saturation_shape_reconciliation_input
    prior_posterior_audit <- econ_seq_sequential_prior_posterior_audit(
      handoff$business_priors,
      fit_obj = child_fit,
      layer = child_layer,
      training_times = if (isTRUE(holdout_contract$legacy_all_rows_training_contract)) NULL else training_times,
      seed = parent_draw_seed
    )
    child_fit$sequential_prior_posterior_audit <- prior_posterior_audit
    # The collective audit consumes measured posterior movement, so attach the
    # training-only prior/posterior audit before classifying child curvature.
    child_fit$collective_saturation_reconciliation_audit <- econ_seq_collective_saturation_reconciliation_audit(child_fit, collective_saturation_reconciliation_input)
    child_fit$collective_saturation_shape_reconciliation_audit <- econ_seq_collective_saturation_shape_reconciliation_audit(child_fit, collective_saturation_shape_reconciliation_input, max_draws = parent_draw_count, seed = parent_draw_seed)
  }
  if (!is.null(output_dir) && nzchar(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    pfx <- if (nzchar(output_prefix %||% "")) paste0(output_prefix, "_") else ""
    data.table::fwrite(handoff$business_priors, file.path(output_dir, paste0(pfx, "child_business_priors.csv")))
    data.table::fwrite(handoff$prior_ledger, file.path(output_dir, paste0(pfx, "prior_ledger.csv")))
    data.table::fwrite(handoff$reference_calibration_input, file.path(output_dir, paste0(pfx, "reference_calibration_input.csv")))
    data.table::fwrite(sequential_transfer_input$effectiveness, file.path(output_dir, paste0(pfx, "hierarchical_effectiveness_transfer.csv")))
    data.table::fwrite(sequential_transfer_input$adstock, file.path(output_dir, paste0(pfx, "hierarchical_adstock_transfer.csv")))
    data.table::fwrite(handoff$branch_decisions, file.path(output_dir, paste0(pfx, "branch_decisions.csv")))
    data.table::fwrite(handoff$parent_effectiveness, file.path(output_dir, paste0(pfx, "parent_effectiveness.csv")))
    data.table::fwrite(handoff$parent_roi_aggregation_audit, file.path(output_dir, paste0(pfx, "parent_roi_aggregation_audit.csv")))
    data.table::fwrite(handoff$parent_child_mapping, file.path(output_dir, paste0(pfx, "parent_child_mapping.csv")))
    data.table::fwrite(handoff$reconciliation_audit, file.path(output_dir, paste0(pfx, "reconciliation_audit.csv")))
    data.table::fwrite(collective_saturation_reconciliation_input$scenarios, file.path(output_dir, paste0(pfx, "collective_saturation_scenarios.csv")))
    data.table::fwrite(collective_saturation_reconciliation_input$members, file.path(output_dir, paste0(pfx, "collective_saturation_members.csv")))
    if (!is.null(child_fit$collective_saturation_reconciliation_audit)) {
      data.table::fwrite(child_fit$collective_saturation_reconciliation_audit$scenarios, file.path(output_dir, paste0(pfx, "collective_saturation_postfit_reconciliation.csv")))
      data.table::fwrite(child_fit$collective_saturation_reconciliation_audit$children, file.path(output_dir, paste0(pfx, "collective_saturation_child_components.csv")))
    }
    if (!is.null(child_fit$collective_saturation_shape_reconciliation_audit)) {
      data.table::fwrite(child_fit$collective_saturation_shape_reconciliation_audit$scenarios, file.path(output_dir, paste0(pfx, "collective_saturation_shape_postfit.csv")))
      data.table::fwrite(child_fit$collective_saturation_shape_reconciliation_audit$children, file.path(output_dir, paste0(pfx, "collective_saturation_shape_child_curvature.csv")))
    }
    if (nrow(collective_saturation_shape_reconciliation_input$mix_selection %||% data.table::data.table())) {
      data.table::fwrite(collective_saturation_shape_reconciliation_input$mix_selection, file.path(output_dir, paste0(pfx, "collective_saturation_shape_mix_selection.csv")))
    }
    data.table::fwrite(child_identification$by_variable, file.path(output_dir, paste0(pfx, "child_identification.csv")))
    data.table::fwrite(child_identification$overall, file.path(output_dir, paste0(pfx, "depth_gate.csv")))
  }
  list(
    package_info = econimap_output_metadata("continue_sequential_hierarchical_bayes", surface = "sequential_empirical_bayes"),
    method = "staged_empirical_bayes_parent_posterior_to_deeper_rollup",
    root_fit = parent_stage$root_fit,
    source_rollup_map = source_rollup_map,
    parent_stage = parent_stage,
    parent_effectiveness = handoff$parent_effectiveness,
    parent_roi_aggregation_audit = handoff$parent_roi_aggregation_audit,
    parent_child_mapping = handoff$parent_child_mapping,
    rollup_layer = child_layer,
    child_identification = child_identification,
    depth_gate = child_identification$overall,
    child_business_priors = handoff$business_priors,
    child_reference_calibration_input = handoff$reference_calibration_input,
    child_sequential_transfer_input = sequential_transfer_input,
    branch_decisions = handoff$branch_decisions,
    child_metadata = child_layer$metadata,
    child_base_prior_specification = child_base_prior_specification,
    prior_ledger = handoff$prior_ledger,
    prior_posterior_audit = prior_posterior_audit,
    holdout_spec = holdout_contract,
    training_times = training_times,
    reconciliation_audit = handoff$reconciliation_audit,
    collective_saturation_reconciliation_input = collective_saturation_reconciliation_input,
    collective_saturation_reconciliation_audit = if (!is.null(child_fit)) child_fit$collective_saturation_reconciliation_audit else list(scenarios = data.table::data.table(), children = data.table::data.table()),
    collective_saturation_shape_reconciliation_input = collective_saturation_shape_reconciliation_input,
    collective_saturation_shape_reconciliation_audit = if (!is.null(child_fit)) child_fit$collective_saturation_shape_reconciliation_audit else list(scenarios = data.table::data.table(), children = data.table::data.table()),
    baseline_spec = baseline_contract,
    curve_transfer_mode = curve_transfer_mode,
    sequential_effectiveness_application = sequential_effectiveness_application,
    sequential_adstock_application = sequential_adstock_application,
    saturation_handoff = saturation_handoff,
    child_fit = child_fit,
    sequential_transfer_posterior_audit = if (!is.null(child_fit)) child_fit$sequential_transfer_posterior_audit else econ_seq_hierarchical_transfer_posterior_audit(list(), sequential_transfer_input),
    limitations = data.table::data.table(
      limitation = "same_kpi_data_reuse",
      treatment = "Parent posterior uncertainty is widened before one aggregate-effectiveness constraint. This remains staged empirical Bayes rather than one joint posterior over every media depth."
    )
  )
}
