# mmm_deck_output_builder.R
# Reporting helpers for turning MMM decomposition outputs into analyst-ready
# tables, charts, and a static HTML dashboard.

if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required.")

mdo_null_coalesce <- function(x, y) if (is.null(x)) y else x

mdo_as_dt <- function(x, label = "input") {
  if (is.null(x)) return(NULL)
  if (data.table::is.data.table(x)) return(data.table::copy(x))
  if (is.data.frame(x)) return(data.table::as.data.table(data.table::copy(x)))
  if (is.character(x) && length(x) == 1L) {
    if (!file.exists(x)) stop(label, " path does not exist: ", x)
    ext <- tolower(tools::file_ext(x))
    if (ext %in% c("csv", "txt")) return(data.table::fread(x))
    if (ext == "rds") return(readRDS(x))
    if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read Excel inputs.")
      }
      return(data.table::as.data.table(readxl::read_excel(x)))
    }
    stop("Unsupported ", label, " file extension: ", ext)
  }
  stop(label, " must be a data.frame, data.table, or file path.")
}

mdo_pick_col <- function(dt, candidates) {
  if (is.null(dt)) return(NA_character_)
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  hit <- candidates[candidates %in% names(dt)]
  if (length(hit)) hit[1] else NA_character_
}

mdo_require_cols <- function(dt, cols, label) {
  miss <- cols[!(cols %in% names(dt))]
  if (length(miss)) stop(label, " is missing required columns: ", paste(miss, collapse = ", "))
  invisible(TRUE)
}

mdo_safe_num <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  out[!is.finite(out)] <- NA_real_
  out
}

mdo_sum_or_na <- function(x) {
  x <- mdo_safe_num(x)
  if (!any(is.finite(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

mdo_as_dateish <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "IDate")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    if (all(is.na(x))) return(as.Date(rep(NA, length(x))))
    med <- median(x, na.rm = TRUE)
    if (is.finite(med) && med > 30000) return(as.Date(x, origin = "1899-12-30"))
    if (is.finite(med) && med > 10000) return(as.Date(x, origin = "1970-01-01"))
  }
  suppressWarnings(as.Date(x))
}

mdo_add_period_fields <- function(dt, time_col, period_granularity = "month") {
  out <- data.table::copy(dt)
  period_granularity <- match.arg(period_granularity, c("week", "month", "quarter", "year", "all"))
  if (period_granularity == "all" || is.na(time_col) || !(time_col %in% names(out))) {
    out[, `:=`(
      period_granularity = "all",
      period_start = as.Date(NA),
      period_label = "All periods",
      period_sort = 1L
    )]
    return(out[])
  }

  d <- mdo_as_dateish(out[[time_col]])
  if (all(is.na(d))) {
    out[, `:=`(
      period_granularity = period_granularity,
      period_start = as.Date(NA),
      period_label = as.character(get(time_col)),
      period_sort = data.table::frank(as.character(get(time_col)), ties.method = "dense")
    )]
    return(out[])
  }

  if (period_granularity == "week") {
    ps <- d
    lab <- format(ps, "%Y-%m-%d")
  } else if (period_granularity == "month") {
    ps <- as.Date(sprintf("%s-01", format(d, "%Y-%m")))
    lab <- format(ps, "%Y-%m")
  } else if (period_granularity == "quarter") {
    yr <- as.integer(format(d, "%Y"))
    q <- floor((as.integer(format(d, "%m")) - 1L) / 3L) + 1L
    ps <- as.Date(sprintf("%04d-%02d-01", yr, (q - 1L) * 3L + 1L))
    lab <- paste0(yr, "-Q", q)
  } else {
    yr <- as.integer(format(d, "%Y"))
    ps <- as.Date(sprintf("%04d-01-01", yr))
    lab <- as.character(yr)
  }
  out[, `:=`(
    period_granularity = period_granularity,
    period_start = ps,
    period_label = lab,
    period_sort = data.table::frank(ps, ties.method = "dense")
  )]
  out[]
}

mdo_standardize_long <- function(long_decomp,
                                 time_col = NULL,
                                 group_col = NULL,
                                 entity_col = NULL,
                                 variable_col = "variable",
                                 contribution_col = "contribution",
                                 actual_col = "y_actual",
                                 fitted_col = "pred",
                                 residual_col = "residual",
                                 sample_col = "sample",
                                 sample_values = NULL,
                                 period_granularity = "month") {
  long <- mdo_as_dt(long_decomp, "long_decomp")
  if (is.null(long) || !nrow(long)) stop("long_decomp must contain at least one row.")

  if (!(variable_col %in% names(long))) stop("variable_col not found in long_decomp: ", variable_col)
  if (!(contribution_col %in% names(long))) stop("contribution_col not found in long_decomp: ", contribution_col)
  if (variable_col != "variable") data.table::setnames(long, variable_col, "variable")
  if (contribution_col != "contribution") data.table::setnames(long, contribution_col, "contribution")

  time_col <- mdo_pick_col(long, c(time_col, "week", "date", "period", "month", "time", "ds"))
  group_col <- mdo_pick_col(long, c(group_col, "geo", "market", "dma", "region", "country", "group"))
  entity_col <- mdo_pick_col(long, c(entity_col, "target_entity", "entity", "brand", "business_unit"))
  sample_col <- mdo_pick_col(long, c(sample_col, "sample", "split"))

  actual_col <- mdo_pick_col(long, c(actual_col, "actual", "y", "dep_var", "kpi"))
  fitted_col <- mdo_pick_col(long, c(fitted_col, "fitted", "y_pred", "prediction", "pred_q50"))
  residual_col <- mdo_pick_col(long, c(residual_col, "error", "resid"))

  long[, variable := as.character(variable)]
  long[, contribution := mdo_safe_num(contribution)]
  if (!is.na(actual_col) && actual_col != "y_actual") data.table::setnames(long, actual_col, "y_actual")
  if (!is.na(fitted_col) && fitted_col != "pred") data.table::setnames(long, fitted_col, "pred")
  if (!is.na(residual_col) && residual_col != "residual") data.table::setnames(long, residual_col, "residual")

  if (!is.null(sample_values) && !is.na(sample_col) && sample_col %in% names(long)) {
    long <- long[get(sample_col) %in% sample_values]
  }
  long <- mdo_add_period_fields(long, time_col, period_granularity)
  data.table::setattr(long, "mdo_cols", list(time_col = time_col, group_col = group_col, entity_col = entity_col, sample_col = sample_col))
  long[]
}

mdo_standardize_wide <- function(wide_decomp,
                                 long_decomp,
                                 time_col = NULL,
                                 group_col = NULL,
                                 entity_col = NULL,
                                 actual_col = "y_actual",
                                 fitted_col = "pred",
                                 residual_col = "residual",
                                 sample_col = "sample",
                                 sample_values = NULL,
                                 period_granularity = "month") {
  if (!is.null(wide_decomp)) {
    wide <- mdo_as_dt(wide_decomp, "wide_decomp")
  } else {
    long_cols <- names(long_decomp)
    id_cols <- setdiff(long_cols, c("variable", "contribution"))
    wide <- unique(long_decomp[, ..id_cols])
  }
  if (!nrow(wide)) return(wide)

  time_col <- mdo_pick_col(wide, c(time_col, "week", "date", "period", "month", "time", "ds"))
  group_col <- mdo_pick_col(wide, c(group_col, "geo", "market", "dma", "region", "country", "group"))
  entity_col <- mdo_pick_col(wide, c(entity_col, "target_entity", "entity", "brand", "business_unit"))
  sample_col <- mdo_pick_col(wide, c(sample_col, "sample", "split"))
  actual_col <- mdo_pick_col(wide, c(actual_col, "actual", "y", "dep_var", "kpi"))
  fitted_col <- mdo_pick_col(wide, c(fitted_col, "fitted", "y_pred", "prediction", "pred_q50"))
  residual_col <- mdo_pick_col(wide, c(residual_col, "error", "resid"))

  if (!is.na(actual_col) && actual_col != "y_actual") data.table::setnames(wide, actual_col, "y_actual")
  if (!is.na(fitted_col) && fitted_col != "pred") data.table::setnames(wide, fitted_col, "pred")
  if (!is.na(residual_col) && residual_col != "residual") data.table::setnames(wide, residual_col, "residual")
  if (!is.null(sample_values) && !is.na(sample_col) && sample_col %in% names(wide)) {
    wide <- wide[get(sample_col) %in% sample_values]
  }
  wide <- mdo_add_period_fields(wide, time_col, period_granularity)
  data.table::setattr(wide, "mdo_cols", list(time_col = time_col, group_col = group_col, entity_col = entity_col, sample_col = sample_col))
  wide[]
}

mdo_infer_spend_map <- function(raw_data, variables, spend_map = NULL, spend_suffix = "_spend") {
  if (!is.null(spend_map)) {
    sm <- mdo_as_dt(spend_map, "spend_map")
    if (!"variable" %in% names(sm)) stop("spend_map must include variable.")
    if (!"spend_col" %in% names(sm)) {
      spend_col <- mdo_pick_col(sm, c("cost_col", "cost", "spend"))
      if (is.na(spend_col)) stop("spend_map must include spend_col or cost_col.")
      data.table::setnames(sm, spend_col, "spend_col")
    }
    sm[, `:=`(variable = as.character(variable), spend_col = as.character(spend_col))]
    return(unique(sm[nzchar(variable) & nzchar(spend_col), .(variable, spend_col)]))
  }
  raw <- mdo_as_dt(raw_data, "raw_data")
  if (is.null(raw) || !nrow(raw)) return(data.table::data.table(variable = character(), spend_col = character()))
  rows <- lapply(as.character(variables), function(v) {
    candidates <- c(paste0(v, spend_suffix), paste0(v, "_cost"), paste0(v, "_investment"), paste0(v, "_media_spend"))
    hit <- candidates[candidates %in% names(raw)]
    if (length(hit)) data.table::data.table(variable = v, spend_col = hit[1]) else NULL
  })
  data.table::rbindlist(rows, fill = TRUE)
}

mdo_assign_roles <- function(variables, media_variables = NULL, baseline_variables = NULL) {
  vars <- as.character(variables)
  lower <- tolower(vars)
  if (is.null(baseline_variables)) {
    baseline_variables <- c("intercept", "intercept_total", "baseline", "base", "trend", "seasonality", "holiday")
  }
  base_lower <- tolower(baseline_variables)
  data.table::fifelse(
    lower == "residual",
    "residual",
    data.table::fifelse(
      vars %in% media_variables,
      "media",
      data.table::fifelse(
        lower %in% base_lower | grepl("intercept|baseline|season|trend|holiday", lower),
        "baseline_control",
        "other_model_term"
      )
    )
  )
}

mdo_split_rollup_path <- function(path) {
  if (length(path) < 1L || is.na(path[1]) || !nzchar(as.character(path[1]))) return(character())
  nodes <- trimws(strsplit(as.character(path[1]), ">", fixed = TRUE)[[1]])
  nodes[nzchar(nodes)]
}

mdo_rollup_reporting_node <- function(path, variable = NA_character_) {
  nodes <- mdo_split_rollup_path(path)
  variable <- as.character(variable)[1]
  if (!length(nodes)) return(variable)
  root_aliases <- c("total", "total_media", "media", "paid_media", "all_media", "all")
  first <- tolower(gsub("[^a-z0-9]+", "_", nodes[1]))
  if (length(nodes) >= 2L && first %in% root_aliases) return(nodes[2])
  nodes[1]
}

mdo_normalize_channel_map <- function(channel_map = NULL, variables) {
  vars <- unique(as.character(variables))
  out <- data.table::data.table(variable = vars, channel = vars, rollup_path = vars)
  if (is.null(channel_map)) return(out[])
  cmap <- data.table::as.data.table(data.table::copy(channel_map))
  if (!"variable" %in% names(cmap)) stop("channel_map must contain variable.", call. = FALSE)
  if (!"channel" %in% names(cmap) && !"rollup_path" %in% names(cmap)) {
    stop("channel_map must contain channel or rollup_path.", call. = FALSE)
  }
  if (!"channel" %in% names(cmap)) cmap[, channel := NA_character_]
  if (!"rollup_path" %in% names(cmap)) cmap[, rollup_path := NA_character_]
  cmap[, `:=`(
    variable = as.character(variable),
    channel = as.character(channel),
    rollup_path = as.character(rollup_path)
  )]
  cmap <- cmap[!is.na(variable) & nzchar(variable)]
  dup <- cmap[duplicated(variable), unique(variable)]
  if (length(dup)) stop("channel_map has duplicate variable rows: ", paste(dup, collapse = ", "), call. = FALSE)
  cmap[is.na(rollup_path) | !nzchar(rollup_path), rollup_path := variable]
  cmap[is.na(channel) | !nzchar(channel),
       channel := mapply(mdo_rollup_reporting_node, rollup_path, variable, USE.NAMES = FALSE)]
  out[cmap[, .(variable, channel, rollup_path)],
      `:=`(channel = i.channel, rollup_path = i.rollup_path),
      on = "variable"]
  out[is.na(channel) | !nzchar(channel), channel := variable]
  out[is.na(rollup_path) | !nzchar(rollup_path), rollup_path := variable]
  out[]
}

mdo_expand_variable_rollup_map <- function(channel_map_normalized) {
  cm <- mdo_as_dt(channel_map_normalized, "channel_map_normalized")
  if (is.null(cm) || !nrow(cm)) {
    return(data.table::data.table(
      variable = character(),
      channel = character(),
      rollup_path = character(),
      rollup_level = integer(),
      rollup_node = character(),
      rollup_node_path = character(),
      is_reporting_channel = logical(),
      is_root_node = logical(),
      is_leaf_node = logical()
    ))
  }
  root_aliases <- c("total", "total_media", "media", "paid_media", "all_media", "all")
  rows <- lapply(seq_len(nrow(cm)), function(i) {
    variable <- as.character(cm$variable[i])
    channel <- as.character(cm$channel[i])
    path <- as.character(cm$rollup_path[i])
    nodes <- mdo_split_rollup_path(path)
    if (!length(nodes)) nodes <- variable
    if (!identical(nodes[length(nodes)], variable)) nodes <- c(nodes, variable)
    node_key <- tolower(gsub("[^a-z0-9]+", "_", nodes))
    data.table::data.table(
      variable = variable,
      channel = channel,
      rollup_path = paste(nodes, collapse = " > "),
      rollup_level = seq_along(nodes),
      rollup_node = nodes,
      rollup_node_path = vapply(seq_along(nodes), function(j) paste(nodes[seq_len(j)], collapse = " > "), character(1)),
      is_reporting_channel = nodes == channel,
      is_root_node = seq_along(nodes) == 1L & node_key %in% root_aliases,
      is_leaf_node = seq_along(nodes) == length(nodes)
    )
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

mdo_fit_metrics <- function(dt, by_cols = character()) {
  req <- c("y_actual", "pred")
  if (!all(req %in% names(dt))) return(data.table::data.table())
  x <- data.table::copy(dt)
  x[, y_actual := mdo_safe_num(y_actual)]
  x[, pred := mdo_safe_num(pred)]
  if (!"residual" %in% names(x)) x[, residual := y_actual - pred]
  x[, residual := mdo_safe_num(residual)]
  calc <- function(.dt) {
    y <- .dt$y_actual
    p <- .dt$pred
    r <- .dt$residual
    ok <- is.finite(y) & is.finite(p)
    y <- y[ok]
    p <- p[ok]
    r <- r[ok]
    if (!length(y)) {
      return(data.table::data.table(n = 0L, actual = NA_real_, pred = NA_real_, residual = NA_real_,
                                    rmse = NA_real_, mae = NA_real_, mape = NA_real_, smape = NA_real_,
                                    r_squared = NA_real_, bias = NA_real_, residual_share_of_actual = NA_real_))
    }
    denom <- sum((y - mean(y, na.rm = TRUE)) ^ 2, na.rm = TRUE)
    data.table::data.table(
      n = length(y),
      actual = sum(y, na.rm = TRUE),
      pred = sum(p, na.rm = TRUE),
      residual = sum(r, na.rm = TRUE),
      rmse = sqrt(mean((y - p) ^ 2, na.rm = TRUE)),
      mae = mean(abs(y - p), na.rm = TRUE),
      mape = mean(abs((y - p) / data.table::fifelse(abs(y) > 1e-8, y, NA_real_)), na.rm = TRUE),
      smape = mean(2 * abs(y - p) / data.table::fifelse(abs(y) + abs(p) > 1e-8, abs(y) + abs(p), NA_real_), na.rm = TRUE),
      r_squared = if (is.finite(denom) && denom > 1e-8) 1 - sum((y - p) ^ 2, na.rm = TRUE) / denom else NA_real_,
      bias = mean(p - y, na.rm = TRUE),
      residual_share_of_actual = sum(r, na.rm = TRUE) / data.table::fifelse(abs(sum(y, na.rm = TRUE)) > 1e-8, sum(y, na.rm = TRUE), NA_real_)
    )
  }
  if (!length(by_cols)) return(calc(x))
  x[, calc(.SD), by = by_cols]
}

mdo_build_spend_totals <- function(raw_data,
                                   spend_map,
                                   time_col,
                                   group_col,
                                   entity_col,
                                   period_granularity = "month",
                                   decomp_domain = NULL,
                                   sample_col = NULL) {
  raw <- mdo_as_dt(raw_data, "raw_data")
  if (is.null(raw) || !nrow(raw) || is.null(spend_map) || !nrow(spend_map)) {
    return(list(total = data.table::data.table(), by_period = data.table::data.table()))
  }
  raw_time_col <- mdo_pick_col(raw, c(time_col, "week", "date", "period", "month", "time", "ds"))
  raw_group_col <- mdo_pick_col(raw, c(group_col, "geo", "market", "dma", "region", "country", "group"))
  raw_entity_col <- mdo_pick_col(raw, c(entity_col, "target_entity", "entity", "brand", "business_unit"))
  raw_sample_col <- mdo_pick_col(raw, c(sample_col, "sample", "split"))
  if (!is.null(decomp_domain) && nrow(decomp_domain)) {
    dom <- data.table::as.data.table(data.table::copy(decomp_domain))
    join_cols <- character()
    if (!is.na(raw_time_col) && !is.na(time_col) && time_col %in% names(dom)) {
      raw[, `__mdo_time_key` := mdo_as_dateish(get(raw_time_col))]
      dom[, `__mdo_time_key` := mdo_as_dateish(get(time_col))]
      join_cols <- c(join_cols, "__mdo_time_key")
    }
    if (!is.na(raw_group_col) && !is.na(group_col) && group_col %in% names(dom)) {
      raw[, `__mdo_group_key` := as.character(get(raw_group_col))]
      dom[, `__mdo_group_key` := as.character(get(group_col))]
      join_cols <- c(join_cols, "__mdo_group_key")
    }
    if (!is.na(raw_entity_col) && !is.na(entity_col) && entity_col %in% names(dom)) {
      raw[, `__mdo_entity_key` := as.character(get(raw_entity_col))]
      dom[, `__mdo_entity_key` := as.character(get(entity_col))]
      join_cols <- c(join_cols, "__mdo_entity_key")
    }
    if (!is.na(raw_sample_col) && !is.na(sample_col) && sample_col %in% names(dom)) {
      raw[, `__mdo_sample_key` := as.character(get(raw_sample_col))]
      dom[, `__mdo_sample_key` := as.character(get(sample_col))]
      join_cols <- c(join_cols, "__mdo_sample_key")
    }
    if (length(join_cols)) {
      dom_keys <- unique(dom[, ..join_cols])
      raw <- raw[dom_keys, on = join_cols, nomatch = 0]
    }
  }
  raw <- mdo_add_period_fields(raw, raw_time_col, period_granularity)

  rows_total <- lapply(seq_len(nrow(spend_map)), function(i) {
    v <- spend_map$variable[i]
    sc <- spend_map$spend_col[i]
    if (!(sc %in% names(raw))) {
      return(data.table::data.table(variable = v, spend_col = sc, spend = NA_real_, spend_rows = 0L, spend_status = "spend_col_missing"))
    }
    spend <- sum(mdo_safe_num(raw[[sc]]), na.rm = TRUE)
    data.table::data.table(variable = v, spend_col = sc, spend = spend, spend_rows = sum(is.finite(mdo_safe_num(raw[[sc]]))), spend_status = "ok")
  })
  total <- data.table::rbindlist(rows_total, fill = TRUE)

  rows_period <- lapply(seq_len(nrow(spend_map)), function(i) {
    v <- spend_map$variable[i]
    sc <- spend_map$spend_col[i]
    if (!(sc %in% names(raw))) return(NULL)
    raw[, .(spend = sum(mdo_safe_num(get(sc)), na.rm = TRUE)), by = .(period_granularity, period_start, period_label, period_sort)
    ][, `:=`(variable = v, spend_col = sc)]
  })
  by_period <- data.table::rbindlist(rows_period, fill = TRUE)

  by_group <- data.table::data.table()
  if (!is.na(raw_group_col) && raw_group_col %in% names(raw)) {
    rows_group <- lapply(seq_len(nrow(spend_map)), function(i) {
      v <- spend_map$variable[i]
      sc <- spend_map$spend_col[i]
      if (!(sc %in% names(raw))) return(NULL)
      raw[, .(spend = sum(mdo_safe_num(get(sc)), na.rm = TRUE)), by = raw_group_col][, `:=`(variable = v, spend_col = sc)]
    })
    by_group <- data.table::rbindlist(rows_group, fill = TRUE)
  }

  by_entity <- data.table::data.table()
  if (!is.na(raw_entity_col) && raw_entity_col %in% names(raw)) {
    rows_entity <- lapply(seq_len(nrow(spend_map)), function(i) {
      v <- spend_map$variable[i]
      sc <- spend_map$spend_col[i]
      if (!(sc %in% names(raw))) return(NULL)
      raw[, .(spend = sum(mdo_safe_num(get(sc)), na.rm = TRUE)), by = raw_entity_col][, `:=`(variable = v, spend_col = sc)]
    })
    by_entity <- data.table::rbindlist(rows_entity, fill = TRUE)
  }

  list(total = total[], by_period = by_period[], by_group = by_group[], by_entity = by_entity[])
}

mdo_empty_kpi_economics <- function(by_period = FALSE) {
  out <- data.table::data.table(
    variable = character(),
    spend_col = character(),
    spend = numeric(),
    spend_rows = integer(),
    spend_status = character(),
    contribution = numeric(),
    role = character(),
    outcome_per_cost = numeric(),
    cost_per_outcome = numeric(),
    signed_economics_flag = character(),
    spend_share = numeric(),
    contribution_share = numeric(),
    efficiency_index = numeric(),
    fair_share_index = numeric()
  )
  if (isTRUE(by_period)) {
    out[, `:=`(
      period_granularity = character(),
      period_start = as.Date(character()),
      period_label = character(),
      period_sort = integer()
    )]
    data.table::setcolorder(out, c("period_granularity", "period_start", "period_label", "period_sort", setdiff(names(out), c("period_granularity", "period_start", "period_label", "period_sort"))))
  }
  out[]
}

mdo_get_table <- function(x, name) {
  if (is.null(x)) return(data.table::data.table())
  if (is.list(x) && !is.null(x[[name]])) return(mdo_as_dt(x[[name]], paste0("optimizer_output$", name)))
  data.table::data.table()
}

mdo_add_missing_cols <- function(dt, cols) {
  out <- data.table::copy(dt)
  for (cc in setdiff(cols, names(out))) out[, (cc) := NA]
  out[]
}

mdo_build_optimizer_deck_tables <- function(optimizer_output = NULL) {
  empty <- data.table::data.table()
  if (is.null(optimizer_output)) {
    return(list(
      optimizer_current_plan = empty,
      optimizer_scenario_summary = empty,
      optimizer_scenario_detail = empty,
      optimizer_plan = empty,
      optimizer_summary = empty,
      optimizer_group_rollup = empty,
      optimizer_saturation_headroom = empty,
      optimizer_response_curves = empty,
      optimizer_response_curve_uncertainty = empty,
      optimizer_scenario_uncertainty_summary = empty,
      optimizer_optimization_uncertainty_summary = empty,
      optimizer_scenario_comparison = empty
    ))
  }
  current_plan <- mdo_get_table(optimizer_output, "current_plan")
  scenario_summary <- mdo_get_table(optimizer_output, "scenario_summary")
  scenario_detail <- mdo_get_table(optimizer_output, "scenario_detail")
  opt_plan <- mdo_get_table(optimizer_output, "optimization_plan")
  opt_summary <- mdo_get_table(optimizer_output, "optimization_summary")
  group_rollup <- mdo_get_table(optimizer_output, "optimization_group_rollup")
  saturation <- mdo_get_table(optimizer_output, "saturation_headroom")
  curves <- mdo_get_table(optimizer_output, "response_curves")
  curve_unc <- mdo_get_table(optimizer_output, "response_curve_uncertainty")
  scen_unc <- mdo_get_table(optimizer_output, "scenario_uncertainty_summary")
  opt_unc <- mdo_get_table(optimizer_output, "optimization_uncertainty_summary")

  for (nm in c("scenario_summary", "scenario_detail", "opt_plan", "opt_summary", "curves", "saturation")) {
    if (exists(nm, inherits = FALSE) && nrow(get(nm))) {
      tmp <- get(nm)
      for (cc in intersect(c("spend", "current_spend", "recommended_spend", "contribution", "expected_contribution",
                             "contribution_vs_current", "incremental_contribution", "roi", "expected_roi",
                             "cost_per_kpi", "expected_cost_per_kpi", "mroi", "expected_mroi", "spend_multiplier"), names(tmp))) {
        tmp[, (cc) := mdo_safe_num(get(cc))]
      }
      assign(nm, tmp)
    }
  }
  if (nrow(curves) && nrow(curve_unc) && all(c("variable", "spend_multiplier") %in% names(curves)) &&
      all(c("variable", "spend_multiplier") %in% names(curve_unc))) {
    curve_unc[, `:=`(variable = as.character(variable), spend_multiplier = mdo_safe_num(spend_multiplier))]
    qcols <- grep("_(q05|q50|q95)$|^draw_n$", names(curve_unc), value = TRUE)
    curves[, `:=`(variable = as.character(variable), spend_multiplier = mdo_safe_num(spend_multiplier))]
    curves[curve_unc[, c("variable", "spend_multiplier", qcols), with = FALSE],
           (qcols) := mget(paste0("i.", qcols)),
           on = c("variable", "spend_multiplier")]
  }

  comparison_rows <- list()
  if (nrow(scenario_summary)) {
    ss <- data.table::copy(scenario_summary)
    ss <- mdo_add_missing_cols(ss, c("scenario", "spend", "contribution", "contribution_vs_current", "roi", "cost_per_kpi",
                                    "expected_profit", "q05_profit", "probability_profit_positive",
                                    "probability_incremental_contribution_positive"))
    comparison_rows[[length(comparison_rows) + 1L]] <- ss[, .(
      plan_type = "scenario",
      plan_name = as.character(scenario),
      spend = mdo_safe_num(spend),
      contribution = mdo_safe_num(contribution),
      incremental_contribution = mdo_safe_num(contribution_vs_current),
      roi = mdo_safe_num(roi),
      cost_per_kpi = mdo_safe_num(cost_per_kpi),
      expected_profit = mdo_safe_num(expected_profit),
      q05_profit = mdo_safe_num(q05_profit),
      probability_profit_positive = mdo_safe_num(probability_profit_positive),
      probability_incremental_contribution_positive = mdo_safe_num(probability_incremental_contribution_positive)
    )]
  }
  if (nrow(opt_summary)) {
    os <- data.table::copy(opt_summary)
    os <- mdo_add_missing_cols(os, c("recommended_spend", "expected_contribution", "incremental_contribution",
                                    "expected_roi", "expected_cost_per_kpi", "optimizer_basis"))
    comparison_rows[[length(comparison_rows) + 1L]] <- os[, .(
      plan_type = "optimized",
      plan_name = if ("optimizer_basis" %in% names(os)) as.character(optimizer_basis) else "optimized",
      spend = mdo_safe_num(recommended_spend),
      contribution = mdo_safe_num(expected_contribution),
      incremental_contribution = mdo_safe_num(incremental_contribution),
      roi = mdo_safe_num(expected_roi),
      cost_per_kpi = mdo_safe_num(expected_cost_per_kpi),
      expected_profit = NA_real_,
      q05_profit = NA_real_,
      probability_profit_positive = NA_real_,
      probability_incremental_contribution_positive = NA_real_
    )]
  }
  if (nrow(opt_unc)) {
    ou <- data.table::copy(opt_unc)
    ou <- mdo_add_missing_cols(ou, c("expected_profit", "q05_profit", "probability_profit_positive",
                                    "probability_incremental_contribution_positive"))
    if (length(comparison_rows)) {
      # Keep the uncertainty fields in a separate optimizer table, but enrich the optimized row when one exists.
      comparison <- data.table::rbindlist(comparison_rows, use.names = TRUE, fill = TRUE)
      comparison[plan_type == "optimized", `:=`(
        expected_profit = mdo_safe_num(ou$expected_profit)[1],
        q05_profit = mdo_safe_num(ou$q05_profit)[1],
        probability_profit_positive = mdo_safe_num(ou$probability_profit_positive)[1],
        probability_incremental_contribution_positive = mdo_safe_num(ou$probability_incremental_contribution_positive)[1]
      )]
    } else {
      comparison <- empty
    }
  } else {
    comparison <- if (length(comparison_rows)) data.table::rbindlist(comparison_rows, use.names = TRUE, fill = TRUE) else empty
  }

  list(
    optimizer_current_plan = current_plan[],
    optimizer_scenario_summary = scenario_summary[],
    optimizer_scenario_detail = scenario_detail[],
    optimizer_plan = opt_plan[],
    optimizer_summary = opt_summary[],
    optimizer_group_rollup = group_rollup[],
    optimizer_saturation_headroom = saturation[],
    optimizer_response_curves = curves[],
    optimizer_response_curve_uncertainty = curve_unc[],
    optimizer_scenario_uncertainty_summary = scen_unc[],
    optimizer_optimization_uncertainty_summary = opt_unc[],
    optimizer_scenario_comparison = comparison[]
  )
}

mdo_build_chart_registry <- function(report_tables) {
  specs <- data.table::data.table(
    chart_id = c(
      "contribution_by_variable", "contribution_trend", "actual_vs_fitted", "residuals_by_period",
      "cost_per_outcome", "spend_vs_contribution", "kpi_decomposition_funnel",
      "optimizer_current_vs_recommended_spend", "optimizer_scenario_incremental_contribution",
      "optimizer_response_curves", "optimizer_mroi_curves", "optimizer_saturation_headroom"
    ),
    chart_name = c(
      "Contribution by variable", "Contribution trend", "Actual vs fitted KPI", "Residuals by period",
      "Cost per KPI outcome", "Spend vs KPI contribution", "KPI decomposition funnel",
      "Current vs recommended spend", "Scenario incremental contribution",
      "Response curves", "Marginal response curves", "Saturation and headroom"
    ),
    audience = c("client", "client", "appendix", "internal_qa", "client", "client", "client",
                 "client", "client", "client", "appendix", "appendix"),
    required_table = c(
      "contribution_by_variable", "contribution_by_period_variable", "fit_by_period", "fit_by_period",
      "kpi_economics", "kpi_economics", "funnel_summary",
      "optimizer_plan", "optimizer_scenario_comparison", "optimizer_response_curves",
      "optimizer_response_curves", "optimizer_saturation_headroom"
    ),
    required_columns = c(
      "variable|contribution|role", "period_label|period_sort|variable|contribution", "period_label|period_sort|actual|pred", "period_label|period_sort|residual",
      "variable|cost_per_outcome", "variable|spend|contribution", "stage|value",
      "variable|current_spend|recommended_spend", "plan_name|incremental_contribution", "variable|spend_multiplier|contribution",
      "variable|spend_multiplier|mroi", "variable|pct_of_peak_grid_contribution"
    ),
    business_question_answered = c(
      "Which drivers contributed the most KPI?", "How did driver contribution move over time?", "How closely did the model fit observed KPI?", "Where are residuals concentrated?",
      "Which channels look efficient on average?", "How does contribution compare with spend?", "How does actual KPI decompose into model components?",
      "How does the recommended plan change spend?", "Which scenarios add the most incremental KPI?", "How does expected contribution change as spend/support changes?",
      "Where is marginal response strongest or weakest?", "Which channels have response headroom?"
    ),
    recommended_slide_title = c(
      "What drove KPI contribution?", "How contribution changed over time", "Model fit over time", "Residual QA by period",
      "Cost per KPI by channel", "Spend and contribution by channel", "KPI decomposition funnel",
      "Recommended budget changes", "Scenario KPI upside", "Response curves by channel",
      "Marginal response by channel", "Saturation and headroom"
    )
  )
  specs[, available := vapply(seq_len(.N), function(i) {
    tab_name <- required_table[i]
    if (!tab_name %in% names(report_tables)) return(FALSE)
    tab <- report_tables[[tab_name]]
    if (is.null(tab) || !nrow(tab)) return(FALSE)
    req <- strsplit(required_columns[i], "\\|")[[1]]
    all(req %in% names(tab))
  }, logical(1))]
  specs[, skip_if_missing_columns := !available]
  specs[]
}

build_mmm_deck_tables <- function(long_decomp,
                                  wide_decomp = NULL,
                                  raw_data = NULL,
                                  modcut = NULL,
                                  spend_map = NULL,
                                  optimizer_output = NULL,
                                  channel_map = NULL,
                                  media_variables = NULL,
                                  baseline_variables = NULL,
                                  time_col = NULL,
                                  group_col = NULL,
                                  entity_col = NULL,
                                  variable_col = "variable",
                                  contribution_col = "contribution",
                                  actual_col = "y_actual",
                                  fitted_col = "pred",
                                  residual_col = "residual",
                                  sample_col = "sample",
                                  sample_values = NULL,
                                  period_granularity = "month",
                                  spend_suffix = "_spend",
                                  kpi_value_per_outcome = NULL) {
  raw_data <- mdo_null_coalesce(raw_data, modcut)
  long <- mdo_standardize_long(
    long_decomp = long_decomp,
    time_col = time_col,
    group_col = group_col,
    entity_col = entity_col,
    variable_col = variable_col,
    contribution_col = contribution_col,
    actual_col = actual_col,
    fitted_col = fitted_col,
    residual_col = residual_col,
    sample_col = sample_col,
    sample_values = sample_values,
    period_granularity = period_granularity
  )
  cols <- attr(long, "mdo_cols")
  time_col <- cols$time_col
  group_col <- cols$group_col
  entity_col <- cols$entity_col
  sample_col <- cols$sample_col
  wide <- mdo_standardize_wide(
    wide_decomp = wide_decomp,
    long_decomp = long,
    time_col = time_col,
    group_col = group_col,
    entity_col = entity_col,
    actual_col = actual_col,
    fitted_col = fitted_col,
    residual_col = residual_col,
    sample_col = sample_col,
    sample_values = sample_values,
    period_granularity = period_granularity
  )
  if (!"y_actual" %in% names(wide)) wide[, y_actual := NA_real_]
  if (!"pred" %in% names(wide)) wide[, pred := NA_real_]
  if (!"residual" %in% names(wide)) {
    wide[, residual := data.table::fifelse(is.finite(mdo_safe_num(y_actual)) & is.finite(mdo_safe_num(pred)), mdo_safe_num(y_actual) - mdo_safe_num(pred), NA_real_)]
  }
  wide[, `:=`(
    y_actual = mdo_safe_num(y_actual),
    pred = mdo_safe_num(pred),
    residual = mdo_safe_num(residual)
  )]

  all_variables <- sort(unique(long$variable))
  inferred_spend_map <- mdo_infer_spend_map(raw_data, all_variables, spend_map = spend_map, spend_suffix = spend_suffix)
  if (is.null(media_variables)) media_variables <- inferred_spend_map$variable
  media_variables <- unique(as.character(media_variables))
  long[, role := mdo_assign_roles(variable, media_variables = media_variables, baseline_variables = baseline_variables)]
  channel_map_normalized <- mdo_normalize_channel_map(channel_map, all_variables)
  variable_rollup_map <- mdo_expand_variable_rollup_map(channel_map_normalized)
  long[, channel := variable]
  long[channel_map_normalized[, .(variable, channel)], channel := i.channel, on = "variable"]
  long[is.na(channel) | !nzchar(channel), channel := variable]
  if (!is.null(channel_map)) {
    cmap <- data.table::as.data.table(data.table::copy(channel_map))
    role_col <- intersect(c("role", "variable_role", "channel_role"), names(cmap))[1]
    if (!is.na(role_col)) {
      cmap[, variable := as.character(variable)]
      role_override <- cmap[, .(variable, role_override__ = as.character(get(role_col)))]
      long[role_override[!is.na(role_override__) & nzchar(role_override__)],
           role := i.role_override__, on = "variable"]
    }
  }

  total_actual <- mdo_sum_or_na(wide$y_actual)
  total_pred <- mdo_sum_or_na(wide$pred)
  modeled_total <- long[role != "residual", sum(contribution, na.rm = TRUE)]
  abs_total <- long[role != "residual", sum(abs(contribution), na.rm = TRUE)]

  contribution_by_variable <- long[, .(
    contribution = sum(contribution, na.rm = TRUE),
    avg_row_contribution = mean(contribution, na.rm = TRUE),
    min_row_contribution = min(contribution, na.rm = TRUE),
    max_row_contribution = max(contribution, na.rm = TRUE),
    n_rows = .N
  ), by = .(variable, role)]
  contribution_by_variable[, `:=`(
    share_of_model_contribution = contribution / data.table::fifelse(abs(modeled_total) > 1e-8, modeled_total, NA_real_),
    share_of_absolute_contribution = abs(contribution) / data.table::fifelse(abs(abs_total) > 1e-8, abs_total, NA_real_),
    share_of_actual_kpi = contribution / data.table::fifelse(abs(total_actual) > 1e-8, total_actual, NA_real_)
  )]
  contribution_by_variable[, contribution_abs_sort__ := abs(contribution)]
  data.table::setorder(contribution_by_variable, -contribution_abs_sort__)
  contribution_by_variable[, contribution_abs_sort__ := NULL]

  contribution_by_channel <- long[, .(
    contribution = sum(contribution, na.rm = TRUE),
    avg_row_contribution = mean(contribution, na.rm = TRUE),
    n_variables = data.table::uniqueN(variable),
    n_rows = .N
  ), by = .(channel, role)]
  contribution_by_channel[, `:=`(
    share_of_model_contribution = contribution / data.table::fifelse(abs(modeled_total) > 1e-8, modeled_total, NA_real_),
    share_of_absolute_contribution = abs(contribution) / data.table::fifelse(abs(abs_total) > 1e-8, abs_total, NA_real_),
    share_of_actual_kpi = contribution / data.table::fifelse(abs(total_actual) > 1e-8, total_actual, NA_real_)
  )]
  contribution_by_channel[, contribution_abs_sort__ := abs(contribution)]
  data.table::setorder(contribution_by_channel, -contribution_abs_sort__)
  contribution_by_channel[, contribution_abs_sort__ := NULL]

  rollup_join <- long[variable_rollup_map[, .(
    variable,
    rollup_level,
    rollup_node,
    rollup_node_path,
    is_reporting_channel,
    is_root_node,
    is_leaf_node
  )], on = "variable", allow.cartesian = TRUE, nomatch = 0]
  contribution_by_rollup_node <- data.table::data.table()
  if (nrow(rollup_join)) {
    contribution_by_rollup_node <- rollup_join[, .(
      contribution = sum(contribution, na.rm = TRUE),
      avg_row_contribution = mean(contribution, na.rm = TRUE),
      n_variables = data.table::uniqueN(variable),
      variables = paste(sort(unique(variable)), collapse = "|"),
      n_rows = .N
    ), by = .(rollup_level, rollup_node, rollup_node_path, is_reporting_channel, is_root_node, is_leaf_node, role)]
    contribution_by_rollup_node[, `:=`(
      share_of_model_contribution = contribution / data.table::fifelse(abs(modeled_total) > 1e-8, modeled_total, NA_real_),
      share_of_absolute_contribution = abs(contribution) / data.table::fifelse(abs(abs_total) > 1e-8, abs_total, NA_real_),
      share_of_actual_kpi = contribution / data.table::fifelse(abs(total_actual) > 1e-8, total_actual, NA_real_)
    )]
    data.table::setorderv(contribution_by_rollup_node, c("rollup_level", "rollup_node", "role"))
  }

  contribution_by_period_variable <- long[, .(
    contribution = sum(contribution, na.rm = TRUE),
    n_rows = .N
  ), by = .(period_granularity, period_start, period_label, period_sort, variable, role)]
  period_totals <- contribution_by_period_variable[role != "residual", .(period_model_contribution = sum(contribution, na.rm = TRUE)), by = period_label]
  contribution_by_period_variable[period_totals, period_model_contribution := i.period_model_contribution, on = "period_label"]
  contribution_by_period_variable[, share_of_period_model_contribution := contribution / data.table::fifelse(abs(period_model_contribution) > 1e-8, period_model_contribution, NA_real_)]
  contribution_by_period_variable[, contribution_abs_sort__ := abs(contribution)]
  data.table::setorderv(contribution_by_period_variable, c("period_sort", "contribution_abs_sort__"), order = c(1L, -1L))
  contribution_by_period_variable[, contribution_abs_sort__ := NULL]

  period_kpi_change <- wide[, .(
    actual = mdo_sum_or_na(y_actual),
    fitted = mdo_sum_or_na(pred),
    residual = mdo_sum_or_na(residual)
  ), by = .(period_granularity, period_start, period_label, period_sort)]
  data.table::setorder(period_kpi_change, period_sort)
  period_kpi_change[, `:=`(
    actual_change = actual - data.table::shift(actual),
    actual_pct_change = actual / data.table::shift(actual) - 1,
    fitted_change = fitted - data.table::shift(fitted),
    fitted_pct_change = fitted / data.table::shift(fitted) - 1
  )]

  period_due_to_variable <- data.table::copy(contribution_by_period_variable)
  data.table::setorder(period_due_to_variable, variable, period_sort)
  period_due_to_variable[, contribution_change := contribution - data.table::shift(contribution), by = variable]
  period_due_to_variable[period_kpi_change[, .(period_label, actual_change, actual_pct_change)],
                         `:=`(actual_change = i.actual_change, actual_pct_change = i.actual_pct_change),
                         on = "period_label"]
  period_due_to_variable[, due_to_pct_of_actual_change := contribution_change / data.table::fifelse(abs(actual_change) > 1e-8, actual_change, NA_real_)]

  contribution_by_period_channel <- long[, .(
    contribution = sum(contribution, na.rm = TRUE),
    n_variables = data.table::uniqueN(variable),
    n_rows = .N
  ), by = .(period_granularity, period_start, period_label, period_sort, channel, role)]
  contribution_by_period_channel[period_totals, period_model_contribution := i.period_model_contribution, on = "period_label"]
  contribution_by_period_channel[, share_of_period_model_contribution := contribution / data.table::fifelse(abs(period_model_contribution) > 1e-8, period_model_contribution, NA_real_)]
  data.table::setorder(contribution_by_period_channel, period_sort, channel)

  period_due_to_channel <- data.table::copy(contribution_by_period_channel)
  data.table::setorder(period_due_to_channel, channel, period_sort)
  period_due_to_channel[, contribution_change := contribution - data.table::shift(contribution), by = channel]
  period_due_to_channel[period_kpi_change[, .(period_label, actual_change, actual_pct_change)],
                        `:=`(actual_change = i.actual_change, actual_pct_change = i.actual_pct_change),
                        on = "period_label"]
  period_due_to_channel[, due_to_pct_of_actual_change := contribution_change / data.table::fifelse(abs(actual_change) > 1e-8, actual_change, NA_real_)]

  contribution_by_period_rollup_node <- data.table::data.table()
  if (nrow(rollup_join)) {
    contribution_by_period_rollup_node <- rollup_join[, .(
      contribution = sum(contribution, na.rm = TRUE),
      n_variables = data.table::uniqueN(variable),
      n_rows = .N
    ), by = .(period_granularity, period_start, period_label, period_sort,
              rollup_level, rollup_node, rollup_node_path, is_reporting_channel, is_root_node, is_leaf_node, role)]
    contribution_by_period_rollup_node[period_totals, period_model_contribution := i.period_model_contribution, on = "period_label"]
    contribution_by_period_rollup_node[, share_of_period_model_contribution := contribution / data.table::fifelse(abs(period_model_contribution) > 1e-8, period_model_contribution, NA_real_)]
    data.table::setorderv(contribution_by_period_rollup_node, c("period_sort", "rollup_level", "rollup_node"))
  }

  contribution_by_group_variable <- data.table::data.table()
  if (!is.na(group_col) && group_col %in% names(long)) {
    contribution_by_group_variable <- long[, .(
      contribution = sum(contribution, na.rm = TRUE),
      n_rows = .N
    ), by = c(group_col, "variable", "role")]
    contribution_by_group_variable[, contribution_abs_sort__ := abs(contribution)]
    data.table::setorderv(contribution_by_group_variable, c(group_col, "contribution_abs_sort__"), order = c(1L, -1L))
    contribution_by_group_variable[, contribution_abs_sort__ := NULL]
  }

  fit_diagnostics <- mdo_fit_metrics(wide)
  if (nrow(fit_diagnostics)) fit_diagnostics[, grain := "overall"]
  fit_by_period <- mdo_fit_metrics(wide, by_cols = c("period_granularity", "period_start", "period_label", "period_sort"))
  fit_by_group <- data.table::data.table()
  if (!is.na(group_col) && group_col %in% names(wide)) {
    fit_by_group <- mdo_fit_metrics(wide, by_cols = group_col)
  }

  spend <- mdo_build_spend_totals(
    raw_data = raw_data,
    spend_map = inferred_spend_map,
    time_col = time_col,
    group_col = group_col,
    entity_col = entity_col,
    period_granularity = period_granularity,
    decomp_domain = unique(long[, intersect(c(time_col, group_col, entity_col, sample_col), names(long)), with = FALSE]),
    sample_col = sample_col
  )
  kpi_economics <- data.table::copy(spend$total)
  if (!ncol(kpi_economics)) kpi_economics <- mdo_empty_kpi_economics(FALSE)
  if (nrow(kpi_economics)) {
    kpi_economics[contribution_by_variable, `:=`(contribution = i.contribution, role = i.role), on = "variable"]
    kpi_economics[, channel := variable]
    kpi_economics[channel_map_normalized[, .(variable, channel)], channel := i.channel, on = "variable"]
    kpi_economics[is.na(channel) | !nzchar(channel), channel := variable]
    total_spend <- sum(kpi_economics$spend, na.rm = TRUE)
    total_media_contribution <- sum(kpi_economics$contribution, na.rm = TRUE)
    kpi_economics[, `:=`(
      outcome_per_cost = contribution / data.table::fifelse(abs(spend) > 1e-8, spend, NA_real_),
      cost_per_outcome = spend / data.table::fifelse(contribution > 1e-8, contribution, NA_real_),
      signed_economics_flag = data.table::fifelse(contribution > 1e-8, "positive_economics", data.table::fifelse(contribution < -1e-8, "negative_contribution_diagnostic", "zero_contribution_diagnostic")),
      spend_share = spend / data.table::fifelse(abs(total_spend) > 1e-8, total_spend, NA_real_),
      contribution_share = contribution / data.table::fifelse(abs(total_media_contribution) > 1e-8, total_media_contribution, NA_real_)
    )]
    kpi_economics[, efficiency_index := contribution_share / data.table::fifelse(abs(spend_share) > 1e-8, spend_share, NA_real_)]
    kpi_economics[, fair_share_index := efficiency_index]
    if (!is.null(kpi_value_per_outcome) && is.finite(as.numeric(kpi_value_per_outcome)[1])) {
      val <- as.numeric(kpi_value_per_outcome)[1]
      kpi_economics[, `:=`(
        kpi_value_per_outcome = val,
        value_contribution = contribution * val,
        value_per_cost = contribution * val / data.table::fifelse(abs(spend) > 1e-8, spend, NA_real_),
        cost_per_value = spend / data.table::fifelse(abs(contribution * val) > 1e-8, contribution * val, NA_real_)
      )]
    }
    data.table::setorderv(kpi_economics, c("signed_economics_flag", "cost_per_outcome"), order = c(1L, 1L))
  }

  kpi_economics_by_channel <- data.table::data.table()
  if (nrow(kpi_economics) && "channel" %in% names(kpi_economics)) {
    kpi_economics_by_channel <- kpi_economics[, .(
      spend = sum(spend, na.rm = TRUE),
      contribution = sum(contribution, na.rm = TRUE),
      n_variables = data.table::uniqueN(variable)
    ), by = .(channel, role)]
    total_channel_spend <- sum(kpi_economics_by_channel$spend, na.rm = TRUE)
    total_channel_contribution <- sum(kpi_economics_by_channel$contribution, na.rm = TRUE)
    kpi_economics_by_channel[, `:=`(
      outcome_per_cost = contribution / data.table::fifelse(abs(spend) > 1e-8, spend, NA_real_),
      cost_per_outcome = spend / data.table::fifelse(contribution > 1e-8, contribution, NA_real_),
      signed_economics_flag = data.table::fifelse(contribution > 1e-8, "positive_economics", data.table::fifelse(contribution < -1e-8, "negative_contribution_diagnostic", "zero_contribution_diagnostic")),
      spend_share = spend / data.table::fifelse(abs(total_channel_spend) > 1e-8, total_channel_spend, NA_real_),
      contribution_share = contribution / data.table::fifelse(abs(total_channel_contribution) > 1e-8, total_channel_contribution, NA_real_)
    )]
    kpi_economics_by_channel[, efficiency_index := contribution_share / data.table::fifelse(abs(spend_share) > 1e-8, spend_share, NA_real_)]
    kpi_economics_by_channel[, fair_share_index := efficiency_index]
    data.table::setorderv(kpi_economics_by_channel, c("signed_economics_flag", "cost_per_outcome"), order = c(1L, 1L))
  }

  kpi_economics_by_rollup_node <- data.table::data.table()
  if (nrow(kpi_economics) && nrow(variable_rollup_map)) {
    econ_rollup <- kpi_economics[variable_rollup_map[, .(
      variable,
      rollup_level,
      rollup_node,
      rollup_node_path,
      is_reporting_channel,
      is_root_node,
      is_leaf_node
    )], on = "variable", allow.cartesian = TRUE, nomatch = 0]
    if (nrow(econ_rollup)) {
      kpi_economics_by_rollup_node <- econ_rollup[, .(
        spend = sum(spend, na.rm = TRUE),
        contribution = sum(contribution, na.rm = TRUE),
        n_variables = data.table::uniqueN(variable),
        variables = paste(sort(unique(variable)), collapse = "|")
      ), by = .(rollup_level, rollup_node, rollup_node_path, is_reporting_channel, is_root_node, is_leaf_node, role)]
      total_rollup_spend <- kpi_economics_by_rollup_node[is_leaf_node == TRUE, sum(spend, na.rm = TRUE)]
      total_rollup_contribution <- kpi_economics_by_rollup_node[is_leaf_node == TRUE, sum(contribution, na.rm = TRUE)]
      kpi_economics_by_rollup_node[, `:=`(
        outcome_per_cost = contribution / data.table::fifelse(abs(spend) > 1e-8, spend, NA_real_),
        cost_per_outcome = spend / data.table::fifelse(contribution > 1e-8, contribution, NA_real_),
        signed_economics_flag = data.table::fifelse(contribution > 1e-8, "positive_economics", data.table::fifelse(contribution < -1e-8, "negative_contribution_diagnostic", "zero_contribution_diagnostic")),
        spend_share = spend / data.table::fifelse(abs(total_rollup_spend) > 1e-8, total_rollup_spend, NA_real_),
        contribution_share = contribution / data.table::fifelse(abs(total_rollup_contribution) > 1e-8, total_rollup_contribution, NA_real_)
      )]
      kpi_economics_by_rollup_node[, efficiency_index := contribution_share / data.table::fifelse(abs(spend_share) > 1e-8, spend_share, NA_real_)]
      kpi_economics_by_rollup_node[, fair_share_index := efficiency_index]
      data.table::setorderv(kpi_economics_by_rollup_node, c("rollup_level", "rollup_node", "signed_economics_flag", "cost_per_outcome"), order = c(1L, 1L, 1L, 1L))
    }
  }

  kpi_economics_by_period <- mdo_empty_kpi_economics(TRUE)
  if (nrow(spend$by_period)) {
    kpi_economics_by_period <- data.table::copy(spend$by_period)
    contrib_period <- contribution_by_period_variable[variable %in% inferred_spend_map$variable, .(
      contribution = sum(contribution, na.rm = TRUE)
    ), by = .(period_granularity, period_start, period_label, period_sort, variable)]
    kpi_economics_by_period[contrib_period, contribution := i.contribution,
                            on = c("period_granularity", "period_start", "period_label", "period_sort", "variable")]
    kpi_economics_by_period[, `:=`(
      outcome_per_cost = contribution / data.table::fifelse(abs(spend) > 1e-8, spend, NA_real_),
      cost_per_outcome = spend / data.table::fifelse(contribution > 1e-8, contribution, NA_real_),
      signed_economics_flag = data.table::fifelse(contribution > 1e-8, "positive_economics", data.table::fifelse(contribution < -1e-8, "negative_contribution_diagnostic", "zero_contribution_diagnostic"))
    )]
    period_totals_econ <- kpi_economics_by_period[, .(
      period_spend_total = sum(spend, na.rm = TRUE),
      period_contribution_total = sum(contribution, na.rm = TRUE)
    ), by = .(period_granularity, period_start, period_label, period_sort)]
    kpi_economics_by_period[period_totals_econ, `:=`(
      spend_share = spend / data.table::fifelse(abs(i.period_spend_total) > 1e-8, i.period_spend_total, NA_real_),
      contribution_share = contribution / data.table::fifelse(abs(i.period_contribution_total) > 1e-8, i.period_contribution_total, NA_real_)
    ), on = c("period_granularity", "period_start", "period_label", "period_sort")]
    kpi_economics_by_period[, efficiency_index := contribution_share / data.table::fifelse(abs(spend_share) > 1e-8, spend_share, NA_real_)]
    kpi_economics_by_period[, fair_share_index := efficiency_index]
    data.table::setorder(kpi_economics_by_period, period_sort, variable)
  }

  diagnostic_flags <- data.table::data.table(
    check = character(),
    severity = character(),
    detail = character()
  )
  add_flag <- function(check, severity, detail) {
    diagnostic_flags <<- data.table::rbindlist(list(
      diagnostic_flags,
      data.table::data.table(check = check, severity = severity, detail = detail)
    ), use.names = TRUE, fill = TRUE)
  }
  if (!nrow(kpi_economics)) add_flag("spend_data_missing", "info", "No spend map/raw modcut was available; KPI economics tables are empty.")
  if (nrow(kpi_economics) && any(kpi_economics$spend_status != "ok" | !is.finite(kpi_economics$spend))) {
    add_flag("spend_mapping_issue", "warning", paste(kpi_economics[spend_status != "ok" | !is.finite(spend), variable], collapse = ", "))
  }
  neg_media <- contribution_by_variable[role == "media" & contribution < 0, variable]
  if (length(neg_media)) add_flag("negative_media_contribution", "warning", paste(neg_media, collapse = ", "))
  if (nrow(fit_diagnostics) && is.finite(fit_diagnostics$residual_share_of_actual[1]) &&
      abs(fit_diagnostics$residual_share_of_actual[1]) > 0.10) {
    add_flag("large_total_residual", "warning", paste0("Residual share of actual KPI is ", round(fit_diagnostics$residual_share_of_actual[1], 4)))
  }
  if (nrow(fit_diagnostics) && is.finite(fit_diagnostics$mape[1]) && fit_diagnostics$mape[1] > 0.20) {
    add_flag("high_mape", "warning", paste0("MAPE is ", round(fit_diagnostics$mape[1], 4)))
  }
  if (nrow(kpi_economics) && any(!is.finite(kpi_economics$cost_per_outcome) | kpi_economics$cost_per_outcome <= 0, na.rm = TRUE)) {
    bad <- kpi_economics[!is.finite(cost_per_outcome) | cost_per_outcome <= 0, variable]
    add_flag("undefined_or_negative_cost_per_outcome", "review", paste(bad, collapse = ", "))
  }
  if (!nrow(diagnostic_flags)) add_flag("no_reporting_flags", "ok", "No reporting guardrail flags were triggered.")

  top_contributor <- contribution_by_variable[role != "residual"][order(-abs(contribution))][1, variable]
  total_spend <- if (nrow(kpi_economics)) sum(kpi_economics$spend, na.rm = TRUE) else NA_real_
  media_contribution <- if (nrow(kpi_economics)) sum(kpi_economics$contribution, na.rm = TRUE) else NA_real_
  executive_summary <- data.table::data.table(
    generated_at = as.character(Sys.time()),
    reporting_period_granularity = period_granularity,
    period_min = if (!is.na(time_col) && time_col %in% names(wide)) as.character(min(wide[[time_col]], na.rm = TRUE)) else NA_character_,
    period_max = if (!is.na(time_col) && time_col %in% names(wide)) as.character(max(wide[[time_col]], na.rm = TRUE)) else NA_character_,
    modeled_rows = nrow(wide),
    variables = length(all_variables),
    media_variables = length(media_variables),
    actual_kpi = total_actual,
    predicted_kpi = total_pred,
    modeled_contribution = modeled_total,
    residual = mdo_sum_or_na(wide$residual),
    rmse = if (nrow(fit_diagnostics)) fit_diagnostics$rmse[1] else NA_real_,
    mape = if (nrow(fit_diagnostics)) fit_diagnostics$mape[1] else NA_real_,
    smape = if (nrow(fit_diagnostics)) fit_diagnostics$smape[1] else NA_real_,
    r_squared = if (nrow(fit_diagnostics)) fit_diagnostics$r_squared[1] else NA_real_,
    top_contributor = top_contributor,
    total_spend = total_spend,
    media_outcome_per_cost = media_contribution / data.table::fifelse(abs(total_spend) > 1e-8, total_spend, NA_real_),
    media_cost_per_outcome = total_spend / data.table::fifelse(media_contribution > 1e-8, media_contribution, NA_real_),
    reporting_flags = paste(diagnostic_flags[severity != "ok", check], collapse = "|")
  )
  baseline_control_contribution <- long[role == "baseline_control", sum(contribution, na.rm = TRUE)]
  other_model_contribution <- long[role == "other_model_term", sum(contribution, na.rm = TRUE)]
  residual_total <- mdo_sum_or_na(wide$residual)
  funnel_summary <- data.table::data.table(
    stage_order = seq_len(8L),
    stage = c(
      "Actual KPI",
      "Fitted KPI",
      "Modeled contribution",
      "Baseline/control contribution",
      "Media contribution",
      "Other modeled contribution",
      "Residual",
      "Media spend"
    ),
    metric_type = c("kpi", "kpi", "kpi", "kpi", "kpi", "kpi", "kpi", "cost"),
    value = c(
      total_actual,
      total_pred,
      modeled_total,
      baseline_control_contribution,
      media_contribution,
      other_model_contribution,
      residual_total,
      total_spend
    )
  )
  funnel_summary[, share_of_actual_kpi := data.table::fifelse(metric_type == "kpi", value / data.table::fifelse(abs(total_actual) > 1e-8, total_actual, NA_real_), NA_real_)]
  funnel_summary[, metric_note := data.table::fifelse(
    metric_type == "cost",
    "Cost metric; not additive to KPI stages.",
    "KPI-unit stage used for model/decomposition funnel."
  )]

  period_slicer_index <- unique(contribution_by_period_variable[, .(period_granularity, period_start, period_label, period_sort)])
  data.table::setorder(period_slicer_index, period_sort)

  optimizer_tables <- mdo_build_optimizer_deck_tables(optimizer_output)

  out <- list(
    executive_summary = executive_summary[],
    contribution_by_variable = contribution_by_variable[],
    contribution_by_channel = contribution_by_channel[],
    contribution_by_rollup_node = contribution_by_rollup_node[],
    contribution_by_period_variable = contribution_by_period_variable[],
    contribution_by_period_channel = contribution_by_period_channel[],
    contribution_by_period_rollup_node = contribution_by_period_rollup_node[],
    period_kpi_change = period_kpi_change[],
    period_due_to_variable = period_due_to_variable[],
    period_due_to_channel = period_due_to_channel[],
    contribution_by_group_variable = contribution_by_group_variable[],
    fit_diagnostics = fit_diagnostics[],
    fit_by_period = fit_by_period[],
    fit_by_group = fit_by_group[],
    funnel_summary = funnel_summary[],
    kpi_economics = kpi_economics[],
    kpi_economics_by_channel = kpi_economics_by_channel[],
    kpi_economics_by_rollup_node = kpi_economics_by_rollup_node[],
    kpi_economics_by_period = kpi_economics_by_period[],
    diagnostic_flags = diagnostic_flags[],
    period_slicer_index = period_slicer_index[],
    spend_map = inferred_spend_map[],
    variable_rollup_map = variable_rollup_map[],
    channel_map_normalized = channel_map_normalized[],
    standardized_long = long[],
    standardized_wide = wide[]
  )
  out <- c(out, optimizer_tables)
  out$chart_registry <- mdo_build_chart_registry(out)
  out
}

mdo_chart_palette <- function(n) {
  base <- c("#2563EB", "#16A34A", "#F97316", "#7C3AED", "#DC2626", "#0891B2", "#4B5563", "#DB2777", "#65A30D", "#9333EA")
  rep(base, length.out = n)
}

mdo_save_plot <- function(p, path, width = 10, height = 6) {
  ggplot2::ggsave(filename = path, plot = p, width = width, height = height, dpi = 160, bg = "white")
  path
}

write_mmm_deck_charts <- function(report_tables,
                                  output_dir,
                                  top_n = 12) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Package 'ggplot2' is not installed; chart PNGs were skipped.")
    return(character())
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  files <- character()
  theme_deck <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  cbv <- data.table::copy(report_tables$contribution_by_variable)
  cbv <- cbv[role != "residual"][order(-abs(contribution))]
  if (nrow(cbv)) {
    p <- ggplot2::ggplot(cbv[seq_len(min(.N, top_n))], ggplot2::aes(x = stats::reorder(variable, contribution), y = contribution, fill = role)) +
      ggplot2::geom_col(width = 0.72) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c(media = "#2563EB", baseline_control = "#16A34A", other_model_term = "#F97316")) +
      ggplot2::labs(title = "Contribution by variable", y = "KPI contribution") +
      theme_deck
    files <- c(files, mdo_save_plot(p, file.path(output_dir, "contribution_by_variable.png"), 9, 5.5))
  }

  cpv <- data.table::copy(report_tables$contribution_by_period_variable)
  cpv <- cpv[role != "residual"]
  if (nrow(cpv)) {
    keep_vars <- cbv[seq_len(min(.N, top_n)), variable]
    cpv[, plot_variable := data.table::fifelse(variable %in% keep_vars, variable, "Other")]
    cpv_plot <- cpv[, .(contribution = sum(contribution, na.rm = TRUE)), by = .(period_sort, period_label, plot_variable)]
    cpv_plot[, plot_variable := factor(plot_variable, levels = unique(c(keep_vars, "Other")))]
    pal <- stats::setNames(mdo_chart_palette(length(unique(cpv_plot$plot_variable))), unique(cpv_plot$plot_variable))
    p <- ggplot2::ggplot(cpv_plot, ggplot2::aes(x = stats::reorder(period_label, period_sort), y = contribution, fill = plot_variable)) +
      ggplot2::geom_col(width = 0.82) +
      ggplot2::scale_fill_manual(values = pal) +
      ggplot2::labs(title = "Contribution trend", y = "KPI contribution") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      theme_deck
    files <- c(files, mdo_save_plot(p, file.path(output_dir, "contribution_trend.png"), 11, 6))
  }

  fbp <- data.table::copy(report_tables$fit_by_period)
  if (nrow(fbp) && all(c("actual", "pred") %in% names(fbp))) {
    fp <- data.table::melt(fbp, id.vars = c("period_sort", "period_label"), measure.vars = c("actual", "pred"),
                           variable.name = "series", value.name = "kpi")
    p <- ggplot2::ggplot(fp, ggplot2::aes(x = stats::reorder(period_label, period_sort), y = kpi, color = series, group = series)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.7) +
      ggplot2::scale_color_manual(values = c(actual = "#111827", pred = "#2563EB")) +
      ggplot2::labs(title = "Actual vs fitted KPI", y = "KPI") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      theme_deck
    files <- c(files, mdo_save_plot(p, file.path(output_dir, "actual_vs_fitted.png"), 11, 5.5))

    p_res <- ggplot2::ggplot(fbp, ggplot2::aes(x = stats::reorder(period_label, period_sort), y = residual)) +
      ggplot2::geom_hline(yintercept = 0, color = "#6B7280") +
      ggplot2::geom_col(fill = "#DC2626", width = 0.78) +
      ggplot2::labs(title = "Residuals by period", y = "Actual minus fitted") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      theme_deck
    files <- c(files, mdo_save_plot(p_res, file.path(output_dir, "residuals_by_period.png"), 11, 4.8))
  }

  econ <- data.table::copy(report_tables$kpi_economics)
  econ <- econ[is.finite(cost_per_outcome) & (!"signed_economics_flag" %in% names(econ) | signed_economics_flag == "positive_economics")][order(cost_per_outcome)]
  if (nrow(econ)) {
    p <- ggplot2::ggplot(econ[seq_len(min(.N, top_n))], ggplot2::aes(x = stats::reorder(variable, -cost_per_outcome), y = cost_per_outcome)) +
      ggplot2::geom_col(fill = "#0891B2", width = 0.72) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Cost per KPI outcome", y = "Cost per outcome") +
      theme_deck
    files <- c(files, mdo_save_plot(p, file.path(output_dir, "cost_per_outcome.png"), 9, 5))

    econ[, bubble_size__ := pmax(abs(contribution), 1e-8)]
    p2 <- ggplot2::ggplot(econ, ggplot2::aes(x = spend, y = contribution, size = bubble_size__, color = fair_share_index, label = variable)) +
      ggplot2::geom_point(alpha = 0.72) +
      ggplot2::geom_text(vjust = -0.8, check_overlap = TRUE, size = 3.2) +
      ggplot2::scale_size_continuous(range = c(3, 11), guide = "none") +
      ggplot2::scale_color_gradient2(low = "#DC2626", mid = "#9CA3AF", high = "#16A34A", midpoint = 1, na.value = "#2563EB") +
      ggplot2::labs(title = "Spend vs KPI contribution bubble chart", x = "Spend", y = "Contribution", color = "Fair-share index") +
      theme_deck
    files <- c(files, mdo_save_plot(p2, file.path(output_dir, "spend_vs_contribution.png"), 7.5, 5.5))
  }

  funnel <- data.table::copy(report_tables$funnel_summary)
  funnel <- funnel[metric_type == "kpi" & is.finite(value)]
  if (nrow(funnel)) {
    p <- ggplot2::ggplot(funnel, ggplot2::aes(x = stats::reorder(stage, -stage_order), y = value)) +
      ggplot2::geom_col(fill = "#4F46E5", width = 0.72) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "KPI decomposition funnel", y = "KPI units") +
      theme_deck
    files <- c(files, mdo_save_plot(p, file.path(output_dir, "kpi_decomposition_funnel.png"), 8.5, 5.2))
  }

  opt_plan <- if ("optimizer_plan" %in% names(report_tables)) data.table::copy(report_tables$optimizer_plan) else data.table::data.table()
  if (nrow(opt_plan) && all(c("variable", "current_spend", "recommended_spend") %in% names(opt_plan))) {
    op <- opt_plan[, .(variable, current_spend = mdo_safe_num(current_spend), recommended_spend = mdo_safe_num(recommended_spend))]
    op <- op[is.finite(current_spend) | is.finite(recommended_spend)]
    if (nrow(op)) {
      op_long <- data.table::melt(op, id.vars = "variable", measure.vars = c("current_spend", "recommended_spend"),
                                  variable.name = "plan", value.name = "spend")
      op_long[, plan := data.table::fifelse(plan == "current_spend", "Current", "Recommended")]
      p <- ggplot2::ggplot(op_long, ggplot2::aes(x = stats::reorder(variable, spend, FUN = max, na.rm = TRUE), y = spend, fill = plan)) +
        ggplot2::geom_col(position = "dodge", width = 0.74) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = c(Current = "#6B7280", Recommended = "#2563EB")) +
        ggplot2::labs(title = "Current vs recommended spend", y = "Spend") +
        theme_deck
      files <- c(files, mdo_save_plot(p, file.path(output_dir, "optimizer_current_vs_recommended_spend.png"), 9, 5.5))
    }
  }

  opt_comp <- if ("optimizer_scenario_comparison" %in% names(report_tables)) data.table::copy(report_tables$optimizer_scenario_comparison) else data.table::data.table()
  if (nrow(opt_comp) && all(c("plan_name", "incremental_contribution") %in% names(opt_comp))) {
    oc <- opt_comp[is.finite(mdo_safe_num(incremental_contribution))]
    if (nrow(oc)) {
      oc[, incremental_contribution := mdo_safe_num(incremental_contribution)]
      p <- ggplot2::ggplot(oc, ggplot2::aes(x = stats::reorder(plan_name, incremental_contribution), y = incremental_contribution, fill = plan_type)) +
        ggplot2::geom_col(width = 0.72) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = c(scenario = "#0891B2", optimized = "#16A34A")) +
        ggplot2::labs(title = "Scenario incremental KPI contribution", y = "Incremental contribution") +
        theme_deck
      files <- c(files, mdo_save_plot(p, file.path(output_dir, "optimizer_scenario_incremental_contribution.png"), 9, 5.5))
    }
  }

  opt_curves <- if ("optimizer_response_curves" %in% names(report_tables)) data.table::copy(report_tables$optimizer_response_curves) else data.table::data.table()
  if (nrow(opt_curves) && all(c("variable", "spend_multiplier", "contribution") %in% names(opt_curves))) {
    rc <- opt_curves[is.finite(mdo_safe_num(spend_multiplier)) & is.finite(mdo_safe_num(contribution))]
    if (nrow(rc)) {
      rc[, `:=`(spend_multiplier = mdo_safe_num(spend_multiplier), contribution = mdo_safe_num(contribution))]
      top_vars <- rc[, .(peak_contribution = max(contribution, na.rm = TRUE)), by = variable][order(-abs(peak_contribution))][seq_len(min(.N, top_n)), variable]
      rc <- rc[variable %in% top_vars]
      current_points <- rc[, .SD[which.min(abs(spend_multiplier - 1))], by = variable]
      pal <- stats::setNames(mdo_chart_palette(length(unique(rc$variable))), unique(rc$variable))
      p <- ggplot2::ggplot(rc, ggplot2::aes(x = spend_multiplier, y = contribution, color = variable)) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(data = current_points, ggplot2::aes(x = spend_multiplier, y = contribution), size = 2.4, inherit.aes = TRUE) +
        ggplot2::scale_color_manual(values = pal) +
        ggplot2::labs(title = "Response curves", x = "Spend/support multiplier", y = "Expected KPI contribution") +
        theme_deck
      files <- c(files, mdo_save_plot(p, file.path(output_dir, "optimizer_response_curves.png"), 9.5, 5.6))
    }
    if ("mroi" %in% names(opt_curves)) {
      mr <- opt_curves[is.finite(mdo_safe_num(spend_multiplier)) & is.finite(mdo_safe_num(mroi))]
      if (nrow(mr)) {
        mr[, `:=`(spend_multiplier = mdo_safe_num(spend_multiplier), mroi = mdo_safe_num(mroi))]
        mr <- mr[variable %in% unique(rc$variable)]
        p <- ggplot2::ggplot(mr, ggplot2::aes(x = spend_multiplier, y = mroi, color = variable)) +
          ggplot2::geom_hline(yintercept = 0, color = "#9CA3AF") +
          ggplot2::geom_line(linewidth = 0.8) +
          ggplot2::scale_color_manual(values = stats::setNames(mdo_chart_palette(length(unique(mr$variable))), unique(mr$variable))) +
          ggplot2::labs(title = "Marginal response curves", x = "Spend/support multiplier", y = "Marginal KPI per cost") +
          theme_deck
        files <- c(files, mdo_save_plot(p, file.path(output_dir, "optimizer_mroi_curves.png"), 9.5, 5.6))
      }
    }
  }

  sat <- if ("optimizer_saturation_headroom" %in% names(report_tables)) data.table::copy(report_tables$optimizer_saturation_headroom) else data.table::data.table()
  if (nrow(sat) && all(c("variable", "pct_of_peak_grid_contribution") %in% names(sat))) {
    sh <- sat[is.finite(mdo_safe_num(pct_of_peak_grid_contribution))]
    if (nrow(sh)) {
      sh[, pct_of_peak_grid_contribution := pmax(pmin(mdo_safe_num(pct_of_peak_grid_contribution), 1), 0)]
      p <- ggplot2::ggplot(sh[order(pct_of_peak_grid_contribution)][seq_len(min(.N, top_n))],
                           ggplot2::aes(x = stats::reorder(variable, pct_of_peak_grid_contribution), y = pct_of_peak_grid_contribution, fill = saturation_band)) +
        ggplot2::geom_col(width = 0.72) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
        ggplot2::labs(title = "Saturation and response headroom", y = "Current share of peak grid contribution") +
        theme_deck
      files <- c(files, mdo_save_plot(p, file.path(output_dir, "optimizer_saturation_headroom.png"), 9, 5.5))
    }
  }

  files
}

mdo_escape_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

mdo_format_cell <- function(x) {
  if (is.numeric(x)) {
    out <- ifelse(is.na(x), "", ifelse(abs(x) >= 1000, format(round(x, 1), big.mark = ",", scientific = FALSE), signif(x, 4)))
    return(out)
  }
  if (inherits(x, "Date")) return(as.character(x))
  mdo_escape_html(x)
}

mdo_table_html <- function(dt, max_rows = 30) {
  if (is.null(dt) || !nrow(dt)) return("<p class=\"empty\">No rows available.</p>")
  show <- utils::head(data.table::as.data.table(dt), max_rows)
  header <- paste0("<th>", mdo_escape_html(names(show)), "</th>", collapse = "")
  rows <- apply(show, 1, function(r) paste0("<tr>", paste0("<td>", mdo_format_cell(r), "</td>", collapse = ""), "</tr>"))
  paste0("<table><thead><tr>", header, "</tr></thead><tbody>", paste(rows, collapse = "\n"), "</tbody></table>")
}

write_mmm_deck_html <- function(report_tables,
                                chart_files = character(),
                                output_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  summary <- report_tables$executive_summary[1]
  cards <- c(
    paste0("Actual KPI|", mdo_format_cell(summary$actual_kpi)),
    paste0("Predicted KPI|", mdo_format_cell(summary$predicted_kpi)),
    paste0("R-squared|", mdo_format_cell(summary$r_squared)),
    paste0("Cost per outcome|", mdo_format_cell(summary$media_cost_per_outcome))
  )
  card_html <- paste(vapply(cards, function(x) {
    parts <- strsplit(x, "\\|", fixed = FALSE)[[1]]
    paste0("<div class=\"card\"><div class=\"label\">", mdo_escape_html(parts[1]), "</div><div class=\"value\">", parts[2], "</div></div>")
  }, character(1)), collapse = "\n")

  chart_html <- ""
  if (length(chart_files)) {
    chart_html <- paste(vapply(chart_files, function(path) {
      rel <- basename(path)
      title <- tools::file_path_sans_ext(gsub("_", " ", basename(path)))
      paste0("<figure><img src=\"charts/", rel, "\" alt=\"", mdo_escape_html(title), "\"><figcaption>", mdo_escape_html(title), "</figcaption></figure>")
    }, character(1)), collapse = "\n")
  }

  periods <- report_tables$period_slicer_index
  period_options <- "<option value=\"__all__\">All periods</option>"
  if (nrow(periods)) {
    period_options <- paste0(period_options, paste0("<option value=\"", mdo_escape_html(periods$period_label), "\">", mdo_escape_html(periods$period_label), "</option>", collapse = ""))
  }
  period_panels <- "<section class=\"period-panel\" data-period=\"__all__\"><h2>All periods</h2>"
  period_panels <- paste0(
    period_panels,
    "<h3>Contribution by variable</h3>",
    mdo_table_html(report_tables$contribution_by_variable[, .(variable, role, contribution, share_of_actual_kpi)], 50),
    "<h3>KPI economics</h3>",
    mdo_table_html(report_tables$kpi_economics[, intersect(c("variable", "spend", "contribution", "outcome_per_cost", "cost_per_outcome", "efficiency_index"), names(report_tables$kpi_economics)), with = FALSE], 50),
    "</section>"
  )
  if (nrow(periods)) {
    panel_rows <- vapply(periods$period_label, function(pl) {
      ctab <- report_tables$contribution_by_period_variable[period_label == pl, .(variable, role, contribution, share_of_period_model_contribution)]
      etab <- report_tables$kpi_economics_by_period[period_label == pl, intersect(c("variable", "spend", "contribution", "outcome_per_cost", "cost_per_outcome"), names(report_tables$kpi_economics_by_period)), with = FALSE]
      paste0("<section class=\"period-panel\" data-period=\"", mdo_escape_html(pl), "\"><h2>", mdo_escape_html(pl), "</h2>",
             "<h3>Contribution by variable</h3>", mdo_table_html(ctab, 40),
             "<h3>KPI economics</h3>", mdo_table_html(etab, 40),
             "</section>")
    }, character(1))
    period_panels <- paste0(period_panels, paste(panel_rows, collapse = "\n"))
  }

  html <- paste0(
    "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>MMM Deck Output Dashboard</title>",
    "<style>",
    "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;margin:0;background:#f8fafc;color:#111827;}",
    "header{background:#111827;color:white;padding:28px 36px;} header h1{margin:0;font-size:28px;letter-spacing:0;} header p{margin:8px 0 0;color:#d1d5db;}",
    "main{max-width:1180px;margin:0 auto;padding:24px 28px 48px;} .cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:12px;margin:0 0 22px;}",
    ".card{background:white;border:1px solid #e5e7eb;border-radius:8px;padding:14px 16px;} .label{color:#6b7280;font-size:12px;text-transform:uppercase;} .value{font-size:22px;font-weight:700;margin-top:4px;}",
    ".charts{display:grid;grid-template-columns:repeat(auto-fit,minmax(360px,1fr));gap:18px;margin:18px 0 28px;} figure{background:white;border:1px solid #e5e7eb;border-radius:8px;margin:0;padding:10px;} figure img{width:100%;height:auto;display:block;} figcaption{font-size:12px;color:#6b7280;padding:8px 4px 2px;text-transform:capitalize;}",
    ".toolbar{display:flex;align-items:center;gap:10px;margin:18px 0;} select{font-size:14px;padding:8px 10px;border:1px solid #d1d5db;border-radius:6px;background:white;}",
    "section{background:white;border:1px solid #e5e7eb;border-radius:8px;padding:18px;margin:16px 0;} h2{margin:0 0 12px;font-size:20px;} h3{margin:18px 0 8px;font-size:15px;color:#374151;}",
    "table{width:100%;border-collapse:collapse;font-size:13px;} th,td{border-bottom:1px solid #e5e7eb;text-align:left;padding:7px 8px;vertical-align:top;} th{background:#f3f4f6;color:#374151;font-weight:700;} .empty{color:#6b7280;}",
    ".flags td:first-child{font-weight:700;} footer{color:#6b7280;font-size:12px;margin-top:22px;}",
    "</style></head><body>",
    "<header><h1>MMM Deck Output Dashboard</h1><p>Static reporting pack built from decomposition outputs, optional modcut spend, and optional optimizer scenarios.</p></header>",
    "<main><div class=\"cards\">", card_html, "</div>",
    "<section><h2>Reporting flags</h2><div class=\"flags\">", mdo_table_html(report_tables$diagnostic_flags, 20), "</div></section>",
    "<div class=\"charts\">", chart_html, "</div>",
    "<section><h2>KPI decomposition funnel</h2>", mdo_table_html(report_tables$funnel_summary, 20), "</section>",
    "<section><h2>Optimizer scenarios</h2>", mdo_table_html(report_tables$optimizer_scenario_comparison, 30),
    "<h3>Recommended plan</h3>", mdo_table_html(report_tables$optimizer_plan[, intersect(c("variable", "current_spend", "recommended_spend", "spend_change", "current_contribution", "expected_contribution", "contribution_change", "expected_roi", "expected_mroi", "expected_cost_per_kpi"), names(report_tables$optimizer_plan)), with = FALSE], 40),
    "<h3>Optimizer chart registry</h3>", mdo_table_html(report_tables$chart_registry, 30), "</section>",
    "<div class=\"toolbar\"><label for=\"periodSelect\">Period</label><select id=\"periodSelect\">", period_options, "</select></div>",
    period_panels,
    "<footer>Use the CSV tables for deck chart rebuilds. Cost metrics are generic KPI economics: outcome per cost and cost per outcome, not revenue ROI unless a KPI value is supplied.</footer>",
    "</main><script>",
    "const select=document.getElementById('periodSelect');",
    "function updatePeriod(){const val=select.value;document.querySelectorAll('.period-panel').forEach(p=>{p.style.display=(val==='__all__'?p.dataset.period==='__all__':p.dataset.period===val)?'block':'none';});}",
    "select.addEventListener('change',updatePeriod);updatePeriod();",
    "</script></body></html>"
  )
  writeLines(html, output_path, useBytes = TRUE)
  output_path
}

write_mmm_deck_excel <- function(report_tables,
                                 output_path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("Package 'openxlsx' is not installed; Excel workbook was skipped.")
    return(NA_character_)
  }
  wb <- openxlsx::createWorkbook()
  table_names <- names(report_tables)[vapply(report_tables, function(x) data.table::is.data.table(x) || is.data.frame(x), logical(1))]
  table_names <- setdiff(table_names, c("standardized_long", "standardized_wide"))
  for (nm in table_names) {
    sheet <- substr(gsub("[^A-Za-z0-9_]", "_", nm), 1, 31)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeDataTable(wb, sheet, data.table::as.data.table(report_tables[[nm]]))
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  }
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  output_path
}

write_mmm_deck_shiny_app <- function(report_tables,
                                     output_dir,
                                     prefix = "") {
  app_dir <- file.path(output_dir, "shiny_app")
  dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)
  data_path <- file.path(app_dir, "mmm_report_tables.rds")
  saveRDS(report_tables, data_path)

  app_lines <- c(
    'required <- c("shiny", "plotly", "DT", "ggplot2", "data.table")',
    'missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]',
    'if (length(missing)) stop("Install required packages first: install.packages(c(", paste(shQuote(missing), collapse = ", "), "))")',
    'suppressPackageStartupMessages(invisible(lapply(required, library, character.only = TRUE)))',
    'tables <- readRDS("mmm_report_tables.rds")',
    'fmt <- function(x) {',
    '  x <- suppressWarnings(as.numeric(x))',
    '  ifelse(is.na(x), "", ifelse(abs(x) >= 1000, format(round(x, 1), big.mark = ",", scientific = FALSE), signif(x, 4)))',
    '}',
    '`%||%` <- function(a, b) {',
    '  if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a',
    '}',
    'table_or_empty <- function(name) {',
    '  if (!is.null(tables[[name]])) as.data.table(tables[[name]]) else data.table()',
    '}',
    'choices_from <- function(table_name, col) {',
    '  dt <- table_or_empty(table_name)',
    '  if (nrow(dt) && col %in% names(dt)) sort(unique(as.character(dt[[col]]))) else character()',
    '}',
    'periods <- table_or_empty("period_slicer_index")',
    'period_choices <- c("All periods" = "__all__")',
    'if (nrow(periods)) period_choices <- c(period_choices, stats::setNames(periods$period_label, periods$period_label))',
    'variable_choices <- sort(unique(c(choices_from("contribution_by_variable", "variable"), choices_from("kpi_economics", "variable"), choices_from("optimizer_response_curves", "variable"))))',
    'curve_choices <- choices_from("optimizer_response_curves", "variable")',
    'role_choices <- c("All roles" = "__all__", stats::setNames(sort(unique(as.character(table_or_empty("contribution_by_variable")$role))), sort(unique(as.character(table_or_empty("contribution_by_variable")$role)))))',
    'fit_overlay_choices <- c("None" = "__none__", stats::setNames(variable_choices, variable_choices))',
    'curve_metric_choices <- intersect(c("contribution", "contribution_vs_current", "roi", "mroi", "cost_per_kpi", "value_per_cost"), names(table_or_empty("optimizer_response_curves")))',
    'if (!length(curve_metric_choices)) curve_metric_choices <- "contribution"',
    'econ_metric_choices <- intersect(c("cost_per_outcome", "outcome_per_cost", "value_per_cost", "cost_per_value", "fair_share_index", "efficiency_index", "spend_share", "contribution_share"), names(table_or_empty("kpi_economics")))',
    'if (!length(econ_metric_choices)) econ_metric_choices <- "cost_per_outcome"',
    'scenario_metric_choices <- intersect(c("incremental_contribution", "contribution", "roi", "cost_per_kpi", "expected_profit", "q05_profit", "probability_profit_positive", "probability_incremental_contribution_positive"), names(table_or_empty("optimizer_scenario_comparison")))',
    'if (!length(scenario_metric_choices)) scenario_metric_choices <- "incremental_contribution"',
    'card <- function(label, value) div(class = "metric-card", div(class = "metric-label", label), div(class = "metric-value", value))',
    'ui <- fluidPage(',
    '  tags$head(tags$style(HTML("',
    '    body { background:#f8fafc; color:#111827; font-family:-apple-system,BlinkMacSystemFont,Segoe UI,sans-serif; }',
    '    .title-row { margin:18px 0 8px; }',
    '    .metric-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:12px; margin:16px 0; }',
    '    .metric-card { background:white; border:1px solid #e5e7eb; border-radius:8px; padding:14px 16px; }',
    '    .metric-label { color:#6b7280; font-size:12px; text-transform:uppercase; }',
    '    .metric-value { font-size:22px; font-weight:700; margin-top:4px; }',
    '    .panel { background:white; border:1px solid #e5e7eb; border-radius:8px; padding:16px; margin-bottom:16px; }',
    '    .control-panel { background:white; border:1px solid #e5e7eb; border-radius:8px; padding:14px 16px; margin-bottom:16px; }',
    '    .selectize-control { max-width:100%; }',
    '    .tab-content { padding-top:8px; }',
    '  "))),',
    '  div(class = "title-row", h2("MMM Deck Output Dashboard"), p("Interactive review layer for decomposition, response curves, KPI economics, optimizer scenarios, and fit diagnostics.")),',
    '  uiOutput("theme_css"),',
    '  uiOutput("metric_cards"),',
    '  div(class = "control-panel", fluidRow(',
    '    column(2, selectInput("period", "Period", choices = period_choices, selected = "__all__")),',
    '    column(4, selectizeInput("focus_variable", "Focus variable", choices = c("All variables" = "__all__", stats::setNames(variable_choices, variable_choices)), selected = "__all__", multiple = FALSE)),',
    '    column(2, selectInput("role_filter", "Role", choices = role_choices, selected = "__all__")),',
    '    column(2, numericInput("top_n", "Top N", value = 12, min = 3, max = 50, step = 1)),',
    '    column(2, actionButton("open_theme", "Theme / Format"))',
    '  )),',
    '  tabsetPanel(',
    '    tabPanel("Overview", br(), fluidRow(column(8, div(class = "panel", selectInput("fit_overlay_variable", "Fit chart right-axis overlay", choices = fit_overlay_choices, selected = "__none__"), plotlyOutput("actual_fit", height = "450px"))), column(4, div(class = "panel", plotlyOutput("cost_bar", height = "450px")))), fluidRow(column(6, div(class = "panel", plotlyOutput("contribution_bar", height = "430px"))), column(6, div(class = "panel", plotlyOutput("spend_share_plot", height = "430px"))))),',
    '    tabPanel("Curves", br(), div(class = "control-panel", fluidRow(column(4, selectizeInput("curve_variable", "Curve variable", choices = curve_choices, selected = if (length(curve_choices)) curve_choices[1] else character(), multiple = FALSE)), column(3, selectInput("curve_metric", "Curve readout", choices = curve_metric_choices, selected = if ("contribution" %in% curve_metric_choices) "contribution" else curve_metric_choices[1])), column(3, checkboxInput("curve_show_interval", "Show q05/q95 band", value = TRUE)))), div(class = "panel", h4("Response / economics curve"), plotlyOutput("optimizer_curve_plot", height = "520px")), div(class = "panel", h4("Curve data"), DTOutput("curve_table"))),',
    '    tabPanel("Contribution", br(), fluidRow(column(7, div(class = "panel", h4("Contribution over time"), plotlyOutput("contribution_trend_plot", height = "430px"))), column(5, div(class = "panel", h4("Period change due-to"), plotlyOutput("due_to_plot", height = "430px")))), div(class = "panel", h4("Contribution by variable"), DTOutput("contribution_table")), div(class = "panel", h4("Contribution trend data"), DTOutput("trend_table"))),',
    '    tabPanel("KPI Economics", br(), div(class = "control-panel", selectInput("econ_metric", "Economics metric", choices = econ_metric_choices, selected = econ_metric_choices[1])), fluidRow(column(6, div(class = "panel", plotlyOutput("spend_scatter", height = "420px"))), column(6, div(class = "panel", plotlyOutput("econ_rank_plot", height = "420px")))), div(class = "panel", DTOutput("econ_table"))),',
    '    tabPanel("Optimizer", br(), div(class = "control-panel", fluidRow(column(4, selectInput("scenario_metric", "Scenario metric", choices = scenario_metric_choices, selected = scenario_metric_choices[1])), column(8, plotlyOutput("optimizer_scenario_plot", height = "330px")))), fluidRow(column(6, div(class = "panel", plotlyOutput("optimizer_spend_plot", height = "420px"))), column(6, div(class = "panel", plotlyOutput("optimizer_saturation_plot", height = "420px")))), div(class = "panel", h4("Recommended plan"), DTOutput("optimizer_plan_table")), div(class = "panel", h4("Scenario comparison"), DTOutput("optimizer_scenario_table"))),',
    '    tabPanel("Posterior / Uncertainty", br(), fluidRow(column(6, div(class = "panel", h4("Scenario contribution uncertainty"), plotlyOutput("scenario_uncertainty_plot", height = "420px"))), column(6, div(class = "panel", h4("Curve uncertainty bands"), DTOutput("curve_uncertainty_table")))), div(class = "panel", h4("Scenario uncertainty table"), DTOutput("scenario_uncertainty_table")), div(class = "panel", h4("Optimization uncertainty table"), DTOutput("optimization_uncertainty_table"))),',
    '    tabPanel("Diagnostics", br(), div(class = "panel", DTOutput("flags_table")), div(class = "panel", DTOutput("fit_table")), div(class = "panel", plotlyOutput("residual_plot", height = "380px")), div(class = "panel", h4("Chart registry"), DTOutput("chart_registry_table")))',
    '  )',
    ')',
    'server <- function(input, output, session) {',
    '  preset_palettes <- list(',
    '    "Executive blue" = c("#2563EB", "#0891B2", "#F59E0B", "#10B981", "#8B5CF6", "#F43F5E", "#64748B", "#0F172A"),',
    '    "Client neutral" = c("#0F172A", "#2563EB", "#64748B", "#D97706", "#059669", "#7C3AED", "#BE123C", "#0891B2"),',
    '    "High contrast" = c("#111827", "#1D4ED8", "#B45309", "#047857", "#9333EA", "#BE185D", "#0E7490", "#4B5563")',
    '  )',
    '  parse_hex_colors <- function(txt) {',
    '    if (is.null(txt) || !nzchar(txt)) return(character())',
    '    bits <- unlist(strsplit(gsub(",", " ", txt), " +", fixed = FALSE))',
    '    bits <- bits[nzchar(bits)]',
    '    bits <- ifelse(substr(bits, 1, 1) == "#", bits, paste0("#", bits))',
    '    bits[grepl("^#[0-9A-Fa-f]{6}$", bits)]',
    '  }',
    '  theme_state <- reactiveValues(',
    '    preset = "Executive blue",',
    '    page_bg = "#f8fafc",',
    '    panel_bg = "#FFFFFF",',
    '    chart_bg = "#FFFFFF",',
    '    header_color = "#111827",',
    '    font_color = "#111827",',
    '    axis_color = "#4B5563",',
    '    grid_color = "#E5E7EB",',
    '    font_family = "-apple-system,BlinkMacSystemFont,Segoe UI,sans-serif",',
    '    base_font_size = 12,',
    '    x_tick_angle = -45,',
    '    series_colors = preset_palettes[["Executive blue"]]',
    '  )',
    '  palette_values <- reactive({',
    '    cols <- theme_state$series_colors',
    '    cols <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]',
    '    if (length(cols)) cols else preset_palettes[["Executive blue"]]',
    '  })',
    '  chart_theme <- function() {',
    '    ggplot2::theme_minimal(base_size = theme_state$base_font_size, base_family = theme_state$font_family) +',
    '      ggplot2::theme(',
    '        plot.background = ggplot2::element_rect(fill = theme_state$chart_bg, color = NA),',
    '        panel.background = ggplot2::element_rect(fill = theme_state$chart_bg, color = NA),',
    '        legend.background = ggplot2::element_rect(fill = theme_state$chart_bg, color = NA),',
    '        text = ggplot2::element_text(color = theme_state$font_color),',
    '        plot.title = ggplot2::element_text(color = theme_state$header_color, face = "bold"),',
    '        axis.text = ggplot2::element_text(color = theme_state$axis_color),',
    '        axis.text.x = ggplot2::element_text(angle = theme_state$x_tick_angle, hjust = 1, color = theme_state$axis_color),',
    '        axis.title = ggplot2::element_text(color = theme_state$axis_color),',
    '        legend.text = ggplot2::element_text(color = theme_state$font_color),',
    '        legend.title = ggplot2::element_text(color = theme_state$font_color),',
    '        panel.grid.major = ggplot2::element_line(color = theme_state$grid_color),',
    '        panel.grid.minor = ggplot2::element_blank()',
    '      )',
    '  }',
    '  plotly_theme <- function(p, title = NULL, x_title = NULL, y_title = NULL, extra = list()) {',
    '    base <- list(',
    '      title = if (!is.null(title)) list(text = title, font = list(color = theme_state$header_color, family = theme_state$font_family, size = theme_state$base_font_size + 4)) else NULL,',
    '      paper_bgcolor = theme_state$page_bg,',
    '      plot_bgcolor = theme_state$chart_bg,',
    '      font = list(color = theme_state$font_color, family = theme_state$font_family, size = theme_state$base_font_size),',
    '      xaxis = list(title = x_title %||% "", tickangle = theme_state$x_tick_angle, color = theme_state$axis_color, gridcolor = theme_state$grid_color),',
    '      yaxis = list(title = y_title %||% "", color = theme_state$axis_color, gridcolor = theme_state$grid_color),',
    '      legend = list(orientation = "h", x = 0, y = -0.2)',
    '    )',
    '    do.call(plotly::layout, c(list(p), base, extra))',
    '  }',
    '  output$theme_css <- renderUI({',
    '    tags$style(HTML(sprintf("body{background:%s;color:%s;font-family:%s;} .panel,.control-panel,.metric-card{background:%s;color:%s;} .title-row h2,.title-row p{color:%s;}", theme_state$page_bg, theme_state$font_color, theme_state$font_family, theme_state$panel_bg, theme_state$font_color, theme_state$header_color)))',
    '  })',
    '  output$theme_color_inputs_tmp <- renderUI({',
    '    n <- input$theme_series_n_tmp %||% length(theme_state$series_colors)',
    '    n <- max(3, min(10, as.integer(n)))',
    '    vals <- palette_values()',
    '    tagList(lapply(seq_len(n), function(i) {',
    '      tags$div(style = "display:inline-block;margin:0 12px 10px 0;min-width:94px;", tags$label(paste("Series", i)), tags$input(id = paste0("theme_series_", i, "_tmp"), type = "color", value = vals[((i - 1) %% length(vals)) + 1]))',
    '    }))',
    '  })',
    '  observeEvent(input$open_theme, {',
    '    showModal(modalDialog(',
    '      title = "Theme and chart formatting",',
    '      size = "l",',
    '      selectInput("theme_preset_tmp", "Preset", choices = c(names(preset_palettes), "Custom"), selected = theme_state$preset),',
    '      fluidRow(',
    '        column(4, tags$label("Page background"), tags$input(id = "theme_page_bg_tmp", type = "color", value = theme_state$page_bg)),',
    '        column(4, tags$label("Chart background"), tags$input(id = "theme_chart_bg_tmp", type = "color", value = theme_state$chart_bg)),',
    '        column(4, tags$label("Panel background"), tags$input(id = "theme_panel_bg_tmp", type = "color", value = theme_state$panel_bg))',
    '      ),',
    '      fluidRow(',
    '        column(3, tags$label("Header color"), tags$input(id = "theme_header_color_tmp", type = "color", value = theme_state$header_color)),',
    '        column(3, tags$label("Font color"), tags$input(id = "theme_font_color_tmp", type = "color", value = theme_state$font_color)),',
    '        column(3, tags$label("Axis color"), tags$input(id = "theme_axis_color_tmp", type = "color", value = theme_state$axis_color)),',
    '        column(3, tags$label("Grid color"), tags$input(id = "theme_grid_color_tmp", type = "color", value = theme_state$grid_color))',
    '      ),',
    '      fluidRow(',
    '        column(4, selectInput("theme_font_family_tmp", "Font family", choices = c("System" = "-apple-system,BlinkMacSystemFont,Segoe UI,sans-serif", "Arial" = "Arial", "Helvetica" = "Helvetica", "Georgia" = "Georgia", "Courier" = "Courier New"), selected = theme_state$font_family)),',
    '        column(4, numericInput("theme_base_font_size_tmp", "Base font size", value = theme_state$base_font_size, min = 9, max = 22, step = 1)),',
    '        column(4, numericInput("theme_x_tick_angle_tmp", "X-axis label angle", value = theme_state$x_tick_angle, min = -90, max = 90, step = 15))',
    '      ),',
    '      numericInput("theme_series_n_tmp", "Series colors", value = length(theme_state$series_colors), min = 3, max = 10, step = 1),',
    '      uiOutput("theme_color_inputs_tmp"),',
    '      footer = tagList(modalButton("Cancel"), actionButton("apply_theme", "Apply theme"))',
    '    ))',
    '  })',
    '  observeEvent(input$apply_theme, {',
    '    preset <- input$theme_preset_tmp %||% "Executive blue"',
    '    n <- max(3, min(10, as.integer(input$theme_series_n_tmp %||% length(theme_state$series_colors))))',
    '    cols <- vapply(seq_len(n), function(i) input[[paste0("theme_series_", i, "_tmp")]] %||% palette_values()[((i - 1) %% length(palette_values())) + 1], character(1))',
    '    if (preset %in% names(preset_palettes) && !identical(preset, "Custom")) cols <- preset_palettes[[preset]]',
    '    theme_state$preset <- preset',
    '    theme_state$page_bg <- input$theme_page_bg_tmp %||% theme_state$page_bg',
    '    theme_state$panel_bg <- input$theme_panel_bg_tmp %||% theme_state$panel_bg',
    '    theme_state$chart_bg <- input$theme_chart_bg_tmp %||% theme_state$chart_bg',
    '    theme_state$header_color <- input$theme_header_color_tmp %||% theme_state$header_color',
    '    theme_state$font_color <- input$theme_font_color_tmp %||% theme_state$font_color',
    '    theme_state$axis_color <- input$theme_axis_color_tmp %||% theme_state$axis_color',
    '    theme_state$grid_color <- input$theme_grid_color_tmp %||% theme_state$grid_color',
    '    theme_state$font_family <- input$theme_font_family_tmp %||% theme_state$font_family',
    '    theme_state$base_font_size <- as.numeric(input$theme_base_font_size_tmp %||% theme_state$base_font_size)',
    '    theme_state$x_tick_angle <- as.numeric(input$theme_x_tick_angle_tmp %||% theme_state$x_tick_angle)',
    '    theme_state$series_colors <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]',
    '    removeModal()',
    '  })',
    '  color_map <- function(keys) { vals <- palette_values(); stats::setNames(rep(vals, length.out = length(keys)), keys) }',
    '  selected_vars <- reactive({',
    '    vars <- input$focus_variable',
    '    if (is.null(vars) || !length(vars) || vars == "__all__") variable_choices else vars',
    '  })',
    '  filter_vars <- function(dt, col = "variable") {',
    '    if (!nrow(dt) || !(col %in% names(dt))) return(dt)',
    '    dt[as.character(get(col)) %in% selected_vars()]',
    '  }',
    '  filter_role <- function(dt) {',
    '    if (!nrow(dt) || input$role_filter == "__all__" || !("role" %in% names(dt))) return(dt)',
    '    dt[as.character(role) == input$role_filter]',
    '  }',
    '  dt_widget <- function(dt, page = 20) {',
    '    dt <- as.data.table(dt)',
    '    w <- datatable(dt, options = list(pageLength = page, scrollX = TRUE), filter = "top", rownames = FALSE)',
    '    bar_cols <- intersect(names(dt), c("spend", "contribution", "outcome_per_cost", "cost_per_outcome", "value_per_cost", "cost_per_value", "roi", "mroi", "expected_roi", "expected_mroi", "fair_share_index", "efficiency_index", "spend_share", "contribution_share", "probability_profit_positive", "probability_incremental_contribution_positive"))',
    '    for (cc in bar_cols) {',
    '      vals <- suppressWarnings(as.numeric(dt[[cc]]))',
    '      if (any(is.finite(vals)) && min(vals, na.rm = TRUE) >= 0 && max(vals, na.rm = TRUE) > 0) {',
    '        w <- formatStyle(w, cc, background = styleColorBar(c(0, max(vals, na.rm = TRUE)), "#DBEAFE"), backgroundSize = "98% 88%", backgroundRepeat = "no-repeat", backgroundPosition = "center")',
    '      }',
    '    }',
    '    w',
    '  }',
    '  summary <- table_or_empty("executive_summary")[1]',
    '  output$metric_cards <- renderUI({',
    '    div(class = "metric-grid",',
    '      card("Actual KPI", fmt(summary$actual_kpi)),',
    '      card("Predicted KPI", fmt(summary$predicted_kpi)),',
    '      card("R-squared", fmt(summary$r_squared)),',
    '      card("Cost per outcome", fmt(summary$media_cost_per_outcome))',
    '    )',
    '  })',
    '  selected_contrib <- reactive({',
    '    dt <- if (input$period == "__all__") { out <- copy(table_or_empty("contribution_by_variable")); out[, period_label := "All periods"]; out } else table_or_empty("contribution_by_period_variable")[period_label == input$period]',
    '    filter_role(filter_vars(dt))',
    '  })',
    '  selected_econ <- reactive({',
    '    dt <- if (input$period == "__all__") copy(table_or_empty("kpi_economics")) else table_or_empty("kpi_economics_by_period")[period_label == input$period]',
    '    filter_vars(dt)',
    '  })',
    '  output$contribution_bar <- renderPlotly({',
    '    dt <- selected_contrib()[role != "residual"][order(-abs(as.numeric(contribution)))]',
    '    validate(need(nrow(dt) > 0, "No contribution rows available."))',
    '    dt <- head(dt, input$top_n)',
    '    p <- ggplot(dt, aes(x = reorder(variable, as.numeric(contribution)), y = as.numeric(contribution), fill = role, text = paste(variable, "<br>Contribution:", fmt(contribution)))) + geom_col(width = 0.72) + coord_flip() + scale_fill_manual(values = color_map(unique(dt$role))) + labs(title = "Contribution by variable", x = NULL, y = "KPI contribution") + chart_theme() + theme(legend.position = "bottom")',
    '    ggplotly(p, tooltip = "text")',
    '  })',
    '  output$cost_bar <- renderPlotly({',
    '    metric <- input$econ_metric',
    '    dt <- selected_econ()',
    '    validate(need(metric %in% names(dt), "Selected economics metric is unavailable."))',
    '    dt[, metric_value__ := suppressWarnings(as.numeric(get(metric)))]',
    '    dt <- dt[is.finite(metric_value__)][order(metric_value__)]',
    '    validate(need(nrow(dt) > 0, "No finite economics rows available."))',
    '    dt <- head(dt, input$top_n)',
    '    p <- ggplot(dt, aes(x = reorder(variable, -metric_value__), y = metric_value__, text = paste(variable, "<br>", metric, ":", fmt(metric_value__)))) + geom_col(fill = palette_values()[2], width = 0.72) + coord_flip() + labs(title = paste("Ranked", gsub("_", " ", metric)), x = NULL, y = gsub("_", " ", metric)) + chart_theme()',
    '    ggplotly(p, tooltip = "text")',
    '  })',
    '  output$actual_fit <- renderPlotly({',
    '    dt <- copy(table_or_empty("fit_by_period"))',
    '    validate(need(nrow(dt) > 0 && all(c("period_label", "period_sort", "actual", "pred") %in% names(dt)), "No fit-by-period rows available."))',
    '    setorder(dt, period_sort)',
    '    p <- plot_ly(dt, x = ~period_label)',
    '    p <- add_lines(p, y = ~actual, name = "Actual", line = list(color = palette_values()[1]), hovertemplate = "%{x}<br>Actual: %{y:,.2f}<extra></extra>")',
    '    p <- add_lines(p, y = ~pred, name = "Fitted", line = list(color = palette_values()[2]), hovertemplate = "%{x}<br>Fitted: %{y:,.2f}<extra></extra>")',
    '    if (!is.null(input$fit_overlay_variable) && input$fit_overlay_variable != "__none__") {',
    '      ov <- table_or_empty("contribution_by_period_variable")[variable == input$fit_overlay_variable, .(overlay = sum(as.numeric(contribution), na.rm = TRUE)), by = .(period_sort, period_label)]',
    '      dt <- merge(dt, ov, by = c("period_sort", "period_label"), all.x = TRUE, sort = FALSE)',
    '      setorder(dt, period_sort)',
    '      p <- add_bars(p, data = dt, x = ~period_label, y = ~overlay, name = input$fit_overlay_variable, yaxis = "y2", marker = list(color = "rgba(37,99,235,0.24)"), hovertemplate = paste0("%{x}<br>", input$fit_overlay_variable, ": %{y:,.2f}<extra></extra>"))',
    '      p <- layout(p, yaxis2 = list(title = input$fit_overlay_variable, overlaying = "y", side = "right", showgrid = FALSE))',
    '    }',
    '    layout(p, title = list(text = "Actual vs fitted KPI", font = list(color = theme_state$header_color, family = theme_state$font_family, size = theme_state$base_font_size + 4)), paper_bgcolor = theme_state$page_bg, plot_bgcolor = theme_state$chart_bg, font = list(color = theme_state$font_color, family = theme_state$font_family, size = theme_state$base_font_size), xaxis = list(title = "", tickangle = theme_state$x_tick_angle, categoryorder = "array", categoryarray = dt$period_label, color = theme_state$axis_color, gridcolor = theme_state$grid_color), yaxis = list(title = "KPI", color = theme_state$axis_color, gridcolor = theme_state$grid_color), legend = list(orientation = "h", x = 0, y = -0.25), barmode = "overlay")',
    '  })',
    '  output$spend_share_plot <- renderPlotly({',
    '    dt <- selected_econ()[is.finite(spend_share) & is.finite(contribution_share)]',
    '    validate(need(nrow(dt) > 0, "No spend-share/contribution-share rows available."))',
    '    p <- ggplot(dt, aes(x = spend_share, y = contribution_share, text = paste(variable, "<br>Spend share:", fmt(spend_share), "<br>Contribution share:", fmt(contribution_share), "<br>Fair-share index:", fmt(fair_share_index)))) + geom_abline(slope = 1, intercept = 0, color = "#9CA3AF", linetype = "dashed") + geom_point(color = palette_values()[2], size = 3) + labs(title = "Fair-share index: contribution share vs spend share", x = "Spend share", y = "Contribution share") + chart_theme()',
    '    ggplotly(p, tooltip = "text")',
    '  })',
    '  output$spend_scatter <- renderPlotly({',
    '    dt <- selected_econ()[is.finite(spend) & is.finite(contribution)]',
    '    validate(need(nrow(dt) > 0, "No spend and contribution rows available."))',
    '    dt[, bubble_size__ := pmax(abs(as.numeric(contribution)), 1e-8)]',
    '    p <- ggplot(dt, aes(x = spend, y = contribution, size = bubble_size__, color = fair_share_index, text = paste(variable, "<br>Spend:", fmt(spend), "<br>Contribution:", fmt(contribution), "<br>Fair-share index:", fmt(fair_share_index)))) + geom_point(alpha = 0.72) + scale_size_continuous(range = c(8, 28), guide = "none") + scale_color_gradient2(low = "#DC2626", mid = "#9CA3AF", high = "#16A34A", midpoint = 1, na.value = palette_values()[2]) + labs(title = "Spend vs KPI contribution bubble chart", x = "Spend", y = "Contribution", color = "Fair-share index") + chart_theme()',
    '    ggplotly(p, tooltip = "text")',
    '  })',
    '  output$econ_rank_plot <- renderPlotly({',
    '    metric <- input$econ_metric',
    '    dt <- selected_econ()',
    '    validate(need(metric %in% names(dt), "Selected economics metric is unavailable."))',
    '    dt[, metric_value__ := suppressWarnings(as.numeric(get(metric)))]',
    '    dt <- dt[is.finite(metric_value__)][order(metric_value__)]',
    '    validate(need(nrow(dt) > 0, "No finite economics rows available."))',
    '    dt <- head(dt, input$top_n)',
    '    p <- ggplot(dt, aes(x = reorder(variable, -metric_value__), y = metric_value__, text = paste(variable, "<br>", metric, ":", fmt(metric_value__)))) + geom_col(fill = palette_values()[2], width = 0.72) + coord_flip() + labs(title = paste("Ranked", gsub("_", " ", metric)), x = NULL, y = gsub("_", " ", metric)) + chart_theme()',
    '    ggplotly(p, tooltip = "text")',
    '  })',
    '  output$residual_plot <- renderPlotly({',
    '    dt <- copy(table_or_empty("fit_by_period"))',
    '    validate(need(nrow(dt) > 0 && "residual" %in% names(dt), "No residual rows available."))',
    '    p <- ggplot(dt, aes(x = reorder(period_label, period_sort), y = as.numeric(residual), text = paste(period_label, "<br>Residual:", fmt(residual)))) + geom_hline(yintercept = 0, color = "#6B7280") + geom_col(fill = "#DC2626", width = 0.78) + labs(title = "Residuals by period", x = NULL, y = "Actual minus fitted") + chart_theme() + theme(axis.text.x = element_text(angle = theme_state$x_tick_angle, hjust = 1))',
    '    ggplotly(p, tooltip = "text")',
    '  })',
    '  selected_curve_vars <- reactive({',
    '    vars <- input$curve_variable',
    '    if (!is.null(vars) && length(vars)) vars else curve_choices[1]',
    '  })',
    '  curve_dt <- reactive({',
    '    dt <- table_or_empty("optimizer_response_curves")',
    '    if (!nrow(dt) || !("variable" %in% names(dt))) return(dt)',
    '    dt[variable %in% selected_curve_vars()]',
    '  })',
    '  draw_curve <- function(metric, title) {',
    '    dt <- copy(curve_dt())',
    '    validate(need(nrow(dt) > 0 && all(c("variable", "spend_multiplier") %in% names(dt)) && metric %in% names(dt), paste("No curve rows available for", metric)))',
    '    dt[, y_metric__ := suppressWarnings(as.numeric(get(metric)))]',
    '    dt <- dt[is.finite(y_metric__)]',
    '    validate(need(nrow(dt) > 0, paste("No finite curve rows available for", metric)))',
    '    dt[, spend_multiplier__ := suppressWarnings(as.numeric(spend_multiplier))]',
    '    dt <- dt[is.finite(spend_multiplier__)]',
    '    data.table::setorder(dt, variable, spend_multiplier__)',
    '    v <- as.character(dt$variable[1])',
    '    col <- palette_values()[1]',
    '    p <- plot_ly()',
    '    q05 <- paste0(metric, "_q05"); q95 <- paste0(metric, "_q95")',
    '    if (isTRUE(input$curve_show_interval) && q05 %in% names(dt) && q95 %in% names(dt)) {',
    '      dt[, y_low__ := suppressWarnings(as.numeric(get(q05)))]',
    '      dt[, y_high__ := suppressWarnings(as.numeric(get(q95)))]',
    '      band <- dt[is.finite(y_low__) & is.finite(y_high__)]',
    '      if (nrow(band)) {',
    '        band_poly <- rbind(band[, .(spend_multiplier__, y_band__ = y_high__)], band[.N:1, .(spend_multiplier__, y_band__ = y_low__)])',
    '        p <- add_trace(p, data = band_poly, x = ~spend_multiplier__, y = ~y_band__, type = "scatter", mode = "lines", fill = "toself", fillcolor = "rgba(37,99,235,0.18)", line = list(color = "rgba(37,99,235,0)"), name = "q05-q95 band", hoverinfo = "skip", showlegend = TRUE)',
    '      }',
    '    }',
    '    p <- add_trace(p, data = dt, x = ~spend_multiplier__, y = ~y_metric__, type = "scatter", mode = "lines", name = v, line = list(color = col, width = 2.5), hovertemplate = paste0(v, "<br>Multiplier: %{x:.2f}<br>", metric, ": %{y:,.2f}<extra></extra>"))',
    '    cur <- dt[which.min(abs(spend_multiplier__ - 1))]',
    '    if (nrow(cur)) p <- add_trace(p, data = cur, x = ~spend_multiplier__, y = ~y_metric__, type = "scatter", mode = "markers", name = "current", showlegend = FALSE, marker = list(color = col, size = 9, symbol = "circle"), hovertemplate = paste0(v, " current<br>Multiplier: %{x:.2f}<br>", metric, ": %{y:,.2f}<extra></extra>"))',
    '    plotly_theme(p, title = title, x_title = "Spend/support multiplier", y_title = gsub("_", " ", metric))',
    '  }',
    '  output$optimizer_curve_plot <- renderPlotly({ draw_curve(input$curve_metric, paste(gsub("_", " ", input$curve_metric), "curve")) })',
    '  output$optimizer_spend_plot <- renderPlotly({',
    '    dt <- filter_vars(table_or_empty("optimizer_plan"))',
    '    validate(need(nrow(dt) > 0 && all(c("variable", "current_spend", "recommended_spend") %in% names(dt)), "No optimizer plan rows available."))',
    '    dt <- dt[, .(variable, current_spend = as.numeric(current_spend), recommended_spend = as.numeric(recommended_spend))]',
    '    long <- melt(dt, id.vars = "variable", measure.vars = c("current_spend", "recommended_spend"), variable.name = "plan", value.name = "spend")',
    '    long[, plan := fifelse(plan == "current_spend", "Current", "Recommended")]',
    '    p <- ggplot(long, aes(x = reorder(variable, spend, FUN = max, na.rm = TRUE), y = spend, fill = plan, text = paste(variable, "<br>", plan, ":", fmt(spend)))) + geom_col(position = "dodge", width = 0.72) + coord_flip() + labs(title = "Current vs recommended spend", x = NULL, y = "Spend") + chart_theme() + theme(legend.position = "bottom")',
    '    ggplotly(p, tooltip = "text")',
    '  })',
    '  output$optimizer_scenario_plot <- renderPlotly({',
    '    metric <- input$scenario_metric',
    '    dt <- table_or_empty("optimizer_scenario_comparison")',
    '    validate(need(nrow(dt) > 0 && "plan_name" %in% names(dt) && metric %in% names(dt), "No optimizer scenario rows available."))',
    '    dt[, metric_value__ := suppressWarnings(as.numeric(get(metric)))]',
    '    dt <- dt[is.finite(metric_value__)]',
    '    validate(need(nrow(dt) > 0, "No finite scenario rows available."))',
    '    p <- ggplot(dt, aes(x = reorder(plan_name, metric_value__), y = metric_value__, fill = plan_type, text = paste(plan_name, "<br>", metric, ":", fmt(metric_value__)))) + geom_col(width = 0.72) + coord_flip() + scale_fill_manual(values = color_map(unique(dt$plan_type))) + labs(title = paste("Scenario", gsub("_", " ", metric)), x = NULL, y = gsub("_", " ", metric)) + chart_theme() + theme(legend.position = "bottom")',
    '    ggplotly(p, tooltip = "text")',
    '  })',
    '  output$optimizer_saturation_plot <- renderPlotly({',
    '    dt <- filter_vars(table_or_empty("optimizer_saturation_headroom"))',
    '    validate(need(nrow(dt) > 0 && all(c("variable", "pct_of_peak_grid_contribution") %in% names(dt)), "No saturation/headroom rows available."))',
    '    dt <- dt[is.finite(as.numeric(pct_of_peak_grid_contribution))]',
    '    p <- ggplot(dt, aes(x = reorder(variable, as.numeric(pct_of_peak_grid_contribution)), y = as.numeric(pct_of_peak_grid_contribution), fill = saturation_band, text = paste(variable, "<br>Share of peak:", fmt(100 * as.numeric(pct_of_peak_grid_contribution)), "%"))) + geom_col(width = 0.72) + coord_flip() + labs(title = "Saturation and response headroom", x = NULL, y = "Current share of peak grid contribution") + chart_theme() + theme(legend.position = "bottom")',
    '    ggplotly(p, tooltip = "text")',
    '  })',
    '  output$scenario_uncertainty_plot <- renderPlotly({',
    '    dt <- table_or_empty("optimizer_scenario_uncertainty_summary")',
    '    validate(need(nrow(dt) > 0 && all(c("scenario", "contribution_q05", "contribution_q50", "contribution_q95") %in% names(dt)), "No scenario uncertainty rows available."))',
    '    dt <- dt[is.finite(as.numeric(contribution_q50))]',
    '    validate(need(nrow(dt) > 0, "No finite scenario uncertainty rows available."))',
    '    setorder(dt, contribution_q50)',
    '    p <- plot_ly(dt, x = ~contribution_q50, y = ~reorder(scenario, contribution_q50), type = "scatter", mode = "markers", marker = list(color = palette_values()[1], size = 9), error_x = list(type = "data", symmetric = FALSE, array = ~pmax(0, contribution_q95 - contribution_q50), arrayminus = ~pmax(0, contribution_q50 - contribution_q05), color = "rgba(37,99,235,0.35)", thickness = 1.5), hovertemplate = "%{y}<br>q50 contribution: %{x:,.2f}<extra></extra>")',
    '    plotly_theme(p, title = "Scenario contribution uncertainty", x_title = "Contribution", y_title = "")',
    '  })',
    '  output$contribution_trend_plot <- renderPlotly({',
    '    dt <- filter_role(filter_vars(table_or_empty("contribution_by_period_variable")))[role != "residual"]',
    '    validate(need(nrow(dt) > 0, "No contribution trend rows available."))',
    '    one_var <- length(selected_vars()) == 1L && selected_vars()[1] %in% as.character(dt$variable)',
    '    if (one_var) {',
    '      dt <- dt[, .(contribution = sum(as.numeric(contribution), na.rm = TRUE), contribution_q05 = if ("contribution_q05" %in% names(.SD)) sum(as.numeric(contribution_q05), na.rm = TRUE) else NA_real_, contribution_q95 = if ("contribution_q95" %in% names(.SD)) sum(as.numeric(contribution_q95), na.rm = TRUE) else NA_real_), by = .(period_sort, period_label, variable)]',
    '      setorder(dt, period_sort)',
    '      p <- plot_ly(dt, x = ~period_label)',
    '      if (all(c("contribution_q05", "contribution_q95") %in% names(dt)) && any(is.finite(dt$contribution_q05) & is.finite(dt$contribution_q95))) {',
    '        band <- dt[is.finite(contribution_q05) & is.finite(contribution_q95)]',
    '        band_poly <- rbind(band[, .(period_label, period_sort, y_band__ = contribution_q95)], band[.N:1, .(period_label, period_sort, y_band__ = contribution_q05)])',
    '        p <- add_trace(p, data = band_poly, x = ~period_label, y = ~y_band__, type = "scatter", mode = "lines", fill = "toself", fillcolor = "rgba(37,99,235,0.18)", line = list(color = "rgba(37,99,235,0)"), name = "q05-q95 band", hoverinfo = "skip")',
    '      }',
    '      p <- add_lines(p, data = dt, x = ~period_label, y = ~contribution, name = selected_vars()[1], line = list(color = palette_values()[1], width = 2.2), hovertemplate = "%{x}<br>Contribution: %{y:,.2f}<extra></extra>")',
    '      p <- add_markers(p, data = dt, x = ~period_label, y = ~contribution, name = "points", showlegend = FALSE, marker = list(color = palette_values()[1], size = 5), hovertemplate = "%{x}<br>Contribution: %{y:,.2f}<extra></extra>")',
    '      plotly_theme(p, title = paste("Contribution trend:", selected_vars()[1]), x_title = "", y_title = "Contribution")',
    '    } else {',
    '      dt <- dt[, .(contribution = sum(as.numeric(contribution), na.rm = TRUE)), by = .(period_sort, period_label, variable)]',
    '      p <- ggplot(dt, aes(x = reorder(period_label, period_sort), y = contribution, fill = variable, text = paste(period_label, "<br>", variable, ":", fmt(contribution)))) + geom_col(width = 0.82) + scale_fill_manual(values = color_map(unique(dt$variable))) + labs(title = "Contribution trend by variable", x = NULL, y = "Contribution") + chart_theme() + theme(axis.text.x = element_text(angle = theme_state$x_tick_angle, hjust = 1), legend.position = "bottom")',
    '      ggplotly(p, tooltip = "text")',
    '    }',
    '  })',
    '  output$due_to_plot <- renderPlotly({',
    '    dt <- filter_vars(table_or_empty("period_due_to_variable"))[is.finite(contribution_change)]',
    '    validate(need(nrow(dt) > 0, "No due-to rows available."))',
    '    latest <- dt[period_sort == max(period_sort, na.rm = TRUE)][order(-abs(contribution_change))]',
    '    latest <- head(latest, input$top_n)',
    '    p <- ggplot(latest, aes(x = reorder(variable, as.numeric(contribution_change)), y = as.numeric(contribution_change), text = paste(variable, "<br>Change contribution:", fmt(contribution_change)))) + geom_hline(yintercept = 0, color = "#9CA3AF") + geom_col(fill = palette_values()[3], width = 0.72) + coord_flip() + labs(title = "Latest period due-to contribution change", x = NULL, y = "Contribution change") + chart_theme()',
    '    ggplotly(p, tooltip = "text")',
    '  })',
    '  output$contribution_table <- renderDT(dt_widget(selected_contrib(), 20))',
    '  output$trend_table <- renderDT(dt_widget(filter_role(filter_vars(table_or_empty("contribution_by_period_variable"))), 20))',
    '  output$econ_table <- renderDT(dt_widget(selected_econ(), 20))',
    '  output$curve_table <- renderDT(dt_widget(curve_dt(), 20))',
    '  output$curve_uncertainty_table <- renderDT(dt_widget(table_or_empty("optimizer_response_curve_uncertainty"), 20))',
    '  output$scenario_uncertainty_table <- renderDT(dt_widget(table_or_empty("optimizer_scenario_uncertainty_summary"), 20))',
    '  output$optimization_uncertainty_table <- renderDT(dt_widget(table_or_empty("optimizer_optimization_uncertainty_summary"), 20))',
    '  output$optimizer_plan_table <- renderDT(dt_widget(filter_vars(table_or_empty("optimizer_plan")), 20))',
    '  output$optimizer_scenario_table <- renderDT(dt_widget(table_or_empty("optimizer_scenario_comparison"), 20))',
    '  output$flags_table <- renderDT(dt_widget(table_or_empty("diagnostic_flags"), 10))',
    '  output$fit_table <- renderDT(dt_widget(table_or_empty("fit_diagnostics"), 10))',
    '  output$chart_registry_table <- renderDT(dt_widget(table_or_empty("chart_registry"), 20))',
    '}',
    'shinyApp(ui, server)'
  )
  app_path <- file.path(app_dir, "app.R")
  writeLines(app_lines, app_path, useBytes = TRUE)

  readme <- c(
    "# MMM Shiny Dashboard",
    "",
    "Run from this directory with:",
    "",
    "```r",
    "install.packages(c('shiny', 'plotly', 'DT', 'ggplot2', 'data.table'))",
    "shiny::runApp('.')",
    "```",
    "",
    "The app reads `mmm_report_tables.rds`, which was created by `write_mmm_deck_shiny_app()`."
  )
  writeLines(readme, file.path(app_dir, "README.md"), useBytes = TRUE)
  app_path
}

write_mmm_deck_outputs <- function(report_tables,
                                   output_dir,
                                   prefix = "",
                                   write_charts = TRUE,
                                   write_html = TRUE,
                                   write_excel = FALSE,
                                   write_shiny = FALSE,
                                   top_n_charts = 12) {
  if (is.null(output_dir) || !nzchar(output_dir)) stop("output_dir is required.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  table_dir <- file.path(output_dir, "tables")
  chart_dir <- file.path(output_dir, "charts")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  pfx <- if (nzchar(prefix)) paste0(prefix, "_") else ""

  table_files <- character()
  table_names <- names(report_tables)[vapply(report_tables, function(x) data.table::is.data.table(x) || is.data.frame(x), logical(1))]
  table_names <- setdiff(table_names, c("standardized_long", "standardized_wide"))
  for (nm in table_names) {
    tab <- data.table::as.data.table(report_tables[[nm]])
    if (!ncol(tab)) next
    path <- file.path(table_dir, paste0(pfx, nm, ".csv"))
    data.table::fwrite(tab, path)
    table_files <- c(table_files, path)
  }

  chart_files <- character()
  if (isTRUE(write_charts)) chart_files <- write_mmm_deck_charts(report_tables, chart_dir, top_n = top_n_charts)

  html_path <- NA_character_
  if (isTRUE(write_html)) {
    html_path <- write_mmm_deck_html(report_tables, chart_files = chart_files, output_path = file.path(output_dir, paste0(pfx, "mmm_deck_dashboard.html")))
  }

  excel_path <- NA_character_
  if (isTRUE(write_excel)) {
    excel_path <- write_mmm_deck_excel(report_tables, file.path(output_dir, paste0(pfx, "mmm_deck_summary.xlsx")))
  }

  shiny_path <- NA_character_
  if (isTRUE(write_shiny)) {
    shiny_path <- write_mmm_deck_shiny_app(report_tables, output_dir = output_dir, prefix = prefix)
  }

  index <- data.table::data.table(
    artifact_type = c(rep("table", length(table_files)), rep("chart", length(chart_files)), "html", "excel", "shiny_app"),
    path = c(table_files, chart_files, html_path, excel_path, shiny_path)
  )
  index <- index[!is.na(path)]
  data.table::fwrite(index, file.path(output_dir, paste0(pfx, "report_index.csv")))

  invisible(list(
    output_dir = output_dir,
    table_files = table_files,
    chart_files = chart_files,
    html_path = html_path,
    excel_path = excel_path,
    shiny_path = shiny_path,
    index = index
  ))
}

run_mmm_deck_output_builder <- function(long_decomp,
                                        wide_decomp = NULL,
                                        raw_data = NULL,
                                        modcut = NULL,
                                        spend_map = NULL,
                                        optimizer_output = NULL,
                                        channel_map = NULL,
                                        output_dir,
                                        prefix = "",
                                        media_variables = NULL,
                                        baseline_variables = NULL,
                                        time_col = NULL,
                                        group_col = NULL,
                                        entity_col = NULL,
                                        variable_col = "variable",
                                        contribution_col = "contribution",
                                        actual_col = "y_actual",
                                        fitted_col = "pred",
                                        residual_col = "residual",
                                        sample_col = "sample",
                                        sample_values = NULL,
                                        period_granularity = "month",
                                        spend_suffix = "_spend",
                                        kpi_value_per_outcome = NULL,
                                        write_charts = TRUE,
                                        write_html = TRUE,
                                        write_excel = FALSE,
                                        write_shiny = FALSE,
                                        top_n_charts = 12) {
  tables <- build_mmm_deck_tables(
    long_decomp = long_decomp,
    wide_decomp = wide_decomp,
    raw_data = raw_data,
    modcut = modcut,
    spend_map = spend_map,
    optimizer_output = optimizer_output,
    channel_map = channel_map,
    media_variables = media_variables,
    baseline_variables = baseline_variables,
    time_col = time_col,
    group_col = group_col,
    entity_col = entity_col,
    variable_col = variable_col,
    contribution_col = contribution_col,
    actual_col = actual_col,
    fitted_col = fitted_col,
    residual_col = residual_col,
    sample_col = sample_col,
    sample_values = sample_values,
    period_granularity = period_granularity,
    spend_suffix = spend_suffix,
    kpi_value_per_outcome = kpi_value_per_outcome
  )
  files <- write_mmm_deck_outputs(
    report_tables = tables,
    output_dir = output_dir,
    prefix = prefix,
    write_charts = write_charts,
    write_html = write_html,
    write_excel = write_excel,
    write_shiny = write_shiny,
    top_n_charts = top_n_charts
  )
  list(tables = tables, files = files)
}
