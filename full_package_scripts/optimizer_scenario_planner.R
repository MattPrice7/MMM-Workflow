# optimizer_scenario_planner.R
#
# Standalone optimizer and scenario planner for MMM response curves.
# The optimizer always allocates against response curves. It can get those
# curves from either:
#   1. A Stan MMM fit object from hier_mmm.R, preferably using fit$response_curves.
#      If that sheet is missing, the same curve points are generated from the
#      fitted transforms and coefficients.
#   2. A precomputed response-curve table, useful for hand-built curves,
#      external models, or baseline planning before a full MMM fit exists.
#
# Industry alignment:
#   - Response curves scale historical media/support flighting by channel.
#   - ROI is historical/average contribution per cost.
#   - mROI is next-dollar incremental outcome around the current or scenario spend.
#   - Optimization is point-estimate decision support. When draw-level response
#     curves are supplied, scenario and optimized-plan uncertainty summaries are
#     reported separately from the point recommendation.

opsp_require_data_table <- function() {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("optimizer_scenario_planner.R requires the data.table package.", call. = FALSE)
  }
  invisible(TRUE)
}

opsp_as_dt <- function(x, label = "input") {
  opsp_require_data_table()
  if (is.null(x)) return(data.table::data.table())
  data.table::as.data.table(data.table::copy(x))
}

opsp_script_dir <- function() {
  tryCatch({
    frames <- sys.frames()
    ofiles <- vapply(frames, function(f) {
      if (!is.null(f$ofile)) as.character(f$ofile)[1] else NA_character_
    }, character(1))
    ofiles <- ofiles[!is.na(ofiles) & nzchar(ofiles)]
    if (length(ofiles)) {
      dirname(normalizePath(ofiles[length(ofiles)], mustWork = FALSE))
    } else {
      file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
      if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)) else getwd()
    }
  }, error = function(e) getwd())
}

opsp_source_hier_if_needed <- function() {
  if (exists("variable_contribution_sum_hier_mmm", mode = "function") &&
      exists("normalize_spend_map_hier_mmm", mode = "function") &&
      exists("get_aligned_raw_data_hier_mmm", mode = "function")) {
    return(invisible(TRUE))
  }
  path <- file.path(opsp_script_dir(), "hier_mmm.R")
  if (!file.exists(path)) {
    stop("MMM fit mode requires hier_mmm.R helpers. Source hier_mmm.R first or place it beside optimizer_scenario_planner.R.", call. = FALSE)
  }
  source(path, chdir = TRUE)
  invisible(TRUE)
}

opsp_num <- function(x) suppressWarnings(as.numeric(x))

opsp_pick_col <- function(dt, candidates) {
  hit <- intersect(candidates, names(dt))
  if (length(hit)) hit[1] else NA_character_
}

opsp_safe_div <- function(num, den) {
  den <- opsp_num(den)
  num <- opsp_num(num)
  out <- rep(NA_real_, length(num))
  ok <- is.finite(num) & is.finite(den) & abs(den) > 1e-8
  out[ok] <- num[ok] / den[ok]
  out
}

opsp_interp_curve_value <- function(table, variable_name, multiplier, value_col, fallback = NA_real_) {
  if (is.null(table) || !nrow(table) || !(value_col %in% names(table))) return(fallback)
  vv <- as.character(variable_name)[1]
  tmp <- table[variable == vv]
  if (!nrow(tmp)) return(fallback)
  x <- opsp_num(tmp$spend_multiplier)
  y <- opsp_num(tmp[[value_col]])
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 1L) return(fallback)
  if (sum(ok) == 1L) return(y[ok][1])
  ord <- order(x[ok])
  stats::approx(x[ok][ord], y[ok][ord], xout = as.numeric(multiplier)[1], rule = 2, ties = mean)$y
}

opsp_clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

opsp_check_unique_variables <- function(x, label) {
  if (!"variable" %in% names(x)) return(invisible(TRUE))
  dup <- unique(x$variable[duplicated(x$variable)])
  if (length(dup)) stop(label, " contains duplicate variable rows: ", paste(dup, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

opsp_infer_variables <- function(fit_obj = NULL, response_curves = NULL, variables = NULL) {
  if (!is.null(variables)) return(unique(as.character(variables)))
  if (!is.null(fit_obj) && !is.null(fit_obj$variable_lookup)) {
    vl <- opsp_as_dt(fit_obj$variable_lookup, "fit_obj$variable_lookup")
    if ("variable" %in% names(vl)) return(unique(as.character(vl$variable)))
  }
  rc <- opsp_as_dt(response_curves, "response_curves")
  if (nrow(rc)) {
    vc <- opsp_pick_col(rc, c("variable", "channel", "media", "driver"))
    if (!is.na(vc)) return(unique(as.character(rc[[vc]])))
  }
  character()
}

opsp_normalize_constraints <- function(variables, current_spend, constraints = NULL,
                                       min_multiplier = 0,
                                       max_multiplier = 3) {
  opsp_require_data_table()
  out <- data.table::data.table(
    variable = as.character(variables),
    current_spend = as.numeric(current_spend),
    min_multiplier = as.numeric(min_multiplier)[1],
    max_multiplier = as.numeric(max_multiplier)[1],
    locked = FALSE,
    fixed_spend = NA_real_,
    min_spend = NA_real_,
    max_spend = NA_real_
  )
  if (!is.null(constraints) && nrow(opsp_as_dt(constraints))) {
    cs <- opsp_as_dt(constraints, "constraints")
    if (!"variable" %in% names(cs)) stop("constraints must include variable.", call. = FALSE)
    cs[, variable := as.character(variable)]
    opsp_check_unique_variables(cs, "constraints")
    for (cc in intersect(c("min_multiplier", "max_multiplier", "locked", "fixed_spend", "min_spend", "max_spend"), names(cs))) {
      out[cs, (cc) := get(paste0("i.", cc)), on = "variable"]
    }
  }
  out[, `:=`(
    current_spend = pmax(opsp_num(current_spend), 0),
    min_multiplier = pmax(opsp_num(min_multiplier), 0),
    max_multiplier = pmax(opsp_num(max_multiplier), 0),
    min_spend = opsp_num(min_spend),
    max_spend = opsp_num(max_spend),
    fixed_spend = opsp_num(fixed_spend),
    locked = as.logical(locked)
  )]
  out[is.na(locked), locked := FALSE]
  out[is.finite(opsp_num(min_spend)) & current_spend > 0, min_multiplier := pmax(min_multiplier, opsp_num(min_spend) / current_spend)]
  out[is.finite(opsp_num(max_spend)) & current_spend > 0, max_multiplier := pmin(max_multiplier, opsp_num(max_spend) / current_spend)]
  out[is.finite(opsp_num(fixed_spend)) & current_spend > 0, `:=`(
    min_multiplier = pmax(opsp_num(fixed_spend) / current_spend, 0),
    max_multiplier = pmax(opsp_num(fixed_spend) / current_spend, 0)
  )]
  out[locked == TRUE, `:=`(min_multiplier = 1, max_multiplier = 1)]
  out[!is.finite(min_multiplier), min_multiplier := 0]
  out[!is.finite(max_multiplier), max_multiplier := pmax(min_multiplier, 1)]
  out[max_multiplier < min_multiplier, max_multiplier := min_multiplier]
  out[]
}

opsp_normalize_variable_group_map <- function(variables, variable_group_map = NULL) {
  vars <- unique(as.character(variables))
  out <- data.table::data.table(variable = vars, planning_group = vars)
  if (is.null(variable_group_map) || !nrow(opsp_as_dt(variable_group_map))) return(out[])
  gm <- opsp_as_dt(variable_group_map, "variable_group_map")
  if (!"variable" %in% names(gm)) stop("variable_group_map must include variable.", call. = FALSE)
  gcol <- opsp_pick_col(gm, c("planning_group", "constraint_group", "group", "parent_channel", "channel_group", "product", "portfolio", "line_of_business", "lob"))
  if (is.na(gcol)) stop("variable_group_map must include a group column such as planning_group, parent_channel, product, or line_of_business.", call. = FALSE)
  gm <- gm[, .(variable = as.character(variable), planning_group = as.character(get(gcol)))]
  gm <- gm[nzchar(variable)]
  dup <- gm[duplicated(variable), unique(variable)]
  if (length(dup)) stop("variable_group_map has duplicate variable rows: ", paste(dup, collapse = ", "), call. = FALSE)
  gm[!nzchar(planning_group) | is.na(planning_group), planning_group := variable]
  out[gm, planning_group := i.planning_group, on = "variable"]
  out[]
}

opsp_normalize_group_constraints <- function(group_constraints = NULL, total_budget = NA_real_) {
  gc <- opsp_as_dt(group_constraints, "group_constraints")
  if (!nrow(gc)) return(data.table::data.table())
  gcol <- opsp_pick_col(gc, c("planning_group", "constraint_group", "group", "parent_channel", "channel_group", "product", "portfolio", "line_of_business", "lob"))
  if (is.na(gcol)) stop("group_constraints must include a group column such as planning_group, parent_channel, product, or line_of_business.", call. = FALSE)
  if (!identical(gcol, "planning_group")) data.table::setnames(gc, gcol, "planning_group")
  gc[, planning_group := as.character(planning_group)]
  gc <- gc[nzchar(planning_group)]
  dup <- gc[duplicated(planning_group), unique(planning_group)]
  if (length(dup)) stop("group_constraints has duplicate planning_group rows: ", paste(dup, collapse = ", "), call. = FALSE)
  for (cc in c("min_spend", "max_spend", "fixed_spend", "min_share", "max_share")) {
    if (!cc %in% names(gc)) gc[, (cc) := NA_real_]
    gc[, (cc) := opsp_num(get(cc))]
  }
  total_budget <- opsp_num(total_budget)[1]
  if (is.finite(total_budget) && total_budget >= 0) {
    gc[is.finite(min_share), min_spend := pmax(opsp_num(min_spend), min_share * total_budget, na.rm = TRUE)]
    gc[is.finite(max_share), max_spend := pmin(opsp_num(max_spend), max_share * total_budget, na.rm = TRUE)]
  }
  gc[is.finite(fixed_spend), `:=`(min_spend = pmax(fixed_spend, 0), max_spend = pmax(fixed_spend, 0))]
  gc[!is.finite(min_spend), min_spend := NA_real_]
  gc[!is.finite(max_spend), max_spend := NA_real_]
  gc[is.finite(min_spend) & is.finite(max_spend) & max_spend < min_spend, max_spend := min_spend]
  gc[, .(planning_group, min_spend, max_spend, fixed_spend, min_share, max_share)]
}

opsp_group_constraint_status <- function(engine,
                                         vars,
                                         multipliers,
                                         variable_group_map = NULL,
                                         group_constraints = NULL,
                                         total_budget = NA_real_) {
  gc <- opsp_normalize_group_constraints(group_constraints, total_budget = total_budget)
  gm <- opsp_normalize_variable_group_map(vars, variable_group_map)
  mult <- stats::setNames(as.numeric(multipliers[vars]), vars)
  spend <- data.table::data.table(
    variable = vars,
    spend = vapply(vars, function(v) opsp_spend_at(engine, v, mult[v]), numeric(1))
  )
  spend[gm, planning_group := i.planning_group, on = "variable"]
  spend[!nzchar(planning_group) | is.na(planning_group), planning_group := variable]
  rollup <- spend[, .(spend = sum(spend, na.rm = TRUE)), by = planning_group]
  if (!nrow(gc)) return(list(ok = TRUE, max_violation = 0, rollup = rollup))
  rollup[gc, `:=`(
    min_spend = i.min_spend,
    max_spend = i.max_spend,
    fixed_spend = i.fixed_spend,
    min_share = i.min_share,
    max_share = i.max_share
  ), on = "planning_group"]
  missing_groups <- setdiff(gc$planning_group, rollup$planning_group)
  if (length(missing_groups)) {
    rollup <- data.table::rbindlist(list(
      rollup,
      gc[planning_group %in% missing_groups, .(planning_group, spend = 0, min_spend, max_spend, fixed_spend, min_share, max_share)]
    ), fill = TRUE)
  }
  rollup[, `:=`(
    min_violation = data.table::fifelse(is.finite(min_spend), pmax(min_spend - spend, 0), 0),
    max_violation = data.table::fifelse(is.finite(max_spend), pmax(spend - max_spend, 0), 0)
  )]
  max_violation <- rollup[, max(c(min_violation, max_violation), na.rm = TRUE)]
  list(ok = is.finite(max_violation) && max_violation <= 1e-6, max_violation = max_violation, rollup = rollup[])
}

opsp_channel_constraint_status <- function(engine,
                                           vars,
                                           multipliers,
                                           constraints_table) {
  vars <- as.character(vars)
  cs <- opsp_as_dt(constraints_table, "constraints_table")
  if (!nrow(cs)) return(list(ok = TRUE, max_violation = 0, detail = data.table::data.table()))
  for (cc in c("min_spend", "max_spend", "fixed_spend")) {
    if (!cc %in% names(cs)) cs[, (cc) := NA_real_]
    cs[, (cc) := opsp_num(get(cc))]
  }
  mult <- stats::setNames(as.numeric(multipliers[vars]), vars)
  detail <- data.table::data.table(
    variable = vars,
    spend = vapply(vars, function(v) opsp_spend_at(engine, v, mult[v]), numeric(1))
  )
  detail[cs[, .(variable = as.character(variable), min_spend, max_spend, fixed_spend)],
         on = "variable", `:=`(
           min_spend = i.min_spend,
           max_spend = i.max_spend,
           fixed_spend = i.fixed_spend
         )]
  detail[is.finite(fixed_spend), `:=`(
    min_spend = pmax(fixed_spend, 0),
    max_spend = pmax(fixed_spend, 0)
  )]
  for (cc in c("min_spend", "max_spend")) if (!cc %in% names(detail)) detail[, (cc) := NA_real_]
  detail[, `:=`(
    min_violation = data.table::fifelse(is.finite(min_spend), pmax(min_spend - spend, 0), 0),
    max_violation = data.table::fifelse(is.finite(max_spend), pmax(spend - max_spend, 0), 0)
  )]
  max_violation <- detail[, max(c(min_violation, max_violation), na.rm = TRUE)]
  list(ok = is.finite(max_violation) && max_violation <= 1e-6, max_violation = max_violation, detail = detail[])
}

opsp_normalize_support_cost_map <- function(support_cost_map = NULL) {
  scm <- opsp_as_dt(support_cost_map, "support_cost_map")
  if (!nrow(scm)) return(data.table::data.table())
  if (!"variable" %in% names(scm)) stop("support_cost_map must include variable.", call. = FALSE)
  scm[, variable := as.character(variable)]
  opsp_check_unique_variables(scm, "support_cost_map")
  unit_col <- opsp_pick_col(scm, c("support_unit", "unit", "media_unit"))
  cps_col <- opsp_pick_col(scm, c("cost_per_support", "spend_per_support", "cost_per_unit", "unit_cost",
                                  "cpp", "cost_per_point", "cost_per_grp", "cost_per_rating_point"))
  cpm_col <- opsp_pick_col(scm, c("cpm", "cost_per_thousand", "cost_per_mille"))
  cur_sup_col <- opsp_pick_col(scm, c("current_support", "base_support", "historical_support",
                                      "current_impressions", "base_impressions", "current_grps"))
  cur_spend_col <- opsp_pick_col(scm, c("current_spend", "base_spend", "historical_spend"))
  out <- scm[, .(variable)]
  out[, support_cost_per_unit := NA_real_]
  out[, support_cost_source := NA_character_]
  if (!is.na(cps_col)) {
    out[, `:=`(
      support_cost_per_unit = opsp_num(scm[[cps_col]]),
      support_cost_source = cps_col
    )]
  }
  if (!is.na(cpm_col)) {
    cpm_val <- opsp_num(scm[[cpm_col]]) / 1000
    out[!is.finite(support_cost_per_unit), `:=`(
      support_cost_per_unit = cpm_val[!is.finite(support_cost_per_unit)],
      support_cost_source = cpm_col
    )]
  }
  out[, current_support_map := if (!is.na(cur_sup_col)) opsp_num(scm[[cur_sup_col]]) else NA_real_]
  out[, current_spend_map := if (!is.na(cur_spend_col)) opsp_num(scm[[cur_spend_col]]) else NA_real_]
  out[, support_unit := if (!is.na(unit_col)) as.character(scm[[unit_col]]) else NA_character_]
  out[!is.finite(support_cost_per_unit) | support_cost_per_unit < 0, support_cost_per_unit := NA_real_]
  out[]
}

opsp_apply_support_costs_to_curves <- function(rc, support_cost_map = NULL) {
  out <- data.table::copy(rc)
  scm <- opsp_normalize_support_cost_map(support_cost_map)
  cost_cols <- c("cost_per_support", "spend_per_support", "cost_per_unit", "unit_cost",
                 "cpp", "cost_per_point", "cost_per_grp", "cost_per_rating_point")
  cpm_cols <- c("cpm", "cost_per_thousand", "cost_per_mille")
  curve_cost_col <- opsp_pick_col(out, cost_cols)
  curve_cpm_col <- opsp_pick_col(out, cpm_cols)
  if (!"support_cost_per_unit" %in% names(out)) out[, support_cost_per_unit := NA_real_]
  if (!"support_cost_source" %in% names(out)) out[, support_cost_source := NA_character_]
  if (!is.na(curve_cost_col)) out[, `:=`(support_cost_per_unit = opsp_num(get(curve_cost_col)), support_cost_source = curve_cost_col)]
  if (!is.na(curve_cpm_col)) {
    out[!is.finite(support_cost_per_unit), `:=`(
      support_cost_per_unit = opsp_num(get(curve_cpm_col)) / 1000,
      support_cost_source = curve_cpm_col
    )]
  }
  if (!"support_unit" %in% names(out)) out[, support_unit := NA_character_]
  if (nrow(scm)) {
    out[scm, `:=`(
      support_cost_per_unit = data.table::fifelse(is.finite(support_cost_per_unit), support_cost_per_unit, i.support_cost_per_unit),
      support_cost_source = data.table::fifelse(!is.na(support_cost_source) & nzchar(as.character(support_cost_source)), as.character(support_cost_source), i.support_cost_source),
      support_unit = data.table::fifelse(!is.na(support_unit) & nzchar(as.character(support_unit)), as.character(support_unit), i.support_unit)
    ), on = "variable"]
    if ("current_support_map" %in% names(scm)) {
      if (!"current_support" %in% names(out)) out[, current_support := NA_real_]
      out[scm, current_support := data.table::fifelse(is.finite(current_support), current_support, i.current_support_map), on = "variable"]
    }
    if ("current_spend_map" %in% names(scm)) {
      if (!"current_spend" %in% names(out)) out[, current_spend := NA_real_]
      out[scm, current_spend := data.table::fifelse(is.finite(current_spend), current_spend, i.current_spend_map), on = "variable"]
    }
  }
  if ("support" %in% names(out) && "support_cost_per_unit" %in% names(out)) {
    if (!"spend" %in% names(out)) out[, spend := NA_real_]
    out[!is.finite(spend) & is.finite(support) & is.finite(support_cost_per_unit),
        spend := support * support_cost_per_unit]
  }
  if ("current_support" %in% names(out) && "support_cost_per_unit" %in% names(out)) {
    if (!"current_spend" %in% names(out)) out[, current_spend := NA_real_]
    out[!is.finite(current_spend) & is.finite(current_support) & is.finite(support_cost_per_unit),
        current_spend := current_support * support_cost_per_unit]
  }
  out[]
}

opsp_normalize_response_curves <- function(response_curves, support_cost_map = NULL) {
  rc <- opsp_as_dt(response_curves, "response_curves")
  if (!nrow(rc)) return(rc)
  vcol <- opsp_pick_col(rc, c("variable", "channel", "media", "driver"))
  if (is.na(vcol)) stop("response_curves must include variable/channel/media/driver.", call. = FALSE)
  if (!identical(vcol, "variable")) data.table::setnames(rc, vcol, "variable")
  mcol <- opsp_pick_col(rc, c("spend_multiplier", "support_multiplier", "multiplier", "scale", "omega"))
  scol <- opsp_pick_col(rc, c("spend", "planned_spend", "total_spend", "cost"))
  csc <- opsp_pick_col(rc, c("current_spend", "base_spend", "historical_spend"))
  sup_col <- opsp_pick_col(rc, c("support", "planned_support", "total_support", "media_support", "modeled_support"))
  cur_sup_col <- opsp_pick_col(rc, c("current_support", "base_support", "historical_support"))
  ccol <- opsp_pick_col(rc, c("contribution", "incremental_outcome", "expected_contribution", "expected_kpi", "outcome", "kpi"))
  if (is.na(ccol)) stop("response_curves must include contribution/incremental_outcome/expected_kpi.", call. = FALSE)
  if (!identical(ccol, "contribution")) data.table::setnames(rc, ccol, "contribution")
  aggregate_mode <- "mean"
  if ("scope" %in% names(rc) && any(tolower(as.character(rc$scope)) == "total", na.rm = TRUE)) {
    rc <- rc[tolower(as.character(scope)) == "total"]
  } else if ("group" %in% names(rc) && any(toupper(as.character(rc$group)) == "ALL", na.rm = TRUE)) {
    rc <- rc[toupper(as.character(group)) == "ALL"]
  } else if ("scope" %in% names(rc) || "group" %in% names(rc)) {
    aggregate_mode <- "sum_groups"
  }
  if (!is.na(scol) && !identical(scol, "spend")) data.table::setnames(rc, scol, "spend")
  if (!is.na(csc) && !identical(csc, "current_spend")) data.table::setnames(rc, csc, "current_spend")
  if (!is.na(sup_col) && !identical(sup_col, "support")) data.table::setnames(rc, sup_col, "support")
  if (!is.na(cur_sup_col) && !identical(cur_sup_col, "current_support")) data.table::setnames(rc, cur_sup_col, "current_support")
  rc <- opsp_apply_support_costs_to_curves(rc, support_cost_map = support_cost_map)
  if (!is.na(mcol)) {
    if (!identical(mcol, "spend_multiplier")) data.table::setnames(rc, mcol, "spend_multiplier")
    rc[, spend_multiplier := opsp_num(spend_multiplier)]
  } else if ("spend" %in% names(rc) && "current_spend" %in% names(rc)) {
    rc[, spend_multiplier := opsp_safe_div(spend, current_spend)]
  } else if ("support" %in% names(rc) && "current_support" %in% names(rc)) {
    rc[, spend_multiplier := opsp_safe_div(support, current_support)]
  } else {
    stop("response_curves must include a multiplier, spend plus current_spend, or support plus current_support.", call. = FALSE)
  }
  rc[, `:=`(
    variable = as.character(variable),
    spend_multiplier = opsp_num(spend_multiplier),
    contribution = opsp_num(contribution)
  )]
  if ("spend" %in% names(rc)) rc[, spend := opsp_num(spend)]
  if ("current_spend" %in% names(rc)) rc[, current_spend := opsp_num(current_spend)]
  if ("support" %in% names(rc)) rc[, support := opsp_num(support)]
  if ("current_support" %in% names(rc)) rc[, current_support := opsp_num(current_support)]
  if ("support_cost_per_unit" %in% names(rc)) rc[, support_cost_per_unit := opsp_num(support_cost_per_unit)]
  if (!"spend" %in% names(rc)) rc[, spend := NA_real_]
  if ("current_spend" %in% names(rc)) {
    rc[!is.finite(spend) & is.finite(current_spend) & is.finite(spend_multiplier),
       spend := current_spend * spend_multiplier]
  }
  rc <- rc[is.finite(spend_multiplier) & is.finite(contribution) & nzchar(variable)]
  if (!nrow(rc)) stop("response_curves has no finite variable/multiplier/contribution rows.", call. = FALSE)
  rc[, .(
    contribution = if (identical(aggregate_mode, "sum_groups")) sum(contribution, na.rm = TRUE) else mean(contribution, na.rm = TRUE),
    spend = if ("spend" %in% names(rc)) {
      if (identical(aggregate_mode, "sum_groups")) sum(spend, na.rm = TRUE) else mean(spend, na.rm = TRUE)
    } else NA_real_,
    current_spend = if ("current_spend" %in% names(rc)) {
      if (identical(aggregate_mode, "sum_groups")) sum(current_spend, na.rm = TRUE) else mean(current_spend, na.rm = TRUE)
    } else NA_real_,
    support = if ("support" %in% names(rc)) {
      if (identical(aggregate_mode, "sum_groups")) sum(support, na.rm = TRUE) else mean(support, na.rm = TRUE)
    } else NA_real_,
    current_support = if ("current_support" %in% names(rc)) {
      if (identical(aggregate_mode, "sum_groups")) sum(current_support, na.rm = TRUE) else mean(current_support, na.rm = TRUE)
    } else NA_real_,
    support_cost_per_unit = if ("support_cost_per_unit" %in% names(rc)) {
      mean(support_cost_per_unit, na.rm = TRUE)
    } else NA_real_,
    support_unit = if ("support_unit" %in% names(rc)) {
      paste(sort(unique(as.character(support_unit[!is.na(support_unit) & nzchar(as.character(support_unit))]))), collapse = "|")
    } else NA_character_,
    support_cost_source = if ("support_cost_source" %in% names(rc)) {
      paste(sort(unique(as.character(support_cost_source[!is.na(support_cost_source) & nzchar(as.character(support_cost_source))]))), collapse = "|")
    } else NA_character_
  ), by = .(variable, spend_multiplier)][order(variable, spend_multiplier)]
}

opsp_normalize_response_curve_draws <- function(response_curve_draws, support_cost_map = NULL) {
  dc <- opsp_as_dt(response_curve_draws, "response_curve_draws")
  if (!nrow(dc)) return(data.table::data.table())
  dcol <- opsp_pick_col(dc, c(".draw", "draw", "posterior_draw", "sample", "iteration"))
  if (is.na(dcol)) return(data.table::data.table())
  if (!identical(dcol, ".draw")) data.table::setnames(dc, dcol, ".draw")
  dc[, .draw := as.character(.draw)]
  dc <- dc[nzchar(.draw)]
  if (!nrow(dc)) return(data.table::data.table())
  out <- data.table::rbindlist(lapply(unique(dc$.draw), function(dd) {
    tmp <- data.table::copy(dc[.draw == dd])
    tmp[, .draw := NULL]
    norm <- tryCatch(opsp_normalize_response_curves(tmp, support_cost_map = support_cost_map), error = function(e) data.table::data.table())
    if (nrow(norm)) norm[, .draw := dd]
    norm
  }), fill = TRUE)
  if (!nrow(out)) return(out)
  data.table::setcolorder(out, c(".draw", setdiff(names(out), ".draw")))
  out[order(variable, .draw, spend_multiplier)]
}

opsp_response_curve_engine <- function(response_curves, variables = NULL, constraints = NULL, support_cost_map = NULL) {
  rc <- opsp_normalize_response_curves(response_curves, support_cost_map = support_cost_map)
  if (!is.null(variables)) rc <- rc[variable %in% as.character(variables)]
  if (!nrow(rc)) stop("No response-curve rows remain for requested variables.", call. = FALSE)

  cur <- rc[, {
    cs <- current_spend[is.finite(current_spend) & current_spend >= 0]
    if (length(cs)) {
      current <- median(cs, na.rm = TRUE)
    } else if (any(abs(spend_multiplier - 1) <= 1e-8) && "spend" %in% names(.SD)) {
      current <- spend[which.min(abs(spend_multiplier - 1))]
    } else {
      current <- NA_real_
    }
    csp <- if ("current_support" %in% names(.SD)) current_support[is.finite(current_support) & current_support >= 0] else numeric()
    if (length(csp)) {
      current_support <- median(csp, na.rm = TRUE)
    } else if ("support" %in% names(.SD) && any(abs(spend_multiplier - 1) <= 1e-8)) {
      current_support <- support[which.min(abs(spend_multiplier - 1))]
    } else {
      current_support <- NA_real_
    }
    current_multiplier <- if (is.finite(current) && "spend" %in% names(.SD) && any(is.finite(spend))) {
      spend_ok <- is.finite(spend)
      spend_multiplier[spend_ok][which.min(abs(spend[spend_ok] - current))]
    } else {
      1
    }
    if (!is.finite(current_multiplier)) current_multiplier <- 1
    data.table::data.table(current_spend = current, current_support = current_support, current_multiplier = current_multiplier)
  }, by = variable]
  if (!is.null(constraints) && nrow(opsp_as_dt(constraints))) {
    cs <- opsp_as_dt(constraints)
    if ("variable" %in% names(cs) && "current_spend" %in% names(cs)) {
      cur[cs[, .(variable = as.character(variable), current_spend_override = opsp_num(current_spend))],
          current_spend := data.table::fifelse(is.finite(i.current_spend_override), i.current_spend_override, current_spend),
          on = "variable"]
    }
  }
  bad <- cur[!is.finite(current_spend) | current_spend < 0, variable]
  if (length(bad)) {
    stop("Could not infer current_spend for response-curve variables: ", paste(bad, collapse = ", "),
         ". Add current_spend to response_curves or constraints.", call. = FALSE)
  }

  eval_one <- function(variable, multiplier) {
    opsp_interp_curve_value(rc, variable, multiplier, "contribution", fallback = NA_real_)
  }
  spend_one <- function(variable, multiplier) {
    vv <- as.character(variable)[1]
    fallback <- cur[variable == vv, current_spend][1] * as.numeric(multiplier)[1]
    opsp_interp_curve_value(rc, variable, multiplier, "spend", fallback = fallback)
  }
  support_one <- function(variable, multiplier) {
    vv <- as.character(variable)[1]
    current_support <- cur[variable == vv, current_support][1]
    fallback <- if (is.finite(current_support)) current_support * as.numeric(multiplier)[1] else NA_real_
    opsp_interp_curve_value(rc, variable, multiplier, "support", fallback = fallback)
  }
  list(
    mode = "response_curve_table",
    variables = cur$variable,
    current_spend = stats::setNames(cur$current_spend, cur$variable),
    current_support = stats::setNames(cur$current_support, cur$variable),
    current_multiplier = stats::setNames(cur$current_multiplier, cur$variable),
    spend = spend_one,
    support = support_one,
    contribution = eval_one,
    source_table = rc
  )
}

opsp_response_curve_draws_from_inputs <- function(fit_obj = NULL,
                                                  response_curve_draws = NULL,
                                                  response_curves = NULL,
                                                  spend_map = NULL,
                                                  raw_data = NULL,
                                                  variables = NULL,
                                                  multiplier_grid = seq(0, 3, by = 0.05),
                                                  step_pct = 0.01,
                                                  spend_suffix = "_spend",
                                                  support_cost_map = NULL,
                                                  uncertainty = c("auto", "none", "draws"),
                                                  posterior_draw_count = 200L,
                                                  posterior_draw_seed = 123L) {
  uncertainty <- match.arg(uncertainty)
  if (identical(uncertainty, "none")) return(data.table::data.table())
  if (!is.null(response_curve_draws) && nrow(opsp_as_dt(response_curve_draws))) {
    return(opsp_normalize_response_curve_draws(response_curve_draws, support_cost_map = support_cost_map))
  }
  if (!is.null(response_curves) && nrow(opsp_as_dt(response_curves))) {
    from_rc <- opsp_normalize_response_curve_draws(response_curves, support_cost_map = support_cost_map)
    if (nrow(from_rc)) return(from_rc)
  }
  if (!is.null(fit_obj) && !is.null(fit_obj$response_curves_draws) && nrow(opsp_as_dt(fit_obj$response_curves_draws))) {
    return(opsp_normalize_response_curve_draws(fit_obj$response_curves_draws, support_cost_map = support_cost_map))
  }
  if (!is.null(fit_obj) && identical(uncertainty, "draws")) {
    opsp_source_hier_if_needed()
    if (exists("build_response_curves_draws_hier_mmm", mode = "function")) {
      draw_curves <- tryCatch(
        build_response_curves_draws_hier_mmm(
          fit_obj = fit_obj,
          spend_map = spend_map,
          raw_data = raw_data,
          variables = variables,
          multiplier_grid = multiplier_grid,
          step_pct = step_pct,
          spend_suffix = spend_suffix,
          max_draws = posterior_draw_count,
          seed = posterior_draw_seed
        ),
        error = function(e) {
          warning("Could not build posterior-draw response curves: ", conditionMessage(e), call. = FALSE)
          data.table::data.table()
        }
      )
      return(opsp_normalize_response_curve_draws(draw_curves, support_cost_map = support_cost_map))
    }
  }
  data.table::data.table()
}

opsp_fit_engine <- function(fit_obj, spend_map = NULL, raw_data = NULL,
                            variables = NULL, spend_suffix = "_spend",
                            support_cost_map = NULL) {
  opsp_source_hier_if_needed()
  vars <- opsp_infer_variables(fit_obj = fit_obj, variables = variables)
  existing_curves <- opsp_as_dt(fit_obj$response_curves, "fit_obj$response_curves")
  if (nrow(existing_curves)) {
    curve_vars <- if (is.null(variables)) NULL else vars
    curve_check <- opsp_normalize_response_curves(existing_curves, support_cost_map = support_cost_map)
    if (!is.null(curve_vars)) curve_check <- curve_check[variable %in% curve_vars]
    has_spend <- nrow(curve_check) &&
      "current_spend" %in% names(curve_check) &&
      all(curve_check[, any(is.finite(current_spend) & current_spend >= 0), by = variable]$V1)
    if (isTRUE(has_spend)) {
      eng <- opsp_response_curve_engine(existing_curves, variables = curve_vars, constraints = NULL, support_cost_map = support_cost_map)
      eng$mode <- "stan_response_curve_sheet"
      return(eng)
    }
  }
  sm <- normalize_spend_map_hier_mmm(fit_obj, spend_map = spend_map, raw_data = raw_data,
                                     spend_suffix = spend_suffix, variables = vars)
  if (!nrow(sm)) stop("MMM fit mode needs spend_map, metadata spend_col/cost_col, or inferable *_spend columns.", call. = FALSE)
  raw_aligned <- get_aligned_raw_data_hier_mmm(fit_obj, raw_data = raw_data)
  missing_cols <- sm[!(spend_col %in% names(raw_aligned)), spend_col]
  if (length(missing_cols)) stop("Spend columns missing from aligned raw data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  sm[, current_spend := vapply(spend_col, function(cc) sum(opsp_num(raw_aligned[[cc]]), na.rm = TRUE), numeric(1))]
  sm[, current_support := vapply(variable, function(vv) {
    if (vv %in% names(raw_aligned)) sum(opsp_num(raw_aligned[[vv]]), na.rm = TRUE) else NA_real_
  }, numeric(1))]
  sm <- sm[current_spend >= 0]
  if (!nrow(sm)) stop("No non-negative spend rows available for optimizer.", call. = FALSE)
  eval_one <- function(variable, multiplier) {
    variable_contribution_sum_hier_mmm(fit_obj, as.character(variable)[1], multiplier = as.numeric(multiplier)[1])
  }
  spend_one <- function(variable, multiplier) {
    sm[variable == as.character(variable)[1], current_spend][1] * as.numeric(multiplier)[1]
  }
  support_one <- function(variable, multiplier) {
    cur_support <- sm[variable == as.character(variable)[1], current_support][1]
    if (is.finite(cur_support)) cur_support * as.numeric(multiplier)[1] else NA_real_
  }
  list(
    mode = "stan_mmm_fit",
    variables = sm$variable,
    current_spend = stats::setNames(sm$current_spend, sm$variable),
    current_support = stats::setNames(sm$current_support, sm$variable),
    current_multiplier = stats::setNames(rep(1, nrow(sm)), sm$variable),
    spend_map = sm,
    spend = spend_one,
    support = support_one,
    contribution = eval_one,
    source_table = data.table::data.table()
  )
}

opsp_engine_from_inputs <- function(fit_obj = NULL,
                                    response_curves = NULL,
                                    spend_map = NULL,
                                    raw_data = NULL,
                                    variables = NULL,
                                    constraints = NULL,
                                    support_cost_map = NULL,
                                    spend_suffix = "_spend") {
  if (!is.null(fit_obj)) {
    return(opsp_fit_engine(fit_obj, spend_map = spend_map, raw_data = raw_data,
                           variables = variables, spend_suffix = spend_suffix,
                           support_cost_map = support_cost_map))
  }
  if (!is.null(response_curves) && nrow(opsp_as_dt(response_curves))) {
    return(opsp_response_curve_engine(response_curves, variables = variables, constraints = constraints, support_cost_map = support_cost_map))
  }
  stop("Pass either fit_obj or response_curves.", call. = FALSE)
}

opsp_spend_at <- function(engine, variable, multiplier) {
  if (!is.null(engine$spend) && is.function(engine$spend)) {
    return(engine$spend(as.character(variable)[1], as.numeric(multiplier)[1]))
  }
  engine$current_spend[[as.character(variable)[1]]] * as.numeric(multiplier)[1]
}

opsp_support_at <- function(engine, variable, multiplier) {
  if (!is.null(engine$support) && is.function(engine$support)) {
    return(engine$support(as.character(variable)[1], as.numeric(multiplier)[1]))
  }
  cs <- if (!is.null(engine$current_support)) engine$current_support[[as.character(variable)[1]]] else NA_real_
  if (is.finite(cs)) cs * as.numeric(multiplier)[1] else NA_real_
}

opsp_multiplier_for_support <- function(engine, variable, target_support) {
  v <- as.character(variable)[1]
  target_support <- opsp_num(target_support)[1]
  if (!is.finite(target_support) || target_support < 0) return(NA_real_)
  tbl <- if (!is.null(engine$source_table)) engine$source_table else data.table::data.table()
  if (nrow(tbl) && all(c("variable", "support", "spend_multiplier") %in% names(tbl))) {
    tmp <- tbl[variable == v & is.finite(support) & is.finite(spend_multiplier)]
    if (nrow(tmp) >= 2L) {
      data.table::setorder(tmp, support, spend_multiplier)
      tmp <- tmp[, .(spend_multiplier = mean(spend_multiplier, na.rm = TRUE)), by = support]
      if (nrow(tmp) >= 2L) {
        return(stats::approx(tmp$support, tmp$spend_multiplier, xout = target_support, rule = 2, ties = mean)$y)
      }
    } else if (nrow(tmp) == 1L && abs(tmp$support[1] - target_support) <= 1e-8) {
      return(tmp$spend_multiplier[1])
    }
  }
  current_support <- if (!is.null(engine$current_support) && v %in% names(engine$current_support)) engine$current_support[[v]] else NA_real_
  if (is.finite(current_support) && current_support > 1e-8) target_support / current_support else NA_real_
}

opsp_eval_variable <- function(engine, variable, multiplier, step_pct = 0.01, value_per_kpi = NA_real_) {
  v <- as.character(variable)[1]
  m <- as.numeric(multiplier)[1]
  cur_spend <- engine$current_spend[[v]]
  current_multiplier <- if (!is.null(engine$current_multiplier) && v %in% names(engine$current_multiplier)) {
    as.numeric(engine$current_multiplier[[v]])
  } else {
    1
  }
  if (!is.finite(current_multiplier)) current_multiplier <- 1
  current_contribution <- engine$contribution(v, current_multiplier)
  contrib <- engine$contribution(v, m)
  up <- engine$contribution(v, m + step_pct)
  inc <- up - contrib
  spend <- opsp_spend_at(engine, v, m)
  up_spend <- opsp_spend_at(engine, v, m + step_pct)
  inc_spend <- up_spend - spend
  support <- opsp_support_at(engine, v, m)
  current_support <- if (!is.null(engine$current_support)) engine$current_support[[v]] else NA_real_
  source_table <- if (!is.null(engine$source_table)) engine$source_table else data.table::data.table()
  support_cost_per_unit <- opsp_interp_curve_value(source_table, v, m, "support_cost_per_unit", fallback = NA_real_)
  support_unit <- if (nrow(source_table) && "support_unit" %in% names(source_table)) {
    vals <- unique(as.character(source_table[variable == v, support_unit]))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals)) paste(sort(vals), collapse = "|") else NA_character_
  } else {
    NA_character_
  }
  support_cost_source <- if (nrow(source_table) && "support_cost_source" %in% names(source_table)) {
    vals <- unique(as.character(source_table[variable == v, support_cost_source]))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals)) paste(sort(vals), collapse = "|") else NA_character_
  } else {
    NA_character_
  }
  data.table::data.table(
    variable = v,
    spend_multiplier = m,
    spend = spend,
    support = support,
    current_support = current_support,
    support_cost_per_unit = support_cost_per_unit,
    support_unit = support_unit,
    support_cost_source = support_cost_source,
    current_multiplier = current_multiplier,
    contribution = contrib,
    contribution_vs_current = contrib - current_contribution,
    roi = opsp_safe_div(contrib, spend),
    mroi = opsp_safe_div(inc, inc_spend),
    incremental_spend_for_mroi = inc_spend,
    incremental_contribution_for_mroi = inc,
    cost_per_kpi = ifelse(is.finite(contrib) && contrib > 1e-8, spend / contrib, NA_real_),
    value_per_cost = ifelse(is.finite(value_per_kpi) && is.finite(spend) && abs(spend) > 1e-8,
                            contrib * value_per_kpi / spend, NA_real_)
  )
}

opsp_eval_table <- function(engine, multipliers, step_pct = 0.01, value_per_kpi = NA_real_) {
  rows <- list()
  idx <- 0L
  for (v in engine$variables) {
    for (m in multipliers) {
      idx <- idx + 1L
      rows[[idx]] <- opsp_eval_variable(engine, v, m, step_pct = step_pct, value_per_kpi = value_per_kpi)
    }
  }
  data.table::rbindlist(rows, fill = TRUE)
}

opsp_current_plan <- function(engine, step_pct = 0.01, value_per_kpi = NA_real_) {
  mult <- if (!is.null(engine$current_multiplier)) as.numeric(engine$current_multiplier[engine$variables]) else rep(1, length(engine$variables))
  names(mult) <- engine$variables
  mult[!is.finite(mult)] <- 1
  dt <- data.table::rbindlist(lapply(engine$variables, function(v) {
    opsp_eval_variable(engine, v, mult[v], step_pct = step_pct, value_per_kpi = value_per_kpi)
  }), fill = TRUE)
  dt[, current_multiplier := spend_multiplier]
  dt[, spend_multiplier := NULL]
  dt[, `:=`(
    current_spend = spend,
    current_support = support,
    current_contribution = contribution,
    current_roi = roi,
    current_mroi = mroi
  )]
  dt[, .(
    variable, current_spend, current_support, current_contribution, current_roi, current_mroi,
    cost_per_kpi, value_per_cost
  )][order(-current_spend)]
}

opsp_build_response_curves <- function(engine,
                                       multiplier_grid = seq(0, 3, by = 0.05),
                                       step_pct = 0.01,
                                       value_per_kpi = NA_real_) {
  multiplier_grid <- sort(unique(pmax(opsp_num(multiplier_grid), 0)))
  multiplier_grid <- multiplier_grid[is.finite(multiplier_grid)]
  if (!length(multiplier_grid)) stop("multiplier_grid must contain finite non-negative values.", call. = FALSE)
  rc <- opsp_eval_table(engine, multiplier_grid, step_pct = step_pct, value_per_kpi = value_per_kpi)
  rc[, decisioning_basis := paste0(engine$mode, "_point_estimate")]
  rc[]
}

opsp_build_saturation_headroom <- function(response_curves,
                                           current_multiplier = 1,
                                           high_saturation_pct = 0.90,
                                           moderate_saturation_pct = 0.70) {
  rc <- opsp_as_dt(response_curves, "response_curves")
  needed <- c("variable", "spend_multiplier", "spend", "contribution")
  if (!nrow(rc) || !all(needed %in% names(rc))) return(data.table::data.table())
  rc[, `:=`(
    variable = as.character(variable),
    spend_multiplier = opsp_num(spend_multiplier),
    spend = opsp_num(spend),
    contribution = opsp_num(contribution)
  )]
  for (cc in intersect(c("roi", "mroi", "cost_per_kpi"), names(rc))) rc[, (cc) := opsp_num(get(cc))]
  rc <- rc[nzchar(variable) & is.finite(spend_multiplier) & is.finite(contribution)]
  if (!nrow(rc)) return(data.table::data.table())

  out <- rc[order(spend_multiplier), {
    tmp <- .SD
    cur_mult <- if ("current_multiplier" %in% names(tmp) && any(is.finite(tmp$current_multiplier))) {
      stats::median(tmp$current_multiplier[is.finite(tmp$current_multiplier)], na.rm = TRUE)
    } else {
      current_multiplier
    }
    if (!is.finite(cur_mult)) cur_mult <- current_multiplier
    cur_i <- which.min(abs(tmp$spend_multiplier - cur_mult))
    max_i <- which.max(tmp$spend_multiplier)
    peak_i <- which.max(tmp$contribution)
    current_contribution <- tmp$contribution[cur_i]
    peak_contribution <- tmp$contribution[peak_i]
    pct_peak <- opsp_safe_div(current_contribution, peak_contribution)
    headroom <- peak_contribution - current_contribution
    target90 <- 0.90 * peak_contribution
    hit90 <- which(tmp$contribution >= target90)
    hit90_i <- if (length(hit90)) hit90[1] else NA_integer_
    curve_diff <- diff(tmp$contribution)
    non_monotonic <- any(is.finite(curve_diff) & curve_diff < -1e-6)
    saturation_band <- if (!is.finite(pct_peak) || !is.finite(peak_contribution) || peak_contribution <= 0) {
      "not_estimable"
    } else if (pct_peak >= high_saturation_pct) {
      "high_saturation_grid"
    } else if (pct_peak >= moderate_saturation_pct) {
      "moderate_saturation_grid"
    } else {
      "low_saturation_grid"
    }
    data.table::data.table(
      current_multiplier = tmp$spend_multiplier[cur_i],
      current_spend = tmp$spend[cur_i],
      current_support = if ("support" %in% names(tmp)) tmp$support[cur_i] else NA_real_,
      current_contribution = current_contribution,
      current_roi = if ("roi" %in% names(tmp)) tmp$roi[cur_i] else NA_real_,
      current_mroi = if ("mroi" %in% names(tmp)) tmp$mroi[cur_i] else NA_real_,
      current_cost_per_kpi = if ("cost_per_kpi" %in% names(tmp)) tmp$cost_per_kpi[cur_i] else NA_real_,
      max_grid_multiplier = tmp$spend_multiplier[max_i],
      max_grid_spend = tmp$spend[max_i],
      max_grid_support = if ("support" %in% names(tmp)) tmp$support[max_i] else NA_real_,
      contribution_at_max_grid_multiplier = tmp$contribution[max_i],
      peak_grid_multiplier = tmp$spend_multiplier[peak_i],
      peak_grid_spend = tmp$spend[peak_i],
      peak_grid_support = if ("support" %in% names(tmp)) tmp$support[peak_i] else NA_real_,
      peak_grid_contribution = peak_contribution,
      contribution_headroom_to_peak_grid = headroom,
      headroom_pct_of_current_contribution = opsp_safe_div(headroom, abs(current_contribution)),
      pct_of_peak_grid_contribution = pct_peak,
      multiplier_to_90pct_peak_grid = if (!is.na(hit90_i)) tmp$spend_multiplier[hit90_i] else NA_real_,
      spend_to_90pct_peak_grid = if (!is.na(hit90_i)) tmp$spend[hit90_i] else NA_real_,
      support_to_90pct_peak_grid = if (!is.na(hit90_i) && "support" %in% names(tmp)) tmp$support[hit90_i] else NA_real_,
      saturation_band = saturation_band,
      curve_non_monotonic_flag = isTRUE(non_monotonic),
      decisioning_basis = if ("decisioning_basis" %in% names(tmp)) tmp$decisioning_basis[cur_i] else NA_character_,
      interpretation_note = "Grid-based response-curve headroom diagnostic. It ranks planning headroom over the supplied curve grid; it is not standalone causal proof of true saturation."
    )
  }, by = variable]
  out[order(saturation_band, variable)][]
}

opsp_evaluate_scenario <- function(engine, multipliers, scenario_name,
                                   step_pct = 0.01,
                                   value_per_kpi = NA_real_) {
  vars <- engine$variables
  mult <- rep(1, length(vars))
  names(mult) <- vars
  if (!is.null(names(multipliers))) {
    hit <- intersect(names(multipliers), vars)
    mult[hit] <- opsp_num(multipliers[hit])
  } else if (length(multipliers) == 1L) {
    mult[] <- opsp_num(multipliers)[1]
  } else if (length(multipliers) == length(vars)) {
    mult[] <- opsp_num(multipliers)
  } else {
    stop("Scenario multipliers must be scalar, named by variable, or same length as variables.", call. = FALSE)
  }
  mult[!is.finite(mult) | mult < 0] <- 0
  rows <- data.table::rbindlist(lapply(vars, function(v) {
    opsp_eval_variable(engine, v, mult[v], step_pct = step_pct, value_per_kpi = value_per_kpi)
  }), fill = TRUE)
  rows[, scenario := scenario_name]
  rows[, spend_multiplier := as.numeric(mult[variable])]
  rows[, .(
    scenario, variable, spend_multiplier, spend, support, current_support, contribution,
    contribution_vs_current, roi, mroi, cost_per_kpi, value_per_cost
  )]
}

opsp_scenario_tables <- function(engine,
                                 scenario_multipliers = c(0.8, 1, 1.2),
                                 scenario_plan = NULL,
                                 step_pct = 0.01,
                                 value_per_kpi = NA_real_) {
  rows <- list()
  idx <- 0L
  for (m in scenario_multipliers) {
    idx <- idx + 1L
    rows[[idx]] <- opsp_evaluate_scenario(
      engine, m, scenario_name = paste0("all_channels_", format(round(m, 4), trim = TRUE), "x"),
      step_pct = step_pct, value_per_kpi = value_per_kpi
    )
  }
  if (!is.null(scenario_plan) && nrow(opsp_as_dt(scenario_plan))) {
    sp <- opsp_as_dt(scenario_plan, "scenario_plan")
    if (!"variable" %in% names(sp)) stop("scenario_plan must include variable.", call. = FALSE)
    if (!"scenario" %in% names(sp)) sp[, scenario := "custom_scenario"]
    spend_col <- opsp_pick_col(sp, c("spend", "recommended_spend", "planned_spend", "new_spend"))
    support_col <- opsp_pick_col(sp, c("support", "recommended_support", "planned_support", "new_support",
                                       "impressions", "planned_impressions", "grps", "planned_grps",
                                       "rating_points", "planned_rating_points"))
    mult_col <- opsp_pick_col(sp, c("spend_multiplier", "support_multiplier", "multiplier"))
    pct_col <- opsp_pick_col(sp, c("spend_delta_pct", "pct_change", "change_pct"))
    delta_col <- opsp_pick_col(sp, c("spend_delta", "incremental_spend"))
    support_pct_col <- opsp_pick_col(sp, c("support_delta_pct", "support_pct_change", "impressions_delta_pct", "grp_delta_pct"))
    support_delta_col <- opsp_pick_col(sp, c("support_delta", "incremental_support", "impressions_delta", "grp_delta"))
    sp[, variable := as.character(variable)]
    for (sc in unique(sp$scenario)) {
      mult <- stats::setNames(rep(1, length(engine$variables)), engine$variables)
      tmp <- sp[scenario == sc]
      for (i in seq_len(nrow(tmp))) {
        v <- tmp$variable[i]
        if (!(v %in% names(mult))) next
        cur <- engine$current_spend[[v]]
        cur_support <- if (!is.null(engine$current_support) && v %in% names(engine$current_support)) engine$current_support[[v]] else NA_real_
        val <- NA_real_
        if (!is.na(mult_col)) val <- opsp_num(tmp[[mult_col]][i])
        if (!is.finite(val) && !is.na(support_col)) val <- opsp_multiplier_for_support(engine, v, opsp_num(tmp[[support_col]][i]))
        if (!is.finite(val) && !is.na(spend_col) && is.finite(cur) && cur > 0) val <- opsp_num(tmp[[spend_col]][i]) / cur
        if (!is.finite(val) && !is.na(pct_col)) val <- 1 + opsp_num(tmp[[pct_col]][i])
        if (!is.finite(val) && !is.na(delta_col) && is.finite(cur) && cur > 0) val <- 1 + opsp_num(tmp[[delta_col]][i]) / cur
        if (!is.finite(val) && !is.na(support_pct_col)) val <- 1 + opsp_num(tmp[[support_pct_col]][i])
        if (!is.finite(val) && !is.na(support_delta_col) && is.finite(cur_support) && cur_support > 0) val <- 1 + opsp_num(tmp[[support_delta_col]][i]) / cur_support
        if (is.finite(val)) mult[v] <- pmax(val, 0)
      }
      idx <- idx + 1L
      rows[[idx]] <- opsp_evaluate_scenario(engine, mult, as.character(sc),
                                            step_pct = step_pct, value_per_kpi = value_per_kpi)
    }
  }
  detail <- data.table::rbindlist(rows, fill = TRUE)
  summary <- detail[, .(
    spend = sum(spend, na.rm = TRUE),
    contribution = sum(contribution, na.rm = TRUE),
    contribution_vs_current = sum(contribution_vs_current, na.rm = TRUE),
    roi = opsp_safe_div(sum(contribution, na.rm = TRUE), sum(spend, na.rm = TRUE)),
    cost_per_kpi = ifelse(sum(contribution, na.rm = TRUE) > 1e-8, sum(spend, na.rm = TRUE) / sum(contribution, na.rm = TRUE), NA_real_)
  ), by = scenario][order(-contribution)]
  list(detail = detail[], summary = summary[])
}

opsp_quantile <- function(x, p) {
  x <- opsp_num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 8))
}

opsp_validate_quantile <- function(p, fallback = 0.05) {
  p <- suppressWarnings(as.numeric(p)[1])
  if (!is.finite(p) || p <= 0 || p >= 1) {
    fallback <- suppressWarnings(as.numeric(fallback)[1])
    if (!is.finite(fallback) || fallback <= 0 || fallback >= 1) fallback <- 0.05
    return(fallback)
  }
  p
}

opsp_mean_finite <- function(x) {
  x <- opsp_num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

opsp_probability_positive <- function(x) {
  x <- opsp_num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x > 0)
}

opsp_robust_objective_choices <- function() {
  c(
    "q05_contribution", "q50_contribution", "mean_contribution",
    "q05_incremental_contribution", "quantile_incremental_contribution", "mean_incremental_contribution",
    "expected_utility", "probability_target",
    "q05_roi", "q05_incremental_roi", "quantile_incremental_roi", "q95_cost_per_kpi",
    "expected_profit", "q05_profit", "quantile_profit",
    "probability_profit_positive", "probability_incremental_contribution_positive"
  )
}

opsp_make_multiplier_sets <- function(vars,
                                      scenario_multipliers = c(0.8, 1, 1.2),
                                      scenario_plan = NULL,
                                      optimization_plan = NULL,
                                      engine = NULL) {
  vars <- as.character(vars)
  rows <- list()
  idx <- 0L
  for (m in scenario_multipliers) {
    idx <- idx + 1L
    rows[[idx]] <- data.table::data.table(
      scenario = paste0("all_channels_", format(round(m, 4), trim = TRUE), "x"),
      variable = vars,
      spend_multiplier = as.numeric(m)[1],
      scenario_type = "scenario"
    )
  }
  if (!is.null(scenario_plan) && nrow(opsp_as_dt(scenario_plan))) {
    sp <- opsp_as_dt(scenario_plan, "scenario_plan")
    if ("variable" %in% names(sp)) {
      if (!"scenario" %in% names(sp)) sp[, scenario := "custom_scenario"]
      spend_col <- opsp_pick_col(sp, c("spend", "recommended_spend", "planned_spend", "new_spend"))
      support_col <- opsp_pick_col(sp, c("support", "recommended_support", "planned_support", "new_support",
                                         "impressions", "planned_impressions", "grps", "planned_grps",
                                         "rating_points", "planned_rating_points"))
      mult_col <- opsp_pick_col(sp, c("spend_multiplier", "support_multiplier", "multiplier"))
      pct_col <- opsp_pick_col(sp, c("spend_delta_pct", "pct_change", "change_pct"))
      delta_col <- opsp_pick_col(sp, c("spend_delta", "incremental_spend"))
      support_pct_col <- opsp_pick_col(sp, c("support_delta_pct", "support_pct_change", "impressions_delta_pct", "grp_delta_pct"))
      support_delta_col <- opsp_pick_col(sp, c("support_delta", "incremental_support", "impressions_delta", "grp_delta"))
      sp[, variable := as.character(variable)]
      for (sc in unique(sp$scenario)) {
        mult <- stats::setNames(rep(1, length(vars)), vars)
        tmp <- sp[scenario == sc]
        for (i in seq_len(nrow(tmp))) {
          v <- tmp$variable[i]
          if (!(v %in% names(mult))) next
          val <- NA_real_
          if (!is.na(mult_col)) val <- opsp_num(tmp[[mult_col]][i])
          if (!is.finite(val) && !is.na(support_col) && !is.null(engine)) val <- opsp_multiplier_for_support(engine, v, opsp_num(tmp[[support_col]][i]))
          if (!is.finite(val) && !is.na(pct_col)) val <- 1 + opsp_num(tmp[[pct_col]][i])
          if (!is.finite(val) && !is.na(spend_col)) {
            cur_spend <- if ("current_spend" %in% names(tmp)) opsp_num(tmp$current_spend[i]) else NA_real_
            if ((!is.finite(cur_spend) || cur_spend <= 0) && !is.null(engine) && !is.null(engine$current_spend) && v %in% names(engine$current_spend)) {
              cur_spend <- engine$current_spend[[v]]
            }
            if (is.finite(cur_spend) && cur_spend > 0) val <- opsp_num(tmp[[spend_col]][i]) / cur_spend
          }
          if (!is.finite(val) && !is.na(delta_col)) {
            cur_spend <- if ("current_spend" %in% names(tmp)) opsp_num(tmp$current_spend[i]) else NA_real_
            if ((!is.finite(cur_spend) || cur_spend <= 0) && !is.null(engine) && !is.null(engine$current_spend) && v %in% names(engine$current_spend)) {
              cur_spend <- engine$current_spend[[v]]
            }
            if (is.finite(cur_spend) && cur_spend > 0) val <- 1 + opsp_num(tmp[[delta_col]][i]) / cur_spend
          }
          if (!is.finite(val) && !is.na(support_pct_col)) val <- 1 + opsp_num(tmp[[support_pct_col]][i])
          if (!is.finite(val) && !is.na(support_delta_col)) {
            cur_support <- if ("current_support" %in% names(tmp)) opsp_num(tmp$current_support[i]) else NA_real_
            if ((!is.finite(cur_support) || cur_support <= 0) && !is.null(engine) && !is.null(engine$current_support) && v %in% names(engine$current_support)) {
              cur_support <- engine$current_support[[v]]
            }
            if (is.finite(cur_support) && cur_support > 0) val <- 1 + opsp_num(tmp[[support_delta_col]][i]) / cur_support
          }
          if (is.finite(val)) mult[v] <- pmax(val, 0)
        }
        idx <- idx + 1L
        rows[[idx]] <- data.table::data.table(
          scenario = as.character(sc),
          variable = vars,
          spend_multiplier = as.numeric(mult[vars]),
          scenario_type = "scenario"
        )
      }
    }
  }
  if (!is.null(optimization_plan) && nrow(opsp_as_dt(optimization_plan))) {
    op <- opsp_as_dt(optimization_plan, "optimization_plan")
    if (all(c("variable", "recommended_multiplier") %in% names(op))) {
      idx <- idx + 1L
      rows[[idx]] <- op[, .(
        scenario = "optimized",
        variable = as.character(variable),
        spend_multiplier = opsp_num(recommended_multiplier),
        scenario_type = "optimized"
      )]
    }
  }
  out <- data.table::rbindlist(rows, fill = TRUE)
  out[variable %in% vars & is.finite(spend_multiplier)]
}

opsp_eval_draw_curve_variable <- function(draw_curves, draw_id, variable, multiplier, step_pct = 0.01, value_per_kpi = NA_real_) {
  tbl <- draw_curves[.draw == as.character(draw_id)]
  v <- as.character(variable)[1]
  m <- as.numeric(multiplier)[1]
  contrib <- opsp_interp_curve_value(tbl, v, m, "contribution")
  current_contrib <- opsp_interp_curve_value(tbl, v, 1, "contribution")
  up <- opsp_interp_curve_value(tbl, v, m + step_pct, "contribution")
  spend <- opsp_interp_curve_value(tbl, v, m, "spend")
  current_spend <- opsp_interp_curve_value(tbl, v, 1, "spend")
  up_spend <- opsp_interp_curve_value(tbl, v, m + step_pct, "spend")
  support <- opsp_interp_curve_value(tbl, v, m, "support")
  inc <- up - contrib
  inc_spend <- up_spend - spend
  spend_vs_current <- spend - current_spend
  incremental_contribution <- contrib - current_contrib
  profit <- ifelse(is.finite(value_per_kpi) && is.finite(contrib) && is.finite(spend),
                   contrib * value_per_kpi - spend, NA_real_)
  incremental_profit <- ifelse(is.finite(value_per_kpi) && is.finite(incremental_contribution) && is.finite(spend_vs_current),
                               incremental_contribution * value_per_kpi - spend_vs_current, NA_real_)
  data.table::data.table(
    .draw = as.character(draw_id),
    variable = v,
    spend_multiplier = m,
    spend = spend,
    spend_vs_current = spend_vs_current,
    support = support,
    contribution = contrib,
    contribution_vs_current = incremental_contribution,
    roi = opsp_safe_div(contrib, spend),
    incremental_roi = opsp_safe_div(incremental_contribution, spend_vs_current),
    mroi = opsp_safe_div(inc, inc_spend),
    cost_per_kpi = ifelse(is.finite(contrib) && contrib > 1e-8, spend / contrib, NA_real_),
    profit = profit,
    incremental_profit = incremental_profit,
    value_per_cost = ifelse(is.finite(value_per_kpi) && is.finite(spend) && abs(spend) > 1e-8,
                            contrib * value_per_kpi / spend, NA_real_)
  )
}

opsp_build_uncertainty_tables <- function(draw_curves,
                                          vars,
                                          scenario_multipliers = c(0.8, 1, 1.2),
                                          scenario_plan = NULL,
                                          optimization_plan = NULL,
                                          engine = NULL,
                                          step_pct = 0.01,
                                          value_per_kpi = NA_real_,
                                          uncertainty_quantile = 0.05) {
  uncertainty_quantile <- opsp_validate_quantile(uncertainty_quantile, fallback = 0.05)
  dc <- opsp_normalize_response_curve_draws(draw_curves)
  if (!nrow(dc)) {
    empty <- data.table::data.table()
    return(list(
      scenario_summary = empty,
      scenario_by_variable = empty,
      optimization_summary = empty,
      optimization_by_variable = empty
    ))
  }
  vars <- intersect(as.character(vars), unique(dc$variable))
  if (!length(vars)) {
    empty <- data.table::data.table()
    return(list(
      scenario_summary = empty,
      scenario_by_variable = empty,
      optimization_summary = empty,
      optimization_by_variable = empty
    ))
  }
  multiplier_sets <- opsp_make_multiplier_sets(
    vars = vars,
    scenario_multipliers = scenario_multipliers,
    scenario_plan = scenario_plan,
    optimization_plan = optimization_plan,
    engine = engine
  )
  if (!nrow(multiplier_sets)) {
    empty <- data.table::data.table()
    return(list(
      scenario_summary = empty,
      scenario_by_variable = empty,
      optimization_summary = empty,
      optimization_by_variable = empty
    ))
  }
  draw_ids <- unique(dc$.draw)
  raw <- data.table::rbindlist(lapply(draw_ids, function(dd) {
    data.table::rbindlist(lapply(seq_len(nrow(multiplier_sets)), function(i) {
      z <- opsp_eval_draw_curve_variable(
        dc,
        draw_id = dd,
        variable = multiplier_sets$variable[i],
        multiplier = multiplier_sets$spend_multiplier[i],
        step_pct = step_pct,
        value_per_kpi = value_per_kpi
      )
      z[, `:=`(
        scenario = multiplier_sets$scenario[i],
        scenario_type = multiplier_sets$scenario_type[i]
      )]
      z
    }), fill = TRUE)
  }), fill = TRUE)
  if (!nrow(raw)) {
    empty <- data.table::data.table()
    return(list(
      scenario_summary = empty,
      scenario_by_variable = empty,
      optimization_summary = empty,
      optimization_by_variable = empty
    ))
  }
  by_var <- raw[, .(
    draw_n = uniqueN(.draw),
    custom_quantile = uncertainty_quantile,
    spend_q05 = opsp_quantile(spend, 0.05),
    spend_q50 = opsp_quantile(spend, 0.50),
    spend_q95 = opsp_quantile(spend, 0.95),
    spend_q_custom = opsp_quantile(spend, uncertainty_quantile),
    contribution_q05 = opsp_quantile(contribution, 0.05),
    contribution_q50 = opsp_quantile(contribution, 0.50),
    contribution_q95 = opsp_quantile(contribution, 0.95),
    contribution_q_custom = opsp_quantile(contribution, uncertainty_quantile),
    contribution_vs_current_q05 = opsp_quantile(contribution_vs_current, 0.05),
    contribution_vs_current_q50 = opsp_quantile(contribution_vs_current, 0.50),
    contribution_vs_current_q95 = opsp_quantile(contribution_vs_current, 0.95),
    contribution_vs_current_q_custom = opsp_quantile(contribution_vs_current, uncertainty_quantile),
    incremental_contribution_q05 = opsp_quantile(contribution_vs_current, 0.05),
    q05_incremental_contribution = opsp_quantile(contribution_vs_current, 0.05),
    mean_incremental_contribution = opsp_mean_finite(contribution_vs_current),
    incremental_contribution_q_custom = opsp_quantile(contribution_vs_current, uncertainty_quantile),
    roi_q05 = opsp_quantile(roi, 0.05),
    roi_q50 = opsp_quantile(roi, 0.50),
    roi_q95 = opsp_quantile(roi, 0.95),
    roi_q_custom = opsp_quantile(roi, uncertainty_quantile),
    incremental_roi_q05 = opsp_quantile(incremental_roi, 0.05),
    q05_incremental_roi = opsp_quantile(incremental_roi, 0.05),
    incremental_roi_q_custom = opsp_quantile(incremental_roi, uncertainty_quantile),
    mroi_q05 = opsp_quantile(mroi, 0.05),
    mroi_q50 = opsp_quantile(mroi, 0.50),
    mroi_q95 = opsp_quantile(mroi, 0.95),
    mroi_q_custom = opsp_quantile(mroi, uncertainty_quantile),
    cost_per_kpi_q05 = opsp_quantile(cost_per_kpi, 0.05),
    cost_per_kpi_q50 = opsp_quantile(cost_per_kpi, 0.50),
    cost_per_kpi_q95 = opsp_quantile(cost_per_kpi, 0.95),
    cost_per_kpi_q_custom = opsp_quantile(cost_per_kpi, uncertainty_quantile),
    expected_profit = opsp_mean_finite(profit),
    profit_q05 = opsp_quantile(profit, 0.05),
    q05_profit = opsp_quantile(profit, 0.05),
    profit_q_custom = opsp_quantile(profit, uncertainty_quantile),
    expected_incremental_profit = opsp_mean_finite(incremental_profit),
    incremental_profit_q05 = opsp_quantile(incremental_profit, 0.05),
    incremental_profit_q_custom = opsp_quantile(incremental_profit, uncertainty_quantile),
    probability_profit_positive = opsp_probability_positive(profit),
    probability_incremental_contribution_positive = opsp_probability_positive(contribution_vs_current),
    value_per_cost_q05 = opsp_quantile(value_per_cost, 0.05),
    value_per_cost_q50 = opsp_quantile(value_per_cost, 0.50),
    value_per_cost_q95 = opsp_quantile(value_per_cost, 0.95),
    value_per_cost_q_custom = opsp_quantile(value_per_cost, uncertainty_quantile)
  ), by = .(scenario_type, scenario, variable, spend_multiplier)]
  draw_summary <- raw[, .(
    spend = sum(spend, na.rm = TRUE),
    spend_vs_current = sum(spend_vs_current, na.rm = TRUE),
    contribution = sum(contribution, na.rm = TRUE),
    contribution_vs_current = sum(contribution_vs_current, na.rm = TRUE),
    profit = sum(profit, na.rm = TRUE),
    incremental_profit = sum(incremental_profit, na.rm = TRUE)
  ), by = .(scenario_type, scenario, .draw)]
  draw_summary[, `:=`(
    roi = opsp_safe_div(contribution, spend),
    incremental_roi = opsp_safe_div(contribution_vs_current, spend_vs_current),
    cost_per_kpi = ifelse(is.finite(contribution) & contribution > 1e-8, spend / contribution, NA_real_),
    value_per_cost = ifelse(is.finite(value_per_kpi) & is.finite(spend) & abs(spend) > 1e-8,
                            contribution * value_per_kpi / spend, NA_real_)
  )]
  summary <- draw_summary[, .(
    draw_n = uniqueN(.draw),
    custom_quantile = uncertainty_quantile,
    spend_q05 = opsp_quantile(spend, 0.05),
    spend_q50 = opsp_quantile(spend, 0.50),
    spend_q95 = opsp_quantile(spend, 0.95),
    spend_q_custom = opsp_quantile(spend, uncertainty_quantile),
    contribution_q05 = opsp_quantile(contribution, 0.05),
    contribution_q50 = opsp_quantile(contribution, 0.50),
    contribution_q95 = opsp_quantile(contribution, 0.95),
    contribution_q_custom = opsp_quantile(contribution, uncertainty_quantile),
    contribution_vs_current_q05 = opsp_quantile(contribution_vs_current, 0.05),
    contribution_vs_current_q50 = opsp_quantile(contribution_vs_current, 0.50),
    contribution_vs_current_q95 = opsp_quantile(contribution_vs_current, 0.95),
    contribution_vs_current_q_custom = opsp_quantile(contribution_vs_current, uncertainty_quantile),
    incremental_contribution_q05 = opsp_quantile(contribution_vs_current, 0.05),
    q05_incremental_contribution = opsp_quantile(contribution_vs_current, 0.05),
    mean_incremental_contribution = opsp_mean_finite(contribution_vs_current),
    incremental_contribution_q_custom = opsp_quantile(contribution_vs_current, uncertainty_quantile),
    roi_q05 = opsp_quantile(roi, 0.05),
    roi_q50 = opsp_quantile(roi, 0.50),
    roi_q95 = opsp_quantile(roi, 0.95),
    roi_q_custom = opsp_quantile(roi, uncertainty_quantile),
    incremental_roi_q05 = opsp_quantile(incremental_roi, 0.05),
    q05_incremental_roi = opsp_quantile(incremental_roi, 0.05),
    incremental_roi_q_custom = opsp_quantile(incremental_roi, uncertainty_quantile),
    cost_per_kpi_q05 = opsp_quantile(cost_per_kpi, 0.05),
    cost_per_kpi_q50 = opsp_quantile(cost_per_kpi, 0.50),
    cost_per_kpi_q95 = opsp_quantile(cost_per_kpi, 0.95),
    cost_per_kpi_q_custom = opsp_quantile(cost_per_kpi, uncertainty_quantile),
    expected_profit = opsp_mean_finite(profit),
    profit_q05 = opsp_quantile(profit, 0.05),
    q05_profit = opsp_quantile(profit, 0.05),
    profit_q_custom = opsp_quantile(profit, uncertainty_quantile),
    expected_incremental_profit = opsp_mean_finite(incremental_profit),
    incremental_profit_q05 = opsp_quantile(incremental_profit, 0.05),
    incremental_profit_q_custom = opsp_quantile(incremental_profit, uncertainty_quantile),
    probability_profit_positive = opsp_probability_positive(profit),
    probability_incremental_contribution_positive = opsp_probability_positive(contribution_vs_current),
    value_per_cost_q05 = opsp_quantile(value_per_cost, 0.05),
    value_per_cost_q50 = opsp_quantile(value_per_cost, 0.50),
    value_per_cost_q95 = opsp_quantile(value_per_cost, 0.95),
    value_per_cost_q_custom = opsp_quantile(value_per_cost, uncertainty_quantile)
  ), by = .(scenario_type, scenario)]
  list(
    scenario_summary = summary[scenario_type == "scenario"][order(scenario)][],
    scenario_by_variable = by_var[scenario_type == "scenario"][order(scenario, variable)][],
    optimization_summary = summary[scenario_type == "optimized"][order(scenario)][],
    optimization_by_variable = by_var[scenario_type == "optimized"][order(scenario, variable)][],
    draw_count = uniqueN(raw$.draw)
  )
}

opsp_finalize_optimizer_plan <- function(engine,
                                         multipliers,
                                         cs,
                                         total_budget,
                                         min_required,
                                         max_possible,
                                         optimizer_basis,
                                         optimizer_iterations,
                                         allow_unallocated,
                                         allocation_history = NULL,
                                         step_pct = 0.01,
                                         value_per_kpi = NA_real_) {
  optimizer_basis_value <- as.character(optimizer_basis)[1]
  plan <- opsp_evaluate_scenario(engine, multipliers, "optimized",
                                 step_pct = step_pct, value_per_kpi = value_per_kpi)
  cur <- opsp_current_plan(engine, step_pct = step_pct, value_per_kpi = value_per_kpi)
  plan[cur[, .(variable, current_spend, current_support, current_contribution, current_roi, current_mroi)],
       on = "variable", `:=`(
         current_spend = i.current_spend,
         current_support = i.current_support,
         current_contribution = i.current_contribution,
         current_roi = i.current_roi,
         current_mroi = i.current_mroi
       )]
  plan[cs[, .(variable, min_multiplier, max_multiplier, locked)], on = "variable", `:=`(
    min_multiplier = i.min_multiplier,
    max_multiplier = i.max_multiplier,
    locked = i.locked
  )]
  data.table::setnames(plan, c("spend_multiplier", "spend", "support", "contribution", "roi", "mroi"),
                       c("recommended_multiplier", "recommended_spend", "recommended_support", "expected_contribution", "expected_roi", "expected_mroi"))
  plan[, `:=`(
    spend_change = recommended_spend - current_spend,
    spend_change_pct = opsp_safe_div(recommended_spend - current_spend, current_spend),
    contribution_change = expected_contribution - current_contribution,
    total_budget = total_budget,
    optimizer_basis = optimizer_basis_value
  )]
  summary <- plan[, .(
    total_budget = total_budget[1],
    current_spend = sum(current_spend, na.rm = TRUE),
    recommended_spend = sum(recommended_spend, na.rm = TRUE),
    current_support = if (any(is.finite(current_support))) sum(current_support, na.rm = TRUE) else NA_real_,
    recommended_support = if (any(is.finite(recommended_support))) sum(recommended_support, na.rm = TRUE) else NA_real_,
    unallocated_budget = pmax(total_budget[1] - sum(recommended_spend, na.rm = TRUE), 0),
    current_contribution = sum(current_contribution, na.rm = TRUE),
    expected_contribution = sum(expected_contribution, na.rm = TRUE),
    incremental_contribution = sum(contribution_change, na.rm = TRUE),
    current_roi = opsp_safe_div(sum(current_contribution, na.rm = TRUE), sum(current_spend, na.rm = TRUE)),
    expected_roi = opsp_safe_div(sum(expected_contribution, na.rm = TRUE), sum(recommended_spend, na.rm = TRUE)),
    current_cost_per_kpi = ifelse(sum(current_contribution, na.rm = TRUE) > 1e-8,
                                  sum(current_spend, na.rm = TRUE) / sum(current_contribution, na.rm = TRUE),
                                  NA_real_),
    expected_cost_per_kpi = ifelse(sum(expected_contribution, na.rm = TRUE) > 1e-8,
                                   sum(recommended_spend, na.rm = TRUE) / sum(expected_contribution, na.rm = TRUE),
                                   NA_real_),
    optimizer_iterations = optimizer_iterations,
    min_required_budget = min_required,
    max_possible_budget = max_possible,
    allow_unallocated = isTRUE(allow_unallocated),
    optimizer_basis = optimizer_basis_value
  )]
  hist <- if (is.null(allocation_history)) data.table::data.table() else data.table::as.data.table(allocation_history)
  list(
    plan = plan[order(-recommended_spend)][],
    summary = summary[],
    allocation_history = hist[]
  )
}

opsp_build_group_rollup <- function(engine,
                                    optimization_plan,
                                    variable_group_map = NULL,
                                    group_constraints = NULL,
                                    total_budget = NA_real_) {
  plan <- opsp_as_dt(optimization_plan, "optimization_plan")
  if (!nrow(plan) || !"variable" %in% names(plan)) return(data.table::data.table())
  vars <- unique(as.character(plan$variable))
  gm <- opsp_normalize_variable_group_map(vars, variable_group_map)
  plan[gm, planning_group := i.planning_group, on = "variable"]
  plan[!nzchar(planning_group) | is.na(planning_group), planning_group := variable]
  needed <- c("current_spend", "recommended_spend", "current_support", "recommended_support",
              "current_contribution", "expected_contribution", "contribution_change")
  for (cc in setdiff(needed, names(plan))) plan[, (cc) := NA_real_]
  rollup <- plan[, .(
    variable_count = data.table::uniqueN(variable),
    variables = paste(sort(unique(variable)), collapse = "|"),
    current_spend = sum(current_spend, na.rm = TRUE),
    recommended_spend = sum(recommended_spend, na.rm = TRUE),
    current_support = if (any(is.finite(current_support))) sum(current_support, na.rm = TRUE) else NA_real_,
    recommended_support = if (any(is.finite(recommended_support))) sum(recommended_support, na.rm = TRUE) else NA_real_,
    current_contribution = sum(current_contribution, na.rm = TRUE),
    expected_contribution = sum(expected_contribution, na.rm = TRUE),
    contribution_change = sum(contribution_change, na.rm = TRUE)
  ), by = planning_group]
  total_budget <- opsp_num(total_budget)[1]
  recommended_total <- rollup[, sum(recommended_spend, na.rm = TRUE)]
  current_total <- rollup[, sum(current_spend, na.rm = TRUE)]
  rollup[, `:=`(
    spend_change = recommended_spend - current_spend,
    spend_change_pct = opsp_safe_div(recommended_spend - current_spend, current_spend),
    current_share = opsp_safe_div(current_spend, current_total),
    recommended_share = opsp_safe_div(recommended_spend, recommended_total),
    target_budget_share = if (is.finite(total_budget) && total_budget > 0) recommended_spend / total_budget else NA_real_,
    current_roi = opsp_safe_div(current_contribution, current_spend),
    expected_roi = opsp_safe_div(expected_contribution, recommended_spend),
    current_cost_per_kpi = ifelse(current_contribution > 1e-8, current_spend / current_contribution, NA_real_),
    expected_cost_per_kpi = ifelse(expected_contribution > 1e-8, recommended_spend / expected_contribution, NA_real_)
  )]
  gc <- opsp_normalize_group_constraints(group_constraints, total_budget = total_budget)
  if (nrow(gc)) {
    rollup[gc, `:=`(
      min_spend = i.min_spend,
      max_spend = i.max_spend,
      fixed_spend = i.fixed_spend,
      min_share = i.min_share,
      max_share = i.max_share
    ), on = "planning_group"]
    missing_groups <- setdiff(gc$planning_group, rollup$planning_group)
    if (length(missing_groups)) {
      rollup <- data.table::rbindlist(list(
        rollup,
        gc[planning_group %in% missing_groups, .(
          planning_group,
          variable_count = 0L,
          variables = "",
          current_spend = 0,
          recommended_spend = 0,
          current_support = NA_real_,
          recommended_support = NA_real_,
          current_contribution = 0,
          expected_contribution = 0,
          contribution_change = 0,
          spend_change = 0,
          spend_change_pct = NA_real_,
          current_share = 0,
          recommended_share = 0,
          target_budget_share = 0,
          current_roi = NA_real_,
          expected_roi = NA_real_,
          current_cost_per_kpi = NA_real_,
          expected_cost_per_kpi = NA_real_,
          min_spend,
          max_spend,
          fixed_spend,
          min_share,
          max_share
        )]
      ), fill = TRUE)
    }
  }
  for (cc in c("min_spend", "max_spend", "fixed_spend", "min_share", "max_share")) {
    if (!cc %in% names(rollup)) rollup[, (cc) := NA_real_]
  }
  rollup[, `:=`(
    min_violation = data.table::fifelse(is.finite(min_spend), pmax(min_spend - recommended_spend, 0), 0),
    max_violation = data.table::fifelse(is.finite(max_spend), pmax(recommended_spend - max_spend, 0), 0)
  )]
  rollup[, `:=`(
    group_constraint_ok = min_violation <= 1e-6 & max_violation <= 1e-6,
    group_constraint_status = data.table::fifelse(min_violation > 1e-6, "below_minimum",
                                                  data.table::fifelse(max_violation > 1e-6, "above_maximum", "ok"))
  )]
  rollup[order(planning_group)][]
}

opsp_optimize_greedy <- function(engine,
                                 constraints = NULL,
                                 total_budget = NULL,
                                 budget_change_pct = 0,
                                 budget_step_frac = 0.005,
                                 min_multiplier = 0,
                                 max_multiplier = 3,
                                 max_iter = 5000,
                                 allow_unallocated = FALSE,
                                 step_pct = 0.01,
                                 value_per_kpi = NA_real_) {
  vars <- engine$variables
  cur_spend <- as.numeric(engine$current_spend[vars])
  cs <- opsp_normalize_constraints(vars, cur_spend, constraints = constraints,
                                   min_multiplier = min_multiplier, max_multiplier = max_multiplier)
  if (is.null(total_budget)) total_budget <- sum(cur_spend, na.rm = TRUE) * (1 + opsp_num(budget_change_pct)[1])
  total_budget <- opsp_num(total_budget)[1]
  if (!is.finite(total_budget) || total_budget < 0) stop("total_budget must be finite and non-negative.", call. = FALSE)
  min_required <- cs[, sum(current_spend * min_multiplier, na.rm = TRUE)]
  max_possible <- cs[, sum(current_spend * max_multiplier, na.rm = TRUE)]
  if (total_budget + 1e-8 < min_required) {
    stop("total_budget is below required minimum spend from constraints. Minimum required: ", round(min_required, 4), call. = FALSE)
  }

  multipliers <- stats::setNames(cs$min_multiplier, cs$variable)
  remaining <- min(total_budget, max_possible) - min_required
  chunk <- max(total_budget * budget_step_frac, total_budget / max_iter, 1e-8)
  history <- list()
  iter <- 0L
  while (remaining > 1e-8 && iter < max_iter) {
    iter <- iter + 1L
    cand <- lapply(seq_len(nrow(cs)), function(i) {
      v <- cs$variable[i]
      if (cs$current_spend[i] <= 1e-8) return(NULL)
      cur_m <- multipliers[v]
      if (cur_m >= cs$max_multiplier[i] - 1e-8) return(NULL)
      add_spend <- min(chunk, remaining, cs$current_spend[i] * (cs$max_multiplier[i] - cur_m))
      if (add_spend <= 1e-8) return(NULL)
      delta_m <- add_spend / cs$current_spend[i]
      base <- engine$contribution(v, cur_m)
      up <- engine$contribution(v, cur_m + delta_m)
      data.table::data.table(
        variable = v,
        add_spend = add_spend,
        delta_multiplier = delta_m,
        incremental_contribution = up - base,
        marginal_response = opsp_safe_div(up - base, add_spend)
      )
    })
    cand <- data.table::rbindlist(cand, fill = TRUE)
    if (!nrow(cand)) break
    cand <- cand[order(-marginal_response)]
    best <- cand[1]
    if (isTRUE(allow_unallocated) && (!is.finite(best$marginal_response) || best$marginal_response <= 0)) break
    multipliers[best$variable] <- multipliers[best$variable] + best$delta_multiplier
    remaining <- remaining - best$add_spend
    history[[iter]] <- best[, optimizer_iteration := iter]
  }
  opsp_finalize_optimizer_plan(
    engine = engine,
    multipliers = multipliers,
    cs = cs,
    total_budget = total_budget,
    min_required = min_required,
    max_possible = max_possible,
    optimizer_basis = paste0(engine$mode, "_greedy_marginal_point_estimate"),
    optimizer_iterations = iter,
    allow_unallocated = allow_unallocated,
    allocation_history = if (length(history)) data.table::rbindlist(history, fill = TRUE) else data.table::data.table(),
    step_pct = step_pct,
    value_per_kpi = value_per_kpi
  )
}

opsp_multiplier_grid_values <- function(lo, hi, step) {
  lo <- as.numeric(lo)[1]
  hi <- as.numeric(hi)[1]
  step <- as.numeric(step)[1]
  if (!is.finite(lo) || !is.finite(hi) || hi < lo) return(lo)
  if (!is.finite(step) || step <= 0) stop("optimization_grid_step must be positive.", call. = FALSE)
  vals <- seq(lo, hi, by = step)
  if (!length(vals) || abs(vals[length(vals)] - hi) > 1e-8) vals <- c(vals, hi)
  unique(round(vals, 10))
}

opsp_optimize_grid <- function(engine,
                               constraints = NULL,
                               variable_group_map = NULL,
                               group_constraints = NULL,
                               total_budget = NULL,
                               budget_change_pct = 0,
                               optimization_grid_step = 0.05,
                               max_grid_combinations = 250000L,
                               min_multiplier = 0,
                               max_multiplier = 3,
                               allow_unallocated = FALSE,
                               step_pct = 0.01,
                               value_per_kpi = NA_real_) {
  vars <- engine$variables
  cur_spend <- as.numeric(engine$current_spend[vars])
  cs <- opsp_normalize_constraints(vars, cur_spend, constraints = constraints,
                                   min_multiplier = min_multiplier, max_multiplier = max_multiplier)
  if (is.null(total_budget)) total_budget <- sum(cur_spend, na.rm = TRUE) * (1 + opsp_num(budget_change_pct)[1])
  total_budget <- opsp_num(total_budget)[1]
  if (!is.finite(total_budget) || total_budget < 0) stop("total_budget must be finite and non-negative.", call. = FALSE)
  min_required <- cs[, sum(vapply(seq_len(.N), function(i) opsp_spend_at(engine, variable[i], min_multiplier[i]), numeric(1)), na.rm = TRUE)]
  max_possible <- cs[, sum(vapply(seq_len(.N), function(i) opsp_spend_at(engine, variable[i], max_multiplier[i]), numeric(1)), na.rm = TRUE)]
  if (total_budget + 1e-8 < min_required) {
    stop("total_budget is below required minimum spend from constraints. Minimum required: ", round(min_required, 4), call. = FALSE)
  }

  grid_values <- stats::setNames(
    lapply(seq_len(nrow(cs)), function(i) opsp_multiplier_grid_values(cs$min_multiplier[i], cs$max_multiplier[i], optimization_grid_step)),
    cs$variable
  )
  combination_count <- prod(vapply(grid_values, length, integer(1)))
  if (!is.finite(combination_count) || combination_count > as.numeric(max_grid_combinations)[1]) {
    stop(
      "Grid optimizer would evaluate ", format(combination_count, scientific = FALSE),
      " combinations. Increase optimization_grid_step, reduce variables, tighten constraints, ",
      "or increase max_grid_combinations deliberately.",
      call. = FALSE
    )
  }
  grid <- data.table::as.data.table(do.call(expand.grid, c(grid_values, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))
  if (!nrow(grid)) stop("Grid optimizer has no candidate allocations.", call. = FALSE)
  spend_mat <- vapply(vars, function(v) vapply(grid[[v]], function(m) opsp_spend_at(engine, v, m), numeric(1)), numeric(nrow(grid)))
  contrib_mat <- vapply(vars, function(v) vapply(grid[[v]], function(m) engine$contribution(v, m), numeric(1)), numeric(nrow(grid)))
  grid[, candidate_spend := rowSums(spend_mat, na.rm = TRUE)]
  grid[, candidate_contribution := rowSums(contrib_mat, na.rm = TRUE)]
  feasible_budget <- min(total_budget, max_possible)
  feasible <- grid[candidate_spend <= feasible_budget + 1e-8 & candidate_spend + 1e-8 >= min_required]
  channel_filtered_count <- 0L
  if (nrow(feasible)) {
    channel_status <- data.table::rbindlist(lapply(seq_len(nrow(feasible)), function(i) {
      mult <- stats::setNames(as.numeric(feasible[i, ..vars]), vars)
      st <- opsp_channel_constraint_status(engine, vars, mult, cs)
      data.table::data.table(candidate_id = i, channel_constraint_ok = st$ok, channel_constraint_max_violation = st$max_violation)
    }), fill = TRUE)
    feasible[, candidate_id := seq_len(.N)]
    feasible[channel_status, `:=`(
      channel_constraint_ok = i.channel_constraint_ok,
      channel_constraint_max_violation = i.channel_constraint_max_violation
    ), on = "candidate_id"]
    channel_filtered_count <- feasible[is.na(channel_constraint_ok) | channel_constraint_ok != TRUE, .N]
    feasible <- feasible[channel_constraint_ok == TRUE]
  }
  gc <- opsp_normalize_group_constraints(group_constraints, total_budget = total_budget)
  group_filtered_count <- 0L
  if (nrow(gc) && nrow(feasible)) {
    group_status <- data.table::rbindlist(lapply(seq_len(nrow(feasible)), function(i) {
      mult <- stats::setNames(as.numeric(feasible[i, ..vars]), vars)
      st <- opsp_group_constraint_status(
        engine = engine,
        vars = vars,
        multipliers = mult,
        variable_group_map = variable_group_map,
        group_constraints = gc,
        total_budget = total_budget
      )
      data.table::data.table(candidate_id = i, group_constraint_ok = st$ok, group_constraint_max_violation = st$max_violation)
    }), fill = TRUE)
    feasible[, candidate_id := seq_len(.N)]
    feasible[group_status, `:=`(
      group_constraint_ok = i.group_constraint_ok,
      group_constraint_max_violation = i.group_constraint_max_violation
    ), on = "candidate_id"]
    group_filtered_count <- feasible[is.na(group_constraint_ok) | group_constraint_ok != TRUE, .N]
    feasible <- feasible[group_constraint_ok == TRUE]
  }
  if (!nrow(feasible)) {
    stop("No feasible grid allocation satisfies budget, channel constraints, and group constraints. Try a smaller optimization_grid_step or relax constraints.", call. = FALSE)
  }
  feasible[, budget_gap_abs := abs(candidate_spend - feasible_budget)]
  if (!isTRUE(allow_unallocated)) {
    best_spend <- feasible[, max(candidate_spend, na.rm = TRUE)]
    feasible <- feasible[candidate_spend >= best_spend - max(optimization_grid_step * max(cur_spend, na.rm = TRUE), 1e-8)]
  }
  data.table::setorder(feasible, -candidate_contribution, budget_gap_abs)
  best <- feasible[1]
  multipliers <- stats::setNames(as.numeric(best[, ..vars]), vars)
  hist <- data.table::data.table(
    optimizer_method = "grid",
    searched_combinations = as.numeric(combination_count),
    feasible_combinations = nrow(feasible),
    channel_constraint_filtered_combinations = channel_filtered_count,
    group_constraint_filtered_combinations = group_filtered_count,
    optimization_grid_step = as.numeric(optimization_grid_step)[1],
    selected_spend = best$candidate_spend[1],
    selected_contribution = best$candidate_contribution[1]
  )
  opsp_finalize_optimizer_plan(
    engine = engine,
    multipliers = multipliers,
    cs = cs,
    total_budget = total_budget,
    min_required = min_required,
    max_possible = max_possible,
    optimizer_basis = paste0(engine$mode, "_grid_search_point_estimate"),
    optimizer_iterations = as.integer(combination_count),
    allow_unallocated = allow_unallocated,
    allocation_history = hist,
    step_pct = step_pct,
    value_per_kpi = value_per_kpi
  )
}

opsp_draw_metrics_for_multipliers <- function(draw_curves,
                                              vars,
                                              multipliers,
                                              step_pct = 0.01,
                                              value_per_kpi = NA_real_,
                                              normalize = TRUE) {
  dc <- if (isTRUE(normalize)) opsp_normalize_response_curve_draws(draw_curves) else data.table::as.data.table(draw_curves)
  if (!nrow(dc)) return(data.table::data.table())
  vars <- intersect(as.character(vars), unique(dc$variable))
  if (!length(vars)) return(data.table::data.table())
  mult <- stats::setNames(rep(1, length(vars)), vars)
  if (!is.null(names(multipliers))) {
    hit <- intersect(names(multipliers), vars)
    mult[hit] <- opsp_num(multipliers[hit])
  } else if (length(multipliers) == length(vars)) {
    mult[] <- opsp_num(multipliers)
  }
  mult[!is.finite(mult) | mult < 0] <- 0
  raw <- data.table::rbindlist(lapply(unique(dc$.draw), function(dd) {
    data.table::rbindlist(lapply(vars, function(v) {
      opsp_eval_draw_curve_variable(
        dc,
        draw_id = dd,
        variable = v,
        multiplier = mult[v],
        step_pct = step_pct,
        value_per_kpi = value_per_kpi
      )
    }), fill = TRUE)
  }), fill = TRUE)
  if (!nrow(raw)) return(data.table::data.table())
  raw[, .(
    spend = sum(spend, na.rm = TRUE),
    spend_vs_current = sum(spend_vs_current, na.rm = TRUE),
    contribution = sum(contribution, na.rm = TRUE),
    contribution_vs_current = sum(contribution_vs_current, na.rm = TRUE),
    profit = sum(profit, na.rm = TRUE),
    incremental_profit = sum(incremental_profit, na.rm = TRUE)
  ), by = .draw][, `:=`(
    roi = opsp_safe_div(contribution, spend),
    incremental_roi = opsp_safe_div(contribution_vs_current, spend_vs_current),
    cost_per_kpi = ifelse(is.finite(contribution) & contribution > 1e-8, spend / contribution, NA_real_),
    value_per_cost = ifelse(is.finite(value_per_kpi) & is.finite(spend) & abs(spend) > 1e-8,
                            contribution * value_per_kpi / spend, NA_real_)
  )][]
}

opsp_robust_objective_score <- function(draw_metrics,
                                        robust_objective = "q05_contribution",
                                        robust_quantile = 0.05,
                                        robust_target_contribution = NULL,
                                        robust_target_cost_per_kpi = NULL,
                                        robust_target_roi = NULL,
                                        robust_risk_aversion = 0) {
  dm <- opsp_as_dt(draw_metrics, "draw_metrics")
  if (!nrow(dm)) return(list(score = NA_real_, summary = data.table::data.table()))
  robust_quantile <- opsp_validate_quantile(robust_quantile, fallback = 0.05)
  obj <- match.arg(
    as.character(robust_objective)[1],
    opsp_robust_objective_choices()
  )
  for (cc in c("spend_vs_current", "incremental_roi", "profit", "incremental_profit")) {
    if (!cc %in% names(dm)) dm[, (cc) := NA_real_]
  }
  contribution_mean <- mean(dm$contribution, na.rm = TRUE)
  incremental_contribution_mean <- opsp_mean_finite(dm$contribution_vs_current)
  contribution_sd <- stats::sd(dm$contribution[is.finite(dm$contribution)], na.rm = TRUE)
  if (!is.finite(contribution_sd)) contribution_sd <- 0
  cost_q95 <- opsp_quantile(dm$cost_per_kpi, 0.95)
  roi_q05 <- opsp_quantile(dm$roi, 0.05)
  incremental_roi_q <- opsp_quantile(dm$incremental_roi, robust_quantile)
  profit_mean <- opsp_mean_finite(dm$profit)
  profit_q <- opsp_quantile(dm$profit, robust_quantile)
  profit_probability <- opsp_probability_positive(dm$profit)
  incremental_contribution_probability <- opsp_probability_positive(dm$contribution_vs_current)
  target_contribution <- suppressWarnings(as.numeric(robust_target_contribution)[1])
  target_cost <- suppressWarnings(as.numeric(robust_target_cost_per_kpi)[1])
  target_roi <- suppressWarnings(as.numeric(robust_target_roi)[1])
  target_ok <- rep(TRUE, nrow(dm))
  if (is.finite(target_contribution)) target_ok <- target_ok & is.finite(dm$contribution) & dm$contribution >= target_contribution
  if (is.finite(target_cost)) target_ok <- target_ok & is.finite(dm$cost_per_kpi) & dm$cost_per_kpi <= target_cost
  if (is.finite(target_roi)) target_ok <- target_ok & is.finite(dm$roi) & dm$roi >= target_roi
  target_probability <- mean(target_ok, na.rm = TRUE)
  score <- switch(
    obj,
    q05_contribution = opsp_quantile(dm$contribution, robust_quantile),
    q50_contribution = opsp_quantile(dm$contribution, 0.50),
    mean_contribution = contribution_mean,
    q05_incremental_contribution = opsp_quantile(dm$contribution_vs_current, robust_quantile),
    quantile_incremental_contribution = opsp_quantile(dm$contribution_vs_current, robust_quantile),
    mean_incremental_contribution = incremental_contribution_mean,
    expected_utility = contribution_mean - pmax(suppressWarnings(as.numeric(robust_risk_aversion)[1]), 0) * contribution_sd,
    probability_target = {
      if (!is.finite(target_contribution) && !is.finite(target_cost) && !is.finite(target_roi)) {
        stop("robust_objective = 'probability_target' requires robust_target_contribution, robust_target_cost_per_kpi, or robust_target_roi.", call. = FALSE)
      }
      target_probability
    },
    q05_roi = roi_q05,
    q05_incremental_roi = incremental_roi_q,
    quantile_incremental_roi = incremental_roi_q,
    q95_cost_per_kpi = -cost_q95,
    expected_profit = profit_mean,
    q05_profit = profit_q,
    quantile_profit = profit_q,
    probability_profit_positive = profit_probability,
    probability_incremental_contribution_positive = incremental_contribution_probability
  )
  summary <- data.table::data.table(
    robust_objective = obj,
    robust_score = score,
    draw_n = uniqueN(dm$.draw),
    robust_quantile = robust_quantile,
    contribution_q05 = opsp_quantile(dm$contribution, 0.05),
    contribution_q50 = opsp_quantile(dm$contribution, 0.50),
    contribution_q95 = opsp_quantile(dm$contribution, 0.95),
    contribution_q_custom = opsp_quantile(dm$contribution, robust_quantile),
    contribution_mean = contribution_mean,
    incremental_contribution_q05 = opsp_quantile(dm$contribution_vs_current, 0.05),
    q05_incremental_contribution = opsp_quantile(dm$contribution_vs_current, 0.05),
    incremental_contribution_q_custom = opsp_quantile(dm$contribution_vs_current, robust_quantile),
    mean_incremental_contribution = incremental_contribution_mean,
    probability_incremental_contribution_positive = incremental_contribution_probability,
    contribution_sd = contribution_sd,
    roi_q05 = roi_q05,
    roi_q50 = opsp_quantile(dm$roi, 0.50),
    roi_q95 = opsp_quantile(dm$roi, 0.95),
    roi_q_custom = opsp_quantile(dm$roi, robust_quantile),
    incremental_roi_q05 = opsp_quantile(dm$incremental_roi, 0.05),
    q05_incremental_roi = opsp_quantile(dm$incremental_roi, 0.05),
    incremental_roi_q_custom = incremental_roi_q,
    cost_per_kpi_q05 = opsp_quantile(dm$cost_per_kpi, 0.05),
    cost_per_kpi_q50 = opsp_quantile(dm$cost_per_kpi, 0.50),
    cost_per_kpi_q95 = cost_q95,
    expected_profit = profit_mean,
    profit_q05 = opsp_quantile(dm$profit, 0.05),
    q05_profit = opsp_quantile(dm$profit, 0.05),
    profit_q_custom = profit_q,
    expected_incremental_profit = opsp_mean_finite(dm$incremental_profit),
    incremental_profit_q05 = opsp_quantile(dm$incremental_profit, 0.05),
    incremental_profit_q_custom = opsp_quantile(dm$incremental_profit, robust_quantile),
    probability_profit_positive = profit_probability,
    target_probability = target_probability
  )
  list(score = score, summary = summary)
}

opsp_optimize_robust_grid <- function(engine,
                                      draw_curves,
                                      constraints = NULL,
                                      variable_group_map = NULL,
                                      group_constraints = NULL,
                                      total_budget = NULL,
                                      budget_change_pct = 0,
                                      optimization_grid_step = 0.10,
                                      max_grid_combinations = 50000L,
                                      min_multiplier = 0,
                                      max_multiplier = 3,
                                      allow_unallocated = FALSE,
                                      robust_objective = opsp_robust_objective_choices(),
                                      robust_quantile = 0.05,
                                      robust_target_contribution = NULL,
                                      robust_target_cost_per_kpi = NULL,
                                      robust_target_roi = NULL,
                                      robust_risk_aversion = 0,
                                      step_pct = 0.01,
                                      value_per_kpi = NA_real_) {
  robust_objective <- match.arg(robust_objective)
  dc <- opsp_normalize_response_curve_draws(draw_curves)
  if (!nrow(dc)) {
    stop("optimizer_method = 'robust_grid' requires draw-level response curves. Pass response_curve_draws, fit$response_curves_draws, or use uncertainty = 'draws' with a Stan fit.", call. = FALSE)
  }
  vars <- intersect(engine$variables, unique(dc$variable))
  if (!length(vars)) stop("Draw-level response curves do not contain any optimizer variables.", call. = FALSE)
  cur_spend <- as.numeric(engine$current_spend[vars])
  cs <- opsp_normalize_constraints(vars, cur_spend, constraints = constraints,
                                   min_multiplier = min_multiplier, max_multiplier = max_multiplier)
  if (is.null(total_budget)) total_budget <- sum(cur_spend, na.rm = TRUE) * (1 + opsp_num(budget_change_pct)[1])
  total_budget <- opsp_num(total_budget)[1]
  if (!is.finite(total_budget) || total_budget < 0) stop("total_budget must be finite and non-negative.", call. = FALSE)
  min_required <- cs[, sum(vapply(seq_len(.N), function(i) opsp_spend_at(engine, variable[i], min_multiplier[i]), numeric(1)), na.rm = TRUE)]
  max_possible <- cs[, sum(vapply(seq_len(.N), function(i) opsp_spend_at(engine, variable[i], max_multiplier[i]), numeric(1)), na.rm = TRUE)]
  if (total_budget + 1e-8 < min_required) {
    stop("total_budget is below required minimum spend from constraints. Minimum required: ", round(min_required, 4), call. = FALSE)
  }
  grid_values <- stats::setNames(
    lapply(seq_len(nrow(cs)), function(i) opsp_multiplier_grid_values(cs$min_multiplier[i], cs$max_multiplier[i], optimization_grid_step)),
    cs$variable
  )
  combination_count <- prod(vapply(grid_values, length, integer(1)))
  if (!is.finite(combination_count) || combination_count > as.numeric(max_grid_combinations)[1]) {
    stop(
      "Robust grid optimizer would evaluate ", format(combination_count, scientific = FALSE),
      " combinations across posterior draws. Increase optimization_grid_step, tighten constraints, ",
      "or increase max_grid_combinations deliberately.",
      call. = FALSE
    )
  }
  grid <- data.table::as.data.table(do.call(expand.grid, c(grid_values, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))
  if (!nrow(grid)) stop("Robust grid optimizer has no candidate allocations.", call. = FALSE)
  spend_mat <- vapply(vars, function(v) vapply(grid[[v]], function(m) opsp_spend_at(engine, v, m), numeric(1)), numeric(nrow(grid)))
  grid[, candidate_spend := rowSums(spend_mat, na.rm = TRUE)]
  feasible_budget <- min(total_budget, max_possible)
  feasible <- grid[candidate_spend <= feasible_budget + 1e-8 & candidate_spend + 1e-8 >= min_required]
  channel_filtered_count <- 0L
  if (nrow(feasible)) {
    channel_status <- data.table::rbindlist(lapply(seq_len(nrow(feasible)), function(i) {
      mult <- stats::setNames(as.numeric(feasible[i, ..vars]), vars)
      st <- opsp_channel_constraint_status(engine, vars, mult, cs)
      data.table::data.table(candidate_id = i, channel_constraint_ok = st$ok, channel_constraint_max_violation = st$max_violation)
    }), fill = TRUE)
    feasible[, candidate_id := seq_len(.N)]
    feasible[channel_status, `:=`(
      channel_constraint_ok = i.channel_constraint_ok,
      channel_constraint_max_violation = i.channel_constraint_max_violation
    ), on = "candidate_id"]
    channel_filtered_count <- feasible[is.na(channel_constraint_ok) | channel_constraint_ok != TRUE, .N]
    feasible <- feasible[channel_constraint_ok == TRUE]
  }
  gc <- opsp_normalize_group_constraints(group_constraints, total_budget = total_budget)
  group_filtered_count <- 0L
  if (nrow(gc) && nrow(feasible)) {
    group_status <- data.table::rbindlist(lapply(seq_len(nrow(feasible)), function(i) {
      mult <- stats::setNames(as.numeric(feasible[i, ..vars]), vars)
      st <- opsp_group_constraint_status(
        engine = engine,
        vars = vars,
        multipliers = mult,
        variable_group_map = variable_group_map,
        group_constraints = gc,
        total_budget = total_budget
      )
      data.table::data.table(candidate_id = i, group_constraint_ok = st$ok, group_constraint_max_violation = st$max_violation)
    }), fill = TRUE)
    feasible[, candidate_id := seq_len(.N)]
    feasible[group_status, `:=`(
      group_constraint_ok = i.group_constraint_ok,
      group_constraint_max_violation = i.group_constraint_max_violation
    ), on = "candidate_id"]
    group_filtered_count <- feasible[is.na(group_constraint_ok) | group_constraint_ok != TRUE, .N]
    feasible <- feasible[group_constraint_ok == TRUE]
  }
  if (!nrow(feasible)) {
    stop("No feasible robust-grid allocation satisfies budget, channel constraints, and group constraints. Try a smaller optimization_grid_step or relax constraints.", call. = FALSE)
  }
  if (!isTRUE(allow_unallocated)) {
    best_spend <- feasible[, max(candidate_spend, na.rm = TRUE)]
    feasible <- feasible[candidate_spend >= best_spend - max(optimization_grid_step * max(cur_spend, na.rm = TRUE), 1e-8)]
  }
  scores <- data.table::rbindlist(lapply(seq_len(nrow(feasible)), function(i) {
    mult <- stats::setNames(as.numeric(feasible[i, ..vars]), vars)
    dm <- opsp_draw_metrics_for_multipliers(
      dc,
      vars = vars,
      multipliers = mult,
      step_pct = step_pct,
      value_per_kpi = value_per_kpi,
      normalize = FALSE
    )
    sc <- opsp_robust_objective_score(
      dm,
      robust_objective = robust_objective,
      robust_quantile = robust_quantile,
      robust_target_contribution = robust_target_contribution,
      robust_target_cost_per_kpi = robust_target_cost_per_kpi,
      robust_target_roi = robust_target_roi,
      robust_risk_aversion = robust_risk_aversion
    )$summary
    cbind(data.table::data.table(candidate_id = i, candidate_spend = feasible$candidate_spend[i]), sc)
  }), fill = TRUE)
  if (!nrow(scores) || all(!is.finite(scores$robust_score))) stop("No finite robust-grid objective score was produced.", call. = FALSE)
  scores[, budget_gap_abs := abs(candidate_spend - feasible_budget)]
  data.table::setorder(scores, -robust_score, budget_gap_abs)
  best_id <- scores$candidate_id[1]
  best <- feasible[best_id]
  multipliers <- stats::setNames(as.numeric(best[, ..vars]), vars)
  hist <- scores[1:min(.N, 100L)]
  hist[, `:=`(
    optimizer_method = "robust_grid",
    searched_combinations = as.numeric(combination_count),
    feasible_combinations = nrow(feasible),
    channel_constraint_filtered_combinations = channel_filtered_count,
    group_constraint_filtered_combinations = group_filtered_count,
    optimization_grid_step = as.numeric(optimization_grid_step)[1]
  )]
  opsp_finalize_optimizer_plan(
    engine = engine,
    multipliers = multipliers,
    cs = cs,
    total_budget = total_budget,
    min_required = min_required,
    max_possible = max_possible,
    optimizer_basis = paste0(engine$mode, "_robust_grid_", robust_objective),
    optimizer_iterations = as.integer(combination_count),
    allow_unallocated = allow_unallocated,
    allocation_history = hist,
    step_pct = step_pct,
    value_per_kpi = value_per_kpi
  )
}

opsp_total_spend_for_multipliers <- function(engine, vars, multipliers) {
  mult <- stats::setNames(as.numeric(multipliers[vars]), vars)
  sum(vapply(vars, function(v) opsp_spend_at(engine, v, mult[v]), numeric(1)), na.rm = TRUE)
}

opsp_point_score_for_multipliers <- function(engine, vars, multipliers) {
  mult <- stats::setNames(as.numeric(multipliers[vars]), vars)
  sum(vapply(vars, function(v) engine$contribution(v, mult[v]), numeric(1)), na.rm = TRUE)
}

opsp_refine_multipliers_continuous <- function(engine,
                                               start_multipliers,
                                               cs,
                                               total_budget,
                                               max_possible,
                                               variable_group_map = NULL,
                                               group_constraints = NULL,
                                               allow_unallocated = FALSE,
                                               draw_curves = NULL,
                                               robust_objective = NULL,
                                               robust_quantile = 0.05,
                                               robust_target_contribution = NULL,
                                               robust_target_cost_per_kpi = NULL,
                                               robust_target_roi = NULL,
                                               robust_risk_aversion = 0,
                                               step_pct = 0.01,
                                               value_per_kpi = NA_real_,
                                               max_iter = 120L,
                                               penalty_weight = 1000) {
  vars <- as.character(cs$variable)
  lower <- stats::setNames(as.numeric(cs$min_multiplier), vars)
  upper <- stats::setNames(as.numeric(cs$max_multiplier), vars)
  start <- stats::setNames(as.numeric(start_multipliers[vars]), vars)
  start[!is.finite(start)] <- lower[!is.finite(start)]
  start <- pmin(pmax(start, lower), upper)
  feasible_budget <- min(as.numeric(total_budget)[1], as.numeric(max_possible)[1])
  if (!is.finite(feasible_budget)) feasible_budget <- as.numeric(total_budget)[1]
  budget_scale <- max(abs(feasible_budget), 1)
  max_iter <- suppressWarnings(as.integer(max_iter)[1])
  if (!is.finite(max_iter) || max_iter <= 0L || all(abs(upper - lower) <= 1e-12)) {
    return(list(multipliers = start, history = data.table::data.table()))
  }
  penalty_weight <- suppressWarnings(as.numeric(penalty_weight)[1])
  if (!is.finite(penalty_weight) || penalty_weight <= 0) penalty_weight <- 1000
  dc <- if (!is.null(draw_curves)) opsp_normalize_response_curve_draws(draw_curves) else data.table::data.table()
  robust_mode <- !is.null(robust_objective) && nrow(dc)

  raw_score <- function(mult) {
    mult <- stats::setNames(as.numeric(mult[vars]), vars)
    if (isTRUE(robust_mode)) {
      dm <- opsp_draw_metrics_for_multipliers(
        dc,
        vars = vars,
        multipliers = mult,
        step_pct = step_pct,
        value_per_kpi = value_per_kpi,
        normalize = FALSE
      )
      opsp_robust_objective_score(
        dm,
        robust_objective = robust_objective,
        robust_quantile = robust_quantile,
        robust_target_contribution = robust_target_contribution,
        robust_target_cost_per_kpi = robust_target_cost_per_kpi,
        robust_target_roi = robust_target_roi,
        robust_risk_aversion = robust_risk_aversion
      )$score
    } else {
      opsp_point_score_for_multipliers(engine, vars, mult)
    }
  }

  score0 <- raw_score(start)
  score_scale <- max(abs(score0), 1)
  objective <- function(par) {
    mult <- stats::setNames(as.numeric(par), vars)
    score <- raw_score(mult)
    spend <- opsp_total_spend_for_multipliers(engine, vars, mult)
    overspend <- max(0, spend - feasible_budget)
    underspend <- if (isTRUE(allow_unallocated)) 0 else max(0, feasible_budget - spend)
    channel_status <- opsp_channel_constraint_status(engine, vars, mult, cs)
    group_status <- opsp_group_constraint_status(
      engine = engine,
      vars = vars,
      multipliers = mult,
      variable_group_map = variable_group_map,
      group_constraints = group_constraints,
      total_budget = total_budget
    )
    channel_violation <- if (is.finite(channel_status$max_violation)) pmax(channel_status$max_violation, 0) else 0
    group_violation <- if (is.finite(group_status$max_violation)) pmax(group_status$max_violation, 0) else 0
    penalty <- penalty_weight * score_scale * (
      (overspend / budget_scale) ^ 2 +
        (underspend / budget_scale) ^ 2 +
        (channel_violation / budget_scale) ^ 2 +
        (group_violation / budget_scale) ^ 2
    )
    if (!is.finite(score)) return(.Machine$double.xmax / 100)
    -(score - penalty)
  }

  fit <- tryCatch(
    stats::optim(
      par = as.numeric(start),
      fn = objective,
      method = "L-BFGS-B",
      lower = as.numeric(lower),
      upper = as.numeric(upper),
      control = list(maxit = max_iter)
    ),
    error = function(e) NULL
  )
  if (is.null(fit) || is.null(fit$par) || any(!is.finite(fit$par))) {
    return(list(multipliers = start, history = data.table::data.table(
      optimizer_phase = "continuous_refine",
      refine_status = "failed",
      objective_before = score0,
      objective_after = score0
    )))
  }
  candidate <- stats::setNames(pmin(pmax(as.numeric(fit$par), lower), upper), vars)
  score1 <- raw_score(candidate)
  spend0 <- opsp_total_spend_for_multipliers(engine, vars, start)
  spend1 <- opsp_total_spend_for_multipliers(engine, vars, candidate)
  channel_status0 <- opsp_channel_constraint_status(engine, vars, start, cs)
  channel_status1 <- opsp_channel_constraint_status(engine, vars, candidate, cs)
  group_status0 <- opsp_group_constraint_status(engine, vars, start, variable_group_map, group_constraints, total_budget)
  group_status1 <- opsp_group_constraint_status(engine, vars, candidate, variable_group_map, group_constraints, total_budget)
  accepted <- is.finite(score1) && score1 >= score0 - 1e-8 &&
    spend1 <= feasible_budget + max(1e-6, budget_scale * 1e-6) &&
    isTRUE(channel_status1$ok) &&
    isTRUE(group_status1$ok)
  if (!isTRUE(allow_unallocated)) {
    accepted <- accepted && spend1 >= feasible_budget - max(1e-4, budget_scale * 1e-4)
  }
  final <- if (isTRUE(accepted)) candidate else start
  hist <- data.table::data.table(
    optimizer_phase = "continuous_refine",
    refine_status = if (isTRUE(accepted)) "accepted" else "rejected",
    convergence = if (is.null(fit$convergence)) NA_integer_ else fit$convergence,
    objective_before = score0,
    objective_after = score1,
    spend_before = spend0,
    spend_after = spend1,
    channel_constraint_violation_before = channel_status0$max_violation,
    channel_constraint_violation_after = channel_status1$max_violation,
    group_constraint_violation_before = group_status0$max_violation,
    group_constraint_violation_after = group_status1$max_violation,
    feasible_budget = feasible_budget,
    optimizer_method = if (isTRUE(robust_mode)) "robust_hybrid" else "hybrid"
  )
  list(multipliers = final, history = hist)
}

opsp_optimize_hybrid <- function(engine,
                                 constraints = NULL,
                                 variable_group_map = NULL,
                                 group_constraints = NULL,
                                 total_budget = NULL,
                                 budget_change_pct = 0,
                                 optimization_grid_step = 0.10,
                                 max_grid_combinations = 250000L,
                                 min_multiplier = 0,
                                 max_multiplier = 3,
                                 allow_unallocated = FALSE,
                                 hybrid_refine_max_iter = 120L,
                                 hybrid_penalty_weight = 1000,
                                 step_pct = 0.01,
                                 value_per_kpi = NA_real_) {
  coarse <- opsp_optimize_grid(
    engine,
    constraints = constraints,
    variable_group_map = variable_group_map,
    group_constraints = group_constraints,
    total_budget = total_budget,
    budget_change_pct = budget_change_pct,
    optimization_grid_step = optimization_grid_step,
    max_grid_combinations = max_grid_combinations,
    min_multiplier = min_multiplier,
    max_multiplier = max_multiplier,
    allow_unallocated = allow_unallocated,
    step_pct = step_pct,
    value_per_kpi = value_per_kpi
  )
  vars <- engine$variables
  cur_spend <- as.numeric(engine$current_spend[vars])
  cs <- opsp_normalize_constraints(vars, cur_spend, constraints = constraints,
                                   min_multiplier = min_multiplier, max_multiplier = max_multiplier)
  if (is.null(total_budget)) total_budget <- sum(cur_spend, na.rm = TRUE) * (1 + opsp_num(budget_change_pct)[1])
  total_budget <- opsp_num(total_budget)[1]
  min_required <- cs[, sum(vapply(seq_len(.N), function(i) opsp_spend_at(engine, variable[i], min_multiplier[i]), numeric(1)), na.rm = TRUE)]
  max_possible <- cs[, sum(vapply(seq_len(.N), function(i) opsp_spend_at(engine, variable[i], max_multiplier[i]), numeric(1)), na.rm = TRUE)]
  start <- stats::setNames(coarse$plan$recommended_multiplier, coarse$plan$variable)
  refine <- opsp_refine_multipliers_continuous(
    engine = engine,
    start_multipliers = start,
    cs = cs,
    total_budget = total_budget,
    max_possible = max_possible,
    variable_group_map = variable_group_map,
    group_constraints = group_constraints,
    allow_unallocated = allow_unallocated,
    step_pct = step_pct,
    value_per_kpi = value_per_kpi,
    max_iter = hybrid_refine_max_iter,
    penalty_weight = hybrid_penalty_weight
  )
  hist <- data.table::rbindlist(list(
    data.table::copy(coarse$allocation_history)[, optimizer_phase := "coarse_grid"],
    refine$history
  ), fill = TRUE)
  opsp_finalize_optimizer_plan(
    engine = engine,
    multipliers = refine$multipliers,
    cs = cs,
    total_budget = total_budget,
    min_required = min_required,
    max_possible = max_possible,
    optimizer_basis = paste0(engine$mode, "_hybrid_grid_refine_point_estimate"),
    optimizer_iterations = coarse$summary$optimizer_iterations[1] + as.integer(hybrid_refine_max_iter),
    allow_unallocated = allow_unallocated,
    allocation_history = hist,
    step_pct = step_pct,
    value_per_kpi = value_per_kpi
  )
}

opsp_optimize_robust_hybrid <- function(engine,
                                        draw_curves,
                                        constraints = NULL,
                                        variable_group_map = NULL,
                                        group_constraints = NULL,
                                        total_budget = NULL,
                                        budget_change_pct = 0,
                                        optimization_grid_step = 0.10,
                                        max_grid_combinations = 50000L,
                                        min_multiplier = 0,
                                        max_multiplier = 3,
                                        allow_unallocated = FALSE,
                                        robust_objective = opsp_robust_objective_choices(),
                                        robust_quantile = 0.05,
                                        robust_target_contribution = NULL,
                                        robust_target_cost_per_kpi = NULL,
                                        robust_target_roi = NULL,
                                        robust_risk_aversion = 0,
                                        hybrid_refine_max_iter = 120L,
                                        hybrid_penalty_weight = 1000,
                                        step_pct = 0.01,
                                        value_per_kpi = NA_real_) {
  robust_objective <- match.arg(robust_objective)
  coarse <- opsp_optimize_robust_grid(
    engine,
    draw_curves = draw_curves,
    constraints = constraints,
    variable_group_map = variable_group_map,
    group_constraints = group_constraints,
    total_budget = total_budget,
    budget_change_pct = budget_change_pct,
    optimization_grid_step = optimization_grid_step,
    max_grid_combinations = max_grid_combinations,
    min_multiplier = min_multiplier,
    max_multiplier = max_multiplier,
    allow_unallocated = allow_unallocated,
    robust_objective = robust_objective,
    robust_quantile = robust_quantile,
    robust_target_contribution = robust_target_contribution,
    robust_target_cost_per_kpi = robust_target_cost_per_kpi,
    robust_target_roi = robust_target_roi,
    robust_risk_aversion = robust_risk_aversion,
    step_pct = step_pct,
    value_per_kpi = value_per_kpi
  )
  vars <- engine$variables
  cur_spend <- as.numeric(engine$current_spend[vars])
  cs <- opsp_normalize_constraints(vars, cur_spend, constraints = constraints,
                                   min_multiplier = min_multiplier, max_multiplier = max_multiplier)
  if (is.null(total_budget)) total_budget <- sum(cur_spend, na.rm = TRUE) * (1 + opsp_num(budget_change_pct)[1])
  total_budget <- opsp_num(total_budget)[1]
  min_required <- cs[, sum(vapply(seq_len(.N), function(i) opsp_spend_at(engine, variable[i], min_multiplier[i]), numeric(1)), na.rm = TRUE)]
  max_possible <- cs[, sum(vapply(seq_len(.N), function(i) opsp_spend_at(engine, variable[i], max_multiplier[i]), numeric(1)), na.rm = TRUE)]
  start <- stats::setNames(coarse$plan$recommended_multiplier, coarse$plan$variable)
  refine <- opsp_refine_multipliers_continuous(
    engine = engine,
    start_multipliers = start,
    cs = cs,
    total_budget = total_budget,
    max_possible = max_possible,
    variable_group_map = variable_group_map,
    group_constraints = group_constraints,
    allow_unallocated = allow_unallocated,
    draw_curves = draw_curves,
    robust_objective = robust_objective,
    robust_quantile = robust_quantile,
    robust_target_contribution = robust_target_contribution,
    robust_target_cost_per_kpi = robust_target_cost_per_kpi,
    robust_target_roi = robust_target_roi,
    robust_risk_aversion = robust_risk_aversion,
    step_pct = step_pct,
    value_per_kpi = value_per_kpi,
    max_iter = hybrid_refine_max_iter,
    penalty_weight = hybrid_penalty_weight
  )
  hist <- data.table::rbindlist(list(
    data.table::copy(coarse$allocation_history)[, optimizer_phase := "coarse_grid"],
    refine$history
  ), fill = TRUE)
  opsp_finalize_optimizer_plan(
    engine = engine,
    multipliers = refine$multipliers,
    cs = cs,
    total_budget = total_budget,
    min_required = min_required,
    max_possible = max_possible,
    optimizer_basis = paste0(engine$mode, "_robust_hybrid_", robust_objective),
    optimizer_iterations = coarse$summary$optimizer_iterations[1] + as.integer(hybrid_refine_max_iter),
    allow_unallocated = allow_unallocated,
    allocation_history = hist,
    step_pct = step_pct,
    value_per_kpi = value_per_kpi
  )
}

opsp_budget_bounds <- function(engine,
                               constraints = NULL,
                               min_multiplier = 0,
                               max_multiplier = 3) {
  vars <- engine$variables
  cur_spend <- as.numeric(engine$current_spend[vars])
  cs <- opsp_normalize_constraints(vars, cur_spend, constraints = constraints,
                                   min_multiplier = min_multiplier, max_multiplier = max_multiplier)
  data.table::data.table(
    current_budget = sum(cur_spend, na.rm = TRUE),
    min_required_budget = cs[, sum(current_spend * min_multiplier, na.rm = TRUE)],
    max_possible_budget = cs[, sum(current_spend * max_multiplier, na.rm = TRUE)]
  )
}

opsp_target_plan <- function(engine,
                             constraints = NULL,
                             target_contribution = NULL,
                             target_incremental_contribution = NULL,
                             target_cost_per_kpi = NULL,
                             target_roi = NULL,
                             budget_step_frac = 0.005,
                             min_multiplier = 0,
                             max_multiplier = 3,
                             max_iter = 5000,
                             search_iter = 28,
                             allow_unallocated = FALSE,
                             step_pct = 0.01,
                             value_per_kpi = NA_real_) {
  targets <- list()
  current_plan <- opsp_current_plan(engine, step_pct = step_pct, value_per_kpi = value_per_kpi)
  current_contribution <- current_plan[, sum(current_contribution, na.rm = TRUE)]
  bounds <- opsp_budget_bounds(engine, constraints = constraints,
                               min_multiplier = min_multiplier, max_multiplier = max_multiplier)
  low_bound <- bounds$min_required_budget[1]
  high_bound <- bounds$max_possible_budget[1]
  if (!is.finite(low_bound)) low_bound <- 0
  if (!is.finite(high_bound)) high_bound <- bounds$current_budget[1]
  if (!is.finite(high_bound) || high_bound < low_bound) high_bound <- low_bound

  optimize_at <- function(budget) {
    opsp_optimize_greedy(
      engine = engine,
      constraints = constraints,
      total_budget = budget,
      budget_step_frac = budget_step_frac,
      min_multiplier = min_multiplier,
      max_multiplier = max_multiplier,
      max_iter = max_iter,
      allow_unallocated = allow_unallocated,
      step_pct = step_pct,
      value_per_kpi = value_per_kpi
    )
  }

  add_summary <- function(opt, target_type, target_value, target_met, target_direction) {
    sm <- data.table::copy(opt$summary)
    sm[, `:=`(
      target_type = target_type,
      target_value = target_value,
      target_met = isTRUE(target_met),
      target_direction = target_direction
    )]
    pl <- data.table::copy(opt$plan)
    pl[, `:=`(
      target_type = target_type,
      target_value = target_value,
      target_met = isTRUE(target_met)
    )]
    list(summary = sm, plan = pl)
  }

  find_min_budget <- function(target_total, target_type, target_value) {
    high <- optimize_at(high_bound)
    high_hit <- high$summary$expected_contribution[1] >= target_total - 1e-8
    if (!isTRUE(high_hit)) {
      return(add_summary(high, target_type, target_value, FALSE, "minimum_budget_for_target_kpi"))
    }
    low <- low_bound
    hi <- high_bound
    for (ii in seq_len(search_iter)) {
      mid <- (low + hi) / 2
      opt <- optimize_at(mid)
      if (opt$summary$expected_contribution[1] >= target_total - 1e-8) hi <- mid else low <- mid
    }
    final <- optimize_at(hi)
    add_summary(final, target_type, target_value, TRUE, "minimum_budget_for_target_kpi")
  }

  find_max_efficient_budget <- function(metric, threshold, target_type) {
    feasible <- function(opt) {
      sm <- opt$summary
      if (identical(metric, "cost_per_kpi")) {
        is.finite(sm$expected_cost_per_kpi[1]) && sm$expected_cost_per_kpi[1] <= threshold + 1e-8
      } else {
        is.finite(sm$expected_roi[1]) && sm$expected_roi[1] >= threshold - 1e-8
      }
    }
    start_budget <- pmin(pmax(bounds$current_budget[1], low_bound), high_bound)
    low <- optimize_at(start_budget)
    if (!feasible(low)) {
      return(add_summary(low, target_type, threshold, FALSE, "maximum_budget_with_target_efficiency"))
    }
    high <- optimize_at(high_bound)
    if (feasible(high)) {
      return(add_summary(high, target_type, threshold, TRUE, "maximum_budget_with_target_efficiency"))
    }
    lo <- start_budget
    hi <- high_bound
    for (ii in seq_len(search_iter)) {
      mid <- (lo + hi) / 2
      opt <- optimize_at(mid)
      if (feasible(opt)) lo <- mid else hi <- mid
    }
    final <- optimize_at(lo)
    add_summary(final, target_type, threshold, TRUE, "maximum_budget_with_target_efficiency")
  }

  tc <- suppressWarnings(as.numeric(target_contribution)[1])
  if (is.finite(tc) && tc >= 0) {
    targets[[length(targets) + 1L]] <- find_min_budget(tc, "target_contribution", tc)
  }
  tic <- suppressWarnings(as.numeric(target_incremental_contribution)[1])
  if (is.finite(tic) && tic >= 0) {
    target_total <- current_contribution + tic
    targets[[length(targets) + 1L]] <- find_min_budget(target_total, "target_incremental_contribution", tic)
  }
  tcpk <- suppressWarnings(as.numeric(target_cost_per_kpi)[1])
  if (is.finite(tcpk) && tcpk > 0) {
    targets[[length(targets) + 1L]] <- find_max_efficient_budget("cost_per_kpi", tcpk, "target_cost_per_kpi")
  }
  troi <- suppressWarnings(as.numeric(target_roi)[1])
  if (is.finite(troi) && troi > 0) {
    targets[[length(targets) + 1L]] <- find_max_efficient_budget("roi", troi, "target_roi")
  }

  if (!length(targets)) {
    return(list(summary = data.table::data.table(), plan = data.table::data.table()))
  }
  list(
    summary = data.table::rbindlist(lapply(targets, `[[`, "summary"), fill = TRUE),
    plan = data.table::rbindlist(lapply(targets, `[[`, "plan"), fill = TRUE)
  )
}

opsp_build_diagnostics <- function(engine, current_plan, optimization, constraints = NULL) {
  flags <- data.table::data.table(flag = character(), severity = character(), detail = character())
  add_flag <- function(flag, severity, detail) {
    flags <<- data.table::rbindlist(list(flags, data.table::data.table(flag = flag, severity = severity, detail = as.character(detail))), fill = TRUE)
  }
  if (any(!is.finite(current_plan$current_spend) | current_plan$current_spend <= 0)) {
    add_flag("zero_or_missing_current_spend", "warning", paste(current_plan[!is.finite(current_spend) | current_spend <= 0, variable], collapse = ", "))
  }
  if (any(is.finite(current_plan$current_contribution) & current_plan$current_contribution <= 0)) {
    add_flag("non_positive_current_contribution", "warning", paste(current_plan[is.finite(current_contribution) & current_contribution <= 0, variable], collapse = ", "))
  }
  if (any(!is.finite(current_plan$current_mroi))) {
    add_flag("missing_current_mroi", "warning", paste(current_plan[!is.finite(current_mroi), variable], collapse = ", "))
  }
  if (!is.null(optimization$plan) && nrow(opsp_as_dt(optimization$plan)) &&
      any(is.finite(optimization$plan$expected_contribution) & optimization$plan$expected_contribution <= 0)) {
    add_flag("non_positive_expected_contribution", "warning", paste(optimization$plan[is.finite(expected_contribution) & expected_contribution <= 0, variable], collapse = ", "))
  }
  if (nrow(optimization$summary) && optimization$summary$unallocated_budget[1] > 1e-6) {
    add_flag("unallocated_budget", "info", optimization$summary$unallocated_budget[1])
  }
  if (identical(engine$mode, "response_curve_table")) {
    add_flag("response_curve_table_mode", "info", "Planner used supplied response curves; posterior/model uncertainty is not available.")
  }
  data.table::data.table(
    decisioning_basis = paste0(engine$mode, "_point_estimate"),
    uncertainty_note = "Scenario planning and optimization are point-estimate decision support. They do not yet optimize over posterior uncertainty.",
    flighting_assumption = "Channel spend/support changes scale the historical response curve for that channel while other channels are held at their current path.",
    cost_assumption = "Cost per media/support unit is treated as constant within each channel unless the supplied response curve already encodes a different cost assumption."
  )[, flags := list(flags)][]
}

opsp_write_outputs <- function(out, output_dir = NULL, output_prefix = "") {
  if (is.null(output_dir) || !nzchar(output_dir)) return(invisible(NULL))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  pfx <- if (nzchar(output_prefix)) paste0(output_prefix, "_") else ""
  write_one <- function(x, nm) {
    if (inherits(x, "data.table") || is.data.frame(x)) {
      data.table::fwrite(data.table::as.data.table(x), file.path(output_dir, paste0(pfx, nm, ".csv")))
    }
  }
  write_one(out$current_plan, "current_plan")
  write_one(out$response_curves, "response_curves")
  write_one(out$saturation_headroom, "saturation_headroom")
  write_one(out$scenario_summary, "scenario_summary")
  write_one(out$scenario_detail, "scenario_detail")
  write_one(out$scenario_uncertainty_summary, "scenario_uncertainty_summary")
  write_one(out$scenario_uncertainty_by_variable, "scenario_uncertainty_by_variable")
  write_one(out$optimization_summary, "optimization_summary")
  write_one(out$optimization_plan, "optimization_plan")
  write_one(out$optimization_group_rollup, "optimization_group_rollup")
  write_one(out$optimization_uncertainty_summary, "optimization_uncertainty_summary")
  write_one(out$optimization_uncertainty_by_variable, "optimization_uncertainty_by_variable")
  write_one(out$allocation_history, "allocation_history")
  write_one(out$target_plan_summary, "target_plan_summary")
  write_one(out$target_plan_detail, "target_plan_detail")
  write_one(out$uncertainty_diagnostics, "uncertainty_diagnostics")
  if (!is.null(out$diagnostics$flags)) write_one(out$diagnostics$flags, "diagnostic_flags")
  invisible(output_dir)
}

run_optimizer_scenario_planner <- function(fit_obj = NULL,
                                           response_curves = NULL,
                                           response_curve_draws = NULL,
                                           spend_map = NULL,
                                           raw_data = NULL,
                                           variables = NULL,
                                           constraints = NULL,
                                           variable_group_map = NULL,
                                           group_constraints = NULL,
                                           total_budget = NULL,
                                           budget_change_pct = 0,
                                           scenario_multipliers = c(0.8, 1, 1.2),
                                           scenario_plan = NULL,
                                           multiplier_grid = seq(0, 3, by = 0.05),
                                           step_pct = 0.01,
                                           budget_step_frac = 0.005,
                                           optimizer_method = c("greedy", "grid", "hybrid", "robust_grid", "robust_hybrid"),
                                           optimization_grid_step = 0.05,
                                           max_grid_combinations = 250000L,
                                           hybrid_refine_max_iter = 120L,
                                           hybrid_penalty_weight = 1000,
                                           uncertainty = c("auto", "none", "draws"),
                                           posterior_draw_count = 200L,
                                           posterior_draw_seed = 123L,
                                           robust_objective = opsp_robust_objective_choices(),
                                           robust_quantile = 0.05,
                                           uncertainty_quantile = NULL,
                                           robust_target_contribution = NULL,
                                           robust_target_cost_per_kpi = NULL,
                                           robust_target_roi = NULL,
                                           robust_risk_aversion = 0,
                                           min_multiplier = 0,
                                           max_multiplier = 3,
                                           max_iter = 5000,
                                           allow_unallocated = FALSE,
                                           target_contribution = NULL,
                                           target_incremental_contribution = NULL,
                                           target_cost_per_kpi = NULL,
                                           target_roi = NULL,
                                           target_search_iter = 28,
                                           value_per_kpi = NA_real_,
                                           kpi_label = "KPI",
                                           spend_suffix = "_spend",
                                           support_cost_map = NULL,
                                           output_dir = NULL,
                                           output_prefix = "") {
  opsp_require_data_table()
  optimizer_method <- match.arg(optimizer_method)
  uncertainty <- match.arg(uncertainty)
  robust_objective <- match.arg(robust_objective)
  robust_quantile <- opsp_validate_quantile(robust_quantile, fallback = 0.05)
  uncertainty_quantile <- if (is.null(uncertainty_quantile)) {
    robust_quantile
  } else {
    opsp_validate_quantile(uncertainty_quantile, fallback = robust_quantile)
  }
  group_constraints_norm <- opsp_normalize_group_constraints(group_constraints, total_budget = total_budget)
  has_group_constraints <- nrow(group_constraints_norm) > 0L
  if (isTRUE(has_group_constraints) && identical(optimizer_method, "greedy")) {
    stop("group_constraints require optimizer_method = 'grid', 'hybrid', 'robust_grid', or 'robust_hybrid'; greedy is a marginal-step heuristic and does not enforce portfolio/group caps.", call. = FALSE)
  }
  has_target_request <- !is.null(target_contribution) ||
    !is.null(target_incremental_contribution) ||
    !is.null(target_cost_per_kpi) ||
    !is.null(target_roi)
  if (isTRUE(has_group_constraints) && isTRUE(has_target_request)) {
    stop("Target-search planning with group_constraints is not yet supported. Run budget optimization with grid/hybrid group constraints, or omit group_constraints for target-search mode.", call. = FALSE)
  }
  engine <- opsp_engine_from_inputs(
    fit_obj = fit_obj,
    response_curves = response_curves,
    spend_map = spend_map,
    raw_data = raw_data,
    variables = variables,
    constraints = constraints,
    support_cost_map = support_cost_map,
    spend_suffix = spend_suffix
  )
  current <- opsp_current_plan(engine, step_pct = step_pct, value_per_kpi = value_per_kpi)
  curves <- opsp_build_response_curves(engine, multiplier_grid = multiplier_grid,
                                       step_pct = step_pct, value_per_kpi = value_per_kpi)
  saturation_headroom <- opsp_build_saturation_headroom(curves)
  scenarios <- opsp_scenario_tables(engine, scenario_multipliers = scenario_multipliers,
                                    scenario_plan = scenario_plan, step_pct = step_pct,
                                    value_per_kpi = value_per_kpi)
  uncertainty_for_draws <- if (optimizer_method %in% c("robust_grid", "robust_hybrid") && identical(uncertainty, "auto")) "draws" else uncertainty
  draw_curves <- opsp_response_curve_draws_from_inputs(
    fit_obj = fit_obj,
    response_curve_draws = response_curve_draws,
    response_curves = response_curves,
    spend_map = spend_map,
    raw_data = raw_data,
    variables = engine$variables,
    multiplier_grid = multiplier_grid,
    step_pct = step_pct,
    spend_suffix = spend_suffix,
    support_cost_map = support_cost_map,
    uncertainty = uncertainty_for_draws,
    posterior_draw_count = posterior_draw_count,
    posterior_draw_seed = posterior_draw_seed
  )
  opt <- if (identical(optimizer_method, "grid")) {
    opsp_optimize_grid(
      engine,
      constraints = constraints,
      variable_group_map = variable_group_map,
      group_constraints = group_constraints_norm,
      total_budget = total_budget,
      budget_change_pct = budget_change_pct,
      optimization_grid_step = optimization_grid_step,
      max_grid_combinations = max_grid_combinations,
      min_multiplier = min_multiplier,
      max_multiplier = max_multiplier,
      allow_unallocated = allow_unallocated,
      step_pct = step_pct,
      value_per_kpi = value_per_kpi
    )
  } else if (identical(optimizer_method, "hybrid")) {
    opsp_optimize_hybrid(
      engine,
      constraints = constraints,
      variable_group_map = variable_group_map,
      group_constraints = group_constraints_norm,
      total_budget = total_budget,
      budget_change_pct = budget_change_pct,
      optimization_grid_step = optimization_grid_step,
      max_grid_combinations = max_grid_combinations,
      min_multiplier = min_multiplier,
      max_multiplier = max_multiplier,
      allow_unallocated = allow_unallocated,
      hybrid_refine_max_iter = hybrid_refine_max_iter,
      hybrid_penalty_weight = hybrid_penalty_weight,
      step_pct = step_pct,
      value_per_kpi = value_per_kpi
    )
  } else if (identical(optimizer_method, "robust_grid")) {
    opsp_optimize_robust_grid(
      engine,
      draw_curves = draw_curves,
      constraints = constraints,
      variable_group_map = variable_group_map,
      group_constraints = group_constraints_norm,
      total_budget = total_budget,
      budget_change_pct = budget_change_pct,
      optimization_grid_step = optimization_grid_step,
      max_grid_combinations = max_grid_combinations,
      min_multiplier = min_multiplier,
      max_multiplier = max_multiplier,
      allow_unallocated = allow_unallocated,
      robust_objective = robust_objective,
      robust_quantile = robust_quantile,
      robust_target_contribution = robust_target_contribution,
      robust_target_cost_per_kpi = robust_target_cost_per_kpi,
      robust_target_roi = robust_target_roi,
      robust_risk_aversion = robust_risk_aversion,
      step_pct = step_pct,
      value_per_kpi = value_per_kpi
    )
  } else if (identical(optimizer_method, "robust_hybrid")) {
    opsp_optimize_robust_hybrid(
      engine,
      draw_curves = draw_curves,
      constraints = constraints,
      variable_group_map = variable_group_map,
      group_constraints = group_constraints_norm,
      total_budget = total_budget,
      budget_change_pct = budget_change_pct,
      optimization_grid_step = optimization_grid_step,
      max_grid_combinations = max_grid_combinations,
      min_multiplier = min_multiplier,
      max_multiplier = max_multiplier,
      allow_unallocated = allow_unallocated,
      robust_objective = robust_objective,
      robust_quantile = robust_quantile,
      robust_target_contribution = robust_target_contribution,
      robust_target_cost_per_kpi = robust_target_cost_per_kpi,
      robust_target_roi = robust_target_roi,
      robust_risk_aversion = robust_risk_aversion,
      hybrid_refine_max_iter = hybrid_refine_max_iter,
      hybrid_penalty_weight = hybrid_penalty_weight,
      step_pct = step_pct,
      value_per_kpi = value_per_kpi
    )
  } else {
    opsp_optimize_greedy(
      engine,
      constraints = constraints,
      total_budget = total_budget,
      budget_change_pct = budget_change_pct,
      budget_step_frac = budget_step_frac,
      min_multiplier = min_multiplier,
      max_multiplier = max_multiplier,
      max_iter = max_iter,
      allow_unallocated = allow_unallocated,
      step_pct = step_pct,
      value_per_kpi = value_per_kpi
    )
  }
  target_plan <- opsp_target_plan(
    engine,
    constraints = constraints,
    target_contribution = target_contribution,
    target_incremental_contribution = target_incremental_contribution,
    target_cost_per_kpi = target_cost_per_kpi,
    target_roi = target_roi,
    budget_step_frac = budget_step_frac,
    min_multiplier = min_multiplier,
    max_multiplier = max_multiplier,
    max_iter = max_iter,
    search_iter = target_search_iter,
    allow_unallocated = allow_unallocated,
    step_pct = step_pct,
    value_per_kpi = value_per_kpi
  )
  optimization_group_rollup <- opsp_build_group_rollup(
    engine = engine,
    optimization_plan = opt$plan,
    variable_group_map = variable_group_map,
    group_constraints = group_constraints_norm,
    total_budget = opt$summary$total_budget[1]
  )
  uncertainty_tables <- opsp_build_uncertainty_tables(
    draw_curves = draw_curves,
    vars = engine$variables,
    scenario_multipliers = scenario_multipliers,
    scenario_plan = scenario_plan,
    optimization_plan = opt$plan,
    engine = engine,
    step_pct = step_pct,
    value_per_kpi = value_per_kpi,
    uncertainty_quantile = uncertainty_quantile
  )
  diagnostics <- opsp_build_diagnostics(engine, current, opt, constraints = constraints)
  uncertainty_diagnostics <- data.table::data.table(
    uncertainty_mode = uncertainty_for_draws,
    response_curve_draw_rows = nrow(draw_curves),
    posterior_draw_count_used = if (is.null(uncertainty_tables$draw_count)) 0L else uncertainty_tables$draw_count,
    uncertainty_note = if (nrow(draw_curves)) {
      if (optimizer_method %in% c("robust_grid", "robust_hybrid")) {
        paste0("Scenario uncertainty summaries are computed from draw-level response curves. The optimized plan was selected by ", optimizer_method, " using a posterior/draw objective.")
      } else {
        "Scenario uncertainty summaries are computed from draw-level response curves. Optimization selected the point-estimate plan."
      }
    } else {
      "No draw-level response curves were available, so uncertainty-aware scenario summaries were not created."
    }
  )
  out <- list(
    inputs_used = data.table::data.table(
      engine_mode = engine$mode,
      variable_count = length(engine$variables),
      kpi_label = kpi_label,
      value_per_kpi = suppressWarnings(as.numeric(value_per_kpi)[1])
    ),
    settings = data.table::data.table(
      total_budget = if (is.null(total_budget)) NA_real_ else suppressWarnings(as.numeric(total_budget)[1]),
      budget_change_pct = suppressWarnings(as.numeric(budget_change_pct)[1]),
      step_pct = step_pct,
      budget_step_frac = budget_step_frac,
      optimizer_method = optimizer_method,
      optimization_grid_step = optimization_grid_step,
      max_grid_combinations = max_grid_combinations,
      hybrid_refine_max_iter = hybrid_refine_max_iter,
      hybrid_penalty_weight = hybrid_penalty_weight,
      uncertainty = uncertainty,
      posterior_draw_count = posterior_draw_count,
      robust_objective = robust_objective,
      robust_quantile = robust_quantile,
      uncertainty_quantile = uncertainty_quantile,
      robust_risk_aversion = robust_risk_aversion,
      min_multiplier = min_multiplier,
      max_multiplier = max_multiplier,
      allow_unallocated = isTRUE(allow_unallocated),
      group_constraint_count = nrow(group_constraints_norm)
    ),
    current_plan = current[],
    response_curves = curves[],
    saturation_headroom = saturation_headroom[],
    scenario_summary = scenarios$summary[],
    scenario_detail = scenarios$detail[],
    scenario_uncertainty_summary = uncertainty_tables$scenario_summary[],
    scenario_uncertainty_by_variable = uncertainty_tables$scenario_by_variable[],
    optimization_summary = opt$summary[],
    optimization_plan = opt$plan[],
    optimization_group_rollup = optimization_group_rollup[],
    optimization_uncertainty_summary = uncertainty_tables$optimization_summary[],
    optimization_uncertainty_by_variable = uncertainty_tables$optimization_by_variable[],
    allocation_history = opt$allocation_history[],
    target_plan_summary = target_plan$summary[],
    target_plan_detail = target_plan$plan[],
    uncertainty_diagnostics = uncertainty_diagnostics[],
    diagnostics = diagnostics
  )
  opsp_write_outputs(out, output_dir = output_dir, output_prefix = output_prefix)
  out
}

optimizer_scenario_planner <- run_optimizer_scenario_planner
run_mmm_optimizer <- run_optimizer_scenario_planner
run_mmm_scenario_planner <- run_optimizer_scenario_planner
