# bau_response_curves.R
#
# Analyst-facing BAU response-curve builder.
#
# This is a deterministic curve-construction utility for planning and sanity
# checks before, beside, or after a full MMM fit. It intentionally separates
# curve shape from business scale: support/spend/population can define a
# conservative curve shape, while contribution/ROI/cost-per-KPI inputs are
# needed before the output is optimizer-ready.

bau_require_data_table <- function() {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("bau_response_curves.R requires the data.table package.", call. = FALSE)
  }
  invisible(TRUE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

bau_as_dt <- function(x, label = "input") {
  bau_require_data_table()
  if (is.null(x)) return(data.table::data.table())
  data.table::as.data.table(data.table::copy(x))
}

bau_num <- function(x) suppressWarnings(as.numeric(x))

bau_pick_col <- function(dt, candidates) {
  hit <- intersect(candidates, names(dt))
  if (length(hit)) hit[1] else NA_character_
}

bau_blank <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

bau_clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

bau_safe_div <- function(num, den) {
  num <- bau_num(num)
  den <- bau_num(den)
  out <- rep(NA_real_, max(length(num), length(den)))
  num <- rep(num, length.out = length(out))
  den <- rep(den, length.out = length(out))
  ok <- is.finite(num) & is.finite(den) & abs(den) > 1e-10
  out[ok] <- num[ok] / den[ok]
  out
}

bau_normalize_curve_type <- function(x) {
  out <- tolower(trimws(as.character(x %||% "hill")))
  out[is.na(out) | !nzchar(out)] <- "hill"
  out[out %in% c("hill_function", "hill_curve", "logistic_saturation")] <- "hill"
  out[out %in% c("weibull_cdf", "exponential", "exp")] <- "weibull"
  bad <- setdiff(unique(out), c("hill", "weibull"))
  if (length(bad)) {
    stop("Unsupported curve_type values: ", paste(bad, collapse = ", "), ". Use 'hill' or 'weibull'.", call. = FALSE)
  }
  out
}

bau_saturation <- function(x, curve_rate, shape = 1, curve_type = "hill") {
  x <- pmax(bau_num(x), 0)
  x[!is.finite(x)] <- 0
  curve_rate <- bau_num(curve_rate)[1]
  shape <- bau_num(shape)[1]
  if (!is.finite(curve_rate) || curve_rate <= 0) curve_rate <- 1
  if (!is.finite(shape) || shape <= 0) shape <- 1
  z <- pmax(x * curve_rate, 1e-12) ^ shape
  curve_type <- bau_normalize_curve_type(curve_type)[1]
  if (identical(curve_type, "hill")) {
    z / (1 + z)
  } else {
    1 - exp(-z)
  }
}

bau_normalize_curve_normalization_scope <- function(x) {
  out <- tolower(trimws(as.character(x %||% "active_train")[1]))
  if (is.na(out) || !nzchar(out)) out <- "active_train"
  out <- gsub("-", "_", out)
  switch(out,
    active = "active_train",
    active_only = "active_train",
    nonzero = "active_train",
    non_zero = "active_train",
    nonzero_train = "active_train",
    active_train = "active_train",
    all = "all_train",
    train = "all_train",
    all_train = "all_train",
    stop("curve_normalization_scope must be 'active_train' or 'all_train'.", call. = FALSE)
  )
}

bau_curve_norm_mask <- function(x_raw, train_mask, scope = "active_train") {
  scope <- bau_normalize_curve_normalization_scope(scope)
  train_mask <- as.logical(train_mask)
  train_mask[is.na(train_mask)] <- FALSE
  if (!any(train_mask)) train_mask <- rep(TRUE, length(x_raw))
  if (identical(scope, "all_train")) return(train_mask)
  active <- train_mask & is.finite(x_raw) & x_raw > 0
  if (any(active)) active else train_mask
}

bau_rate_from_anchor <- function(anchor_x, anchor_saturation = 0.50, shape = 1, curve_type = "hill") {
  anchor_x <- bau_num(anchor_x)[1]
  anchor_saturation <- bau_clip(bau_num(anchor_saturation)[1], 0.01, 0.99)
  shape <- bau_num(shape)[1]
  if (!is.finite(anchor_x) || anchor_x <= 0 || !is.finite(anchor_saturation)) return(NA_real_)
  if (!is.finite(shape) || shape <= 0) shape <- 1
  curve_type <- bau_normalize_curve_type(curve_type)[1]
  if (identical(curve_type, "hill")) {
    (anchor_saturation / (1 - anchor_saturation)) ^ (1 / shape) / anchor_x
  } else {
    (-log(1 - anchor_saturation)) ^ (1 / shape) / anchor_x
  }
}

bau_adstock <- function(x, decay = 0) {
  x <- pmax(bau_num(x), 0)
  x[!is.finite(x)] <- 0
  decay <- bau_clip(bau_num(decay)[1], 0, 0.999)
  out <- numeric(length(x))
  carry <- 0
  for (ii in seq_along(x)) {
    carry <- x[ii] + decay * carry
    out[ii] <- carry
  }
  out
}

bau_first_finite <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- bau_num(vals)
  vals <- vals[is.finite(vals)]
  if (length(vals)) vals[1] else NA_real_
}

bau_apply_anchor_overrides <- function(vm, channel_curve_anchors = NULL) {
  if (is.null(channel_curve_anchors)) return(vm)
  if (!inherits(channel_curve_anchors, "data.frame") && !inherits(channel_curve_anchors, "data.table") &&
      !is.null(names(channel_curve_anchors))) {
    channel_curve_anchors <- data.table::data.table(
      variable = names(channel_curve_anchors),
      anchor_saturation = bau_num(unlist(channel_curve_anchors, use.names = FALSE))
    )
  }
  anchors <- bau_as_dt(channel_curve_anchors, "channel_curve_anchors")
  if (!nrow(anchors)) return(vm)
  if (!"variable" %in% names(anchors)) {
    vcol <- bau_pick_col(anchors, c("channel", "media", "driver"))
    if (is.na(vcol)) stop("channel_curve_anchors must include variable/channel/media/driver.", call. = FALSE)
    data.table::setnames(anchors, vcol, "variable")
  }
  acol <- bau_pick_col(anchors, c("anchor_saturation", "median_anchor_saturation", "saturation_at_median", "curve_anchor"))
  if (is.na(acol)) stop("channel_curve_anchors must include anchor_saturation or median_anchor_saturation.", call. = FALSE)
  anchors <- anchors[, .(variable = as.character(variable), anchor_override = bau_num(get(acol)))]
  anchors <- anchors[nzchar(variable) & is.finite(anchor_override)]
  dup <- anchors[duplicated(variable), unique(variable)]
  if (length(dup)) stop("channel_curve_anchors has duplicate variable rows: ", paste(dup, collapse = ", "), call. = FALSE)
  vm[anchors, anchor_saturation := bau_clip(i.anchor_override, 0.01, 0.99), on = "variable"]
  vm[]
}

bau_prepare_rrate_series <- function(dt,
                                     support_col,
                                     dep_var_col,
                                     date_col = NULL,
                                     train_col = "__bau_train__") {
  x <- data.table::copy(dt)
  x[, `:=`(
    support_raw__ = pmax(bau_num(get(support_col)), 0),
    dep_var_raw__ = bau_num(get(dep_var_col)),
    train__ = as.logical(get(train_col))
  )]
  x[is.na(train__), train__ := FALSE]
  if (!is.null(date_col) && nzchar(date_col) && date_col %in% names(x)) {
    out <- x[, .(
      support_raw__ = sum(support_raw__, na.rm = TRUE),
      dep_var_raw__ = if (all(is.na(dep_var_raw__))) NA_real_ else sum(dep_var_raw__, na.rm = TRUE),
      train__ = any(train__, na.rm = TRUE)
    ), by = date_col]
    data.table::setorderv(out, date_col)
  } else {
    out <- x[, .(support_raw__, dep_var_raw__, train__)]
  }
  out[!is.finite(support_raw__) | is.na(support_raw__), support_raw__ := 0]
  out[]
}

bau_fit_rrate_candidate <- function(y,
                                    x_raw,
                                    train_mask,
                                    rrate,
                                    anchor_saturation,
                                    shape,
                                    curve_type,
                                    curve_normalization_scope = "active_train",
                                    positive_effect = TRUE,
                                    wrong_sign_penalty = 25) {
  train_mask <- as.logical(train_mask)
  train_mask[is.na(train_mask)] <- FALSE
  x_raw <- pmax(bau_num(x_raw), 0)
  y <- bau_num(y)
  carry <- bau_adstock(x_raw, decay = rrate)
  norm_mask <- bau_curve_norm_mask(x_raw, train_mask, curve_normalization_scope)
  carry_scale <- mean(carry[norm_mask], na.rm = TRUE)
  if (!is.finite(carry_scale) || carry_scale <= 1e-10) carry_scale <- 1
  z <- carry / carry_scale
  active <- train_mask & is.finite(z) & z > 0 & is.finite(x_raw) & x_raw > 0
  anchor_x <- stats::median(z[active], na.rm = TRUE)
  rate <- bau_rate_from_anchor(anchor_x, anchor_saturation = anchor_saturation, shape = shape, curve_type = curve_type)
  if (!is.finite(rate) || rate <= 0) {
    return(list(score = Inf, sse = Inf, coef = NA_real_, active_weeks = sum(active), rate = NA_real_))
  }
  sat <- bau_saturation(z, curve_rate = rate, shape = shape, curve_type = curve_type)
  cur_mean <- mean(sat[norm_mask], na.rm = TRUE)
  if (!is.finite(cur_mean) || cur_mean <= 1e-10) cur_mean <- 1
  feature <- sat / cur_mean
  ok <- train_mask & is.finite(y) & is.finite(feature)
  if (sum(ok) < 6L || stats::sd(feature[ok]) <= 1e-10 || stats::sd(y[ok]) <= 1e-10) {
    return(list(score = Inf, sse = Inf, coef = NA_real_, active_weeks = sum(active), rate = rate))
  }
  trend <- seq(-1, 1, length.out = length(feature))
  design <- cbind(1, feature, trend)
  fit <- tryCatch(stats::lm.fit(design[ok, , drop = FALSE], y[ok]), error = function(e) NULL)
  if (is.null(fit) || any(!is.finite(fit$coefficients))) {
    return(list(score = Inf, sse = Inf, coef = NA_real_, active_weeks = sum(active), rate = rate))
  }
  resid <- y[ok] - as.numeric(design[ok, , drop = FALSE] %*% fit$coefficients)
  sse <- sum(resid^2, na.rm = TRUE)
  score <- sse
  coef <- unname(fit$coefficients[2])
  if (isTRUE(positive_effect) && is.finite(coef) && coef < 0) score <- score * wrong_sign_penalty
  list(score = score, sse = sse, coef = coef, active_weeks = sum(active), rate = rate)
}

bau_estimate_rrate_one <- function(dt,
                                   variable,
                                   support_col,
                                   dep_var_col,
                                   date_col = NULL,
                                   train_col = "__bau_train__",
                                   default_rrate = 0,
                                   rrate_grid = seq(0, 0.80, by = 0.05),
                                   rrate_bounds = c(0, 0.90),
                                   rrate_min_active_weeks = 8L,
                                   rrate_min_improvement = 0.01,
                                   rrate_complexity_penalty = 0.002,
                                   rrate_plateau_rel_tol = 0.01,
                                   rrate_plateau_abs_tol = 1e-6,
                                   anchor_saturation = 0.50,
                                   shape = 1,
                                   curve_type = "hill",
                                   curve_normalization_scope = "active_train") {
  lo <- bau_num(rrate_bounds[1])
  hi <- bau_num(rrate_bounds[2])
  if (!is.finite(lo)) lo <- 0
  if (!is.finite(hi)) hi <- 0.90
  lo <- bau_clip(lo, 0, 0.999)
  hi <- bau_clip(hi, lo + 1e-4, 0.999)
  default_rrate <- bau_clip(bau_num(default_rrate)[1], lo, hi)
  grid <- sort(unique(bau_clip(bau_num(c(rrate_grid, lo, hi, 0, default_rrate)), lo, hi)))
  grid <- grid[is.finite(grid)]
  if (!length(grid)) grid <- default_rrate
  ser <- bau_prepare_rrate_series(
    dt,
    support_col = support_col,
    dep_var_col = dep_var_col,
    date_col = date_col,
    train_col = train_col
  )
  if (!nrow(ser)) {
    return(data.table::data.table(
      variable = variable,
      rrate_selected = default_rrate,
      rrate_raw_best = NA_real_,
      rrate_source = "default_no_data",
      rrate_estimation_status = "not_estimated_no_data",
      rrate_active_weeks = 0L,
      rrate_improvement = NA_real_,
      rrate_score_selected = NA_real_,
      rrate_score_default = NA_real_,
      rrate_at_bound = FALSE,
      rrate_plateau_adjusted = FALSE,
      rrate_coef_sign = NA_real_
    ))
  }
  train_mask <- as.logical(ser$train__)
  if (!any(train_mask)) train_mask <- rep(TRUE, nrow(ser))
  active_weeks <- sum(train_mask & is.finite(ser$support_raw__) & ser$support_raw__ > 0, na.rm = TRUE)
  if (active_weeks < as.integer(rrate_min_active_weeks)) {
    return(data.table::data.table(
      variable = variable,
      rrate_selected = default_rrate,
      rrate_raw_best = NA_real_,
      rrate_source = "default_insufficient_active_weeks",
      rrate_estimation_status = "not_estimated_insufficient_active_weeks",
      rrate_active_weeks = active_weeks,
      rrate_improvement = NA_real_,
      rrate_score_selected = NA_real_,
      rrate_score_default = NA_real_,
      rrate_at_bound = FALSE,
      rrate_plateau_adjusted = FALSE,
      rrate_coef_sign = NA_real_
    ))
  }
  fits <- lapply(grid, function(rr) {
    z <- bau_fit_rrate_candidate(
      y = ser$dep_var_raw__,
      x_raw = ser$support_raw__,
      train_mask = train_mask,
      rrate = rr,
      anchor_saturation = anchor_saturation,
      shape = shape,
      curve_type = curve_type,
      curve_normalization_scope = curve_normalization_scope
    )
    data.table::data.table(
      rrate = rr,
      raw_score = z$score,
      raw_sse = z$sse,
      coef = z$coef,
      active_weeks = z$active_weeks
    )
  })
  fit_dt <- data.table::rbindlist(fits, fill = TRUE)
  fit_dt[is.finite(raw_score), score := raw_score * (1 + rrate_complexity_penalty * rrate^2)]
  finite_fit <- fit_dt[is.finite(score)]
  if (!nrow(finite_fit)) {
    return(data.table::data.table(
      variable = variable,
      rrate_selected = default_rrate,
      rrate_raw_best = NA_real_,
      rrate_source = "default_all_candidates_failed",
      rrate_estimation_status = "not_estimated_all_candidates_failed",
      rrate_active_weeks = active_weeks,
      rrate_improvement = NA_real_,
      rrate_score_selected = NA_real_,
      rrate_score_default = NA_real_,
      rrate_at_bound = FALSE,
      rrate_plateau_adjusted = FALSE,
      rrate_coef_sign = NA_real_
    ))
  }
  data.table::setorder(finite_fit, score, rrate)
  best <- finite_fit[1]
  default_fit <- finite_fit[which.min(abs(rrate - default_rrate))]
  default_score <- default_fit$score[1]
  improvement <- if (is.finite(default_score) && default_score > 0) {
    pmax(0, (default_score - best$score[1]) / default_score)
  } else {
    NA_real_
  }
  selected <- best$rrate[1]
  selected_score <- best$score[1]
  plateau_adjusted <- FALSE
  if (is.finite(selected_score)) {
    plateau_limit <- selected_score * (1 + rrate_plateau_rel_tol) + rrate_plateau_abs_tol
    plateau <- finite_fit[score <= plateau_limit]
    if (nrow(plateau) && min(plateau$rrate, na.rm = TRUE) < selected - 1e-12) {
      selected <- min(plateau$rrate, na.rm = TRUE)
      selected_score <- plateau[which.min(rrate), score][1]
      plateau_adjusted <- TRUE
    }
  }
  if (!is.finite(improvement) || improvement < rrate_min_improvement) {
    selected <- default_rrate
    selected_score <- default_score
    source <- "default_weak_incremental_fit"
    status <- "not_estimated_weak_incremental_fit"
    plateau_adjusted <- FALSE
  } else {
    source <- "estimated_pooled_univariate_diagnostic"
    status <- "estimated_pooled_univariate_diagnostic"
  }
  selected_fit <- finite_fit[which.min(abs(rrate - selected))]
  data.table::data.table(
    variable = variable,
    rrate_selected = selected,
    rrate_raw_best = best$rrate[1],
    rrate_source = source,
    rrate_estimation_status = status,
    rrate_active_weeks = active_weeks,
    rrate_improvement = improvement,
    rrate_score_selected = selected_score,
    rrate_score_default = default_score,
    rrate_at_bound = abs(best$rrate[1] - lo) < 1e-10 || abs(best$rrate[1] - hi) < 1e-10,
    rrate_plateau_adjusted = plateau_adjusted,
    rrate_coef_sign = sign(selected_fit$coef[1])
  )
}

bau_normalize_variable_map <- function(data,
                                       variable_map = NULL,
                                       variables = NULL,
                                       support_suffix = "_support",
                                       spend_suffix = "_spend",
                                       curve_type = "hill",
                                       anchor_saturation = 0.50,
                                       shape = 1,
                                       adstock_decay = 0,
                                       channel_curve_anchors = NULL) {
  dt <- bau_as_dt(data, "data")
  if (is.null(variable_map)) {
    vars <- unique(as.character(variables %||% character()))
    if (!length(vars)) {
      support_hits <- grep(paste0(support_suffix, "$"), names(dt), value = TRUE)
      spend_hits <- grep(paste0(spend_suffix, "$"), names(dt), value = TRUE)
      vars <- unique(c(
        sub(paste0(support_suffix, "$"), "", support_hits),
        sub(paste0(spend_suffix, "$"), "", spend_hits)
      ))
      vars <- vars[nzchar(vars)]
    }
    if (!length(vars)) stop("Pass variable_map or variables; no support/spend variables could be inferred.", call. = FALSE)
    vm <- data.table::data.table(variable = vars)
    vm[, support_col := data.table::fifelse(variable %in% names(dt), variable, paste0(variable, support_suffix))]
    vm[!(support_col %in% names(dt)) & paste0(variable, spend_suffix) %in% names(dt), support_col := paste0(variable, spend_suffix)]
    vm[, spend_col := paste0(variable, spend_suffix)]
    vm[!(spend_col %in% names(dt)), spend_col := NA_character_]
  } else {
    vm <- bau_as_dt(variable_map, "variable_map")
    if (!"variable" %in% names(vm)) {
      vcol <- bau_pick_col(vm, c("channel", "media", "driver", "support_col", "modeled_x_col"))
      if (is.na(vcol)) stop("variable_map must include variable, channel, media, driver, support_col, or modeled_x_col.", call. = FALSE)
      vm[, variable := as.character(get(vcol))]
    }
    scol <- bau_pick_col(vm, c("support_col", "modeled_x_col", "media_col", "x_col"))
    if (is.na(scol)) {
      vm[, support_col := as.character(variable)]
    } else if (!identical(scol, "support_col")) {
      vm[, support_col := as.character(get(scol))]
    }
    if (!"support_col" %in% names(vm)) vm[, support_col := as.character(variable)]
    if (!"spend_col" %in% names(vm)) vm[, spend_col := NA_character_]
    vm[, `:=`(
      variable = as.character(variable),
      support_col = as.character(support_col),
      spend_col = as.character(spend_col)
    )]
    vm[bau_blank(support_col) & variable %in% names(dt), support_col := variable]
    vm[bau_blank(support_col) & paste0(variable, support_suffix) %in% names(dt), support_col := paste0(variable, support_suffix)]
    vm[bau_blank(spend_col) & paste0(variable, spend_suffix) %in% names(dt), spend_col := paste0(variable, spend_suffix)]
    vm[bau_blank(spend_col) & grepl("spend", support_col, ignore.case = TRUE), spend_col := support_col]
    vm[bau_blank(spend_col), spend_col := NA_character_]
  }

  vm <- vm[nzchar(as.character(variable))]
  dup <- vm[duplicated(variable), unique(variable)]
  if (length(dup)) stop("variable_map contains duplicate variable rows: ", paste(dup, collapse = ", "), call. = FALSE)

  missing_support <- vm[bau_blank(support_col) | !(support_col %in% names(dt)), .(variable, support_col)]
  if (nrow(missing_support)) {
    stop(
      "Could not recover support/modeled columns for: ",
      paste(paste0(missing_support$variable, " -> ", missing_support$support_col), collapse = "; "),
      call. = FALSE
    )
  }

  if (!"curve_type" %in% names(vm)) vm[, curve_type := curve_type]
  if (!"anchor_saturation" %in% names(vm)) {
    acol <- bau_pick_col(vm, c("median_anchor_saturation", "saturation_at_median", "curve_anchor"))
    if (!is.na(acol)) vm[, anchor_saturation := bau_num(get(acol))] else vm[, anchor_saturation := anchor_saturation]
  }
  if (!"shape" %in% names(vm)) {
    shcol <- bau_pick_col(vm, c("dvalue", "curve_shape", "hill_shape", "weibull_shape"))
    if (!is.na(shcol)) vm[, shape := bau_num(get(shcol))] else vm[, shape := shape]
  }
  supplied_adstock__ <- "adstock_decay" %in% names(vm)
  if (!"adstock_decay" %in% names(vm)) {
    rcol <- bau_pick_col(vm, c("rrate", "decay", "adstock_rate"))
    if (!is.na(rcol)) {
      vm[, adstock_decay := bau_num(get(rcol))]
      supplied_adstock__ <- TRUE
    } else {
      vm[, adstock_decay := adstock_decay]
    }
  }
  vm[, adstock_decay_supplied := is.finite(bau_num(adstock_decay)) & isTRUE(supplied_adstock__)]
  if (!"adstock_decay_source" %in% names(vm)) {
    vm[, adstock_decay_source := data.table::fifelse(adstock_decay_supplied, "supplied", "default")]
  }

  for (cc in c("current_contribution", "current_roi", "roi_like", "cost_per_kpi", "current_cost_per_kpi", "value_per_kpi")) {
    if (!cc %in% names(vm)) vm[, (cc) := NA_real_]
  }
  vm[, `:=`(
    curve_type = bau_normalize_curve_type(curve_type),
    anchor_saturation = bau_clip(bau_num(anchor_saturation), 0.01, 0.99),
    shape = pmax(bau_num(shape), 0.05),
    adstock_decay = bau_clip(bau_num(adstock_decay), 0, 0.999),
    current_contribution = bau_num(current_contribution),
    current_roi = bau_num(current_roi),
    roi_like = bau_num(roi_like),
    current_cost_per_kpi = bau_num(current_cost_per_kpi),
    cost_per_kpi = bau_num(cost_per_kpi),
    value_per_kpi = bau_num(value_per_kpi)
  )]
  vm[!is.finite(current_roi) & is.finite(roi_like), current_roi := roi_like]
  vm[!is.finite(current_cost_per_kpi) & is.finite(cost_per_kpi), current_cost_per_kpi := cost_per_kpi]
  vm[!is.finite(anchor_saturation), anchor_saturation := 0.50]
  vm[!is.finite(shape), shape := 1]
  vm[!is.finite(adstock_decay), `:=`(adstock_decay = 0, adstock_decay_supplied = FALSE, adstock_decay_source = "default")]
  bau_apply_anchor_overrides(vm, channel_curve_anchors = channel_curve_anchors)
}

bau_train_mask <- function(dt, train_col = NULL, holdout_col = NULL) {
  n <- nrow(dt)
  mask <- rep(TRUE, n)
  if (!is.null(train_col) && nzchar(train_col) && train_col %in% names(dt)) {
    mask <- as.logical(dt[[train_col]])
    mask[is.na(mask)] <- FALSE
  }
  if (!is.null(holdout_col) && nzchar(holdout_col) && holdout_col %in% names(dt)) {
    hold <- as.logical(dt[[holdout_col]])
    hold[is.na(hold)] <- FALSE
    mask <- mask & !hold
  }
  if (!any(mask)) mask <- rep(TRUE, n)
  mask
}

bau_prepare_scope_series <- function(dt,
                                     support_col,
                                     spend_col,
                                     population_col = NULL,
                                     date_col = NULL,
                                     group_col = NULL,
                                     group_value = "ALL",
                                     scope = "total",
                                     train_col = "__bau_train__",
                                     support_basis = "raw") {
  x <- data.table::copy(dt)
  if (!is.null(group_col) && nzchar(group_col) && group_col %in% names(x) && !identical(scope, "total")) {
    x <- x[as.character(get(group_col)) == as.character(group_value)]
  }
  if (!nrow(x)) return(data.table::data.table())
  x[, `:=`(
    support_raw__ = pmax(bau_num(get(support_col)), 0),
    spend_raw__ = if (!is.na(spend_col) && nzchar(spend_col) && spend_col %in% names(x)) pmax(bau_num(get(spend_col)), 0) else NA_real_,
    train__ = as.logical(get(train_col))
  )]
  x[is.na(train__), train__ := FALSE]
  if (!is.null(population_col) && nzchar(population_col) && population_col %in% names(x)) {
    x[, population_raw__ := bau_num(get(population_col))]
    x[!is.finite(population_raw__) | population_raw__ <= 0, population_raw__ := NA_real_]
  } else {
    x[, population_raw__ := NA_real_]
  }

  if (!is.null(date_col) && nzchar(date_col) && date_col %in% names(x)) {
    ser <- x[, .(
      support_raw__ = sum(support_raw__, na.rm = TRUE),
      spend_raw__ = if (all(is.na(spend_raw__))) NA_real_ else sum(spend_raw__, na.rm = TRUE),
      population_raw__ = if (all(is.na(population_raw__))) NA_real_ else sum(unique(stats::na.omit(population_raw__)), na.rm = TRUE),
      train__ = any(train__, na.rm = TRUE)
    ), by = date_col]
    data.table::setorderv(ser, date_col)
  } else {
    ser <- x[, .(support_raw__, spend_raw__, population_raw__, train__)]
  }
  if (!nrow(ser)) return(ser)

  use_pop <- identical(support_basis, "per_population_index") && any(is.finite(ser$population_raw__) & ser$population_raw__ > 0)
  if (use_pop) {
    pop_scale <- stats::median(ser$population_raw__[ser$train__ & is.finite(ser$population_raw__) & ser$population_raw__ > 0], na.rm = TRUE)
    if (!is.finite(pop_scale) || pop_scale <= 0) pop_scale <- stats::median(ser$population_raw__[is.finite(ser$population_raw__) & ser$population_raw__ > 0], na.rm = TRUE)
    if (!is.finite(pop_scale) || pop_scale <= 0) {
      use_pop <- FALSE
      pop_scale <- NA_real_
    }
  } else {
    pop_scale <- NA_real_
  }
  ser[, `:=`(
    curve_x__ = if (isTRUE(use_pop)) support_raw__ / population_raw__ * pop_scale else support_raw__,
    support_basis_used__ = if (isTRUE(use_pop)) "per_population_index" else "raw",
    population_scale__ = pop_scale
  )]
  ser[!is.finite(curve_x__) | is.na(curve_x__), curve_x__ := 0]
  ser[]
}

#' Create BAU response curves from observed support/spend.
#'
#' @param data Raw model-cut or planning data.
#' @param variable_map Optional table with `variable`, `support_col` or
#'   `modeled_x_col`, optional `spend_col`, `curve_type`, `anchor_saturation`,
#'   `shape`/`dvalue`, `adstock_decay`/`rrate`, and optional business-scale
#'   fields (`current_contribution`, `current_roi`, `cost_per_kpi`).
#' @param dep_var_col Optional dependent-variable/KPI column used only for
#'   guarded pooled rrate diagnostics. If omitted, BAU never estimates rrate.
#'   If supplied, BAU estimates one shared rrate per variable across the data
#'   cut unless a per-variable rrate was supplied and `rrate_overwrite_supplied`
#'   is `FALSE`.
#' @param channel_curve_anchors Optional named vector/list or table with
#'   `variable` and `anchor_saturation` overrides. This is the median non-zero
#'   support saturation anchor, for example 0.50 by default or 0.30 for a
#'   channel you believe is less saturated at median support.
#' @return A list with `response_curves`, `curve_metadata`,
#'   `rrate_diagnostics`, and `settings`.
create_bau_response_curves <- function(data,
                                       variable_map = NULL,
                                       variables = NULL,
                                       group_col = NULL,
                                       date_col = NULL,
                                       dep_var_col = NULL,
                                       population_col = NULL,
                                       support_basis = c("auto", "raw", "per_population_index"),
                                       curve_scope = c("auto", "total", "group", "both"),
                                       curve_type = "hill",
                                       anchor_saturation = 0.50,
                                       channel_curve_anchors = NULL,
                                       shape = 1,
                                       adstock_decay = 0,
                                       estimate_rrate = NULL,
                                       rrate_grid = seq(0, 0.80, by = 0.05),
                                       rrate_bounds = c(0, 0.90),
                                       rrate_min_active_weeks = 8L,
                                       rrate_min_improvement = 0.01,
                                       rrate_complexity_penalty = 0.002,
                                       rrate_plateau_rel_tol = 0.01,
                                       rrate_plateau_abs_tol = 1e-6,
                                       rrate_overwrite_supplied = FALSE,
                                       normalize_curve_x = TRUE,
                                       curve_normalization_scope = c("active_train", "all_train"),
                                       multiplier_grid = seq(0, 3, by = 0.05),
                                       step_pct = 0.01,
                                       train_col = NULL,
                                       holdout_col = NULL,
                                       support_suffix = "_support",
                                       spend_suffix = "_spend",
                                       value_per_kpi = NA_real_) {
  bau_require_data_table()
  dt <- bau_as_dt(data, "data")
  if (!nrow(dt)) stop("data must contain at least one row.", call. = FALSE)
  support_basis <- match.arg(support_basis)
  curve_scope <- match.arg(curve_scope)
  curve_normalization_scope <- bau_normalize_curve_normalization_scope(match.arg(curve_normalization_scope))
  if (is.null(population_col) || !nzchar(as.character(population_col)[1])) {
    population_col <- bau_pick_col(dt, c("population", "pop", "households", "hh", "market_size", "market_population"))
  }
  if (is.na(population_col) || !(population_col %in% names(dt))) population_col <- NULL
  if (is.null(dep_var_col) || !nzchar(as.character(dep_var_col)[1])) {
    dep_var_col <- NULL
  } else {
    dep_var_col <- as.character(dep_var_col)[1]
    if (!(dep_var_col %in% names(dt))) stop("dep_var_col not found in data: ", dep_var_col, call. = FALSE)
  }
  if (is.null(estimate_rrate)) estimate_rrate <- !is.null(dep_var_col)
  estimate_rrate <- isTRUE(estimate_rrate)

  vm <- bau_normalize_variable_map(
    dt,
    variable_map = variable_map,
    variables = variables,
    support_suffix = support_suffix,
    spend_suffix = spend_suffix,
    curve_type = curve_type,
    anchor_saturation = anchor_saturation,
    shape = shape,
    adstock_decay = adstock_decay,
    channel_curve_anchors = channel_curve_anchors
  )

  multiplier_grid <- sort(unique(pmax(bau_num(multiplier_grid), 0)))
  multiplier_grid <- multiplier_grid[is.finite(multiplier_grid)]
  if (!length(multiplier_grid)) stop("multiplier_grid must include finite non-negative values.", call. = FALSE)
  if (!any(abs(multiplier_grid - 1) < 1e-8)) multiplier_grid <- sort(unique(c(multiplier_grid, 1)))
  step_pct <- bau_num(step_pct)[1]
  if (!is.finite(step_pct) || step_pct <= 0) step_pct <- 0.01
  normalize_curve_x <- isTRUE(normalize_curve_x)
  value_per_kpi <- bau_num(value_per_kpi)[1]

  dt[, ("__bau_train__") := bau_train_mask(dt, train_col = train_col, holdout_col = holdout_col)]
  use_group <- !is.null(group_col) && nzchar(group_col) && group_col %in% names(dt)
  if (!use_group) {
    group_col <- NULL
    group_values <- "ALL"
  } else {
    group_values <- sort(unique(as.character(dt[[group_col]])))
    group_values <- group_values[nzchar(group_values) & !is.na(group_values)]
    if (!length(group_values)) group_values <- "ALL"
  }
  include_group <- curve_scope %in% c("group", "both") || (identical(curve_scope, "auto") && use_group)
  include_total <- curve_scope %in% c("total", "both", "auto")
  scopes <- c(if (include_group) "group", if (include_total) "total")
  if (!length(scopes)) scopes <- "total"

  rrate_diag_rows <- vector("list", nrow(vm))
  for (ii in seq_len(nrow(vm))) {
    v <- as.character(vm$variable[ii])
    supplied <- isTRUE(vm$adstock_decay_supplied[ii])
    can_estimate <- estimate_rrate && !is.null(dep_var_col) && (!supplied || isTRUE(rrate_overwrite_supplied))
    if (isTRUE(can_estimate)) {
      est <- bau_estimate_rrate_one(
        dt = dt,
        variable = v,
        support_col = as.character(vm$support_col[ii]),
        dep_var_col = dep_var_col,
        date_col = date_col,
        train_col = "__bau_train__",
        default_rrate = bau_num(vm$adstock_decay[ii]),
        rrate_grid = rrate_grid,
        rrate_bounds = rrate_bounds,
        rrate_min_active_weeks = rrate_min_active_weeks,
        rrate_min_improvement = rrate_min_improvement,
        rrate_complexity_penalty = rrate_complexity_penalty,
        rrate_plateau_rel_tol = rrate_plateau_rel_tol,
        rrate_plateau_abs_tol = rrate_plateau_abs_tol,
        anchor_saturation = bau_num(vm$anchor_saturation[ii]),
        shape = bau_num(vm$shape[ii]),
        curve_type = as.character(vm$curve_type[ii]),
        curve_normalization_scope = curve_normalization_scope
      )
      vm[ii, `:=`(
        adstock_decay = bau_clip(est$rrate_selected[1], 0, 0.999),
        adstock_decay_source = est$rrate_source[1]
      )]
    } else {
      est <- data.table::data.table(
        variable = v,
        rrate_selected = bau_num(vm$adstock_decay[ii]),
        rrate_raw_best = NA_real_,
        rrate_source = if (supplied) "supplied" else if (is.null(dep_var_col)) "default_no_dep_var" else "default_estimation_disabled",
        rrate_estimation_status = if (supplied) "not_estimated_supplied_rrate" else if (is.null(dep_var_col)) "not_estimated_no_dep_var" else "not_estimated_disabled",
        rrate_active_weeks = NA_integer_,
        rrate_improvement = NA_real_,
        rrate_score_selected = NA_real_,
        rrate_score_default = NA_real_,
        rrate_at_bound = FALSE,
        rrate_plateau_adjusted = FALSE,
        rrate_coef_sign = NA_real_
      )
      vm[ii, adstock_decay_source := est$rrate_source[1]]
    }
    rrate_diag_rows[[ii]] <- est
  }
  rrate_diagnostics <- data.table::rbindlist(rrate_diag_rows, fill = TRUE)

  rows <- list()
  meta_rows <- list()
  for (ii in seq_len(nrow(vm))) {
    v <- as.character(vm$variable[ii])
    support_col <- as.character(vm$support_col[ii])
    spend_col <- as.character(vm$spend_col[ii])
    if (is.na(spend_col) || !nzchar(spend_col) || !(spend_col %in% names(dt))) spend_col <- NA_character_
    ct <- as.character(vm$curve_type[ii])
    sh <- bau_num(vm$shape[ii])
    rr <- bau_num(vm$adstock_decay[ii])
    anchor <- bau_num(vm$anchor_saturation[ii])
    current_contribution_input <- bau_num(vm$current_contribution[ii])
    current_roi_input <- bau_num(vm$current_roi[ii])
    current_cpkpi_input <- bau_num(vm$current_cost_per_kpi[ii])
    value_per_kpi_i <- bau_first_finite(vm$value_per_kpi[ii], value_per_kpi)
    rdiag <- rrate_diagnostics[variable == v][1]

    scope_groups <- list()
    if ("group" %in% scopes) {
      scope_groups <- c(scope_groups, stats::setNames(as.list(group_values), group_values))
    }
    if ("total" %in% scopes) {
      scope_groups <- c(scope_groups, list(ALL = "ALL"))
    }

    for (gg_name in names(scope_groups)) {
      gg <- scope_groups[[gg_name]]
      sc <- if (identical(gg_name, "ALL") && "total" %in% scopes) "total" else "group"
      basis_requested <- support_basis
      if (identical(basis_requested, "auto")) {
        basis_requested <- if (!is.null(population_col) && identical(sc, "group")) "per_population_index" else "raw"
      }
      ser <- bau_prepare_scope_series(
        dt,
        support_col = support_col,
        spend_col = spend_col,
        population_col = population_col,
        date_col = date_col,
        group_col = group_col,
        group_value = gg,
        scope = sc,
        train_col = "__bau_train__",
        support_basis = basis_requested
      )
      if (!nrow(ser)) next
      train_mask <- as.logical(ser$train__)
      if (!any(train_mask)) train_mask <- rep(TRUE, nrow(ser))
      x_curve <- pmax(bau_num(ser$curve_x__), 0)
      x_raw <- pmax(bau_num(ser$support_raw__), 0)
      spend_raw <- pmax(bau_num(ser$spend_raw__), 0)
      carry <- bau_adstock(x_curve, decay = rr)
      norm_mask <- bau_curve_norm_mask(x_raw, train_mask, curve_normalization_scope)
      carry_scale <- if (normalize_curve_x) mean(carry[norm_mask], na.rm = TRUE) else 1
      if (!is.finite(carry_scale) || carry_scale <= 1e-10) carry_scale <- 1
      z <- carry / carry_scale
      active <- train_mask & is.finite(z) & z > 0 & is.finite(x_raw) & x_raw > 0
      anchor_x <- stats::median(z[active], na.rm = TRUE)
      rate <- bau_rate_from_anchor(anchor_x, anchor_saturation = anchor, shape = sh, curve_type = ct)
      if (!is.finite(rate) || rate <= 0) {
        rate <- 1
        curve_status <- "fallback_flat_or_no_support"
      } else {
        curve_status <- "anchored"
      }
      sat_current <- bau_saturation(z, curve_rate = rate, shape = sh, curve_type = ct)
      current_response_mean <- mean(sat_current[norm_mask], na.rm = TRUE)
      if (!is.finite(current_response_mean) || current_response_mean <= 1e-10) current_response_mean <- 1
      current_support <- sum(x_raw[train_mask], na.rm = TRUE)
      current_spend <- if (all(is.na(spend_raw))) NA_real_ else sum(spend_raw[train_mask], na.rm = TRUE)

      current_contribution <- current_contribution_input
      if (!is.finite(current_contribution) && is.finite(current_spend) && current_spend > 0 && is.finite(current_roi_input)) {
        current_contribution <- current_spend * current_roi_input
      }
      if (!is.finite(current_contribution) && is.finite(current_spend) && current_spend > 0 &&
          is.finite(current_cpkpi_input) && current_cpkpi_input > 0) {
        current_contribution <- current_spend / current_cpkpi_input
      }
      optimizer_ready <- is.finite(current_contribution) && is.finite(current_spend) && current_spend > 0

      make_one <- function(m) {
        carry_eval <- bau_adstock(x_curve * m, decay = rr)
        z_eval <- carry_eval / carry_scale
        sat_eval <- bau_saturation(z_eval, curve_rate = rate, shape = sh, curve_type = ct)
        response_mean <- mean(sat_eval[norm_mask], na.rm = TRUE)
        response_index <- if (is.finite(response_mean)) response_mean / current_response_mean else NA_real_

        carry_up <- bau_adstock(x_curve * (m + step_pct), decay = rr)
        sat_up <- bau_saturation(carry_up / carry_scale, curve_rate = rate, shape = sh, curve_type = ct)
        response_up <- mean(sat_up[norm_mask], na.rm = TRUE) / current_response_mean
        spend <- if (is.finite(current_spend)) current_spend * m else NA_real_
        support <- current_support * m
        contribution <- if (is.finite(current_contribution)) current_contribution * response_index else NA_real_
        incremental_contribution_for_mroi <- if (is.finite(current_contribution)) current_contribution * (response_up - response_index) else NA_real_
        incremental_spend_for_mroi <- if (is.finite(current_spend)) current_spend * step_pct else NA_real_
        data.table::data.table(
          scope = sc,
          group = if (identical(sc, "total")) "ALL" else as.character(gg),
          variable = v,
          spend_multiplier = m,
          spend = spend,
          current_spend = current_spend,
          support = support,
          current_support = current_support,
          response_index = response_index,
          saturation = response_mean,
          current_saturation = current_response_mean,
          contribution = contribution,
          current_contribution = current_contribution,
          contribution_vs_current = contribution - current_contribution,
          incremental_spend_for_mroi = incremental_spend_for_mroi,
          incremental_contribution_for_mroi = incremental_contribution_for_mroi,
          roi = if (is.finite(spend) && spend > 1e-10 && is.finite(contribution)) contribution / spend else NA_real_,
          mroi = if (is.finite(incremental_spend_for_mroi) && incremental_spend_for_mroi > 1e-10) incremental_contribution_for_mroi / incremental_spend_for_mroi else NA_real_,
          cost_per_kpi = if (is.finite(contribution) && contribution > 1e-10 && is.finite(spend)) spend / contribution else NA_real_,
          value_per_cost = if (is.finite(value_per_kpi_i) && is.finite(spend) && spend > 1e-10 && is.finite(contribution)) contribution * value_per_kpi_i / spend else NA_real_,
          marginal_response_index = (response_up - response_index) / step_pct,
          curve_type = ct,
          anchor_saturation = anchor,
          curve_shape = sh,
          adstock_decay = rr,
          adstock_decay_source = as.character(rdiag$rrate_source %||% vm$adstock_decay_source[ii]),
          curve_rate = rate,
          support_basis = ser$support_basis_used__[1],
          population_scale = ser$population_scale__[1],
          optimizer_ready = optimizer_ready,
          response_curve_basis = "bau_anchor_from_historical_support",
          uncertainty_note = "Deterministic BAU curve. Shape comes from historical support and anchor settings; business scale comes only from supplied contribution/ROI/cost-per-KPI inputs.",
          flighting_assumption = "Each curve scales the historical support path in the supplied data cut.",
          cost_assumption = if (is.na(spend_col)) "No spend_col was available; economics are not optimizer-ready." else "Spend scales with support using historical spend totals."
        )
      }
      rows[[length(rows) + 1L]] <- data.table::rbindlist(lapply(multiplier_grid, make_one), fill = TRUE)
      meta_rows[[length(meta_rows) + 1L]] <- data.table::data.table(
        scope = sc,
        group = if (identical(sc, "total")) "ALL" else as.character(gg),
        variable = v,
        support_col = support_col,
        spend_col = spend_col,
        curve_type = ct,
        anchor_saturation = anchor,
        curve_shape = sh,
        adstock_decay = rr,
        adstock_decay_source = as.character(rdiag$rrate_source %||% vm$adstock_decay_source[ii]),
        rrate_estimation_status = as.character(rdiag$rrate_estimation_status %||% NA_character_),
        rrate_improvement = bau_num(rdiag$rrate_improvement %||% NA_real_),
        rrate_active_weeks = as.integer(rdiag$rrate_active_weeks %||% NA_integer_),
        rrate_at_bound = isTRUE(rdiag$rrate_at_bound),
        rrate_plateau_adjusted = isTRUE(rdiag$rrate_plateau_adjusted),
        curve_rate = rate,
        anchor_x = anchor_x,
        saturation_at_anchor = bau_saturation(anchor_x, rate, sh, ct),
        current_support = current_support,
        current_spend = current_spend,
        current_contribution = current_contribution,
        support_basis = ser$support_basis_used__[1],
        curve_normalization_scope = curve_normalization_scope,
        population_col = population_col %||% NA_character_,
        population_scale = ser$population_scale__[1],
        rows_used = sum(train_mask, na.rm = TRUE),
        curve_status = curve_status,
        optimizer_ready = optimizer_ready,
        evidence_note = if (optimizer_ready) {
          "Curve has a business scale and can feed scenario/optimizer tools."
        } else {
          "Curve shape is usable for BAU planning/audit, but no contribution/ROI/cost-per-KPI scale was supplied."
        }
      )
    }
  }

  response_curves <- data.table::rbindlist(rows, fill = TRUE)
  curve_metadata <- data.table::rbindlist(meta_rows, fill = TRUE)
  if (nrow(response_curves)) {
    data.table::setorderv(response_curves, c("variable", "scope", "group", "spend_multiplier"))
  }
  if (nrow(curve_metadata)) {
    data.table::setorderv(curve_metadata, c("variable", "scope", "group"))
  }
  list(
    package_info = econimap_output_metadata("create_bau_response_curves", surface = "bau_response_curves"),
    response_curves = response_curves[],
    curve_metadata = curve_metadata[],
    rrate_diagnostics = rrate_diagnostics[],
    settings = list(
      curve_type_default = curve_type,
      anchor_saturation_default = anchor_saturation,
      dep_var_col = dep_var_col,
      estimate_rrate = estimate_rrate,
      rrate_overwrite_supplied = rrate_overwrite_supplied,
      support_basis = support_basis,
      curve_scope = curve_scope,
      normalize_curve_x = normalize_curve_x,
      curve_normalization_scope = curve_normalization_scope,
      train_col = train_col,
      holdout_col = holdout_col,
      date_col = date_col,
      group_col = group_col,
      population_col = population_col
    )
  )
}

build_bau_response_curves <- create_bau_response_curves
create_baseline_response_curves <- create_bau_response_curves
