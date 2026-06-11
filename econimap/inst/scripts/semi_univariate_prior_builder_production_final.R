# semi_univariate_prior_builder_production_final.R
# Legacy implementation file for the MMM prior evidence engine.
# Source mmm_workflow.R in new analyst workbooks; this file remains stable so
# older notebooks that call prior_builder() continue to work.
#
# Purpose: analyst-facing prior-evidence builder with:
# - semi-univariate/profile curve scans used as one evidence layer, not as a final causal model
# - optional variable_map spend/support pairing
# - modeled_x_col support/spend handoff
# - total-level shared curve option
# - built-in Fourier, holiday, and week-of-month controls with explicit week-ending alignment
# - rrate estimation, evidence-weighted cvalue anchoring, saturation transform, coefficient estimation
# - safer duplicate-week aggregation, curve default handling, and prior precision handoff
# - data-granularity routing, transformed collinearity/VIF diagnostics, and benchmark-prior blending
# - observational ramp evidence, missing-data gates, and contribution/elasticity sanity bounds

if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required.")

`%||%` <- function(x, y) if (is.null(x)) y else x

pb_clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

pb_mean_index <- function(x) {
  x <- as.numeric(x)
  m <- mean(x, na.rm = TRUE)
  if (!is.finite(m) || abs(m) < 1e-12) return(rep(NA_real_, length(x)))
  x / m
}

pb_normalize_curve_type <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out[is.na(out) | !nzchar(out)] <- "weibull"
  out[out %in% c("weibull_cdf", "exponential", "exp")] <- "weibull"
  out[out %in% c("hill_function", "hill_curve", "logistic_saturation")] <- "hill"
  bad <- setdiff(unique(out), c("weibull", "hill"))
  if (length(bad)) stop("Unsupported curve_type values: ", paste(bad, collapse = ", "), ". Use 'weibull' or 'hill'.")
  out
}

pb_parse_date <- function(x, label = "date_col") {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "IDate")) return(as.Date(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))

  if (is.numeric(x)) {
    # Excel serial fallback, only if dates look like serial dates.
    if (all(is.na(x)) || median(x, na.rm = TRUE) < 20000) {
      stop("Could not parse ", label, ": numeric values do not look like dates.")
    }
    return(as.Date(x, origin = "1899-12-30"))
  }

  xc <- as.character(x)
  formats <- c("%m/%d/%Y", "%Y-%m-%d", "%m-%d-%Y", "%Y/%m/%d", "%d-%b-%Y")
  for (fmt in formats) {
    d <- suppressWarnings(as.Date(xc, format = fmt))
    if (sum(!is.na(d)) >= max(1L, floor(0.90 * length(d)))) return(d)
  }
  d <- suppressWarnings(as.Date(xc))
  if (sum(!is.na(d)) >= max(1L, floor(0.90 * length(d)))) return(d)
  stop("Could not parse ", label, " as dates.")
}

pb_force_numeric_vec <- function(x, colname) {
  if (is.numeric(x)) return(as.numeric(x))
  old_bad <- !is.na(x) & trimws(as.character(x)) != ""
  y <- suppressWarnings(as.numeric(x))
  new_bad <- old_bad & is.na(y)
  if (any(new_bad)) {
    vals <- unique(as.character(x[new_bad]))
    stop("Column '", colname, "' contains non-numeric values: ", paste(utils::head(vals, 5), collapse = ", "))
  }
  y
}

pb_adstock <- function(x, rrate) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- 0
  rrate <- pb_clip(as.numeric(rrate)[1], 0, 0.99)
  out <- numeric(length(x))
  for (i in seq_along(x)) {
    out[i] <- x[i] + if (i == 1L) 0 else rrate * out[i - 1L]
  }
  out
}

pb_saturation <- function(x_adstocked, cvalue, dvalue = 1, curve_type = "weibull") {
  x <- pmax(as.numeric(x_adstocked), 0)
  z <- pmax(x * as.numeric(cvalue)[1], 1e-12)
  dvalue <- as.numeric(dvalue)[1]
  if (!is.finite(dvalue) || dvalue <= 0) dvalue <- 1
  pow_z <- z ^ dvalue
  curve_type <- pb_normalize_curve_type(curve_type)[1]
  if (identical(curve_type, "hill")) pow_z / (1 + pow_z) else 1 - exp(-pow_z)
}

pb_calc_cvalue <- function(x_adstocked_mi, anchor_saturation = 0.50, dvalue = 1, active_mask = NULL, curve_type = "weibull") {
  # IMPORTANT: cvalue is defined on the SAME SCALE used in the saturation transform.
  # The prior-builder curve scale is mean-indexed adstocked x, not raw impressions/clicks/spend.
  # Therefore median active mean-indexed adstocked x is the anchor point.
  x <- as.numeric(x_adstocked_mi)
  if (is.null(active_mask)) {
    active_mask <- is.finite(x) & x > 0
  } else {
    active_mask <- as.logical(active_mask) & is.finite(x) & x > 0
  }
  x_anchor <- stats::median(x[active_mask], na.rm = TRUE)
  if (!is.finite(x_anchor) || x_anchor <= 0) return(NA_real_)
  anchor_saturation <- pb_clip(as.numeric(anchor_saturation)[1], 0.01, 0.99)
  curve_type <- pb_normalize_curve_type(curve_type)[1]
  if (identical(curve_type, "hill")) {
    (anchor_saturation / (1 - anchor_saturation)) ^ (1 / dvalue) / x_anchor
  } else {
    (-log(1 - anchor_saturation)) ^ (1 / dvalue) / x_anchor
  }
}

pb_cvalue_anchor_x <- function(x_adstocked_mi,
                               active_mask = NULL,
                               method = c("median_active", "industry_hybrid", "industry_default", "active_quantile"),
                               industry_half_saturation = 0.80,
                               active_quantile = 0.75,
                               hybrid_industry_weight = NA_real_) {
  method <- match.arg(method)
  x <- as.numeric(x_adstocked_mi)
  if (is.null(active_mask)) active_mask <- is.finite(x) & x > 0
  active_mask <- as.logical(active_mask) & is.finite(x) & x > 0
  if (!any(active_mask)) return(list(anchor_x = NA_real_, method = method, data_weight = NA_real_, active_cv = NA_real_))
  xa <- x[active_mask]
  med <- stats::median(xa, na.rm = TRUE)
  q <- as.numeric(stats::quantile(xa, probs = pb_clip(active_quantile, 0.50, 0.95), na.rm = TRUE, names = FALSE))
  industry <- as.numeric(industry_half_saturation)[1]
  if (!is.finite(industry) || industry <= 0) industry <- 0.80
  active_cv <- stats::sd(xa, na.rm = TRUE) / mean(xa, na.rm = TRUE)
  if (!is.finite(active_cv)) active_cv <- NA_real_
  if (!is.finite(med) || med <= 0) med <- industry
  if (!is.finite(q) || q <= 0) q <- med

  if (identical(method, "median_active")) {
    return(list(anchor_x = med, method = method, data_weight = 1, active_cv = active_cv))
  }
  if (identical(method, "industry_default")) {
    return(list(anchor_x = industry, method = method, data_weight = 0, active_cv = active_cv))
  }
  if (identical(method, "active_quantile")) {
    return(list(anchor_x = q, method = method, data_weight = 1, active_cv = active_cv))
  }

  w_ind <- as.numeric(hybrid_industry_weight)[1]
  if (!is.finite(w_ind)) {
    data_weight <- if (is.finite(active_cv)) pb_clip((active_cv - 0.10) / 0.50, 0.25, 0.75) else 0.50
    w_ind <- 1 - data_weight
  } else {
    w_ind <- pb_clip(w_ind, 0, 1)
    data_weight <- 1 - w_ind
  }
  data_anchor <- if (is.finite(active_cv) && active_cv < 0.20) q else med
  anchor_x <- exp(data_weight * log(max(data_anchor, 1e-8)) + w_ind * log(max(industry, 1e-8)))
  list(anchor_x = anchor_x, method = method, data_weight = data_weight, active_cv = active_cv)
}

pb_calc_cvalue_with_anchor <- function(x_adstocked_mi,
                                       anchor_saturation = 0.50,
                                       dvalue = 1,
                                       active_mask = NULL,
                                       cvalue_anchor_method = c("median_active", "industry_hybrid", "industry_default", "active_quantile"),
                                       cvalue_industry_half_saturation = 0.80,
                                       cvalue_active_quantile = 0.75,
                                       cvalue_hybrid_industry_weight = NA_real_,
                                       curve_type = "weibull") {
  cvalue_anchor_method <- match.arg(cvalue_anchor_method)
  ax <- pb_cvalue_anchor_x(
    x_adstocked_mi = x_adstocked_mi,
    active_mask = active_mask,
    method = cvalue_anchor_method,
    industry_half_saturation = cvalue_industry_half_saturation,
    active_quantile = cvalue_active_quantile,
    hybrid_industry_weight = cvalue_hybrid_industry_weight
  )
  anchor_saturation <- pb_clip(as.numeric(anchor_saturation)[1], 0.01, 0.99)
  dvalue <- as.numeric(dvalue)[1]
  cvalue <- if (is.finite(dvalue) && dvalue > 0 && is.finite(ax$anchor_x) && ax$anchor_x > 0) {
    curve_type <- pb_normalize_curve_type(curve_type)[1]
    if (identical(curve_type, "hill")) {
      (anchor_saturation / (1 - anchor_saturation)) ^ (1 / dvalue) / ax$anchor_x
    } else {
      (-log(1 - anchor_saturation)) ^ (1 / dvalue) / ax$anchor_x
    }
  } else {
    NA_real_
  }
  list(
    cvalue = cvalue,
    cvalue_anchor_x = ax$anchor_x,
    cvalue_anchor_method = cvalue_anchor_method,
    cvalue_anchor_data_weight = ax$data_weight,
    cvalue_anchor_active_cv = ax$active_cv
  )
}

pb_weekday_to_wday <- function(week_end_day = "Sunday") {
  if (is.numeric(week_end_day)) {
    out <- as.integer(week_end_day[1])
    if (!is.finite(out) || out < 0L || out > 6L) stop("Numeric week_end_day must be 0=Sunday through 6=Saturday.")
    return(out)
  }

  x <- tolower(trimws(as.character(week_end_day[1])))
  x <- sub("day$", "", x)
  map <- c(sun = 0L, mon = 1L, tue = 2L, tues = 2L, wed = 3L, thu = 4L, thur = 4L, thurs = 4L, fri = 5L, sat = 6L)
  if (!x %in% names(map)) {
    stop("week_end_day must be one of Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, or numeric 0-6.")
  }
  unname(map[[x]])
}

pb_wday_name <- function(wday_num) {
  c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[[as.integer(wday_num) + 1L]]
}

pb_date_to_week_end <- function(dates, week_end_day = "Sunday") {
  d <- pb_parse_date(dates, "dates")
  target <- pb_weekday_to_wday(week_end_day)
  current <- as.POSIXlt(d)$wday
  d + ((target - current) %% 7L)
}

pb_easter <- function(year) {
  y <- as.integer(year)
  a <- y %% 19L
  b <- y %/% 100L
  c <- y %% 100L
  d <- b %/% 4L
  e <- b %% 4L
  f <- (b + 8L) %/% 25L
  g <- (b - f + 1L) %/% 3L
  h <- (19L * a + b - d - g + 15L) %% 30L
  i <- c %/% 4L
  k <- c %% 4L
  l <- (32L + 2L * e + 2L * i - h - k) %% 7L
  m <- (a + 11L * h + 22L * l) %/% 451L
  month <- (h + l - 7L * m + 114L) %/% 31L
  day <- ((h + l - 7L * m + 114L) %% 31L) + 1L
  as.Date(sprintf("%04d-%02d-%02d", y, month, day))
}

pb_nth_weekday <- function(year, month, weekday, n) {
  d <- as.Date(sprintf("%04d-%02d-01", year, month))
  d + ((weekday - as.POSIXlt(d)$wday) %% 7) + 7 * (n - 1L)
}

pb_last_weekday <- function(year, month, weekday) {
  first <- as.Date(sprintf("%04d-%02d-01", year, month))
  next_month <- if (month == 12L) as.Date(sprintf("%04d-01-01", year + 1L)) else as.Date(sprintf("%04d-%02d-01", year, month + 1L))
  last <- next_month - 1L
  last - ((as.POSIXlt(last)$wday - weekday) %% 7)
}

pb_us_holidays <- function(years) {
  data.table::rbindlist(lapply(years, function(y) {
    tgive <- pb_nth_weekday(y, 11, 4, 4)
    data.table::data.table(
      holiday = c(
        "newyear", "mlk", "presidents", "easter", "mothersday", "memday",
        "fathersday", "juneteenth", "july4", "labday", "halloween",
        "tgive", "blackfriday", "cybermonday", "xmas"
      ),
      hdate = as.Date(c(
        sprintf("%04d-01-01", y),
        as.character(pb_nth_weekday(y, 1, 1, 3)),
        as.character(pb_nth_weekday(y, 2, 1, 3)),
        as.character(pb_easter(y)),
        as.character(pb_nth_weekday(y, 5, 0, 2)),
        as.character(pb_last_weekday(y, 5, 1)),
        as.character(pb_nth_weekday(y, 6, 0, 3)),
        sprintf("%04d-06-19", y),
        sprintf("%04d-07-04", y),
        as.character(pb_nth_weekday(y, 9, 1, 1)),
        sprintf("%04d-10-31", y),
        as.character(tgive),
        as.character(tgive + 1L),
        as.character(tgive + 4L),
        sprintf("%04d-12-25", y)
      ))
    )
  }))
}

pb_build_controls <- function(dates,
                              use_fourier = TRUE,
                              fourier_period = 52.18,
                              fourier_K = 2,
                              use_holidays = TRUE,
                              holiday_window_weeks = c(-1, 0, 1),
                              use_week_of_month = TRUE,
                              week_end_day = "Sunday") {
  # All model dates are converted to the containing week-ending date before holiday matching.
  # Example: if week_end_day = "Sunday", Thanksgiving Thursday maps to that Sunday's model week.
  raw_dates <- pb_parse_date(dates, "dates")
  model_week_end <- pb_date_to_week_end(raw_dates, week_end_day)
  week_end_num <- pb_weekday_to_wday(week_end_day)
  week_end_name <- pb_wday_name(week_end_num)

  dt <- data.table::data.table(date__ = raw_dates, model_week_end__ = model_week_end)
  cols <- character()
  dropped_controls <- character()

  if (isTRUE(use_fourier) && fourier_K > 0) {
    t_idx <- as.numeric(model_week_end - min(model_week_end, na.rm = TRUE)) / 7
    season_period <- as.numeric(fourier_period)
    for (k in seq_len(as.integer(fourier_K))) {
      s <- paste0("fourier_sin_", k)
      c <- paste0("fourier_cos_", k)
      dt[[s]] <- sin((2 * pi * k * t_idx) / season_period)
      dt[[c]] <- cos((2 * pi * k * t_idx) / season_period)
      cols <- c(cols, s, c)
    }
  }

  holiday_map <- data.table::data.table()
  if (isTRUE(use_holidays)) {
    yr_min <- as.integer(format(min(model_week_end, na.rm = TRUE) - 370L, "%Y"))
    yr_max <- as.integer(format(max(model_week_end, na.rm = TRUE) + 370L, "%Y"))
    yrs <- seq(yr_min, yr_max)
    hd <- pb_us_holidays(yrs)
    hd[, holiday_week_end := pb_date_to_week_end(hdate, week_end_day)]
    holiday_map <- hd[, .(holiday, hdate, holiday_week_end)]

    for (h in unique(hd$holiday)) {
      base_week_ends <- unique(hd[holiday == h, holiday_week_end])
      for (w in holiday_window_weeks) {
        nm <- paste0("hol_", h, ifelse(w < 0, paste0("_m", abs(w)), ifelse(w > 0, paste0("_p", w), "_w0")))
        dt[[nm]] <- as.integer(model_week_end %in% (base_week_ends + 7L * as.integer(w)))
        cols <- c(cols, nm)
      }
    }
  }

  if (isTRUE(use_week_of_month)) {
    wom <- pmin(5L, ((as.integer(format(model_week_end, "%d")) - 1L) %/% 7L) + 1L)
    for (i in 2:5) {
      nm <- paste0("wom_", i)
      dt[[nm]] <- as.integer(wom == i)
      cols <- c(cols, nm)
    }
  }

  metadata <- list(
    week_end_day = week_end_name,
    week_end_day_num = week_end_num,
    holiday_alignment_rule = "holiday_date_and_input_date_mapped_to_containing_week_end",
    input_date_min = min(raw_dates, na.rm = TRUE),
    input_date_max = max(raw_dates, na.rm = TRUE),
    model_week_end_min = min(model_week_end, na.rm = TRUE),
    model_week_end_max = max(model_week_end, na.rm = TRUE),
    holiday_window_weeks = holiday_window_weeks,
    holiday_map = holiday_map
  )

  if (!length(cols)) {
    out <- matrix(numeric(0), nrow = length(raw_dates), ncol = 0)
    colnames(out) <- character()
    attr(out, "controls_used") <- character()
    attr(out, "controls_dropped_zero_variance") <- character()
    attr(out, "control_metadata") <- metadata
    return(out)
  }

  # Drop zero-variance controls to prevent singular design clutter.
  keep <- vapply(cols, function(nm) {
    z <- dt[[nm]]
    is.finite(stats::var(z, na.rm = TRUE)) && stats::var(z, na.rm = TRUE) > 1e-12
  }, logical(1))
  dropped_controls <- cols[!keep]
  cols <- cols[keep]

  if (!length(cols)) {
    out <- matrix(numeric(0), nrow = length(raw_dates), ncol = 0)
    colnames(out) <- character()
    attr(out, "controls_used") <- character()
    attr(out, "controls_dropped_zero_variance") <- dropped_controls
    attr(out, "control_metadata") <- metadata
    return(out)
  }

  out <- as.matrix(dt[, ..cols])
  attr(out, "controls_used") <- cols
  attr(out, "controls_dropped_zero_variance") <- dropped_controls
  attr(out, "control_metadata") <- metadata
  out
}

pb_parse_variable_map <- function(variable_map = NULL, media_cols = character(), curve_cols = NULL) {
  if (is.null(variable_map)) {
    if (!length(media_cols)) stop("Provide media_cols when variable_map is NULL.")
    vm <- data.table::data.table(
      variable = as.character(media_cols),
      spend_col = as.character(media_cols),
      support_col = NA_character_,
      support_type = NA_character_,
      modeled_x_col = as.character(media_cols),
      modeled_x_basis = "spend",
      curve_type = "weibull"
    )
  } else {
    vm <- data.table::as.data.table(data.table::copy(variable_map))
    if (!"variable" %in% names(vm)) {
      if ("channel" %in% names(vm)) vm[, variable := as.character(channel)] else stop("variable_map must contain variable or channel.")
    }
    for (nm in c("spend_col", "support_col", "support_type", "modeled_x_col", "modeled_x_basis", "curve_type")) {
      if (!nm %in% names(vm)) vm[, (nm) := NA_character_]
    }
    for (nm in c("variable", "spend_col", "support_col", "support_type", "modeled_x_col", "modeled_x_basis", "curve_type")) {
      vm[, (nm) := as.character(get(nm))]
      vm[get(nm) %in% c("", "NA", "NaN", "NULL"), (nm) := NA_character_]
    }
    for (i in seq_len(nrow(vm))) {
      if (is.na(vm$modeled_x_col[i])) {
        vm$modeled_x_col[i] <- if (!is.na(vm$support_col[i])) vm$support_col[i] else vm$spend_col[i]
      }
      if (is.na(vm$modeled_x_basis[i])) {
        if (!is.na(vm$support_col[i]) && identical(vm$modeled_x_col[i], vm$support_col[i])) {
          vm$modeled_x_basis[i] <- "support"
        } else if (!is.na(vm$spend_col[i]) && identical(vm$modeled_x_col[i], vm$spend_col[i])) {
          vm$modeled_x_basis[i] <- "spend"
        } else {
          vm$modeled_x_basis[i] <- "custom"
        }
      }
    }
  }
  if (!"curve_type" %in% names(vm)) vm[, curve_type := "weibull"]
  vm[is.na(curve_type) | !nzchar(curve_type), curve_type := "weibull"]
  vm[, curve_type := pb_normalize_curve_type(curve_type)]
  if (anyDuplicated(vm$variable)) stop("variable_map has duplicate variable names.")
  if (is.null(curve_cols)) curve_cols <- vm$variable
  vm[, has_curve := variable %in% curve_cols]
  vm[]
}

pb_vm_value <- function(vm, row_i, candidates, default = NA, type = c("numeric", "character", "logical")) {
  type <- match.arg(type)
  if (is.null(vm) || !nrow(vm)) return(default)
  for (nm in candidates) {
    if (!nm %in% names(vm)) next
    x <- vm[[nm]][row_i]
    if (type == "numeric") {
      y <- suppressWarnings(as.numeric(x))
      if (is.finite(y)) return(y)
    } else if (type == "logical") {
      if (is.logical(x) && !is.na(x)) return(as.logical(x))
      y <- tolower(trimws(as.character(x)))
      if (y %in% c("true", "t", "yes", "y", "1")) return(TRUE)
      if (y %in% c("false", "f", "no", "n", "0")) return(FALSE)
    } else {
      y <- trimws(as.character(x))
      if (!is.na(y) && nzchar(y) && !y %in% c("NA", "NaN", "NULL")) return(y)
    }
  }
  default
}

pb_variable_map_curve_anchor_overrides <- function(vm) {
  if (is.null(vm) || !nrow(vm) || !"variable" %in% names(vm)) return(NULL)
  rows <- vector("list", nrow(vm))
  for (i in seq_len(nrow(vm))) {
    variable <- as.character(vm$variable[i])
    sat <- pb_vm_value(
      vm, i,
      c("anchor_saturation_handoff", "anchor_saturation", "median_anchor_saturation",
        "median_anchor", "saturation_at_median", "saturation_at_anchor"),
      default = NA_real_,
      type = "numeric"
    )
    cvalue_mult <- pb_vm_value(vm, i, c("cvalue_multiplier_handoff", "cvalue_multiplier"), default = NA_real_, type = "numeric")
    rel <- pb_vm_value(vm, i, c("anchor_reliability", "reliability"), default = NA_real_, type = "numeric")
    wt <- pb_vm_value(vm, i, c("anchor_weight_final", "anchor_weight"), default = NA_real_, type = "numeric")
    lower <- pb_vm_value(vm, i, c("anchor_lower_90", "anchor_saturation_lower_90"), default = NA_real_, type = "numeric")
    upper <- pb_vm_value(vm, i, c("anchor_upper_90", "anchor_saturation_upper_90"), default = NA_real_, type = "numeric")
    width <- pb_vm_value(vm, i, c("anchor_uncertainty_width_90", "anchor_saturation_width_90"), default = NA_real_, type = "numeric")
    should_drive <- pb_vm_value(vm, i, "anchor_should_drive_curve_prior", default = NA, type = "logical")
    actionability <- pb_vm_value(vm, i, "anchor_actionability_tier", default = NA_character_, type = "character")
    authority <- pb_vm_value(vm, i, "anchor_authority_tier", default = NA_character_, type = "character")

    has_override <- is.finite(sat) || is.finite(cvalue_mult) || is.finite(rel) || is.finite(wt) ||
      is.finite(lower) || is.finite(upper) || is.finite(width) ||
      !is.na(should_drive) || !is.na(actionability) || !is.na(authority)
    if (!has_override) next

    if (is.finite(sat)) sat <- pb_clip(sat, 0.05, 0.95)
    if (!is.finite(cvalue_mult) && is.finite(sat)) {
      cvalue_mult <- (-log(1 - sat)) / (-log(1 - 0.50))
    }
    if (!is.finite(rel)) rel <- if (is.finite(sat) || is.finite(cvalue_mult)) 0.35 else NA_real_
    if (!is.finite(wt) && is.finite(rel)) wt <- pb_clip(rel, 0, 1)
    if (!is.finite(width) && is.finite(lower) && is.finite(upper)) width <- abs(upper - lower)
    if (!nzchar(as.character(actionability %||% "")) || is.na(actionability)) actionability <- if (isTRUE(should_drive)) "actionable" else "directional"
    if (!nzchar(as.character(authority %||% "")) || is.na(authority)) authority <- "analyst_variable_map"

    rows[[i]] <- data.table::data.table(
      variable = variable,
      anchor_saturation = sat,
      anchor_saturation_handoff = sat,
      cvalue_multiplier = cvalue_mult,
      cvalue_multiplier_handoff = cvalue_mult,
      anchor_authority_tier = authority,
      anchor_actionability_tier = actionability,
      anchor_should_drive_curve_prior = if (is.na(should_drive)) FALSE else should_drive,
      anchor_weight_final = wt,
      reliability = rel,
      anchor_uncertainty_width_90 = width,
      anchor_lower_90 = lower,
      anchor_upper_90 = upper,
      anchor_source = "variable_map_override"
    )
  }
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  if (!nrow(out)) return(NULL)
  out
}

pb_merge_curve_anchor_overrides <- function(curve_anchors, overrides) {
  if (is.null(overrides) || !nrow(overrides)) return(curve_anchors)
  ov <- data.table::as.data.table(data.table::copy(overrides))
  if (is.null(curve_anchors) || !nrow(curve_anchors)) return(ov[])
  ca <- data.table::as.data.table(data.table::copy(curve_anchors))
  if (!"variable" %in% names(ca)) return(ov[])
  for (nm in setdiff(names(ov), names(ca))) ca[, (nm) := NA]
  for (nm in setdiff(names(ca), names(ov))) ov[, (nm) := NA]
  ov <- ov[, names(ca), with = FALSE]
  for (i in seq_len(nrow(ov))) {
    target <- as.character(ov$variable[i])
    idx <- which(as.character(ca$variable) == target)
    if (!length(idx)) {
      ca <- data.table::rbindlist(list(ca, ov[i]), use.names = TRUE, fill = TRUE)
      next
    }
    idx <- idx[1]
    for (nm in setdiff(names(ov), "variable")) {
      val <- ov[[nm]][i]
      use_val <- if (is.character(val)) {
        !is.na(val) && nzchar(val)
      } else if (is.logical(val)) {
        !is.na(val)
      } else {
        is.finite(suppressWarnings(as.numeric(val)))
      }
      if (isTRUE(use_val)) ca[idx, (nm) := val]
    }
  }
  ca[]
}

pb_support_diag_one <- function(dt, variable, spend_col_internal, support_col_internal, modeled_x_col_internal, support_type, modeled_x_basis) {
  spend <- if (!is.na(spend_col_internal) && spend_col_internal %in% names(dt)) dt[[spend_col_internal]] else rep(NA_real_, nrow(dt))
  support <- if (!is.na(support_col_internal) && support_col_internal %in% names(dt)) dt[[support_col_internal]] else rep(NA_real_, nrow(dt))
  modeled <- if (!is.na(modeled_x_col_internal) && modeled_x_col_internal %in% names(dt)) dt[[modeled_x_col_internal]] else rep(NA_real_, nrow(dt))

  has_spend <- any(is.finite(spend))
  has_support <- any(is.finite(support))
  input_class <- if (has_spend && has_support) "spend_and_support" else if (has_spend) "spend_only" else if (has_support) "support_only" else "missing_or_invalid"

  cps <- ifelse(is.finite(spend) & is.finite(support) & support > 0, spend / support, NA_real_)
  cps_active <- cps[is.finite(cps)]
  cor_ss <- suppressWarnings(stats::cor(spend, support, use = "pairwise.complete.obs"))
  if (!is.finite(cor_ss)) cor_ss <- NA_real_

  modeled_active <- is.finite(modeled) & modeled > 0
  modeled_share <- mean(modeled_active, na.rm = TRUE)
  modeled_total <- sum(pmax(modeled, 0), na.rm = TRUE)
  modeled_top4 <- if (modeled_total > 0) sum(utils::head(sort(pmax(modeled, 0), decreasing = TRUE), 4), na.rm = TRUE) / modeled_total else NA_real_

  data.table::data.table(
    variable = variable,
    input_class = input_class,
    modeled_x_basis = modeled_x_basis,
    support_type = support_type,
    has_spend = has_spend,
    has_support = has_support,
    spend_support_cor = cor_ss,
    cost_per_support_median = if (length(cps_active)) stats::median(cps_active, na.rm = TRUE) else NA_real_,
    cost_per_support_iqr_ratio = if (length(cps_active) >= 4 && stats::median(cps_active, na.rm = TRUE) > 0) stats::IQR(cps_active, na.rm = TRUE) / stats::median(cps_active, na.rm = TRUE) else NA_real_,
    modeled_active_week_share = modeled_share,
    modeled_top4_concentration = modeled_top4
  )
}

pb_make_model_dt <- function(dt,
                             date_col,
                             dep_var_col,
                             vm,
                             base_cols,
                             control_cols,
                             total_level_shared_curve,
                             duplicate_date_non_additive_strategy = c("mean", "first")) {
  duplicate_date_non_additive_strategy <- match.arg(duplicate_date_non_additive_strategy)
  date_raw <- dt[[date_col]]
  dates <- pb_parse_date(date_raw, date_col)
  out <- data.table::data.table(date__ = dates, y__ = pb_force_numeric_vec(dt[[dep_var_col]], dep_var_col))

  map_rows <- list()
  for (i in seq_len(nrow(vm))) {
    v <- vm$variable[i]
    mx <- vm$modeled_x_col[i]
    if (is.na(mx) || !mx %in% names(dt)) stop("modeled_x_col for variable '", v, "' is missing from input_data: ", mx)
    out[[v]] <- pb_force_numeric_vec(dt[[mx]], mx)

    spend_internal <- NA_character_
    support_internal <- NA_character_
    if (!is.na(vm$spend_col[i]) && vm$spend_col[i] %in% names(dt)) {
      spend_internal <- paste0("__spend__", v)
      out[[spend_internal]] <- pb_force_numeric_vec(dt[[vm$spend_col[i]]], vm$spend_col[i])
    }
    if (!is.na(vm$support_col[i]) && vm$support_col[i] %in% names(dt)) {
      support_internal <- paste0("__support__", v)
      out[[support_internal]] <- pb_force_numeric_vec(dt[[vm$support_col[i]]], vm$support_col[i])
    }
    map_rows[[v]] <- data.table::data.table(
      variable = v,
      modeled_x_internal = v,
      spend_internal = spend_internal,
      support_internal = support_internal
    )
  }

  for (bc in base_cols) {
    if (!bc %in% names(dt)) stop("base_col missing from input_data: ", bc)
    out[[bc]] <- pb_force_numeric_vec(dt[[bc]], bc)
  }
  for (cc in control_cols) {
    if (!cc %in% names(dt)) stop("control_col missing from input_data: ", cc)
    out[[cc]] <- pb_force_numeric_vec(dt[[cc]], cc)
  }

  map_dt <- data.table::rbindlist(map_rows, use.names = TRUE, fill = TRUE)

  if (isTRUE(total_level_shared_curve) && anyDuplicated(out$date__) > 0) {
    additive_cols <- unique(c("y__", vm$variable, map_dt$spend_internal, map_dt$support_internal))
    additive_cols <- additive_cols[!is.na(additive_cols) & additive_cols %in% names(out)]

    non_additive_cols <- unique(c(base_cols, control_cols))
    non_additive_cols <- non_additive_cols[!is.na(non_additive_cols) & non_additive_cols %in% names(out)]
    non_additive_cols <- setdiff(non_additive_cols, additive_cols)

    out_add <- out[, lapply(.SD, sum, na.rm = TRUE), by = date__, .SDcols = additive_cols]
    if (length(non_additive_cols)) {
      if (identical(duplicate_date_non_additive_strategy, "first")) {
        out_non <- out[, lapply(.SD, function(z) z[which(!is.na(z))[1] %||% 1L]), by = date__, .SDcols = non_additive_cols]
      } else {
        out_non <- out[, lapply(.SD, mean, na.rm = TRUE), by = date__, .SDcols = non_additive_cols]
      }
      out <- merge(out_add, out_non, by = "date__", all = TRUE)
    } else {
      out <- out_add
    }
  }

  data.table::setorder(out, date__)
  attr(out, "map_dt") <- map_dt
  out[]
}

pb_apply_holdout_filter <- function(dt,
                                    date_col,
                                    holdout_col = NULL,
                                    holdout_value = TRUE,
                                    holdout_last_n = 0L) {
  out <- data.table::as.data.table(data.table::copy(dt))
  n_input <- nrow(out)
  is_holdout <- rep(FALSE, n_input)
  if (!is.null(holdout_col) && nzchar(as.character(holdout_col)[1])) {
    holdout_col <- as.character(holdout_col)[1]
    if (!holdout_col %in% names(out)) stop("holdout_col not found in input_data: ", holdout_col)
    hv <- out[[holdout_col]]
    if (is.logical(hv) && identical(holdout_value, TRUE)) {
      is_holdout <- is_holdout | (hv %in% TRUE)
    } else {
      is_holdout <- is_holdout | (as.character(hv) %in% as.character(holdout_value))
    }
  }
  holdout_last_n <- as.integer(holdout_last_n %||% 0L)[1]
  if (is.finite(holdout_last_n) && holdout_last_n > 0L) {
    d <- pb_parse_date(out[[date_col]], date_col)
    holdout_dates <- utils::tail(sort(unique(d[!is.na(d)])), holdout_last_n)
    is_holdout <- is_holdout | (d %in% holdout_dates)
  }
  out[, is_prior_training__ := !is_holdout]
  if (!any(out$is_prior_training__)) stop("All rows are holdout. At least one training row is required for prior building.")
  audit <- data.table::data.table(
    input_row_n = n_input,
    training_row_n = sum(out$is_prior_training__),
    holdout_row_n = sum(!out$is_prior_training__),
    holdout_col = as.character(holdout_col %||% NA_character_),
    holdout_value = paste(as.character(holdout_value), collapse = "|"),
    holdout_last_n = as.integer(holdout_last_n %||% 0L)
  )
  list(data = out[is_prior_training__ == TRUE][, is_prior_training__ := NULL][], audit = audit)
}

pb_get_bound <- function(coef_bounds, variable, default = "free") {
  if (is.null(coef_bounds)) return(default)
  cb <- data.table::as.data.table(data.table::copy(coef_bounds))
  if (!all(c("variable", "coef_bound") %in% names(cb))) return(default)
  target_variable <- as.character(variable)[1]
  z <- cb[as.character(get("variable")) == target_variable]
  if (!nrow(z)) return(default)
  as.character(z[["coef_bound"]][1])
}

pb_fixed_rrate <- function(fixed_rrate_by_var, variable) {
  if (is.null(fixed_rrate_by_var)) return(NA_real_)
  fr <- data.table::as.data.table(data.table::copy(fixed_rrate_by_var))
  if (!all(c("variable", "rrate") %in% names(fr))) return(NA_real_)
  target_variable <- as.character(variable)[1]
  z <- fr[as.character(get("variable")) == target_variable]
  if (!nrow(z)) return(NA_real_)
  as.numeric(z[["rrate"]][1])
}

pb_impute_linear <- function(x, fill_all_missing = 0) {
  x <- suppressWarnings(as.numeric(x))
  ok <- is.finite(x)
  if (all(ok)) return(x)
  if (!any(ok)) {
    fill <- suppressWarnings(as.numeric(fill_all_missing)[1])
    if (!is.finite(fill)) fill <- 0
    return(rep(fill, length(x)))
  }
  if (sum(ok) == 1L) {
    x[!ok] <- x[ok][1]
    return(x)
  }
  stats::approx(x = which(ok), y = x[ok], xout = seq_along(x), rule = 2)$y
}

pb_missing_data_report <- function(dt, cols, policy = "warn_keep") {
  cols <- unique(cols[cols %in% names(dt)])
  if (!length(cols)) return(data.table::data.table())
  data.table::rbindlist(lapply(cols, function(cc) {
    z <- suppressWarnings(as.numeric(dt[[cc]]))
    miss <- !is.finite(z) | is.na(z)
    data.table::data.table(
      column = cc,
      missing_n = sum(miss),
      missing_share = mean(miss),
      nonzero_n = sum(is.finite(z) & z != 0, na.rm = TRUE),
      missing_data_policy = policy
    )
  }), use.names = TRUE, fill = TRUE)
}

pb_apply_missing_data_policy <- function(model_dt,
                                         predictor_cols,
                                         control_cols = character(),
                                         policy = c("warn_keep", "linear_interpolate", "zero_fill", "drop_rows")) {
  policy <- match.arg(policy)
  dt <- data.table::as.data.table(data.table::copy(model_dt))
  predictor_cols <- unique(predictor_cols[predictor_cols %in% names(dt)])
  control_cols <- unique(control_cols[control_cols %in% names(dt)])
  cols <- unique(c("y__", predictor_cols, control_cols))
  before <- pb_missing_data_report(dt, cols, policy = policy)

  if (identical(policy, "warn_keep")) {
    if (nrow(before) && any(before$missing_n > 0)) {
      warning("Missing values detected in prior_builder inputs. They are being kept; coefficient fits use complete cases and adstock treats non-finite media as zero. Set missing_data_policy = 'linear_interpolate', 'zero_fill', or 'drop_rows' for explicit handling.")
    }
  } else if (identical(policy, "linear_interpolate")) {
    for (cc in unique(c(predictor_cols, control_cols))) {
      dt[, (cc) := pb_impute_linear(get(cc), fill_all_missing = 0)]
    }
  } else if (identical(policy, "zero_fill")) {
    for (cc in unique(c(predictor_cols, control_cols))) {
      dt[!is.finite(get(cc)) | is.na(get(cc)), (cc) := 0]
    }
  } else if (identical(policy, "drop_rows")) {
    keep_cols <- unique(c("y__", predictor_cols, control_cols))
    keep_cols <- keep_cols[keep_cols %in% names(dt)]
    keep <- stats::complete.cases(dt[, ..keep_cols])
    dt <- dt[keep]
  }

  after <- pb_missing_data_report(dt, cols, policy = policy)
  if (nrow(before)) {
    before <- before[, .(
      column,
      missing_data_policy,
      missing_n_before = missing_n,
      missing_share_before = missing_share,
      nonzero_n_before = nonzero_n
    )]
    after <- after[, .(
      column,
      missing_data_policy,
      missing_n_after = missing_n,
      missing_share_after = missing_share,
      nonzero_n_after = nonzero_n
    )]
    diag <- before[after, on = c("column", "missing_data_policy")]
    diag[, missing_action := policy]
  } else {
    diag <- data.table::data.table()
  }
  list(data = dt[], diagnostics = diag[])
}

pb_missing_data_guard_one <- function(modeled_before = NA_real_,
                                      modeled_after = NA_real_,
                                      spend_before = NA_real_,
                                      spend_after = NA_real_,
                                      support_before = NA_real_,
                                      support_after = NA_real_,
                                      policy = "warn_keep") {
  before <- suppressWarnings(as.numeric(c(modeled_before, spend_before, support_before)))
  after <- suppressWarnings(as.numeric(c(modeled_after, spend_after, support_after)))
  max_before <- if (any(is.finite(before))) max(before[is.finite(before)], na.rm = TRUE) else 0
  max_after <- if (any(is.finite(after))) max(after[is.finite(after)], na.rm = TRUE) else 0

  if (!is.finite(max_before) || max_before <= 0) {
    return(data.table::data.table(
      missing_data_class = "no_missing_detected",
      missing_data_action = policy,
      max_missing_share_before = 0,
      max_missing_share_after = max_after,
      missing_data_risk_multiplier = 1
    ))
  }

  severity <- if (max_before >= 0.30) {
    "high"
  } else if (max_before >= 0.15) {
    "medium"
  } else {
    "low"
  }
  base_risk <- switch(
    severity,
    low = 2,
    medium = 4,
    high = 8,
    4
  )
  if (identical(policy, "warn_keep") && is.finite(max_after) && max_after > 0) {
    base_risk <- max(base_risk, if (max_after >= 0.30) 12 else if (max_after >= 0.15) 8 else 4)
    klass <- paste0("missing_kept_", severity)
  } else if (identical(policy, "linear_interpolate")) {
    klass <- paste0("missing_interpolated_", severity)
  } else if (identical(policy, "zero_fill")) {
    base_risk <- max(base_risk, if (max_before >= 0.15) 6 else 3)
    klass <- paste0("missing_zero_filled_", severity)
  } else if (identical(policy, "drop_rows")) {
    klass <- paste0("missing_rows_dropped_", severity)
  } else {
    klass <- paste0("missing_handled_", severity)
  }
  if (is.finite(max_after) && max_after > 0 && !identical(policy, "warn_keep")) {
    klass <- paste0(klass, "_remaining")
    base_risk <- max(base_risk, 8)
  }
  data.table::data.table(
    missing_data_class = klass,
    missing_data_action = policy,
    max_missing_share_before = max_before,
    max_missing_share_after = max_after,
    missing_data_risk_multiplier = base_risk
  )
}

pb_anchor_for <- function(curve_anchors, variable, default = 0.50) {
  info <- pb_anchor_info_for(curve_anchors, variable)
  val <- info$anchor_saturation_handoff %||% info$anchor_saturation
  if (!is.finite(val)) default else pb_clip(val, 0.05, 0.95)
}

pb_anchor_info_for <- function(curve_anchors, variable) {
  empty <- list(
    anchor_saturation = NA_real_,
    anchor_saturation_handoff = NA_real_,
    cvalue_multiplier = NA_real_,
    cvalue_multiplier_handoff = NA_real_,
    anchor_should_drive_curve_prior = NA,
    anchor_authority_tier = NA_character_,
    anchor_actionability_tier = NA_character_,
    anchor_source = NA_character_,
    anchor_weight_final = NA_real_,
    reliability = NA_real_,
    anchor_uncertainty_width_90 = NA_real_,
    anchor_lower_90 = NA_real_,
    anchor_upper_90 = NA_real_
  )
  if (is.null(curve_anchors)) return(empty)
  ca <- data.table::as.data.table(data.table::copy(curve_anchors))
  if (!"variable" %in% names(ca)) return(empty)
  val_col <- if ("anchor_saturation_handoff" %in% names(ca)) "anchor_saturation_handoff" else if ("anchor_saturation" %in% names(ca)) "anchor_saturation" else NA_character_
  target_variable <- as.character(variable)[1]
  z <- ca[as.character(get("variable")) == target_variable]
  if (!nrow(z)) return(empty)
  get_num <- function(nm) if (nm %in% names(z)) as.numeric(z[[nm]][1]) else NA_real_
  get_chr <- function(nm) if (nm %in% names(z)) as.character(z[[nm]][1]) else NA_character_
  get_lgl <- function(nm) {
    if (!(nm %in% names(z))) return(NA)
    x <- z[[nm]][1]
    if (is.logical(x)) return(ifelse(is.na(x), NA, x))
    y <- tolower(trimws(as.character(x)))
    if (y %in% c("true", "t", "yes", "y", "1")) TRUE else if (y %in% c("false", "f", "no", "n", "0")) FALSE else NA
  }
  out <- empty
  if (!is.na(val_col)) {
    out$anchor_saturation <- get_num(val_col)
    out$anchor_saturation_handoff <- get_num(val_col)
  }
  out$cvalue_multiplier <- get_num("cvalue_multiplier")
  out$cvalue_multiplier_handoff <- get_num("cvalue_multiplier_handoff")
  out$anchor_should_drive_curve_prior <- get_lgl("anchor_should_drive_curve_prior")
  out$anchor_authority_tier <- get_chr("anchor_authority_tier")
  out$anchor_actionability_tier <- get_chr("anchor_actionability_tier")
  out$anchor_weight_final <- get_num("anchor_weight_final")
  out$reliability <- get_num("reliability")
  out$anchor_uncertainty_width_90 <- get_num("anchor_uncertainty_width_90")
  out$anchor_lower_90 <- get_num("anchor_lower_90")
  out$anchor_upper_90 <- get_num("anchor_upper_90")
  out$anchor_source <- get_chr("anchor_source")
  out
}

pb_anchor_cvalue_precision_multiplier <- function(anchor_info,
                                                  actionable_multiplier = 6,
                                                  directional_multiplier = 2,
                                                  max_multiplier = 10) {
  if (is.null(anchor_info)) return(1)
  actionability <- tolower(trimws(as.character(anchor_info$anchor_actionability_tier %||% "")))
  should_drive <- isTRUE(anchor_info$anchor_should_drive_curve_prior)
  weight <- as.numeric(anchor_info$anchor_weight_final %||% NA_real_)
  if (!is.finite(weight)) weight <- if (should_drive) 0.60 else if (identical(actionability, "directional")) 0.35 else 0
  mult <- 1
  if (should_drive || identical(actionability, "actionable")) {
    mult <- actionable_multiplier * max(weight, 0.50)
  } else if (identical(actionability, "directional")) {
    mult <- directional_multiplier * max(weight, 0.25)
  }
  pb_clip(mult, 1, max_multiplier)
}

pb_detect_spend_ramps <- function(x_raw,
                                  min_abs_change_index = 0.25,
                                  min_pct_change = 0.20,
                                  window = 2L) {
  x <- pmax(suppressWarnings(as.numeric(x_raw)), 0)
  n <- length(x)
  empty <- data.table::data.table(
    ramp_flag = rep(FALSE, n),
    ramp_core = rep(FALSE, n),
    abs_change_index = rep(NA_real_, n),
    pct_change = rep(NA_real_, n),
    ramp_n = 0L,
    ramp_share = 0,
    ramp_core_n = 0L,
    ramp_variation_score = NA_real_
  )
  if (n < 3L || all(!is.finite(x))) return(empty)

  active <- is.finite(x) & x > 0
  med_active <- stats::median(x[active], na.rm = TRUE)
  if (!is.finite(med_active) || med_active <= 0) return(empty)

  dx <- c(NA_real_, diff(x))
  prev <- c(NA_real_, x[-n])
  abs_change_index <- abs(dx) / med_active
  pct_denom <- pmax(abs(prev), 0.10 * med_active, 1e-8)
  pct_change <- abs(dx) / pct_denom
  ramp_core <- is.finite(abs_change_index) &
    is.finite(pct_change) &
    (abs_change_index >= min_abs_change_index | pct_change >= min_pct_change) &
    (x > 0 | c(FALSE, x[-n] > 0))
  ramp_core[is.na(ramp_core)] <- FALSE

  ramp_flag <- ramp_core
  w <- max(0L, as.integer(window)[1])
  if (w > 0L && any(ramp_core)) {
    core_idx <- which(ramp_core)
    expanded <- unique(unlist(lapply(core_idx, function(ii) seq.int(max(1L, ii - w), min(n, ii + w)))))
    ramp_flag[expanded] <- TRUE
  }

  ramp_n <- sum(ramp_flag)
  data.table::data.table(
    ramp_flag = ramp_flag,
    ramp_core = ramp_core,
    abs_change_index = abs_change_index,
    pct_change = pct_change,
    ramp_n = ramp_n,
    ramp_share = ramp_n / n,
    ramp_core_n = sum(ramp_core),
    ramp_variation_score = if (any(ramp_core)) stats::median(abs_change_index[ramp_core], na.rm = TRUE) else NA_real_
  )
}

pb_fit_for_rrate <- function(y_mi,
                             x_raw,
                             controls,
                             rrate,
                             anchor_saturation,
                             dvalue,
                             bound,
                             wrong_sign_rrate_penalty = 100,
                             cvalue_override = NA_real_,
                             score_weights = NULL,
                             cvalue_anchor_method = c("industry_hybrid", "median_active", "industry_default", "active_quantile"),
                             cvalue_industry_half_saturation = 0.80,
                             cvalue_active_quantile = 0.75,
                             cvalue_hybrid_industry_weight = NA_real_,
                             curve_type = "weibull") {
  cvalue_anchor_method <- match.arg(cvalue_anchor_method)
  curve_type <- pb_normalize_curve_type(curve_type)[1]
  x_ad_raw <- pb_adstock(x_raw, rrate)
  x_ad_mi <- pb_mean_index(x_ad_raw)
  active_mask <- is.finite(x_raw) & as.numeric(x_raw) > 0
  anchor_meta <- pb_calc_cvalue_with_anchor(
    x_ad_mi,
    anchor_saturation = anchor_saturation,
    dvalue = dvalue,
    active_mask = active_mask,
    cvalue_anchor_method = cvalue_anchor_method,
    cvalue_industry_half_saturation = cvalue_industry_half_saturation,
    cvalue_active_quantile = cvalue_active_quantile,
    cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
    curve_type = curve_type
  )
  cvalue <- if (is.finite(cvalue_override)) as.numeric(cvalue_override)[1] else anchor_meta$cvalue
  if (!is.finite(cvalue)) return(list(score = Inf, coef = NA_real_, cvalue = NA_real_, x_handoff = rep(NA_real_, length(x_raw))))
  x_sat <- pb_saturation(x_ad_mi, cvalue, dvalue, curve_type = curve_type)
  x_handoff <- pb_mean_index(x_sat)

  X <- cbind(intercept = 1, controls, x_handoff = x_handoff)
  ok <- stats::complete.cases(y_mi, X)
  if (sum(ok) < max(8L, ncol(X) + 3L)) return(list(score = Inf, coef = NA_real_, cvalue = cvalue, x_handoff = x_handoff))

  w <- rep(1, length(y_mi))
  if (!is.null(score_weights)) {
    w0 <- suppressWarnings(as.numeric(score_weights))
    if (length(w0) == length(y_mi)) {
      w[is.finite(w0) & w0 > 0] <- w0[is.finite(w0) & w0 > 0]
    }
  }
  fit <- if (any(abs(w[ok] - 1) > 1e-8)) {
    stats::lm.wfit(x = X[ok, , drop = FALSE], y = y_mi[ok], w = w[ok])
  } else {
    stats::lm.fit(x = X[ok, , drop = FALSE], y = y_mi[ok])
  }
  coefs <- fit$coefficients
  coef_x <- unname(coefs[length(coefs)])
  coefs_pred <- coefs
  coefs_pred[!is.finite(coefs_pred)] <- 0
  pred <- as.numeric(X[ok, , drop = FALSE] %*% coefs_pred)
  err2 <- (y_mi[ok] - pred) ^ 2
  rmse_unweighted <- sqrt(mean(err2, na.rm = TRUE))
  rmse <- sqrt(stats::weighted.mean(err2, w = w[ok], na.rm = TRUE))
  if (!is.finite(rmse)) rmse <- Inf
  if (!is.finite(rmse_unweighted)) rmse_unweighted <- Inf

  wrong_sign <- FALSE
  if (bound %in% c("pos", "positive", "+") && is.finite(coef_x) && coef_x < 0) wrong_sign <- TRUE
  if (bound %in% c("neg", "negative", "-") && is.finite(coef_x) && coef_x > 0) wrong_sign <- TRUE
  if (isTRUE(wrong_sign)) {
    rmse <- if (is.finite(wrong_sign_rrate_penalty)) rmse * wrong_sign_rrate_penalty else Inf
  }

  list(
    score = rmse,
    rmse = rmse_unweighted,
    coef = as.numeric(coef_x),
    cvalue = cvalue,
    cvalue_anchor_x = anchor_meta$cvalue_anchor_x,
    cvalue_anchor_method = anchor_meta$cvalue_anchor_method,
    cvalue_anchor_data_weight = anchor_meta$cvalue_anchor_data_weight,
    cvalue_anchor_active_cv = anchor_meta$cvalue_anchor_active_cv,
    x_handoff = x_handoff
  )
}

pb_curve_handoff <- function(x_raw,
                             rrate,
                             anchor_saturation,
                             dvalue,
                             cvalue = NA_real_,
                             cvalue_anchor_method = c("industry_hybrid", "median_active", "industry_default", "active_quantile"),
                             cvalue_industry_half_saturation = 0.80,
                             cvalue_active_quantile = 0.75,
                             cvalue_hybrid_industry_weight = NA_real_,
                             curve_type = "weibull") {
  cvalue_anchor_method <- match.arg(cvalue_anchor_method)
  curve_type <- pb_normalize_curve_type(curve_type)[1]
  x_raw <- suppressWarnings(as.numeric(x_raw))
  x_ad_raw <- pb_adstock(x_raw, rrate)
  x_ad_mi <- pb_mean_index(x_ad_raw)
  if (!is.finite(cvalue)) {
    active_mask <- is.finite(x_raw) & x_raw > 0
    meta <- pb_calc_cvalue_with_anchor(
      x_ad_mi,
      anchor_saturation = anchor_saturation,
      dvalue = dvalue,
      active_mask = active_mask,
      cvalue_anchor_method = cvalue_anchor_method,
      cvalue_industry_half_saturation = cvalue_industry_half_saturation,
      cvalue_active_quantile = cvalue_active_quantile,
      cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
      curve_type = curve_type
    )
    cvalue <- meta$cvalue
  }
  if (!is.finite(cvalue)) return(rep(NA_real_, length(x_raw)))
  pb_mean_index(pb_saturation(x_ad_mi, cvalue, dvalue, curve_type = curve_type))
}

pb_clean_sanity_bounds <- function(sanity_bounds = NULL) {
  if (is.null(sanity_bounds)) return(NULL)
  sb <- data.table::as.data.table(data.table::copy(sanity_bounds))
  if (!"variable" %in% names(sb)) stop("sanity_bounds must contain variable.")
  sb[, variable := as.character(variable)]
  sb
}

pb_sanity_bound_for <- function(bounds_dt, var_name, col, default = NA_real_) {
  if (is.null(bounds_dt) || !nrow(bounds_dt) || !(col %in% names(bounds_dt))) return(default)
  z <- bounds_dt[as.character(bounds_dt[["variable"]]) == as.character(var_name)]
  if (!nrow(z)) return(default)
  val <- suppressWarnings(as.numeric(z[[col]][1]))
  if (is.finite(val)) val else default
}

pb_curve_sanity_check_one <- function(variable,
                                      y_raw,
                                      x_raw,
                                      x_handoff,
                                      coef,
                                      spend,
                                      has_curve,
                                      rrate = NA_real_,
                                      cvalue = NA_real_,
                                      dvalue = 1,
                                      anchor_saturation = 0.50,
                                      sanity_bounds = NULL,
                                      default_max_abs_contribution_share = 0.75,
                                      default_max_abs_elasticity = 3.0,
                                      cvalue_anchor_method = "industry_hybrid",
                                      cvalue_industry_half_saturation = 0.80,
                                      cvalue_active_quantile = 0.75,
                                      cvalue_hybrid_industry_weight = NA_real_,
                                      curve_type = "weibull") {
  y_raw <- suppressWarnings(as.numeric(y_raw))
  x_raw <- suppressWarnings(as.numeric(x_raw))
  x_handoff <- suppressWarnings(as.numeric(x_handoff))
  coef <- suppressWarnings(as.numeric(coef)[1])
  spend <- suppressWarnings(as.numeric(spend))

  y_total <- sum(y_raw[is.finite(y_raw)], na.rm = TRUE)
  y_mean <- mean(y_raw[is.finite(y_raw)], na.rm = TRUE)
  spend_total <- sum(pmax(spend, 0), na.rm = TRUE)
  contrib_model <- coef * x_handoff
  contrib_total <- if (is.finite(y_mean)) sum(contrib_model, na.rm = TRUE) * y_mean else NA_real_
  contrib_share <- if (is.finite(y_total) && abs(y_total) > 1e-8) contrib_total / y_total else NA_real_
  outcome_per_cost <- if (is.finite(spend_total) && spend_total > 1e-8 && is.finite(contrib_total)) contrib_total / spend_total else NA_real_
  cost_per_outcome <- if (is.finite(contrib_total) && abs(contrib_total) > 1e-8 && is.finite(spend_total)) spend_total / contrib_total else NA_real_

  x_up <- x_raw * 1.01
  x_up_handoff <- if (isTRUE(has_curve)) {
    pb_curve_handoff(
      x_up,
      rrate = rrate,
      anchor_saturation = anchor_saturation,
      dvalue = dvalue,
      cvalue = cvalue,
      cvalue_anchor_method = cvalue_anchor_method,
      cvalue_industry_half_saturation = cvalue_industry_half_saturation,
      cvalue_active_quantile = cvalue_active_quantile,
      cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
      curve_type = curve_type
    )
  } else {
    pb_mean_index(x_up)
  }
  base_sum <- sum(contrib_model, na.rm = TRUE)
  up_sum <- sum(coef * x_up_handoff, na.rm = TRUE)
  elasticity <- if (is.finite(y_total) && abs(y_total) > 1e-8 && is.finite(y_mean)) {
    ((up_sum - base_sum) * y_mean / y_total) / 0.01
  } else {
    NA_real_
  }

  max_abs_contrib <- pb_sanity_bound_for(sanity_bounds, variable, "max_abs_contribution_share", default_max_abs_contribution_share)
  min_contrib <- pb_sanity_bound_for(sanity_bounds, variable, "min_contribution_share", NA_real_)
  max_contrib <- pb_sanity_bound_for(sanity_bounds, variable, "max_contribution_share", NA_real_)
  max_abs_elast <- pb_sanity_bound_for(sanity_bounds, variable, "max_abs_elasticity", default_max_abs_elasticity)
  min_elast <- pb_sanity_bound_for(sanity_bounds, variable, "min_elasticity", NA_real_)
  max_elast <- pb_sanity_bound_for(sanity_bounds, variable, "max_elasticity", NA_real_)
  min_cpo <- pb_sanity_bound_for(sanity_bounds, variable, "min_cost_per_outcome", NA_real_)
  max_cpo <- pb_sanity_bound_for(sanity_bounds, variable, "max_cost_per_outcome", NA_real_)
  min_opc <- pb_sanity_bound_for(sanity_bounds, variable, "min_outcome_per_cost", NA_real_)
  max_opc <- pb_sanity_bound_for(sanity_bounds, variable, "max_outcome_per_cost", NA_real_)

  flags <- character()
  if (is.finite(contrib_share) && is.finite(max_abs_contrib) && abs(contrib_share) > max_abs_contrib) flags <- c(flags, "abs_contribution_share")
  if (is.finite(contrib_share) && is.finite(min_contrib) && contrib_share < min_contrib) flags <- c(flags, "min_contribution_share")
  if (is.finite(contrib_share) && is.finite(max_contrib) && contrib_share > max_contrib) flags <- c(flags, "max_contribution_share")
  if (is.finite(elasticity) && is.finite(max_abs_elast) && abs(elasticity) > max_abs_elast) flags <- c(flags, "abs_elasticity")
  if (is.finite(elasticity) && is.finite(min_elast) && elasticity < min_elast) flags <- c(flags, "min_elasticity")
  if (is.finite(elasticity) && is.finite(max_elast) && elasticity > max_elast) flags <- c(flags, "max_elasticity")
  if (is.finite(cost_per_outcome) && is.finite(min_cpo) && cost_per_outcome < min_cpo) flags <- c(flags, "min_cost_per_outcome")
  if (is.finite(cost_per_outcome) && is.finite(max_cpo) && cost_per_outcome > max_cpo) flags <- c(flags, "max_cost_per_outcome")
  if (is.finite(outcome_per_cost) && is.finite(min_opc) && outcome_per_cost < min_opc) flags <- c(flags, "min_outcome_per_cost")
  if (is.finite(outcome_per_cost) && is.finite(max_opc) && outcome_per_cost > max_opc) flags <- c(flags, "max_outcome_per_cost")

  risk <- if (length(flags)) {
    if (any(flags %in% c("abs_contribution_share", "abs_elasticity"))) 10 else 6
  } else {
    1
  }
  data.table::data.table(
    variable = variable,
    implied_contribution_total = contrib_total,
    implied_contribution_share = contrib_share,
    implied_elasticity = elasticity,
    implied_spend_total = spend_total,
    implied_cost_per_outcome = cost_per_outcome,
    implied_outcome_per_cost = outcome_per_cost,
    sanity_bound_class = if (length(flags)) "outside_sanity_bounds" else "within_sanity_bounds",
    sanity_bound_flags = if (length(flags)) paste(unique(flags), collapse = "|") else "",
    sanity_bound_risk_multiplier = risk
  )
}

pb_estimate_rrate <- function(y_mi,
                              x_raw,
                              controls,
                              anchor_saturation,
                              dvalue,
                              bound,
                              rrate_bounds,
                              method,
                              grid_n,
                              wrong_sign_rrate_penalty = 100,
                              upper_bound_fallback = TRUE,
                              upper_bound_tolerance = 0.02,
                              plateau_rel_tol = 0.01,
                              plateau_abs_tol = 0.0001,
                              plateau_grid_n = NULL,
                              cvalue_anchor_method = c("industry_hybrid", "median_active", "industry_default", "active_quantile"),
                              cvalue_industry_half_saturation = 0.80,
                              cvalue_active_quantile = 0.75,
                              cvalue_hybrid_industry_weight = NA_real_,
                              curve_type = "weibull") {
  cvalue_anchor_method <- match.arg(cvalue_anchor_method)
  curve_type <- pb_normalize_curve_type(curve_type)[1]
  lo <- as.numeric(rrate_bounds[1]); hi <- as.numeric(rrate_bounds[2])
  if (!is.finite(lo)) lo <- 0
  if (!is.finite(hi)) hi <- 0.80
  lo <- pb_clip(lo, 0, 0.98); hi <- pb_clip(hi, lo + 1e-6, 0.99)
  attach_diag <- function(val, diag) {
    val <- as.numeric(val)[1]
    attr(val, "rrate_diagnostics") <- diag
    val
  }

  score_fun <- function(r) {
    pb_fit_for_rrate(
      y_mi, x_raw, controls, r, anchor_saturation, dvalue, bound, wrong_sign_rrate_penalty,
      cvalue_anchor_method = cvalue_anchor_method,
      cvalue_industry_half_saturation = cvalue_industry_half_saturation,
      cvalue_active_quantile = cvalue_active_quantile,
      cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
      curve_type = curve_type
    )$score
  }
  grid_n_eff <- max(3L, as.integer(grid_n), as.integer(plateau_grid_n %||% grid_n))
  rr_grid <- seq(lo, hi, length.out = grid_n_eff)
  sc_grid <- vapply(rr_grid, score_fun, numeric(1))
  if (all(!is.finite(sc_grid))) {
    val <- stats::median(rr_grid, na.rm = TRUE)
    return(attach_diag(val, list(
      rrate_raw_best = val,
      rrate_selected = val,
      rrate_score_best = Inf,
      rrate_score_selected = Inf,
      rrate_score_upper = Inf,
      rrate_at_upper_bound = FALSE,
      rrate_plateau_adjusted = FALSE,
      rrate_selection_reason = "all_rrate_candidates_failed"
    )))
  }

  if (identical(method, "grid")) {
    raw_best <- rr_grid[which.min(sc_grid)]
    raw_score <- min(sc_grid, na.rm = TRUE)
  } else {
    opt <- stats::optimize(score_fun, interval = c(lo, hi))
    raw_best <- as.numeric(opt$minimum)
    raw_score <- as.numeric(opt$objective)
  }
  upper_score <- score_fun(hi)
  best_score <- min(c(raw_score, sc_grid, upper_score), na.rm = TRUE)
  span <- hi - lo
  near_upper <- is.finite(raw_best) && raw_best >= hi - max(0, as.numeric(upper_bound_tolerance)[1]) * span
  upper_indistinguishable <- is.finite(upper_score) && upper_score <= best_score * (1 + max(0, as.numeric(plateau_rel_tol)[1])) + max(0, as.numeric(plateau_abs_tol)[1])
  at_upper <- near_upper || isTRUE(upper_indistinguishable && which.min(sc_grid) == length(sc_grid))

  selected <- raw_best
  selected_score <- raw_score
  adjusted <- FALSE
  reason <- if (at_upper) "raw_best_at_or_near_upper_bound" else "raw_best"
  if (isTRUE(upper_bound_fallback) && isTRUE(at_upper) && is.finite(upper_score)) {
    threshold <- upper_score * (1 + max(0, as.numeric(plateau_rel_tol)[1])) + max(0, as.numeric(plateau_abs_tol)[1])
    ok <- is.finite(sc_grid) & sc_grid <= threshold
    if (any(ok)) {
      selected <- min(rr_grid[ok], na.rm = TRUE)
      selected_score <- sc_grid[match(selected, rr_grid)]
      adjusted <- is.finite(selected) && is.finite(raw_best) && selected < raw_best - 1e-8
      reason <- if (adjusted) "upper_bound_plateau_smallest_indistinguishable_rrate" else "upper_bound_plateau_kept_raw_best"
    }
  }

  attach_diag(selected, list(
    rrate_raw_best = raw_best,
    rrate_selected = selected,
    rrate_score_best = raw_score,
    rrate_score_selected = selected_score,
    rrate_score_upper = upper_score,
    rrate_at_upper_bound = at_upper,
    rrate_plateau_adjusted = adjusted,
    rrate_selection_reason = reason
  ))
}

pb_estimate_cvalue_from_data <- function(y_mi,
                                         x_raw,
                                         controls,
                                         rrate,
                                         anchor_saturation,
                                         dvalue,
                                         bound,
                                         wrong_sign_rrate_penalty = 100,
                                         mode = c("auto", "never", "always"),
                                         search_multipliers = c(0.35, 3.0),
                                         search_bounds = c(0.05, 8.0),
                                         grid_n = 31L,
                                         min_score_improvement = 0.02,
                                         anchor_penalty = 0.01,
                                         ramp_min_abs_change_index = 0.25,
                                         ramp_min_pct_change = 0.20,
                                         ramp_window = 2L,
                                         ramp_weight = 4,
                                         min_ramp_points = 8L,
                                         min_ramp_share = 0.08,
                                         cvalue_anchor_method = c("industry_hybrid", "median_active", "industry_default", "active_quantile"),
                                         cvalue_industry_half_saturation = 0.80,
                                         cvalue_active_quantile = 0.75,
                                         cvalue_hybrid_industry_weight = NA_real_,
                                         curve_type = "weibull",
                                         use_observed_diminishing_returns = TRUE,
                                         observed_diminishing_min_obs_per_bin = 8L,
                                         flatten_when_no_observed_diminishing = TRUE,
                                         flat_cvalue_multiplier = 0.15,
                                         flat_cvalue_rel_tol = 0.02,
                                         flat_cvalue_abs_tol = 0.001,
                                         extra_cvalue_candidates = NULL,
                                         preferred_cvalue = NA_real_,
                                         preferred_cvalue_source = "observational_cvalue_candidate",
                                         preferred_cvalue_reliability = NA_real_,
                                         preferred_cvalue_rel_tol = 0.03,
                                         preferred_cvalue_abs_tol = 0.001) {
  mode <- match.arg(mode)
  cvalue_anchor_method <- match.arg(cvalue_anchor_method)
  curve_type <- pb_normalize_curve_type(curve_type)[1]
  anchor_fit <- pb_fit_for_rrate(
    y_mi, x_raw, controls, rrate, anchor_saturation, dvalue, bound,
    wrong_sign_rrate_penalty = wrong_sign_rrate_penalty,
    cvalue_anchor_method = cvalue_anchor_method,
    cvalue_industry_half_saturation = cvalue_industry_half_saturation,
    cvalue_active_quantile = cvalue_active_quantile,
    cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
    curve_type = curve_type
  )
  anchor_cvalue <- anchor_fit$cvalue
  ramp <- pb_detect_spend_ramps(
    x_raw,
    min_abs_change_index = ramp_min_abs_change_index,
    min_pct_change = ramp_min_pct_change,
    window = ramp_window
  )
  ramp_n <- if (nrow(ramp)) ramp$ramp_n[1] else 0L
  ramp_share <- if (nrow(ramp)) ramp$ramp_share[1] else 0
  ramp_score <- if (nrow(ramp)) ramp$ramp_variation_score[1] else NA_real_
  dim_ret <- if (isTRUE(use_observed_diminishing_returns)) {
    pb_observed_diminishing_returns(
      y_mi = y_mi,
      x_raw = x_raw,
      controls = controls,
      rrate = rrate,
      dvalue = dvalue,
      curve_type = curve_type,
      min_obs_per_bin = observed_diminishing_min_obs_per_bin,
      cvalue_bounds = search_bounds
    )
  } else {
    pb_observed_diminishing_returns(numeric(0), numeric(0), controls, NA_real_)
  }
  flat_signal <- isTRUE(flatten_when_no_observed_diminishing) &&
    identical(dim_ret$observed_curve_evidence_class[1], "weak_or_flat_diminishing_returns_signal")
  extra_cvalue_candidates <- suppressWarnings(as.numeric(extra_cvalue_candidates %||% numeric()))
  extra_cvalue_candidates <- extra_cvalue_candidates[is.finite(extra_cvalue_candidates) & extra_cvalue_candidates > 0]
  preferred_cvalue <- suppressWarnings(as.numeric(preferred_cvalue)[1])
  preferred_cvalue_reliability <- suppressWarnings(as.numeric(preferred_cvalue_reliability)[1])
  if (!is.finite(preferred_cvalue_reliability)) preferred_cvalue_reliability <- 0
  preferred_cvalue_reliability <- pb_clip(preferred_cvalue_reliability, 0, 1)
  has_preferred_cvalue <- is.finite(preferred_cvalue) && preferred_cvalue > 0
  preferred_conflicts_total_diminishing <- isTRUE(has_preferred_cvalue) &&
    grepl("pooled_group_ramp_pooled_supports_flatter_curve", as.character(preferred_cvalue_source %||% "")) &&
    identical(dim_ret$observed_curve_evidence_class[1], "supports_diminishing_returns") &&
    is.finite(dim_ret$observed_slope_cvalue[1]) &&
    dim_ret$observed_slope_cvalue[1] > preferred_cvalue * 1.25
  if (isTRUE(preferred_conflicts_total_diminishing)) {
    preferred_cvalue_reliability <- min(preferred_cvalue_reliability, 0.25)
    preferred_cvalue_source <- paste0(as.character(preferred_cvalue_source %||% "pooled_group_ramp"), "_total_diminishing_signal_conflict")
  }

  no_override <- function(source, reason) {
    list(
      fit = anchor_fit,
      cvalue_anchor = anchor_cvalue,
      cvalue_data_driven = NA_real_,
      cvalue_final_source = source,
      cvalue_data_reason = reason,
      cvalue_data_score_anchor = anchor_fit$score,
      cvalue_data_score_best = NA_real_,
      cvalue_data_improvement = 0,
      cvalue_data_multiplier = 1,
      cvalue_flat_candidate = NA_real_,
      cvalue_flat_candidate_used = FALSE,
      preferred_cvalue = if (has_preferred_cvalue) preferred_cvalue else NA_real_,
      preferred_cvalue_used = FALSE,
      preferred_cvalue_source = as.character(preferred_cvalue_source %||% NA_character_),
      preferred_cvalue_reliability = preferred_cvalue_reliability,
      cvalue_anchor_x = anchor_fit$cvalue_anchor_x,
      cvalue_anchor_method = anchor_fit$cvalue_anchor_method,
      cvalue_anchor_data_weight = anchor_fit$cvalue_anchor_data_weight,
      cvalue_anchor_active_cv = anchor_fit$cvalue_anchor_active_cv,
      ramp_flag = if (nrow(ramp)) ramp$ramp_flag else rep(FALSE, length(x_raw)),
      ramp_n = ramp_n,
      ramp_share = ramp_share,
      ramp_variation_score = ramp_score,
      observed_curve_evidence_class = dim_ret$observed_curve_evidence_class[1],
      observed_marginal_slope_low = dim_ret$observed_marginal_slope_low[1],
      observed_marginal_slope_high = dim_ret$observed_marginal_slope_high[1],
      observed_marginal_slope_ratio = dim_ret$observed_marginal_slope_ratio[1],
      observed_slope_cvalue = dim_ret$observed_slope_cvalue[1],
      observed_diminishing_returns_score = dim_ret$observed_diminishing_returns_score[1],
      observed_low_spend_n = dim_ret$observed_low_spend_n[1],
      observed_high_spend_n = dim_ret$observed_high_spend_n[1],
      observed_low_spend_x = dim_ret$observed_low_spend_x[1],
      observed_high_spend_x = dim_ret$observed_high_spend_x[1],
      observed_spend_spread_p90_p10 = dim_ret$observed_spend_spread_p90_p10[1]
    )
  }

  if (!is.finite(anchor_cvalue) || identical(mode, "never")) {
    return(no_override("anchor_50_sat", if (identical(mode, "never")) "data_driven_cvalue_disabled" else "invalid_anchor_cvalue"))
  }
  if ((ramp_n < min_ramp_points || !is.finite(ramp_share) || ramp_share < min_ramp_share) &&
      !length(extra_cvalue_candidates) && !isTRUE(has_preferred_cvalue)) {
    return(no_override("anchor_50_sat", "insufficient_meaningful_spend_ramps"))
  }

  lo_mult <- max(min(search_multipliers, na.rm = TRUE), 1e-3)
  hi_mult <- max(search_multipliers, na.rm = TRUE)
  if (!is.finite(lo_mult) || !is.finite(hi_mult) || lo_mult >= hi_mult) {
    lo_mult <- 0.35; hi_mult <- 3
  }
  lo_abs <- min(search_bounds, na.rm = TRUE)
  hi_abs <- max(search_bounds, na.rm = TRUE)
  if (!is.finite(lo_abs) || !is.finite(hi_abs) || lo_abs >= hi_abs) {
    lo_abs <- 0.05; hi_abs <- 8
  }
  mult <- exp(seq(log(lo_mult), log(hi_mult), length.out = max(5L, as.integer(grid_n))))
  cand <- unique(pb_clip(anchor_cvalue * mult, lo_abs, hi_abs))
  if (isTRUE(use_observed_diminishing_returns) && is.finite(dim_ret$observed_slope_cvalue[1])) {
    cand <- unique(c(cand, pb_clip(dim_ret$observed_slope_cvalue[1], lo_abs, hi_abs)))
  }
  if (length(extra_cvalue_candidates)) {
    cand <- unique(c(cand, pb_clip(extra_cvalue_candidates, lo_abs, hi_abs)))
  }
  if (isTRUE(has_preferred_cvalue)) {
    preferred_cvalue <- pb_clip(preferred_cvalue, lo_abs, hi_abs)
    cand <- unique(c(cand, preferred_cvalue))
  }
  flat_candidate <- NA_real_
  if (isTRUE(flat_signal)) {
    flat_candidate <- pb_clip(anchor_cvalue * pb_clip(as.numeric(flat_cvalue_multiplier)[1], 0.02, 0.75), lo_abs, hi_abs)
    if (is.finite(flat_candidate)) cand <- unique(c(cand, flat_candidate))
  }
  cand <- sort(unique(c(cand, anchor_cvalue)))
  weights <- rep(1, length(x_raw))
  weights[ramp$ramp_flag == TRUE] <- 1 + max(0, as.numeric(ramp_weight)[1])

  rows <- lapply(cand, function(cv) {
    fit <- pb_fit_for_rrate(
      y_mi, x_raw, controls, rrate, anchor_saturation, dvalue, bound,
      wrong_sign_rrate_penalty = wrong_sign_rrate_penalty,
      cvalue_override = cv,
      score_weights = weights,
      cvalue_anchor_method = cvalue_anchor_method,
      cvalue_industry_half_saturation = cvalue_industry_half_saturation,
      cvalue_active_quantile = cvalue_active_quantile,
      cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
      curve_type = curve_type
    )
    multiplier <- cv / anchor_cvalue
    penalty <- as.numeric(anchor_penalty)[1] * abs(log(multiplier))
    data.table::data.table(
      cvalue = cv,
      multiplier = multiplier,
      score = fit$score,
      rmse = fit$rmse,
      penalized_score = fit$score * (1 + penalty)
    )
  })
  scores <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  scores <- scores[is.finite(penalized_score)]
  if (!nrow(scores)) return(no_override("anchor_50_sat", "all_cvalue_candidates_failed"))
  data.table::setorder(scores, penalized_score)
  best <- scores[1]
  selected <- best
  flat_used <- FALSE
  best_fit <- pb_fit_for_rrate(
    y_mi, x_raw, controls, rrate, anchor_saturation, dvalue, bound,
    wrong_sign_rrate_penalty = wrong_sign_rrate_penalty,
    cvalue_override = best$cvalue,
    score_weights = weights,
    cvalue_anchor_method = cvalue_anchor_method,
    cvalue_industry_half_saturation = cvalue_industry_half_saturation,
    cvalue_active_quantile = cvalue_active_quantile,
    cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
    curve_type = curve_type
  )
  anchor_weighted <- pb_fit_for_rrate(
    y_mi, x_raw, controls, rrate, anchor_saturation, dvalue, bound,
    wrong_sign_rrate_penalty = wrong_sign_rrate_penalty,
    cvalue_override = anchor_cvalue,
    score_weights = weights,
    cvalue_anchor_method = cvalue_anchor_method,
    cvalue_industry_half_saturation = cvalue_industry_half_saturation,
    cvalue_active_quantile = cvalue_active_quantile,
    cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
    curve_type = curve_type
  )
  improvement <- if (is.finite(anchor_weighted$score) && anchor_weighted$score > 0) {
    (anchor_weighted$score - best$score) / anchor_weighted$score
  } else {
    0
  }
  final_source <- "data_driven_ramp_weighted_grid"
  final_reason <- "meaningful_spend_ramps_and_fit_improvement"
  preferred_used <- FALSE
  if (isTRUE(flat_signal) && is.finite(anchor_weighted$score)) {
    flat_threshold <- min(best$score, anchor_weighted$score, na.rm = TRUE) *
      (1 + max(0, as.numeric(flat_cvalue_rel_tol)[1])) +
      max(0, as.numeric(flat_cvalue_abs_tol)[1])
    flat_ok <- scores[is.finite(cvalue) & cvalue <= anchor_cvalue & score <= flat_threshold]
    if (nrow(flat_ok)) {
      data.table::setorder(flat_ok, cvalue)
      selected <- flat_ok[1]
      flat_used <- is.finite(selected$cvalue) && selected$cvalue < anchor_cvalue - 1e-10
      if (isTRUE(flat_used)) {
        final_source <- "observed_flat_ramp_equivalent_fit"
        final_reason <- "meaningful_spend_ramps_no_observed_diminishing_returns_flatter_curve"
      }
    }
  }
  if (isTRUE(has_preferred_cvalue) && is.finite(anchor_weighted$score)) {
    pref_row <- scores[which.min(abs(cvalue - preferred_cvalue))]
    pref_rel_tol <- max(0, as.numeric(preferred_cvalue_rel_tol)[1])
    if (grepl("pooled_group_ramp", as.character(preferred_cvalue_source %||% "")) &&
        preferred_cvalue_reliability >= 0.75 &&
        !isTRUE(preferred_conflicts_total_diminishing)) {
      pref_rel_tol <- max(pref_rel_tol, 0.35)
    }
    pref_threshold <- min(best$score, anchor_weighted$score, na.rm = TRUE) *
      (1 + pref_rel_tol * pmax(0.25, preferred_cvalue_reliability)) +
      max(0, as.numeric(preferred_cvalue_abs_tol)[1])
    force_high_reliability_pooled <- grepl("pooled_group_ramp", as.character(preferred_cvalue_source %||% "")) &&
      preferred_cvalue_reliability >= 0.90
    if (nrow(pref_row) && is.finite(pref_row$score[1]) && (pref_row$score[1] <= pref_threshold || isTRUE(force_high_reliability_pooled))) {
      selected <- pref_row[1]
      preferred_used <- TRUE
      final_source <- as.character(preferred_cvalue_source %||% "observational_cvalue_candidate")
      final_reason <- paste0("preferred_observational_cvalue_with_equivalent_or_better_fit; reliability=", signif(preferred_cvalue_reliability, 3))
    }
  }
  selected_improvement <- if (is.finite(anchor_weighted$score) && anchor_weighted$score > 0) {
    (anchor_weighted$score - selected$score) / anchor_weighted$score
  } else {
    0
  }
  use_best <- identical(mode, "always") ||
    isTRUE(flat_used) ||
    isTRUE(preferred_used) ||
    (is.finite(selected_improvement) && selected_improvement >= min_score_improvement)
  if (!isTRUE(use_best)) {
    return(no_override("anchor_50_sat", "data_driven_cvalue_did_not_clear_improvement_threshold"))
  }

  if (isTRUE(flat_used) || abs(selected$cvalue - best$cvalue) > 1e-12) {
    best_fit <- pb_fit_for_rrate(
      y_mi, x_raw, controls, rrate, anchor_saturation, dvalue, bound,
      wrong_sign_rrate_penalty = wrong_sign_rrate_penalty,
      cvalue_override = selected$cvalue,
      score_weights = weights,
      cvalue_anchor_method = cvalue_anchor_method,
      cvalue_industry_half_saturation = cvalue_industry_half_saturation,
      cvalue_active_quantile = cvalue_active_quantile,
      cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
      curve_type = curve_type
    )
  }

  list(
    fit = best_fit,
    cvalue_anchor = anchor_cvalue,
    cvalue_data_driven = selected$cvalue,
    cvalue_final_source = final_source,
    cvalue_data_reason = final_reason,
    cvalue_data_score_anchor = anchor_weighted$score,
    cvalue_data_score_best = selected$score,
    cvalue_data_improvement = selected_improvement,
    cvalue_data_multiplier = selected$cvalue / anchor_cvalue,
    cvalue_flat_candidate = flat_candidate,
    cvalue_flat_candidate_used = flat_used,
    preferred_cvalue = if (has_preferred_cvalue) preferred_cvalue else NA_real_,
    preferred_cvalue_used = preferred_used,
    preferred_cvalue_source = as.character(preferred_cvalue_source %||% NA_character_),
    preferred_cvalue_reliability = preferred_cvalue_reliability,
    cvalue_anchor_x = best_fit$cvalue_anchor_x,
    cvalue_anchor_method = best_fit$cvalue_anchor_method,
    cvalue_anchor_data_weight = best_fit$cvalue_anchor_data_weight,
    cvalue_anchor_active_cv = best_fit$cvalue_anchor_active_cv,
    ramp_flag = if (nrow(ramp)) ramp$ramp_flag else rep(FALSE, length(x_raw)),
    ramp_n = ramp_n,
    ramp_share = ramp_share,
    ramp_variation_score = ramp_score,
    observed_curve_evidence_class = dim_ret$observed_curve_evidence_class[1],
    observed_marginal_slope_low = dim_ret$observed_marginal_slope_low[1],
    observed_marginal_slope_high = dim_ret$observed_marginal_slope_high[1],
    observed_marginal_slope_ratio = dim_ret$observed_marginal_slope_ratio[1],
    observed_slope_cvalue = dim_ret$observed_slope_cvalue[1],
    observed_diminishing_returns_score = dim_ret$observed_diminishing_returns_score[1],
    observed_low_spend_n = dim_ret$observed_low_spend_n[1],
    observed_high_spend_n = dim_ret$observed_high_spend_n[1],
    observed_low_spend_x = dim_ret$observed_low_spend_x[1],
    observed_high_spend_x = dim_ret$observed_high_spend_x[1],
    observed_spend_spread_p90_p10 = dim_ret$observed_spend_spread_p90_p10[1]
  )
}

pb_partial_r2 <- function(y, x, controls) {
  Xc <- cbind(intercept = 1, controls)
  ok <- stats::complete.cases(y, x, Xc)
  if (sum(ok) < ncol(Xc) + 6L) return(NA_real_)
  fit_y <- stats::lm.fit(Xc[ok, , drop = FALSE], y[ok])
  fit_x <- stats::lm.fit(Xc[ok, , drop = FALSE], x[ok])
  ry <- y[ok] - as.numeric(Xc[ok, , drop = FALSE] %*% replace(fit_y$coefficients, !is.finite(fit_y$coefficients), 0))
  rx <- x[ok] - as.numeric(Xc[ok, , drop = FALSE] %*% replace(fit_x$coefficients, !is.finite(fit_x$coefficients), 0))
  r <- suppressWarnings(stats::cor(ry, rx))
  if (!is.finite(r)) return(NA_real_)
  r ^ 2
}

pb_coef_for_x <- function(y, x, controls) {
  X <- cbind(intercept = 1, controls, x = x)
  ok <- stats::complete.cases(y, X)
  if (sum(ok) < max(8L, ncol(X) + 3L)) return(NA_real_)
  fit <- stats::lm.fit(X[ok, , drop = FALSE], y[ok])
  as.numeric(tail(fit$coefficients, 1))
}

pb_future_spend_placebo <- function(y_mi,
                                    x_handoff,
                                    controls,
                                    lead = 4L,
                                    bound = "free",
                                    target_sign = NA_real_,
                                    warning_ratio = 0.75,
                                    review_ratio = 0.40,
                                    min_signal_partial_r2 = 0.0025) {
  n <- length(x_handoff)
  empty <- data.table::data.table(
    future_spend_placebo_class = "not_available",
    future_spend_placebo_lead = as.integer(lead)[1],
    future_spend_placebo_coef = NA_real_,
    future_spend_placebo_partial_r2 = NA_real_,
    future_spend_placebo_ratio = NA_real_
  )
  lead <- as.integer(lead)[1]
  if (!is.finite(lead) || lead < 1L || n <= lead + 12L) return(empty)
  x <- as.numeric(x_handoff)
  x_future <- c(x[(lead + 1L):n], rep(NA_real_, lead))
  current_r2 <- pb_partial_r2(y_mi, x, controls)
  future_r2 <- pb_partial_r2(y_mi, x_future, controls)
  future_coef <- pb_coef_for_x(y_mi, x_future, controls)
  ratio <- if (is.finite(current_r2) && current_r2 > 0) future_r2 / current_r2 else NA_real_

  target <- NA_real_
  if (bound %in% c("pos", "positive", "+")) target <- 1
  if (bound %in% c("neg", "negative", "-")) target <- -1
  if (!is.finite(target) && is.finite(target_sign) && target_sign != 0) target <- sign(target_sign)
  same_sign <- !is.finite(target) || (is.finite(future_coef) && sign(future_coef) == target)

  cls <- "placebo_not_informative"
  min_r2 <- max(0, as.numeric(min_signal_partial_r2)[1])
  warn <- max(0, as.numeric(warning_ratio)[1])
  review <- max(0, as.numeric(review_ratio)[1])
  if (!is.finite(future_r2)) {
    cls <- "placebo_not_available"
  } else if (isTRUE(same_sign) && future_r2 >= min_r2 && (!is.finite(current_r2) || current_r2 < min_r2)) {
    cls <- "future_spend_stronger_than_current_warning"
  } else if (isTRUE(same_sign) && is.finite(ratio) && ratio >= warn && future_r2 >= min_r2) {
    cls <- "future_spend_placebo_warning"
  } else if (isTRUE(same_sign) && is.finite(ratio) && ratio >= review && future_r2 >= min_r2) {
    cls <- "future_spend_placebo_review"
  } else if (is.finite(current_r2) && current_r2 >= min_r2) {
    cls <- "placebo_pass"
  }

  data.table::data.table(
    future_spend_placebo_class = cls,
    future_spend_placebo_lead = lead,
    future_spend_placebo_coef = future_coef,
    future_spend_placebo_partial_r2 = future_r2,
    future_spend_placebo_ratio = ratio
  )
}

pb_weighted_geomean <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  ok <- is.finite(x) & x > 0 & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  exp(stats::weighted.mean(log(x[ok]), w[ok], na.rm = TRUE))
}

pb_pooled_group_ramp_evidence_one <- function(input_data,
                                              date_col,
                                              dep_var_col,
                                              variable_row,
                                              group_col = NULL,
                                              rrate,
                                              anchor_saturation,
                                              dvalue,
                                              bound,
                                              base_cols = character(),
                                              control_cols = character(),
                                              use_fourier = TRUE,
                                              fourier_period = 52.18,
                                              fourier_K = 2,
                                              use_holidays = TRUE,
                                              holiday_window_weeks = c(-1, 0, 1),
                                              week_end_day = "Sunday",
                                              use_week_of_month = TRUE,
                                              cvalue_anchor_method = c("industry_hybrid", "median_active", "industry_default", "active_quantile"),
                                              cvalue_industry_half_saturation = 0.80,
                                              cvalue_active_quantile = 0.75,
                                              cvalue_hybrid_industry_weight = NA_real_,
                                              curve_type = "weibull",
                                              cvalue_search_multipliers = c(0.35, 3.0),
                                              cvalue_search_bounds = c(0.05, 8.0),
                                              cvalue_grid_n = 31L,
                                              cvalue_min_score_improvement = 0.02,
                                              cvalue_anchor_penalty = 0.01,
                                              cvalue_ramp_min_abs_change_index = 0.25,
                                              cvalue_ramp_min_pct_change = 0.20,
                                              cvalue_ramp_window = 2L,
                                              cvalue_ramp_weight = 4,
                                              cvalue_min_ramp_points = 6L,
                                              cvalue_min_ramp_share = 0.05,
                                              observed_diminishing_min_obs_per_bin = 6L,
                                              min_groups = 2L,
                                              min_obs_per_group = 26L,
                                              min_usable_groups = 2L,
                                              min_total_weight = 0.10,
                                              blend_max = 0.65,
                                              precision_multiplier_max = 4,
                                              future_spend_placebo_lead = 4L,
                                              future_spend_placebo_warning_ratio = 0.75,
                                              future_spend_placebo_review_ratio = 0.40) {
  # Observational quasi-experiment heuristic:
  # - score each geo/segment ramp separately with the same guarded cvalue profile search
  # - keep only material spend movements with enough local observations
  # - downweight reads with weak spread, conflicting slope evidence, or future-spend placebo risk
  # - pool cvalue on the log scale so one noisy group cannot dominate the shared curve
  # This is intentionally not labeled as lift calibration; it is an evidence-weighted
  # fallback when randomized geo lift is unavailable.
  cvalue_anchor_method <- match.arg(cvalue_anchor_method)
  curve_type <- pb_normalize_curve_type(curve_type)[1]
  empty_summary <- data.table::data.table(
    variable = as.character(variable_row$variable[1] %||% NA_character_),
    pooled_ramp_group_col = as.character(group_col %||% NA_character_),
    pooled_ramp_groups_tested = 0L,
    pooled_ramp_usable_groups = 0L,
    pooled_ramp_supports_diminishing_groups = 0L,
    pooled_ramp_flat_groups = 0L,
    pooled_ramp_flat_negative_groups = 0L,
    pooled_ramp_conflict_groups = 0L,
    pooled_ramp_placebo_warning_groups = 0L,
    pooled_ramp_weight_total = 0,
    pooled_ramp_reliability = 0,
    pooled_ramp_cvalue = NA_real_,
    pooled_ramp_cvalue_blend_weight = 0,
    pooled_ramp_precision_multiplier = 1,
    pooled_ramp_evidence_class = "not_available",
    pooled_ramp_reason = "group_ramp_pooling_not_available"
  )
  empty <- list(summary = empty_summary, details = data.table::data.table())
  if (is.null(group_col) || !nzchar(as.character(group_col)[1])) return(empty)
  dt <- data.table::as.data.table(data.table::copy(input_data))
  group_col <- as.character(group_col)[1]
  if (!all(c(date_col, dep_var_col, group_col) %in% names(dt))) return(empty)
  v <- as.character(variable_row$variable[1])
  mx <- as.character(variable_row$modeled_x_col[1] %||% v)
  if (!mx %in% names(dt)) return(empty)
  cols_needed <- unique(c(date_col, dep_var_col, group_col, mx, base_cols, control_cols))
  cols_needed <- cols_needed[cols_needed %in% names(dt)]
  dt <- dt[, ..cols_needed]
  dt[, date__ := pb_parse_date(get(date_col), date_col)]
  dt[, group__ := as.character(get(group_col))]
  dt[, y__ := pb_force_numeric_vec(get(dep_var_col), dep_var_col)]
  dt[, x__ := pb_force_numeric_vec(get(mx), mx)]
  data.table::setorder(dt, group__, date__)

  groups <- dt[, .N, by = group__][N >= min_obs_per_group, group__]
  if (length(groups) < min_groups) {
    empty_summary$pooled_ramp_groups_tested <- length(groups)
    empty_summary$pooled_ramp_evidence_class <- "insufficient_groups"
    empty_summary$pooled_ramp_reason <- "fewer_groups_than_required"
    return(list(summary = empty_summary, details = data.table::data.table()))
  }

  rows <- lapply(groups, function(g) {
    gd <- dt[group__ == g]
    y_mi_g <- pb_mean_index(gd$y__)
    controls_g <- pb_build_controls(
      gd$date__,
      use_fourier = use_fourier,
      fourier_period = fourier_period,
      fourier_K = fourier_K,
      use_holidays = use_holidays,
      holiday_window_weeks = holiday_window_weeks,
      use_week_of_month = use_week_of_month,
      week_end_day = week_end_day
    )
    user_cols <- unique(c(base_cols, control_cols))
    user_cols <- user_cols[user_cols %in% names(gd)]
    if (length(user_cols)) {
      user_controls <- as.matrix(gd[, ..user_cols])
      controls_g <- cbind(controls_g, user_controls)
    }
    if (is.null(dim(controls_g))) controls_g <- matrix(controls_g, ncol = 1)
    if (!ncol(controls_g)) controls_g <- matrix(numeric(0), nrow = nrow(gd), ncol = 0)

    res <- tryCatch(
      pb_estimate_cvalue_from_data(
        y_mi = y_mi_g,
        x_raw = gd$x__,
        controls = controls_g,
        rrate = rrate,
        anchor_saturation = anchor_saturation,
        dvalue = dvalue,
        bound = bound,
        mode = "auto",
        search_multipliers = cvalue_search_multipliers,
        search_bounds = cvalue_search_bounds,
        grid_n = cvalue_grid_n,
        min_score_improvement = cvalue_min_score_improvement,
        anchor_penalty = cvalue_anchor_penalty,
        ramp_min_abs_change_index = cvalue_ramp_min_abs_change_index,
        ramp_min_pct_change = cvalue_ramp_min_pct_change,
        ramp_window = cvalue_ramp_window,
        ramp_weight = cvalue_ramp_weight,
        min_ramp_points = cvalue_min_ramp_points,
        min_ramp_share = cvalue_min_ramp_share,
        cvalue_anchor_method = cvalue_anchor_method,
        cvalue_industry_half_saturation = cvalue_industry_half_saturation,
        cvalue_active_quantile = cvalue_active_quantile,
        cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
        curve_type = curve_type,
        use_observed_diminishing_returns = TRUE,
        observed_diminishing_min_obs_per_bin = observed_diminishing_min_obs_per_bin,
        flatten_when_no_observed_diminishing = TRUE
      ),
      error = function(e) NULL
    )
    if (is.null(res)) {
      return(data.table::data.table(
        variable = v, group_value = g, group_ramp_candidate_cvalue = NA_real_,
        group_ramp_weight = 0, group_ramp_evidence_class = "fit_failed",
        group_ramp_final_source = NA_character_, group_ramp_reason = "group_fit_failed",
        group_ramp_period_n = 0L, group_ramp_period_share = 0,
        group_observed_marginal_slope_ratio = NA_real_,
        group_future_spend_placebo_class = NA_character_
      ))
    }
    fit <- res$fit
    placebo <- pb_future_spend_placebo(
      y_mi = y_mi_g,
      x_handoff = fit$x_handoff,
      controls = controls_g,
      lead = future_spend_placebo_lead,
      bound = bound,
      target_sign = fit$coef,
      warning_ratio = future_spend_placebo_warning_ratio,
      review_ratio = future_spend_placebo_review_ratio
    )
    candidate <- if (is.finite(res$cvalue_data_driven)) res$cvalue_data_driven else if (is.finite(res$observed_slope_cvalue)) res$observed_slope_cvalue else NA_real_
    eclass <- as.character(res$observed_curve_evidence_class %||% "not_available")
    fsource <- as.character(res$cvalue_final_source %||% "")
    placebo_class <- as.character(placebo$future_spend_placebo_class[1] %||% "")
    spread <- suppressWarnings(as.numeric(res$observed_spend_spread_p90_p10 %||% NA_real_))
    slope_ratio <- suppressWarnings(as.numeric(res$observed_marginal_slope_ratio %||% NA_real_))
    w <- 0
    if (is.finite(candidate) && candidate > 0 && res$ramp_n >= cvalue_min_ramp_points && is.finite(res$ramp_share) && res$ramp_share >= cvalue_min_ramp_share) {
      w <- pmin(1, res$ramp_n / max(cvalue_min_ramp_points, 1)) *
        pmin(1, res$ramp_share / max(cvalue_min_ramp_share, 0.01))
      if (is.finite(spread)) w <- w * pmin(1.5, pmax(0.25, spread / 2))
      if (identical(eclass, "supports_diminishing_returns")) w <- w * 1.15
      if (identical(eclass, "weak_or_flat_diminishing_returns_signal")) {
        w <- w * 0.45
        if (!is.finite(slope_ratio) || slope_ratio <= 0) {
          w <- w * 0.35
        } else if (slope_ratio < 0.25) {
          w <- w * 0.60
        }
      }
      if (grepl("warning", placebo_class)) w <- w * 0.20
      if (identical(eclass, "contradicts_diminishing_returns")) w <- w * 0.25
    }
    data.table::data.table(
      variable = v,
      group_value = g,
      group_ramp_candidate_cvalue = candidate,
      group_ramp_weight = w,
      group_ramp_evidence_class = eclass,
      group_ramp_final_source = fsource,
      group_ramp_reason = as.character(res$cvalue_data_reason %||% NA_character_),
      group_ramp_period_n = as.integer(res$ramp_n %||% 0L),
      group_ramp_period_share = as.numeric(res$ramp_share %||% 0),
      group_observed_marginal_slope_ratio = as.numeric(res$observed_marginal_slope_ratio %||% NA_real_),
      group_future_spend_placebo_class = placebo_class
    )
  })
  details <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  tested <- nrow(details)
  usable <- details[is.finite(group_ramp_candidate_cvalue) & group_ramp_candidate_cvalue > 0 & group_ramp_weight > 0]
  support_n <- usable[group_ramp_evidence_class == "supports_diminishing_returns", .N]
  flat_n <- usable[group_ramp_evidence_class == "weak_or_flat_diminishing_returns_signal" |
                     grepl("observed_flat", group_ramp_final_source), .N]
  flat_negative_n <- usable[
    (group_ramp_evidence_class == "weak_or_flat_diminishing_returns_signal" |
       grepl("observed_flat", group_ramp_final_source)) &
      (!is.finite(group_observed_marginal_slope_ratio) | group_observed_marginal_slope_ratio <= 0),
    .N
  ]
  conflict_n <- details[group_ramp_evidence_class == "contradicts_diminishing_returns", .N]
  placebo_n <- details[grepl("warning", group_future_spend_placebo_class), .N]
  placebo_share <- if (tested > 0) placebo_n / tested else 0
  weight_total <- usable[, sum(group_ramp_weight, na.rm = TRUE)]
  pooled <- pb_weighted_geomean(usable$group_ramp_candidate_cvalue, usable$group_ramp_weight)
  usable_n <- nrow(usable)
  consistency <- if (usable_n > 0) max(support_n, flat_n) / usable_n else 0
  conflict_share <- if (tested > 0) conflict_n / tested else 0
  flat_negative_share <- if (flat_n > 0) flat_negative_n / flat_n else 0
  reliability <- pb_clip(pmin(1, usable_n / max(min_usable_groups, 1)) *
                           pmin(1, weight_total / max(min_total_weight, 0.01)) *
                           consistency * (1 - 0.75 * conflict_share) * (1 - 0.50 * placebo_share) *
                           (1 - 0.50 * flat_negative_share), 0, 1)
  eclass <- if (usable_n < min_usable_groups || !is.finite(pooled) || weight_total < min_total_weight) {
    "insufficient_group_ramps"
  } else if (conflict_share >= 0.40 ||
             (support_n > 0 && flat_n > 0 && abs(support_n - flat_n) <= 1L) ||
             (support_n > 0 && flat_negative_n >= max(2L, ceiling(0.50 * flat_n)))) {
    "pooled_mixed_or_conflicting_ramps"
  } else if (flat_n > support_n) {
    "pooled_supports_flatter_curve"
  } else {
    "pooled_supports_diminishing_returns"
  }
  multiplier <- if (eclass %in% c("pooled_supports_flatter_curve", "pooled_supports_diminishing_returns")) {
    1 + reliability * (max(1, precision_multiplier_max) - 1)
  } else if (identical(eclass, "pooled_mixed_or_conflicting_ramps")) {
    0.50
  } else {
    1
  }
  summary <- data.table::data.table(
    variable = v,
    pooled_ramp_group_col = group_col,
    pooled_ramp_groups_tested = tested,
    pooled_ramp_usable_groups = usable_n,
    pooled_ramp_supports_diminishing_groups = support_n,
    pooled_ramp_flat_groups = flat_n,
    pooled_ramp_flat_negative_groups = flat_negative_n,
    pooled_ramp_conflict_groups = conflict_n,
    pooled_ramp_placebo_warning_groups = placebo_n,
    pooled_ramp_weight_total = weight_total,
    pooled_ramp_reliability = reliability,
    pooled_ramp_cvalue = pooled,
    pooled_ramp_cvalue_blend_weight = if (eclass %in% c("pooled_supports_flatter_curve", "pooled_supports_diminishing_returns")) reliability * pb_clip(blend_max, 0, 1) else 0,
    pooled_ramp_precision_multiplier = multiplier,
    pooled_ramp_evidence_class = eclass,
    pooled_ramp_reason = paste0("usable_groups=", usable_n, "; support_dim=", support_n, "; flat=", flat_n, "; conflict=", conflict_n, "; placebo_warning=", placebo_n)
  )
  list(summary = summary, details = details)
}

pb_coef_shrinkage_for <- function(coef_center_shrinkage, variable_name, default = 1) {
  target_variable <- as.character(variable_name)[1]
  if (is.null(coef_center_shrinkage)) return(default)
  if (is.numeric(coef_center_shrinkage) && length(coef_center_shrinkage) == 1L) {
    z <- as.numeric(coef_center_shrinkage)[1]
    return(if (is.finite(z) && z > 0) pb_clip(z, 0.01, 2) else default)
  }
  if (is.numeric(coef_center_shrinkage) && !is.null(names(coef_center_shrinkage))) {
    hit <- coef_center_shrinkage[target_variable]
    if (length(hit) && is.finite(as.numeric(hit)[1]) && as.numeric(hit)[1] > 0) return(pb_clip(as.numeric(hit)[1], 0.01, 2))
  }
  if (is.data.frame(coef_center_shrinkage) || data.table::is.data.table(coef_center_shrinkage)) {
    cs <- data.table::as.data.table(data.table::copy(coef_center_shrinkage))
    if ("variable" %in% names(cs)) {
      val_col <- intersect(c("coef_center_shrinkage", "coef_shrinkage", "shrinkage", "factor"), names(cs))
      if (length(val_col)) {
        z <- suppressWarnings(as.numeric(cs[as.character(get("variable")) == target_variable, get(val_col[1])][1]))
        if (is.finite(z) && z > 0) return(pb_clip(z, 0.01, 2))
      }
    }
  }
  default
}

pb_spend_level_guard <- function(x_raw,
                                 rrate = NA_real_,
                                 cvalue = NA_real_,
                                 dvalue = 1,
                                 curve_type = "weibull",
                                 active_share = NA_real_,
                                 ramp_share = NA_real_,
                                 recent_n = 13L,
                                 under_active_share_threshold = 0.20,
                                 under_max_saturation_threshold = 0.65,
                                 under_p90_to_median_threshold = 1.25,
                                 under_ramp_share_threshold = 0.08,
                                 over_saturation_threshold = 0.85,
                                 over_saturation_week_share_threshold = 0.35,
                                 over_recent_saturation_threshold = 0.80) {
  x <- pmax(suppressWarnings(as.numeric(x_raw)), 0)
  n <- length(x)
  empty <- data.table::data.table(
    spend_level_class = "not_available",
    spend_guard_action = "no_spend_level_guard_available",
    spend_guard_risk_multiplier = 1,
    observed_saturation_median_active = NA_real_,
    observed_saturation_p90 = NA_real_,
    observed_saturation_max = NA_real_,
    high_saturation_week_share = NA_real_,
    recent_saturation_mean = NA_real_,
    active_spend_p90_to_median = NA_real_,
    recent_spend_to_active_median = NA_real_,
    under_spend_flag = FALSE,
    over_spend_flag = FALSE
  )
  if (!n || !is.finite(rrate) || !is.finite(cvalue) || cvalue <= 0 || !is.finite(dvalue) || dvalue <= 0 || all(!is.finite(x))) {
    return(empty)
  }
  active <- is.finite(x) & x > 0
  if (!any(active)) return(empty)

  x_ad_mi <- pb_mean_index(pb_adstock(x, rrate))
  sat <- pb_saturation(x_ad_mi, cvalue, dvalue, curve_type = curve_type)
  sat_active <- sat[active & is.finite(sat)]
  med_active_spend <- stats::median(x[active], na.rm = TRUE)
  p90_active_spend <- as.numeric(stats::quantile(x[active], 0.90, na.rm = TRUE, names = FALSE))
  p90_to_median <- if (is.finite(med_active_spend) && med_active_spend > 0) p90_active_spend / med_active_spend else NA_real_
  recent_n <- max(1L, min(n, as.integer(recent_n)[1]))
  recent_idx <- seq.int(n - recent_n + 1L, n)
  recent_spend <- mean(x[recent_idx], na.rm = TRUE)
  recent_spend_to_median <- if (is.finite(med_active_spend) && med_active_spend > 0) recent_spend / med_active_spend else NA_real_
  high_share <- mean(sat >= over_saturation_threshold, na.rm = TRUE)
  recent_sat <- mean(sat[recent_idx], na.rm = TRUE)
  sat_max <- max(sat_active, na.rm = TRUE)

  under_flag <- (is.finite(active_share) && active_share < under_active_share_threshold) ||
    (is.finite(sat_max) && sat_max < under_max_saturation_threshold) ||
    (is.finite(ramp_share) && ramp_share < under_ramp_share_threshold &&
       is.finite(p90_to_median) && p90_to_median < under_p90_to_median_threshold)
  over_flag <- (is.finite(high_share) && high_share >= over_saturation_week_share_threshold) ||
    (is.finite(recent_sat) && recent_sat >= over_recent_saturation_threshold)

  spend_class <- "supported_spend_range"
  action <- "standard_curve_review"
  risk <- 1
  if (under_flag && over_flag) {
    spend_class <- "mixed_sparse_and_high_saturation"
    action <- "keep_curve_loose_review_low_and_high_spend_extrapolation"
    risk <- 8
  } else if (under_flag) {
    spend_class <- "under_spent_or_limited_support"
    action <- "keep_curve_loose_avoid_high_spend_extrapolation"
    risk <- 6
  } else if (over_flag) {
    spend_class <- "over_spent_or_high_saturation"
    action <- "review_incrementality_and_do_not_overtrust_flat_observational_fit"
    risk <- 4
  }

  data.table::data.table(
    spend_level_class = spend_class,
    spend_guard_action = action,
    spend_guard_risk_multiplier = risk,
    observed_saturation_median_active = stats::median(sat_active, na.rm = TRUE),
    observed_saturation_p90 = as.numeric(stats::quantile(sat_active, 0.90, na.rm = TRUE, names = FALSE)),
    observed_saturation_max = sat_max,
    high_saturation_week_share = high_share,
    recent_saturation_mean = recent_sat,
    active_spend_p90_to_median = p90_to_median,
    recent_spend_to_active_median = recent_spend_to_median,
    under_spend_flag = under_flag,
    over_spend_flag = over_flag
  )
}

pb_residualize_against_controls <- function(y, controls) {
  y <- as.numeric(y)
  Xc <- cbind(intercept = 1, controls)
  ok <- stats::complete.cases(y, Xc)
  out <- rep(NA_real_, length(y))
  if (sum(ok) < max(8L, ncol(Xc) + 3L)) return(out)
  fit <- stats::lm.fit(Xc[ok, , drop = FALSE], y[ok])
  co <- fit$coefficients
  co[!is.finite(co)] <- 0
  pred <- as.numeric(Xc[ok, , drop = FALSE] %*% co)
  out[ok] <- y[ok] - pred
  out
}

pb_expected_coef_sign <- function(bound) {
  b <- tolower(trimws(as.character(bound %||% "")))
  if (b %in% c("pos", "positive", "+", "lower0", "nonnegative")) return(1L)
  if (b %in% c("neg", "negative", "-", "upper0", "nonpositive")) return(-1L)
  0L
}

pb_multivariate_ridge_scan <- function(y_mi,
                                       transformed_dt,
                                       controls,
                                       priors_dt,
                                       lambda_grid = 10 ^ seq(-4, 3, length.out = 32),
                                       min_obs = 30L) {
  empty <- data.table::data.table()
  if (is.null(transformed_dt) || !nrow(transformed_dt) ||
      !all(c("date", "variable", "x_handoff") %in% names(transformed_dt)) ||
      is.null(priors_dt) || nrow(priors_dt) < 2L) {
    return(empty)
  }
  vars <- as.character(priors_dt$variable)
  wide <- data.table::dcast(
    data.table::as.data.table(transformed_dt)[variable %in% vars],
    date ~ variable,
    value.var = "x_handoff"
  )
  data.table::setorder(wide, date)
  vars <- vars[vars %in% names(wide)]
  if (length(vars) < 2L) return(empty)
  X_raw <- as.matrix(wide[, ..vars])
  storage.mode(X_raw) <- "double"
  y <- suppressWarnings(as.numeric(y_mi))
  n <- min(length(y), nrow(X_raw))
  if (n <= 0L) return(empty)
  y <- y[seq_len(n)]
  X_raw <- X_raw[seq_len(n), , drop = FALSE]
  controls <- as.matrix(controls %||% matrix(numeric(0), nrow = n, ncol = 0))
  if (nrow(controls) != n) controls <- matrix(numeric(0), nrow = n, ncol = 0)

  ok <- stats::complete.cases(y, X_raw, controls)
  if (sum(ok) < max(as.integer(min_obs), ncol(X_raw) + ncol(controls) + 6L)) {
    return(data.table::data.table(
      variable = vars,
      multivariate_coef_scan_class = "insufficient_observations",
      multivariate_ridge_coef = NA_real_,
      multivariate_ridge_lambda = NA_real_,
      multivariate_ridge_gcv = NA_real_,
      multivariate_ridge_condition_number = NA_real_,
      multivariate_ridge_max_abs_corr = NA_real_,
      multivariate_ridge_direction_ok = NA,
      multivariate_ridge_to_univariate_ratio = NA_real_
    ))
  }

  y_res <- pb_residualize_against_controls(y, controls)
  X_res <- apply(X_raw, 2, function(xx) pb_residualize_against_controls(xx, controls))
  if (is.null(dim(X_res))) X_res <- matrix(X_res, ncol = 1)
  ok2 <- stats::complete.cases(y_res, X_res)
  if (sum(ok2) < max(as.integer(min_obs), ncol(X_res) + 6L)) {
    return(data.table::data.table(
      variable = vars,
      multivariate_coef_scan_class = "insufficient_residualized_observations",
      multivariate_ridge_coef = NA_real_,
      multivariate_ridge_lambda = NA_real_,
      multivariate_ridge_gcv = NA_real_,
      multivariate_ridge_condition_number = NA_real_,
      multivariate_ridge_max_abs_corr = NA_real_,
      multivariate_ridge_direction_ok = NA,
      multivariate_ridge_to_univariate_ratio = NA_real_
    ))
  }

  yv <- y_res[ok2]
  Xm <- X_res[ok2, , drop = FALSE]
  x_sd <- apply(Xm, 2, stats::sd, na.rm = TRUE)
  keep <- is.finite(x_sd) & x_sd > 1e-8
  out <- data.table::data.table(variable = vars)
  out[, `:=`(
    multivariate_coef_scan_class = "not_estimated",
    multivariate_ridge_coef = NA_real_,
    multivariate_ridge_lambda = NA_real_,
    multivariate_ridge_gcv = NA_real_,
    multivariate_ridge_condition_number = NA_real_,
    multivariate_ridge_max_abs_corr = NA_real_,
    multivariate_ridge_direction_ok = NA,
    multivariate_ridge_to_univariate_ratio = NA_real_
  )]
  if (sum(keep) < 2L) {
    out[, multivariate_coef_scan_class := "insufficient_media_variation_after_controls"]
    return(out[])
  }
  vars_keep <- vars[keep]
  Xm <- Xm[, keep, drop = FALSE]
  x_center <- colMeans(Xm, na.rm = TRUE)
  x_scale <- x_sd[keep]
  Xs <- sweep(sweep(Xm, 2, x_center, "-"), 2, x_scale, "/")
  yc <- yv - mean(yv, na.rm = TRUE)
  XtX <- crossprod(Xs)
  Xty <- crossprod(Xs, yc)
  eig <- tryCatch(eigen(XtX, symmetric = TRUE, only.values = TRUE)$values, error = function(e) rep(NA_real_, ncol(Xs)))
  eig_pos <- eig[is.finite(eig) & eig > 1e-10]
  condition <- if (length(eig_pos)) sqrt(max(eig_pos) / min(eig_pos)) else Inf
  corr_mat <- suppressWarnings(stats::cor(Xs, use = "pairwise.complete.obs"))
  max_corr <- if (ncol(Xs) > 1L && all(dim(corr_mat) > 1L)) {
    z <- abs(corr_mat[upper.tri(corr_mat)])
    if (any(is.finite(z))) max(z[is.finite(z)], na.rm = TRUE) else NA_real_
  } else {
    NA_real_
  }
  lambda_grid <- suppressWarnings(as.numeric(lambda_grid))
  lambda_grid <- sort(unique(lambda_grid[is.finite(lambda_grid) & lambda_grid >= 0]))
  if (!length(lambda_grid)) lambda_grid <- 10 ^ seq(-4, 3, length.out = 32)

  score_one <- function(lambda) {
    beta <- tryCatch(
      as.numeric(solve(XtX + diag(lambda, ncol(Xs)), Xty)),
      error = function(e) as.numeric(qr.solve(XtX + diag(lambda, ncol(Xs)), Xty))
    )
    pred <- as.numeric(Xs %*% beta)
    rss <- sum((yc - pred) ^ 2, na.rm = TRUE)
    df <- if (length(eig_pos)) sum(eig_pos / (eig_pos + lambda), na.rm = TRUE) else ncol(Xs)
    den <- pmax(length(yc) - df, 1)
    data.table::data.table(lambda = lambda, gcv = rss / (den ^ 2), rss = rss, df = df, beta = list(beta))
  }
  scores <- data.table::rbindlist(lapply(lambda_grid, score_one), use.names = TRUE, fill = TRUE)
  if (!nrow(scores) || all(!is.finite(scores$gcv))) {
    out[variable %in% vars_keep, multivariate_coef_scan_class := "ridge_failed"]
    return(out[])
  }
  best <- scores[which.min(gcv)]
  beta_std <- as.numeric(best$beta[[1]])
  beta_orig <- beta_std / x_scale
  scan_class <- if (is.finite(condition) && condition >= 100) "joint_scan_highly_ill_conditioned" else "joint_scan_ok"

  prior_lookup <- data.table::as.data.table(priors_dt)[, .(
    variable = as.character(variable),
    coef_bound = as.character(coef_bound %||% ""),
    coef_prior_unshrunk = suppressWarnings(as.numeric(coef_prior_unshrunk))
  )]
  add <- data.table::data.table(
    variable = vars_keep,
    multivariate_coef_scan_class = scan_class,
    multivariate_ridge_coef = beta_orig,
    multivariate_ridge_lambda = as.numeric(best$lambda),
    multivariate_ridge_gcv = as.numeric(best$gcv),
    multivariate_ridge_condition_number = condition,
    multivariate_ridge_max_abs_corr = max_corr
  )
  add[prior_lookup, on = "variable", `:=`(
    coef_bound = i.coef_bound,
    coef_prior_unshrunk_lookup = i.coef_prior_unshrunk
  )]
  add[, expected_sign := vapply(coef_bound, pb_expected_coef_sign, integer(1))]
  add[, multivariate_ridge_direction_ok := expected_sign == 0L | sign(multivariate_ridge_coef) == expected_sign]
  add[, multivariate_ridge_to_univariate_ratio := fifelse(
    is.finite(coef_prior_unshrunk_lookup) & abs(coef_prior_unshrunk_lookup) > 1e-12,
    multivariate_ridge_coef / coef_prior_unshrunk_lookup,
    NA_real_
  )]
  add[is.na(multivariate_ridge_direction_ok), multivariate_ridge_direction_ok := FALSE]
  add[multivariate_ridge_direction_ok == FALSE, multivariate_coef_scan_class := "joint_scan_wrong_sign"]
  out[add, on = "variable", `:=`(
    multivariate_coef_scan_class = i.multivariate_coef_scan_class,
    multivariate_ridge_coef = i.multivariate_ridge_coef,
    multivariate_ridge_lambda = i.multivariate_ridge_lambda,
    multivariate_ridge_gcv = i.multivariate_ridge_gcv,
    multivariate_ridge_condition_number = i.multivariate_ridge_condition_number,
    multivariate_ridge_max_abs_corr = i.multivariate_ridge_max_abs_corr,
    multivariate_ridge_direction_ok = i.multivariate_ridge_direction_ok,
    multivariate_ridge_to_univariate_ratio = i.multivariate_ridge_to_univariate_ratio
  )]
  out[]
}

pb_bin_slope <- function(y, x, idx, min_obs = 8L) {
  ok <- idx & stats::complete.cases(y, x)
  n <- sum(ok)
  if (n < min_obs || stats::sd(x[ok], na.rm = TRUE) <= 1e-8) {
    return(list(slope = NA_real_, n = n))
  }
  fit <- stats::lm.fit(cbind(intercept = 1, x = x[ok]), y[ok])
  slope <- as.numeric(tail(fit$coefficients, 1))
  list(slope = slope, n = n)
}

pb_observed_diminishing_returns <- function(y_mi,
                                            x_raw,
                                            controls,
                                            rrate,
                                            dvalue = 1,
                                            curve_type = "weibull",
                                            low_quantile = 0.35,
                                            high_quantile = 0.65,
                                            min_obs_per_bin = 8L,
                                            min_spread_p90_p10 = 1.25,
                                            support_ratio_threshold = 0.85,
                                            cvalue_bounds = c(0.05, 8.0)) {
  x_raw <- pmax(suppressWarnings(as.numeric(x_raw)), 0)
  n <- length(x_raw)
  empty <- data.table::data.table(
    observed_curve_evidence_class = "not_available",
    observed_marginal_slope_low = NA_real_,
    observed_marginal_slope_high = NA_real_,
    observed_marginal_slope_ratio = NA_real_,
    observed_slope_cvalue = NA_real_,
    observed_diminishing_returns_score = NA_real_,
    observed_low_spend_n = 0L,
    observed_high_spend_n = 0L,
    observed_low_spend_x = NA_real_,
    observed_high_spend_x = NA_real_,
    observed_spend_spread_p90_p10 = NA_real_
  )
  if (n < 20L || !is.finite(rrate) || !is.finite(dvalue) || dvalue <= 0 || all(!is.finite(x_raw))) return(empty)
  curve_type <- pb_normalize_curve_type(curve_type)[1]

  x_ad_mi <- pb_mean_index(pb_adstock(x_raw, rrate))
  active <- is.finite(y_mi) & is.finite(x_ad_mi) & is.finite(x_raw) & x_raw > 0 & x_ad_mi > 0
  if (sum(active) < 2L * min_obs_per_bin) {
    empty$observed_curve_evidence_class <- "insufficient_active_observations"
    return(empty)
  }

  p10 <- as.numeric(stats::quantile(x_ad_mi[active], 0.10, na.rm = TRUE, names = FALSE))
  p90 <- as.numeric(stats::quantile(x_ad_mi[active], 0.90, na.rm = TRUE, names = FALSE))
  spread <- if (is.finite(p10) && p10 > 0) p90 / p10 else NA_real_
  if (!is.finite(spread) || spread < min_spread_p90_p10) {
    empty$observed_curve_evidence_class <- "insufficient_spend_range"
    empty$observed_spend_spread_p90_p10 <- spread
    return(empty)
  }

  qlo <- as.numeric(stats::quantile(x_ad_mi[active], pb_clip(low_quantile, 0.10, 0.49), na.rm = TRUE, names = FALSE))
  qhi <- as.numeric(stats::quantile(x_ad_mi[active], pb_clip(high_quantile, 0.51, 0.90), na.rm = TRUE, names = FALSE))
  if (!is.finite(qlo) || !is.finite(qhi) || qhi <= qlo) {
    empty$observed_curve_evidence_class <- "insufficient_spend_range"
    empty$observed_spend_spread_p90_p10 <- spread
    return(empty)
  }

  y_resid <- pb_residualize_against_controls(y_mi, controls)
  low_idx <- active & x_ad_mi <= qlo
  high_idx <- active & x_ad_mi >= qhi
  low <- pb_bin_slope(y_resid, x_ad_mi, low_idx, min_obs = min_obs_per_bin)
  high <- pb_bin_slope(y_resid, x_ad_mi, high_idx, min_obs = min_obs_per_bin)
  out <- empty
  out$observed_low_spend_n <- low$n
  out$observed_high_spend_n <- high$n
  out$observed_low_spend_x <- stats::median(x_ad_mi[low_idx], na.rm = TRUE)
  out$observed_high_spend_x <- stats::median(x_ad_mi[high_idx], na.rm = TRUE)
  out$observed_spend_spread_p90_p10 <- spread
  out$observed_marginal_slope_low <- low$slope
  out$observed_marginal_slope_high <- high$slope

  if (!is.finite(low$slope) || !is.finite(high$slope)) {
    out$observed_curve_evidence_class <- "insufficient_bin_observations"
    return(out)
  }
  if (low$slope <= 0) {
    out$observed_curve_evidence_class <- "no_positive_low_spend_signal"
    return(out)
  }

  ratio <- high$slope / low$slope
  out$observed_marginal_slope_ratio <- ratio
  out$observed_diminishing_returns_score <- 1 - ratio
  threshold <- pb_clip(as.numeric(support_ratio_threshold)[1], 0.50, 0.98)
  if (is.finite(ratio) && ratio > 0 && ratio < threshold) {
    out$observed_curve_evidence_class <- "supports_diminishing_returns"
    target <- pb_clip(ratio, 0.02, 0.98)
    x_low <- out$observed_low_spend_x
    x_high <- out$observed_high_spend_x
    lo <- min(cvalue_bounds, na.rm = TRUE)
    hi <- max(cvalue_bounds, na.rm = TRUE)
    if (!is.finite(lo) || !is.finite(hi) || lo <= 0 || hi <= lo) {
      lo <- 0.05; hi <- 8.0
    }
    if (is.finite(x_low) && is.finite(x_high) && x_high > x_low && x_low > 0) {
      ratio_for_cvalue <- function(cv) {
        if (identical(curve_type, "hill")) {
          z_high <- (cv * x_high) ^ dvalue
          z_low <- (cv * x_low) ^ dvalue
          num <- (x_high ^ (dvalue - 1)) / ((1 + z_high) ^ 2)
          den <- (x_low ^ (dvalue - 1)) / ((1 + z_low) ^ 2)
        } else {
          num <- (x_high ^ (dvalue - 1)) * exp(-((cv * x_high) ^ dvalue))
          den <- (x_low ^ (dvalue - 1)) * exp(-((cv * x_low) ^ dvalue))
        }
        if (!is.finite(num) || !is.finite(den) || den <= 0) return(Inf)
        num / den
      }
      opt <- tryCatch(
        stats::optimize(function(cv) (log(ratio_for_cvalue(cv)) - log(target)) ^ 2, interval = c(lo, hi)),
        error = function(e) NULL
      )
      if (!is.null(opt) && is.finite(opt$minimum)) out$observed_slope_cvalue <- opt$minimum
    }
  } else if (is.finite(ratio) && ratio > 1 / threshold) {
    out$observed_curve_evidence_class <- "contradicts_diminishing_returns"
  } else {
    out$observed_curve_evidence_class <- "weak_or_flat_diminishing_returns_signal"
  }
  out
}

pb_boot_sign_stability <- function(y_mi, x_handoff, controls, bound, target_sign = NA_real_, reps = 40L, seed = 123L) {
  X <- cbind(intercept = 1, controls, x_handoff = x_handoff)
  ok_all <- stats::complete.cases(y_mi, X)
  if (sum(ok_all) < ncol(X) + 8L) return(NA_real_)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  signs <- numeric(reps)
  idx_pool <- which(ok_all)
  for (b in seq_len(reps)) {
    idx <- sample(idx_pool, size = length(idx_pool), replace = TRUE)
    fit <- stats::lm.fit(X[idx, , drop = FALSE], y_mi[idx])
    cx <- tail(fit$coefficients, 1)
    signs[b] <- ifelse(is.finite(cx), sign(cx), NA_real_)
  }
  signs <- signs[is.finite(signs)]
  if (!length(signs)) return(NA_real_)

  target <- NA_real_
  if (bound %in% c("pos", "positive", "+")) target <- 1
  if (bound %in% c("neg", "negative", "-")) target <- -1
  if (!is.finite(target) && is.finite(target_sign) && target_sign != 0) target <- sign(target_sign)
  if (!is.finite(target)) return(NA_real_)

  mean(signs == target, na.rm = TRUE)
}

pb_vif_for_matrix <- function(mat, min_obs = 12L) {
  if (is.null(dim(mat)) || ncol(mat) < 2L) return(rep(NA_real_, ncol(mat)))
  out <- rep(NA_real_, ncol(mat))
  for (j in seq_len(ncol(mat))) {
    others <- setdiff(seq_len(ncol(mat)), j)
    sub <- mat[, c(j, others), drop = FALSE]
    ok <- stats::complete.cases(sub)
    if (sum(ok) < max(min_obs, length(others) + 4L)) next
    y <- sub[ok, 1]
    Xo <- sub[ok, -1, drop = FALSE]
    keep <- vapply(seq_len(ncol(Xo)), function(k) {
      s <- stats::sd(Xo[, k], na.rm = TRUE)
      is.finite(s) && s > 1e-8
    }, logical(1))
    Xo <- Xo[, keep, drop = FALSE]
    if (!ncol(Xo) || length(y) < ncol(Xo) + 4L) next
    X <- cbind(intercept = 1, Xo)
    fit <- tryCatch(stats::lm.fit(X, y), error = function(e) NULL)
    if (is.null(fit)) next
    co <- fit$coefficients
    co[!is.finite(co)] <- 0
    pred <- as.numeric(X %*% co)
    tss <- sum((y - mean(y, na.rm = TRUE)) ^ 2, na.rm = TRUE)
    rss <- sum((y - pred) ^ 2, na.rm = TRUE)
    if (!is.finite(tss) || tss <= 1e-12 || !is.finite(rss)) next
    r2 <- pb_clip(1 - rss / tss, 0, 0.999)
    out[j] <- 1 / (1 - r2)
  }
  names(out) <- colnames(mat)
  out
}

pb_identification_diagnostics_from_tables <- function(priors_dt,
                                                      evidence_dt,
                                                      support_dt,
                                                      transformed_dt,
                                                      corr_threshold = 0.85,
                                                      severe_corr_threshold = 0.95,
                                                      vif_threshold = 5,
                                                      severe_vif_threshold = 10) {
  priors_dt <- data.table::as.data.table(data.table::copy(priors_dt))
  evidence_dt <- data.table::as.data.table(data.table::copy(evidence_dt))
  support_dt <- data.table::as.data.table(data.table::copy(support_dt))
  transformed_dt <- data.table::as.data.table(data.table::copy(transformed_dt))

  empty <- list(
    variable_diagnostics = data.table::data.table(),
    collinearity_pairs = data.table::data.table(),
    transformed_wide = data.table::data.table()
  )
  if (!nrow(priors_dt) || !all(c("date", "variable", "x_handoff") %in% names(transformed_dt))) return(empty)

  transformed_dt[, variable := as.character(variable)]
  id_cols <- intersect(c("date", "model_week_end"), names(transformed_dt))
  wide <- data.table::dcast(
    transformed_dt,
    stats::as.formula(paste(paste(id_cols, collapse = " + "), "~ variable")),
    value.var = "x_handoff",
    fun.aggregate = function(z) mean(z, na.rm = TRUE)
  )
  vars <- intersect(as.character(priors_dt$variable), setdiff(names(wide), id_cols))
  vars <- vars[vapply(vars, function(v) {
    s <- stats::sd(wide[[v]], na.rm = TRUE)
    is.finite(s) && s > 1e-8
  }, logical(1))]

  if (length(vars) < 2L) {
    vd <- unique(priors_dt[, .(variable)])
    vd[, `:=`(
      max_abs_correlation = NA_real_,
      n_corr_pairs_over_threshold = 0L,
      n_corr_pairs_severe = 0L,
      max_vif = NA_real_,
      collinearity_class = "not_enough_variables",
      collinearity_risk_multiplier = 1,
      requires_external_prior = FALSE
    )]
    return(list(variable_diagnostics = vd[], collinearity_pairs = data.table::data.table(), transformed_wide = wide[]))
  }

  mat <- as.matrix(wide[, ..vars])
  storage.mode(mat) <- "double"
  cm <- suppressWarnings(stats::cor(mat, use = "pairwise.complete.obs"))
  idx <- which(upper.tri(cm), arr.ind = TRUE)
  pairs_all <- data.table::data.table(
    variable_1 = colnames(cm)[idx[, 1]],
    variable_2 = colnames(cm)[idx[, 2]],
    correlation = as.numeric(cm[idx]),
    abs_correlation = abs(as.numeric(cm[idx]))
  )
  pairs_all <- pairs_all[is.finite(abs_correlation)]
  pairs_all[, pair_class := data.table::fifelse(
    abs_correlation >= severe_corr_threshold, "severe",
    data.table::fifelse(abs_correlation >= corr_threshold, "high",
                        data.table::fifelse(abs_correlation >= 0.70, "moderate", "low"))
  )]
  pairs_flagged <- pairs_all[abs_correlation >= min(0.70, corr_threshold)][order(-abs_correlation)]

  pair_long <- data.table::rbindlist(list(
    pairs_all[, .(variable = variable_1, paired_variable = variable_2, abs_correlation)],
    pairs_all[, .(variable = variable_2, paired_variable = variable_1, abs_correlation)]
  ), use.names = TRUE, fill = TRUE)
  pair_summary <- pair_long[, .(
    max_abs_correlation = max(abs_correlation, na.rm = TRUE),
    n_corr_pairs_over_threshold = sum(abs_correlation >= corr_threshold, na.rm = TRUE),
    n_corr_pairs_severe = sum(abs_correlation >= severe_corr_threshold, na.rm = TRUE)
  ), by = variable]

  vif <- pb_vif_for_matrix(mat)
  vif_dt <- data.table::data.table(variable = names(vif), max_vif = as.numeric(vif))

  vd <- unique(priors_dt[, .(variable)])
  vd <- pair_summary[vd, on = "variable"]
  vd <- vif_dt[vd, on = "variable"]
  vd[!is.finite(max_abs_correlation), max_abs_correlation := NA_real_]
  vd[is.na(n_corr_pairs_over_threshold), n_corr_pairs_over_threshold := 0L]
  vd[is.na(n_corr_pairs_severe), n_corr_pairs_severe := 0L]

  vd[, collinearity_class := data.table::fifelse(
    (is.finite(max_abs_correlation) & max_abs_correlation >= severe_corr_threshold) |
      (is.finite(max_vif) & max_vif >= severe_vif_threshold),
    "severe_multicollinearity",
    data.table::fifelse(
      (is.finite(max_abs_correlation) & max_abs_correlation >= corr_threshold) |
        (is.finite(max_vif) & max_vif >= vif_threshold),
      "high_multicollinearity",
      data.table::fifelse(
        (is.finite(max_abs_correlation) & max_abs_correlation >= 0.70) |
          (is.finite(max_vif) & max_vif >= 3),
        "moderate_multicollinearity",
        "low_multicollinearity"
      )
    )
  )]
  vd[, collinearity_risk_multiplier := data.table::fifelse(
    collinearity_class == "severe_multicollinearity", 12,
    data.table::fifelse(collinearity_class == "high_multicollinearity", 6,
                        data.table::fifelse(collinearity_class == "moderate_multicollinearity", 2, 1))
  )]
  vd[, requires_external_prior := collinearity_class %in% c("high_multicollinearity", "severe_multicollinearity")]
  vd[, recommended_identification_action := data.table::fifelse(
    collinearity_class == "severe_multicollinearity",
    "aggregate_or_force_strong_external_roi_contribution_prior",
    data.table::fifelse(
      collinearity_class == "high_multicollinearity",
      "use_external_prior_or_aggregate_correlated_channels",
      data.table::fifelse(
        collinearity_class == "moderate_multicollinearity",
        "run_sensitivity_and_keep_priors_loose",
        "standard_prior_workflow"
      )
    )
  )]

  keep_evidence <- intersect(c(
    "variable", "active_week_share", "top4_concentration", "residualized_partial_r2",
    "bootstrap_sign_stability", "evidence_tier"
  ), names(evidence_dt))
  if (length(keep_evidence) > 1L) vd <- unique(evidence_dt[, ..keep_evidence])[vd, on = "variable"]
  keep_support <- intersect(c(
    "variable", "input_class", "modeled_x_basis", "support_type", "spend_support_cor",
    "cost_per_support_iqr_ratio", "modeled_active_week_share", "modeled_top4_concentration"
  ), names(support_dt))
  if (length(keep_support) > 1L) vd <- unique(support_dt[, ..keep_support])[vd, on = "variable"]

  list(
    variable_diagnostics = vd[],
    collinearity_pairs = pairs_flagged[],
    transformed_wide = wide[]
  )
}

pb_apply_identification_adjustments <- function(priors_dt,
                                                evidence_dt,
                                                identification,
                                                min_precision = 1e-6) {
  priors_dt <- data.table::as.data.table(data.table::copy(priors_dt))
  evidence_dt <- data.table::as.data.table(data.table::copy(evidence_dt))
  vd <- identification$variable_diagnostics
  if (is.null(vd) || !nrow(vd)) return(list(priors = priors_dt, evidence = evidence_dt))
  vd <- data.table::as.data.table(data.table::copy(vd))
  diagnostic_defaults <- list(
    max_abs_correlation = NA_real_,
    n_corr_pairs_over_threshold = 0L,
    n_corr_pairs_severe = 0L,
    max_vif = NA_real_,
    collinearity_class = "not_enough_variables",
    collinearity_risk_multiplier = 1,
    requires_external_prior = FALSE,
    recommended_identification_action = "standard_prior_workflow"
  )
  for (nm in names(diagnostic_defaults)) {
    if (!(nm %in% names(vd))) vd[, (nm) := diagnostic_defaults[[nm]]]
  }
  adj_cols <- c(
    "variable", "max_abs_correlation", "n_corr_pairs_over_threshold", "n_corr_pairs_severe",
    "max_vif", "collinearity_class", "collinearity_risk_multiplier",
    "requires_external_prior", "recommended_identification_action"
  )
  adj_cols <- intersect(adj_cols, names(vd))
  adj <- vd[, ..adj_cols]

  priors_dt[adj, on = "variable", `:=`(
    max_abs_correlation = i.max_abs_correlation,
    n_corr_pairs_over_threshold = i.n_corr_pairs_over_threshold,
    n_corr_pairs_severe = i.n_corr_pairs_severe,
    max_vif = i.max_vif,
    collinearity_class = i.collinearity_class,
    collinearity_risk_multiplier = i.collinearity_risk_multiplier,
    requires_external_prior = i.requires_external_prior,
    recommended_identification_action = i.recommended_identification_action
  )]
  priors_dt[!is.finite(collinearity_risk_multiplier) | is.na(collinearity_risk_multiplier), collinearity_risk_multiplier := 1]
  priors_dt[, coef_precision_pre_collinearity := coef_precision_final]
  priors_dt[collinearity_risk_multiplier > 1, coef_precision_final := pmax(coef_precision_final / collinearity_risk_multiplier, min_precision)]

  if (!"anchor_should_drive_curve_prior" %in% names(priors_dt)) priors_dt[, anchor_should_drive_curve_prior := FALSE]
  priors_dt[is.na(anchor_should_drive_curve_prior), anchor_should_drive_curve_prior := FALSE]
  priors_dt[, `:=`(
    rrate_precision_pre_collinearity = rrate_precision_final,
    cvalue_precision_pre_collinearity = cvalue_precision_final,
    dvalue_precision_pre_collinearity = dvalue_precision_final
  )]
  priors_dt[collinearity_risk_multiplier > 1 & has_curve == TRUE,
            rrate_precision_final := pmax(rrate_precision_final / sqrt(collinearity_risk_multiplier), min_precision)]
  priors_dt[collinearity_risk_multiplier > 1 & has_curve == TRUE & anchor_should_drive_curve_prior != TRUE,
            cvalue_precision_final := pmax(cvalue_precision_final / sqrt(collinearity_risk_multiplier), min_precision)]
  priors_dt[collinearity_risk_multiplier > 1 & has_curve == TRUE,
            dvalue_precision_final := pmax(dvalue_precision_final / sqrt(collinearity_risk_multiplier), min_precision)]

  priors_dt[collinearity_risk_multiplier > 1, coef_prior_handoff_reason := paste0(
    coef_prior_handoff_reason,
    "; collinearity=", collinearity_class,
    "; precision_relaxed_x", collinearity_risk_multiplier
  )]
  priors_dt[collinearity_risk_multiplier >= 6, `:=`(
    coef_prior_handoff_tier = "requires_external_prior_or_aggregation",
    use_as_production_prior = FALSE,
    recommended_prior_strategy = "requires_external_prior_or_aggregation",
    identification_class = "multicollinear_needs_external_identification"
  )]

  evidence_dt[adj, on = "variable", `:=`(
    max_abs_correlation = i.max_abs_correlation,
    n_corr_pairs_over_threshold = i.n_corr_pairs_over_threshold,
    n_corr_pairs_severe = i.n_corr_pairs_severe,
    max_vif = i.max_vif,
    collinearity_class = i.collinearity_class,
    collinearity_risk_multiplier = i.collinearity_risk_multiplier,
    requires_external_prior = i.requires_external_prior,
    recommended_identification_action = i.recommended_identification_action
  )]
  list(priors = priors_dt[], evidence = evidence_dt[])
}

build_mmm_priors_engine <- function(input_data = NULL,
                          input_file = NULL,
                          sheet = NULL,
                          date_col,
                          dep_var_col,
                          holdout_col = NULL,
                          holdout_value = TRUE,
                          holdout_last_n = 0L,
                          variable_map = NULL,
                          media_cols = character(),
                          curve_cols = NULL,
                          base_cols = character(),
                          control_cols = character(),
                          coef_bounds = NULL,
                          fixed_rrate_by_var = NULL,
                          estimate_rrate = TRUE,
                          rrate_search_method = c("optimize", "grid"),
                          rrate_bounds = c(0, 0.80),
                          rrate_grid_n = 21L,
                          wrong_sign_rrate_penalty = 100,
                          rrate_upper_bound_fallback = TRUE,
                          rrate_upper_bound_tolerance = 0.02,
                          rrate_plateau_rel_tol = 0.01,
                          rrate_plateau_abs_tol = 0.0001,
                          rrate_plateau_grid_n = NULL,
                          estimate_cvalue_from_data = c("auto", "never", "always"),
                          cvalue_search_multipliers = c(0.35, 3.0),
                          cvalue_search_bounds = c(0.05, 8.0),
                          cvalue_grid_n = 31L,
                          cvalue_min_score_improvement = 0.02,
                          cvalue_anchor_penalty = 0.01,
                          cvalue_ramp_min_abs_change_index = 0.25,
                          cvalue_ramp_min_pct_change = 0.20,
                          cvalue_ramp_window = 2L,
                          cvalue_ramp_weight = 4,
                          cvalue_min_ramp_points = 8L,
                          cvalue_min_ramp_share = 0.08,
                          cvalue_anchor_method = c("industry_hybrid", "median_active", "industry_default", "active_quantile"),
                          cvalue_industry_half_saturation = 0.80,
                          cvalue_active_quantile = 0.75,
                          cvalue_hybrid_industry_weight = NA_real_,
                          observed_diminishing_returns = TRUE,
                          observed_diminishing_min_obs_per_bin = 8L,
                          flatten_when_no_observed_diminishing = TRUE,
                          flat_cvalue_multiplier = 0.15,
                          flat_cvalue_rel_tol = 0.02,
                          flat_cvalue_abs_tol = 0.001,
                          future_spend_placebo_guard = TRUE,
                          future_spend_placebo_lead = 4L,
                          future_spend_placebo_warning_ratio = 0.75,
                          future_spend_placebo_review_ratio = 0.40,
                          pooled_ramp_group_col = NULL,
                          pooled_ramp_min_groups = 2L,
                          pooled_ramp_min_obs_per_group = 26L,
                          pooled_ramp_min_usable_groups = 2L,
                          pooled_ramp_min_total_weight = 0.10,
                          pooled_ramp_blend_max = 0.65,
                          pooled_ramp_precision_multiplier_max = 4,
                          coef_center_shrinkage = 1,
                          spend_level_guard = TRUE,
                          spend_guard_recent_n = 13L,
                          under_spend_active_share_threshold = 0.20,
                          under_spend_max_saturation_threshold = 0.65,
                          under_spend_p90_to_median_threshold = 1.25,
                          under_spend_ramp_share_threshold = 0.08,
                          over_spend_saturation_threshold = 0.85,
                          over_spend_week_share_threshold = 0.35,
                          over_spend_recent_saturation_threshold = 0.80,
                          sanity_bounds = NULL,
                          elasticity_sanity_checks = TRUE,
                          default_max_abs_contribution_share = 0.75,
                          default_max_abs_elasticity = 3.0,
                          curve_anchors = NULL,
                          dvalue_default = 1,
                          use_fourier = TRUE,
                          fourier_period = 52.18,
                          fourier_K = 2,
                          use_holidays = TRUE,
                          holiday_window_weeks = c(-1, 0, 1),
                          week_end_day = "Sunday",
                          use_week_of_month = TRUE,
                          floor_value = 0.00005,
                          total_level_shared_curve = TRUE,
                          duplicate_date_non_additive_strategy = c("mean", "first"),
                          missing_data_policy = c("warn_keep", "linear_interpolate", "zero_fill", "drop_rows"),
                          rrate_precision_default = 1,
                          cvalue_precision_default = 1,
                          dvalue_precision_default = 1,
                          diagnose_collinearity = TRUE,
                          adjust_priors_for_collinearity = TRUE,
                          collinearity_threshold = 0.85,
                          severe_collinearity_threshold = 0.95,
                          vif_threshold = 5,
                          severe_vif_threshold = 10,
                          multivariate_coef_scan = TRUE,
                          multivariate_coef_prior_mode = c("diagnostic", "auto", "always", "never"),
                          multivariate_coef_lambda_grid = 10 ^ seq(-4, 3, length.out = 32),
                          multivariate_coef_min_obs = 30L,
                          return_wrapper = TRUE,
                          verbose = FALSE,
                          ...) {
  rrate_search_method <- match.arg(rrate_search_method)
  estimate_cvalue_from_data <- match.arg(estimate_cvalue_from_data)
  cvalue_anchor_method <- match.arg(cvalue_anchor_method)
  duplicate_date_non_additive_strategy <- match.arg(duplicate_date_non_additive_strategy)
  missing_data_policy <- match.arg(missing_data_policy)
  multivariate_coef_prior_mode <- match.arg(multivariate_coef_prior_mode)
  if (identical(multivariate_coef_prior_mode, "never")) multivariate_coef_scan <- FALSE
  sanity_bounds <- pb_clean_sanity_bounds(sanity_bounds)

  if (is.null(input_data)) {
    if (is.null(input_file)) stop("Provide input_data or input_file.")
    if (grepl("\\.csv$", input_file, ignore.case = TRUE)) {
      dt <- data.table::fread(input_file)
    } else if (grepl("\\.xlsx?$", input_file, ignore.case = TRUE)) {
      if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' is required for Excel input.")
      dt <- data.table::as.data.table(readxl::read_excel(input_file, sheet = sheet %||% 1))
    } else {
      stop("Unsupported input_file type. Use csv/xlsx or pass input_data.")
    }
  } else {
    dt <- data.table::as.data.table(data.table::copy(input_data))
  }

  vm <- pb_parse_variable_map(variable_map, media_cols, curve_cols)
  if (!length(media_cols)) media_cols <- vm$variable
  if (is.null(curve_cols)) curve_cols <- vm$variable
  curve_cols <- intersect(curve_cols, vm$variable)
  vm[, has_curve := variable %in% curve_cols]
  curve_anchor_overrides <- pb_variable_map_curve_anchor_overrides(vm)
  curve_anchors <- pb_merge_curve_anchor_overrides(curve_anchors, curve_anchor_overrides)

  required_raw <- unique(c(date_col, dep_var_col, holdout_col, vm$modeled_x_col, vm$spend_col, vm$support_col, base_cols, control_cols))
  required_raw <- required_raw[!is.na(required_raw) & nzchar(required_raw)]
  missing_raw <- setdiff(required_raw, names(dt))
  if (length(missing_raw)) stop("Columns missing from input_data: ", paste(missing_raw, collapse = ", "))
  input_row_n <- nrow(dt)
  holdout_info <- pb_apply_holdout_filter(
    dt = dt,
    date_col = date_col,
    holdout_col = holdout_col,
    holdout_value = holdout_value,
    holdout_last_n = holdout_last_n
  )
  dt <- holdout_info$data

  model_dt <- pb_make_model_dt(
    dt, date_col, dep_var_col, vm, base_cols, control_cols, total_level_shared_curve,
    duplicate_date_non_additive_strategy = duplicate_date_non_additive_strategy
  )
  map_dt <- attr(model_dt, "map_dt")
  missing_predictor_cols <- unique(c(vm$variable, map_dt$spend_internal, map_dt$support_internal))
  missing_predictor_cols <- missing_predictor_cols[!is.na(missing_predictor_cols) & missing_predictor_cols %in% names(model_dt)]
  missing_handled <- pb_apply_missing_data_policy(
    model_dt = model_dt,
    predictor_cols = missing_predictor_cols,
    control_cols = unique(c(base_cols, control_cols)),
    policy = missing_data_policy
  )
  model_dt <- missing_handled$data
  attr(model_dt, "map_dt") <- map_dt
  missing_data_diagnostics <- missing_handled$diagnostics
  missing_share_for <- function(col, stage = c("before", "after")) {
    stage <- match.arg(stage)
    if (is.null(col) || is.na(col) || !nzchar(col) || !nrow(missing_data_diagnostics)) return(NA_real_)
    z <- missing_data_diagnostics[column == col]
    if (!nrow(z)) return(NA_real_)
    field <- if (identical(stage, "before")) "missing_share_before" else "missing_share_after"
    val <- suppressWarnings(as.numeric(z[[field]][1]))
    if (is.finite(val)) val else NA_real_
  }
  y_raw <- model_dt$y__
  y_mi <- pb_mean_index(y_raw)

  built_controls <- pb_build_controls(
    model_dt$date__,
    use_fourier = use_fourier,
    fourier_period = fourier_period,
    fourier_K = fourier_K,
    use_holidays = use_holidays,
    holiday_window_weeks = holiday_window_weeks,
    use_week_of_month = use_week_of_month,
    week_end_day = week_end_day
  )
  controls_metadata <- attr(built_controls, "control_metadata")
  controls_dropped_zero_variance <- attr(built_controls, "controls_dropped_zero_variance")

  user_controls <- NULL
  user_control_cols <- unique(c(base_cols, control_cols))
  if (length(user_control_cols)) user_controls <- as.matrix(model_dt[, ..user_control_cols])
  controls <- cbind(built_controls, user_controls)
  if (is.null(dim(controls))) controls <- matrix(controls, ncol = 1)
  if (!ncol(controls)) controls <- matrix(numeric(0), nrow = nrow(model_dt), ncol = 0)

  priors <- list()
  evidence <- list()
  support_diags <- list()
  transformed <- list()
  pooled_group_ramp_details <- list()
  pooled_group_ramp_summaries <- list()

  for (i in seq_len(nrow(vm))) {
    v <- vm$variable[i]
    if (verbose) message("Estimating prior for ", v)
    raw_x <- as.numeric(model_dt[[v]])
    bound <- pb_get_bound(coef_bounds, v, "free")
    has_curve <- v %in% curve_cols
    curve_type <- pb_normalize_curve_type(vm$curve_type[i] %||% "weibull")[1]
    dvalue <- as.numeric(dvalue_default)[1]
    anchor_info <- pb_anchor_info_for(curve_anchors, v)
    anchor <- if (has_curve) pb_anchor_for(curve_anchors, v, 0.50) else NA_real_
    anchor_cvalue_precision_multiplier <- if (has_curve) pb_anchor_cvalue_precision_multiplier(anchor_info) else 1

    support_row <- map_dt[variable == v]
    modeled_missing_share_before <- missing_share_for(v, "before")
    modeled_missing_share_after <- missing_share_for(v, "after")
    spend_missing_share_before <- missing_share_for(support_row$spend_internal[1], "before")
    spend_missing_share_after <- missing_share_for(support_row$spend_internal[1], "after")
    support_missing_share_before <- missing_share_for(support_row$support_internal[1], "before")
    support_missing_share_after <- missing_share_for(support_row$support_internal[1], "after")
    missing_guard <- pb_missing_data_guard_one(
      modeled_before = modeled_missing_share_before,
      modeled_after = modeled_missing_share_after,
      spend_before = spend_missing_share_before,
      spend_after = spend_missing_share_after,
      support_before = support_missing_share_before,
      support_after = support_missing_share_after,
      policy = missing_data_policy
    )
    support_diags[[v]] <- pb_support_diag_one(
      model_dt, v,
      spend_col_internal = support_row$spend_internal[1],
      support_col_internal = support_row$support_internal[1],
      modeled_x_col_internal = v,
      support_type = vm$support_type[i],
      modeled_x_basis = vm$modeled_x_basis[i]
    )

    rrate <- NA_real_; cvalue <- NA_real_; x_handoff <- pb_mean_index(raw_x); rrate_method <- "not_curved"
    rrate_raw_best <- NA_real_; rrate_score_best <- NA_real_; rrate_score_selected <- NA_real_; rrate_score_upper <- NA_real_
    rrate_at_upper_bound <- FALSE; rrate_plateau_adjusted <- FALSE; rrate_selection_reason <- "not_curved"
    cvalue_anchor <- NA_real_; cvalue_data_driven <- NA_real_; cvalue_final_source <- "not_curved"
    cvalue_data_reason <- "not_curved"; cvalue_data_score_anchor <- NA_real_; cvalue_data_score_best <- NA_real_
    cvalue_data_improvement <- NA_real_; cvalue_data_multiplier <- NA_real_
    cvalue_flat_candidate <- NA_real_; cvalue_flat_candidate_used <- FALSE
    preferred_cvalue <- NA_real_; preferred_cvalue_used <- FALSE; preferred_cvalue_source <- NA_character_; preferred_cvalue_reliability <- 0
    pooled_ramp_summary <- data.table::data.table(
      variable = v,
      pooled_ramp_group_col = as.character(pooled_ramp_group_col %||% NA_character_),
      pooled_ramp_groups_tested = 0L,
      pooled_ramp_usable_groups = 0L,
      pooled_ramp_supports_diminishing_groups = 0L,
      pooled_ramp_flat_groups = 0L,
      pooled_ramp_flat_negative_groups = 0L,
      pooled_ramp_conflict_groups = 0L,
      pooled_ramp_placebo_warning_groups = 0L,
      pooled_ramp_weight_total = 0,
      pooled_ramp_reliability = 0,
      pooled_ramp_cvalue = NA_real_,
      pooled_ramp_cvalue_blend_weight = 0,
      pooled_ramp_precision_multiplier = 1,
      pooled_ramp_evidence_class = "not_requested",
      pooled_ramp_reason = "pooled_group_ramp_not_requested"
    )
    cvalue_anchor_x <- NA_real_; cvalue_anchor_method_used <- NA_character_; cvalue_anchor_data_weight <- NA_real_; cvalue_anchor_active_cv <- NA_real_
    ramp_flag <- rep(FALSE, length(raw_x)); ramp_n <- 0L; ramp_share <- 0; ramp_variation_score <- NA_real_
    observed_curve_evidence_class <- NA_character_; observed_marginal_slope_low <- NA_real_; observed_marginal_slope_high <- NA_real_
    observed_marginal_slope_ratio <- NA_real_; observed_slope_cvalue <- NA_real_; observed_diminishing_returns_score <- NA_real_
    observed_low_spend_n <- 0L; observed_high_spend_n <- 0L; observed_low_spend_x <- NA_real_; observed_high_spend_x <- NA_real_
    observed_spend_spread_p90_p10 <- NA_real_
    if (has_curve) {
      fr <- pb_fixed_rrate(fixed_rrate_by_var, v)
      if (is.finite(fr)) {
        rrate <- pb_clip(fr, 0, 0.99)
        rrate_method <- "fixed_rrate_by_var"
        rrate_raw_best <- rrate
        rrate_selection_reason <- "fixed_rrate_by_var"
      } else if (isTRUE(estimate_rrate)) {
        rrate_est <- pb_estimate_rrate(
          y_mi, raw_x, controls, anchor, dvalue, bound, rrate_bounds, rrate_search_method, rrate_grid_n, wrong_sign_rrate_penalty,
          upper_bound_fallback = rrate_upper_bound_fallback,
          upper_bound_tolerance = rrate_upper_bound_tolerance,
          plateau_rel_tol = rrate_plateau_rel_tol,
          plateau_abs_tol = rrate_plateau_abs_tol,
          plateau_grid_n = rrate_plateau_grid_n,
          cvalue_anchor_method = cvalue_anchor_method,
          cvalue_industry_half_saturation = cvalue_industry_half_saturation,
          cvalue_active_quantile = cvalue_active_quantile,
          cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
          curve_type = curve_type
        )
        rdiag <- attr(rrate_est, "rrate_diagnostics")
        rrate <- as.numeric(rrate_est)
        rrate_raw_best <- as.numeric(rdiag$rrate_raw_best %||% rrate)
        rrate_score_best <- as.numeric(rdiag$rrate_score_best %||% NA_real_)
        rrate_score_selected <- as.numeric(rdiag$rrate_score_selected %||% NA_real_)
        rrate_score_upper <- as.numeric(rdiag$rrate_score_upper %||% NA_real_)
        rrate_at_upper_bound <- isTRUE(rdiag$rrate_at_upper_bound)
        rrate_plateau_adjusted <- isTRUE(rdiag$rrate_plateau_adjusted)
        rrate_selection_reason <- as.character(rdiag$rrate_selection_reason %||% "estimated_rrate")
        rrate_method <- paste0("estimated_", rrate_search_method)
      } else {
        rrate <- 0.20
        rrate_method <- "default_0.20"
        rrate_raw_best <- rrate
        rrate_selection_reason <- "default_0.20"
      }
      if (!is.null(pooled_ramp_group_col) && nzchar(as.character(pooled_ramp_group_col)[1]) &&
          as.character(pooled_ramp_group_col)[1] %in% names(dt)) {
        pooled <- pb_pooled_group_ramp_evidence_one(
          input_data = dt,
          date_col = date_col,
          dep_var_col = dep_var_col,
          variable_row = vm[i],
          group_col = pooled_ramp_group_col,
          rrate = rrate,
          anchor_saturation = anchor,
          dvalue = dvalue,
          bound = bound,
          base_cols = base_cols,
          control_cols = control_cols,
          use_fourier = use_fourier,
          fourier_period = fourier_period,
          fourier_K = fourier_K,
          use_holidays = use_holidays,
          holiday_window_weeks = holiday_window_weeks,
          week_end_day = week_end_day,
          use_week_of_month = use_week_of_month,
          cvalue_anchor_method = cvalue_anchor_method,
          cvalue_industry_half_saturation = cvalue_industry_half_saturation,
          cvalue_active_quantile = cvalue_active_quantile,
          cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
          curve_type = curve_type,
          cvalue_search_multipliers = cvalue_search_multipliers,
          cvalue_search_bounds = cvalue_search_bounds,
          cvalue_grid_n = cvalue_grid_n,
          cvalue_min_score_improvement = cvalue_min_score_improvement,
          cvalue_anchor_penalty = cvalue_anchor_penalty,
          cvalue_ramp_min_abs_change_index = cvalue_ramp_min_abs_change_index,
          cvalue_ramp_min_pct_change = cvalue_ramp_min_pct_change,
          cvalue_ramp_window = cvalue_ramp_window,
          cvalue_ramp_weight = cvalue_ramp_weight,
          cvalue_min_ramp_points = min(6L, as.integer(cvalue_min_ramp_points)),
          cvalue_min_ramp_share = min(0.05, as.numeric(cvalue_min_ramp_share)),
          observed_diminishing_min_obs_per_bin = min(6L, as.integer(observed_diminishing_min_obs_per_bin)),
          min_groups = pooled_ramp_min_groups,
          min_obs_per_group = pooled_ramp_min_obs_per_group,
          min_usable_groups = pooled_ramp_min_usable_groups,
          min_total_weight = pooled_ramp_min_total_weight,
          blend_max = pooled_ramp_blend_max,
          precision_multiplier_max = pooled_ramp_precision_multiplier_max,
          future_spend_placebo_lead = future_spend_placebo_lead,
          future_spend_placebo_warning_ratio = future_spend_placebo_warning_ratio,
          future_spend_placebo_review_ratio = future_spend_placebo_review_ratio
        )
        pooled_ramp_summary <- pooled$summary
        pooled_group_ramp_summaries[[v]] <- pooled$summary
        pooled_group_ramp_details[[v]] <- pooled$details
        if (nrow(pooled$summary) &&
            pooled$summary$pooled_ramp_evidence_class[1] %in% c("pooled_supports_flatter_curve", "pooled_supports_diminishing_returns") &&
            is.finite(pooled$summary$pooled_ramp_cvalue[1]) &&
            pooled$summary$pooled_ramp_reliability[1] >= 0.10) {
          preferred_cvalue <- pooled$summary$pooled_ramp_cvalue[1]
          preferred_cvalue_source <- paste0("pooled_group_ramp_", pooled$summary$pooled_ramp_evidence_class[1])
          preferred_cvalue_reliability <- pooled$summary$pooled_ramp_reliability[1]
        }
      }
      cvalue_result <- pb_estimate_cvalue_from_data(
        y_mi = y_mi,
        x_raw = raw_x,
        controls = controls,
        rrate = rrate,
        anchor_saturation = anchor,
        dvalue = dvalue,
        bound = bound,
        wrong_sign_rrate_penalty = wrong_sign_rrate_penalty,
        mode = estimate_cvalue_from_data,
        search_multipliers = cvalue_search_multipliers,
        search_bounds = cvalue_search_bounds,
        grid_n = cvalue_grid_n,
        min_score_improvement = cvalue_min_score_improvement,
        anchor_penalty = cvalue_anchor_penalty,
        ramp_min_abs_change_index = cvalue_ramp_min_abs_change_index,
        ramp_min_pct_change = cvalue_ramp_min_pct_change,
        ramp_window = cvalue_ramp_window,
        ramp_weight = cvalue_ramp_weight,
        min_ramp_points = cvalue_min_ramp_points,
        min_ramp_share = cvalue_min_ramp_share,
        cvalue_anchor_method = cvalue_anchor_method,
        cvalue_industry_half_saturation = cvalue_industry_half_saturation,
        cvalue_active_quantile = cvalue_active_quantile,
        cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
        curve_type = curve_type,
        use_observed_diminishing_returns = observed_diminishing_returns,
        observed_diminishing_min_obs_per_bin = observed_diminishing_min_obs_per_bin,
        flatten_when_no_observed_diminishing = flatten_when_no_observed_diminishing,
        flat_cvalue_multiplier = flat_cvalue_multiplier,
        flat_cvalue_rel_tol = flat_cvalue_rel_tol,
        flat_cvalue_abs_tol = flat_cvalue_abs_tol,
        extra_cvalue_candidates = pooled_ramp_summary$pooled_ramp_cvalue[is.finite(pooled_ramp_summary$pooled_ramp_cvalue)],
        preferred_cvalue = preferred_cvalue,
        preferred_cvalue_source = preferred_cvalue_source,
        preferred_cvalue_reliability = preferred_cvalue_reliability
      )
      fit_final <- cvalue_result$fit
      cvalue <- fit_final$cvalue
      coef_raw <- fit_final$coef
      x_handoff <- fit_final$x_handoff
      cvalue_anchor <- cvalue_result$cvalue_anchor
      cvalue_data_driven <- cvalue_result$cvalue_data_driven
      cvalue_final_source <- cvalue_result$cvalue_final_source
      cvalue_data_reason <- cvalue_result$cvalue_data_reason
      cvalue_data_score_anchor <- cvalue_result$cvalue_data_score_anchor
      cvalue_data_score_best <- cvalue_result$cvalue_data_score_best
      cvalue_data_improvement <- cvalue_result$cvalue_data_improvement
      cvalue_data_multiplier <- cvalue_result$cvalue_data_multiplier
      cvalue_flat_candidate <- cvalue_result$cvalue_flat_candidate
      cvalue_flat_candidate_used <- cvalue_result$cvalue_flat_candidate_used
      preferred_cvalue <- cvalue_result$preferred_cvalue
      preferred_cvalue_used <- cvalue_result$preferred_cvalue_used
      preferred_cvalue_source <- cvalue_result$preferred_cvalue_source
      preferred_cvalue_reliability <- cvalue_result$preferred_cvalue_reliability
      ramp_flag <- cvalue_result$ramp_flag
      ramp_n <- cvalue_result$ramp_n
      ramp_share <- cvalue_result$ramp_share
      ramp_variation_score <- cvalue_result$ramp_variation_score
      cvalue_anchor_x <- cvalue_result$cvalue_anchor_x
      cvalue_anchor_method_used <- cvalue_result$cvalue_anchor_method
      cvalue_anchor_data_weight <- cvalue_result$cvalue_anchor_data_weight
      cvalue_anchor_active_cv <- cvalue_result$cvalue_anchor_active_cv
      observed_curve_evidence_class <- cvalue_result$observed_curve_evidence_class
      observed_marginal_slope_low <- cvalue_result$observed_marginal_slope_low
      observed_marginal_slope_high <- cvalue_result$observed_marginal_slope_high
      observed_marginal_slope_ratio <- cvalue_result$observed_marginal_slope_ratio
      observed_slope_cvalue <- cvalue_result$observed_slope_cvalue
      observed_diminishing_returns_score <- cvalue_result$observed_diminishing_returns_score
      observed_low_spend_n <- cvalue_result$observed_low_spend_n
      observed_high_spend_n <- cvalue_result$observed_high_spend_n
      observed_low_spend_x <- cvalue_result$observed_low_spend_x
      observed_high_spend_x <- cvalue_result$observed_high_spend_x
      observed_spend_spread_p90_p10 <- cvalue_result$observed_spend_spread_p90_p10
    } else {
      X <- cbind(intercept = 1, controls, x_handoff = x_handoff)
      ok <- stats::complete.cases(y_mi, X)
      if (sum(ok) < max(8L, ncol(X) + 3L)) {
        coef_raw <- NA_real_
      } else {
        fit <- stats::lm.fit(X[ok, , drop = FALSE], y_mi[ok])
        coef_raw <- as.numeric(tail(fit$coefficients, 1))
      }
    }

    status <- "ok"
    coef_prior <- coef_raw
    if (!is.finite(coef_raw)) {
      status <- "fit_failed"
      coef_prior <- NA_real_
    } else if (bound %in% c("pos", "positive", "+") && coef_raw <= 0) {
      status <- "coef_replaced_to_positive_floor"
      coef_prior <- abs(floor_value)
    } else if (bound %in% c("neg", "negative", "-") && coef_raw >= 0) {
      status <- "coef_replaced_to_negative_floor"
      coef_prior <- -abs(floor_value)
    }
    coef_prior_unshrunk <- coef_prior
    shrinkage <- pb_coef_shrinkage_for(coef_center_shrinkage, v, default = 1)
    if (identical(status, "ok") && is.finite(coef_prior) && is.finite(shrinkage) && shrinkage > 0 && abs(shrinkage - 1) > 1e-12) {
      coef_prior <- coef_prior * shrinkage
    }

    active_share <- mean(is.finite(raw_x) & raw_x > 0, na.rm = TRUE)
    total_x <- sum(pmax(raw_x, 0), na.rm = TRUE)
    top4 <- if (is.finite(total_x) && total_x > 0) sum(utils::head(sort(pmax(raw_x, 0), decreasing = TRUE), 4), na.rm = TRUE) / total_x else NA_real_
    partial_r2 <- pb_partial_r2(y_mi, x_handoff, controls)
    boot_stab <- pb_boot_sign_stability(y_mi, x_handoff, controls, bound, target_sign = coef_raw, reps = 40L, seed = 100 + i)
    placebo <- if (isTRUE(future_spend_placebo_guard) && isTRUE(has_curve)) {
      pb_future_spend_placebo(
        y_mi = y_mi,
        x_handoff = x_handoff,
        controls = controls,
        lead = future_spend_placebo_lead,
        bound = bound,
        target_sign = coef_raw,
        warning_ratio = future_spend_placebo_warning_ratio,
        review_ratio = future_spend_placebo_review_ratio
      )
    } else {
      pb_future_spend_placebo(numeric(0), numeric(0), controls, lead = 0L)
    }
    spend_guard <- if (isTRUE(spend_level_guard) && isTRUE(has_curve)) {
      pb_spend_level_guard(
        x_raw = raw_x,
        rrate = rrate,
        cvalue = cvalue,
        dvalue = dvalue,
        curve_type = curve_type,
        active_share = active_share,
        ramp_share = ramp_share,
        recent_n = spend_guard_recent_n,
        under_active_share_threshold = under_spend_active_share_threshold,
        under_max_saturation_threshold = under_spend_max_saturation_threshold,
        under_p90_to_median_threshold = under_spend_p90_to_median_threshold,
        under_ramp_share_threshold = under_spend_ramp_share_threshold,
        over_saturation_threshold = over_spend_saturation_threshold,
        over_saturation_week_share_threshold = over_spend_week_share_threshold,
        over_recent_saturation_threshold = over_spend_recent_saturation_threshold
      )
    } else {
      pb_spend_level_guard(numeric(0))
    }
    spend_for_sanity <- if (!is.na(support_row$spend_internal[1]) && support_row$spend_internal[1] %in% names(model_dt)) {
      model_dt[[support_row$spend_internal[1]]]
    } else {
      raw_x
    }
    sanity <- if (isTRUE(elasticity_sanity_checks)) {
      pb_curve_sanity_check_one(
        variable = v,
        y_raw = y_raw,
        x_raw = raw_x,
        x_handoff = x_handoff,
        coef = coef_prior,
        spend = spend_for_sanity,
        has_curve = has_curve,
        rrate = rrate,
        cvalue = cvalue,
        dvalue = dvalue,
        anchor_saturation = anchor,
        sanity_bounds = sanity_bounds,
        default_max_abs_contribution_share = default_max_abs_contribution_share,
        default_max_abs_elasticity = default_max_abs_elasticity,
        cvalue_anchor_method = cvalue_anchor_method,
        cvalue_industry_half_saturation = cvalue_industry_half_saturation,
        cvalue_active_quantile = cvalue_active_quantile,
        cvalue_hybrid_industry_weight = cvalue_hybrid_industry_weight,
        curve_type = curve_type
      )
    } else {
      data.table::data.table(
        implied_contribution_total = NA_real_,
        implied_contribution_share = NA_real_,
        implied_elasticity = NA_real_,
        implied_spend_total = NA_real_,
        implied_cost_per_outcome = NA_real_,
        implied_outcome_per_cost = NA_real_,
        sanity_bound_class = "not_run",
        sanity_bound_flags = "",
        sanity_bound_risk_multiplier = 1
      )
    }

    # Conservative final handoff: floor/failed variables must not get tight precision.
    risk <- 1
    if (!identical(status, "ok")) risk <- max(risk, 50)
    if (is.finite(active_share) && active_share < 0.20) risk <- max(risk, 10)
    if (is.finite(top4) && top4 > 0.60) risk <- max(risk, 10)
    if (is.finite(partial_r2) && partial_r2 < 0.0025) risk <- max(risk, 8)
    if (is.finite(boot_stab) && boot_stab < 0.70) risk <- max(risk, 12)
    if (nrow(spend_guard) && is.finite(spend_guard$spend_guard_risk_multiplier[1])) {
      risk <- max(risk, spend_guard$spend_guard_risk_multiplier[1])
    }
    if (nrow(missing_guard) && is.finite(missing_guard$missing_data_risk_multiplier[1])) {
      risk <- max(risk, missing_guard$missing_data_risk_multiplier[1])
    }
    if (nrow(sanity) && is.finite(sanity$sanity_bound_risk_multiplier[1])) {
      risk <- max(risk, sanity$sanity_bound_risk_multiplier[1])
    }
    if (isTRUE(has_curve) && identical(observed_curve_evidence_class, "contradicts_diminishing_returns")) {
      risk <- max(risk, 4)
    }
    if (isTRUE(has_curve) && placebo$future_spend_placebo_class[1] %in% c("future_spend_placebo_warning", "future_spend_stronger_than_current_warning")) {
      risk <- max(risk, 8)
    } else if (isTRUE(has_curve) && identical(placebo$future_spend_placebo_class[1], "future_spend_placebo_review")) {
      risk <- max(risk, 4)
    }

    base_precision <- if (is.finite(coef_prior_unshrunk) && abs(coef_prior_unshrunk) > 1e-12 && identical(status, "ok")) min(1 / (coef_prior_unshrunk ^ 2), 1e6) else 1
    if (identical(status, "ok") && is.finite(shrinkage) && shrinkage < 1) base_precision <- base_precision * shrinkage ^ 2
    coef_precision_final <- base_precision / risk
    if (!identical(status, "ok")) coef_precision_final <- min(coef_precision_final, 1)

    curve_precision_scale <- if (isTRUE(has_curve)) 1 / risk else NA_real_
    rrate_precision_final <- if (isTRUE(has_curve)) as.numeric(rrate_precision_default)[1] * curve_precision_scale else NA_real_
    cvalue_precision_final <- if (isTRUE(has_curve)) as.numeric(cvalue_precision_default)[1] * curve_precision_scale * anchor_cvalue_precision_multiplier else NA_real_
    dvalue_precision_final <- if (isTRUE(has_curve)) as.numeric(dvalue_precision_default)[1] * curve_precision_scale else NA_real_
    if (isTRUE(has_curve) && isTRUE(preferred_cvalue_used) &&
        nrow(pooled_ramp_summary) && is.finite(pooled_ramp_summary$pooled_ramp_precision_multiplier[1])) {
      cvalue_precision_final <- cvalue_precision_final * pooled_ramp_summary$pooled_ramp_precision_multiplier[1]
    }
    stan_observed_cvalue <- if (isTRUE(has_curve) && is.finite(preferred_cvalue)) {
      preferred_cvalue
    } else if (isTRUE(has_curve) && is.finite(cvalue_data_driven)) {
      cvalue_data_driven
    } else if (isTRUE(has_curve) && is.finite(observed_slope_cvalue)) {
      observed_slope_cvalue
    } else {
      NA_real_
    }
    stan_observed_cvalue_reliability <- if (isTRUE(has_curve) && isTRUE(preferred_cvalue_used)) {
      preferred_cvalue_reliability
    } else if (isTRUE(has_curve) && is.finite(cvalue_data_improvement) && cvalue_data_improvement > 0) {
      pb_clip(cvalue_data_improvement * 3, 0.10, 0.60)
    } else if (isTRUE(has_curve) && is.finite(observed_slope_cvalue)) {
      0.25
    } else {
      0
    }
    stan_observed_cvalue_source <- if (isTRUE(has_curve) && isTRUE(preferred_cvalue_used)) {
      preferred_cvalue_source
    } else if (isTRUE(has_curve) && is.finite(cvalue_data_driven)) {
      cvalue_final_source
    } else if (isTRUE(has_curve) && is.finite(observed_slope_cvalue)) {
      "observed_low_high_slope_cvalue"
    } else {
      NA_character_
    }
    if (isTRUE(has_curve) && is.finite(stan_observed_cvalue_reliability) && is.finite(risk) && risk > 1) {
      stan_observed_cvalue_reliability <- stan_observed_cvalue_reliability / sqrt(risk)
    }

    tier <- if (!identical(status, "ok")) {
      "directional_only_or_manual_review"
    } else if (risk >= 12) {
      "loose_directional_prior"
    } else if (risk >= 8) {
      "use_with_extra_relaxation"
    } else {
      "usable_prior"
    }

    priors[[v]] <- data.table::data.table(
      variable = v,
      role = "media",
      has_curve = has_curve,
      curve_type = curve_type,
      rrate = rrate,
      rrate_raw_best = rrate_raw_best,
      rrate_score_best = rrate_score_best,
      rrate_score_selected = rrate_score_selected,
      rrate_score_upper = rrate_score_upper,
      rrate_at_upper_bound = rrate_at_upper_bound,
      rrate_plateau_adjusted = rrate_plateau_adjusted,
      rrate_selection_reason = rrate_selection_reason,
      cvalue = cvalue,
      cvalue_anchor = cvalue_anchor,
      cvalue_data_driven = cvalue_data_driven,
      cvalue_final_source = cvalue_final_source,
      cvalue_data_reason = cvalue_data_reason,
      cvalue_data_score_anchor = cvalue_data_score_anchor,
      cvalue_data_score_best = cvalue_data_score_best,
      cvalue_data_improvement = cvalue_data_improvement,
      cvalue_data_multiplier = cvalue_data_multiplier,
      cvalue_flat_candidate = cvalue_flat_candidate,
      cvalue_flat_candidate_used = cvalue_flat_candidate_used,
      preferred_cvalue = preferred_cvalue,
      preferred_cvalue_used = preferred_cvalue_used,
      preferred_cvalue_source = preferred_cvalue_source,
      preferred_cvalue_reliability = preferred_cvalue_reliability,
      pooled_ramp_group_col = pooled_ramp_summary$pooled_ramp_group_col[1],
      pooled_ramp_groups_tested = pooled_ramp_summary$pooled_ramp_groups_tested[1],
      pooled_ramp_usable_groups = pooled_ramp_summary$pooled_ramp_usable_groups[1],
      pooled_ramp_supports_diminishing_groups = pooled_ramp_summary$pooled_ramp_supports_diminishing_groups[1],
      pooled_ramp_flat_groups = pooled_ramp_summary$pooled_ramp_flat_groups[1],
      pooled_ramp_flat_negative_groups = pooled_ramp_summary$pooled_ramp_flat_negative_groups[1],
      pooled_ramp_conflict_groups = pooled_ramp_summary$pooled_ramp_conflict_groups[1],
      pooled_ramp_placebo_warning_groups = pooled_ramp_summary$pooled_ramp_placebo_warning_groups[1],
      pooled_ramp_weight_total = pooled_ramp_summary$pooled_ramp_weight_total[1],
      pooled_ramp_reliability = pooled_ramp_summary$pooled_ramp_reliability[1],
      pooled_ramp_cvalue = pooled_ramp_summary$pooled_ramp_cvalue[1],
      pooled_ramp_cvalue_blend_weight = pooled_ramp_summary$pooled_ramp_cvalue_blend_weight[1],
      pooled_ramp_precision_multiplier = pooled_ramp_summary$pooled_ramp_precision_multiplier[1],
      pooled_ramp_evidence_class = pooled_ramp_summary$pooled_ramp_evidence_class[1],
      pooled_ramp_reason = pooled_ramp_summary$pooled_ramp_reason[1],
      stan_observed_cvalue = stan_observed_cvalue,
      stan_observed_cvalue_source = stan_observed_cvalue_source,
      stan_observed_cvalue_reliability = stan_observed_cvalue_reliability,
      cvalue_anchor_x = cvalue_anchor_x,
      cvalue_anchor_method = cvalue_anchor_method_used,
      cvalue_anchor_data_weight = cvalue_anchor_data_weight,
      cvalue_anchor_active_cv = cvalue_anchor_active_cv,
      dvalue = if (has_curve) dvalue else NA_real_,
      rrate_precision_final = rrate_precision_final,
      cvalue_precision_final = cvalue_precision_final,
      dvalue_precision_final = dvalue_precision_final,
      anchor_saturation = anchor,
      coef_raw = coef_raw,
      coef_prior_unshrunk = coef_prior_unshrunk,
      coef_prior = coef_prior,
      coef_prior_half_sensitivity = if (is.finite(coef_prior_unshrunk)) 0.5 * coef_prior_unshrunk else NA_real_,
      coef_center_shrinkage = shrinkage,
      coef_precision = base_precision,
      coef_prior_final = coef_prior,
      coef_precision_final = coef_precision_final,
      coef_bound = bound,
      status = status,
      modeled_x_col = vm$modeled_x_col[i],
      modeled_x_basis = vm$modeled_x_basis[i],
      spend_col = vm$spend_col[i],
      support_col = vm$support_col[i],
      support_type = vm$support_type[i],
      missing_data_policy = missing_data_policy,
      modeled_x_missing_share_before = modeled_missing_share_before,
      modeled_x_missing_share_after = modeled_missing_share_after,
      spend_missing_share_before = spend_missing_share_before,
      spend_missing_share_after = spend_missing_share_after,
      support_missing_share_before = support_missing_share_before,
      support_missing_share_after = support_missing_share_after,
      missing_data_class = missing_guard$missing_data_class[1],
      missing_data_action = missing_guard$missing_data_action[1],
      max_missing_share_before = missing_guard$max_missing_share_before[1],
      max_missing_share_after = missing_guard$max_missing_share_after[1],
      missing_data_risk_multiplier = missing_guard$missing_data_risk_multiplier[1],
      rrate_method = rrate_method,
      curve_scope = if (isTRUE(total_level_shared_curve)) "total_level_shared_curve_prior" else "input_level_curve_prior",
      curve_rule = if (has_curve) "anchor_saturation_at_median_active_mean_indexed_adstocked_modeled_x" else "uncurved_mean_indexed_x",
      anchor_should_drive_curve_prior = anchor_info$anchor_should_drive_curve_prior,
      anchor_authority_tier = anchor_info$anchor_authority_tier,
      anchor_actionability_tier = anchor_info$anchor_actionability_tier,
      anchor_source = anchor_info$anchor_source,
      anchor_weight_final = anchor_info$anchor_weight_final,
      anchor_reliability = anchor_info$reliability,
      anchor_uncertainty_width_90 = anchor_info$anchor_uncertainty_width_90,
      anchor_lower_90 = anchor_info$anchor_lower_90,
      anchor_upper_90 = anchor_info$anchor_upper_90,
      anchor_cvalue_precision_multiplier = anchor_cvalue_precision_multiplier,
      ramp_period_n = ramp_n,
      ramp_period_share = ramp_share,
      ramp_variation_score = ramp_variation_score,
      observed_curve_evidence_class = observed_curve_evidence_class,
      observed_marginal_slope_low = observed_marginal_slope_low,
      observed_marginal_slope_high = observed_marginal_slope_high,
      observed_marginal_slope_ratio = observed_marginal_slope_ratio,
      observed_slope_cvalue = observed_slope_cvalue,
      observed_diminishing_returns_score = observed_diminishing_returns_score,
      observed_low_spend_n = observed_low_spend_n,
      observed_high_spend_n = observed_high_spend_n,
      observed_low_spend_x = observed_low_spend_x,
      observed_high_spend_x = observed_high_spend_x,
      observed_spend_spread_p90_p10 = observed_spend_spread_p90_p10,
      future_spend_placebo_class = placebo$future_spend_placebo_class[1],
      future_spend_placebo_lead = placebo$future_spend_placebo_lead[1],
      future_spend_placebo_coef = placebo$future_spend_placebo_coef[1],
      future_spend_placebo_partial_r2 = placebo$future_spend_placebo_partial_r2[1],
      future_spend_placebo_ratio = placebo$future_spend_placebo_ratio[1],
      spend_level_class = spend_guard$spend_level_class[1],
      spend_guard_action = spend_guard$spend_guard_action[1],
      spend_guard_risk_multiplier = spend_guard$spend_guard_risk_multiplier[1],
      implied_contribution_total = sanity$implied_contribution_total[1],
      implied_contribution_share = sanity$implied_contribution_share[1],
      implied_elasticity = sanity$implied_elasticity[1],
      implied_spend_total = sanity$implied_spend_total[1],
      implied_cost_per_outcome = sanity$implied_cost_per_outcome[1],
      implied_outcome_per_cost = sanity$implied_outcome_per_cost[1],
      sanity_bound_class = sanity$sanity_bound_class[1],
      sanity_bound_flags = sanity$sanity_bound_flags[1],
      sanity_bound_risk_multiplier = sanity$sanity_bound_risk_multiplier[1],
      observed_saturation_median_active = spend_guard$observed_saturation_median_active[1],
      observed_saturation_p90 = spend_guard$observed_saturation_p90[1],
      observed_saturation_max = spend_guard$observed_saturation_max[1],
      high_saturation_week_share = spend_guard$high_saturation_week_share[1],
      recent_saturation_mean = spend_guard$recent_saturation_mean[1],
      active_spend_p90_to_median = spend_guard$active_spend_p90_to_median[1],
      recent_spend_to_active_median = spend_guard$recent_spend_to_active_median[1],
      under_spend_flag = spend_guard$under_spend_flag[1],
      over_spend_flag = spend_guard$over_spend_flag[1],
      coef_hierarchy_scale = if ("coef_hierarchy_scale" %in% names(vm)) {
        z <- suppressWarnings(as.numeric(vm$coef_hierarchy_scale[i]))
        if (is.finite(z) && z > 0) pb_clip(z, 0.02, 5) else 1
      } else 1,
      coef_prior_handoff_tier = tier,
      coef_prior_handoff_reason = paste0("status=", status, "; risk_multiplier=", risk, "; coef_center_shrinkage=", signif(shrinkage, 4)),
      use_as_production_prior = tier %in% c("usable_prior", "use_with_extra_relaxation"),
      use_as_directional_starting_value = tier %in% c("usable_prior", "use_with_extra_relaxation", "loose_directional_prior", "directional_only_or_manual_review"),
      recommended_prior_strategy = tier,
      identification_class = if (risk <= 1) "stable_signal" else if (risk < 12) "usable_but_uncertain" else "weak_or_fragile_signal"
    )

    evidence[[v]] <- data.table::data.table(
      variable = v,
      curve_type = curve_type,
      coef_raw = coef_raw,
      coef_prior_unshrunk = coef_prior_unshrunk,
      coef_prior_final = coef_prior,
      coef_prior_half_sensitivity = if (is.finite(coef_prior_unshrunk)) 0.5 * coef_prior_unshrunk else NA_real_,
      coef_center_shrinkage = shrinkage,
      coef_precision_final = coef_precision_final,
      status = status,
      rrate = rrate,
      rrate_raw_best = rrate_raw_best,
      rrate_score_best = rrate_score_best,
      rrate_score_selected = rrate_score_selected,
      rrate_score_upper = rrate_score_upper,
      rrate_at_upper_bound = rrate_at_upper_bound,
      rrate_plateau_adjusted = rrate_plateau_adjusted,
      rrate_selection_reason = rrate_selection_reason,
      cvalue = cvalue,
      cvalue_anchor = cvalue_anchor,
      cvalue_data_driven = cvalue_data_driven,
      cvalue_final_source = cvalue_final_source,
      cvalue_data_reason = cvalue_data_reason,
      cvalue_data_score_anchor = cvalue_data_score_anchor,
      cvalue_data_score_best = cvalue_data_score_best,
      cvalue_data_improvement = cvalue_data_improvement,
      cvalue_data_multiplier = cvalue_data_multiplier,
      cvalue_flat_candidate = cvalue_flat_candidate,
      cvalue_flat_candidate_used = cvalue_flat_candidate_used,
      preferred_cvalue = preferred_cvalue,
      preferred_cvalue_used = preferred_cvalue_used,
      preferred_cvalue_source = preferred_cvalue_source,
      preferred_cvalue_reliability = preferred_cvalue_reliability,
      pooled_ramp_group_col = pooled_ramp_summary$pooled_ramp_group_col[1],
      pooled_ramp_groups_tested = pooled_ramp_summary$pooled_ramp_groups_tested[1],
      pooled_ramp_usable_groups = pooled_ramp_summary$pooled_ramp_usable_groups[1],
      pooled_ramp_supports_diminishing_groups = pooled_ramp_summary$pooled_ramp_supports_diminishing_groups[1],
      pooled_ramp_flat_groups = pooled_ramp_summary$pooled_ramp_flat_groups[1],
      pooled_ramp_flat_negative_groups = pooled_ramp_summary$pooled_ramp_flat_negative_groups[1],
      pooled_ramp_conflict_groups = pooled_ramp_summary$pooled_ramp_conflict_groups[1],
      pooled_ramp_placebo_warning_groups = pooled_ramp_summary$pooled_ramp_placebo_warning_groups[1],
      pooled_ramp_weight_total = pooled_ramp_summary$pooled_ramp_weight_total[1],
      pooled_ramp_reliability = pooled_ramp_summary$pooled_ramp_reliability[1],
      pooled_ramp_cvalue = pooled_ramp_summary$pooled_ramp_cvalue[1],
      pooled_ramp_cvalue_blend_weight = pooled_ramp_summary$pooled_ramp_cvalue_blend_weight[1],
      pooled_ramp_precision_multiplier = pooled_ramp_summary$pooled_ramp_precision_multiplier[1],
      pooled_ramp_evidence_class = pooled_ramp_summary$pooled_ramp_evidence_class[1],
      pooled_ramp_reason = pooled_ramp_summary$pooled_ramp_reason[1],
      stan_observed_cvalue = stan_observed_cvalue,
      stan_observed_cvalue_source = stan_observed_cvalue_source,
      stan_observed_cvalue_reliability = stan_observed_cvalue_reliability,
      cvalue_anchor_x = cvalue_anchor_x,
      cvalue_anchor_method = cvalue_anchor_method_used,
      cvalue_anchor_data_weight = cvalue_anchor_data_weight,
      cvalue_anchor_active_cv = cvalue_anchor_active_cv,
      rrate_precision_final = rrate_precision_final,
      cvalue_precision_final = cvalue_precision_final,
      dvalue_precision_final = dvalue_precision_final,
      active_week_share = active_share,
      top4_concentration = top4,
      residualized_partial_r2 = partial_r2,
      bootstrap_sign_stability = boot_stab,
      controls_used = paste(colnames(controls), collapse = ","),
      controls_dropped_zero_variance = paste(controls_dropped_zero_variance, collapse = ","),
      week_end_day = controls_metadata$week_end_day,
      missing_data_policy = missing_data_policy,
      modeled_x_missing_share_before = modeled_missing_share_before,
      modeled_x_missing_share_after = modeled_missing_share_after,
      spend_missing_share_before = spend_missing_share_before,
      spend_missing_share_after = spend_missing_share_after,
      support_missing_share_before = support_missing_share_before,
      support_missing_share_after = support_missing_share_after,
      missing_data_class = missing_guard$missing_data_class[1],
      missing_data_action = missing_guard$missing_data_action[1],
      max_missing_share_before = missing_guard$max_missing_share_before[1],
      max_missing_share_after = missing_guard$max_missing_share_after[1],
      missing_data_risk_multiplier = missing_guard$missing_data_risk_multiplier[1],
      anchor_actionability_tier = anchor_info$anchor_actionability_tier,
      anchor_should_drive_curve_prior = anchor_info$anchor_should_drive_curve_prior,
      anchor_source = anchor_info$anchor_source,
      anchor_cvalue_precision_multiplier = anchor_cvalue_precision_multiplier,
      ramp_period_n = ramp_n,
      ramp_period_share = ramp_share,
      ramp_variation_score = ramp_variation_score,
      observed_curve_evidence_class = observed_curve_evidence_class,
      observed_marginal_slope_low = observed_marginal_slope_low,
      observed_marginal_slope_high = observed_marginal_slope_high,
      observed_marginal_slope_ratio = observed_marginal_slope_ratio,
      observed_slope_cvalue = observed_slope_cvalue,
      observed_diminishing_returns_score = observed_diminishing_returns_score,
      observed_low_spend_n = observed_low_spend_n,
      observed_high_spend_n = observed_high_spend_n,
      observed_low_spend_x = observed_low_spend_x,
      observed_high_spend_x = observed_high_spend_x,
      observed_spend_spread_p90_p10 = observed_spend_spread_p90_p10,
      future_spend_placebo_class = placebo$future_spend_placebo_class[1],
      future_spend_placebo_lead = placebo$future_spend_placebo_lead[1],
      future_spend_placebo_coef = placebo$future_spend_placebo_coef[1],
      future_spend_placebo_partial_r2 = placebo$future_spend_placebo_partial_r2[1],
      future_spend_placebo_ratio = placebo$future_spend_placebo_ratio[1],
      spend_level_class = spend_guard$spend_level_class[1],
      spend_guard_action = spend_guard$spend_guard_action[1],
      spend_guard_risk_multiplier = spend_guard$spend_guard_risk_multiplier[1],
      implied_contribution_total = sanity$implied_contribution_total[1],
      implied_contribution_share = sanity$implied_contribution_share[1],
      implied_elasticity = sanity$implied_elasticity[1],
      implied_spend_total = sanity$implied_spend_total[1],
      implied_cost_per_outcome = sanity$implied_cost_per_outcome[1],
      implied_outcome_per_cost = sanity$implied_outcome_per_cost[1],
      sanity_bound_class = sanity$sanity_bound_class[1],
      sanity_bound_flags = sanity$sanity_bound_flags[1],
      sanity_bound_risk_multiplier = sanity$sanity_bound_risk_multiplier[1],
      observed_saturation_median_active = spend_guard$observed_saturation_median_active[1],
      observed_saturation_p90 = spend_guard$observed_saturation_p90[1],
      observed_saturation_max = spend_guard$observed_saturation_max[1],
      high_saturation_week_share = spend_guard$high_saturation_week_share[1],
      recent_saturation_mean = spend_guard$recent_saturation_mean[1],
      active_spend_p90_to_median = spend_guard$active_spend_p90_to_median[1],
      recent_spend_to_active_median = spend_guard$recent_spend_to_active_median[1],
      under_spend_flag = spend_guard$under_spend_flag[1],
      over_spend_flag = spend_guard$over_spend_flag[1],
      evidence_tier = tier
    )

    transformed[[v]] <- data.table::data.table(
      date = model_dt$date__,
      model_week_end = pb_date_to_week_end(model_dt$date__, week_end_day),
      variable = v,
      raw_x = raw_x,
      ramp_period_flag = ramp_flag,
      x_handoff = x_handoff
    )
  }

  priors_dt <- data.table::rbindlist(priors, use.names = TRUE, fill = TRUE)
  evidence_dt <- data.table::rbindlist(evidence, use.names = TRUE, fill = TRUE)
  support_dt <- data.table::rbindlist(support_diags, use.names = TRUE, fill = TRUE)
  transformed_dt <- data.table::rbindlist(transformed, use.names = TRUE, fill = TRUE)
  multivariate_coef_scan_dt <- if (isTRUE(multivariate_coef_scan)) {
    pb_multivariate_ridge_scan(
      y_mi = y_mi,
      transformed_dt = transformed_dt,
      controls = controls,
      priors_dt = priors_dt,
      lambda_grid = multivariate_coef_lambda_grid,
      min_obs = multivariate_coef_min_obs
    )
  } else {
    data.table::data.table()
  }
  if (nrow(multivariate_coef_scan_dt)) {
    priors_dt[multivariate_coef_scan_dt, on = "variable", `:=`(
      multivariate_coef_scan_class = i.multivariate_coef_scan_class,
      multivariate_ridge_coef = i.multivariate_ridge_coef,
      multivariate_ridge_lambda = i.multivariate_ridge_lambda,
      multivariate_ridge_gcv = i.multivariate_ridge_gcv,
      multivariate_ridge_condition_number = i.multivariate_ridge_condition_number,
      multivariate_ridge_max_abs_corr = i.multivariate_ridge_max_abs_corr,
      multivariate_ridge_direction_ok = i.multivariate_ridge_direction_ok,
      multivariate_ridge_to_univariate_ratio = i.multivariate_ridge_to_univariate_ratio
    )]
    evidence_dt[multivariate_coef_scan_dt, on = "variable", `:=`(
      multivariate_coef_scan_class = i.multivariate_coef_scan_class,
      multivariate_ridge_coef = i.multivariate_ridge_coef,
      multivariate_ridge_lambda = i.multivariate_ridge_lambda,
      multivariate_ridge_gcv = i.multivariate_ridge_gcv,
      multivariate_ridge_condition_number = i.multivariate_ridge_condition_number,
      multivariate_ridge_max_abs_corr = i.multivariate_ridge_max_abs_corr,
      multivariate_ridge_direction_ok = i.multivariate_ridge_direction_ok,
      multivariate_ridge_to_univariate_ratio = i.multivariate_ridge_to_univariate_ratio
    )]
    priors_dt[, coef_prior_source := "semi_univariate_profile_scan"]
    priors_dt[, use_multivariate_coef_prior__ := is.finite(multivariate_ridge_coef) &
      multivariate_ridge_direction_ok == TRUE &
      multivariate_coef_scan_class %in% c("joint_scan_ok", "joint_scan_highly_ill_conditioned")]
    if (identical(multivariate_coef_prior_mode, "auto")) {
      priors_dt[, use_multivariate_coef_prior__ := use_multivariate_coef_prior__ &
        is.finite(multivariate_ridge_max_abs_corr) & multivariate_ridge_max_abs_corr >= collinearity_threshold]
    } else if (identical(multivariate_coef_prior_mode, "always")) {
      priors_dt[, use_multivariate_coef_prior__ := use_multivariate_coef_prior__]
    } else {
      priors_dt[, use_multivariate_coef_prior__ := FALSE]
    }
    priors_dt[use_multivariate_coef_prior__ == TRUE, `:=`(
      coef_prior_pre_multivariate = coef_prior_final,
      coef_prior_final = multivariate_ridge_coef,
      coef_prior = multivariate_ridge_coef,
      coef_prior_source = "multivariate_ridge_scan",
      coef_precision_final = pmin(
        coef_precision_final,
        fifelse(abs(multivariate_ridge_coef) > 1e-12, 1 / (multivariate_ridge_coef ^ 2) / 6, coef_precision_final)
      ),
      coef_prior_handoff_reason = paste0(
        coef_prior_handoff_reason,
        "; multivariate_ridge_scan_center=", signif(multivariate_ridge_coef, 5),
        "; lambda=", signif(multivariate_ridge_lambda, 4)
      )
    )]
    priors_dt[, use_multivariate_coef_prior__ := NULL]
    evidence_dt[priors_dt[, .(variable, coef_prior_source, coef_prior_pre_multivariate, coef_prior_final, coef_precision_final)],
                on = "variable", `:=`(
                  coef_prior_source = i.coef_prior_source,
                  coef_prior_pre_multivariate = i.coef_prior_pre_multivariate,
                  coef_prior_final = i.coef_prior_final,
                  coef_precision_final = i.coef_precision_final
                )]
  }

  identification_diagnostics <- if (isTRUE(diagnose_collinearity)) {
    pb_identification_diagnostics_from_tables(
      priors_dt = priors_dt,
      evidence_dt = evidence_dt,
      support_dt = support_dt,
      transformed_dt = transformed_dt,
      corr_threshold = collinearity_threshold,
      severe_corr_threshold = severe_collinearity_threshold,
      vif_threshold = vif_threshold,
      severe_vif_threshold = severe_vif_threshold
    )
  } else {
    list(
      variable_diagnostics = data.table::data.table(),
      collinearity_pairs = data.table::data.table(),
      transformed_wide = data.table::data.table()
    )
  }

  if (isTRUE(adjust_priors_for_collinearity) && nrow(identification_diagnostics$variable_diagnostics)) {
    adjusted <- pb_apply_identification_adjustments(
      priors_dt = priors_dt,
      evidence_dt = evidence_dt,
      identification = identification_diagnostics
    )
    priors_dt <- adjusted$priors
    evidence_dt <- adjusted$evidence
  }
  default_prior_cols <- list(
    curve_type = "weibull",
    coef_hierarchy_scale = 1,
    coef_prior_unshrunk = NA_real_,
    coef_prior_half_sensitivity = NA_real_,
    coef_center_shrinkage = 1,
    coef_prior_source = "semi_univariate_profile_scan",
    coef_prior_pre_multivariate = NA_real_,
    multivariate_coef_scan_class = "not_run",
    multivariate_ridge_coef = NA_real_,
    multivariate_ridge_lambda = NA_real_,
    multivariate_ridge_gcv = NA_real_,
    multivariate_ridge_condition_number = NA_real_,
    multivariate_ridge_max_abs_corr = NA_real_,
    multivariate_ridge_direction_ok = NA,
    multivariate_ridge_to_univariate_ratio = NA_real_,
    cvalue_anchor = NA_real_,
    cvalue_data_driven = NA_real_,
    cvalue_final_source = NA_character_,
    cvalue_data_reason = NA_character_,
    cvalue_data_score_anchor = NA_real_,
    cvalue_data_score_best = NA_real_,
    cvalue_data_improvement = NA_real_,
    cvalue_data_multiplier = NA_real_,
    cvalue_flat_candidate = NA_real_,
    cvalue_flat_candidate_used = FALSE,
    preferred_cvalue = NA_real_,
    preferred_cvalue_used = FALSE,
    preferred_cvalue_source = NA_character_,
    preferred_cvalue_reliability = 0,
    pooled_ramp_group_col = NA_character_,
    pooled_ramp_groups_tested = 0L,
    pooled_ramp_usable_groups = 0L,
    pooled_ramp_supports_diminishing_groups = 0L,
    pooled_ramp_flat_groups = 0L,
    pooled_ramp_flat_negative_groups = 0L,
    pooled_ramp_conflict_groups = 0L,
    pooled_ramp_placebo_warning_groups = 0L,
    pooled_ramp_weight_total = 0,
    pooled_ramp_reliability = 0,
    pooled_ramp_cvalue = NA_real_,
    pooled_ramp_cvalue_blend_weight = 0,
    pooled_ramp_precision_multiplier = 1,
    pooled_ramp_evidence_class = NA_character_,
    pooled_ramp_reason = NA_character_,
    stan_observed_cvalue = NA_real_,
    stan_observed_cvalue_source = NA_character_,
    stan_observed_cvalue_reliability = 0,
    cvalue_anchor_x = NA_real_,
    cvalue_anchor_method = NA_character_,
    cvalue_anchor_data_weight = NA_real_,
    cvalue_anchor_active_cv = NA_real_,
    rrate_raw_best = NA_real_,
    rrate_score_best = NA_real_,
    rrate_score_selected = NA_real_,
    rrate_score_upper = NA_real_,
    rrate_at_upper_bound = FALSE,
    rrate_plateau_adjusted = FALSE,
    rrate_selection_reason = NA_character_,
    ramp_period_n = 0L,
    ramp_period_share = 0,
    ramp_variation_score = NA_real_,
    observed_curve_evidence_class = NA_character_,
    observed_marginal_slope_low = NA_real_,
    observed_marginal_slope_high = NA_real_,
    observed_marginal_slope_ratio = NA_real_,
    observed_slope_cvalue = NA_real_,
    observed_diminishing_returns_score = NA_real_,
    observed_low_spend_n = 0L,
    observed_high_spend_n = 0L,
    observed_low_spend_x = NA_real_,
    observed_high_spend_x = NA_real_,
    observed_spend_spread_p90_p10 = NA_real_,
    future_spend_placebo_class = NA_character_,
    future_spend_placebo_lead = NA_integer_,
    future_spend_placebo_coef = NA_real_,
    future_spend_placebo_partial_r2 = NA_real_,
    future_spend_placebo_ratio = NA_real_,
    spend_level_class = NA_character_,
    spend_guard_action = NA_character_,
    spend_guard_risk_multiplier = 1,
    missing_data_policy = missing_data_policy,
    modeled_x_missing_share_before = NA_real_,
    modeled_x_missing_share_after = NA_real_,
    spend_missing_share_before = NA_real_,
    spend_missing_share_after = NA_real_,
    support_missing_share_before = NA_real_,
    support_missing_share_after = NA_real_,
    missing_data_class = "not_checked",
    missing_data_action = missing_data_policy,
    max_missing_share_before = NA_real_,
    max_missing_share_after = NA_real_,
    missing_data_risk_multiplier = 1,
    implied_contribution_total = NA_real_,
    implied_contribution_share = NA_real_,
    implied_elasticity = NA_real_,
    implied_spend_total = NA_real_,
    implied_cost_per_outcome = NA_real_,
    implied_outcome_per_cost = NA_real_,
    sanity_bound_class = "not_run",
    sanity_bound_flags = "",
    sanity_bound_risk_multiplier = 1,
    observed_saturation_median_active = NA_real_,
    observed_saturation_p90 = NA_real_,
    observed_saturation_max = NA_real_,
    high_saturation_week_share = NA_real_,
    recent_saturation_mean = NA_real_,
    active_spend_p90_to_median = NA_real_,
    recent_spend_to_active_median = NA_real_,
    under_spend_flag = FALSE,
    over_spend_flag = FALSE,
    anchor_should_drive_curve_prior = NA,
    anchor_authority_tier = NA_character_,
    anchor_actionability_tier = NA_character_,
    anchor_weight_final = NA_real_,
    anchor_reliability = NA_real_,
    anchor_uncertainty_width_90 = NA_real_,
    anchor_lower_90 = NA_real_,
    anchor_upper_90 = NA_real_,
    max_abs_correlation = NA_real_,
    n_corr_pairs_over_threshold = 0L,
    n_corr_pairs_severe = 0L,
    max_vif = NA_real_,
    collinearity_class = NA_character_,
    collinearity_risk_multiplier = 1,
    requires_external_prior = FALSE,
    recommended_identification_action = NA_character_
  )
  for (nm in names(default_prior_cols)) {
    if (!(nm %in% names(priors_dt))) priors_dt[, (nm) := default_prior_cols[[nm]]]
  }

  out <- list(
    priors = priors_dt,
    evidence_stack = evidence_dt,
    support_diagnostics = support_dt,
    transformed_x_handoff = transformed_dt,
    multivariate_coef_scan = multivariate_coef_scan_dt,
    missing_data_diagnostics = missing_data_diagnostics,
    holdout_audit = holdout_info$audit,
    pooled_group_ramp_summary = if (length(pooled_group_ramp_summaries)) data.table::rbindlist(pooled_group_ramp_summaries, use.names = TRUE, fill = TRUE) else data.table::data.table(),
    pooled_group_ramp_details = if (length(pooled_group_ramp_details)) data.table::rbindlist(pooled_group_ramp_details, use.names = TRUE, fill = TRUE) else data.table::data.table(),
    identification_diagnostics = identification_diagnostics$variable_diagnostics,
    collinearity_pairs = identification_diagnostics$collinearity_pairs,
    transformed_x_wide = identification_diagnostics$transformed_wide,
    control_metadata = controls_metadata,
    metadata_handoff = priors_dt[, .(
      variable,
      role,
      source_entity = "GLOBAL",
      has_curve,
      curve_type,
      coef = coef_prior_final,
      coef_precision = coef_precision_final,
      coef_bound,
      coef_prior_unshrunk,
      coef_prior_half_sensitivity,
      coef_center_shrinkage,
      coef_prior_source,
      coef_prior_pre_multivariate,
      multivariate_coef_scan_class,
      multivariate_ridge_coef,
      multivariate_ridge_lambda,
      multivariate_ridge_gcv,
      multivariate_ridge_condition_number,
      multivariate_ridge_max_abs_corr,
      multivariate_ridge_direction_ok,
      multivariate_ridge_to_univariate_ratio,
      rrate = ifelse(has_curve & is.finite(rrate), rrate, 0),
      rrate_raw_best,
      rrate_score_best,
      rrate_score_selected,
      rrate_score_upper,
      rrate_at_upper_bound,
      rrate_plateau_adjusted,
      rrate_selection_reason,
      cvalue = ifelse(has_curve & is.finite(cvalue), cvalue, 0),
      cvalue_anchor,
      cvalue_data_driven,
      cvalue_final_source,
      cvalue_data_reason,
      cvalue_data_score_anchor,
      cvalue_data_score_best,
      cvalue_data_improvement,
      cvalue_data_multiplier,
      cvalue_flat_candidate,
      cvalue_flat_candidate_used,
      preferred_cvalue,
      preferred_cvalue_used,
      preferred_cvalue_source,
      preferred_cvalue_reliability,
      pooled_ramp_group_col,
      pooled_ramp_groups_tested,
      pooled_ramp_usable_groups,
      pooled_ramp_supports_diminishing_groups,
      pooled_ramp_flat_groups,
      pooled_ramp_flat_negative_groups,
      pooled_ramp_conflict_groups,
      pooled_ramp_placebo_warning_groups,
      pooled_ramp_weight_total,
      pooled_ramp_reliability,
      pooled_ramp_cvalue,
      pooled_ramp_cvalue_blend_weight,
      pooled_ramp_precision_multiplier,
      pooled_ramp_evidence_class,
      pooled_ramp_reason,
      stan_observed_cvalue,
      stan_observed_cvalue_source,
      stan_observed_cvalue_reliability,
      cvalue_anchor_x,
      cvalue_anchor_method,
      cvalue_anchor_data_weight,
      cvalue_anchor_active_cv,
      dvalue = ifelse(has_curve & is.finite(dvalue), dvalue, 0),
      rrate_precision = ifelse(has_curve & is.finite(rrate_precision_final), rrate_precision_final, 1),
      cvalue_precision = ifelse(has_curve & is.finite(cvalue_precision_final), cvalue_precision_final, 1),
      dvalue_precision = ifelse(has_curve & is.finite(dvalue_precision_final), dvalue_precision_final, 1),
      rrate_precision_final,
      cvalue_precision_final,
      dvalue_precision_final,
      anchor_saturation,
      anchor_source,
      modeled_x_col,
      modeled_x_basis,
      spend_col,
      support_col,
      support_type,
      missing_data_policy,
      modeled_x_missing_share_before,
      modeled_x_missing_share_after,
      spend_missing_share_before,
      spend_missing_share_after,
      support_missing_share_before,
      support_missing_share_after,
      missing_data_class,
      missing_data_action,
      max_missing_share_before,
      max_missing_share_after,
      missing_data_risk_multiplier,
      coef_hierarchy_scale,
      curve_scope,
      ramp_period_n,
      ramp_period_share,
      ramp_variation_score,
      observed_curve_evidence_class,
      observed_marginal_slope_low,
      observed_marginal_slope_high,
      observed_marginal_slope_ratio,
      observed_slope_cvalue,
      observed_diminishing_returns_score,
      observed_low_spend_n,
      observed_high_spend_n,
      observed_low_spend_x,
      observed_high_spend_x,
      observed_spend_spread_p90_p10,
      future_spend_placebo_class,
      future_spend_placebo_lead,
      future_spend_placebo_coef,
      future_spend_placebo_partial_r2,
      future_spend_placebo_ratio,
      spend_level_class,
      spend_guard_action,
      spend_guard_risk_multiplier,
      implied_contribution_total,
      implied_contribution_share,
      implied_elasticity,
      implied_spend_total,
      implied_cost_per_outcome,
      implied_outcome_per_cost,
      sanity_bound_class,
      sanity_bound_flags,
      sanity_bound_risk_multiplier,
      observed_saturation_median_active,
      observed_saturation_p90,
      observed_saturation_max,
      high_saturation_week_share,
      recent_saturation_mean,
      active_spend_p90_to_median,
      recent_spend_to_active_median,
      under_spend_flag,
      over_spend_flag,
      anchor_should_drive_curve_prior,
      anchor_authority_tier,
      anchor_actionability_tier,
      anchor_source,
      anchor_weight_final,
      anchor_reliability,
      anchor_uncertainty_width_90,
      anchor_lower_90,
      anchor_upper_90,
      max_abs_correlation,
      n_corr_pairs_over_threshold,
      n_corr_pairs_severe,
      max_vif,
      collinearity_class,
      collinearity_risk_multiplier,
      requires_external_prior,
      recommended_identification_action,
      coef_prior_handoff_tier,
      coef_prior_handoff_reason,
      use_as_production_prior,
      use_as_directional_starting_value,
      recommended_prior_strategy,
      identification_class
    )]
  )

  if (isTRUE(return_wrapper)) out else data.table::copy(out$priors)
}

prior_builder <- build_mmm_priors_engine
prior_recovery_builder <- build_mmm_priors_engine
build_mmm_priors <- build_mmm_priors_engine

make_hier_metadata_from_prior_output <- function(prior_output) {
  if (is.list(prior_output) && !is.null(prior_output$metadata_handoff)) return(data.table::copy(prior_output$metadata_handoff))
  if (data.table::is.data.table(prior_output) || is.data.frame(prior_output)) {
    dt <- data.table::as.data.table(data.table::copy(prior_output))

    if (!"role" %in% names(dt)) dt[, role := "media"]
    if (!"coef_prior_final" %in% names(dt) && "coef_prior" %in% names(dt)) dt[, coef_prior_final := coef_prior]
    if (!"coef_precision_final" %in% names(dt) && "coef_precision" %in% names(dt)) dt[, coef_precision_final := coef_precision]
    for (nm in c("rrate_precision_final", "cvalue_precision_final", "dvalue_precision_final")) {
      if (!nm %in% names(dt)) dt[, (nm) := NA_real_]
    }
    required <- c("variable", "has_curve", "coef_prior_final", "coef_precision_final", "coef_bound", "rrate", "cvalue", "dvalue")
    missing_required <- setdiff(required, names(dt))
    if (length(missing_required)) stop("prior_output is missing required columns: ", paste(missing_required, collapse = ", "))

    dt[, has_curve := as.logical(has_curve)]
    dt[!has_curve | !is.finite(rrate), rrate := 0]
    dt[!has_curve | !is.finite(cvalue), cvalue := 0]
    dt[!has_curve | !is.finite(dvalue), dvalue := 0]
    dt[!is.finite(rrate_precision_final), rrate_precision_final := 1]
    dt[!is.finite(cvalue_precision_final), cvalue_precision_final := 1]
    dt[!is.finite(dvalue_precision_final), dvalue_precision_final := 1]

    md <- dt[, .(
      variable,
      role,
      source_entity = if ("source_entity" %in% names(dt)) as.character(source_entity) else "GLOBAL",
      has_curve,
      curve_type = if ("curve_type" %in% names(dt)) pb_normalize_curve_type(curve_type) else "weibull",
      coef = coef_prior_final,
      coef_precision = coef_precision_final,
      coef_bound,
      rrate,
      cvalue,
      dvalue,
      rrate_precision = ifelse(is.finite(rrate_precision_final), rrate_precision_final, 1),
      cvalue_precision = ifelse(is.finite(cvalue_precision_final), cvalue_precision_final, 1),
      dvalue_precision = ifelse(is.finite(dvalue_precision_final), dvalue_precision_final, 1),
      rrate_precision_final,
      cvalue_precision_final,
      dvalue_precision_final,
      coef_hierarchy_scale = if ("coef_hierarchy_scale" %in% names(dt)) as.numeric(coef_hierarchy_scale) else 1,
      anchor_should_drive_curve_prior = if ("anchor_should_drive_curve_prior" %in% names(dt)) as.logical(anchor_should_drive_curve_prior) else NA,
      collinearity_class = if ("collinearity_class" %in% names(dt)) as.character(collinearity_class) else NA_character_,
      collinearity_risk_multiplier = if ("collinearity_risk_multiplier" %in% names(dt)) as.numeric(collinearity_risk_multiplier) else 1,
      requires_external_prior = if ("requires_external_prior" %in% names(dt)) as.logical(requires_external_prior) else FALSE,
      recommended_identification_action = if ("recommended_identification_action" %in% names(dt)) as.character(recommended_identification_action) else NA_character_
    )]
    optional_cols <- intersect(c(
      "cvalue_anchor", "cvalue_data_driven", "cvalue_final_source", "cvalue_data_reason",
      "cvalue_data_improvement", "cvalue_data_multiplier", "cvalue_flat_candidate",
      "cvalue_flat_candidate_used", "preferred_cvalue", "preferred_cvalue_used",
      "preferred_cvalue_source", "preferred_cvalue_reliability", "pooled_ramp_group_col",
      "pooled_ramp_groups_tested", "pooled_ramp_usable_groups",
      "pooled_ramp_supports_diminishing_groups", "pooled_ramp_flat_groups",
      "pooled_ramp_flat_negative_groups",
      "pooled_ramp_conflict_groups", "pooled_ramp_placebo_warning_groups",
      "pooled_ramp_weight_total", "pooled_ramp_reliability", "pooled_ramp_cvalue",
      "pooled_ramp_cvalue_blend_weight", "pooled_ramp_precision_multiplier",
      "pooled_ramp_evidence_class", "pooled_ramp_reason", "stan_observed_cvalue",
      "stan_observed_cvalue_source", "stan_observed_cvalue_reliability",
      "anchor_saturation", "anchor_source", "anchor_should_drive_curve_prior",
      "anchor_authority_tier", "anchor_actionability_tier", "anchor_reliability",
      "anchor_weight_final", "anchor_uncertainty_width_90", "anchor_lower_90", "anchor_upper_90",
      "observed_curve_evidence_class", "observed_marginal_slope_ratio",
      "observed_slope_cvalue", "future_spend_placebo_class",
      "future_spend_placebo_ratio", "missing_data_policy",
      "modeled_x_missing_share_before", "modeled_x_missing_share_after",
      "spend_missing_share_before", "spend_missing_share_after",
      "support_missing_share_before", "support_missing_share_after",
      "missing_data_class", "missing_data_action", "max_missing_share_before",
      "max_missing_share_after", "missing_data_risk_multiplier",
      "implied_contribution_total", "implied_contribution_share",
      "implied_elasticity", "implied_spend_total",
      "implied_cost_per_outcome", "implied_outcome_per_cost",
      "sanity_bound_class", "sanity_bound_flags", "sanity_bound_risk_multiplier",
      "coef_prior_source", "coef_prior_pre_multivariate", "multivariate_coef_scan_class",
      "multivariate_ridge_coef", "multivariate_ridge_lambda", "multivariate_ridge_gcv",
      "multivariate_ridge_condition_number", "multivariate_ridge_max_abs_corr",
      "multivariate_ridge_direction_ok", "multivariate_ridge_to_univariate_ratio"
    ), names(dt))
    if (length(optional_cols)) md <- cbind(md, dt[, ..optional_cols])
    return(md[])
  }
  stop("Unsupported prior_output.")
}

build_prior_identification_diagnostics <- function(prior_output,
                                                   corr_threshold = 0.85,
                                                   severe_corr_threshold = 0.95,
                                                   vif_threshold = 5,
                                                   severe_vif_threshold = 10) {
  if (!is.list(prior_output) || is.null(prior_output$priors) || is.null(prior_output$transformed_x_handoff)) {
    stop("prior_output must be the list returned by prior_builder().")
  }
  pb_identification_diagnostics_from_tables(
    priors_dt = prior_output$priors,
    evidence_dt = prior_output$evidence_stack %||% data.table::data.table(),
    support_dt = prior_output$support_diagnostics %||% data.table::data.table(),
    transformed_dt = prior_output$transformed_x_handoff,
    corr_threshold = corr_threshold,
    severe_corr_threshold = severe_corr_threshold,
    vif_threshold = vif_threshold,
    severe_vif_threshold = severe_vif_threshold
  )
}

apply_identification_adjustments_to_prior_output <- function(prior_output,
                                                            identification = NULL,
                                                            min_precision = 1e-6) {
  if (!is.list(prior_output) || is.null(prior_output$priors)) {
    stop("prior_output must be the list returned by prior_builder().")
  }
  out <- prior_output
  if (is.null(identification)) identification <- build_prior_identification_diagnostics(prior_output)
  adjusted <- pb_apply_identification_adjustments(
    priors_dt = out$priors,
    evidence_dt = out$evidence_stack %||% data.table::data.table(),
    identification = identification,
    min_precision = min_precision
  )
  out$priors <- adjusted$priors
  out$evidence_stack <- adjusted$evidence
  out$identification_diagnostics <- identification$variable_diagnostics
  out$collinearity_pairs <- identification$collinearity_pairs
  out$transformed_x_wide <- identification$transformed_wide
  out$metadata_handoff <- make_hier_metadata_from_prior_output(out$priors)
  out
}

diagnose_mmm_data_granularity <- function(input_data,
                                          date_col,
                                          dep_var_col,
                                          variable_map = NULL,
                                          media_cols = character(),
                                          curve_cols = NULL,
                                          geo_col = NULL,
                                          min_geo_variation_share = 0.20,
                                          strong_geo_variation_share = 0.75) {
  dt <- data.table::as.data.table(data.table::copy(input_data))
  if (!date_col %in% names(dt)) stop("date_col not found: ", date_col)
  if (!dep_var_col %in% names(dt)) stop("dep_var_col not found: ", dep_var_col)
  vm <- pb_parse_variable_map(variable_map, media_cols, curve_cols)

  needed <- unique(c(vm$modeled_x_col, vm$spend_col, vm$support_col))
  needed <- needed[!is.na(needed) & nzchar(needed)]
  missing <- setdiff(needed, names(dt))
  if (length(missing)) stop("Mapped marketing columns missing from input_data: ", paste(missing, collapse = ", "))

  dt[, date__ := pb_parse_date(get(date_col), date_col)]
  dt[, dep__ := pb_force_numeric_vec(get(dep_var_col), dep_var_col)]
  has_geo <- !is.null(geo_col) && nzchar(geo_col) && geo_col %in% names(dt)
  if (has_geo) dt[, geo__ := as.character(get(geo_col))] else dt[, geo__ := "TOTAL"]

  variation_one <- function(col) {
    if (is.na(col) || !nzchar(col) || !(col %in% names(dt))) {
      return(list(geo_variation_week_share = NA_real_, active_geo_week_share = NA_real_, n_active_geos = NA_integer_))
    }
    z <- data.table::data.table(date__ = dt$date__, geo__ = dt$geo__, value__ = pb_force_numeric_vec(dt[[col]], col))
    wk <- z[, .(
      n_geo = data.table::uniqueN(geo__),
      sd_geo = stats::sd(value__, na.rm = TRUE),
      any_active = any(value__ > 0, na.rm = TRUE)
    ), by = date__]
    list(
      geo_variation_week_share = if (has_geo && nrow(wk)) mean(wk$n_geo >= 2 & is.finite(wk$sd_geo) & wk$sd_geo > 1e-8, na.rm = TRUE) else 0,
      active_geo_week_share = if (nrow(wk)) mean(wk$any_active, na.rm = TRUE) else NA_real_,
      n_active_geos = if (has_geo) z[value__ > 0, data.table::uniqueN(geo__)] else 1L
    )
  }

  dep_var <- variation_one(dep_var_col)
  variable_rows <- data.table::rbindlist(lapply(seq_len(nrow(vm)), function(i) {
    mx <- variation_one(vm$modeled_x_col[i])
    spend <- variation_one(vm$spend_col[i])
    class <- if (!has_geo) {
      "national_or_aggregate_input"
    } else if (!is.finite(mx$geo_variation_week_share) || mx$geo_variation_week_share < min_geo_variation_share) {
      "national_repeated_or_no_geo_marketing"
    } else if (mx$geo_variation_week_share >= strong_geo_variation_share) {
      "geo_level_marketing"
    } else {
      "partially_geo_resolved_marketing"
    }
    data.table::data.table(
      variable = vm$variable[i],
      modeled_x_col = vm$modeled_x_col[i],
      spend_col = vm$spend_col[i],
      modeled_x_basis = vm$modeled_x_basis[i],
      media_granularity_class = class,
      modeled_x_geo_variation_week_share = mx$geo_variation_week_share,
      spend_geo_variation_week_share = spend$geo_variation_week_share,
      active_geo_week_share = mx$active_geo_week_share,
      n_active_geos = mx$n_active_geos,
      coef_hierarchy_scale_recommended = if (class == "geo_level_marketing") 1 else if (class == "partially_geo_resolved_marketing") 0.50 else 0.20
    )
  }), use.names = TRUE, fill = TRUE)

  n_geo <- if (has_geo) data.table::uniqueN(dt$geo__) else 1L
  geo_media_share <- mean(variable_rows$media_granularity_class == "geo_level_marketing", na.rm = TRUE)
  partial_media_share <- mean(variable_rows$media_granularity_class == "partially_geo_resolved_marketing", na.rm = TRUE)
  data_level <- if (!has_geo || n_geo < 2L) {
    "national_or_aggregate_mmm"
  } else if (geo_media_share >= 0.75) {
    "geo_kpi_geo_media_hierarchical_mmm"
  } else if ((geo_media_share + partial_media_share) > 0) {
    "mixed_geo_kpi_partial_geo_media_mmm"
  } else {
    "geo_kpi_national_media_mmm"
  }
  workflow <- switch(
    data_level,
    national_or_aggregate_mmm = "Run a national/aggregate model; rely on external ROI/contribution priors, holdouts, response-curve sanity checks, and sensitivity runs.",
    geo_kpi_geo_media_hierarchical_mmm = "Run the hierarchical geo model with shared variable-level curves and group-level coefficients; use geo holdouts where possible.",
    mixed_geo_kpi_partial_geo_media_mmm = "Run hierarchical geo MMM only for truly geo-varying media; shrink group coefficient variation for national-only media and require stronger external priors for multicollinear variables.",
    geo_kpi_national_media_mmm = "Use geo outcomes mainly for baseline/control learning; aggregate or strongly shrink national-only media effects because geo rows do not identify geo-specific media response.",
    "Review data granularity before modeling."
  )

  list(
    summary = data.table::data.table(
      data_level = data_level,
      n_rows = nrow(dt),
      n_weeks = data.table::uniqueN(dt$date__),
      geo_col = if (has_geo) geo_col else NA_character_,
      n_geos = n_geo,
      dep_geo_variation_week_share = dep_var$geo_variation_week_share,
      geo_level_media_share = geo_media_share,
      partial_geo_media_share = partial_media_share,
      workflow_recommendation = workflow
    ),
    variable_granularity = variable_rows[],
    workflow_recommendation = workflow
  )
}

apply_data_granularity_adjustments_to_prior_output <- function(prior_output,
                                                               granularity_audit,
                                                               min_coef_hierarchy_scale = 0.05) {
  if (!is.list(prior_output) || is.null(prior_output$metadata_handoff)) {
    stop("prior_output must be the list returned by prior_builder().")
  }
  if (is.null(granularity_audit$variable_granularity)) {
    stop("granularity_audit must be returned by diagnose_mmm_data_granularity().")
  }
  out <- prior_output
  vg <- data.table::as.data.table(data.table::copy(granularity_audit$variable_granularity))
  vg[, coef_hierarchy_scale_recommended := pmax(as.numeric(coef_hierarchy_scale_recommended), min_coef_hierarchy_scale)]

  update_one <- function(dt) {
    dt <- data.table::as.data.table(data.table::copy(dt))
    if (!"coef_hierarchy_scale" %in% names(dt)) dt[, coef_hierarchy_scale := 1]
    dt[vg, on = "variable", `:=`(
      media_granularity_class = i.media_granularity_class,
      modeled_x_geo_variation_week_share = i.modeled_x_geo_variation_week_share,
      spend_geo_variation_week_share = i.spend_geo_variation_week_share,
      coef_hierarchy_scale = pmin(coef_hierarchy_scale, i.coef_hierarchy_scale_recommended)
    )]
    dt[]
  }

  out$priors <- update_one(out$priors)
  out$metadata_handoff <- update_one(out$metadata_handoff)
  if (!is.null(out$evidence_stack)) out$evidence_stack <- update_one(out$evidence_stack)
  out$data_granularity_audit <- granularity_audit
  out
}

apply_benchmark_priors_to_metadata <- function(metadata_input,
                                               benchmark_priors,
                                               benchmark_precision_weight = 1,
                                               max_precision = Inf) {
  if (is.list(metadata_input) && !is.null(metadata_input$metadata_handoff)) {
    md <- data.table::as.data.table(data.table::copy(metadata_input$metadata_handoff))
  } else if (data.table::is.data.table(metadata_input) || is.data.frame(metadata_input)) {
    md <- data.table::as.data.table(data.table::copy(metadata_input))
  } else if (is.character(metadata_input) && length(metadata_input) == 1L) {
    path <- path.expand(metadata_input)
    if (!file.exists(path)) stop("metadata_input file does not exist: ", path)
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("csv", "txt", "tsv")) {
      md <- data.table::fread(path)
    } else if (ext == "rds") {
      md <- data.table::as.data.table(readRDS(path))
    } else if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' is required for Excel metadata input.")
      md <- data.table::as.data.table(readxl::read_excel(path))
    } else {
      stop("Unsupported metadata_input file type: .", ext)
    }
  } else {
    stop("metadata_input must be a prior_builder output, data.table, data.frame, or file path.")
  }
  bp <- data.table::as.data.table(data.table::copy(benchmark_priors))
  if (!"variable" %in% names(bp)) stop("benchmark_priors must contain variable.")
  if (!"coef_precision" %in% names(bp) && "coef_precision_uncapped" %in% names(bp)) bp[, coef_precision := coef_precision_uncapped]
  if (!all(c("variable", "coef", "coef_precision") %in% names(md))) {
    stop("metadata_input must be a metadata handoff table with at least variable, coef, and coef_precision.")
  }
  bp[, variable := as.character(variable)]
  benchmark_precision_weight <- as.numeric(benchmark_precision_weight)[1]
  if (!is.finite(benchmark_precision_weight) || benchmark_precision_weight <= 0) benchmark_precision_weight <- 1
  max_precision <- suppressWarnings(as.numeric(max_precision)[1])
  if (!is.finite(max_precision) || max_precision <= 0) max_precision <- Inf
  audit_cols <- intersect(c(
    "prior_metric", "input_prior_metric", "prior_distribution", "stan_prior_distribution",
    "input_prior_mean", "input_prior_sd", "input_prior_precision", "input_precision_preserved",
    "coef_sd", "coef_precision_uncapped", "coef_precision_was_capped",
    "outcome_per_cost", "cost_per_outcome", "roi", "mroi", "kpi_value_per_outcome",
    "economic_prior_basis", "prior_source", "evidence_source", "evidence_notes", "warning"
  ), names(bp))
  if (length(audit_cols)) {
    audit <- unique(bp[, c("variable", audit_cols), with = FALSE], by = "variable")
    new_cols <- paste0("benchmark_", audit_cols)
    data.table::setnames(audit, audit_cols, new_cols)
    for (bc in new_cols) {
      md[audit, on = "variable", (bc) := get(paste0("i.", bc))]
    }
  }

  blend_pair <- function(center_col, precision_col) {
    if (!(center_col %in% names(bp))) return(invisible(NULL))
    bprec_col <- paste0(center_col, "_precision")
    bsd_col <- paste0(center_col, "_sd")
    if (bprec_col %in% names(bp)) {
      bp[, bench_precision__ := suppressWarnings(as.numeric(get(bprec_col)))]
    } else if (bsd_col %in% names(bp)) {
      bp[, bench_precision__ := 1 / pmax(suppressWarnings(as.numeric(get(bsd_col))) ^ 2, 1e-8)]
    } else {
      stop("benchmark_priors has ", center_col, " but needs ", bprec_col, " or ", bsd_col, ".")
    }
    bp[, bench_center__ := suppressWarnings(as.numeric(get(center_col)))]
    tmp <- bp[is.finite(bench_center__) & is.finite(bench_precision__) & bench_precision__ > 0,
              .(variable, bench_center__, bench_precision__ = pmin(bench_precision__ * benchmark_precision_weight, max_precision))]
    md[tmp, on = "variable", `:=`(
      blend_prior_source = "benchmark_precision_weighted",
      blend_prior_fields = data.table::fifelse(nzchar(blend_prior_fields), paste0(blend_prior_fields, "|", center_col), center_col),
      tmp_center__ = i.bench_center__,
      tmp_precision__ = i.bench_precision__
    )]
    if (!"tmp_center__" %in% names(md)) return(invisible(NULL))
    hit <- is.finite(md$tmp_center__) & is.finite(md$tmp_precision__) & md$tmp_precision__ > 0
    md[hit, (center_col) := ((get(center_col) * get(precision_col)) + (tmp_center__ * tmp_precision__)) / pmax(get(precision_col) + tmp_precision__, 1e-8)]
    md[hit, (precision_col) := pmin(get(precision_col) + tmp_precision__, max_precision)]
    md[, c("tmp_center__", "tmp_precision__") := NULL]
    invisible(NULL)
  }

  if (!"blend_prior_fields" %in% names(md)) md[, blend_prior_fields := ""]
  md[is.na(blend_prior_fields), blend_prior_fields := ""]
  if (!"blend_prior_source" %in% names(md)) md[, blend_prior_source := NA_character_]
  blend_pair("coef", "coef_precision")
  blend_pair("rrate", "rrate_precision")
  blend_pair("cvalue", "cvalue_precision")
  blend_pair("dvalue", "dvalue_precision")
  md[]
}
