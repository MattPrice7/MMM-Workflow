# synthetic_mmm_data_generators.R
#
# Reusable known-truth synthetic data generators for MMM validation, quasi-geo
# testing, optimizer checks, and future Neural MMM experiments. These are support
# utilities, not part of the production modeling path.

synth_require_data_table <- function() {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("synthetic_mmm_data_generators.R requires the data.table package.", call. = FALSE)
  }
  invisible(TRUE)
}

synth_num <- function(x) suppressWarnings(as.numeric(x))

synth_geometric_adstock <- function(x, rate = 0.30) {
  x <- synth_num(x)
  rate <- synth_num(rate)[1]
  if (!is.finite(rate)) rate <- 0
  rate <- min(max(rate, 0), 0.99)
  out <- numeric(length(x))
  carry <- 0
  for (i in seq_along(x)) {
    carry <- x[i] + rate * carry
    out[i] <- carry
  }
  out
}

synth_saturate_media <- function(x, curve_rate = 0.02, shape = 1,
                                 curve_type = c("weibull", "hill")) {
  x <- pmax(synth_num(x), 0)
  curve_rate <- synth_num(curve_rate)[1]
  shape <- synth_num(shape)[1]
  curve_type <- match.arg(tolower(as.character(curve_type)[1]), c("weibull", "hill"))
  if (!is.finite(curve_rate) || curve_rate <= 0) curve_rate <- 0.02
  if (!is.finite(shape) || shape <= 0) shape <- 1
  z <- pmax(curve_rate * x, 0)
  if (identical(curve_type, "hill")) {
    zz <- z ^ shape
    zz / (1 + zz)
  } else {
    1 - exp(-(z ^ shape))
  }
}

synth_curve_rate_from_anchor <- function(anchor_x,
                                         anchor_saturation = 0.50,
                                         shape = 1,
                                         curve_type = c("weibull", "hill")) {
  anchor_x <- synth_num(anchor_x)[1]
  anchor_saturation <- min(max(synth_num(anchor_saturation)[1], 0.01), 0.99)
  shape <- synth_num(shape)[1]
  curve_type <- match.arg(tolower(as.character(curve_type)[1]), c("weibull", "hill"))
  if (!is.finite(anchor_x) || anchor_x <= 0) return(NA_real_)
  if (!is.finite(shape) || shape <= 0) shape <- 1
  if (identical(curve_type, "hill")) {
    (anchor_saturation / (1 - anchor_saturation)) ^ (1 / shape) / anchor_x
  } else {
    (-log(1 - anchor_saturation)) ^ (1 / shape) / anchor_x
  }
}

synth_apply_media_effect <- function(panel, variable, truth_row) {
  synth_require_data_table()
  dt <- data.table::copy(panel)
  variable <- as.character(variable)[1]
  if (!variable %in% names(dt)) stop("variable not found in panel: ", variable, call. = FALSE)
  adstock_rate <- synth_num(truth_row$adstock_rate)[1]
  curve_rate <- synth_num(truth_row$curve_rate)[1]
  shape <- synth_num(truth_row$shape)[1]
  coef <- synth_num(truth_row$coef)[1]
  curve_type <- as.character(truth_row$curve_type)[1]
  dt[, support_adstock__ := synth_geometric_adstock(get(variable), adstock_rate), by = geo]
  dt[, support_sat__ := synth_saturate_media(support_adstock__, curve_rate, shape, curve_type)]
  dt[, effect__ := coef * support_sat__]
  out <- dt$effect__
  attr(out, "adstock") <- dt$support_adstock__
  attr(out, "saturation") <- dt$support_sat__
  out
}

make_synthetic_mmm_panel <- function(seed = 20260602,
                                     n_weeks = 104L,
                                     geos = paste0("G", 1:8),
                                     start_date = as.Date("2023-01-01"),
                                     curve_type = c("weibull", "hill"),
                                     include_population = TRUE,
                                     include_national_media = TRUE,
                                     media_correlation = 0.65,
                                     noise_sd = 20,
                                     holdout_last_n = 8L) {
  synth_require_data_table()
  curve_type <- match.arg(tolower(as.character(curve_type)[1]), c("weibull", "hill"))
  set.seed(seed)
  n_weeks <- as.integer(n_weeks)[1]
  if (!is.finite(n_weeks) || n_weeks < 24L) stop("n_weeks must be at least 24.", call. = FALSE)
  weeks <- seq.Date(as.Date(start_date), by = "week", length.out = n_weeks)
  dt <- data.table::CJ(week = weeks, geo = as.character(geos))
  dt[, `:=`(
    entity = "brand",
    week_index = as.integer(match(week, weeks)),
    geo_index = as.integer(factor(geo))
  )]
  geo_tbl <- data.table::data.table(
    geo = as.character(geos),
    population = round(stats::runif(length(geos), 250000, 3500000)),
    geo_quality = stats::runif(length(geos), 0.85, 1.15)
  )
  if (!isTRUE(include_population)) geo_tbl[, population := NA_real_]
  dt[geo_tbl, `:=`(population = i.population, geo_quality = i.geo_quality), on = "geo"]

  common_wave <- sin(dt$week_index / 6) + 0.5 * cos(dt$week_index / 11)
  geo_wave <- sin(dt$week_index / 5 + dt$geo_index / 3)
  corr <- min(max(synth_num(media_correlation)[1], 0), 0.95)
  base_media <- 100 + 18 * common_wave + 8 * geo_wave
  id_noise <- function(sd) stats::rnorm(nrow(dt), 0, sd)

  dt[, tv := pmax(0, base_media * (1 + 0.12 * geo_quality) + id_noise(12))]
  dt[, search := pmax(0, corr * tv + (1 - corr) * (80 + 12 * cos(week_index / 7) + id_noise(18)))]
  dt[, social := pmax(0, 55 + 9 * sin(week_index / 4 + geo_index) + 0.30 * tv + id_noise(10))]
  if (isTRUE(include_national_media)) {
    national <- data.table::data.table(
      week = weeks,
      national_video = pmax(0, 90 + 18 * sin(seq_along(weeks) / 8) + stats::rnorm(length(weeks), 0, 6))
    )
    dt[national, national_video := i.national_video, on = "week"]
  }
  dt[, promo := as.integer(week_index %% 13L %in% c(0L, 1L))]
  dt[, price_index := 1 + 0.04 * cos(week_index / 10) + 0.01 * geo_index]
  dt[, holiday := as.integer(format(week, "%m-%d") %in% c("11-24", "12-22", "12-29"))]

  variables <- c("tv", "search", "social", if (isTRUE(include_national_media)) "national_video")
  truth <- data.table::data.table(
    variable = variables,
    role = "media",
    curve_type = curve_type,
    adstock_rate = c(0.45, 0.15, 0.25, if (isTRUE(include_national_media)) 0.55),
    shape = c(1.15, 0.90, 1.05, if (isTRUE(include_national_media)) 1.00),
    anchor_saturation = c(0.55, 0.35, 0.45, if (isTRUE(include_national_media)) 0.50),
    coef = c(220, 120, 75, if (isTRUE(include_national_media)) 160)
  )
  truth[, anchor_x := vapply(variable, function(v) {
    stats::median(dt[[v]][dt[[v]] > 0], na.rm = TRUE)
  }, numeric(1))]
  truth[, curve_rate := mapply(
    synth_curve_rate_from_anchor,
    anchor_x = anchor_x,
    anchor_saturation = anchor_saturation,
    shape = shape,
    curve_type = curve_type
  )]

  dt[, baseline := 900 + 2.2 * week_index + 35 * sin(2 * pi * week_index / 52) + 12 * geo_index]
  dt[, control_contribution := -180 * (price_index - 1) + 45 * promo + 35 * holiday]
  media_effect_cols <- character()
  for (v in truth$variable) {
    eff <- synth_apply_media_effect(dt, v, truth[variable == v])
    cc <- paste0(v, "_contribution_true")
    dt[, (cc) := eff]
    media_effect_cols <- c(media_effect_cols, cc)
  }
  dt[, media_contribution_true := rowSums(.SD), .SDcols = media_effect_cols]
  dt[, y_true := baseline + control_contribution + media_contribution_true]
  dt[, y := y_true + stats::rnorm(.N, 0, synth_num(noise_sd)[1])]
  for (v in truth$variable) dt[, paste0(v, "_spend") := get(v) * stats::runif(1, 0.85, 1.15)]
  dt[, is_holdout := week > sort(unique(week))[max(1L, length(unique(week)) - as.integer(holdout_last_n)[1])]]

  metadata <- truth[, .(
    variable,
    source_entity = "GLOBAL",
    role,
    curve_type,
    anchor_saturation,
    rrate = adstock_rate,
    rrate_precision = 16,
    cvalue = curve_rate,
    cvalue_precision = 16,
    dvalue = shape,
    dvalue_precision = 16,
    coef = coef,
    coef_precision = 9,
    coef_bound = "pos",
    coef_hierarchy_scale = data.table::fifelse(variable == "national_video", 0, 1)
  )]
  control_metadata <- data.table::data.table(
    variable = c("promo", "price_index", "holiday"),
    source_entity = "GLOBAL",
    role = "control",
    curve_type = "weibull",
    rrate = 0,
    rrate_precision = 1,
    cvalue = 0,
    cvalue_precision = 1,
    dvalue = 0,
    dvalue_precision = 1,
    coef = c(0.02, -0.05, 0.02),
    coef_precision = c(4, 4, 4),
    coef_bound = c("pos", "neg", "pos"),
    coef_hierarchy_scale = 0
  )
  metadata <- data.table::rbindlist(list(metadata, control_metadata), fill = TRUE)
  spend_map <- truth[, .(variable, spend_col = paste0(variable, "_spend"))]
  variable_map <- truth[, .(variable, modeled_x_col = variable, spend_col = paste0(variable, "_spend"))]
  channel_map <- truth[, .(variable, channel = variable, parent_channel = variable, role = "media")]
  list(
    data = dt[],
    metadata = metadata[],
    truth = truth[],
    spend_map = spend_map[],
    variable_map = variable_map[],
    channel_map = channel_map[],
    notes = data.table::data.table(
      generator = "make_synthetic_mmm_panel",
      seed = seed,
      known_truth = TRUE,
      intended_use = "MMM, optimizer, decomposition, and future Neural MMM validation"
    )
  )
}

make_synthetic_quasi_geo_panel <- function(seed = 20260602,
                                           n_weeks = 72L,
                                           geos = paste0("G", 1:7),
                                           event_type = c("up_ramp", "down_ramp", "turn_on", "turn_off", "bundle", "contaminated", "national_repeated"),
                                           treated_geo = "G1",
                                           event_start_index = 36L,
                                           event_length = 4L,
                                           effect_per_unit = 1.5) {
  synth_require_data_table()
  event_type <- match.arg(tolower(as.character(event_type)[1]),
                          c("up_ramp", "down_ramp", "turn_on", "turn_off", "bundle", "contaminated", "national_repeated"))
  set.seed(seed)
  weeks <- seq.Date(as.Date("2024-01-07"), by = "week", length.out = as.integer(n_weeks)[1])
  dt <- data.table::CJ(week = weeks, geo = as.character(geos))
  dt[, `:=`(
    week_index = as.integer(match(week, weeks)),
    geo_index = as.integer(factor(geo))
  )]
  dt[, tv := pmax(0, 45 + 3 * sin(week_index / 4) + 2 * geo_index + stats::rnorm(.N, 0, 2))]
  dt[, search := pmax(0, 35 + 2 * cos(week_index / 5) + geo_index + stats::rnorm(.N, 0, 1.5))]
  dt[, social := pmax(0, 25 + 1.5 * sin(week_index / 3 + geo_index) + stats::rnorm(.N, 0, 1.2))]
  if (identical(event_type, "national_repeated")) {
    national_series <- 40 + 5 * sin(seq_along(weeks) / 7)
    national_series[event_start_index:(event_start_index + event_length - 1L)] <-
      national_series[event_start_index:(event_start_index + event_length - 1L)] + 35
    dt[, tv := national_series[week_index]]
  }
  dt[, y_base := 1000 + 5 * week_index + 18 * geo_index + 15 * sin(2 * pi * week_index / 52)]
  event_weeks <- weeks[event_start_index:(event_start_index + event_length - 1L)]
  before <- data.table::copy(dt)
  hit <- dt$geo == treated_geo & dt$week %in% event_weeks
  if (identical(event_type, "up_ramp")) dt[hit, tv := tv + 40]
  if (identical(event_type, "down_ramp")) dt[hit, tv := pmax(0, tv - 25)]
  if (identical(event_type, "turn_on")) {
    dt[geo == treated_geo & week < event_weeks[1], tv := 0]
    dt[hit, tv := 45]
  }
  if (identical(event_type, "turn_off")) {
    dt[geo == treated_geo & week < event_weeks[1], tv := 45]
    dt[hit, tv := 0]
  }
  if (identical(event_type, "bundle")) dt[hit, `:=`(tv = tv + 35, search = search + 25)]
  if (identical(event_type, "contaminated")) dt[hit, `:=`(tv = tv + 35, search = search + 25, social = social + 20)]
  dt[before, tv_pre_event__ := i.tv, on = .(week, geo)]
  dt[before, search_pre_event__ := i.search, on = .(week, geo)]
  dt[, incremental_tv_true := tv - tv_pre_event__]
  dt[, incremental_search_true := search - search_pre_event__]
  dt[, y := y_base + 0.8 * tv + 0.5 * search + stats::rnorm(.N, 0, 6)]
  dt[hit, y := y + effect_per_unit * incremental_tv_true]
  dt[, `:=`(
    tv_spend = tv,
    search_spend = search,
    social_spend = social
  )]
  variable_map <- data.table::data.table(
    variable = c("tv", "search", "social"),
    modeled_x_col = c("tv", "search", "social"),
    spend_col = c("tv_spend", "search_spend", "social_spend")
  )
  truth <- data.table::data.table(
    event_type = event_type,
    treated_geo = treated_geo,
    event_start = event_weeks[1],
    event_end = event_weeks[length(event_weeks)],
    effect_per_unit = effect_per_unit,
    intended_use = "quasi-geo evidence and future Neural MMM causal-recovery validation"
  )
  list(data = dt[], variable_map = variable_map[], truth = truth[])
}

make_synthetic_decomp_outputs <- function(seed = 20260602, n_periods = 24L) {
  synth_require_data_table()
  set.seed(seed)
  weeks <- seq.Date(as.Date("2024-01-07"), by = "week", length.out = as.integer(n_periods)[1])
  raw <- data.table::data.table(
    week = weeks,
    tv_spend = 100 + 10 * sin(seq_along(weeks) / 4),
    search_spend = 80 + 8 * cos(seq_along(weeks) / 5)
  )
  long <- data.table::data.table(
    week = rep(weeks, each = 4),
    variable = rep(c("baseline", "tv", "search", "residual"), length(weeks))
  )
  long[, contribution := fifelse(variable == "baseline", 900 + 2 * as.integer(factor(week)),
                          fifelse(variable == "tv", 25 + 5 * sin(as.integer(factor(week)) / 4),
                            fifelse(variable == "search", 18 + 3 * cos(as.integer(factor(week)) / 5),
                              stats::rnorm(.N, 0, 2))))]
  totals <- long[, .(pred = sum(contribution[variable != "residual"], na.rm = TRUE),
                     residual = sum(contribution[variable == "residual"], na.rm = TRUE)), by = week]
  totals[, y_actual := pred + residual]
  long[totals, `:=`(pred = i.pred, residual = i.residual, y_actual = i.y_actual), on = "week"]
  wide <- totals[, .(week, y_actual, pred, residual)]
  list(long_decomp = long[], wide_decomp = wide[], raw_data = raw[])
}
