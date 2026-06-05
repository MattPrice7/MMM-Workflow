#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

script_dir <- tryCatch({
  frames <- sys.frames()
  ofiles <- vapply(frames, function(f) if (!is.null(f$ofile)) as.character(f$ofile)[1] else NA_character_, character(1))
  ofiles <- ofiles[!is.na(ofiles) & nzchar(ofiles)]
  if (length(ofiles)) {
    dirname(normalizePath(ofiles[length(ofiles)], mustWork = FALSE))
  } else {
    file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)) else getwd()
  }
}, error = function(e) getwd())

workflow_dir <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
scripts_dir <- normalizePath(file.path(workflow_dir, "full_package_scripts"), mustWork = TRUE)
output_dir <- file.path(script_dir, "output")

source(file.path(scripts_dir, "optimizer_scenario_planner.R"), chdir = TRUE)
source(file.path(scripts_dir, "mmm_deck_output_builder.R"), chdir = TRUE)

cache_path <- file.path(script_dir, "showcase_synthetic_inputs.rds")
force_rebuild_data <- identical(tolower(Sys.getenv("MMM_SHOWCASE_REBUILD_DATA", "false")), "true")

if (!file.exists(cache_path) || force_rebuild_data) {
  set.seed(20260603)

  weeks <- seq.Date(as.Date("2024-01-07"), by = "week", length.out = 78)
  markets <- c("Northeast", "South", "Midwest", "West")
  base <- CJ(week = weeks, market = markets)
  base[, week_index := as.integer(factor(week))]
  base[, season := sin(2 * pi * week_index / 52)]
  base[, trend := week_index / max(week_index)]
  base[, market_scale := fifelse(market == "Northeast", 1.15,
                          fifelse(market == "South", 1.05,
                          fifelse(market == "Midwest", 0.92, 0.86)))]

  media_vars <- c("tv", "paid_search", "paid_social", "retail_media", "display")
  base[, tv := pmax(0, 85 + 35 * season + rnorm(.N, 0, 9)) * market_scale]
  base[, paid_search := pmax(0, 65 + 10 * trend + rnorm(.N, 0, 7)) * market_scale]
  base[, paid_social := pmax(0, 55 + 25 * (week_index %% 13 %in% 1:5) + rnorm(.N, 0, 8)) * market_scale]
  base[, retail_media := pmax(0, 38 + 18 * (week_index %% 26 %in% 14:20) + rnorm(.N, 0, 5)) * market_scale]
  base[, display := pmax(0, 30 + 14 * (week_index %% 10 %in% 3:6) + rnorm(.N, 0, 5)) * market_scale]

  spend_rates <- c(tv = 18, paid_search = 9, paid_social = 7, retail_media = 11, display = 4)
  for (v in media_vars) {
    base[, (paste0(v, "_spend")) := get(v) * spend_rates[[v]]]
  }

  sat <- function(x, scale, max_effect) max_effect * (1 - exp(-pmax(x, 0) / scale))
  base[, baseline := 620 * market_scale + 45 * season + 35 * trend]
  base[, tv_contribution := sat(tv, 120, 155)]
  base[, paid_search_contribution := sat(paid_search, 75, 125)]
  base[, paid_social_contribution := sat(paid_social, 70, 92)]
  base[, retail_media_contribution := sat(retail_media, 55, 78)]
  base[, display_contribution := sat(display, 48, 42)]
  base[, promo_contribution := 25 * (week_index %% 13 %in% 10:12)]
  base[, residual := rnorm(.N, 0, 18)]
  base[, y_actual := baseline + tv_contribution + paid_search_contribution + paid_social_contribution +
         retail_media_contribution + display_contribution + promo_contribution + residual]
  base[, pred := y_actual - residual]

  long_decomp <- rbindlist(list(
    base[, .(week, market, variable = "baseline", contribution = baseline, y_actual, pred, residual)],
    base[, .(week, market, variable = "tv", contribution = tv_contribution, y_actual, pred, residual)],
    base[, .(week, market, variable = "paid_search", contribution = paid_search_contribution, y_actual, pred, residual)],
    base[, .(week, market, variable = "paid_social", contribution = paid_social_contribution, y_actual, pred, residual)],
    base[, .(week, market, variable = "retail_media", contribution = retail_media_contribution, y_actual, pred, residual)],
    base[, .(week, market, variable = "display", contribution = display_contribution, y_actual, pred, residual)],
    base[, .(week, market, variable = "promo", contribution = promo_contribution, y_actual, pred, residual)]
  ), fill = TRUE)

  wide_decomp <- base[, .(week, market, y_actual, pred, residual)]
  raw_data <- base[, c("week", "market", media_vars, paste0(media_vars, "_spend")), with = FALSE]
  spend_map <- data.table(variable = media_vars, spend_col = paste0(media_vars, "_spend"))
  channel_map <- data.table(
    variable = c(media_vars, "baseline", "promo"),
    channel = c("TV", "Paid Search", "Paid Social", "Retail Media", "Display", "Baseline", "Promo"),
    role = c(rep("media", length(media_vars)), "baseline_control", "baseline_control")
  )

  curve_grid <- seq(0, 2.2, by = 0.05)
  current_spend <- raw_data[, lapply(.SD, sum), .SDcols = paste0(media_vars, "_spend")]
  current_spend <- setNames(as.numeric(current_spend[1]), media_vars)
  curve_params <- data.table(
    variable = media_vars,
    asymptote = c(52000, 43000, 33000, 28000, 16500),
    rate = c(1.15, 0.75, 0.95, 0.85, 0.65)
  )
  response_curves <- rbindlist(lapply(media_vars, function(v) {
    p <- curve_params[variable == v]
    data.table(
      variable = v,
      spend_multiplier = curve_grid,
      current_spend = current_spend[[v]],
      support = curve_grid * raw_data[, sum(get(v), na.rm = TRUE)],
      current_support = raw_data[, sum(get(v), na.rm = TRUE)],
      contribution = p$asymptote * (1 - exp(-p$rate * curve_grid))
    )
  }))

  draw_curves <- rbindlist(lapply(seq_len(12), function(draw_id) {
    draw_scale <- rlnorm(1, 0, 0.12)
    tmp <- copy(response_curves)
    tmp[, .draw := draw_id]
    tmp[, contribution := contribution * draw_scale * rlnorm(.N, 0, 0.02)]
    tmp[]
  }), fill = TRUE)

  optimizer_output <- run_optimizer_scenario_planner(
    response_curves = response_curves,
    response_curve_draws = draw_curves,
    total_budget = sum(current_spend) * 1.08,
    optimizer_method = "greedy",
    uncertainty_quantile = 0.20,
    optimization_grid_step = 0.55,
    max_grid_combinations = 250000L,
    max_multiplier = 2.2,
    multiplier_grid = curve_grid,
    value_per_kpi = 30,
    scenario_multipliers = c(0.85, 1.00, 1.15, 1.30),
    scenario_plan = data.table(
      scenario = c("search_social_push", "search_social_push", "tv_hold_efficiency_push", "tv_hold_efficiency_push"),
      variable = c("paid_search", "paid_social", "tv", "paid_search"),
      spend_multiplier = c(1.30, 1.25, 1.00, 1.35)
    )
  )

  saveRDS(list(
    media_vars = media_vars,
    long_decomp = long_decomp,
    wide_decomp = wide_decomp,
    raw_data = raw_data,
    spend_map = spend_map,
    channel_map = channel_map,
    optimizer_output = optimizer_output
  ), cache_path)
} else {
  cached <- readRDS(cache_path)
  invisible(list2env(cached, envir = environment()))
}

result <- run_mmm_deck_output_builder(
  long_decomp = long_decomp,
  wide_decomp = wide_decomp,
  raw_data = raw_data,
  spend_map = spend_map,
  optimizer_output = optimizer_output,
  channel_map = channel_map,
  output_dir = output_dir,
  prefix = "showcase",
  media_variables = media_vars,
  baseline_variables = c("baseline", "promo"),
  time_col = "week",
  group_col = "market",
  period_granularity = "week",
  kpi_value_per_outcome = 30,
  write_charts = TRUE,
  write_html = TRUE,
  write_excel = FALSE,
  write_shiny = TRUE,
  top_n_charts = 12
)

manifest <- data.table(
  artifact = c("static_html", "shiny_app", "tables", "charts", "excel"),
  path = c(
    result$files$html_path,
    result$files$shiny_path,
    file.path(output_dir, "tables"),
    file.path(output_dir, "charts"),
    result$files$excel_path
  )
)
fwrite(manifest, file.path(script_dir, "showcase_manifest.csv"))

cat("Chart builder showcase written to:\n")
cat(output_dir, "\n")
cat("Shiny app:\n")
cat(result$files$shiny_path, "\n")
cat("Static HTML:\n")
cat(result$files$html_path, "\n")
