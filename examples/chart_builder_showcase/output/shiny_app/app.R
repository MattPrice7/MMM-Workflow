required <- c("shiny", "plotly", "DT", "ggplot2", "data.table")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages first: install.packages(c(", paste(shQuote(missing), collapse = ", "), "))")
suppressPackageStartupMessages(invisible(lapply(required, library, character.only = TRUE)))
tables <- readRDS("mmm_report_tables.rds")
fmt <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", ifelse(abs(x) >= 1000, format(round(x, 1), big.mark = ",", scientific = FALSE), signif(x, 4)))
}
`%||%` <- function(a, b) {
  if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a
}
table_or_empty <- function(name) {
  if (!is.null(tables[[name]])) as.data.table(tables[[name]]) else data.table()
}
choices_from <- function(table_name, col) {
  dt <- table_or_empty(table_name)
  if (nrow(dt) && col %in% names(dt)) sort(unique(as.character(dt[[col]]))) else character()
}
periods <- table_or_empty("period_slicer_index")
period_index <- copy(periods)
if (nrow(period_index) && "period_start" %in% names(period_index)) {
  period_index[, period_date__ := as.Date(period_start)]
  period_index[, quarter_label := paste0(format(period_date__, "%Y"), " Q", ((as.integer(format(period_date__, "%m")) - 1L) %/% 3L) + 1L)]
  period_index[, year_label := format(period_date__, "%Y")]
}
period_group_choices <- c("All periods" = "__all__")
if (nrow(period_index) && "quarter_label" %in% names(period_index)) {
  q <- unique(period_index[order(period_sort)]$quarter_label)
  y <- unique(period_index[order(period_sort)]$year_label)
  period_group_choices <- c(period_group_choices, stats::setNames(paste0("quarter::", q), q), stats::setNames(paste0("year::", y), y))
}
period_compare_choices <- if (nrow(period_index) && "quarter_label" %in% names(period_index)) unique(period_index[order(period_sort)]$quarter_label) else character()
variable_choices <- sort(unique(c(choices_from("contribution_by_variable", "variable"), choices_from("kpi_economics", "variable"), choices_from("optimizer_response_curves", "variable"))))
curve_choices <- choices_from("optimizer_response_curves", "variable")
stan_posterior_variable_choices <- choices_from("stan_posterior_variable_draws", "variable")
optimizer_posterior_variable_choices <- choices_from("optimizer_scenario_uncertainty_draws_by_variable", "variable")
if (!length(optimizer_posterior_variable_choices)) optimizer_posterior_variable_choices <- choices_from("optimizer_optimization_uncertainty_draws_by_variable", "variable")
posterior_source_choices <- character()
if (length(stan_posterior_variable_choices)) posterior_source_choices <- c(posterior_source_choices, "Stan variable posterior" = "stan")
if (length(optimizer_posterior_variable_choices)) posterior_source_choices <- c(posterior_source_choices, "Optimizer scenario posterior" = "optimizer")
if (!length(posterior_source_choices)) posterior_source_choices <- c("No draw-level posterior tables" = "none")
posterior_variable_choices <- if (length(stan_posterior_variable_choices)) stan_posterior_variable_choices else optimizer_posterior_variable_choices
if (!length(posterior_variable_choices)) posterior_variable_choices <- curve_choices
role_choices <- c("All roles" = "__all__", stats::setNames(sort(unique(as.character(table_or_empty("contribution_by_variable")$role))), sort(unique(as.character(table_or_empty("contribution_by_variable")$role)))))
fit_overlay_choices <- c("None" = "__none__", stats::setNames(variable_choices, variable_choices))
curve_metric_choices <- intersect(c("contribution", "contribution_vs_current", "roi", "mroi", "cost_per_kpi", "value_per_cost"), names(table_or_empty("optimizer_response_curves")))
if (!length(curve_metric_choices)) curve_metric_choices <- "contribution"
econ_metric_choices <- intersect(c("cost_per_outcome", "outcome_per_cost", "value_per_cost", "cost_per_value", "fair_share_index", "efficiency_index", "spend_share", "contribution_share"), names(table_or_empty("kpi_economics")))
if (!length(econ_metric_choices)) econ_metric_choices <- "cost_per_outcome"
scenario_metric_choices <- intersect(c("incremental_contribution", "contribution", "roi", "cost_per_kpi", "expected_profit", "q05_profit", "probability_profit_positive", "probability_incremental_contribution_positive"), names(table_or_empty("optimizer_scenario_comparison")))
if (!length(scenario_metric_choices)) scenario_metric_choices <- "incremental_contribution"
card <- function(label, value) div(class = "metric-card", div(class = "metric-label", label), div(class = "metric-value", value))
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f8fafc; color:#111827; font-family:-apple-system,BlinkMacSystemFont,Segoe UI,sans-serif; }
    .title-row { margin:18px 0 8px; }
    .metric-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:12px; margin:16px 0; }
    .metric-card { background:white; border:1px solid #e5e7eb; border-radius:8px; padding:14px 16px; }
    .metric-label { color:#6b7280; font-size:12px; text-transform:uppercase; }
    .metric-value { font-size:22px; font-weight:700; margin-top:4px; }
    .panel { background:white; border:1px solid #e5e7eb; border-radius:8px; padding:16px; margin-bottom:16px; }
    .control-panel { background:white; border:1px solid #e5e7eb; border-radius:8px; padding:14px 16px; margin-bottom:16px; }
    .selectize-control { max-width:100%; }
    .tab-content { padding-top:8px; }
  "))),
  div(class = "title-row", h2("MMM Deck Output Dashboard"), p("Interactive review layer for decomposition, response curves, KPI economics, optimizer scenarios, and fit diagnostics.")),
  uiOutput("theme_css"),
  uiOutput("metric_cards"),
  div(class = "control-panel", fluidRow(
    column(3, selectInput("period_group", "Reporting period", choices = period_group_choices, selected = "__all__")),
    column(3, uiOutput("variable_filter_ui")),
    column(2, selectInput("role_filter", "Role", choices = role_choices, selected = "__all__")),
    column(2, numericInput("top_n", "Items shown", value = 12, min = 3, max = 50, step = 1)),
    column(2, uiOutput("theme_button_ui"))
  )),
  tabsetPanel(
    tabPanel("Overview", br(), fluidRow(column(8, div(class = "panel", selectInput("fit_overlay_variable", "Fit chart right-axis overlay", choices = fit_overlay_choices, selected = "__none__"), plotlyOutput("actual_fit", height = "450px"))), column(4, div(class = "panel", plotlyOutput("cost_bar", height = "450px")))), fluidRow(column(6, div(class = "panel", plotlyOutput("contribution_bar", height = "430px"))), column(6, div(class = "panel", plotlyOutput("spend_share_plot", height = "430px"))))),
    tabPanel("Curves", br(), div(class = "control-panel", fluidRow(column(3, uiOutput("curve_filter_ui")), column(3, selectInput("curve_metric", "Curve readout", choices = curve_metric_choices, selected = if ("contribution" %in% curve_metric_choices) "contribution" else curve_metric_choices[1])), column(3, checkboxInput("curve_show_interval", "Show q05/q95 band for one selected curve", value = TRUE)), column(3, actionButton("open_chart_colors", "Chart colors")))), div(class = "panel", h4("Response / economics curve"), plotlyOutput("optimizer_curve_plot", height = "540px"))),
    tabPanel("Contribution", br(), div(class = "control-panel", fluidRow(column(4, actionButton("open_period_compare", "Compare periods")), column(8, uiOutput("period_compare_label")))), fluidRow(column(7, div(class = "panel", h4("Contribution over time"), plotlyOutput("contribution_trend_plot", height = "430px"))), column(5, div(class = "panel", h4("Period change due-to"), plotlyOutput("due_to_plot", height = "430px")))), div(class = "panel", h4("Selected period comparison"), plotlyOutput("period_compare_plot", height = "430px"))),
    tabPanel("KPI Economics", br(), div(class = "control-panel", selectInput("econ_metric", "Economics metric", choices = econ_metric_choices, selected = econ_metric_choices[1])), fluidRow(column(6, div(class = "panel", plotlyOutput("spend_scatter", height = "420px"))), column(6, div(class = "panel", plotlyOutput("econ_rank_plot", height = "420px"))))),
    tabPanel("Optimizer", br(), div(class = "control-panel", fluidRow(column(4, selectInput("scenario_metric", "Scenario metric", choices = scenario_metric_choices, selected = scenario_metric_choices[1])), column(8, plotlyOutput("optimizer_scenario_plot", height = "330px")))), fluidRow(column(6, div(class = "panel", plotlyOutput("optimizer_spend_plot", height = "420px"))), column(6, div(class = "panel", plotlyOutput("optimizer_saturation_plot", height = "420px"))))),
    tabPanel("Posterior / Uncertainty", br(), div(class = "control-panel", fluidRow(column(3, selectInput("posterior_source", "Posterior source", choices = posterior_source_choices, selected = posterior_source_choices[1])), column(3, selectInput("posterior_variable", "Variable", choices = posterior_variable_choices, selected = if (length(posterior_variable_choices)) posterior_variable_choices[1] else character())), column(2, selectInput("posterior_scenario", "Scenario", choices = c("Auto" = "__auto__"))), column(2, selectInput("posterior_x", "X metric", choices = c("Contribution" = "contribution", "ROI" = "roi", "mROI" = "mroi", "Cost per KPI" = "cost_per_kpi", "Outcome per cost" = "outcome_per_cost"), selected = "contribution")), column(2, selectInput("posterior_y", "Y metric", choices = c("Contribution" = "contribution", "ROI" = "roi", "mROI" = "mroi", "Cost per KPI" = "cost_per_kpi", "Outcome per cost" = "outcome_per_cost"), selected = "roi")))), fluidRow(column(6, div(class = "panel", h4("Scenario contribution uncertainty"), plotlyOutput("scenario_uncertainty_plot", height = "420px"))), column(6, div(class = "panel", h4("2D posterior draw distribution"), plotlyOutput("posterior_2d_plot", height = "420px"))))),
    tabPanel("Diagnostics", br(), div(class = "panel", DTOutput("flags_table")), div(class = "panel", DTOutput("fit_table")), div(class = "panel", plotlyOutput("residual_plot", height = "380px")), div(class = "panel", h4("Chart registry"), DTOutput("chart_registry_table")))
  )
)
server <- function(input, output, session) {
  preset_palettes <- list(
    "Default light" = c("#2563EB", "#0891B2", "#F59E0B", "#10B981", "#8B5CF6", "#F43F5E", "#64748B", "#0F172A"),
    "Default dark" = c("#60A5FA", "#22D3EE", "#FBBF24", "#34D399", "#A78BFA", "#FB7185", "#CBD5E1", "#F8FAFC")
  )
  parse_hex_colors <- function(txt) {
    if (is.null(txt) || !nzchar(txt)) return(character())
    bits <- unlist(strsplit(gsub(",", " ", txt), " +", fixed = FALSE))
    bits <- bits[nzchar(bits)]
    bits <- ifelse(substr(bits, 1, 1) == "#", bits, paste0("#", bits))
    bits[grepl("^#[0-9A-Fa-f]{6}$", bits)]
  }
  theme_state <- reactiveValues(
    preset = "Default light",
    page_bg = "#f8fafc",
    panel_bg = "#FFFFFF",
    chart_bg = "#FFFFFF",
    header_color = "#111827",
    font_color = "#111827",
    axis_color = "#4B5563",
    grid_color = "#E5E7EB",
    font_family = "-apple-system,BlinkMacSystemFont,Segoe UI,sans-serif",
    base_font_size = 12,
    x_tick_angle = -45,
    series_colors = preset_palettes[["Default light"]]
  )
  selection_state <- reactiveValues(
    vars = variable_choices,
    curve_vars = curve_choices,
    compare_base = if (length(period_compare_choices) >= 2) period_compare_choices[length(period_compare_choices) - 1L] else period_compare_choices[1],
    compare_target = if (length(period_compare_choices) >= 1) period_compare_choices[length(period_compare_choices)] else period_compare_choices[1]
  )
  apply_theme_preset <- function(preset, cols = NULL) {
    if (identical(preset, "Default light")) {
      theme_state$page_bg <- "#F8FAFC"; theme_state$panel_bg <- "#FFFFFF"; theme_state$chart_bg <- "#FFFFFF"; theme_state$header_color <- "#111827"; theme_state$font_color <- "#111827"; theme_state$axis_color <- "#4B5563"; theme_state$grid_color <- "#E5E7EB"; theme_state$series_colors <- preset_palettes[["Default light"]]
    } else if (identical(preset, "Default dark")) {
      theme_state$page_bg <- "#0F172A"; theme_state$panel_bg <- "#111827"; theme_state$chart_bg <- "#111827"; theme_state$header_color <- "#F8FAFC"; theme_state$font_color <- "#E5E7EB"; theme_state$axis_color <- "#CBD5E1"; theme_state$grid_color <- "#334155"; theme_state$series_colors <- preset_palettes[["Default dark"]]
    } else if (identical(preset, "Custom") && length(cols)) {
      theme_state$series_colors <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]
    }
    theme_state$preset <- preset
  }
  palette_values <- reactive({
    cols <- theme_state$series_colors
    cols <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]
    if (length(cols)) cols else preset_palettes[["Default light"]]
  })
  chart_theme <- function() {
    ggplot2::theme_minimal(base_size = theme_state$base_font_size, base_family = theme_state$font_family) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = theme_state$chart_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = theme_state$chart_bg, color = NA),
        legend.background = ggplot2::element_rect(fill = theme_state$chart_bg, color = NA),
        text = ggplot2::element_text(color = theme_state$font_color),
        plot.title = ggplot2::element_text(color = theme_state$header_color, face = "bold"),
        axis.text = ggplot2::element_text(color = theme_state$axis_color),
        axis.text.x = ggplot2::element_text(angle = theme_state$x_tick_angle, hjust = 1, color = theme_state$axis_color),
        axis.title = ggplot2::element_text(color = theme_state$axis_color),
        legend.text = ggplot2::element_text(color = theme_state$font_color),
        legend.title = ggplot2::element_text(color = theme_state$font_color),
        panel.grid.major = ggplot2::element_line(color = theme_state$grid_color),
        panel.grid.minor = ggplot2::element_blank()
      )
  }
  plotly_theme <- function(p, title = NULL, x_title = NULL, y_title = NULL, extra = list()) {
    base <- list(
      title = if (!is.null(title)) list(text = title, font = list(color = theme_state$header_color, family = theme_state$font_family, size = theme_state$base_font_size + 4)) else NULL,
      paper_bgcolor = theme_state$page_bg,
      plot_bgcolor = theme_state$chart_bg,
      font = list(color = theme_state$font_color, family = theme_state$font_family, size = theme_state$base_font_size),
      xaxis = list(title = x_title %||% "", tickangle = theme_state$x_tick_angle, color = theme_state$axis_color, gridcolor = theme_state$grid_color),
      yaxis = list(title = y_title %||% "", color = theme_state$axis_color, gridcolor = theme_state$grid_color),
      legend = list(orientation = "h", x = 0, y = -0.2)
    )
    do.call(plotly::layout, c(list(p), base, extra))
  }
  output$theme_css <- renderUI({
    tags$style(HTML(sprintf("body{background:%s;color:%s;font-family:%s;} .panel,.control-panel,.metric-card{background:%s;color:%s;} .title-row h2,.title-row p{color:%s;}", theme_state$page_bg, theme_state$font_color, theme_state$font_family, theme_state$panel_bg, theme_state$font_color, theme_state$header_color)))
  })
  selection_label <- function(x, all_choices, all_label = "All") {
    x <- intersect(as.character(x), as.character(all_choices))
    if (!length(x) || length(x) == length(all_choices)) all_label else paste(length(x), "selected")
  }
  output$variable_filter_ui <- renderUI({
    tagList(tags$label("Variables"), actionButton("open_variable_filter", selection_label(selection_state$vars, variable_choices, "All variables")))
  })
  output$curve_filter_ui <- renderUI({
    tagList(tags$label("Curves"), actionButton("open_curve_filter", selection_label(selection_state$curve_vars, curve_choices, "All curves")))
  })
  output$theme_button_ui <- renderUI({
    tagList(tags$label("Theme"), actionButton("open_theme", theme_state$preset %||% "Theme"))
  })
  output$period_compare_label <- renderUI({
    base <- paste(selection_state$compare_base %||% character(), collapse = ", ")
    target <- paste(selection_state$compare_target %||% character(), collapse = ", ")
    tags$span(style = "color:#4B5563;", paste0("Comparing ", ifelse(nzchar(base), base, "base"), " vs ", ifelse(nzchar(target), target, "comparison")))
  })
  output$theme_color_inputs_tmp <- renderUI({
    n <- input$theme_series_n_tmp %||% length(theme_state$series_colors)
    n <- max(3, min(10, as.integer(n)))
    vals <- palette_values()
    tagList(lapply(seq_len(n), function(i) {
      tags$div(style = "display:inline-block;margin:0 12px 10px 0;min-width:94px;", tags$label(paste("Series", i)), tags$input(id = paste0("theme_series_", i, "_tmp"), type = "color", value = vals[((i - 1) %% length(vals)) + 1]))
    }))
  })
  observeEvent(input$open_theme, {
    showModal(modalDialog(
      title = "Theme and chart formatting",
      size = "l",
      fluidRow(
        column(4, actionButton("apply_theme_light", "Default light")),
        column(4, actionButton("apply_theme_dark", "Default dark")),
        column(4, tags$strong("Custom theme"))
      ),
      tags$hr(),
      fluidRow(
        column(4, tags$label("Page background"), tags$input(id = "theme_page_bg_tmp", type = "color", value = theme_state$page_bg)),
        column(4, tags$label("Chart background"), tags$input(id = "theme_chart_bg_tmp", type = "color", value = theme_state$chart_bg)),
        column(4, tags$label("Panel background"), tags$input(id = "theme_panel_bg_tmp", type = "color", value = theme_state$panel_bg))
      ),
      fluidRow(
        column(3, tags$label("Header color"), tags$input(id = "theme_header_color_tmp", type = "color", value = theme_state$header_color)),
        column(3, tags$label("Font color"), tags$input(id = "theme_font_color_tmp", type = "color", value = theme_state$font_color)),
        column(3, tags$label("Axis color"), tags$input(id = "theme_axis_color_tmp", type = "color", value = theme_state$axis_color)),
        column(3, tags$label("Grid color"), tags$input(id = "theme_grid_color_tmp", type = "color", value = theme_state$grid_color))
      ),
      fluidRow(
        column(4, selectInput("theme_font_family_tmp", "Font family", choices = c("System" = "-apple-system,BlinkMacSystemFont,Segoe UI,sans-serif", "Arial" = "Arial", "Helvetica" = "Helvetica", "Georgia" = "Georgia", "Courier" = "Courier New"), selected = theme_state$font_family)),
        column(4, numericInput("theme_base_font_size_tmp", "Base font size", value = theme_state$base_font_size, min = 9, max = 22, step = 1)),
        column(4, numericInput("theme_x_tick_angle_tmp", "X-axis label angle", value = theme_state$x_tick_angle, min = -90, max = 90, step = 15))
      ),
      numericInput("theme_series_n_tmp", "Series colors", value = length(theme_state$series_colors), min = 3, max = 10, step = 1),
      uiOutput("theme_color_inputs_tmp"),
      footer = tagList(modalButton("Cancel"), actionButton("apply_theme", "Apply custom theme"))
    ))
  })
  observeEvent(input$apply_theme_light, {
    apply_theme_preset("Default light")
    removeModal()
  })
  observeEvent(input$apply_theme_dark, {
    apply_theme_preset("Default dark")
    removeModal()
  })
  observeEvent(input$apply_theme, {
    preset <- "Custom"
    n <- max(3, min(10, as.integer(input$theme_series_n_tmp %||% length(theme_state$series_colors))))
    cols <- vapply(seq_len(n), function(i) input[[paste0("theme_series_", i, "_tmp")]] %||% palette_values()[((i - 1) %% length(palette_values())) + 1], character(1))
    if (identical(preset, "Custom")) {
      theme_state$page_bg <- input$theme_page_bg_tmp %||% theme_state$page_bg
      theme_state$panel_bg <- input$theme_panel_bg_tmp %||% theme_state$panel_bg
      theme_state$chart_bg <- input$theme_chart_bg_tmp %||% theme_state$chart_bg
      theme_state$header_color <- input$theme_header_color_tmp %||% theme_state$header_color
      theme_state$font_color <- input$theme_font_color_tmp %||% theme_state$font_color
      theme_state$axis_color <- input$theme_axis_color_tmp %||% theme_state$axis_color
      theme_state$grid_color <- input$theme_grid_color_tmp %||% theme_state$grid_color
    }
    apply_theme_preset(preset, cols)
    theme_state$font_family <- input$theme_font_family_tmp %||% theme_state$font_family
    theme_state$base_font_size <- as.numeric(input$theme_base_font_size_tmp %||% theme_state$base_font_size)
    theme_state$x_tick_angle <- as.numeric(input$theme_x_tick_angle_tmp %||% theme_state$x_tick_angle)
    removeModal()
  })
  observeEvent(input$open_variable_filter, {
    showModal(modalDialog(title = "Variables", size = "m", checkboxGroupInput("variables_tmp", NULL, choices = variable_choices, selected = selection_state$vars), footer = tagList(modalButton("Cancel"), actionButton("apply_variables", "Apply"))))
  })
  observeEvent(input$apply_variables, {
    vals <- intersect(as.character(input$variables_tmp %||% variable_choices), variable_choices)
    selection_state$vars <- if (length(vals)) vals else variable_choices
    removeModal()
  })
  observeEvent(input$open_curve_filter, {
    showModal(modalDialog(title = "Curves", size = "m", checkboxGroupInput("curve_variables_tmp", NULL, choices = curve_choices, selected = selection_state$curve_vars), footer = tagList(modalButton("Cancel"), actionButton("apply_curve_variables", "Apply"))))
  })
  observeEvent(input$apply_curve_variables, {
    vals <- intersect(as.character(input$curve_variables_tmp %||% curve_choices), curve_choices)
    selection_state$curve_vars <- if (length(vals)) vals else curve_choices
    removeModal()
  })
  observeEvent(input$open_chart_colors, {
    showModal(modalDialog(title = "Chart colors", size = "l", numericInput("theme_series_n_tmp", "Series colors", value = length(theme_state$series_colors), min = 3, max = 10, step = 1), uiOutput("theme_color_inputs_tmp"), footer = tagList(modalButton("Cancel"), actionButton("apply_chart_colors", "Apply colors"))))
  })
  observeEvent(input$apply_chart_colors, {
    n <- max(3, min(10, as.integer(input$theme_series_n_tmp %||% length(theme_state$series_colors))))
    cols <- vapply(seq_len(n), function(i) input[[paste0("theme_series_", i, "_tmp")]] %||% palette_values()[((i - 1) %% length(palette_values())) + 1], character(1))
    theme_state$series_colors <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]
    theme_state$preset <- "Custom"
    removeModal()
  })
  observeEvent(input$open_period_compare, {
    showModal(modalDialog(title = "Compare periods", size = "m", selectizeInput("compare_base_tmp", "Base periods", choices = period_compare_choices, selected = selection_state$compare_base, multiple = TRUE), selectizeInput("compare_target_tmp", "Comparison periods", choices = period_compare_choices, selected = selection_state$compare_target, multiple = TRUE), footer = tagList(modalButton("Cancel"), actionButton("apply_period_compare", "Apply comparison"))))
  })
  observeEvent(input$apply_period_compare, {
    selection_state$compare_base <- intersect(as.character(input$compare_base_tmp %||% character()), period_compare_choices)
    selection_state$compare_target <- intersect(as.character(input$compare_target_tmp %||% character()), period_compare_choices)
    removeModal()
  })
  color_map <- function(keys) { vals <- palette_values(); stats::setNames(rep(vals, length.out = length(keys)), keys) }
  selected_vars <- reactive({
    vars <- intersect(as.character(selection_state$vars %||% variable_choices), variable_choices)
    if (!length(vars)) variable_choices else vars
  })
  selected_period_labels <- reactive({
    sel <- input$period_group %||% "__all__"
    if (!nrow(period_index) || identical(sel, "__all__")) return(if ("period_label" %in% names(period_index)) as.character(period_index$period_label) else character())
    if (startsWith(sel, "quarter::")) return(as.character(period_index[quarter_label == sub("^quarter::", "", sel)]$period_label))
    if (startsWith(sel, "year::")) return(as.character(period_index[year_label == sub("^year::", "", sel)]$period_label))
    as.character(period_index$period_label)
  })
  filter_periods <- function(dt) {
    if (!nrow(dt) || !(("period_label") %in% names(dt)) || identical(input$period_group %||% "__all__", "__all__")) return(dt)
    dt[as.character(period_label) %in% selected_period_labels()]
  }
  filter_vars <- function(dt, col = "variable") {
    if (!nrow(dt) || !(col %in% names(dt))) return(dt)
    dt[as.character(get(col)) %in% selected_vars()]
  }
  filter_role <- function(dt) {
    if (!nrow(dt) || input$role_filter == "__all__" || !("role" %in% names(dt))) return(dt)
    dt[as.character(role) == input$role_filter]
  }
  dt_widget <- function(dt, page = 20) {
    dt <- as.data.table(dt)
    datatable(dt, options = list(pageLength = page, scrollX = TRUE), filter = "top", rownames = FALSE)
  }
  summary <- table_or_empty("executive_summary")[1]
  output$metric_cards <- renderUI({
    div(class = "metric-grid",
      card("Actual KPI", fmt(summary$actual_kpi)),
      card("Predicted KPI", fmt(summary$predicted_kpi)),
      card("R-squared", fmt(summary$r_squared)),
      card("Cost per outcome", fmt(summary$media_cost_per_outcome))
    )
  })
  selected_contrib <- reactive({
    dt <- if (identical(input$period_group %||% "__all__", "__all__")) { out <- copy(table_or_empty("contribution_by_variable")); out[, period_label := "All periods"]; out } else filter_periods(table_or_empty("contribution_by_period_variable"))[, .(contribution = sum(as.numeric(contribution), na.rm = TRUE), role = role[1], n_rows = sum(as.numeric(n_rows), na.rm = TRUE)), by = variable]
    filter_role(filter_vars(dt))
  })
  selected_econ <- reactive({
    dt <- if (identical(input$period_group %||% "__all__", "__all__")) copy(table_or_empty("kpi_economics")) else filter_periods(table_or_empty("kpi_economics_by_period"))[, .(spend = sum(as.numeric(spend), na.rm = TRUE), contribution = sum(as.numeric(contribution), na.rm = TRUE), role = role[1], spend_col = spend_col[1]), by = variable]
    if (nrow(dt)) {
      total_spend__ <- sum(as.numeric(dt$spend), na.rm = TRUE); total_contrib__ <- sum(abs(as.numeric(dt$contribution)), na.rm = TRUE)
      dt[, `:=`(outcome_per_cost = ifelse(abs(spend) > 1e-8, contribution / spend, NA_real_), cost_per_outcome = ifelse(abs(contribution) > 1e-8, spend / contribution, NA_real_), spend_share = ifelse(total_spend__ > 0, spend / total_spend__, NA_real_), contribution_share = ifelse(total_contrib__ > 0, abs(contribution) / total_contrib__, NA_real_))]
      dt[, `:=`(fair_share_index = ifelse(is.finite(spend_share) & spend_share > 1e-8, contribution_share / spend_share, NA_real_), efficiency_index = outcome_per_cost)]
    }
    filter_vars(dt)
  })
  output$contribution_bar <- renderPlotly({
    dt <- selected_contrib()[role != "residual"][order(-abs(as.numeric(contribution)))]
    validate(need(nrow(dt) > 0, "No contribution rows available."))
    dt <- head(dt, input$top_n)
    p <- ggplot(dt, aes(x = reorder(variable, as.numeric(contribution)), y = as.numeric(contribution), fill = role, text = paste(variable, "<br>Contribution:", fmt(contribution)))) + geom_col(width = 0.72) + coord_flip() + scale_fill_manual(values = color_map(unique(dt$role))) + labs(title = "Contribution by variable", x = NULL, y = "KPI contribution") + chart_theme() + theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text")
  })
  output$cost_bar <- renderPlotly({
    metric <- input$econ_metric
    dt <- selected_econ()
    validate(need(metric %in% names(dt), "Selected economics metric is unavailable."))
    dt[, metric_value__ := suppressWarnings(as.numeric(get(metric)))]
    dt <- dt[is.finite(metric_value__)][order(metric_value__)]
    validate(need(nrow(dt) > 0, "No finite economics rows available."))
    dt <- head(dt, input$top_n)
    p <- ggplot(dt, aes(x = reorder(variable, -metric_value__), y = metric_value__, text = paste(variable, "<br>", metric, ":", fmt(metric_value__)))) + geom_col(fill = palette_values()[2], width = 0.72) + coord_flip() + labs(title = paste("Ranked", gsub("_", " ", metric)), x = NULL, y = gsub("_", " ", metric)) + chart_theme()
    ggplotly(p, tooltip = "text")
  })
  output$actual_fit <- renderPlotly({
    dt <- copy(table_or_empty("fit_by_period"))
    validate(need(nrow(dt) > 0 && all(c("period_label", "period_sort", "actual", "pred") %in% names(dt)), "No fit-by-period rows available."))
    setorder(dt, period_sort)
    p <- plot_ly(dt, x = ~period_label)
    p <- add_lines(p, y = ~actual, name = "Actual", line = list(color = palette_values()[1]), hovertemplate = "%{x}<br>Actual: %{y:,.2f}<extra></extra>")
    p <- add_lines(p, y = ~pred, name = "Fitted", line = list(color = palette_values()[2]), hovertemplate = "%{x}<br>Fitted: %{y:,.2f}<extra></extra>")
    if (!is.null(input$fit_overlay_variable) && input$fit_overlay_variable != "__none__") {
      ov <- table_or_empty("contribution_by_period_variable")[variable == input$fit_overlay_variable, .(overlay = sum(as.numeric(contribution), na.rm = TRUE)), by = .(period_sort, period_label)]
      dt <- merge(dt, ov, by = c("period_sort", "period_label"), all.x = TRUE, sort = FALSE)
      setorder(dt, period_sort)
      p <- add_bars(p, data = dt, x = ~period_label, y = ~overlay, name = input$fit_overlay_variable, yaxis = "y2", marker = list(color = "rgba(37,99,235,0.24)"), hovertemplate = paste0("%{x}<br>", input$fit_overlay_variable, ": %{y:,.2f}<extra></extra>"))
      p <- layout(p, yaxis2 = list(title = input$fit_overlay_variable, overlaying = "y", side = "right", showgrid = FALSE))
    }
    layout(p, title = list(text = "Actual vs fitted KPI", font = list(color = theme_state$header_color, family = theme_state$font_family, size = theme_state$base_font_size + 4)), paper_bgcolor = theme_state$page_bg, plot_bgcolor = theme_state$chart_bg, font = list(color = theme_state$font_color, family = theme_state$font_family, size = theme_state$base_font_size), xaxis = list(title = "", tickangle = theme_state$x_tick_angle, categoryorder = "array", categoryarray = dt$period_label, color = theme_state$axis_color, gridcolor = theme_state$grid_color), yaxis = list(title = "KPI", color = theme_state$axis_color, gridcolor = theme_state$grid_color), legend = list(orientation = "h", x = 0, y = -0.25), barmode = "overlay")
  })
  output$spend_share_plot <- renderPlotly({
    dt <- selected_econ()[is.finite(spend_share) & is.finite(contribution_share)]
    validate(need(nrow(dt) > 0, "No spend-share/contribution-share rows available."))
    p <- ggplot(dt, aes(x = spend_share, y = contribution_share, text = paste(variable, "<br>Spend share:", fmt(spend_share), "<br>Contribution share:", fmt(contribution_share), "<br>Fair-share index:", fmt(fair_share_index)))) + geom_abline(slope = 1, intercept = 0, color = "#9CA3AF", linetype = "dashed") + geom_point(color = palette_values()[2], size = 3) + labs(title = "Fair-share index: contribution share vs spend share", x = "Spend share", y = "Contribution share") + chart_theme()
    ggplotly(p, tooltip = "text")
  })
  output$spend_scatter <- renderPlotly({
    dt <- selected_econ()[is.finite(spend) & is.finite(contribution)]
    validate(need(nrow(dt) > 0, "No spend and contribution rows available."))
    dt[, bubble_size__ := pmax(abs(as.numeric(contribution)), 1e-8)]
    p <- ggplot(dt, aes(x = spend, y = contribution, size = bubble_size__, color = fair_share_index, text = paste(variable, "<br>Spend:", fmt(spend), "<br>Contribution:", fmt(contribution), "<br>Fair-share index:", fmt(fair_share_index)))) + geom_point(alpha = 0.72) + scale_size_continuous(range = c(8, 28), guide = "none") + scale_color_gradient2(low = "#DC2626", mid = "#9CA3AF", high = "#16A34A", midpoint = 1, na.value = palette_values()[2]) + labs(title = "Spend vs KPI contribution bubble chart", x = "Spend", y = "Contribution", color = "Fair-share index") + chart_theme()
    ggplotly(p, tooltip = "text")
  })
  output$econ_rank_plot <- renderPlotly({
    metric <- input$econ_metric
    dt <- selected_econ()
    validate(need(metric %in% names(dt), "Selected economics metric is unavailable."))
    dt[, metric_value__ := suppressWarnings(as.numeric(get(metric)))]
    dt <- dt[is.finite(metric_value__)][order(metric_value__)]
    validate(need(nrow(dt) > 0, "No finite economics rows available."))
    dt <- head(dt, input$top_n)
    p <- ggplot(dt, aes(x = reorder(variable, -metric_value__), y = metric_value__, text = paste(variable, "<br>", metric, ":", fmt(metric_value__)))) + geom_col(fill = palette_values()[2], width = 0.72) + coord_flip() + labs(title = paste("Ranked", gsub("_", " ", metric)), x = NULL, y = gsub("_", " ", metric)) + chart_theme()
    ggplotly(p, tooltip = "text")
  })
  output$residual_plot <- renderPlotly({
    dt <- copy(table_or_empty("fit_by_period"))
    validate(need(nrow(dt) > 0 && "residual" %in% names(dt), "No residual rows available."))
    p <- ggplot(dt, aes(x = reorder(period_label, period_sort), y = as.numeric(residual), text = paste(period_label, "<br>Residual:", fmt(residual)))) + geom_hline(yintercept = 0, color = "#6B7280") + geom_col(fill = "#DC2626", width = 0.78) + labs(title = "Residuals by period", x = NULL, y = "Actual minus fitted") + chart_theme() + theme(axis.text.x = element_text(angle = theme_state$x_tick_angle, hjust = 1))
    ggplotly(p, tooltip = "text")
  })
  selected_curve_vars <- reactive({
    vars <- intersect(as.character(selection_state$curve_vars %||% curve_choices), curve_choices)
    if (length(vars)) vars else curve_choices
  })
  curve_dt <- reactive({
    dt <- table_or_empty("optimizer_response_curves")
    if (!nrow(dt) || !("variable" %in% names(dt))) return(dt)
    dt[variable %in% selected_curve_vars()]
  })
  draw_curve <- function(metric, title) {
    dt <- copy(curve_dt())
    validate(need(nrow(dt) > 0 && all(c("variable", "spend_multiplier") %in% names(dt)) && metric %in% names(dt), paste("No curve rows available for", metric)))
    dt[, y_metric__ := suppressWarnings(as.numeric(get(metric)))]
    dt <- dt[is.finite(y_metric__)]
    validate(need(nrow(dt) > 0, paste("No finite curve rows available for", metric)))
    dt[, spend_multiplier__ := suppressWarnings(as.numeric(spend_multiplier))]
    dt <- dt[is.finite(spend_multiplier__)]
    data.table::setorder(dt, variable, spend_multiplier__)
    vars <- unique(as.character(dt$variable))
    cols <- color_map(vars)
    p <- plot_ly()
    q05 <- paste0(metric, "_q05"); q95 <- paste0(metric, "_q95")
    if (length(vars) == 1L && isTRUE(input$curve_show_interval) && q05 %in% names(dt) && q95 %in% names(dt)) {
      dt[, y_low__ := suppressWarnings(as.numeric(get(q05)))]
      dt[, y_high__ := suppressWarnings(as.numeric(get(q95)))]
      band <- dt[is.finite(y_low__) & is.finite(y_high__)]
      if (nrow(band)) {
        band_poly <- rbind(band[, .(spend_multiplier__, y_band__ = y_high__)], band[.N:1, .(spend_multiplier__, y_band__ = y_low__)])
        p <- add_trace(p, data = band_poly, x = ~spend_multiplier__, y = ~y_band__, type = "scatter", mode = "lines", fill = "toself", fillcolor = "rgba(37,99,235,0.18)", line = list(color = "rgba(37,99,235,0)"), name = "q05-q95 band", hoverinfo = "skip", showlegend = TRUE)
      }
    }
    for (v in vars) {
      sub <- dt[as.character(variable) == v]
      p <- add_trace(p, data = sub, x = ~spend_multiplier__, y = ~y_metric__, type = "scatter", mode = "lines", name = v, line = list(color = unname(cols[[v]]), width = 2.3), hovertemplate = paste0(v, "<br>Multiplier: %{x:.2f}<br>", metric, ": %{y:,.2f}<extra></extra>"))
      cur <- sub[which.min(abs(spend_multiplier__ - 1))]
      if (nrow(cur)) p <- add_trace(p, data = cur, x = ~spend_multiplier__, y = ~y_metric__, type = "scatter", mode = "markers", name = paste(v, "current"), showlegend = FALSE, marker = list(color = unname(cols[[v]]), size = 8, symbol = "circle"), hovertemplate = paste0(v, " current<br>Multiplier: %{x:.2f}<br>", metric, ": %{y:,.2f}<extra></extra>"))
    }
    plotly_theme(p, title = title, x_title = "Spend/support multiplier", y_title = gsub("_", " ", metric))
  }
  output$optimizer_curve_plot <- renderPlotly({ draw_curve(input$curve_metric, paste(gsub("_", " ", input$curve_metric), "curve")) })
  output$optimizer_spend_plot <- renderPlotly({
    dt <- filter_vars(table_or_empty("optimizer_plan"))
    validate(need(nrow(dt) > 0 && all(c("variable", "current_spend", "recommended_spend") %in% names(dt)), "No optimizer plan rows available."))
    dt <- dt[, .(variable, current_spend = as.numeric(current_spend), recommended_spend = as.numeric(recommended_spend))]
    long <- melt(dt, id.vars = "variable", measure.vars = c("current_spend", "recommended_spend"), variable.name = "plan", value.name = "spend")
    long[, plan := fifelse(plan == "current_spend", "Current", "Recommended")]
    p <- ggplot(long, aes(x = reorder(variable, spend, FUN = max, na.rm = TRUE), y = spend, fill = plan, text = paste(variable, "<br>", plan, ":", fmt(spend)))) + geom_col(position = "dodge", width = 0.72) + coord_flip() + labs(title = "Current vs recommended spend", x = NULL, y = "Spend") + chart_theme() + theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text")
  })
  output$optimizer_scenario_plot <- renderPlotly({
    metric <- input$scenario_metric
    dt <- table_or_empty("optimizer_scenario_comparison")
    validate(need(nrow(dt) > 0 && "plan_name" %in% names(dt) && metric %in% names(dt), "No optimizer scenario rows available."))
    dt[, metric_value__ := suppressWarnings(as.numeric(get(metric)))]
    dt <- dt[is.finite(metric_value__)]
    validate(need(nrow(dt) > 0, "No finite scenario rows available."))
    p <- ggplot(dt, aes(x = reorder(plan_name, metric_value__), y = metric_value__, fill = plan_type, text = paste(plan_name, "<br>", metric, ":", fmt(metric_value__)))) + geom_col(width = 0.72) + coord_flip() + scale_fill_manual(values = color_map(unique(dt$plan_type))) + labs(title = paste("Scenario", gsub("_", " ", metric)), x = NULL, y = gsub("_", " ", metric)) + chart_theme() + theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text")
  })
  output$optimizer_saturation_plot <- renderPlotly({
    dt <- filter_vars(table_or_empty("optimizer_saturation_headroom"))
    validate(need(nrow(dt) > 0 && all(c("variable", "pct_of_peak_grid_contribution") %in% names(dt)), "No saturation/headroom rows available."))
    dt <- dt[is.finite(as.numeric(pct_of_peak_grid_contribution))]
    p <- ggplot(dt, aes(x = reorder(variable, as.numeric(pct_of_peak_grid_contribution)), y = as.numeric(pct_of_peak_grid_contribution), fill = saturation_band, text = paste(variable, "<br>Share of peak:", fmt(100 * as.numeric(pct_of_peak_grid_contribution)), "%"))) + geom_col(width = 0.72) + coord_flip() + labs(title = "Saturation and response headroom", x = NULL, y = "Current share of peak grid contribution") + chart_theme() + theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text")
  })
  output$scenario_uncertainty_plot <- renderPlotly({
    dt <- table_or_empty("optimizer_scenario_uncertainty_summary")
    validate(need(nrow(dt) > 0 && all(c("scenario", "contribution_q05", "contribution_q50", "contribution_q95") %in% names(dt)), "No scenario uncertainty rows available."))
    dt <- dt[is.finite(as.numeric(contribution_q50))]
    validate(need(nrow(dt) > 0, "No finite scenario uncertainty rows available."))
    setorder(dt, contribution_q50)
    p <- plot_ly(dt, x = ~contribution_q50, y = ~reorder(scenario, contribution_q50), type = "scatter", mode = "markers", marker = list(color = palette_values()[1], size = 9), error_x = list(type = "data", symmetric = FALSE, array = ~pmax(0, contribution_q95 - contribution_q50), arrayminus = ~pmax(0, contribution_q50 - contribution_q05), color = "rgba(37,99,235,0.35)", thickness = 1.5), hovertemplate = "%{y}<br>q50 contribution: %{x:,.2f}<extra></extra>")
    plotly_theme(p, title = "Scenario contribution uncertainty", x_title = "Contribution", y_title = "")
  })
  output$contribution_trend_plot <- renderPlotly({
    dt <- filter_role(filter_vars(table_or_empty("contribution_by_period_variable")))[role != "residual"]
    validate(need(nrow(dt) > 0, "No contribution trend rows available."))
    one_var <- length(selected_vars()) == 1L && selected_vars()[1] %in% as.character(dt$variable)
    if (one_var) {
      dt <- dt[, .(contribution = sum(as.numeric(contribution), na.rm = TRUE), contribution_q05 = if ("contribution_q05" %in% names(.SD)) sum(as.numeric(contribution_q05), na.rm = TRUE) else NA_real_, contribution_q95 = if ("contribution_q95" %in% names(.SD)) sum(as.numeric(contribution_q95), na.rm = TRUE) else NA_real_), by = .(period_sort, period_label, variable)]
      setorder(dt, period_sort)
      p <- plot_ly(dt, x = ~period_label)
      if (all(c("contribution_q05", "contribution_q95") %in% names(dt)) && any(is.finite(dt$contribution_q05) & is.finite(dt$contribution_q95))) {
        band <- dt[is.finite(contribution_q05) & is.finite(contribution_q95)]
        band_poly <- rbind(band[, .(period_label, period_sort, y_band__ = contribution_q95)], band[.N:1, .(period_label, period_sort, y_band__ = contribution_q05)])
        p <- add_trace(p, data = band_poly, x = ~period_label, y = ~y_band__, type = "scatter", mode = "lines", fill = "toself", fillcolor = "rgba(37,99,235,0.18)", line = list(color = "rgba(37,99,235,0)"), name = "q05-q95 band", hoverinfo = "skip")
      }
      p <- add_lines(p, data = dt, x = ~period_label, y = ~contribution, name = selected_vars()[1], line = list(color = palette_values()[1], width = 2.2), hovertemplate = "%{x}<br>Contribution: %{y:,.2f}<extra></extra>")
      p <- add_markers(p, data = dt, x = ~period_label, y = ~contribution, name = "points", showlegend = FALSE, marker = list(color = palette_values()[1], size = 5), hovertemplate = "%{x}<br>Contribution: %{y:,.2f}<extra></extra>")
      plotly_theme(p, title = paste("Contribution trend:", selected_vars()[1]), x_title = "", y_title = "Contribution")
    } else {
      dt <- dt[, .(contribution = sum(as.numeric(contribution), na.rm = TRUE)), by = .(period_sort, period_label, variable)]
      p <- ggplot(dt, aes(x = reorder(period_label, period_sort), y = contribution, fill = variable, text = paste(period_label, "<br>", variable, ":", fmt(contribution)))) + geom_col(width = 0.82) + scale_fill_manual(values = color_map(unique(dt$variable))) + labs(title = "Contribution trend by variable", x = NULL, y = "Contribution") + chart_theme() + theme(axis.text.x = element_text(angle = theme_state$x_tick_angle, hjust = 1), legend.position = "bottom")
      ggplotly(p, tooltip = "text")
    }
  })
  output$due_to_plot <- renderPlotly({
    dt <- filter_vars(filter_periods(table_or_empty("period_due_to_variable")))[is.finite(contribution_change)]
    validate(need(nrow(dt) > 0, "No due-to rows available."))
    latest <- dt[period_sort == max(period_sort, na.rm = TRUE)][order(-abs(contribution_change))]
    latest <- head(latest, input$top_n)
    p <- ggplot(latest, aes(x = reorder(variable, as.numeric(contribution_change)), y = as.numeric(contribution_change), text = paste(variable, "<br>Change contribution:", fmt(contribution_change)))) + geom_hline(yintercept = 0, color = "#9CA3AF") + geom_col(fill = palette_values()[3], width = 0.72) + coord_flip() + labs(title = "Latest period due-to contribution change", x = NULL, y = "Contribution change") + chart_theme()
    ggplotly(p, tooltip = "text")
  })
  output$period_compare_plot <- renderPlotly({
    dt <- filter_role(filter_vars(table_or_empty("contribution_by_period_variable")))[role != "residual"]
    validate(need(nrow(dt) > 0 && nrow(period_index) > 0 && length(selection_state$compare_base) && length(selection_state$compare_target), "Choose base and comparison periods."))
    pi <- period_index[, .(period_label, quarter_label)]
    dt <- merge(dt, pi, by = "period_label", all.x = TRUE, sort = FALSE)
    base <- dt[quarter_label %in% selection_state$compare_base, .(base_contribution = sum(as.numeric(contribution), na.rm = TRUE)), by = variable]
    comp <- dt[quarter_label %in% selection_state$compare_target, .(comparison_contribution = sum(as.numeric(contribution), na.rm = TRUE)), by = variable]
    out <- merge(base, comp, by = "variable", all = TRUE)
    out[is.na(base_contribution), base_contribution := 0]
    out[is.na(comparison_contribution), comparison_contribution := 0]
    out[, `:=`(change = comparison_contribution - base_contribution, pct_change = ifelse(abs(base_contribution) > 1e-8, (comparison_contribution - base_contribution) / abs(base_contribution), NA_real_))]
    out <- out[is.finite(change)][order(-abs(change))]
    validate(need(nrow(out) > 0, "No comparable contribution rows available."))
    out <- head(out, input$top_n)
    p <- ggplot(out, aes(x = reorder(variable, change), y = change, fill = change >= 0, text = paste(variable, "<br>Base:", fmt(base_contribution), "<br>Comparison:", fmt(comparison_contribution), "<br>Change:", fmt(change), "<br>% change:", fmt(100 * pct_change), "%"))) + geom_hline(yintercept = 0, color = "#9CA3AF") + geom_col(width = 0.72, show.legend = FALSE) + coord_flip() + scale_fill_manual(values = c("TRUE" = "#16A34A", "FALSE" = "#DC2626")) + labs(title = "Contribution change: selected periods", x = NULL, y = "Contribution change") + chart_theme()
    ggplotly(p, tooltip = "text")
  })
  stan_posterior_draw_dt <- reactive({ table_or_empty("stan_posterior_variable_draws") })
  optimizer_posterior_draw_dt <- reactive({
    dt <- table_or_empty("optimizer_scenario_uncertainty_draws_by_variable")
    if (!nrow(dt)) dt <- table_or_empty("optimizer_optimization_uncertainty_draws_by_variable")
    dt
  })
  posterior_draw_dt <- reactive({
    src <- input$posterior_source %||% if (nrow(stan_posterior_draw_dt())) "stan" else "optimizer"
    if (identical(src, "stan")) return(stan_posterior_draw_dt())
    if (identical(src, "optimizer")) return(optimizer_posterior_draw_dt())
    data.table()
  })
  observe({
    dt <- posterior_draw_dt()
    choices <- if (nrow(dt) && "variable" %in% names(dt)) sort(unique(as.character(dt$variable))) else character()
    preferred <- intersect(choices, curve_choices)
    selected <- if (length(preferred)) preferred[1] else if (length(choices)) choices[1] else character()
    updateSelectInput(session, "posterior_variable", choices = stats::setNames(choices, choices), selected = selected)
  })
  observe({
    dt <- posterior_draw_dt()
    choices <- if (!identical(input$posterior_source, "stan") && nrow(dt) && "scenario" %in% names(dt)) sort(unique(as.character(dt$scenario))) else character()
    updateSelectInput(session, "posterior_scenario", choices = c("Auto" = "__auto__", stats::setNames(choices, choices)), selected = if (length(choices)) choices[1] else "__auto__")
  })
  output$posterior_2d_plot <- renderPlotly({
    dt <- posterior_draw_dt()
    validate(need(nrow(dt) > 0, "No draw-level posterior rows available. Pass posterior_decomp_draws for Stan diagnostics or optimizer draw curves for scenario uncertainty."))
    vars <- input$posterior_variable %||% selected_vars()[1]
    dt <- dt[as.character(variable) == vars]
    if (!identical(input$posterior_source, "stan") && !is.null(input$posterior_scenario) && input$posterior_scenario != "__auto__" && "scenario" %in% names(dt)) dt <- dt[as.character(scenario) == input$posterior_scenario]
    xmetric <- input$posterior_x %||% "roi"; ymetric <- input$posterior_y %||% "contribution"
    validate(need(nrow(dt) > 0 && xmetric %in% names(dt) && ymetric %in% names(dt), "No posterior draws available for the selected variable/scenario/metrics."))
    dt[, x__ := suppressWarnings(as.numeric(get(xmetric)))]
    dt[, y__ := suppressWarnings(as.numeric(get(ymetric)))]
    dt <- dt[is.finite(x__) & is.finite(y__)]
    validate(need(nrow(dt) > 2, "Not enough finite posterior draws for a 2D distribution."))
    p <- plot_ly(dt, x = ~x__, y = ~y__, type = "scatter", mode = "markers", marker = list(color = palette_values()[1], size = 6, opacity = 0.35), hovertemplate = paste0(vars, "<br>", xmetric, ": %{x:,.4f}<br>", ymetric, ": %{y:,.2f}<extra></extra>"))
    if (nrow(dt) >= 20) p <- add_histogram2dcontour(p, data = dt, x = ~x__, y = ~y__, contours = list(coloring = "none"), line = list(color = "rgba(15,23,42,0.45)", width = 1), showscale = FALSE, hoverinfo = "skip")
    title_prefix <- if (identical(input$posterior_source, "stan")) "Stan posterior variable shadow:" else "Optimizer scenario posterior:"
    plotly_theme(p, title = paste(title_prefix, vars), x_title = gsub("_", " ", xmetric), y_title = gsub("_", " ", ymetric))
  })
  output$contribution_table <- renderDT(dt_widget(selected_contrib(), 20))
  output$trend_table <- renderDT(dt_widget(filter_role(filter_vars(table_or_empty("contribution_by_period_variable"))), 20))
  output$econ_table <- renderDT(dt_widget(selected_econ(), 20))
  output$curve_table <- renderDT(dt_widget(curve_dt(), 20))
  output$curve_uncertainty_table <- renderDT(dt_widget(table_or_empty("optimizer_response_curve_uncertainty"), 20))
  output$scenario_uncertainty_table <- renderDT(dt_widget(table_or_empty("optimizer_scenario_uncertainty_summary"), 20))
  output$optimization_uncertainty_table <- renderDT(dt_widget(table_or_empty("optimizer_optimization_uncertainty_summary"), 20))
  output$optimizer_plan_table <- renderDT(dt_widget(filter_vars(table_or_empty("optimizer_plan")), 20))
  output$optimizer_scenario_table <- renderDT(dt_widget(table_or_empty("optimizer_scenario_comparison"), 20))
  output$flags_table <- renderDT(dt_widget(table_or_empty("diagnostic_flags"), 10))
  output$fit_table <- renderDT(dt_widget(table_or_empty("fit_diagnostics"), 10))
  output$chart_registry_table <- renderDT(dt_widget(table_or_empty("chart_registry"), 20))
}
shinyApp(ui, server)
