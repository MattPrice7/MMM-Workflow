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
period_choices <- c("All periods" = "__all__")
if (nrow(periods)) period_choices <- c(period_choices, stats::setNames(periods$period_label, periods$period_label))
variable_choices <- sort(unique(c(choices_from("contribution_by_variable", "variable"), choices_from("kpi_economics", "variable"), choices_from("optimizer_response_curves", "variable"))))
curve_choices <- choices_from("optimizer_response_curves", "variable")
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
    column(2, selectInput("period", "Period", choices = period_choices, selected = "__all__")),
    column(4, selectizeInput("variables", "Variables", choices = variable_choices, selected = variable_choices, multiple = TRUE, options = list(plugins = list("remove_button")))),
    column(2, selectInput("role_filter", "Role", choices = role_choices, selected = "__all__")),
    column(2, numericInput("top_n", "Top N", value = 12, min = 3, max = 50, step = 1)),
    column(2, actionButton("open_theme", "Theme / Format"))
  )),
  tabsetPanel(
    tabPanel("Overview", br(), fluidRow(column(8, div(class = "panel", selectInput("fit_overlay_variable", "Fit chart right-axis overlay", choices = fit_overlay_choices, selected = "__none__"), plotlyOutput("actual_fit", height = "450px"))), column(4, div(class = "panel", plotlyOutput("cost_bar", height = "450px")))), fluidRow(column(6, div(class = "panel", plotlyOutput("contribution_bar", height = "430px"))), column(6, div(class = "panel", plotlyOutput("spend_share_plot", height = "430px"))))),
    tabPanel("Curves", br(), div(class = "control-panel", fluidRow(column(5, selectizeInput("curve_variables", "Curve variables", choices = curve_choices, selected = curve_choices, multiple = TRUE, options = list(plugins = list("remove_button")))), column(3, selectInput("curve_metric", "Response curve metric", choices = curve_metric_choices, selected = if ("contribution" %in% curve_metric_choices) "contribution" else curve_metric_choices[1])), column(3, selectInput("curve_roi_metric", "ROI / marginal metric", choices = curve_metric_choices, selected = if ("mroi" %in% curve_metric_choices) "mroi" else curve_metric_choices[1])))), fluidRow(column(6, div(class = "panel", plotlyOutput("optimizer_response_curve", height = "450px"))), column(6, div(class = "panel", plotlyOutput("optimizer_mroi_curve", height = "450px")))), div(class = "panel", DTOutput("curve_table"))),
    tabPanel("Contribution", br(), fluidRow(column(7, div(class = "panel", plotlyOutput("contribution_trend_plot", height = "430px"))), column(5, div(class = "panel", plotlyOutput("due_to_plot", height = "430px")))), div(class = "panel", DTOutput("contribution_table")), div(class = "panel", DTOutput("trend_table"))),
    tabPanel("KPI Economics", br(), div(class = "control-panel", selectInput("econ_metric", "Economics metric", choices = econ_metric_choices, selected = econ_metric_choices[1])), fluidRow(column(6, div(class = "panel", plotlyOutput("spend_scatter", height = "420px"))), column(6, div(class = "panel", plotlyOutput("econ_rank_plot", height = "420px")))), div(class = "panel", DTOutput("econ_table"))),
    tabPanel("Optimizer", br(), div(class = "control-panel", fluidRow(column(4, selectInput("scenario_metric", "Scenario metric", choices = scenario_metric_choices, selected = scenario_metric_choices[1])), column(8, plotlyOutput("optimizer_scenario_plot", height = "330px")))), fluidRow(column(6, div(class = "panel", plotlyOutput("optimizer_spend_plot", height = "420px"))), column(6, div(class = "panel", plotlyOutput("optimizer_saturation_plot", height = "420px")))), div(class = "panel", h4("Recommended plan"), DTOutput("optimizer_plan_table")), div(class = "panel", h4("Scenario comparison"), DTOutput("optimizer_scenario_table"))),
    tabPanel("Diagnostics", br(), div(class = "panel", DTOutput("flags_table")), div(class = "panel", DTOutput("fit_table")), div(class = "panel", plotlyOutput("residual_plot", height = "380px")), div(class = "panel", h4("Chart registry"), DTOutput("chart_registry_table")))
  )
)
server <- function(input, output, session) {
  preset_palettes <- list(
    "Executive blue" = c("#2563EB", "#0891B2", "#F59E0B", "#10B981", "#8B5CF6", "#F43F5E", "#64748B", "#0F172A"),
    "Client neutral" = c("#0F172A", "#2563EB", "#64748B", "#D97706", "#059669", "#7C3AED", "#BE123C", "#0891B2"),
    "High contrast" = c("#111827", "#1D4ED8", "#B45309", "#047857", "#9333EA", "#BE185D", "#0E7490", "#4B5563")
  )
  parse_hex_colors <- function(txt) {
    if (is.null(txt) || !nzchar(txt)) return(character())
    bits <- unlist(strsplit(gsub(",", " ", txt), " +", fixed = FALSE))
    bits <- bits[nzchar(bits)]
    bits <- ifelse(substr(bits, 1, 1) == "#", bits, paste0("#", bits))
    bits[grepl("^#[0-9A-Fa-f]{6}$", bits)]
  }
  theme_state <- reactiveValues(
    preset = "Executive blue",
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
    series_colors = preset_palettes[["Executive blue"]]
  )
  palette_values <- reactive({
    cols <- theme_state$series_colors
    cols <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]
    if (length(cols)) cols else preset_palettes[["Executive blue"]]
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
      selectInput("theme_preset_tmp", "Preset", choices = c(names(preset_palettes), "Custom"), selected = theme_state$preset),
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
      footer = tagList(modalButton("Cancel"), actionButton("apply_theme", "Apply theme"))
    ))
  })
  observeEvent(input$apply_theme, {
    preset <- input$theme_preset_tmp %||% "Executive blue"
    n <- max(3, min(10, as.integer(input$theme_series_n_tmp %||% length(theme_state$series_colors))))
    cols <- vapply(seq_len(n), function(i) input[[paste0("theme_series_", i, "_tmp")]] %||% palette_values()[((i - 1) %% length(palette_values())) + 1], character(1))
    if (preset %in% names(preset_palettes) && !identical(preset, "Custom")) cols <- preset_palettes[[preset]]
    theme_state$preset <- preset
    theme_state$page_bg <- input$theme_page_bg_tmp %||% theme_state$page_bg
    theme_state$panel_bg <- input$theme_panel_bg_tmp %||% theme_state$panel_bg
    theme_state$chart_bg <- input$theme_chart_bg_tmp %||% theme_state$chart_bg
    theme_state$header_color <- input$theme_header_color_tmp %||% theme_state$header_color
    theme_state$font_color <- input$theme_font_color_tmp %||% theme_state$font_color
    theme_state$axis_color <- input$theme_axis_color_tmp %||% theme_state$axis_color
    theme_state$grid_color <- input$theme_grid_color_tmp %||% theme_state$grid_color
    theme_state$font_family <- input$theme_font_family_tmp %||% theme_state$font_family
    theme_state$base_font_size <- as.numeric(input$theme_base_font_size_tmp %||% theme_state$base_font_size)
    theme_state$x_tick_angle <- as.numeric(input$theme_x_tick_angle_tmp %||% theme_state$x_tick_angle)
    theme_state$series_colors <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]
    removeModal()
  })
  color_map <- function(keys) { vals <- palette_values(); stats::setNames(rep(vals, length.out = length(keys)), keys) }
  selected_vars <- reactive({
    vars <- input$variables
    if (is.null(vars) || !length(vars)) variable_choices else vars
  })
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
    w <- datatable(dt, options = list(pageLength = page, scrollX = TRUE), filter = "top", rownames = FALSE)
    bar_cols <- intersect(names(dt), c("spend", "contribution", "outcome_per_cost", "cost_per_outcome", "value_per_cost", "cost_per_value", "roi", "mroi", "expected_roi", "expected_mroi", "fair_share_index", "efficiency_index", "spend_share", "contribution_share", "probability_profit_positive", "probability_incremental_contribution_positive"))
    for (cc in bar_cols) {
      vals <- suppressWarnings(as.numeric(dt[[cc]]))
      if (any(is.finite(vals)) && min(vals, na.rm = TRUE) >= 0 && max(vals, na.rm = TRUE) > 0) {
        w <- formatStyle(w, cc, background = styleColorBar(c(0, max(vals, na.rm = TRUE)), "#DBEAFE"), backgroundSize = "98% 88%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
      }
    }
    w
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
    dt <- if (input$period == "__all__") { out <- copy(table_or_empty("contribution_by_variable")); out[, period_label := "All periods"]; out } else table_or_empty("contribution_by_period_variable")[period_label == input$period]
    filter_role(filter_vars(dt))
  })
  selected_econ <- reactive({
    dt <- if (input$period == "__all__") copy(table_or_empty("kpi_economics")) else table_or_empty("kpi_economics_by_period")[period_label == input$period]
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
    p <- ggplot(dt, aes(x = spend, y = contribution, text = paste(variable, "<br>Spend:", fmt(spend), "<br>Contribution:", fmt(contribution)))) + geom_point(color = palette_values()[2], size = 3) + labs(title = "Spend vs KPI contribution", x = "Spend", y = "Contribution") + chart_theme()
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
    vars <- input$curve_variables
    if (!is.null(vars) && length(vars)) vars else curve_choices
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
    for (v in vars) {
      sub <- dt[as.character(variable) == v]
      p <- add_trace(p, data = sub, x = ~spend_multiplier__, y = ~y_metric__, type = "scatter", mode = "lines", name = v, line = list(color = unname(cols[[v]]), width = 2.2), hovertemplate = paste0(v, "<br>Multiplier: %{x:.2f}<br>", metric, ": %{y:,.2f}<extra></extra>"))
      cur <- sub[which.min(abs(spend_multiplier__ - 1))]
      if (nrow(cur)) p <- add_trace(p, data = cur, x = ~spend_multiplier__, y = ~y_metric__, type = "scatter", mode = "markers", name = paste(v, "current"), showlegend = FALSE, marker = list(color = unname(cols[[v]]), size = 8, symbol = "circle"), hovertemplate = paste0(v, " current<br>Multiplier: %{x:.2f}<br>", metric, ": %{y:,.2f}<extra></extra>"))
    }
    plotly_theme(p, title = title, x_title = "Spend/support multiplier", y_title = gsub("_", " ", metric))
  }
  output$optimizer_response_curve <- renderPlotly({ draw_curve(input$curve_metric, "Response curves") })
  output$optimizer_mroi_curve <- renderPlotly({ draw_curve(input$curve_roi_metric, "ROI / marginal response curves") })
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
  output$contribution_trend_plot <- renderPlotly({
    dt <- filter_role(filter_vars(table_or_empty("contribution_by_period_variable")))[role != "residual"]
    validate(need(nrow(dt) > 0, "No contribution trend rows available."))
    dt <- dt[, .(contribution = sum(as.numeric(contribution), na.rm = TRUE)), by = .(period_sort, period_label, variable)]
    p <- ggplot(dt, aes(x = reorder(period_label, period_sort), y = contribution, color = variable, group = variable, text = paste(period_label, "<br>", variable, ":", fmt(contribution)))) + geom_line(linewidth = 0.8) + scale_color_manual(values = color_map(unique(dt$variable))) + labs(title = "Contribution trend", x = NULL, y = "Contribution") + chart_theme() + theme(axis.text.x = element_text(angle = theme_state$x_tick_angle, hjust = 1), legend.position = "bottom")
    ggplotly(p, tooltip = "text")
  })
  output$due_to_plot <- renderPlotly({
    dt <- filter_vars(table_or_empty("period_due_to_variable"))[is.finite(contribution_change)]
    validate(need(nrow(dt) > 0, "No due-to rows available."))
    latest <- dt[period_sort == max(period_sort, na.rm = TRUE)][order(-abs(contribution_change))]
    latest <- head(latest, input$top_n)
    p <- ggplot(latest, aes(x = reorder(variable, as.numeric(contribution_change)), y = as.numeric(contribution_change), text = paste(variable, "<br>Change contribution:", fmt(contribution_change)))) + geom_hline(yintercept = 0, color = "#9CA3AF") + geom_col(fill = palette_values()[3], width = 0.72) + coord_flip() + labs(title = "Latest period due-to contribution change", x = NULL, y = "Contribution change") + chart_theme()
    ggplotly(p, tooltip = "text")
  })
  output$contribution_table <- renderDT(dt_widget(selected_contrib(), 20))
  output$trend_table <- renderDT(dt_widget(filter_role(filter_vars(table_or_empty("contribution_by_period_variable"))), 20))
  output$econ_table <- renderDT(dt_widget(selected_econ(), 20))
  output$curve_table <- renderDT(dt_widget(curve_dt(), 20))
  output$optimizer_plan_table <- renderDT(dt_widget(filter_vars(table_or_empty("optimizer_plan")), 20))
  output$optimizer_scenario_table <- renderDT(dt_widget(table_or_empty("optimizer_scenario_comparison"), 20))
  output$flags_table <- renderDT(dt_widget(table_or_empty("diagnostic_flags"), 10))
  output$fit_table <- renderDT(dt_widget(table_or_empty("fit_diagnostics"), 10))
  output$chart_registry_table <- renderDT(dt_widget(table_or_empty("chart_registry"), 20))
}
shinyApp(ui, server)
