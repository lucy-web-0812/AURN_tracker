# AURN tracker: monthly air quality from Defra's Automatic Urban and Rural Network.
# Files in R/ (config, data, summaries, plots) are loaded automatically by Shiny.

library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)

# ---- Shared data: one copy for everyone using this app instance -------------
initial_store <- read_store()
if (is.null(initial_store)) {
  initial_store <- tryCatch(refresh_store(NULL), error = function(e) NULL)
}
if (is.null(initial_store)) {
  stop("No saved data (data/aurn_monthly.rds) and Defra couldn't be reached. ",
       "Run preprocessing/update_data.R once to build the data file.")
}
STORE <- reactiveVal(initial_store)
LAST_FAILED_CHECK <- new.env()   # stops every new visitor retrying a failing download

# ---- Small helpers -----------------------------------------------------------
fmt_month <- function(d) format(d, "%b %Y")
first_of_month <- function(d) as.Date(format(as.Date(d), "%Y-%m-01"))

time_ago <- function(t) {
  if (is.null(t) || is.na(t)) return("never")
  mins <- as.numeric(difftime(Sys.time(), t, units = "mins"))
  if (mins < 2) return("just now")
  if (mins < 120) return(paste(round(mins), "minutes ago"))
  if (mins < 48 * 60) return(paste(round(mins / 60), "hours ago"))
  paste(round(mins / 1440), "days ago")
}

pollutant_choices <- function(store) {
  avail <- intersect(names(POLLUTANTS), unique(store$data$pollutant))
  list(names = lapply(avail, function(p) HTML(POLLUTANTS[[p]]$html)), values = avail)
}

site_choices <- function(sites) setNames(sites$code, paste0(sites$site, " (", sites$code, ")"))

# ---- UI ----------------------------------------------------------------------
# A function, so each new visitor gets the latest data's date range and sites.
ui <- function(req) {
  st <- isolate(STORE())
  pc <- pollutant_choices(st)
  dmin <- min(st$data$date)
  dmax <- max(st$data$date)
  types <- sort(unique(st$sites$site_type))
  default_sites <- intersect(DEFAULT_SITES, st$sites$code)
  if (!length(default_sites)) default_sites <- st$sites$code[1]

  page_navbar(
    title = "AURN tracker",
    window_title = "AURN tracker",
    bg = "#1f6f43", inverse = TRUE,
    theme = bs_theme(version = 5, primary = "#1f6f43",
                     base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif")),
    fillable = c("Explore", "Site rankings", "Data & download"),
    header = tags$style(HTML("
      #rankings, #dl_preview { height: auto !important; }
      table.dataTable.compact tbody td { padding: 4px 8px; }
      .dt-nowrap { white-space: nowrap; }
    ")),
    sidebar = sidebar(
      width = 290,
      radioButtons("pollutant", "Pollutant", choiceNames = pc$names, choiceValues = pc$values,
                   selected = pc$values[1], inline = TRUE),
      selectizeInput("sites", "Sites", choices = site_choices(st$sites), selected = default_sites,
                     multiple = TRUE,
                     options = list(maxItems = MAX_SITES, plugins = list("remove_button"),
                                    placeholder = "Type a site name, or click the map")),
      sliderInput("dates", "Dates shown on charts", min = dmin, max = dmax,
                  value = c(max(dmin, as.Date(paste0(as.integer(format(dmax, "%Y")) - 5, "-01-01"))), dmax),
                  timeFormat = "%b %Y", step = 31),
      selectInput("benchmark", "Compare against", BENCHMARK_CHOICES),
      accordion(
        open = FALSE,
        accordion_panel(
          "More options", icon = bs_icon("sliders"),
          checkboxGroupInput("site_types", "Site types on map and rankings",
                             choices = types, selected = types),
          checkboxInput("capture_filter",
                        paste0("Only use months with at least ", MIN_CAPTURE * 100, "% data capture"),
                        value = TRUE)
        )
      ),
      tags$hr(class = "my-1"),
      uiOutput("data_status"),
      actionButton("refresh", "Check for new data", icon = icon("rotate"),
                   class = "btn-sm btn-outline-secondary")
    ),

    nav_panel(
      "Explore", icon = bs_icon("graph-up"),
      layout_columns(
        fill = FALSE, col_widths = c(4, 4, 4),
        uiOutput("vb_mean"), uiOutput("vb_change"), uiOutput("vb_latest")
      ),
      layout_columns(
        col_widths = c(5, 7), min_height = 460,
        card(
          full_screen = TRUE,
          card_header("Monitoring sites",
                      tooltip(bs_icon("info-circle"),
                              "Colour shows each site's latest 12-month mean against the chosen benchmark. Click a site to add or remove it from the charts.")),
          card_body(padding = 0, leafletOutput("map", height = "100%"))
        ),
        navset_card_tab(
          full_screen = TRUE,
          nav_panel(
            "Over time",
            radioButtons("ts_mode", NULL, inline = TRUE,
                         choices = c("Monthly means" = "monthly", "Rolling 12-month mean" = "rolling")),
            plotlyOutput("ts", height = "100%")
          ),
          nav_panel("Annual means", plotlyOutput("annual", height = "100%")),
          nav_panel("Seasonal pattern", plotlyOutput("seasonal", height = "100%"))
        )
      )
    ),

    nav_panel(
      "Site rankings", icon = bs_icon("list-ol"),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          uiOutput("rank_title", inline = TRUE),
          downloadButton("dl_rankings", "Download table", class = "btn-sm btn-outline-primary")
        ),
        p(class = "text-muted small mb-1",
          "Click a row to add that site to the charts on the Explore tab. ",
          paste0("Sites need ", MIN_MONTHS_FOR_MEAN, " of the last 12 months to get a mean.")),
        DTOutput("rankings")
      )
    ),

    nav_panel(
      "Data & download", icon = bs_icon("download"),
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Download"),
          radioButtons("dl_scope", "What to include",
                       choices = c("Selected sites, this pollutant" = "sel_one",
                                   "Selected sites, all pollutants" = "sel_all",
                                   "All sites, this pollutant" = "all_one",
                                   "All sites, all pollutants" = "all_all")),
          radioButtons("dl_format", "Layout",
                       choices = c("Long: one row per site and month" = "long",
                                   "Wide: one column per site" = "wide")),
          checkboxInput("dl_use_dates", "Only the dates chosen in the sidebar", value = TRUE),
          p(class = "small text-muted",
            "Values are monthly means in µg/m³. The data-capture filter in More options applies here too."),
          downloadButton("dl_data", "Download CSV", class = "btn-primary")
        ),
        card(
          card_header(textOutput("dl_preview_title", inline = TRUE)),
          DTOutput("dl_preview")
        )
      )
    ),

    nav_panel(
      "About", icon = bs_icon("info-circle"),
      card(
        card_body(
          h4("About this app"),
          p("Monthly mean concentrations from Defra's Automatic Urban and Rural Network (AURN), ",
            "downloaded with the ", tags$a("openair", href = "https://openair-project.github.io/book/", target = "_blank"),
            " R package from ", tags$a("UK-AIR", href = "https://uk-air.defra.gov.uk/", target = "_blank"), "."),
          h5("How the data stays up to date"),
          p("The app keeps a saved copy of the data. When someone opens it and that copy is more than ",
            REFRESH_EVERY_HOURS, " hours old, it re-downloads the current and previous year from Defra ",
            "(the only years that change) and merges them in. You can also press ", tags$b("Check for new data"), "."),
          p("Recent months are provisional and may be revised when Defra ratifies the data."),
          h5("Benchmarks (annual mean, µg/m³)"),
          tableOutput("benchmark_table"),
          h5("How the numbers are worked out"),
          tags$ul(
            tags$li("12-month mean: the average of the last 12 monthly means, ending at the latest month available anywhere in the network. Sites need at least ",
                    MIN_MONTHS_FOR_MEAN, " months to get a value."),
            tags$li("This is a quick indicator, not a formal compliance assessment, which uses ratified hourly data."),
            tags$li("Seasonal pattern: the average for each calendar month across the chosen dates; the shaded band is ±1 standard deviation.")
          )
        )
      )
    ),
    nav_spacer(),
    nav_item(tags$span(class = "navbar-text small text-white-50", "Data: Defra UK-AIR"))
  )
}

# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Refreshing ----
  do_refresh <- function(manual) {
    id <- showNotification("Checking Defra for new data…", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    res <- tryCatch(refresh_store(isolate(STORE())), error = function(e) e)
    if (inherits(res, "error")) {
      LAST_FAILED_CHECK$time <- Sys.time()
      showNotification(conditionMessage(res), type = "warning", duration = 8)
    } else {
      STORE(res)
      if (manual || isTRUE(attr(res, "changed"))) {
        showNotification(if (isTRUE(attr(res, "changed"))) "New data loaded." else "Already up to date.",
                         type = "message", duration = 4)
      }
    }
  }

  # Check automatically shortly after the page has loaded (so the page appears first).
  startup_step <- reactiveVal(0)
  observe({
    step <- isolate(startup_step())
    if (step >= 2) return()
    startup_step(step + 1)
    if (step == 0) return(invalidateLater(800))
    failed_recently <- !is.null(LAST_FAILED_CHECK$time) &&
      difftime(Sys.time(), LAST_FAILED_CHECK$time, units = "mins") < 60
    if (needs_refresh(isolate(STORE())) && !failed_recently) do_refresh(manual = FALSE)
  })

  observeEvent(input$refresh, do_refresh(manual = TRUE))

  # When new data arrives, extend the date slider and pollutant list.
  slider_max <- reactiveVal(max(isolate(STORE())$data$date))
  observeEvent(STORE(), ignoreInit = TRUE, {
    st <- STORE()
    dmax <- max(st$data$date)
    # if the slider was showing the latest month, keep it on the (new) latest month
    end <- if (isTRUE(as.Date(input$dates[2]) >= slider_max())) dmax else input$dates[2]
    slider_max(dmax)
    updateSliderInput(session, "dates", min = min(st$data$date), max = dmax, value = c(input$dates[1], end))
    pc <- pollutant_choices(st)
    sel <- if (input$pollutant %in% pc$values) input$pollutant else pc$values[1]
    updateRadioButtons(session, "pollutant", choiceNames = pc$names, choiceValues = pc$values,
                       selected = sel, inline = TRUE)
  })

  output$data_status <- renderUI({
    invalidateLater(60 * 1000)
    st <- STORE()
    div(class = "small text-muted mb-1",
        div(bs_icon("calendar3"), " Data up to ", tags$b(fmt_month(max(st$data$date)))),
        div(bs_icon("clock-history"), " Checked with Defra ", time_ago(st$checked_at)))
  })

  # ---- Core reactives ----
  sites_tbl <- reactive(STORE()$sites)

  pol_data <- reactive({
    req(input$pollutant)
    d <- STORE()$data
    d <- d[d$pollutant == input$pollutant, , drop = FALSE]
    if (isTRUE(input$capture_filter)) d <- d[is.na(d$capture) | d$capture >= MIN_CAPTURE, , drop = FALSE]
    d
  })

  bm <- reactive(benchmark_value(input$pollutant, input$benchmark))

  summary_tbl <- reactive(site_summary(pol_data(), sites_tbl(), bm()$value))

  window_text <- reactive({
    end <- attr(summary_tbl(), "window_end")
    paste(fmt_month(add_months(end, -11)), "to", fmt_month(end))
  })

  date_range <- reactive(c(first_of_month(input$dates[1]), as.Date(input$dates[2])))

  chart_data <- reactive({
    d <- pol_data()
    r <- date_range()
    d[d$code %in% input$sites & d$date >= r[1] & d$date <= r[2], , drop = FALSE]
  })

  # Fixed colour per site for as long as it stays selected.
  colour_map <- reactiveVal(character())
  observeEvent(input$sites, ignoreNULL = FALSE, {
    sel <- input$sites
    cur <- colour_map()
    cur <- cur[names(cur) %in% sel]
    for (s in setdiff(sel, names(cur))) cur[s] <- setdiff(SERIES_COLOURS, cur)[1]
    colour_map(cur[sel])
  })

  # Keep the site list in step with the pollutant and site-type filters.
  observe({
    s <- sites_tbl()
    measured <- unique(pol_data()$code)
    sel <- isolate(input$sites)
    keep <- s[(s$code %in% measured & s$site_type %in% input$site_types) | s$code %in% sel, ]
    updateSelectizeInput(session, "sites", choices = site_choices(keep), selected = sel)
  })

  # ---- Map ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4)) |>
      addProviderTiles(providers$OpenStreetMap) |>
      fitBounds(-7.5, 50, 1.8, 58.6)
  })

  map_ready <- reactiveVal(FALSE)
  observeEvent(input$map_zoom, map_ready(TRUE), once = TRUE)

  observe({
    req(map_ready())
    sm <- summary_tbl()
    req(sm)
    s <- merge(sm, sites_tbl()[, c("code", "latitude", "longitude")], by = "code")
    s <- s[s$site_type %in% input$site_types & !is.na(s$latitude), ]
    s$selected <- s$code %in% input$sites
    s <- s[order(s$selected), ]   # selected sites drawn on top
    val <- ifelse(is.na(s$mean_12m), "not enough recent data", sprintf("%.1f µg/m³", s$mean_12m))
    labels <- lapply(sprintf("<b>%s</b> (%s)<br>%s<br>12-month mean: %s", s$site, s$code, s$site_type, val), HTML)
    b <- bm()
    leafletProxy("map") |>
      clearGroup("sites") |>
      clearControls() |>
      addCircleMarkers(
        data = s, lng = ~longitude, lat = ~latitude, layerId = ~code, group = "sites",
        radius = ifelse(s$selected, 9, 6), weight = ifelse(s$selected, 3, 1),
        color = ifelse(s$selected, "#0b0b0b", "#ffffff"), opacity = 1,
        fillColor = unname(STATUS_COLOURS[s$status]), fillOpacity = 0.9,
        label = labels
      ) |>
      addLegend("bottomright", colors = unname(STATUS_COLOURS), labels = names(STATUS_COLOURS),
                title = HTML(paste0(POLLUTANTS[[input$pollutant]]$html, " vs ", b$name, " (", b$value, ")")),
                opacity = 0.9)
  })

  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    sel <- input$sites
    sel <- if (id %in% sel) setdiff(sel, id) else c(sel, id)
    if (length(sel) > MAX_SITES) {
      showNotification(paste("Up to", MAX_SITES, "sites at once - removed the oldest."), duration = 4)
      sel <- utils::tail(sel, MAX_SITES)
    }
    updateSelectizeInput(session, "sites", selected = sel)
  })

  # ---- Value boxes (first selected site) ----
  focus <- reactive({
    validate(need(length(input$sites) > 0, "Pick a site in the sidebar or on the map."))
    code <- input$sites[1]
    sm <- summary_tbl()
    list(code = code, name = sites_tbl()$site[sites_tbl()$code == code],
         row = if (is.null(sm)) NULL else sm[sm$code == code, , drop = FALSE])
  })

  no_data_box <- function(title, f) {
    value_box(title = title, value = "No data", showcase = bs_icon("dash-circle"), theme = "light",
              p(paste0(f$name, " has no ", pollutant_label(input$pollutant), " data for this period.")))
  }

  output$vb_mean <- renderUI({
    f <- focus(); r <- f$row; b <- bm()
    title <- paste("12-month mean ·", f$name)
    if (is.null(r) || !nrow(r) || is.na(r$mean_12m)) return(no_data_box(title, f))
    above <- r$mean_12m > b$value
    value_box(
      title = title, value = sprintf("%.1f µg/m³", r$mean_12m),
      showcase = bs_icon(if (above) "exclamation-triangle" else "check-circle"),
      theme = if (above) "danger" else "success",
      p(sprintf("%s the %s of %s", if (above) "Above" else "Below", b$name, b$value)),
      p(class = "small", window_text())
    )
  })

  output$vb_change <- renderUI({
    f <- focus(); r <- f$row
    title <- "Change on previous 12 months"
    if (is.null(r) || !nrow(r) || is.na(r$change_pct)) {
      return(value_box(title = title, value = "–", showcase = bs_icon("arrow-left-right"), theme = "light",
                       p("Needs two full years of recent data.")))
    }
    up <- r$change_pct > 0
    value_box(
      title = title, value = sprintf("%+.0f%%", r$change_pct),
      showcase = bs_icon(if (up) "arrow-up-right" else "arrow-down-right"),
      theme = "light",
      p(sprintf("From %.1f to %.1f µg/m³", r$mean_prev, r$mean_12m)),
      p(class = "small", if (up) "Higher than the year before" else "Lower than the year before")
    )
  })

  output$vb_latest <- renderUI({
    f <- focus(); r <- f$row
    title <- "Latest month"
    if (is.null(r) || !nrow(r)) return(no_data_box(title, f))
    cap <- pol_data()$capture[pol_data()$code == f$code & pol_data()$date == r$latest_month]
    value_box(
      title = title, value = sprintf("%.1f µg/m³", r$latest_value),
      showcase = bs_icon("calendar-event"), theme = "light",
      p(fmt_month(r$latest_month)),
      p(class = "small", if (length(cap) && !is.na(cap[1])) sprintf("%.0f%% data capture", 100 * cap[1]) else "")
    )
  })

  # ---- Charts ----
  chart_colours <- reactive({
    validate(need(length(input$sites) > 0, "Pick one or more sites in the sidebar or on the map."))
    colour_map()[input$sites]
  })

  output$ts <- renderPlotly(plot_timeseries(chart_data(), sites_tbl(), chart_colours(), input$pollutant, bm(),
                                            rolling = identical(input$ts_mode, "rolling")))
  output$annual <- renderPlotly(plot_annual(chart_data(), sites_tbl(), chart_colours(), input$pollutant, bm()))
  output$seasonal <- renderPlotly(plot_seasonal(chart_data(), sites_tbl(), chart_colours(), input$pollutant, bm()))

  # ---- Rankings ----
  rank_tbl <- reactive({
    sm <- summary_tbl()
    req(sm)
    sm[sm$site_type %in% input$site_types, , drop = FALSE]
  })

  output$rank_title <- renderUI({
    sm <- rank_tbl(); b <- bm()
    n_valid <- sum(!is.na(sm$mean_12m))
    n_above <- sum(sm$status == "Above benchmark")
    tags$span(HTML(sprintf("<b>%d of %d</b> sites are above the %s (%s µg/m³) for %s · %s",
                           n_above, n_valid, b$name, b$value, POLLUTANTS[[input$pollutant]]$html, window_text())))
  })

  rank_display <- reactive({
    sm <- rank_tbl()
    data.frame(
      Site = sm$site, Code = sm$code, Type = sm$site_type,
      `12-month mean` = round(sm$mean_12m, 1),
      `Previous 12 months` = round(sm$mean_prev, 1),
      `Change (%)` = round(sm$change_pct, 0),
      `Months of data` = sm$months_12m,
      `Latest data` = fmt_month(sm$latest_month),
      Status = ifelse(sm$status == "Above benchmark", "▲ Above",
                      ifelse(sm$status == "Below benchmark", "✓ Below", "– No recent data")),
      check.names = FALSE
    )
  })

  output$rankings <- renderDT({
    datatable(rank_display(), rownames = FALSE, selection = "single", fillContainer = FALSE,
              class = "compact hover stripe",
              options = list(pageLength = 25, order = list(list(3, "desc")), scrollX = TRUE,
                             columnDefs = list(list(className = "dt-nowrap", targets = 8)))) |>
      formatStyle("Status", color = styleEqual(c("▲ Above", "✓ Below", "– No recent data"),
                                               c("#b42323", "#1f6f43", "#6b6a66")), fontWeight = "600")
  })

  observeEvent(input$rankings_rows_selected, {
    code <- rank_display()$Code[input$rankings_rows_selected]
    sel <- unique(c(input$sites, code))
    updateSelectizeInput(session, "sites", selected = utils::tail(sel, MAX_SITES))
    showNotification(paste("Added", rank_display()$Site[input$rankings_rows_selected], "to the charts."), duration = 3)
  })

  output$dl_rankings <- downloadHandler(
    filename = function() paste0("aurn_rankings_", input$pollutant, "_", format(attr(summary_tbl(), "window_end"), "%Y-%m"), ".csv"),
    content = function(file) utils::write.csv(rank_display(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  # ---- Downloads ----
  dl_long <- reactive({
    d <- STORE()$data
    if (input$dl_scope %in% c("sel_one", "all_one")) d <- d[d$pollutant == input$pollutant, , drop = FALSE]
    if (input$dl_scope %in% c("sel_one", "sel_all")) d <- d[d$code %in% input$sites, , drop = FALSE]
    if (isTRUE(input$capture_filter)) d <- d[is.na(d$capture) | d$capture >= MIN_CAPTURE, , drop = FALSE]
    if (isTRUE(input$dl_use_dates)) {
      r <- date_range()
      d <- d[d$date >= r[1] & d$date <= r[2], , drop = FALSE]
    }
    s <- sites_tbl()
    m <- match(d$code, s$code)
    data.frame(site = s$site[m], code = d$code, site_type = s$site_type[m],
               latitude = s$latitude[m], longitude = s$longitude[m],
               pollutant = d$pollutant, date = d$date,
               value_ugm3 = round(d$value, 3), data_capture = round(d$capture, 3))
  })

  dl_table <- reactive({
    d <- dl_long()
    if (input$dl_format == "wide" && nrow(d)) to_wide(d) else d
  })

  output$dl_preview_title <- renderText({
    d <- dl_table()
    sprintf("Preview: %s rows, %s columns", format(nrow(d), big.mark = ","), ncol(d))
  })

  output$dl_preview <- renderDT({
    datatable(dl_table(), rownames = FALSE, selection = "none", fillContainer = FALSE,
              class = "compact stripe nowrap",
              options = list(pageLength = 15, scrollX = TRUE, searching = FALSE))
  })

  output$dl_data <- downloadHandler(
    filename = function() {
      pol <- if (input$dl_scope %in% c("sel_one", "all_one")) input$pollutant else "all-pollutants"
      span <- if (isTRUE(input$dl_use_dates)) paste(format(date_range(), "%Y-%m"), collapse = "_to_") else "all-dates"
      paste0("aurn_monthly_", pol, "_", span, ".csv")
    },
    content = function(file) utils::write.csv(dl_table(), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  )

  output$benchmark_table <- renderTable({
    data.frame(
      Pollutant = vapply(BENCHMARKS$pollutant, pollutant_label, character(1)),
      `UK legal limit` = BENCHMARKS$uk_limit,
      `England 2040 target` = ifelse(is.na(BENCHMARKS$uk_target), "–", BENCHMARKS$uk_target),
      `WHO 2021 guideline` = BENCHMARKS$who,
      check.names = FALSE
    )
  }, striped = TRUE)
}

shinyApp(ui, server)
