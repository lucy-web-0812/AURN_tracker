# ---------------------------------------------------------------------------
# Charts (plotly). `colours` is a named vector: site code -> hex colour.
# ---------------------------------------------------------------------------

MONTH_LABELS <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

empty_plot <- function(msg) {
  plotly::plot_ly() |>
    plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(text = msg, showarrow = FALSE, font = list(size = 15, color = "#52514e"))),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
    ) |>
    plotly::config(displayModeBar = FALSE)
}

benchmark_line <- function(bm) {
  list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = bm$value, y1 = bm$value,
       line = list(color = "#52514e", dash = "dash", width = 1.5))
}

benchmark_note <- function(bm) {
  list(xref = "paper", x = 1, y = bm$value, xanchor = "right", yanchor = "bottom",
       text = paste0(bm$name, " (", bm$value, ")"), showarrow = FALSE,
       font = list(size = 11, color = "#52514e"))
}

base_layout <- function(p, ytitle, bm, xaxis = list()) {
  plotly::layout(
    p,
    yaxis = list(title = ytitle, rangemode = "tozero", gridcolor = "#e7e6e2", zeroline = FALSE),
    xaxis = modifyList(list(title = "", showgrid = FALSE), xaxis),
    shapes = if (!is.na(bm$value)) list(benchmark_line(bm)) else list(),
    annotations = if (!is.na(bm$value)) list(benchmark_note(bm)) else list(),
    hovermode = "x unified",
    legend = list(orientation = "h", y = -0.15),
    margin = list(t = 20, r = 10),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
  ) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
                   toImageButtonOptions = list(format = "png", filename = "aurn_chart", scale = 2))
}

y_title <- function(pollutant) paste0(pollutant_label(pollutant), " (µg/m³)")

plot_timeseries <- function(d, sites, colours, pollutant, bm, rolling = FALSE) {
  if (!nrow(d)) return(empty_plot("No data for these sites and dates"))
  if (rolling) d <- rolling_12m(d)
  p <- plotly::plot_ly()
  for (code in names(colours)) {
    s <- d[d$code == code, ]
    if (!nrow(s)) next
    # fill gaps with NA so lines break instead of joining across missing months
    full <- data.frame(date = seq(min(s$date), max(s$date), by = "month"))
    s <- merge(full, s, by = "date", all.x = TRUE)
    y <- if (rolling) s$roll else s$value
    nm <- sites$site[sites$code == code]
    p <- plotly::add_trace(
      p, x = s$date, y = y, type = "scatter", mode = "lines", name = nm,
      line = list(color = colours[[code]], width = 2),
      hovertemplate = paste0(nm, ": %{y:.1f}<extra></extra>")
    )
  }
  ylab <- if (rolling) paste("12-month mean", y_title(pollutant)) else paste("Monthly mean", y_title(pollutant))
  base_layout(p, ylab, bm, xaxis = list(
    rangeselector = list(buttons = list(
      list(count = 1, label = "1y", step = "year", stepmode = "backward"),
      list(count = 3, label = "3y", step = "year", stepmode = "backward"),
      list(count = 5, label = "5y", step = "year", stepmode = "backward"),
      list(step = "all", label = "All")
    ))
  ))
}

plot_annual <- function(d, sites, colours, pollutant, bm) {
  a <- annual_means(d)
  if (is.null(a) || !nrow(a)) return(empty_plot("No data for these sites and dates"))
  p <- plotly::plot_ly()
  for (code in names(colours)) {
    s <- a[a$code == code, ]
    if (!nrow(s)) next
    nm <- sites$site[sites$code == code]
    txt <- ifelse(s$n < 12, paste0(" (", s$n, " months)"), "")
    p <- plotly::add_trace(
      p, x = s$year, y = s$mean, type = "bar", name = nm,
      marker = list(color = colours[[code]],
                    opacity = ifelse(s$n >= MIN_MONTHS_FOR_MEAN, 1, 0.4)),
      text = txt, textposition = "none",
      hovertemplate = paste0(nm, ": %{y:.1f}%{text}<extra></extra>")
    )
  }
  base_layout(p, paste("Annual mean", y_title(pollutant)), bm,
              xaxis = list(dtick = 1, tickformat = "d")) |>
    plotly::layout(barmode = "group", bargap = 0.25)
}

plot_seasonal <- function(d, sites, colours, pollutant, bm) {
  s_all <- seasonal_profile(d)
  if (is.null(s_all) || !nrow(s_all)) return(empty_plot("No data for these sites and dates"))
  p <- plotly::plot_ly()
  for (code in names(colours)) {
    s <- s_all[s_all$code == code, ]
    if (!nrow(s)) next
    s <- s[order(s$month), ]
    nm <- sites$site[sites$code == code]
    col <- colours[[code]]
    sd <- ifelse(is.na(s$sd), 0, s$sd)
    # ±1 standard deviation band
    p <- plotly::add_ribbons(
      p, x = s$month, ymin = pmax(s$mean - sd, 0), ymax = s$mean + sd,
      fillcolor = grDevices::adjustcolor(col, alpha.f = 0.15), line = list(width = 0),
      name = nm, legendgroup = code, showlegend = FALSE, hoverinfo = "skip"
    )
    p <- plotly::add_trace(
      p, x = s$month, y = s$mean, type = "scatter", mode = "lines+markers", name = nm,
      legendgroup = code, line = list(color = col, width = 2),
      marker = list(color = col, size = 8, line = list(color = "#ffffff", width = 2)),
      customdata = round(sd, 1),
      hovertemplate = paste0(nm, ": %{y:.1f} ± %{customdata}<extra></extra>")
    )
  }
  base_layout(p, paste("Typical monthly", y_title(pollutant)), bm,
              xaxis = list(tickvals = 1:12, ticktext = MONTH_LABELS))
}
