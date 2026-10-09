# ---------------------------------------------------------------------------
# Settings you might want to change. Everything else reads from here.
# ---------------------------------------------------------------------------

# Pollutants the app downloads and shows. Remove a line to drop one.
POLLUTANTS <- list(
  pm2.5 = list(label = "PM2.5", html = "PM<sub>2.5</sub>"),
  pm10  = list(label = "PM10",  html = "PM<sub>10</sub>"),
  no2   = list(label = "NO2",   html = "NO<sub>2</sub>")
)

# Annual-mean benchmarks (µg/m³). NA = no benchmark of that kind for that
# pollutant, in which case the app falls back to the UK limit.
BENCHMARKS <- data.frame(
  pollutant = c("pm2.5", "pm10", "no2"),
  uk_limit  = c(20, 40, 40),   # Air Quality Standards Regulations 2010
  uk_target = c(10, NA, NA),   # England PM2.5 target for 2040 (Environment Act 2021)
  who       = c(5, 15, 10)     # WHO 2021 air quality guidelines
)

BENCHMARK_CHOICES <- c(
  "UK legal limit"        = "uk_limit",
  "England 2040 target"   = "uk_target",
  "WHO 2021 guideline"    = "who"
)

FIRST_YEAR            <- 2015   # earliest year to download
REFRESH_EVERY_HOURS   <- 12     # how old the cache can get before the app checks Defra again
MIN_CAPTURE           <- 0.75   # a month needs this much data capture to count (when filter is on)
MIN_MONTHS_FOR_MEAN   <- 9      # months needed in a 12-month window for a "valid" mean
MAX_SITES             <- 6      # sites that can be plotted together
DEFAULT_SITES         <- c("MY1", "KC1")  # London Marylebone Road, London N. Kensington

CACHE_PATH <- file.path("data", "aurn_monthly.rds")

# Line colours, assigned to sites in this fixed order (colour-blind checked).
SERIES_COLOURS <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300")

# Map status colours.
STATUS_COLOURS <- c(
  "Below benchmark" = "#0ca30c",
  "Above benchmark" = "#d03b3b",
  "No recent data"  = "#9a9993"
)

pollutant_label <- function(p) POLLUTANTS[[p]]$label

benchmark_value <- function(pollutant, which) {
  row <- BENCHMARKS[BENCHMARKS$pollutant == pollutant, , drop = FALSE]
  if (!nrow(row)) return(list(value = NA_real_, name = "None"))
  v <- row[[which]]
  name <- names(BENCHMARK_CHOICES)[BENCHMARK_CHOICES == which]
  if (is.na(v)) {
    v <- row$uk_limit
    name <- "UK legal limit"
  }
  list(value = v, name = name)
}
