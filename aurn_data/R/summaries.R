# ---------------------------------------------------------------------------
# Summary statistics. All functions take the long table for ONE pollutant
# (code, date, value, capture) that has already had the capture filter applied.
# ---------------------------------------------------------------------------

add_months <- function(d, n) seq(d, by = paste(n, "months"), length.out = 2)[2]

# 12-month window ending at `end` (a first-of-month Date).
window_mean <- function(d, end) {
  start <- add_months(end, -11)
  w <- d[d$date >= start & d$date <= end, , drop = FALSE]
  if (!nrow(w)) {
    return(data.frame(code = character(), mean = numeric(), n = integer()))
  }
  out <- stats::aggregate(value ~ code, data = w, FUN = function(x) c(mean = mean(x), n = length(x)))
  data.frame(code = out$code, mean = out$value[, "mean"], n = as.integer(out$value[, "n"]))
}

# One row per site: latest 12-month mean, previous 12 months, change, latest month.
site_summary <- function(d, sites, benchmark) {
  if (!nrow(d)) return(NULL)
  end <- max(d$date)                      # latest month anywhere in the network
  cur <- window_mean(d, end)
  prev <- window_mean(d, add_months(end, -12))
  latest <- d[order(d$code, d$date, decreasing = TRUE), ]
  latest <- latest[!duplicated(latest$code), c("code", "date", "value")]
  names(latest) <- c("code", "latest_month", "latest_value")

  out <- merge(sites[, c("code", "site", "site_type")], latest, by = "code")
  out <- merge(out, setNames(cur, c("code", "mean_12m", "months_12m")), by = "code", all.x = TRUE)
  out <- merge(out, setNames(prev, c("code", "mean_prev", "months_prev")), by = "code", all.x = TRUE)
  out$months_12m[is.na(out$months_12m)] <- 0L
  valid <- out$months_12m >= MIN_MONTHS_FOR_MEAN
  prev_valid <- !is.na(out$months_prev) & out$months_prev >= MIN_MONTHS_FOR_MEAN
  out$mean_12m[!valid] <- NA
  out$mean_prev[!prev_valid] <- NA
  out$change_pct <- 100 * (out$mean_12m - out$mean_prev) / out$mean_prev
  out$status <- ifelse(!valid, "No recent data",
                       ifelse(out$mean_12m > benchmark, "Above benchmark", "Below benchmark"))
  attr(out, "window_end") <- end
  out[order(-out$mean_12m, na.last = TRUE), ]
}

# Calendar-year means per site.
annual_means <- function(d) {
  if (!nrow(d)) return(NULL)
  d$year <- as.integer(format(d$date, "%Y"))
  out <- stats::aggregate(value ~ code + year, data = d,
                          FUN = function(x) c(mean = mean(x), n = length(x)))
  data.frame(code = out$code, year = out$year,
             mean = out$value[, "mean"], n = as.integer(out$value[, "n"]))
}

# Typical value for each calendar month, across all years in the range.
seasonal_profile <- function(d) {
  if (!nrow(d)) return(NULL)
  d$month <- as.integer(format(d$date, "%m"))
  out <- stats::aggregate(value ~ code + month, data = d,
                          FUN = function(x) c(mean = mean(x), sd = stats::sd(x), n = length(x)))
  data.frame(code = out$code, month = out$month, mean = out$value[, "mean"],
             sd = out$value[, "sd"], n = as.integer(out$value[, "n"]))
}

# Rolling 12-month mean per site (needs MIN_MONTHS_FOR_MEAN months in each window).
rolling_12m <- function(d) {
  if (!nrow(d)) return(d)
  do.call(rbind, lapply(split(d, d$code), function(s) {
    s <- s[order(s$date), ]
    s$roll <- vapply(seq_len(nrow(s)), function(i) {
      w <- s$value[s$date > add_months(s$date[i], -12) & s$date <= s$date[i]]
      if (length(w) >= MIN_MONTHS_FOR_MEAN) mean(w) else NA_real_
    }, numeric(1))
    s
  }))
}

# Wide table (one column per site, or per site and pollutant) for download.
# Takes the long download table (site, code, pollutant, date, value_ugm3, ...).
to_wide <- function(long) {
  long$col <- paste0(long$site, " (", long$code, ")")
  if (length(unique(long$pollutant)) > 1) long$col <- paste(long$col, long$pollutant, sep = " - ")
  w <- tidyr::pivot_wider(long[, c("date", "col", "value_ugm3")], names_from = col, values_from = value_ugm3)
  w <- w[order(w$date), ]
  w[, c("date", sort(setdiff(names(w), "date")))]
}
