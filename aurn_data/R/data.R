# ---------------------------------------------------------------------------
# Loading, caching and refreshing AURN monthly data.
#
# The app keeps one file, data/aurn_monthly.rds, holding a list:
#   data        long table: code, date, pollutant, value, capture
#   sites       one row per site: code, site, site_type, latitude, longitude
#   checked_at  when Defra was last checked
#   updated_at  when new data last arrived
#   pollutants  pollutants covered by the full download
#
# A refresh only re-downloads the current and previous year (these are the
# files Defra keeps updating), then swaps those years into the cache. If the
# cache is missing a pollutant listed in config.R, it downloads all years once.
# ---------------------------------------------------------------------------

read_store <- function(path = CACHE_PATH) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

write_store <- function(store, path = CACHE_PATH) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(store, path)
  invisible(store)
}

needs_refresh <- function(store, now = Sys.time(), hours = REFRESH_EVERY_HOURS) {
  if (is.null(store) || is.null(store$checked_at) || is.na(store$checked_at)) return(TRUE)
  if (!all(names(POLLUTANTS) %in% store$pollutants)) return(TRUE)
  difftime(now, store$checked_at, units = "hours") > hours
}

# Turn openair's wide monthly table into the long format the app uses.
tidy_monthly <- function(raw, pollutants = names(POLLUTANTS)) {
  raw <- as.data.frame(raw)
  present <- intersect(pollutants, names(raw))
  out <- lapply(present, function(p) {
    cap <- paste0(p, "_capture")
    data.frame(
      code      = as.character(raw$code),
      date      = as.Date(raw$date),
      pollutant = p,
      value     = suppressWarnings(as.numeric(raw[[p]])),
      capture   = if (cap %in% names(raw)) suppressWarnings(as.numeric(raw[[cap]])) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(out)
  if (!nrow(out)) return(out)
  out <- out[!is.na(out$value) & !is.na(out$code), , drop = FALSE]
  # openair gives capture as a fraction; guard against percentages.
  if (any(out$capture > 1.5, na.rm = TRUE)) out$capture <- out$capture / 100
  out[order(out$code, out$pollutant, out$date), , drop = FALSE]
}

# One row per site, using site metadata where available.
build_sites <- function(raw, meta = NULL, old_sites = NULL) {
  raw <- as.data.frame(raw)
  sites <- unique(data.frame(code = raw$code, site = raw$site, stringsAsFactors = FALSE))
  sites <- sites[!duplicated(sites$code), , drop = FALSE]
  cols <- c("code", "site", "site_type", "latitude", "longitude")
  if (!is.null(meta) && nrow(meta)) {
    meta <- as.data.frame(meta)
    meta <- meta[!duplicated(meta$code), intersect(cols, names(meta)), drop = FALSE]
    sites <- dplyr::left_join(sites, meta[, setdiff(names(meta), "site"), drop = FALSE], by = "code")
  }
  for (cl in cols) if (!cl %in% names(sites)) sites[[cl]] <- NA
  sites <- sites[, cols]
  if (!is.null(old_sites)) {
    # keep sites we knew about before but that weren't in this download
    sites <- dplyr::bind_rows(sites, old_sites[!old_sites$code %in% sites$code, cols, drop = FALSE])
    # fill missing coordinates/types from what we had
    m <- match(sites$code, old_sites$code)
    for (cl in c("site_type", "latitude", "longitude")) {
      gap <- is.na(sites[[cl]]) & !is.na(m)
      sites[[cl]][gap] <- old_sites[[cl]][m[gap]]
    }
  }
  sites$site_type[is.na(sites$site_type)] <- "Unknown"
  sites[order(sites$site), , drop = FALSE]
}

# Download one year of AURN monthly means from Defra (via openair).
fetch_year_defra <- function(year) {
  if (!requireNamespace("openair", quietly = TRUE)) {
    stop("The openair package is needed to download data: install.packages('openair')")
  }
  openair::importUKAQ(year = year, source = "aurn", data_type = "monthly")
}

fetch_meta_defra <- function() {
  openair::importMeta(source = "aurn")
}

# Check Defra and merge anything new into the store.
# fetch_year / fetch_meta can be swapped out for testing.
refresh_store <- function(store = read_store(), full = FALSE, now = Sys.time(),
                          fetch_year = fetch_year_defra, fetch_meta = fetch_meta_defra,
                          path = CACHE_PATH) {
  this_year <- as.integer(format(now, "%Y"))
  need_full <- full || is.null(store) || is.null(store$data) ||
    !all(names(POLLUTANTS) %in% store$pollutants)
  years <- if (need_full) FIRST_YEAR:this_year else (this_year - 1):this_year

  got <- lapply(years, function(y) tryCatch(fetch_year(y), error = function(e) NULL))
  ok <- !vapply(got, function(x) is.null(x) || !NROW(x), logical(1))
  # the current year may not exist yet in early January; that's fine
  if (!any(ok)) stop("Couldn't download any data from Defra. Showing the saved copy instead.")
  if (need_full && sum(ok) < length(years) - 1) {
    stop("The full download was incomplete. Showing the saved copy instead.")
  }

  raw <- dplyr::bind_rows(lapply(got[ok], as.data.frame))
  meta <- tryCatch(fetch_meta(), error = function(e) NULL)
  fresh <- tidy_monthly(raw)
  years_ok <- years[ok]

  if (need_full || is.null(store$data)) {
    data <- fresh
  } else {
    keep <- !(as.integer(format(store$data$date, "%Y")) %in% years_ok)
    data <- dplyr::bind_rows(store$data[keep, , drop = FALSE], fresh)
    data <- data[order(data$code, data$pollutant, data$date), , drop = FALSE]
  }

  old_n <- if (is.null(store$data)) 0 else nrow(store$data)
  old_latest <- if (is.null(store$data)) as.Date(NA) else max(store$data$date)
  changed <- is.null(store$data) || nrow(data) != old_n || max(data$date) != old_latest ||
    !isTRUE(all.equal(sum(data$value), sum(store$data$value)))

  new_store <- list(
    data       = data,
    sites      = build_sites(raw, meta, old_sites = store$sites),
    checked_at = now,
    updated_at = if (changed) now else store$updated_at,
    pollutants = if (need_full) names(POLLUTANTS) else store$pollutants
  )
  write_store(new_store, path)
  attr(new_store, "changed") <- changed
  new_store
}

# Build a starting cache from the CSV made by preprocessing/data_loader.R.
# Its checked_at is left empty so the app tops it up on first run.
store_from_csv <- function(csv) {
  raw <- readr::read_csv(csv, show_col_types = FALSE, guess_max = 20000)
  list(
    data       = tidy_monthly(raw),
    sites      = build_sites(raw, meta = raw),
    checked_at = as.POSIXct(NA),
    updated_at = as.POSIXct(NA),
    pollutants = intersect(names(POLLUTANTS), names(raw))
  )
}
