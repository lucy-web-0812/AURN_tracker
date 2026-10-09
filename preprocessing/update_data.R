# Rebuild or top up the app's data file (aurn_data/data/aurn_monthly.rds).
#
# The app refreshes itself, so you only need this to:
#   - build the full file before deploying (recommended after adding a pollutant), or
#   - update the data on a schedule outside the app.
#
# Run from the project folder (AURN_tracker), e.g. in RStudio: source("preprocessing/update_data.R")

library(openair)

app_dir <- "aurn_data"
for (f in list.files(file.path(app_dir, "R"), full.names = TRUE)) source(f)

cache <- file.path(app_dir, CACHE_PATH)
full_rebuild <- TRUE   # FALSE = only re-download the current and previous year

store <- refresh_store(read_store(cache), full = full_rebuild, path = cache)

message("Saved ", cache, ": ", format(nrow(store$data), big.mark = ","), " site-months, ",
        length(unique(store$data$code)), " sites, ",
        format(min(store$data$date), "%b %Y"), " to ", format(max(store$data$date), "%b %Y"), ".")
