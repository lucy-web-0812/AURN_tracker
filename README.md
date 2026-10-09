# AURN_tracker

A Shiny app for exploring monthly air quality data from Defra's Automatic Urban and Rural Network (AURN): pick sites, plot time series, compare against limits and download the data.

## What's where

```
aurn_data/                 <- the app (deploy this folder)
  app.R                    layout and server logic
  R/config.R               settings: pollutants, benchmarks, refresh interval, colours
  R/data.R                 downloading, caching and refreshing data
  R/summaries.R            12-month means, annual means, seasonal profile
  R/plots.R                the plotly charts
  data/aurn_monthly.rds    saved copy of the data (the app keeps this up to date)
preprocessing/
  update_data.R            rebuild the data file by hand (optional)
  data_loader.R            original exploration script
```

## Packages

```r
install.packages(c("shiny", "bslib", "bsicons", "leaflet", "plotly", "DT",
                   "dplyr", "tidyr", "readr", "openair"))
```

## How the automatic refresh works

1. The app loads `data/aurn_monthly.rds` straight away, so it opens fast.
2. If that copy hasn't been checked with Defra in the last 12 hours (`REFRESH_EVERY_HOURS` in `R/config.R`), the app re-downloads **only the current and previous year** (the only years Defra still changes) and merges them in.
3. Anyone can press **Check for new data** in the sidebar.
4. If UK-AIR Defra data can't be reached, the app keeps showing the saved copy and says so.

If you add a pollutant to `POLLUTANTS` in `R/config.R`, the next refresh downloads every year from 2015 once.


## Things you can change in `R/config.R`

- `POLLUTANTS`: add or remove pollutants (for example `o3`)
- `BENCHMARKS`: limit and guideline values
- `DEFAULT_SITES`: the sites shown when the app opens
- `MIN_CAPTURE`, `MIN_MONTHS_FOR_MEAN`: data quality rules
