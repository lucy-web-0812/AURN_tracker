
library(shiny)
library(shinydashboard)
library(openair)
library(tidyverse)
library(leaflet)
library(dygraphs)
library(plotly)


# 
# todays_date <- today(tzone = "UTC")
# 
# 
# network_meta_data<- importMeta(source = c("aurn", "aqe", "saqn", "waqn")) 
# 
# 
# monthly_pm_data <- importUKAQ(year = 2015:2025,  pollutant = c("pm2.5", "pm10"), verbose = T, data_type = "daily", source = c("aurn")) |> #, "aqe", "saqn", "waqn", "ni", "local"
#   filter(if_any(c(pm2.5, pm10), ~ !is.na(.)))  
# 
# 
# network_locations <- network_meta_data |> 
#   filter(code %in% monthly_pm_data$code)



# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "AURN data"),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
    box(
      width = 6, 
      leafletOutput("map")
      ),
    box(
      width = 6,
      valueBoxOutput("compliancebox_pm2.5", width = 6), 
      valueBoxOutput("compliancebox_pm10", width = 6)# dygraphOutput("timeseries")
    )
  ),
  fluidRow(
    box(
      title = "Monthly Averages",
      width = 12, 
      plotlyOutput("monthly_averages")
      # dygraphOutput("timeseries")
    )
    )
 )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$map <- renderLeaflet({
    leaflet(network_locations) |>
      addTiles() |>
      addMarkers(
        lat = ~latitude,
        lng = ~longitude,
        popup = ~site,
        layerId = ~code, 
       #clusterOptions = markerClusterOptions(), 
        group = "Markers"
      ) |> 
      addLayersControl(overlayGroups = c("Markers"))
    
  })
  
  
  selected_site <- reactiveVal(NULL)
  
  observeEvent(input$map_marker_click, {
    site_clicked <- input$map_marker_click$id
    selected_site(site_clicked)
  })
  
  
  
  
  output$timeseries <- renderDygraph({
    req(selected_site())
    
    
    # Filter data for selected site
    site_data <- monthly_pm_data |> 
      filter(code == selected_site()) 
    
    # Select only numeric columns (pollutants) and create xts object.
    numeric_cols <- site_data |> 
      select(where(is.numeric)) 
    
    # Create xts object with date as index
    xts_data <- xts::xts(numeric_cols, order.by = site_data$date) 
    
    # Create dygraph
    dygraph(xts_data) |> 
      dyRangeSelector(dateWindow = c("2015-01-01", "2025-10-01")) |> 
      dyOptions(stackedGraph = FALSE) |> 
      dyAxis("x", drawGrid = FALSE) |> 
      dyRoller(showRoller = TRUE, rollPeriod = 1) |> 
      dyUnzoom() |> 
      dyCrosshair(direction = "vertical")
  })
  
  output$monthly_averages <- renderPlotly({
    req(selected_site())
    
    # Filter data for selected site
    site_data <- monthly_pm_data |> 
      filter(code == selected_site()) |> 
      mutate(month = month(date)) |>
      group_by(month) |>
      summarise(
        monthly_average_pm2.5 = mean(pm2.5, na.rm = T),
        monthly_average_pm10 = mean(pm10, na.rm = T), 
        st_dev_pm2.5 = sd(pm2.5, na.rm = T), 
        st_dev_pm10 = sd(pm10, na.rm = T)
      ) |>
      pivot_longer(
        cols = c(monthly_average_pm2.5, monthly_average_pm10, st_dev_pm2.5, st_dev_pm10),
        names_to = c("metric", "pollutant"),
        names_pattern = "(.*)_(pm.*)",
        values_to = "value"
      )  |>
      pivot_wider(
        names_from = metric,
        values_from = value
      )
    
    
    
    ggplotly(
      site_data |> 
        ggplot(aes(x = month, y = monthly_average, colour = pollutant)) +
        geom_point() +
        geom_line() +
        geom_ribbon(aes(ymin = monthly_average - st_dev,
                        ymax = monthly_average + st_dev,
                        fill = pollutant),
                    alpha = 0.2,
                    colour = NA) +
        geom_hline(yintercept = 40, linetype = "dashed") +
        geom_hline(yintercept = 20, linetype = "dashed") +
        scale_x_continuous(name = "Month", 
                           breaks = seq(1,12,1), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
        scale_y_continuous(name = "PM Concentration", limits = c(0,NA), expand = c(0,0)) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(colour = "lightgrey"),
              panel.grid.minor.y = element_line(colour = "lightgrey"))
      
    )
    
    
    
    
  })
  
  
  output$compliancebox_pm2.5 <- renderValueBox({
    
    req(selected_site())
    
    pm_average_year <- monthly_pm_data |> 
      filter(code == selected_site()) |> 
      filter(between(date, max(date) - months(11), max(date))) |> 
      summarise(annual_average = mean(pm2.5, na.rm = T)) |> 
      pull()
    
    
    validate(
      need(!is.na(pm_average_year), "No PM2.5 data available for this site")
    )
    
    
    value_box_colour <- if (pm_average_year > 10){
      colour = "red"
    } else {
      colour = "green"
    }
    
  
    valueBox(
      round(pm_average_year, 2), 
      "12 month average PM2.5 at site",
      icon =icon("smog"),
      color = value_box_colour
    )
  })
    
  
  output$compliancebox_pm10 <- renderValueBox({
    
    req(selected_site())
    
    pm_average_year <- monthly_pm_data |> 
      filter(code == selected_site()) |> 
      filter(between(date, max(date) - months(11), max(date))) |> 
      summarise(annual_average = mean(pm10, na.rm = T)) |> 
      pull()
    
    validate(
      need(!is.na(pm_average_year), "No PM10 data available for this site")
    )
    
    
    
    value_box_colour <- if (pm_average_year > 10){
      colour = "red"
    } else {
      colour = "green"
    }
    
    
    valueBox(
      round(pm_average_year, 2), 
      "12 month average PM10 at site",
      icon = icon("smog"),
      color = value_box_colour
    )
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
