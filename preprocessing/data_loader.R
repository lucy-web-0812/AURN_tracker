
# Data loader so doesn't need to run everytime.... 

library(openair)
library(tidyverse)
library(dygraphs)
library(plotly)



network_meta_data<- importMeta(source = c("aurn", "aqe", "saqn", "waqn")) 


monthly_pm_data <- importUKAQ(year = 2015:2025,  pollutant = c("pm2.5", "pm10"), verbose = T, data_type = "monthly", source = c("aurn")) |> #, "aqe", "saqn", "waqn", "ni", "local"
  filter(if_any(c(pm2.5, pm10), ~ !is.na(.)))  


network_locations <- network_meta_data |> 
  filter(code %in% monthly_pm_data$code)


monthly_pm_data_with_locations <-  monthly_pm_data |> 
  left_join(network_locations)



write_csv(monthly_pm_data_with_locations, "monthly_pm_data_with_locations.csv")




monthly_pm_data |> 
  filter(code == "ACTH") |> 
  mutate(year = year(date)) |> 
  group_by(year) |> 
  summarise(annual_average = mean(pm10, na.rm = T)) |> 
  ggplot(aes(x = year, y = annual_average)) +
  geom_hline(yintercept = 10) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0,NA))




# what does this look like monthly? 

site_data <- monthly_pm_data |>
  filter(site == "Sibton")  |>
  mutate(month = month(date)) |>
  group_by(month) |>
  summarise(
    monthly_average_pm2.5 = mean(pm2.5, na.rm = T),
    monthly_average_pm10 = mean(pm10, na.rm = T), 
    st_dev_pm2.5 = sd(pm2.5, na.rm = T), 
    st_dev_pm10 = sd(pm10, na.rm = T), 
    count = n()
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
              colour = NA)  +
  facet_wrap(~pollutant)

)




averaged_data <- monthly_pm_data  |> 
  filter(code== "BEX") |> 
  mutate(month = month(date)) |> 
  group_by(month) |> 
  summarise(mean = mean(pm2.5, na.rm = T), 
            sd = sd(pm2.5, na.rm = T))



ggplotly(
monthly_pm_data |>
  filter(code == "BEX")  |>
  mutate(month = month(date)) |>
  mutate(year = year(date))  |> 
  ggplot(aes(x= month, y = pm2.5, colour = year)) +
  geom_line(aes(group = year), colour = "lightgrey", linetype = "dashed", linewidth  = 1.2) + 
  geom_line(data = averaged_data, aes(x = month, y = mean), colour = "red",linetype = "dashed", linewidth = 1.4) +
  geom_ribbon(data = averaged_data, aes(x = month, ymin = mean - sd, ymax = mean + sd, y = mean), colour = "red",linetype = "dashed", linewidth = 1.4, fill = "red", alpha = 0.2) +
  geom_point(data = averaged_data, aes(x = month, y = mean), colour = "red", size = 3) 

)



a <- monthly_pm_data |> 
  filter(code == "BEX") |> 
  group_by(code) |> 
  filter(between(date, max(date) - months(11), max(date))) |> 
  summarise(annual_average = mean(pm2.5, na.rm = T)) |> 
  pull()




# Looking at making just pm2.5 data  


monthly_pm_data_with_locations_pm2.5 <- monthly_pm_data_with_locations |> 
  select(source, date, uka_code, code, site,
         pm2.5, pm2.5_capture, latitude, longitude, site_type) |> 
  pivot_wider(id_cols = c(source,uka_code, code, site,latitude, longitude, site_type), names_from = date, values_from = c(pm2.5))




write_csv(monthly_pm_data_with_locations_pm2.5, "monthly_pm_data_with_locations_pm2.5.csv")
