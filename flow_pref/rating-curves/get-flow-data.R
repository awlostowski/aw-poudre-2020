library(here)
library(tidyverse)
library(lubridate)
library(rvest)
library(magrittr)
library(stringr)
library(stringi)
library(RSocrata)
library(tidyverse)


# Function that retrieves flow data frame https://opendata.fcgov.com/ by sensor name and returns a tidy dataframe.
get_flow_data <- function(sensor_name) {
  
  url <- "https://opendata.fcgov.com/resource/f5as-vvbj.json?sensor_name="
  
  sensor_url <- paste0(url, gsub(" ", "%20", sensor_name))
  
  flow_data <- RSocrata::read.socrata(
      url = sensor_url                    # 'https://opendata.fcgov.com/resource/f5as-vvbj.json?sensor_name=Poudre%20Park'
  )
  
  flow_data <- flow_data %>% 
    dplyr::select(sensor_name, timestamp, stage_ft, flow_cfs) %>% 
    dplyr::mutate(
      flow_cfs   = as.numeric(flow_cfs),
      stage_ft   = as.numeric(stage_ft),
      date       = lubridate::ymd(as.Date(timestamp)),
      time       = format(timestamp, "%H:%M")
    ) %>% 
    dplyr::select(sensor_name, date, time, stage_ft, flow_cfs) 
}

# test function 
sensor_name <-  "Poudre Park"
poudre_park_flow <- get_flow_data(sensor_name = sensor_name)

# save to local path 
# path <- "C:/Users/angus/OneDrive/Desktop/lynker/AWW/data/"
# saveRDS(poudre_park_flow, paste0(path, "poudre_park/poudre_park_flow.rds"))

# ---- Plot historical flows @ Poudre Park ----

# average flows per day
average_flow <- poudre_park_flow %>% 
  mutate(
    month  = month(date),
    year   =  year(date),
    day    = day(date)
    ) %>% 
  group_by(date) %>% 
  summarise(
    flow_cfs = mean(flow_cfs, na.rm = T)
  )

# average daily flow cfs
ggplot() +
  geom_line(data = average_flow, aes(x = date, y = flow_cfs))




















