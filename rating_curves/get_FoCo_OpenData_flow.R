##------------------------------------------------------------------------------
##
## Script name: get_gage_flow.R
##
## Purpose of script: Download, tidy, and save flow data from FoCo OpenData
##
## Author: Adam N. Wlostowski and Angus Watters
##
## Date Created: 2021-12-28
##
## Copyright (c) Adam N. Wlostowski, 2021
## Email: awlostowski@lynker.com
##
## ---------------------------
##
## Notes:
##   
##
##------------------------------------------------------------------------------

remove(list = ls())  # clear all workspace variables
cat("\014")          # clear command line

## load packages
library(here)
library(tidyverse)
library(lubridate)
library(RSocrata)
library(logger)

##------------------------------------------------------------------------------
## Function definitions

getOpenData <- function(sensor_name) {
  # 
  # Retrieve flow DataFrame from https://opendata.fcgov.com/ by sensor name
  #
  # Args:
  #   sensor_name (str): name of sensor for which data will be retrieved
  #
  # Returns:
  #   Tidy DataFrame of gage flow data
  
  # full URL of City of Fort Collins OpenData site
  url <- "https://opendata.fcgov.com/resource/f5as-vvbj.json?sensor_name="
  
  # sensor-specific URL
  sensor_url <- paste0(url, gsub(" ", "%20", sensor_name))
  
  # retrieve data using the RSocrata library
  logger::log_info(
    "Downloading flow and stage data from opendata.fcgov.com at {sensor_name}"
    )
  flow_data <- RSocrata::read.socrata(
      url = sensor_url
  )
  
  # tidy data
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

##------------------------------------------------------------------------------
## Executed statements

sensor_name <- "Poudre Park"
poudre_park_flow <- getOpenData(sensor_name)

# save gage data to local path
path <- here::here("data","gauge")
filename <- "poudre_park_flow.RDS"
saveRDS(poudre_park_flow, paste0(path, "/", filename))

# ---- Plot historical flows @ Poudre Park ----
poudre_park_flow <- readRDS(paste0(path, "/", filename))

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
  geom_line(data = average_flow, aes(x = date, y = flow_cfs)) +
  labs( 
    title = "Streamflow at Poudre Park",
    y = "Flow (cfs)",
    x= "Date"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title  = element_text(size = 14)
  ) 
logger::log_info("saving a plot of Poudre Park flow observationsto {path}")
ggsave(paste0(path, "/poudre_park_flow.png"))



















