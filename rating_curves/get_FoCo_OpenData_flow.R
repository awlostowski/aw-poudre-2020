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
##  - updated 12/28/21 to enable data get from multiple sites. 
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
source(here::here('rating_curves', 'get_flow_utils.R'))

##------------------------------------------------------------------------------

## Executed statements

# list of sensor sites
sensors <- c("Poudre Park", "Rustic")

# save gage data to local path
path <- here::here("data","gauge")

for (s in sensors) {
  
  station.data <- getOpenDataFlow(s)
  
  # save data to disk as RDS
  filename <- paste0(
    gsub(" ", "_", tolower(s)),
    "_flow.RDS"
    )
  saveRDS(station.data, paste0(path, "/", filename))
  
}


# ---- Plot historical flows @ Poudre Park ----
for (s in sensors) {
  
  filename <- paste0(
    gsub(" ", "_", tolower(s)),
    "_flow.RDS"
  )
  
  figurename <- paste0(
    gsub(" ", "_", tolower(s)),
    "_flow.png"
  )
  
  station.data <- readRDS(paste0(path, "/", filename))
  
  # average flows per day
  average_flow <- station.data %>% 
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
      title = paste("Streamflow at", s),
      y = "Flow (cfs)",
      x= "Date"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 14),
      axis.title  = element_text(size = 14)
    ) 
  logger::log_info("saving a plot of {s} flow observations to {path}")
  ggsave(paste0(path, "/", figurename))
  
}






















