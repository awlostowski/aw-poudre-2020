##------------------------------------------------------------------------------
##
## Script name: boatable_days_calculator.R
##
## Purpose of script: 
##  Calculate the annual number of boatable days for American Whitewater reaches
##  on the Poudre River, extending from downtown Fort Collins at the Whitewater
##  Park, through Big South near Cameron Pass. 
##
## Author: Adam N. Wlostowski
##
## Date Created: 2022-01-05
##
## Copyright (c) Adam N. Wlostowski, 2022
## Email: awlostowski@lynker.com
##
## ---------------------------
##
## Notes:
##
## - Need to access flow data from gage in Fort Collins
##   
##
##------------------------------------------------------------------------------

remove(list = ls())  # clear all workspace variables
cat("\014")          # clear command line

## load packages
library(here)
library(tidyverse)
library(dataRetrieval)
library(readr)
library(lubridate)

source(here::here('rating_curves', 'get_flow_utils.R'))

plot_path <- here::here('boatable_days','boatable_day_plots')

##------------------------------------------------------------------------------
## Function definitions

mean_flow = function(data, group_col) {
  
  data %>% 
    group_by(.dots = lazyeval::lazy(group_col)) %>% 
    summarize(flow = mean(flow, na.rm = T))
}

# ggplot theme
th <- theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text  = element_text(size = 11),
    legend.text = element_text(size = 12)
  )

##------------------------------------------------------------------------------
## Executed statements

# create a list of AW reaches and corresponding reference gages
site.gages <- list(
  "Poudre Whitewater Park" = "USGS_06752260",
  "Filter Plant"           = "CLAFTCCO",
  "Bridges"                = "Pineview model",
  "Poudre Park"            = "Pineview model",
  "Lower Mishawaka"        = "Pineview model",
  "Upper Mishawaka"        = "Pineview model",
  "The Narrows"            = "Pineview model",
  "Grandpa's Gorge"        = "Pineview model",
  "White Mile Run"         = "Pineview model",
  "Spencer Heights"        = "Pineview model",
  "Big South"              = "LAPLODCO"
)

# load flow preference data
flow_pref <- read_csv(here::here('private_data','flow_pref_by_reach.csv'))

# Retrieve historical flow observations and models
flow.usgs <- dataRetrieval::readNWISdv(
  siteNumbers = '06752260', 
  parameterCd = '00060',
  startDate = '1976-01-01',
  endDate = '2022-01-01') %>%
  rename(flow = 'X_00060_00003', date = Date) %>%
  mutate(site = 'USGS_06752260') %>%
  select(date, flow, site)
  
flow.CLAFTCCO <- GetCDSSStationFlow(
  site_abbrev = 'CLAFTCCO',
  save.data = TRUE) %>% 
  mean_flow(., date) %>%
  mutate(site = 'CLAFTCCO')

flow.LAPLODCO <- GetCDSSStationFlow(
  site_abbrev = 'LAPLODCO',
  save.data = TRUE) %>% 
  mean_flow(., date) %>%
  mutate(site = 'LAPLODCO')

flow.model <- readRDS(
  here::here('boatable_days','simulated_historical_pineview_flow.RDS')
  ) %>%
  mutate(site = 'Pineview model')

# join all flow records together
flow <- flow.usgs %>%
  rbind(flow.LAPLODCO) %>%
  rbind(flow.CLAFTCCO) %>%
  rbind(flow.model)

# clear individual records from memory
rm(flow.usgs, flow.CLAFTCCO, flow.LAPLODCO, flow.model)

#==========================

boatable.days <- data.frame()
for (site in names(site.gages)) {
  
  site_preference <- flow_pref %>%
    filter(segment == !!site)
  
  min.acceptable = site_preference$minimally.acceptable.flow[1]
  max.acceptable = site_preference$maximally.acceptable.flow[1]
  
  if (is.na(max.acceptable)) {
    max.acceptable = 100000000000
  }
  
  # tabulate annual boatable days 
  q <- flow %>%
    filter(site == !!site.gages[[site]]) %>%
    mutate(boatable = if_else(flow >= min.acceptable & flow <= max.acceptable,1,0)) %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(year) %>%
    summarize(boatable.days = sum(boatable, na.rm = T), total_flow = sum(flow, na.rm = T)) %>%
    mutate(year.type = case_when(total_flow <= unname(quantile(total_flow, prob = 0.25)) ~ "dry",
                                 total_flow >  unname(quantile(total_flow, prob = 0.25)) & 
                                   total_flow <= unname(quantile(total_flow, prob = 0.5)) ~ "dry-typical",
                                 total_flow > unname(quantile(total_flow, prob = 0.5)) & 
                                   total_flow <= unname(quantile(total_flow, prob = 0.75)) ~ "wet-typical",
                                 total_flow > unname(quantile(total_flow, prob = 0.75)) ~ "wet")) %>%
    mutate(segment = site) %>%
    select(segment, year, year.type, boatable.days)
  
  # bind site data to larger dataframe
  boatable.days <- rbind(boatable.days, q)
  
  # create a bar chart of boatable days for this site
  ggplot(data = q, aes(x = year, y = boatable.days, fill = year.type)) +
    geom_bar(stat = "identity") +
    labs(
      title  = paste('Boatable Days at', site),
      y      = "Annual Number of Boatable Days",
      x      = "Calendar Year",
      colour = " "
    ) +
    scale_y_continuous(breaks = seq(0, 150, by = 10),
                       limits = c(0, 150)) +
    scale_fill_manual(values=c("#d7191c", "#fdae61", "#2b83ba", "#abdda4")) +
    th
  filename <- paste0(gsub(" ","_", tolower(site)), '_boatable_days.png')
  ggsave(paste0(plot_path,'/', filename))
  
}

