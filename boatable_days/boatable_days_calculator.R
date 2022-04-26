# **********************************************************************
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
## **********************************
##
## Notes:
##
## - Need to access flow data from gage in Fort Collins
##   
##
# **********************************************************************

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

# **********************************************************************
# Function definitions
mean_flow = function(data, group_col) {
  
  data %>% 
    group_by(.dots = lazyeval::lazy(group_col)) %>% 
    summarize(flow = mean(flow, na.rm = T))
}

# ggplot theme
th <- theme_bw() +
  theme(
    plot.title     = element_text(size = 26, face = "bold"),
    axis.title     = element_text(size = 18, face = "bold"),
    axis.text      = element_text(size = 16),
    strip.text.x   = element_text(size = 18, color = "black",face = "bold"),
    strip.text.y   = element_text(size = 18, color = "black",face = "bold"),
    legend.text    = element_text(size = 16),
    plot.subtitle  = element_text(size = 20)
  )
# **********************************************************************
## Executed statements

# create a list of AW reaches and corresponding reference gages
site_gages <- data.frame(
  "Poudre Whitewater Park" = "USGS_06752260",
  "Filter Plant"           = "CLAFTCCO",
  "Bridges"                = "Pineview model",
  "Poudre Park"            = "Pineview model",
  "Lower Mishawaka"        = "Pineview model",
  "Upper Mishawaka"        = "Pineview model",
  "The Narrows"            = "Pineview model",
  "Grandpas Gorge"        = "Pineview model",
  "White Mile Run"         = "Pineview model",
  "Spencer Heights"        = "Pineview model",
  "Big South"              = "LAPLODCO"
) %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "segment", 
    values_to = "site"
  ) %>% 
  mutate(
    # segment = stringr::str_replace_all(segment,"\\.","_")
    segment = stringr::str_replace_all(segment,"\\."," ")
  )

# ---- Flow preference data ----
flow_pref <- read_csv(here::here('data','flow_pref_by_reach.csv')) %>% 
  mutate(
    segment = case_when(
      segment == "Grandpa's Gorge" ~ "Grandpas Gorge",
      TRUE ~ segment
    )
  )

# ---- Historical flows & models ----
# look for these data to be saved to disk first, before downloading and saving.

usgs_file <- here::here(
  "data/gauge", 
  "pourde_whitewater_park_usgs_station_flow.rds"
)

# Import data
if (file.exists(usgs_file)) {
  flow.usgs  <-readRDS(file = usgs_file)
} else {
  logger::log_error(
    'Downloading Poudre Whitewater park USGS gauge data \nCould not locate \n{usgs_file}'
  )
  # Poudre Whitewater Park
  flow.usgs <- dataRetrieval::readNWISdv(
    siteNumbers = '06752260', 
    parameterCd = '00060',
    startDate   = '1976-01-01',
    endDate     = '2022-03-16'
  ) %>%
    rename(flow = 'X_00060_00003', date = Date) %>%
    mutate(site = 'USGS_06752260') %>%
    select(date, flow, site)
  
  # save to disk
  saveRDS(flow.usgs, usgs_file)
  stop()
}

# Filter plant file
CLAFTCCO_file <- here::here(
  "data/gauge", 
  "CLAFTCCO_station_flow.RDS"
)

# Import data
if (file.exists(CLAFTCCO_file)) {
  flow.CLAFTCCO  <-readRDS(file = CLAFTCCO_file) %>% 
    mean_flow(., date) %>%
    mutate(site = 'CLAFTCCO')
} else {
  logger::log_error(
    'Downloading CLAFTCCO gauge data \nCould not locate \n{CLAFTCCO_file}'
  )
  # Filter plant
  flow.CLAFTCCO <- GetCDSSStationFlow(
    site_abbrev = 'CLAFTCCO',
    save.data = TRUE) %>% 
    mean_flow(., date) %>%
    mutate(site = 'CLAFTCCO')
  stop()
}

# Big South file
LAPLODCO_file <- here::here(
  "data/gauge", 
  "LAPLODCO_station_flow.RDS"
)

# Import data
if (file.exists(LAPLODCO_file)) {
  flow.LAPLODCO  <-readRDS(file = LAPLODCO_file) %>% 
    mean_flow(., date) %>%
    mutate(site = 'LAPLODCO')
} else {
  logger::log_error(
    'Downloading LAPLODCO gauge data \nCould not locate \n{LAPLODCO_file}'
  )
  
  # Big South
  flow.LAPLODCO <- GetCDSSStationFlow(
    site_abbrev = 'LAPLODCO',
    save.data = TRUE) %>% 
    mean_flow(., date) %>%
    mutate(site = 'LAPLODCO')
  stop()
}

# Flow preference data
flowpref.file  <- here::here(
  "private_data", 
  "flow-pref-data_20220105.RDS"
  # "flow-pref-data_20200630.RDS"
)

# Pineview flow model
flow_model <- readRDS(
  here::here('boatable_days','simulated_historical_pineview_flow.RDS')
) %>%
  mutate(site = 'Pineview model')

# simulated flow model using Reservoir flows
reservoir_flows <- readRDS(
  here::here('reservoirs','reservoir_flows_daily.rds')) %>%
  pivot_wider(
    id_cols     = c(tidyselect::matches("structure"), tidyselect::matches("flow_type"), date),
    names_from  = c(tidyselect::matches("flow_type"), tidyselect::matches("structure")),
    values_from = flow
  )

# join all flow records together
flow <- flow.usgs %>%
  rbind(flow.LAPLODCO) %>%
  rbind(flow.CLAFTCCO) %>%
  rbind(flow_model) %>% 
  left_join(
    reservoir_flows,
    by = "date"
  ) %>% 
  tibble() %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    flow = case_when(
      flow < 0 ~ 0,
      TRUE     ~ flow
    )
  ) %>% 
  mutate(
    diversion = case_when(
      site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (diversion_long_draw + diversion_chambers + diversion_joe_wright + 
                                                                       diversion_peterson + diversion_barnes_meadow),
      site == "LAPLODCO"                                          ~ (diversion_long_draw + diversion_peterson)
    ),
    release = case_when(
      site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (release_long_draw + release_chambers + release_joe_wright + 
                                                                       release_peterson + release_barnes_meadow),
      site == "LAPLODCO"                                          ~ (release_long_draw + release_peterson)
    ),
    dvolume = case_when(
      site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (dvolume_long_draw + dvolume_chambers + dvolume_joe_wright + 
                                                                       dvolume_peterson + dvolume_barnes_meadow),
      site == "LAPLODCO"                                          ~ (dvolume_long_draw + dvolume_peterson)
    ),
    nat_flow = case_when(
      site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (flow + dvolume),
      site == "LAPLODCO"                                          ~ (flow + dvolume)
    )
  ) %>% 
  dplyr::select(site, date, flow, nat_flow, diversion, release, dvolume)

# clear individual records from memory
rm(flow.usgs, flow.CLAFTCCO, flow.LAPLODCO, flow_model)

# ********************************************
# ---- Loop through each segment of river ----
# ********************************************

boatable_year_lst   <- list()
boatable_month_lst  <- list()
boatable_day_lst    <- list()

for (i in 1:nrow(site_gages)) {
  
  site_preference <- flow_pref %>%
    filter(segment == site_gages$segment[i])
  
  logger::log_info("Calculating boatable days in year @ {site_gages$segment[i]}")
  
  min.acceptable = site_preference$minimally.acceptable.flow[1]
  max.acceptable = site_preference$maximally.acceptable.flow[1]
  
  if (is.na(max.acceptable)) {
    max.acceptable = 100000000000
  }
  
  flow_year <- flow %>% 
    filter(site == site_gages$site[i]) %>% 
    mutate(year = lubridate::year(date)) %>%
    group_by(year) %>%
    summarize(
      flow       = sum(flow, na.rm = T),
      nat_flow   = sum(nat_flow, na.rm = T),
      diversion  = sum(diversion, na.rm = T),
      release    = sum(release, na.rm = T),
      dvolume    = sum(dvolume, na.rm = T)
    ) %>% 
    ungroup()
  
  # simulated monthly total flows to join with monthly boatable days
  flow_month <- flow %>% 
    filter(site == site_gages$site[i])%>% 
    mutate( 
      year  = lubridate::year(date),
      month = lubridate::month(date)
    ) %>%
    group_by(year, month) %>%
    summarize(
      flow       = sum(flow, na.rm = T),
      nat_flow   = sum(nat_flow, na.rm = T),
      diversion  = sum(diversion, na.rm = T),
      release    = sum(release, na.rm = T),
      dvolume    = sum(dvolume, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(date  = as.Date(paste0(year, "-", month, "-01"))) %>% 
    dplyr::select(date, flow, nat_flow, diversion, release, dvolume)
  
  
  # daily total flows to join with monthly boatable days
  flow_day <- flow %>% 
    filter(site == site_gages$site[i]) %>% 
    ungroup() %>% 
    dplyr::select(date, flow, nat_flow, diversion, release, dvolume)
  
  # Annual boatable days - Observed Flows
  boat_year <- boatable_days(
    flow_data      = filter(flow, site == site_gages$site[i]) %>% 
      dplyr::select(site, date, flow),
    min_acceptable = min.acceptable,
    max_acceptable = max.acceptable,
    timescale      = "year") %>%    
    mutate(
      segment = site_gages$segment[i]
    ) %>%
    select(segment, year, year_type, boatable_days)
  
  # Annual boatable days - Naturalized Flows
  boat_nat_year <- boatable_days(
    flow_data      = filter(flow, site == site_gages$site[i]) %>% 
      dplyr::select(site, date, nat_flow) %>% 
      dplyr::rename(flow = nat_flow),
    min_acceptable = min.acceptable,
    max_acceptable = max.acceptable,
    timescale      = "year") %>%    
    mutate(
      segment = site_gages$segment[i]
    ) %>%
    select(segment, year, year_type, nat_boatable_days = boatable_days)
  
  # join observed + simulated
  q_year <- boat_year %>% 
    left_join( 
      dplyr::select(boat_nat_year, year, nat_boatable_days),
      by = "year"
    ) %>% 
    left_join(
      flow_year,
      by = "year"
    ) %>% 
    ungroup()
  
  logger::log_info("Calculating boatable days in month @ {site_gages$segment[i]}")
  
  # Annual boatable days - Observed Flows
  boat_month <- boatable_days(
    flow_data      = filter(flow, site == site_gages$site[i]) %>% 
      dplyr::select(site, date, flow),
    min_acceptable = min.acceptable,
    max_acceptable = max.acceptable,
    timescale      = "month") %>%    
    mutate(
      segment = site_gages$segment[i]
    ) %>%
    select(segment, date, month_type, boatable_days)
  
  # Annual boatable days - Naturalized Flows
  boat_nat_month <- boatable_days(
    # flow_data      =  filter(flow, site == site.gages[[i]]),
    flow_data      = filter(flow, site == site_gages$site[i]) %>% 
      dplyr::select(site, date, nat_flow) %>% 
      dplyr::rename(flow = nat_flow),
    min_acceptable = min.acceptable,
    max_acceptable = max.acceptable,
    timescale      = "month") %>%    
    mutate(
      segment = site_gages$segment[i]
    ) %>%
    select(segment, date, month_type, nat_boatable_days = boatable_days)
  
  # join observed + simulated
  q_month <- boat_month %>% 
    left_join( 
      dplyr::select(boat_nat_month, date, nat_boatable_days),
      by = "date"
    ) %>% 
    left_join(
      flow_month,
      by = "date"
    ) %>% 
    ungroup()
  
  logger::log_info("Calculating boatable days @ {site_gages$segment[i]}")
  
  # boatable days from observed flow
  boat_day <- boatable_days(
    flow_data      = filter(flow, site == site_gages$site[i]) %>% 
      dplyr::select(site, date, flow),
    min_acceptable = min.acceptable,
    max_acceptable = max.acceptable,
    timescale      = "day"
  ) %>% 
    mutate(
      segment = site_gages$segment[i]
    ) %>%
    select(segment, date, boatable_days)
  
  # boatable days from Simulated reservoir flows
  boat_nat_day <- boatable_days(
    flow_data      = filter(flow, site == site_gages$site[i]) %>% 
      dplyr::select(site, date, nat_flow) %>% 
      dplyr::rename(flow = nat_flow),
    min_acceptable = min.acceptable,
    max_acceptable = max.acceptable,
    timescale      = "day"
  ) %>% 
    mutate(
      segment = site_gages$segment[i]
    ) %>%
    select(segment, date, nat_boatable_days = boatable_days)
  
  # join observed + simulated
  q_day <- boat_day %>% 
    left_join( 
      dplyr::select(boat_nat_day, date, nat_boatable_days),
      by = "date"
    ) %>% 
    left_join(
      flow_day,
      by = "date"
    ) 
  
  logger::log_info("Plotting annual boatable days @ {site_gages$segment[i]}")
  
  # pivot data long for plotting
  q_year_long <- q_year %>% 
    rename("Boatable Days" = boatable_days, "Naturalized Boatable Days" = nat_boatable_days) %>% 
    pivot_longer(cols = c("Boatable Days", "Naturalized Boatable Days"))
  
  # create a line chart of boatable days & simulated boatable days for this site
  boatable_days_plot <- 
    ggplot() +
    geom_line(data = q_year_long, aes(x = year, y = value, col = name), size = 1.5) +
    geom_point(data = q_year_long, aes(x = year, y = value, col = name), size = 3) +
    # geom_col(data = q_year_long, aes(x = year, y = value, fill = year_type)) +
    # facet_wrap(~name) +
    labs(
      title  = paste('Boatable Days at', site_gages$segment[i]),
      subtitle = "Naturalized Boatable days calculated using simulated flows\nNaturalized flows = Pineview flow model + (Diversions - releases)",
      y      = "Annual Number of Boatable Days",
      x      = "Calendar Year",
      col    = ""
    ) +
    scale_y_continuous(
      breaks = seq(0, 150, by = 10),
      limits = c(0, 150)
    ) +
    # scale_fill_manual(values=c("#d7191c", "#fdae61", "#2b83ba", "#abdda4")) +
    th
  
  # save ggplot to "aw-poudre-2020/boatable_days/boatable_day_plots/"
  filename <- paste0(gsub(" ","_",  tolower(site_gages$segment[i])), '_boatable_days.png')
  
  # Export plot
  ggsave(
    paste0(plot_path,'/', filename),
    plot   = boatable_days_plot,
    width  = 46,
    height = 28, 
    units  = "cm"
  )
  
  # iterativly add boatable days dataframes to lists (year, month, and days)
  boatable_year_lst[[i]]  <- q_year
  boatable_month_lst[[i]] <- q_month
  boatable_day_lst[[i]]   <- q_day

  rm(q_year, q_month, q_day, 
     boat_nat_day, boat_nat_month, boat_nat_year, 
     boat_day, boat_month, boat_year, 
     flow_day, flow_month, flow_year, q_year_long
  )
}

# *******************************
# ---- Summarize & Save Data ----
# *******************************

# Yearly boatable days 
boatable_year  <- bind_rows(boatable_year_lst) %>% 
  mutate(
    dboatable = boatable_days - nat_boatable_days 
  ) %>%
  mutate(
    dboatable_cat = case_when(
      dboatable > 0   ~ "More boatable days observed",
      dboatable == 0  ~ "Same boatable days observed/simulated",
      dboatable < 0   ~ "More boatable days simulated"
    ),
    dboatable_cat = factor(dboatable_cat,
                           levels = c(
                             "More boatable days observed",
                             "Same boatable days observed/simulated",
                             "More boatable days simulated"))
  )

# Daily boatable days 
boatable_day   <- bind_rows(boatable_day_lst) %>% 
  left_join(
    dplyr::select(site_gages, segment, site),
    by = c("segment")
    ) %>% 
  dplyr::relocate(segment, site, date) %>% 
  mutate(
    dboatable = boatable_days - nat_boatable_days 
  ) %>%
  mutate(
    dboatable_cat = case_when(
      dboatable > 0   ~ "More boatable days observed",
      dboatable == 0  ~ "Same boatable days observed/simulated",
      dboatable < 0   ~ "More boatable days simulated"
    ),
    dboatable_cat = factor(dboatable_cat,
                           levels = c(
                             "More boatable days observed",
                             "Same boatable days observed/simulated",
                             "More boatable days simulated")
                           )
  )

# Normalize the flow variable by mean historical flow for the same day of year. This way, the variable represents whether or not flow is higher or lower than is typical for that time of year. Values below 1 show that flow is lower than is typical, while values greater than 1 show that flow is higher than normal.

# Divide the monthly diversion and release volume by  initial flow. This would tell us the diversions/release rate as a proportion of the streamflow rate at top of the month for which the diversion/releases rate applies

# Calculate initial monthly flow (initial_flow) & change in flow from prior month (dflow)
month_flows <- boatable_day %>% 
  mutate(
    year  = lubridate::year(date),
    month = lubridate::month(date),
    day   = lubridate::day(date)
  ) %>% 
  mutate(
    date_ym = as.Date(paste0(year, "-", month, "-01")),
  ) %>% 
  group_by(segment, date_ym) %>% 
  slice(
    which.min(day),
    which.max(day)
  ) %>% 
  arrange(segment, date) %>%
  mutate(
    days_in_month  = days_in_month(date),
    flow_diff      = diff(flow)
    ) %>% 
  dplyr::relocate(segment, site, date, date_ym, day, days_in_month, 
                  flow, flow_diff) %>% 
  ungroup() %>% 
  group_by(segment) %>%
  arrange(segment, date, .by_group	= T) %>% 
  mutate(
    dflow          = lag(flow_diff)/lag(days_in_month)
  ) %>% 
  ungroup() %>%   
  dplyr::relocate(segment, site, date, date_ym, day, days_in_month, 
                  flow, flow_diff, dflow) %>% 
  group_by(segment, date_ym) %>%
  slice(which.min(day)) %>%
  ungroup() %>% 
  group_by(segment) %>% 
  mutate(
    dflow        = zoo::na.locf(dflow, fromLast = T),
    dflow        = round(dflow, 5),
    flow         = round(flow, 5),
    flow_diff    = round(flow_diff, 5)
  ) %>%
  ungroup() %>%
  rename(initial_flow = flow) %>% 
  # dplyr::select(segment, site, year, month, initial_flow = flow, flow_diff, dflow) %>%
  group_by(segment, month) %>% 
  mutate(
    initial_flow_mean   = round(mean(initial_flow, na.rm = T), 4)
  ) %>% 
  ungroup() %>%
  mutate(
    # initial_flow_pct    = round(initial_flow/)
    initial_flow_normal = round(initial_flow/initial_flow_mean, 4),
    initial_flow_anom   = round(initial_flow - initial_flow_mean, 4),
    segment             = factor(segment),
    month               = lubridate::month(date, label = T),
    diversion_prop      = round((diversion/initial_flow), 4),
    release_prop        = round((release/initial_flow), 4),
  ) %>% 
  ungroup()  %>% 
  dplyr::select(segment, site, date = date_ym, month,  # year,
                initial_flow, initial_flow_mean,
                # initial_flow_normal_divide_mean, initial_flow_normal_minus_mean,
                initial_flow_normal, initial_flow_anom,
                dflow,
                diversion, release, diversion_prop, release_prop, dvolume
                ) 


# Replace NA/ Inf w/ Zeros
is.na(month_flows)<-sapply(month_flows, is.infinite)
month_flows[is.na(month_flows)]<-0

# Join normalized flows & change in flows
boatable_month <-  bind_rows(boatable_month_lst) %>% 
  dplyr::select(segment, date, boatable_days, nat_boatable_days, flow, nat_flow) %>% 
  left_join(
    month_flows, 
    by = c("segment", "date")
    ) %>% 
  dplyr::relocate(segment, site, date, month, boatable_days, nat_boatable_days, flow, nat_flow)

# Divide the monthly diversion and release volume by the flow rate at the beginning of the year (initial flow). This would tell us the diversion (or release) rate as a proportion of the streamflow rate at the top of the month for which the diversion (or release) rate applies.


# Save boatable years/months/days dataframes as RDS
path     <- here::here("data/boatable_days/")
filename1 <- 'boatable_days_year.rds'
filename2 <- 'boatable_days_month_v2.rds'
filename3 <- 'boatable_days_day.rds'

logger::log_info('Saving yearly/monthly/daily boatable days data:\n{filename1}\n{filename2}\n{filename3}')

saveRDS(boatable_year, paste0(path, "/", filename1))
saveRDS(boatable_month, paste0(path, "/", filename2))
saveRDS(boatable_day, paste0(path, "/", filename3))

# *************************
# ---- Flow Timeseries ----
# *************************

# ************************
# ----  Monthly flows ----
# ************************

# Calculate initial monthly flow (initial_flow) & change in flow from prior month (dflow)
flows_month_ts <- boatable_day %>% 
  # filter(segment == "Poudre Park") %>%
  mutate(
    year  = lubridate::year(date),
    month = lubridate::month(date),
    day   = lubridate::day(date)
  ) %>% 
  mutate(
    date_ym = as.Date(paste0(year, "-", month, "-01")),
  ) %>% 
  # filter(date_ym == "2015-03-01") %>%
  # filter(date_ym >= "2015-03-01", date_ym <= "2015-04-01") %>%
  # filter(year == 2015) %>%
  group_by(segment, date_ym) %>%
  mutate(
    total_flow   = sum(flow, na.rm = T)
  ) %>% 
  slice(
    which.min(day),
    which.max(day)
  ) %>%
  arrange(segment, date) %>% 
  mutate(
    days_in_month  = days_in_month(date)
    ) %>% 
  ungroup()

# Start of month flows
month_initial_flow <- 
  flows_month_ts %>% 
  group_by(segment, date_ym) %>% 
  slice(which.min(date)) %>% 
  ungroup() %>%
  dplyr::select(segment, year, month_start_date = date,  date_ym,
                days_in_month, initial_flow = flow) 

# End of month flows
month_end_flow <- 
  flows_month_ts %>% 
  group_by(segment, date_ym) %>% 
  slice(which.max(date)) %>% 
  ungroup() %>% 
  mutate(
    nat_end_flow = flow + diversion - release
  ) %>%
  dplyr::select(segment, year, month_end_date = date, 
                date_ym, end_flow = flow, nat_end_flow, diversion, release)

# Join Start and end of week flows
flow_differences <- 
  left_join(
    month_initial_flow,
    month_end_flow,
    by = c("segment", "year", "date_ym")
  ) %>% 
  dplyr::relocate(
    segment, year, month_start_date, month_end_date, date_ym, days_in_month,
    initial_flow, end_flow, nat_end_flow, diversion, release
    ) %>% 
  mutate(
    dflow         = (end_flow - initial_flow)/days_in_month,
    nat_dflow     = (nat_end_flow - initial_flow)/days_in_month
  ) %>%  
  dplyr::relocate(
    segment, year, month_start_date, month_end_date, date_ym, days_in_month,
    initial_flow, end_flow, nat_end_flow, dflow, nat_dflow, diversion, release
  )

# Total flow in week and change in flow over week 
flow_month_summary <- 
  flows_month_ts %>% 
  dplyr::select(segment, date, date_ym, year, total_flow) %>% 
  ungroup() %>% 
  group_by(segment, date_ym) %>% 
  summarize(
    total_flow = mean(total_flow, na.rm = T)
  ) %>% 
  left_join(
    flow_differences, 
    by = c("segment", "date_ym")
  ) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(
    month = lubridate::month(date_ym, label = T),
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "winter",
      month %in% c("Mar", "Apr", "May") ~ "spring",
      month %in% c("Jun", "Jul", "Aug") ~ "summer",
      month %in% c("Sep", "Oct", "Nov") ~ "fall"
    ),
    season = factor(season)
  ) %>%
  dplyr::select(
    segment, date = date_ym, month_start_date, month_end_date, season, 
    initial_flow, end_flow, nat_end_flow, dflow, nat_dflow, 
    diversion, release, total_flow) %>% 
  ungroup()

# Replace NA/ Inf w/ Zeros
is.na(flow_month_summary) <- sapply(flow_month_summary, is.infinite)
flow_month_summary[is.na(flow_month_summary)] <- 0

# Save flow timeseries dataframes as RDS
path               <- here::here("data/boatable_days/")
flow_month_ts_path <- 'flow_month_timeseries.rds'

logger::log_info('Saving flow timeseries data:\n{flow_month_ts_path}')
saveRDS(flow_month_summary, paste0(path, "/", flow_month_ts_path))

# ***********************
# ----  Weekly flows ----
# ***********************


# Calculate weekly flows (initial_flow) & change in flow from prior week (dflow)
flows_week_ts <- boatable_day %>% 
  # filter(segment == "Poudre Park") %>%
  mutate(
    year      = lubridate::year(date),
    month     = lubridate::month(date),
    week      = lubridate:::week(date),
    day_num   = lubridate::day(date)
  ) %>% 
  mutate(
    date_ym = as.Date(paste0(year, "-", month, "-01")),
  ) %>% 
  # filter(year %in% c(2015, 2016)) %>%
  group_by(segment, year, week) %>%
  mutate(
    total_flow   = sum(flow, na.rm = T),
    days_in_week = n()
  ) %>%
  group_by(segment, year, week) %>%
  # group_by(segment, date_ym) %>%
  slice(
    which.min(day_num),
    which.max(day_num)
  ) %>%
  arrange(segment, date) %>%
  mutate(
    days_in_month  = days_in_month(date),
    flow_diff      = diff(flow)
    # between0       = as.numeric(difftime(date, lag(date,1))), 
    # days_in_week        = ifelse(is.na(between0), 0, between0),
  ) %>%
  dplyr::relocate(segment, site, date, date_ym, day_num, week, days_in_week,
                  days_in_month, flow, flow_diff, total_flow) %>% 
  ungroup() %>%
  group_by(segment) %>%
  arrange(segment, date, .by_group	= T) %>%
  mutate(    
    dflow            = flow_diff/days_in_week
  ) %>%
  ungroup() %>%
  dplyr::relocate(segment, site, date, date_ym, day_num, week,  days_in_week, 
                  days_in_month, flow, flow_diff, total_flow, dflow, diversion, release) 
 

# Start of week flows
week_initial_flow <- 
  flows_week_ts %>% 
  group_by(segment, year, week) %>% 
  slice(which.min(date)) %>% 
  ungroup() %>%
  dplyr::select(segment, year, week_start_date = date, 
                day_num, week, days_in_week, initial_flow = flow) 

# End of week flows
week_end_flow <- 
  flows_week_ts %>% 
  group_by(segment, year, week) %>% 
  slice(which.max(date)) %>% 
  ungroup() %>% 
  mutate(
    nat_end_flow = flow + diversion - release
  ) %>% 
  dplyr::select(segment, year, week_end_date = date, 
                # date_ym, 
                week, end_flow = flow, nat_end_flow, diversion, release)

# Join Start and end of week flows
flow_differences <- 
  left_join(
    week_initial_flow,
    week_end_flow,
    by = c("segment", "year", "week")
    ) %>% 
  mutate(
    year  = lubridate::year(week_start_date), 
    month = lubridate::month(week_start_date) 
  ) %>% 
  dplyr::relocate(segment, year, month, week_start_date, week_end_date,
                  day_num, week, days_in_week, initial_flow, end_flow, nat_end_flow) %>% 
  mutate(
    # dflow = (end_flow - initial_flow)/days_in_week
    dflow         = (end_flow - initial_flow)/days_in_week,
    nat_dflow     = (nat_end_flow - initial_flow)/days_in_week
  ) %>% 
  dplyr::relocate(segment, year, month, week_start_date, week_end_date,
                  day_num, week, days_in_week, initial_flow, end_flow, nat_end_flow, dflow, nat_dflow) 

# Total flow in week and change in flow over week 
flow_week_summary <- 
  flows_week_ts %>% 
  dplyr::select(segment, date, year, week, total_flow, dflow) %>% 
  ungroup() %>% 
  group_by(segment, year, week) %>% 
  summarize(
    total_flow = mean(total_flow, na.rm = T)
    ) %>% 
  left_join(
    flow_differences, 
    by = c("segment", "year", "week")
    ) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(
    month = lubridate::month(month, label = T),
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "winter",
      month %in% c("Mar", "Apr", "May") ~ "spring",
      month %in% c("Jun", "Jul", "Aug") ~ "summer",
      month %in% c("Sep", "Oct", "Nov") ~ "fall"
    ),
    season = factor(season)
  ) %>%
  dplyr::select(segment, week_start_date, week_end_date, year, season, month, week, days_in_week, initial_flow, end_flow, nat_end_flow, dflow, nat_dflow, diversion, release, total_flow) %>% 
  ungroup()

# Replace NA/ Inf w/ Zeros
is.na(flow_week_summary) <- sapply(flow_week_summary, is.infinite)
flow_week_summary[is.na(flow_week_summary)] <- 0

# Save flow timeseries dataframes as RDS
path         <- here::here("data/boatable_days/")
flow_week_ts_path <- 'flow_week_timeseries.rds'

logger::log_info('Saving flow weekly timeseries data:\n{flow_week_ts_path}')
saveRDS(flow_week_summary, paste0(path, "/", flow_week_ts_path))

# **********************
# ----  Daily flows ----
# **********************

# Calculate weekly flows (initial_flow) & change in flow from prior week (dflow)
flows_day_ts <- boatable_day %>% 
  # filter(segment == "Poudre Park") %>%
  mutate(
    year      = lubridate::year(date),
    month     = lubridate::month(date),
    week      = lubridate::week(date),
    day_num   = lubridate::day(date)
  ) %>% 
  mutate(
    date_ym = as.Date(paste0(year, "-", month, "-01")),
  ) %>% 
  # filter(year %in% c(2015)) %>%
  group_by(segment) %>%
  mutate(
    end_flow  = lag(flow) + diversion - release,
    dflow     = flow - end_flow,
    end_flow  = zoo::na.locf(end_flow, fromLast = T),
    dflow     = zoo::na.locf(dflow, fromLast = T)
  ) %>% 
  ungroup() %>% 
  dplyr::select(segment,site, date, flow, end_flow, dflow)

# Replace NA/ Inf w/ Zeros
is.na(flows_day_ts)<-sapply(flows_day_ts, is.infinite)
flows_day_ts[is.na(flows_day_ts)] <- 0

# Save flow timeseries dataframes as RDS
path         <- here::here("data/boatable_days/")
flow_day_ts_path <- 'flow_day_timeseries.rds'

logger::log_info('Saving flow weekly timeseries data:\n{flow_day_ts_path}')
saveRDS(flows_day_ts, paste0(path, "/", flow_day_ts_path))

# ***********************************
# ---- Exploratory Summary plots ----
# ***********************************

# Plot difference in observed & simulated boatable days in years per segment
ggplot() +
  geom_point(data = na.omit(boatable_year), aes(x = dvolume, y = dboatable, col = dboatable_cat)) +
  facet_wrap(~segment) +
  labs(
    title  = "Yearly Difference in observed vs. simulated Boatable days",
    subtitle = "dboatable = observed boatable - simulated boatable \ndvolume = observed flow + (Diversions - Releases)",
    y      = "dboatable",
    x      = "dvolume",
    col = " "
  ) +
  scale_x_continuous(
    breaks = seq(-5000, 40000, by = 10000),
    limits = c(-5000, 40000)) +
  th

# Plot difference in observed & simulated boatable days in months per segment
ggplot() +
  geom_point(data = (na.omit(boatable_month) %>%  
               mutate(
                 dboatable_cat = case_when(
                   dboatable > 0   ~ "More boatable days observed",
                   dboatable == 0  ~ "Same boatable days observed/simulated",
                   dboatable < 0   ~ "More boatable days simulated"
                  ),
                 dboatable_cat = factor(dboatable_cat,
                                        levels = c(
                                           "More boatable days observed",
                                           "Same boatable days observed/simulated",
                                           "More boatable days simulated")))), 
             aes(x = dvolume, y = dboatable, col = dboatable_cat)) +
  facet_wrap(~segment) +
  labs(
    title  = "Monthly Difference in observed vs. simulated Boatable days",
    subtitle = "dboatable = observed boatable - simulated boatable \ndvolume = observed flow + (Diversions - Releases)",
    y      = "dboatable",
    x      = "dvolume",
    col = " "
  ) +
  scale_x_continuous(
    breaks = seq(-5000, 40000, by = 10000),
    limits = c(-5000, 40000)) +
  th

# Plot difference in observed & simulated daily boatable days per segment
ggplot() +
  geom_point(data = na.omit(boatable_day), aes(x = dvolume, y = dboatable, col = dboatable_cat)) +
  facet_wrap(~segment) +
  labs(
    title  = "Daily Difference in observed vs. simulated Boatable days",
    subtitle = "dboatable = observed boatable - simulated boatable \ndvolume = observed flow + (Diversions - Releases)",
    y      = "dboatable",
    x      = "dvolume",
    col = " "
  ) +
  th

# ********************************
# ---- Model Flow Time series ----
# ********************************

# Plot a single water year of model-estimated flow at Poudre Park as a time series (line label: flow at Poudre Park). 
# On the same set of axes, plot the reservoir diversion and releases as a stair-stepping line (line label: Net reservoir flow). Also, plot the naturalized flow at Poudre Park, which we calculate by adding/subtracting the reservoir diversion/releases from the model-estimated flow at Poudre Park (line label: naturalized flow at Poudre Park).

boatable_day <- boatable_day %>% 
  dplyr::relocate(segment, site, date) %>% 
  dplyr::relocate(segment, site, date, boatable_days, nat_boatable_days,
                  flow, nat_flow, diversion, release, dvolume, dboatable, dboatable_cat)

pp <- boatable_day %>% 
  filter(segment == "Poudre Park", date >= "2018-10-01", date <= "2019-10-01") %>% 
  dplyr::select(
    date,
    "Model Flow"             = flow, 
    "Naturalized Model Flow" = nat_flow,
    "Diversions"             = diversion,
    "Releases"               = release) %>% 
  # dplyr::select(date, flow, sim_flow, diversion, release) %>% 
  pivot_longer(
    cols = c(-date),
    names_to = "variable",
    values_to = "value"
  ) %>% 
  mutate(
    variable = factor(variable, levels = c(
      "Model Flow", "Naturalized Model Flow",  "Diversions", "Releases"))
    # "flow", "sim_flow", "diversion", "release"))
  )

pp_flow_plot <- 
  ggplot() +
  geom_line(data = pp, aes(x = date, y = value, col = variable), size = 1.2) +
  labs(
    title  = "Model flow, Naturalized Model Flow, Diversions & Releases @ Poudre Park",
    subtitle = "Naturalized Model Flow = Model flow + (Diversions - Releases)",
    y      = "Flow (CFS)",
    x      = "Date",
    col    = ""
  ) +
  scale_color_manual(values=c( "cyan3","darkblue", "red", "chartreuse3")) +
  th

pp_flow_plot

# Export plot
ggsave(
  paste0(plot_path,'/poudre_park_model_flow.png'),
  plot   = pp_flow_plot,
  width  = 46,
  height = 28, 
  units  = "cm"
)

# *****************************************
# ---- Change in monthly boatable days ----
# *****************************************

# Plot the change in monthly boatable days due to reservoir operations as a 3x4 facet plot. 
# Each plot should have a commonly scaled y-axis (axis label: Change in boatable days). X-axis is months of the year – April, May, June, July, August, September, October. Use bars to show the increase/decrease in boatable days attributable to reservoir operations. Positive bars indicate that reservoir operations increase boatable days, negative bars indicate that reservoir operations decrease boatable days. The title of each subplot should be the reach name, e.g. “Poudre Park”.

# Monthly mean change in boatable days w/ error bars representing min/max change in boatable days for the month
change_boatable_days <- boatable_month %>% 
  mutate(
    month     = lubridate::month(date, label = T)
  ) %>% 
  group_by(segment, month) %>% 
  summarize(
    min_dboatable         = min(dboatable, na.rm = T),
    max_dboatable         = max(dboatable, na.rm = T),
    nat_boatable_days     = mean(nat_boatable_days, na.rm = T), 
    boatable_days         = mean(boatable_days, na.rm = T),
    dboatable             = boatable_days - nat_boatable_days 
  ) %>% 
  mutate(month = factor(month))

change_boatable_days_plot <- 
  ggplot() +
    geom_bar(
      data = change_boatable_days, 
      aes(x    = month,
          y    = dboatable),
      position=position_dodge(), stat="identity") +
    geom_errorbar(
      data = change_boatable_days,
      aes(x    = month,
          y    = dboatable,
          ymin = min_dboatable,
          ymax = max_dboatable),
      width = .2) +
    facet_wrap(~segment) + 
    labs(
      title    = "Mean Monthly Difference in boatable days: Managed vs Naturalized flows",
      subtitle = "Change in boatable days = Managed boatable days - Naturalized boatable days\nError bars representing min/max change in boatable days for the month",
      y = "Change in boatable days",
      x = "Month"
    ) +
    th

change_boatable_days_plot

# Export plot
ggsave(
  paste0(plot_path,'/change_boatable_days.png'),
  plot   = change_boatable_days_plot,
  width  = 60,
  height = 28, 
  units  = "cm"
)

# *****************************************************
# ----Mean & STD of annual change in boatable days ----
# *****************************************************

# Plot the average (and standard deviation) of annual boatable days categorized by year type in a 3x4 facet plot. The x-axes will contain four year-type categories: dry, dry-typical, wet-typical and wet. The y-axis is the change in boatable days attributable to reservoir operations (axis label: Change in boatable days). Four bars per plot, color coded by year-type. Positive bars indicate that reservoir operations increase boatable days, negative bars indicate that reservoir operations decrease boatable days. Use vertical lines to indicate the interannual standard deviation in boatable days for a given year type on each bar.  The title of each subplot should be the reach name, e.g. “Poudre Park”.
# 


boatable_year_type <- boatable_year %>% 
  group_by(segment, year_type) %>% 
  summarize(
    min_dboatable         = min(dboatable, na.rm = T),
    max_dboatable         = max(dboatable, na.rm = T),
    avg_boatable_days     = mean(dboatable, na.rm = T),         # Average change in boatable days
    sd_boatable_days      = sd(dboatable, na.rm = T)  
  ) 

boatable_year_type_plot <- 
  ggplot() +
    geom_bar(
      data = boatable_year_type, 
      aes(x    = year_type,
          y    = avg_boatable_days,
          fill = year_type),
      position=position_dodge(), stat="identity") +
    geom_errorbar(
      data = boatable_year_type,
      aes(x    = year_type,
          y    = avg_boatable_days,
          ymin = min_dboatable,
          ymax = max_dboatable),
      width = .2) +
    scale_fill_manual(values=c("#d7191c", "#fdae61", "#2b83ba", "#abdda4")) +
    facet_wrap(~segment) +
    labs(
      title    = "Annual average change in boatable days by year type",
      subtitle = "Error bars representing min/max change in boatable days for the year type",
      y        = "Annual average change in boatable days",
      x        = "Year type",
      fill     = ""
    ) +
    th

boatable_year_type_plot

# Export plot
ggsave(
  paste0(plot_path,'/mean_annual_boatable_days.png'),
  plot   = boatable_year_type_plot,
  width  = 46,
  height = 28, 
  units  = "cm"
)

# **********************************************************************

# boatable days from observed flow
# boat_year <- boatable_days(
#   # flow_data      =  filter(flow, site == site.gages[[i]]),
#   flow_data      = filter(flow, 
#                           site == site_gages$site[i]) %>% 
#     dplyr::select(site, date, flow),
#   min_acceptable = min.acceptable,
#   max_acceptable = max.acceptable,
#   timescale      = "year"
# ) %>% 
#   mutate(
#     segment = site_gages$segment[i]
#   ) %>%
#   select(segment, year, year_type, boatable_days)
# # annual total flows to join w/ boatable days in year data
# flow_year <- flow %>% 
#   filter(site == site_gages$site[i])%>% 
#   mutate(year = lubridate::year(date)) %>%
#   group_by(year) %>%
#   summarize(
#     flow = sum(flow, na.rm = T)
#   ) %>% 
#   left_join(
#     sim_flow_year, 
#     by = "year"
#   ) %>% 
#   ungroup()
# 
# # boatable days from observed flow
# boat_year <- boatable_days(
#     # flow_data      =  filter(flow, site == site.gages[[i]]),
#     flow_data      = filter(flow, site == site_gages$site[i]),
#     min_acceptable = min.acceptable,
#     max_acceptable = max.acceptable,
#     timescale      = "year"
#     ) %>% 
#   mutate(
#     segment = site_gages$segment[i]
#   ) %>%
#   select(segment, year, year_type, boatable_days)
# 
# # boatable days from Simulated reservoir flows
# boat_sim_year <- boatable_days(
#       flow_data      = filter(sim_flow, site == site_gages$site[i]),
#       min_acceptable = min.acceptable,
#       max_acceptable = max.acceptable,
#       timescale      = "year"
#     ) %>% 
#   mutate(
#     segment = site_gages$segment[i]
#   ) %>%
#   select(segment, year, year_type, sim_boatable_days = boatable_days)
# 
# # join observed + simulated
# q_year <- boat_year %>% 
#   left_join( 
#     dplyr::select(boat_sim_year, year, sim_boatable_days),
#     by = "year"
#     ) %>% 
#   left_join(
#     flow_year,
#     by = "year"
#     ) %>% 
#   ungroup()
# 
# logger::log_info("Calculating boatable days in month @ {site_gages$segment[i]}")
# 
# # simulated monthly total flows to join with monthly boatable days
# sim_flow_month <- sim_flow %>% 
#   filter(site == site_gages$site[i])%>% 
#   mutate( 
#     year  = lubridate::year(date),
#     month = lubridate::month(date)
#     ) %>%
#   group_by(year, month) %>%
#   summarize(
#     sim_flow = sum(flow, na.rm = T)
#   ) %>% 
#   ungroup()
# 
# # monthly total flows to join with monthly boatable days
# flow_month <- flow %>% 
#   filter(site == site_gages$site[i])%>% 
#   mutate( 
#     year  = lubridate::year(date),
#     month = lubridate::month(date)
#   ) %>%
#   group_by(year, month) %>%
#   summarize(
#     flow = sum(flow, na.rm = T)
#   ) %>% 
#   ungroup() %>% 
#   left_join(
#     sim_flow_month, 
#     by = c("year", "month")
#   ) %>% 
#   ungroup() %>% 
#   mutate(date  = as.Date(paste0(year, "-", month, "-01"))) %>% 
#   dplyr::select(date, flow, sim_flow)
# 
# # boatable days from observed flow
# boat_month <- boatable_days(
#   # flow_data      =  filter(flow, site == site.gages[[i]]),
#   flow_data      = filter(flow, site == site_gages$site[i]),
#   min_acceptable = min.acceptable,
#   max_acceptable = max.acceptable,
#   timescale      = "month"
# ) %>% 
#   mutate(
#     segment = site_gages$segment[i]
#   ) %>%
#   select(segment, date, month_type, boatable_days)
# 
# # boatable days from Simulated reservoir flows
# boat_sim_month <- boatable_days(
#   flow_data      = filter(sim_flow, site == site_gages$site[i]),
#   min_acceptable = min.acceptable,
#   max_acceptable = max.acceptable,
#   timescale      = "month"
# ) %>% 
#   mutate(
#     segment = site_gages$segment[i]
#   ) %>%
#   select(segment, date, month_type,  sim_boatable_days = boatable_days)
# 
# # join observed + simulated
# q_month <- boat_month %>%    
#   ungroup() %>% 
#   left_join( 
#     dplyr::select(boat_sim_month, date, sim_boatable_days),
#     by = "date"
#   ) %>% 
#   left_join(
#     flow_month,
#     by = "date"
#   )
# 
# logger::log_info("Calculating boatable days @ {site_gages$segment[i]}")
# 
# # simulated daily total flows to join with monthly boatable days
# sim_flow_day <- sim_flow %>% 
#   filter(site == site_gages$site[i]) %>% 
#   rename(sim_flow = flow)
# 
# # daily total flows to join with monthly boatable days
# flow_day <- flow %>% 
#   filter(site == site_gages$site[i]) %>% 
#   left_join(
#     dplyr::select(sim_flow_day, -site), 
#     by = "date") %>% 
#   ungroup() %>% 
#   dplyr::select(date, flow, sim_flow)
# 
# # boatable days from observed flow
# boat_day <- boatable_days(
#   flow_data      = filter(flow, site == site_gages$site[i]),
#   min_acceptable = min.acceptable,
#   max_acceptable = max.acceptable,
#   timescale      = "day"
# ) %>% 
#   mutate(
#     segment = site_gages$segment[i]
#   ) %>%
#   select(segment, date, boatable_days)
# 
# # boatable days from Simulated reservoir flows
# boat_sim_day <- boatable_days(
#   flow_data      = filter(sim_flow, site == site_gages$site[i]),
#   min_acceptable = min.acceptable,
#   max_acceptable = max.acceptable,
#   timescale      = "day"
# ) %>% 
#   mutate(
#     segment = site_gages$segment[i]
#   ) %>%
#   select(segment, date, sim_boatable_days = boatable_days)
# 
# # join observed + simulated
# q_day <- boat_day %>% 
#   left_join( 
#     dplyr::select(boat_sim_day, date, sim_boatable_days),
#     by = "date"
#   ) %>% 
#   left_join(
#     flow_day,
#     by = "date"
#   ) 
# logger::log_info("Plotting annual boatable days @ {site_gages$segment[i]}")

# pivot data long for plotting
# q_year_long <- q_year %>% 
#   rename("Boatable Days" = boatable_days, "Simulated Boatable Days" = sim_boatable_days) %>% 
#   pivot_longer(cols = c("Boatable Days", "Simulated Boatable Days"))
# 
# # create a line chart of boatable days & simulated boatable days for this site
# boatable_days_plot <- 
#   ggplot() +
#     geom_line(data = q_year_long, aes(x = year, y = value, col = name), size = 1.5) +
#     geom_point(data = q_year_long, aes(x = year, y = value, col = name), size = 3) +
#     # geom_col(data = q_year_long, aes(x = year, y = value, fill = year_type)) +
#     # facet_wrap(~name) +
#     labs(
#       title  = paste('Boatable Days at', site_gages$segment[i]),
#       subtitle = "Simulated Boatable days calculated using simulated flows\nSimulated flows = Pineview flow model - (Diversions - releases)",
#       y      = "Annual Number of Boatable Days",
#       x      = "Calendar Year",
#       col    = ""
#     ) +
#     scale_y_continuous(
#       breaks = seq(0, 150, by = 10),
#       limits = c(0, 150)
#       ) +
#     # scale_fill_manual(values=c("#d7191c", "#fdae61", "#2b83ba", "#abdda4")) +
#     th
#   
# # save ggplot to "aw-poudre-2020/boatable_days/boatable_day_plots/"
# filename <- paste0(gsub(" ","_",  tolower(site_gages$segment[i])), '_boatable_days.png')
# 
# # Export plot
# ggsave(
#   paste0(plot_path,'/', filename),
#   plot   = boatable_days_plot,
#   width  = 46,
#   height = 28, 
#   units  = "cm"
# )
# # ggsave(paste0(plot_path,'/', filename))
# 
# # iterativly add boatable days dataframes to lists (year, month, and days)
# boatable_year_lst[[i]]  <- q_year
# boatable_month_lst[[i]] <- q_month
# boatable_day_lst[[i]]   <- q_day
# 
# 
# 
# rm(q_year, q_month, q_day, boat_sim_day, boat_sim_month,
#    boat_sim_year, boat_year, boat_day, flow_day, sim_flow_day, 
#    flow_month, sim_flow_month, flow_year, sim_flow_year, boat_month,
#    q_year_long
#    )
# }


# Yearly boatable days 
boatable_year  <- bind_rows(boatable_year_lst) %>% 
  mutate(
    dboatable = nat_boatable_days - boatable_days
  )

# Monthly boatable days 
boatable_month <- bind_rows(boatable_month_lst) %>% 
  mutate(
    dboatable  = nat_boatable_days - boatable_days
  ) %>%
  mutate(
    dboatable_cat = case_when(
      dboatable < 0   ~ "More boatable days observed",
      dboatable == 0  ~ "Same boatable days observed/simulated",
      dboatable > 0   ~ "More boatable days simulated"
    ),
    dboatable_cat = factor(dboatable_cat,
                           levels = c(
                             "More boatable days observed",
                             "Same boatable days observed/simulated",
                             "More boatable days simulated"))
  )

# Plot difference in observed & simulated boatable days in months per segment
ggplot() +
  geom_point(data = na.omit(boatable_month), aes(x = dvolume, y = dboatable, col = dboatable_cat)) +
  facet_wrap(~segment) +
  labs(
    title  = "Monthly Difference in observed vs. simulated Boatable days",
    subtitle = "dboatable = observed boatable - simulated boatable\ndvolume = observed flow - simulated flow",
    y      = "dboatable",
    x      = "dvolume",
    col = " "
  ) +
  scale_x_continuous(
    breaks = seq(-5000, 40000, by = 10000),
    limits = c(-5000, 40000)) +
  th

# Daily boatable days 
boatable_day   <- bind_rows(boatable_day_lst) %>% 
  mutate(
    dboatable = nat_boatable_days - boatable_days
  ) %>%
  mutate(
    dboatable_cat = case_when(
      dboatable < 0   ~ "More boatable days observed",
      dboatable == 0  ~ "Same boatable days observed/simulated",
      dboatable > 0   ~ "More boatable days simulated"
    ),
    dboatable_cat = factor(dboatable_cat,
                           levels = c(
                             "More boatable days observed",
                             "Same boatable days observed/simulated",
                             "More boatable days simulated"))
  ) 

# Plot difference in observed & simulated daily boatable days per segment
ggplot() +
  geom_point(data = na.omit(boatable_day), aes(x = dvolume, y = dboatable, col = dboatable_cat)) +
  facet_wrap(~segment) +
  labs(
    title  = "Daily Difference in observed vs. simulated Boatable days",
    subtitle = "dboatable = observed boatable - simulated boatable\ndvolume = observed flow - simulated flow",
    y      = "dboatable",
    x      = "dvolume",
    col = " "
  ) +
  th

# ********************************
# ---- Save Boatable days RDS ----
# ********************************

# save data to disk as RDS
path     <- here::here("data/boatable_days/")
filename1 <- 'boatable_days_year.rds'
filename2 <- 'boatable_days_month.rds'
filename3 <- 'boatable_days_day.rds'

logger::log_info(
  'Saving yearly/monthly/daily boatable days data:\n{filename1}\n{filename2}\n{filename3}'
)

saveRDS(boatable_year, paste0(path, "/", filename1))
saveRDS(boatable_month, paste0(path, "/", filename2))
saveRDS(boatable_day, paste0(path, "/", filename3))




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
site_gages <- data.frame(
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
) %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "segment", 
    values_to = "site"
    ) %>% 
  mutate(
    # segment = stringr::str_replace_all(segment,"\\.","_")
    segment = stringr::str_replace_all(segment,"\\."," ")
  )
  
# load flow preference data
flow_pref <- read_csv(here::here('data','flow_pref_by_reach.csv'))

# look for these data to be saved to disk first, before downloading and saving.
# Retrieve historical flow observations and models
#===========================
# Load tidy'd survey data

usgs_file <- here::here(
  "data/gauge", 
  "pourde_whitewater_park_usgs_station_flow.rds"
)

# Import data
if (file.exists(usgs_file)) {
  flow.usgs  <-readRDS(file = usgs_file)
} else {
  logger::log_error(
    'Downloading Poudre Whitewater park USGS gauge data \nCould not locate \n{usgs_file}'
  )
  # Poudre Whitewater Park
  flow.usgs <- dataRetrieval::readNWISdv(
    siteNumbers = '06752260', 
    parameterCd = '00060',
    startDate   = '1976-01-01',
    endDate     = '2022-03-16'
  ) %>%
    rename(flow = 'X_00060_00003', date = Date) %>%
    mutate(site = 'USGS_06752260') %>%
    select(date, flow, site)
  
  # save to disk
  saveRDS(flow.usgs, usgs_file)
  stop()
}

# Filter plant file
CLAFTCCO_file <- here::here(
  "data/gauge", 
  "CLAFTCCO_station_flow.RDS"
  )

# Import data
if (file.exists(CLAFTCCO_file)) {
  flow.CLAFTCCO  <-readRDS(file = CLAFTCCO_file) %>% 
    mean_flow(., date) %>%
    mutate(site = 'CLAFTCCO')
} else {
  logger::log_error(
    'Downloading CLAFTCCO gauge data \nCould not locate \n{CLAFTCCO_file}'
  )
  # Filter plant
  flow.CLAFTCCO <- GetCDSSStationFlow(
    site_abbrev = 'CLAFTCCO',
    save.data = TRUE) %>% 
    mean_flow(., date) %>%
    mutate(site = 'CLAFTCCO')
  stop()
}

# Big South file
LAPLODCO_file <- here::here(
  "data/gauge", 
  "LAPLODCO_station_flow.RDS"
)

# Import data
if (file.exists(LAPLODCO_file)) {
  flow.LAPLODCO  <-readRDS(file = LAPLODCO_file) %>% 
    mean_flow(., date) %>%
    mutate(site = 'LAPLODCO')
} else {
  logger::log_error(
    'Downloading LAPLODCO gauge data \nCould not locate \n{LAPLODCO_file}'
  )
  # Big South
  flow.LAPLODCO <- GetCDSSStationFlow(
    site_abbrev = 'LAPLODCO',
    save.data = TRUE) %>% 
    mean_flow(., date) %>%
    mutate(site = 'LAPLODCO')
  stop()
}

# Flow preference data
flowpref.file  <- here::here(
  "private_data", 
  "flow-pref-data_20220105.RDS"
  # "flow-pref-data_20200630.RDS"
)

# Pineview flow model
flow_model <- readRDS(
  here::here('boatable_days','simulated_historical_pineview_flow.RDS')
  ) %>%
  mutate(site = 'Pineview model')

# simulated flow model using Reservoir flows
# sim_flow_model <- readRDS(
#   here::here('reservoirs','simulated_natural_flow_pineview_flow.rds')
# ) %>%
#   dplyr::select(date, flow = sim_nat) %>% 
#   mutate(site = 'Pineview model')

# simulated flow model using Reservoir flows
reservoir_flows <- readRDS(
  here::here('reservoirs','reservoir_flows_daily.rds')
) %>%
  pivot_wider(
    id_cols     = c(structure, flow_type, date),
    names_from  = c(flow_type, structure),
    values_from = flow
    # id_cols     = c(structure, flow_type, date),
    # names_from  = "structure",
    # names_glue  = "{structure}_{.value}",
    # values_from = c(flow),
    # values_fn   = mean
  ) 
  # mutate(site = 'Pineview model')


# join all flow records together
flow <- flow.usgs %>%
  rbind(flow.LAPLODCO) %>%
  rbind(flow.CLAFTCCO) %>%
  rbind(flow_model) %>% 
  left_join(
    reservoir_flows,
    by = "date"
  ) %>% 
  tibble() %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    nat_flow = case_when(
      site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (flow + dvolume_long_draw + dvolume_chambers + dvolume_joe_wright + 
                                                                            dvolume_peterson + dvolume_barnes_meadow),
      site == "LAPLODCO"                                          ~ (flow + dvolume_long_draw + dvolume_peterson)
      ),
    diversion = case_when(
      site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (diversion_long_draw + diversion_chambers + diversion_joe_wright + 
                                                                       diversion_peterson + diversion_barnes_meadow),
      site == "LAPLODCO"                                          ~ (diversion_long_draw + diversion_peterson)
    ),
    release = case_when(
      site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (release_long_draw + release_chambers + release_joe_wright + 
                                                                       release_peterson + release_barnes_meadow),
      site == "LAPLODCO"                                          ~ (release_long_draw + release_peterson)
    ),
    dvolume = case_when(
      site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (dvolume_long_draw + dvolume_chambers + dvolume_joe_wright + 
                                                                       dvolume_peterson + dvolume_barnes_meadow),
      site == "LAPLODCO"                                          ~ (dvolume_long_draw + dvolume_peterson)
    )
  ) %>% 
  dplyr::relocate(site, date, flow, nat_flow, diversion, release, dvolume)
  # dplyr::select(site, date, flow, nat_flow, diversion, release, dvolume)

# clear individual records from memory
rm(flow.usgs, flow.CLAFTCCO, flow.LAPLODCO, flow_model)

# ********************************************
# ---- Loop through each segment of river ----
# ********************************************

boatable_year_lst   <- list()
boatable_month_lst  <- list()
boatable_day_lst    <- list()

for (i in 1:nrow(site_gages)) {

  site_preference <- flow_pref %>%
    filter(segment == site_gages$segment[i])
  
  logger::log_info("Calculating boatable days in year @ {site_gages$segment[i]}")
  
  min.acceptable = site_preference$minimally.acceptable.flow[1]
  max.acceptable = site_preference$maximally.acceptable.flow[1]
  
  if (is.na(max.acceptable)) {
    max.acceptable = 100000000000
  }
  
  # simulated total flows to join w/ boatable days in year data
  # sim_flow_year <- sim_flow %>% 
  #   filter(site == site_gages$site[i])%>% 
  #   mutate(year = lubridate::year(date)) %>%
  #   group_by(year) %>%
  #   summarize(
  #     sim_flow = sum(flow, na.rm = T)
  #   ) %>% 
  #   ungroup()
  flow_year <- flow %>% 
    filter(site == site_gages$site[i]) %>% 
    mutate(year = lubridate::year(date)) %>%
    group_by(year) %>%
    summarize(
      flow       = sum(flow, na.rm = T),
      nat_flow   = sum(nat_flow, na.rm = T),
      diversion  = sum(diversion, na.rm = T),
      release    = sum(release, na.rm = T),
      dvolume    = sum(dvolume, na.rm = T)
    ) %>% 
    ungroup()
  
  # simulated monthly total flows to join with monthly boatable days
  flow_month <- flow %>% 
    filter(site == site_gages$site[i])%>% 
    mutate( 
      year  = lubridate::year(date),
      month = lubridate::month(date)
    ) %>%
    group_by(year, month) %>%
    summarize(
      flow       = sum(flow, na.rm = T),
      nat_flow   = sum(nat_flow, na.rm = T),
      diversion  = sum(diversion, na.rm = T),
      release    = sum(release, na.rm = T),
      dvolume    = sum(dvolume, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(date  = as.Date(paste0(year, "-", month, "-01"))) %>% 
    dplyr::select(date, flow, nat_flow, diversion, release, dvolume)
  
  
  # daily total flows to join with monthly boatable days
  flow_day <- flow %>% 
    filter(site == site_gages$site[i]) %>% 
    # left_join(
    #   dplyr::select(sim_flow_day, -site), 
    #   by = "date") %>% 
    ungroup() %>% 
    dplyr::select(date, flow, nat_flow, diversion, release, dvolume)
  
  # Annual boatable days - Observed Flows
  boat_year <- boatable_days(
    # flow_data      =  filter(flow, site == site.gages[[i]]),
    flow_data      = filter(flow, site == site_gages$site[i]) %>% 
                          dplyr::select(site, date, flow),
    min_acceptable = min.acceptable,
    max_acceptable = max.acceptable,
    timescale      = "year") %>%    
    mutate(
      segment = site_gages$segment[i]
    ) %>%
    select(segment, year, year_type, boatable_days)
  
  # Annual boatable days - Naturalized Flows
  boat_nat_year <- boatable_days(
    # flow_data      =  filter(flow, site == site.gages[[i]]),
    flow_data      = filter(flow, site == site_gages$site[i]) %>% 
      dplyr::select(site, date, flow = nat_flow),
    min_acceptable = min.acceptable,
    max_acceptable = max.acceptable,
    timescale      = "year") %>%    
    mutate(
      segment = site_gages$segment[i]
    ) %>%
    select(segment, year, year_type, nat_boatable_days = boatable_days)
  
  # join observed + simulated
  q_year <- boat_year %>% 
    left_join( 
      dplyr::select(boat_nat_year, year, nat_boatable_days),
      by = "year"
    ) %>% 
    left_join(
      flow_year,
      by = "year"
    ) %>% 
    ungroup()
  
   logger::log_info("Calculating boatable days in month @ {site_gages$segment[i]}")
   
   # Annual boatable days - Observed Flows
   boat_month <- boatable_days(
     # flow_data      =  filter(flow, site == site.gages[[i]]),
     flow_data      = filter(flow, site == site_gages$site[i]) %>% 
       dplyr::select(site, date, flow),
     min_acceptable = min.acceptable,
     max_acceptable = max.acceptable,
     timescale      = "month") %>%    
     mutate(
       segment = site_gages$segment[i]
     ) %>%
     select(segment, date, month_type, boatable_days)
   
   # Annual boatable days - Naturalized Flows
   boat_nat_month <- boatable_days(
     # flow_data      =  filter(flow, site == site.gages[[i]]),
     flow_data      = filter(flow, site == site_gages$site[i]) %>% 
       dplyr::select(site, date, flow = nat_flow),
     min_acceptable = min.acceptable,
     max_acceptable = max.acceptable,
     timescale      = "month") %>%    
     mutate(
       segment = site_gages$segment[i]
     ) %>%
     select(segment, date, month_type, nat_boatable_days = boatable_days)
   
   # join observed + simulated
   q_month <- boat_month %>% 
     left_join( 
       dplyr::select(boat_nat_month, date, nat_boatable_days),
       by = "date"
     ) %>% 
     left_join(
       flow_month,
       by = "date"
     ) %>% 
     ungroup()
   
   logger::log_info("Calculating boatable days @ {site_gages$segment[i]}")
   
   # boatable days from observed flow
   boat_day <- boatable_days(
     flow_data      = filter(flow, site == site_gages$site[i]) %>% 
       dplyr::select(site, date, flow),
     min_acceptable = min.acceptable,
     max_acceptable = max.acceptable,
     timescale      = "day"
   ) %>% 
     mutate(
       segment = site_gages$segment[i]
     ) %>%
     select(segment, date, boatable_days)
   
   # boatable days from Simulated reservoir flows
   boat_nat_day <- boatable_days(
     flow_data      = filter(flow, site == site_gages$site[i]) %>% 
       dplyr::select(site, date, flow = nat_flow),
     min_acceptable = min.acceptable,
     max_acceptable = max.acceptable,
     timescale      = "day"
   ) %>% 
     mutate(
       segment = site_gages$segment[i]
     ) %>%
     select(segment, date, nat_boatable_days = boatable_days)
   
   # join observed + simulated
   q_day <- boat_day %>% 
     left_join( 
       dplyr::select(boat_nat_day, date, nat_boatable_days),
       by = "date"
     ) %>% 
     left_join(
       flow_day,
       by = "date"
     ) 
   
   # simulated daily total flows to join with monthly boatable days
   # sim_flow_day <- sim_flow %>% 
   #   filter(site == site_gages$site[i]) %>% 
   #   rename(sim_flow = flow)
   # 
   # # daily total flows to join with monthly boatable days
   # flow_day <- flow %>% 
   #   filter(site == site_gages$site[i]) %>% 
   #   left_join(
   #     dplyr::select(sim_flow_day, -site), 
   #     by = "date") %>% 
   #   ungroup() %>% 
   #   dplyr::select(date, flow, sim_flow)
   # 
 
   logger::log_info("Plotting annual boatable days @ {site_gages$segment[i]}")
   
   # pivot data long for plotting
   q_year_long <- q_year %>% 
     rename("Boatable Days" = boatable_days, "Naturalized Boatable Days" = nat_boatable_days) %>% 
     pivot_longer(cols = c("Boatable Days", "Naturalized Boatable Days"))
   
   # create a line chart of boatable days & simulated boatable days for this site
   boatable_days_plot <- 
     ggplot() +
       geom_line(data = q_year_long, aes(x = year, y = value, col = name), size = 1.5) +
       geom_point(data = q_year_long, aes(x = year, y = value, col = name), size = 3) +
       # geom_col(data = q_year_long, aes(x = year, y = value, fill = year_type)) +
       # facet_wrap(~name) +
       labs(
         title  = paste('Boatable Days at', site_gages$segment[i]),
         subtitle = "Naturalized Boatable days calculated using simulated flows\nNaturalized flows = Pineview flow model - (Diversions - releases)",
         y      = "Annual Number of Boatable Days",
         x      = "Calendar Year",
         col    = ""
       ) +
       scale_y_continuous(
         breaks = seq(0, 150, by = 10),
         limits = c(0, 150)
       ) +
       # scale_fill_manual(values=c("#d7191c", "#fdae61", "#2b83ba", "#abdda4")) +
       th
   
   # save ggplot to "aw-poudre-2020/boatable_days/boatable_day_plots/"
   filename <- paste0(gsub(" ","_",  tolower(site_gages$segment[i])), '_boatable_days.png')
   
   # Export plot
   ggsave(
     paste0(plot_path,'/', filename),
     plot   = boatable_days_plot,
     width  = 46,
     height = 28, 
     units  = "cm"
   )
   # ggsave(paste0(plot_path,'/', filename))
   
   # iterativly add boatable days dataframes to lists (year, month, and days)
   boatable_year_lst[[i]]  <- q_year
   boatable_month_lst[[i]] <- q_month
   boatable_day_lst[[i]]   <- q_day
   
   
   
   rm(q_year, q_month, q_day, 
      boat_nat_day, boat_nat_month, boat_nat_year, 
      boat_day, boat_month, boat_year, 
      flow_day, flow_month, flow_year, q_year_long
   )
  # boatable days from observed flow
  # boat_year <- boatable_days(
  #   # flow_data      =  filter(flow, site == site.gages[[i]]),
  #   flow_data      = filter(flow, 
  #                           site == site_gages$site[i]) %>% 
  #     dplyr::select(site, date, flow),
  #   min_acceptable = min.acceptable,
  #   max_acceptable = max.acceptable,
  #   timescale      = "year"
  # ) %>% 
  #   mutate(
  #     segment = site_gages$segment[i]
  #   ) %>%
  #   select(segment, year, year_type, boatable_days)
  # # annual total flows to join w/ boatable days in year data
  # flow_year <- flow %>% 
  #   filter(site == site_gages$site[i])%>% 
  #   mutate(year = lubridate::year(date)) %>%
  #   group_by(year) %>%
  #   summarize(
  #     flow = sum(flow, na.rm = T)
  #   ) %>% 
  #   left_join(
  #     sim_flow_year, 
  #     by = "year"
  #   ) %>% 
  #   ungroup()
  # 
  # # boatable days from observed flow
  # boat_year <- boatable_days(
  #     # flow_data      =  filter(flow, site == site.gages[[i]]),
  #     flow_data      = filter(flow, site == site_gages$site[i]),
  #     min_acceptable = min.acceptable,
  #     max_acceptable = max.acceptable,
  #     timescale      = "year"
  #     ) %>% 
  #   mutate(
  #     segment = site_gages$segment[i]
  #   ) %>%
  #   select(segment, year, year_type, boatable_days)
  # 
  # # boatable days from Simulated reservoir flows
  # boat_sim_year <- boatable_days(
  #       flow_data      = filter(sim_flow, site == site_gages$site[i]),
  #       min_acceptable = min.acceptable,
  #       max_acceptable = max.acceptable,
  #       timescale      = "year"
  #     ) %>% 
  #   mutate(
  #     segment = site_gages$segment[i]
  #   ) %>%
  #   select(segment, year, year_type, sim_boatable_days = boatable_days)
  # 
  # # join observed + simulated
  # q_year <- boat_year %>% 
  #   left_join( 
  #     dplyr::select(boat_sim_year, year, sim_boatable_days),
  #     by = "year"
  #     ) %>% 
  #   left_join(
  #     flow_year,
  #     by = "year"
  #     ) %>% 
  #   ungroup()
  # 
  # logger::log_info("Calculating boatable days in month @ {site_gages$segment[i]}")
  # 
  # # simulated monthly total flows to join with monthly boatable days
  # sim_flow_month <- sim_flow %>% 
  #   filter(site == site_gages$site[i])%>% 
  #   mutate( 
  #     year  = lubridate::year(date),
  #     month = lubridate::month(date)
  #     ) %>%
  #   group_by(year, month) %>%
  #   summarize(
  #     sim_flow = sum(flow, na.rm = T)
  #   ) %>% 
  #   ungroup()
  # 
  # # monthly total flows to join with monthly boatable days
  # flow_month <- flow %>% 
  #   filter(site == site_gages$site[i])%>% 
  #   mutate( 
  #     year  = lubridate::year(date),
  #     month = lubridate::month(date)
  #   ) %>%
  #   group_by(year, month) %>%
  #   summarize(
  #     flow = sum(flow, na.rm = T)
  #   ) %>% 
  #   ungroup() %>% 
  #   left_join(
  #     sim_flow_month, 
  #     by = c("year", "month")
  #   ) %>% 
  #   ungroup() %>% 
  #   mutate(date  = as.Date(paste0(year, "-", month, "-01"))) %>% 
  #   dplyr::select(date, flow, sim_flow)
  # 
  # # boatable days from observed flow
  # boat_month <- boatable_days(
  #   # flow_data      =  filter(flow, site == site.gages[[i]]),
  #   flow_data      = filter(flow, site == site_gages$site[i]),
  #   min_acceptable = min.acceptable,
  #   max_acceptable = max.acceptable,
  #   timescale      = "month"
  # ) %>% 
  #   mutate(
  #     segment = site_gages$segment[i]
  #   ) %>%
  #   select(segment, date, month_type, boatable_days)
  # 
  # # boatable days from Simulated reservoir flows
  # boat_sim_month <- boatable_days(
  #   flow_data      = filter(sim_flow, site == site_gages$site[i]),
  #   min_acceptable = min.acceptable,
  #   max_acceptable = max.acceptable,
  #   timescale      = "month"
  # ) %>% 
  #   mutate(
  #     segment = site_gages$segment[i]
  #   ) %>%
  #   select(segment, date, month_type,  sim_boatable_days = boatable_days)
  # 
  # # join observed + simulated
  # q_month <- boat_month %>%    
  #   ungroup() %>% 
  #   left_join( 
  #     dplyr::select(boat_sim_month, date, sim_boatable_days),
  #     by = "date"
  #   ) %>% 
  #   left_join(
  #     flow_month,
  #     by = "date"
  #   )
  # 
  # logger::log_info("Calculating boatable days @ {site_gages$segment[i]}")
  # 
  # # simulated daily total flows to join with monthly boatable days
  # sim_flow_day <- sim_flow %>% 
  #   filter(site == site_gages$site[i]) %>% 
  #   rename(sim_flow = flow)
  # 
  # # daily total flows to join with monthly boatable days
  # flow_day <- flow %>% 
  #   filter(site == site_gages$site[i]) %>% 
  #   left_join(
  #     dplyr::select(sim_flow_day, -site), 
  #     by = "date") %>% 
  #   ungroup() %>% 
  #   dplyr::select(date, flow, sim_flow)
  # 
  # # boatable days from observed flow
  # boat_day <- boatable_days(
  #   flow_data      = filter(flow, site == site_gages$site[i]),
  #   min_acceptable = min.acceptable,
  #   max_acceptable = max.acceptable,
  #   timescale      = "day"
  # ) %>% 
  #   mutate(
  #     segment = site_gages$segment[i]
  #   ) %>%
  #   select(segment, date, boatable_days)
  # 
  # # boatable days from Simulated reservoir flows
  # boat_sim_day <- boatable_days(
  #   flow_data      = filter(sim_flow, site == site_gages$site[i]),
  #   min_acceptable = min.acceptable,
  #   max_acceptable = max.acceptable,
  #   timescale      = "day"
  # ) %>% 
  #   mutate(
  #     segment = site_gages$segment[i]
  #   ) %>%
  #   select(segment, date, sim_boatable_days = boatable_days)
  # 
  # # join observed + simulated
  # q_day <- boat_day %>% 
  #   left_join( 
  #     dplyr::select(boat_sim_day, date, sim_boatable_days),
  #     by = "date"
  #   ) %>% 
  #   left_join(
  #     flow_day,
  #     by = "date"
  #   ) 
  # logger::log_info("Plotting annual boatable days @ {site_gages$segment[i]}")
  
  # pivot data long for plotting
  # q_year_long <- q_year %>% 
  #   rename("Boatable Days" = boatable_days, "Simulated Boatable Days" = sim_boatable_days) %>% 
  #   pivot_longer(cols = c("Boatable Days", "Simulated Boatable Days"))
  # 
  # # create a line chart of boatable days & simulated boatable days for this site
  # boatable_days_plot <- 
  #   ggplot() +
  #     geom_line(data = q_year_long, aes(x = year, y = value, col = name), size = 1.5) +
  #     geom_point(data = q_year_long, aes(x = year, y = value, col = name), size = 3) +
  #     # geom_col(data = q_year_long, aes(x = year, y = value, fill = year_type)) +
  #     # facet_wrap(~name) +
  #     labs(
  #       title  = paste('Boatable Days at', site_gages$segment[i]),
  #       subtitle = "Simulated Boatable days calculated using simulated flows\nSimulated flows = Pineview flow model - (Diversions - releases)",
  #       y      = "Annual Number of Boatable Days",
  #       x      = "Calendar Year",
  #       col    = ""
  #     ) +
  #     scale_y_continuous(
  #       breaks = seq(0, 150, by = 10),
  #       limits = c(0, 150)
  #       ) +
  #     # scale_fill_manual(values=c("#d7191c", "#fdae61", "#2b83ba", "#abdda4")) +
  #     th
  #   
  # # save ggplot to "aw-poudre-2020/boatable_days/boatable_day_plots/"
  # filename <- paste0(gsub(" ","_",  tolower(site_gages$segment[i])), '_boatable_days.png')
  # 
  # # Export plot
  # ggsave(
  #   paste0(plot_path,'/', filename),
  #   plot   = boatable_days_plot,
  #   width  = 46,
  #   height = 28, 
  #   units  = "cm"
  # )
  # # ggsave(paste0(plot_path,'/', filename))
  # 
  # # iterativly add boatable days dataframes to lists (year, month, and days)
  # boatable_year_lst[[i]]  <- q_year
  # boatable_month_lst[[i]] <- q_month
  # boatable_day_lst[[i]]   <- q_day
  # 
  # 
  # 
  # rm(q_year, q_month, q_day, boat_sim_day, boat_sim_month,
  #    boat_sim_year, boat_year, boat_day, flow_day, sim_flow_day, 
  #    flow_month, sim_flow_month, flow_year, sim_flow_year, boat_month,
  #    q_year_long
  #    )
}


# Yearly boatable days 
boatable_year  <- bind_rows(boatable_year_lst) %>% 
  mutate(
    dvolume   = flow - sim_flow, 
    dboatable = sim_boatable_days - boatable_days
    )

# Monthly boatable days 
boatable_month <- bind_rows(boatable_month_lst) %>% 
  mutate(
    dvolume   = flow - sim_flow,
    dboatable = sim_boatable_days - boatable_days
  ) %>%
  mutate(
    dboatable_cat = case_when(
      dboatable > 0   ~ "More boatable days observed",
      dboatable == 0  ~ "Same boatable days observed/simulated",
      dboatable < 0   ~ "More boatable days simulated"
    ),
    dboatable_cat = factor(dboatable_cat,
                           levels = c(
                             "More boatable days observed",
                             "Same boatable days observed/simulated",
                             "More boatable days simulated"))
  )
  
# Plot difference in observed & simulated boatable days in months per segment
ggplot() +
  geom_point(data = na.omit(boatable_month), aes(x = dvolume, y = dboatable, col = dboatable_cat)) +
  facet_wrap(~segment) +
  labs(
    title  = "Monthly Difference in observed vs. simulated Boatable days",
    subtitle = "dboatable = observed boatable - simulated boatable\ndvolume = observed flow - simulated flow",
    y      = "dboatable",
    x      = "dvolume",
    col = " "
  ) +
  scale_x_continuous(
    breaks = seq(-5000, 40000, by = 10000),
    limits = c(-5000, 40000)) +
  th

# Daily boatable days 
boatable_day   <- bind_rows(boatable_day_lst) %>% 
  mutate(
    dvolume   = flow - sim_flow,
    dboatable = sim_boatable_days - boatable_days
    ) %>%
  mutate(
    dboatable_cat = case_when(
      dboatable > 0   ~ "More boatable days observed",
      dboatable == 0  ~ "Same boatable days observed/simulated",
      dboatable < 0   ~ "More boatable days simulated"
    ),
    dboatable_cat = factor(dboatable_cat,
                           levels = c(
                             "More boatable days observed",
                             "Same boatable days observed/simulated",
                             "More boatable days simulated"))
  ) 

# Plot difference in observed & simulated daily boatable days per segment
ggplot() +
  geom_point(data = na.omit(boatable_day), aes(x = dvolume, y = dboatable, col = dboatable_cat)) +
  facet_wrap(~segment) +
  labs(
    title  = "Daily Difference in observed vs. simulated Boatable days",
    subtitle = "dboatable = observed boatable - simulated boatable\ndvolume = observed flow - simulated flow",
    y      = "dboatable",
    x      = "dvolume",
    col = " "
  ) +
  th

# ********************************
# ---- Save Boatable days RDS ----
# ********************************

# save data to disk as RDS
path     <- here::here("data/boatable_days/")
filename1 <- 'boatable_days_year.rds'
filename2 <- 'boatable_days_month.rds'
filename3 <- 'boatable_days_day.rds'

logger::log_info(
  'Saving yearly/monthly/daily boatable days data:\n{filename1}\n{filename2}\n{filename3}'
)

saveRDS(boatable_year, paste0(path, "/", filename1))
saveRDS(boatable_month, paste0(path, "/", filename2))
saveRDS(boatable_day, paste0(path, "/", filename3))


