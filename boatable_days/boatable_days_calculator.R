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
    names_to  = "site_name", 
    values_to = "site"
    ) %>% 
  mutate(
    # site_name = stringr::str_replace_all(site_name,"\\.","_")
    site_name = stringr::str_replace_all(site_name,"\\."," ")
  )
  
# load flow preference data
flow_pref <- read_csv(here::here('data','flow_pref_by_reach.csv'))

# TODO: look for these data to be saved to disk first, before downloading and saving.
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
sim_flow_model <- readRDS(
  here::here('reservoirs','simulated_natural_flow_pineview_flow.rds')
) %>%
  dplyr::select(date, flow = sim_nat) %>% 
  mutate(site = 'Pineview model')

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
names(flow)[4:18]
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
  # pivot_longer(cols = c(-date, -flow, -site)) %>% 
  # cleaner::na_replace(value) %>% 
  # pivot_wider(
  #   id_cols = c(date, flow, name),
  #   names_from = name,
  #   values_from = value
  # )
  # filter(site == "Pineview model") %>% 
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
  dplyr::select(site, date, flow, nat_flow, diversion, release, dvolume)
  # dplyr::relocate(site, date, flow, nat_flow, diversion, release, dvolume)

 # join all flow records together (simulated)
sim_flow <- flow.usgs %>%
  rbind(flow.LAPLODCO) %>%
  rbind(flow.CLAFTCCO) %>%
  rbind(sim_flow_model)

# clear individual records from memory
rm(flow.usgs, flow.CLAFTCCO, flow.LAPLODCO, flow_model)

# ********************************************
# ---- Loop through each segment of river ----
# ********************************************

boatable_year_lst   <- list()
boatable_month_lst  <- list()
boatable_day_lst    <- list()
i = 1
for (i in 1:nrow(site_gages)) {

  site_preference <- flow_pref %>%
    filter(segment == site_gages$site_name[i])
  
  logger::log_info("Calculating boatable days in year @ {site_gages$site_name[i]}")
  
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
      segment = site_gages$site_name[i]
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
      segment = site_gages$site_name[i]
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
  
   logger::log_info("Calculating boatable days in month @ {site_gages$site_name[i]}")
   
   # Annual boatable days - Observed Flows
   boat_month <- boatable_days(
     # flow_data      =  filter(flow, site == site.gages[[i]]),
     flow_data      = filter(flow, site == site_gages$site[i]) %>% 
       dplyr::select(site, date, flow),
     min_acceptable = min.acceptable,
     max_acceptable = max.acceptable,
     timescale      = "month") %>%    
     mutate(
       segment = site_gages$site_name[i]
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
       segment = site_gages$site_name[i]
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
   
   logger::log_info("Calculating boatable days @ {site_gages$site_name[i]}")
   
   # boatable days from observed flow
   boat_day <- boatable_days(
     flow_data      = filter(flow, site == site_gages$site[i]) %>% 
       dplyr::select(site, date, flow),
     min_acceptable = min.acceptable,
     max_acceptable = max.acceptable,
     timescale      = "day"
   ) %>% 
     mutate(
       segment = site_gages$site_name[i]
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
       segment = site_gages$site_name[i]
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
 
   logger::log_info("Plotting annual boatable days @ {site_gages$site_name[i]}")
   
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
       title  = paste('Boatable Days at', site_gages$site_name[i]),
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
   filename <- paste0(gsub(" ","_",  tolower(site_gages$site_name[i])), '_boatable_days.png')
   
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
   
   
   
   rm(q_year, q_month, q_day, boat_sim_day, boat_sim_month,
      boat_sim_year, boat_year, boat_day, flow_day, sim_flow_day, 
      flow_month, sim_flow_month, flow_year, sim_flow_year, boat_month,
      q_year_long
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
  #     segment = site_gages$site_name[i]
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
  #     segment = site_gages$site_name[i]
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
  #     segment = site_gages$site_name[i]
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
  # logger::log_info("Calculating boatable days in month @ {site_gages$site_name[i]}")
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
  #     segment = site_gages$site_name[i]
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
  #     segment = site_gages$site_name[i]
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
  # logger::log_info("Calculating boatable days @ {site_gages$site_name[i]}")
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
  #     segment = site_gages$site_name[i]
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
  #     segment = site_gages$site_name[i]
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
  # logger::log_info("Plotting annual boatable days @ {site_gages$site_name[i]}")
  
  # pivot data long for plotting
  q_year_long <- q_year %>% 
    rename("Boatable Days" = boatable_days, "Simulated Boatable Days" = sim_boatable_days) %>% 
    pivot_longer(cols = c("Boatable Days", "Simulated Boatable Days"))
  
  # create a line chart of boatable days & simulated boatable days for this site
  boatable_days_plot <- 
    ggplot() +
      geom_line(data = q_year_long, aes(x = year, y = value, col = name), size = 1.5) +
      geom_point(data = q_year_long, aes(x = year, y = value, col = name), size = 3) +
      # geom_col(data = q_year_long, aes(x = year, y = value, fill = year_type)) +
      # facet_wrap(~name) +
      labs(
        title  = paste('Boatable Days at', site_gages$site_name[i]),
        subtitle = "Simulated Boatable days calculated using simulated flows\nSimulated flows = Pineview flow model - (Diversions - releases)",
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
  filename <- paste0(gsub(" ","_",  tolower(site_gages$site_name[i])), '_boatable_days.png')
  
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
  
  
  
  rm(q_year, q_month, q_day, boat_sim_day, boat_sim_month,
     boat_sim_year, boat_year, boat_day, flow_day, sim_flow_day, 
     flow_month, sim_flow_month, flow_year, sim_flow_year, boat_month,
     q_year_long
     )
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


