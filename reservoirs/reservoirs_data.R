##------------------------------------------------------------------------------
##
## Script name: reservoirs_data.R
##
## Purpose of script: 
##  Retrieve reservoir data from CDSS, and calculate water balance/effect of reservoir releases on downstream flow in Poudre Canyon
## 
##
## Author: Angus Watters & Adam N. Wlostowski
##
## Date Created: 2022-02-02
##
## Copyright (c) Adam N. Wlostowski, 2021
## Email: awlostowski@lynker.com
## Email: awatters@lynker.com
##
## ---------------------------
##
## Notes:
##
##------------------------------------------------------------------------------

remove(list = ls())  # clear all workspace variables
cat("\014")          # clear command line

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

## load packages
library(here)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(logger)

source("reservoirs/get_reservoir_utils.R")

# WDID:
# long draw                             - 0303676
# chambers                              - 0303679   
# Joe Wright (mountain supply res 20)   - 0303678   
# Peterson Lake Res                     - 0303677   
# Barnes Meadow Res                     - 0303683
# Zimmerman Lake Res                    - 0303758  # ***No data avaliable at Zimmerman Lake Reservoir***

# Reservoirs and WDIDs
res_structures <- data.frame(
                          structure  =  c('long_draw', 'chambers', 'joe_wright', 'peterson', 'barnes_meadow'), # "zimmerman",
                          wdid       =  c("0303676", "0303679", "0303678", "0303677", "0303683")               # "0303758"
                        )

reservoir_lst <- list()

# Loop through reservoir WDIDs and pull data from CDSS using getCDSSDiversionFlow()
for (i in 1:length(res_structures$wdid)) {
      
      logger::log_info("Getting stage/volume/flow data  @  {res_structures$structure[i]}  --  WDID: {res_structures$wdid[i]}")
  
      # get stage/volume data for reservoir
      stage <- getCDSSDiversionFlow(wdid = res_structures$wdid[i], data_type = "stage")
      stage <- stage %>%   
        mutate(
          month = month(date),
          year  = year(date)
        ) %>% 
        group_by(month, year) %>% 
        summarize(
          volume   = mean(volume, na.rm = T),
          stage    = mean(stage, na.rm = T)
        ) %>% 
        ungroup() %>% 
        mutate(date_ym = as.Date(paste0(year, "-", month, "-01"))) %>% 
        dplyr::select(date_ym, stage, volume)
      
      # get releases data for reservoir
      release <- getCDSSDiversionFlow(wdid = res_structures$wdid[i], data_type = "release")
      
      release <- release %>%   
        mutate(
          month = month(date),
          year  = year(date)
        ) %>% 
        group_by(month, year) %>% 
        summarize(
          release   = mean(release, na.rm = T)
        ) %>% 
        ungroup() %>% 
        mutate(date_ym = as.Date(paste0(year, "-", month, "-01"))) %>% 
        dplyr::select(date_ym, release)
      
        # ungroup() %>% 
        # mutate(date_ym = as.Date(paste0(year, "-", month, "-01"))) %>% 
        # dplyr::select(datetime, date, date_ym, release)
      
      # get flow data for reservoir and sum flow by month 
      flow <- getCDSSDiversionFlow(wdid = res_structures$wdid[i], data_type = "flow") 
      
      flow_unit <- flow$unit[1]
      
      # Summarize reservoir total monthly flow 
      flow <- flow %>% 
        mutate(
          month = month(date),
          year  = year(date)
               ) %>% 
        group_by(wdid, month, year) %>% 
        summarize(flow = mean(flow, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(
          date_ym = as.Date(paste0(year, "-", month, "-01"))
          ) %>% 
        dplyr::select(date_ym, flow)
    
      
      # Join Stage/volume and flow data
      res_data <- left_join(
                          stage, 
                          flow, 
                          by = c("date_ym")  # by = c("month", "year")
                          ) %>% 
        left_join(
                release,
                by = c("date_ym")
                ) %>% 
        mutate(
          structure = res_structures$structure[i],
          wdid      = res_structures$wdid[i],
          source    = "CDSS"
          ) %>% 
        dplyr::select(wdid, structure, date = date_ym, stage, volume, diversion = flow, release, source)
      
      # add reservoir data to list of reservoirs
      reservoir_lst[[i]] <- res_data
      
      # remove data made during loop
      rm(res_data, flow, stage, release)
      
}

# bind rows of all reservoirs
reservoirs <- bind_rows(reservoir_lst)

# Summarize reservoir data for plotting
reservoirs_summary <- reservoirs %>%
  pivot_longer(
    cols       = c(stage, volume, diversion, release),   # pivot data for plotting
    names_to   = "variable",
    values_to  = "value"
    ) %>% 
  filter(variable != "volume", value <= 1000)            # remove volume data and outlier stage value for plotting

# Faceted plot showing Diversions/Releases/Stage @ Barnes Meadow, Chambers, Joe Wright, Long Draw, and Peterson 
ggplot() +
  geom_col(data = reservoirs_summary, aes(x = date, y = value, fill = variable)) + 
  facet_grid(variable~structure) +
  labs(
    title    = "Diversions + Releases + Stage @ Poudre River Reservoirs",
    x        = "Date",
    y        = "Value",
    fill     = "",
    caption = "Diversions/Releases (CFS)\nStage (ft)"
  ) +
  theme_bw() +
  theme(
    axis.text      = element_text(size = 12),
    axis.title     = element_text(size = 14, face = "bold"),
    plot.title     = element_text(size = 18, face = "bold"),
    strip.text.x   = element_text(size = 12, color = "black"),
    strip.text.y   = element_text(size = 12, color = "black"),
    plot.caption   = element_text(size = 12)
  )














