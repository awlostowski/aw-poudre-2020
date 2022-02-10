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
      
      logger::log_info("Getting stage/volume/release/diversion flow data  @  {res_structures$structure[i]}  --  WDID: {res_structures$wdid[i]}")
  
      # get stage/volume data for reservoir
      stage <- getCDSSDiversionFlow(wdid = res_structures$wdid[i], data_type = "stage")
      
      stage <- stage %>%   
        mutate(
          month = month(date),
          year  = year(date)
        ) %>% 
        group_by(month, year) %>% 
        summarize(
          stage    = mean(stage, na.rm = T)
        ) %>% 
        ungroup() %>% 
        mutate(date_ym = as.Date(paste0(year, "-", month, "-01"))) %>% 
        dplyr::select(date_ym, stage)
      
      # get stage/volume data for reservoir
      volume <- getCDSSDiversionFlow(wdid = res_structures$wdid[i], data_type = "volume")
      volume <- volume %>%   
        mutate(
          month = month(date),
          year  = year(date)
        ) %>% 
        group_by(month, year) %>% 
        summarize(
          volume   = mean(volume, na.rm = T)
        ) %>% 
        ungroup() %>% 
        mutate(date_ym = as.Date(paste0(year, "-", month, "-01"))) %>% 
        dplyr::select(date_ym, volume)
      
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
      res_data <- stage %>% 
        left_join(
          volume,
          by = c("date_ym")
        ) %>% 
        left_join(
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
        dplyr::select(structure, wdid, date = date_ym, stage, volume, diversion = flow, release, source)
      
      # add change in storage and inflow columns
      res_data <- res_data %>% 
        group_by(structure) %>%
        arrange(structure) %>%
        mutate(
          dvolume             = volume - lag(volume),
          inflow              = dvolume + release, 
          dvolume_direction   = case_when(                 # volume change was positive or negative
            dvolume < 0  ~ "NEG",
            dvolume >= 0 ~ "POS"
          ),
          inflow_abs          = case_when(                 # inflow = change in storage - releases
            inflow < 0  ~ 0,
            inflow >= 0 ~ inflow
          )
        ) %>%
        ungroup() %>% 
        dplyr::relocate(
          structure, wdid, date, 
          stage, volume, diversion, release,
          inflow, inflow_abs, dvolume, dvolume_direction, source
          )
        # mutate(across(where(anyNA), ~ replace_na(., 0)))
      
      # add reservoir data to list of reservoirs
      reservoir_lst[[i]] <- res_data
      
      # remove data made during loop
      rm(res_data, flow, stage, release, volume)
      
}

# bind rows of all reservoirs
reservoirs <- bind_rows(reservoir_lst)

rm(res_structures, i)
# ***************
# ---- Plots ----
# ***************

# Summarize reservoir data for plotting

# facet plot Volume @ Barnes Meadow, Chambers, Joe Wright, Long Draw, & Peterson 
volume_data <- reservoirs %>%
  pivot_longer(
    cols       = c(stage, volume, diversion, release, dvolume, inflow, inflow_abs),   # pivot data for plotting
    names_to   = "variable",
    values_to  = "value"
    )  %>% 
  filter(variable == "volume")

volume_plot <-  ggplot() +  
    geom_col(data = volume_data, aes(x = date, y = value, fill = variable)) + 
    # facet_grid(variable~structure) +
    facet_grid(structure~variable) +
    labs(
      title    = "Volume @ Poudre River Reservoirs",
      x        = "Date",
      y        = "Value",
      fill     = ""
    ) +
    theme_bw() +
    theme(
      axis.text      = element_text(size = 12),
      axis.title     = element_text(size = 14, face = "bold"),
      plot.title     = element_text(size = 18, face = "bold"),
      strip.text.x   = element_text(size = 12, color = "black",face = "bold"),
      strip.text.y   = element_text(size = 12, color = "black",face = "bold"),
      plot.caption   = element_text(size = 12)
    )

ggsave(
  "C:/Users/angus/OneDrive/Desktop/reservoir_volume_plot.jpeg", 
  plot = volume_plot,
  width = 36,
  height = 28, 
  units = "cm"
       )

rm(volume_data)
# Summary plots w/o volume data

# facet plot Diversions/Releases/Stage @ Barnes Meadow, Chambers, Joe Wright, Long Draw, & Peterson 
diversion_release_data <- reservoirs %>%  
  pivot_longer(
    cols       = c(stage, volume, diversion, release, dvolume, inflow, inflow_abs),   # pivot data for plotting
    names_to   = "variable",
    values_to  = "value"
  )  %>% 
  filter(variable %in%  c("stage", "diversion", "release"), value <= 1000)         # remove volume data and outlier stage value for plotting
    
diversion_release_plot <- 
  ggplot() +
     geom_col(data = diversion_release_data, aes(x = date, y = value, fill = variable)) + 
     facet_grid(structure~variable) +
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
        strip.text.x   = element_text(size = 12, color = "black",face = "bold"),
        strip.text.y   = element_text(size = 12, color = "black",face = "bold"),
        plot.caption   = element_text(size = 12)
     )

ggsave(
  "C:/Users/angus/OneDrive/Desktop/reservoir_flows_plot.jpeg", 
  plot = diversion_release_plot,
  width = 36,
  height = 28, 
  units = "cm"
)
# ---- Water Balance ----

water_balance <- reservoirs %>%
  # filter(structure == "long_draw") %>%
  dplyr::relocate(structure, wdid, date) %>%
  group_by(structure) %>%
  arrange(structure) %>%
  mutate(
    dvolume             = volume - lag(volume),
    inflow              = dvolume + release, 
    dvolume_direction   = case_when(
                                dvolume < 0  ~ "NEG",
                                dvolume >= 0 ~ "POS"
                                ),
    inflow_abs          = case_when(
                                inflow < 0 ~ 0,
                                inflow >= 0 ~ inflow
                                )
  ) %>%
  ungroup() %>% 
  mutate(across(where(anyNA), ~ replace_na(., 0))) %>%         # replace NA w/ 0
  dplyr::relocate(
    structure, wdid, date, 
    stage, volume, diversion, release,
    inflow, inflow_abs, dvolume, dvolume_direction, source
    )


reservoirs %>% 
  pivot_longer(
    cols       = c(stage, volume, diversion, release, dvolume, inflow, inflow_abs),   # pivot data for plotting
    names_to   = "variable",
    values_to  = "value"
  )  %>% 
  filter(variable %in% c("dvolume", "inflow", "volume")) %>%                      # filter change in volume, inflow/inflow absolute values
  ggplot() + 
   geom_col(aes(x = date, y = value, fill = variable)) + 
  facet_grid(variable~structure, scales = "free") +
    # facet_grid(structure~variable) +
    labs(
      title    = "Change in Volume + Inflow @ Poudre River Reservoirs",
      x        = "Date",
      y        = "Value",
      fill     = ""
      # caption = "Diversions/Releases (CFS)\nStage (ft)"
    ) +
    theme_bw() +
    theme(
      axis.text      = element_text(size = 12),
      axis.title     = element_text(size = 14, face = "bold"),
      plot.title     = element_text(size = 18, face = "bold"),
      strip.text.x   = element_text(size = 12, color = "black",face = "bold"),
      strip.text.y   = element_text(size = 12, color = "black",face = "bold"),
      plot.caption   = element_text(size = 12)
    )








