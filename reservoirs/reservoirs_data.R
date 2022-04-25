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
library(cleaner)
library(nwmTools)
library(AOI)
library(sf)
library(dataRetrieval)

source("reservoirs/get_reservoir_utils.R")

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
        mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>%
        dplyr::select(date, stage)
      
      # get stage/volume data for reservoir
      volume <- getCDSSDiversionFlow(wdid = res_structures$wdid[i], data_type = "volume")
      volume <- volume %>%   
        mutate(
          month = month(date),
          year  = year(date)
        ) %>% 
        group_by(month, year) %>%
        summarize(
          volume = mean(volume, na.rm = T)
        ) %>% 
        ungroup() %>% 
        mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>% 
        dplyr::select(date, volume)
      

      # get releases data for reservoir
      release <- getCDSSDiversionFlow(
                                    wdid      = res_structures$wdid[i],
                                    data_type = "release",
                                    timescale = "monthly"
                                    ) %>% dplyr::select(date, release)
      
      
      # get flow data for reservoir and sum flow by month 
      diversion <- getCDSSDiversionFlow(
                                      wdid      = res_structures$wdid[i], 
                                      data_type = "flow",
                                      timescale = "monthly"
                                      ) %>% dplyr::select(date, diversion = flow)
      
      # Join Stage/volume and flow data
      res_data <- stage %>% 
        left_join(
          volume,
          by = c("date")
        ) %>% 
        left_join(
          diversion, 
          by = c("date")  # by = c("month", "year")
          ) %>% 
        left_join(
          release,
          by = c("date")
          ) %>% 
        mutate(
          structure = res_structures$structure[i],
          wdid      = res_structures$wdid[i],
          source    = "CDSS"
          ) %>% 
        dplyr::select(
          structure, wdid, date, 
          stage, volume, diversion, release, source
          )
    
      # add change in storage and inflow columns
      res_data <- res_data %>% 
        cleaner::na_replace(release, volume, diversion) %>% 
        group_by(structure) %>%
        arrange(date) %>%
        mutate(
          dvolume             = volume - lag(volume),
          outflow             = (diversion + release),
          inflow              = dvolume + outflow
          ) %>%
        ungroup() %>% 
        dplyr::relocate(
          structure, wdid, date, 
          stage, volume, diversion, release,
          outflow, dvolume, inflow, source
        ) 
      
      # add reservoir data to list of reservoirs
      reservoir_lst[[i]] <- res_data
      
      # remove data made during loop
      rm(res_data, flow, stage, release, volume)
      
}

# bind rows of all reservoirs
reservoirs <- bind_rows(reservoir_lst)


# *********************
# ---- Volume plot ----
# *********************

# Summarize reservoir data for plotting

# facet plot Volume @ Barnes Meadow, Chambers, Joe Wright, Long Draw, & Peterson 
volume_data <- reservoirs %>%
  pivot_longer(
    cols       = c(stage, volume, diversion, release, dvolume, inflow, outflow),   # pivot data for plotting
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
volume_plot

# save
# ggsave(
#   "plots/reservoirs/reservoir_volume_plot.png",
#   plot   = volume_plot,
#   width  = 36,
#   height = 28, 
#   units  = "cm"
# )

rm(volume_data)


# ***********************************
# ---- Diversion & Releases plot ----
# ***********************************

# facet plot Diversions/Releases/Stage @ Barnes Meadow, Chambers, Joe Wright, Long Draw, & Peterson 
diversion_release_data <- reservoirs %>%  
  pivot_longer(
    cols       = c(stage, volume, diversion, release, dvolume, inflow, outflow),   # pivot data for plotting
    names_to   = "variable",
    values_to  = "value"
  )  %>% 
  filter(variable %in%  c("diversion", "release"))         # remove volume data and outlier stage value for plotting
    
diversion_release_plot <- 
  ggplot() +
     geom_col(data = diversion_release_data, aes(x = date, y = value, fill = variable)) + 
     facet_grid(structure~variable) +
     labs(
        title    = "Diversions + Releases + Stage @ Poudre River Reservoirs",
        x        = "Date",
        y        = "AF/day",
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

diversion_release_plot

# save
# ggsave(
#   "plots/reservoirs/reservoir_flows_plot.png",
#   plot   = diversion_release_plot,
#   width  = 36,
#   height = 28, 
#   units  = "cm"
# )


# ***********************


# ***********************
#  ---- Stream Gages ----
# ***********************

api_token <- "2fx%2B0sUzKbpOWeqkWzbU4BIIOtpwoVyE"

# CDSS & USGS gages below + above reservoirs

# ********************
# ---- CDSS gages ----
# ********************

# Gages abbreviations below/above reservoirs
gage_structures <- data.frame(
  structure    =  c('long_draw', 'chambers', 'joe_wright'), #"joe_wright"),
  position     =  c("below",     "below",    "below"),    # "above"),    
  abbrev       =  c("LAPLODCO",  "JWCCHACO", "JOEBELCO"),  # "JOECRECO"),              
  wdid         =  c("0301675",   "0301133",  "0301256"),  #  NA),
  usgs_id      =  c(NA,          NA,         "06746110")  # "06746095")
)

gage_lst <- list()

# Loop through gages from CDSS API
for (i in 1:length(gage_structures$abbrev)) {
  
  logger::log_info("Getting gage data  @  {gage_structures$structure[i]}  --  Abbrev: {gage_structures$abbrev[i]}")

  gage <- GetCDSSStationFlow(
    site_abbrev = gage_structures$abbrev[i],
    api_key     = api_token,
    timescale   = "daily"
  )
  
  # summarize to total acre feet of flow in each month
  gage_month <- gage %>%
    mutate(
      month         = lubridate::month(date),
      year          = lubridate::year(date),
      flow_af_day   = flow*1.983                              # Convert CFS to flow AF/day
    ) %>% 
    group_by(month, year) %>%
    summarize(
      flow   = round(sum(flow_af_day, na.rm = T), 3)          # Sum daily AF/day to get total acre feet of flow in each month (AF)      
    ) %>%
    ungroup() %>%
    mutate(
      date      = as.Date(paste0(year, "-", month, "-01")),
      # wyear     = lfstat::water_year(date),
      structure = gage_structures$structure[i],
      abbrev    = gage_structures$abbrev[i],
      wdid      = gage_structures$wdid[i],
      source    = "CDSS"
    ) %>% 
    # dplyr::relocate(date, year, month, wyear, flow)
    dplyr::select(-year, -month) %>% 
    dplyr::relocate(structure, wdid, abbrev, date, flow, source)
  
  gage_lst[[i]] <- gage_month
  
}

cdss_gages <- bind_rows(gage_lst) 

# *******************************
# ---- Gage + reservoir data ----
# *******************************


# Join CDSS gages and reservoir data
res_gages <- reservoirs %>% 
  left_join(
    dplyr::select(cdss_gages, structure, date, flow),
    by = c("structure", "date")
  ) %>% 
  rename(gage_flow = flow) %>% 
  mutate(gage_inflow_calc = dvolume + gage_flow)

# pivot data long for plotting
water_balance <- res_gages %>% 
  pivot_longer(
    cols       = c(stage, volume, diversion, release, dvolume,
                   inflow, outflow, gage_flow, gage_inflow_calc),   # pivot data for plotting
    names_to   = "variable",
    values_to  = "value"
  ) %>% 
  mutate(
    wyear      = as.numeric(as.character(lfstat::water_year(date, origin = "din"))),
    date_wyear = as.Date(paste0(wyear, "-", lubridate::month(date), "-01"))
  ) %>% 
  filter(
    variable %in% c("dvolume", "inflow","gage_inflow_calc", "gage_flow", "outflow"),
    date >= "2017-10-01",
    date <= "2021-10-01",
    structure %in% c("joe_wright", "long_draw", "chambers")
    )

# factors for facet plots
water_balance$variable <-  factor(water_balance$variable, 
                                     levels=c("dvolume", "outflow", "inflow", "gage_flow", "gage_inflow_calc")
                                  )

# ****************************************************
# ---- Water balance plot (Gage + reservoir data) ----
# ****************************************************

water_balance_plot <- 
  ggplot() + 
    geom_col(data = water_balance, aes(x = date, y = value, fill = variable)) + 
    facet_grid(variable~structure) +
    # facet_grid(structure~variable) +
    labs(
      title    = "Water balance from Reservoir data vs. stream gage",
      x        = "Date",
      y        = "Volume (AF)",
      subtitle = "outflow = diversions + releases \ninflow = dvolume + outflow \ngage_inflow_calc = dvolume + gage_flow",
      fill     = ""
    ) +
    theme_bw() +
    theme(
      axis.text      = element_text(size = 11),
      axis.title     = element_text(size = 14, face = "bold"),
      plot.title     = element_text(size = 18, face = "bold"),
      strip.text.x   = element_text(size = 12, color = "black",face = "bold"),
      strip.text.y   = element_text(size = 12, color = "black",face = "bold"),
      plot.caption   = element_text(size = 12)
    )
water_balance_plot
# Export plot
save_plot(filename = paste0("plots/flow_pref/flow_pref_",
                            segment_name2,
                            "_annotated.png"),
          plot = boat_flow_pref_plots,
          base_width = 10,
          base_height = 10
)
ggsave(
  "plots/reservoirs/res_gage_water_balance_plot.png",
  plot   = water_balance_plot,
  width  = 36,
  height = 28, 
  units  = "cm"
)




# ********************
# ---- USGS gages ----
# ********************

above_jw_usgs_id <- "06746095"

# USGS stream gages
usgs_lst <- list()

usgs_gage_ids <- gage_structures$usgs_id[3]

# Loop through gages from USGS API (using dataRetrieval package)
for (i in 1:length(usgs_gage_ids)) {
  
  logger::log_info("Getting USGS gage data  @  {usgs_gage_ids[i]}")
  
  start_date <- as.Date("1975-10-01")
  # end_date   <- as.Date("2020-10-01")

  usgs_gage <- readNWISdata(
    sites       =  usgs_gage_ids[i],
    service     = "dv",
    parameterCd = "00060",
    startDate   = start_date
    # endDate     = end_date
  )
  
  # if(nrow(usgs_gage) > 0) {
  usgs_gage <- usgs_gage %>% 
    setNames(c("source", "usgs_id", "date", "flow", "flow_code", "tz_cd")) %>% 
    dplyr::select(date, flow, usgs_id, source)

  usgs_lst[[i]] <- usgs_gage
}

usgs_gages <- bind_rows(usgs_lst)

# Summarize USGS gages
usgs_gages <- usgs_gages %>% 
    mutate(
      month            = month(date),
      year             = year(date),
      flow_af_day      = flow*1.983                     # Convert CFS to flow AF/day
    ) %>%
  group_by(usgs_id, month, year) %>% 
  summarize(
      flow   = round(sum(flow_af_day, na.rm = T), 3)    # Total acre feet of flow in each month (AF)
  ) %>% 
  mutate(
      date      = as.Date(paste0(year, "-", month, "-01")),
      source    = "USGS"
  ) %>% 
  ungroup() %>%
  left_join(gage_structures, by = "usgs_id")


# ***************************************
# ---- Joe Wright Water Balance plot ----
# ***************************************

# stream gages above and below Joe Wright reservoir
joe_wright_gages <- usgs_gages %>% 
  dplyr::select(-usgs_id, -abbrev, -wdid, -month, -year) %>%
  pivot_wider(id_cols = c(-position), names_from = "position", values_from = "flow") %>% 
  setNames(c("date", "source", "structure", "flow_above", "flow_below")) %>% 
  mutate(flow_diff = flow_above - flow_below) 

# pivot data long for plotting
jw_water_balance <- reservoirs %>%
  filter(structure == "joe_wright") 
  left_join(
    dplyr::select(joe_wright_gages, date, flow_above, flow_below, flow_diff),
    by = "date"
  ) %>% 
  filter(date >= "1982-10-01", date <= "1983-10-01") %>% 
  pivot_longer(
    cols       = c(stage, volume, diversion, release, dvolume, 
                   inflow, outflow, flow_above, flow_below, flow_diff),   # pivot data for plotting
    names_to   = "variable",
    values_to  = "value"
  )  %>% 
  filter(
    variable %in% c("dvolume", "inflow", "outflow", "flow_above", "flow_below", "flow_diff")
    # date <= "2020-10-01", date > "2017-10-01"
  )    # filter change in volume, inflow/inflow absolute values

# factors for facets
jw_water_balance$variable <-  factor(jw_water_balance$variable, 
                                     levels=c("dvolume", "inflow", "outflow","flow_diff", "flow_above", "flow_below"))
jw_water_balance_plot <- 
  ggplot() + 
    geom_col(data = jw_water_balance, aes(x = date, y = value, fill = variable)) + 
    facet_wrap(~variable) +
    # facet_grid(variable~structure, scales = "free") +
    labs(
      title    = "Water balance from Reservoir data vs. stream gage data",
      x        = "Date",
      y        = "AF/Day",
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
jw_water_balance_plot

# save Joe Wright plot
# ggsave(
#   "plots/reservoirs/joe_wright_water_balance_plot.png",
#   plot   = jw_water_balance_plot,
#   width  = 36,
#   height = 28, 
#   units  = "cm"
# )








