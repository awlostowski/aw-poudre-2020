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

# reservoirs we are concerned with are "flow through" reservoirs. This means that the total outflow includes both run of river throughflow (i.e. water that simply flows in and flows out) plus the "release" volume, which is a managed release.

# Total outflow = Run of River throughflow + "Release volume"
# The "diversion" volume is the volume of water that is placed into storage by the reservoir.

# water balance equation:
# dS = diversion - releases

# Natural flow below reservoir:
# Qnat = Qobs + sum(Diversion) - sum(Release)
# where Qobs is the observed flow downstream of the reservoir(s).

# We can calculate what the flow would otherwise be lower in the canyon by just analyzing the reservoir release and diversion data.

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

source("reservoirs/get_reservoir_utils.R")

# Reservoirs and WDIDs
res_structures <- data.frame(
        structure  =  c('long_draw', 'chambers', 'joe_wright', 'peterson', 'barnes_meadow'), # "zimmerman",
        wdid       =  c("0303676", "0303679", "0303678", "0303677", "0303683")               # "0303758"
        )

reservoir_lst <- list()

# Loop through reservoir WDIDs and pull data from CDSS using getCDSSDiversionFlow()
for (i in 1:length(res_structures$wdid)) {
      
      logger::log_info("Getting stage/volume/release/diversion data:\n{res_structures$structure[i]}\nWDID: {res_structures$wdid[i]}")
  
      # get stage/volume data for reservoir
      stage <- getCDSSDiversionFlow(
          wdid      = res_structures$wdid[i],
          data_type = "stage"
        ) %>%
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
      volume <- getCDSSDiversionFlow(
          wdid      = res_structures$wdid[i], 
          data_type = "volume"
        ) %>%   
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
        ) %>%
        dplyr::select(date, release)

      
      # get flow data for reservoir and sum flow by month 
      diversion <- getCDSSDiversionFlow(
          wdid      = res_structures$wdid[i], 
          data_type = "flow",
          timescale = "monthly" 
        ) %>% 
        dplyr::select(date, diversion = flow)
      
      # Join Stage/volume and flow data
      res_data <- stage %>% 
        left_join(
          volume,
          by = c("date")
        ) %>% 
        left_join(
          diversion, 
          by = c("date")  
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
      
      # water balance equation:
      # dS = diversion - releases
      
      # add change in storage, outflow, & inflow columns
      res_data <- res_data %>%
        cleaner::na_replace(release, volume, diversion) %>% 
        group_by(structure) %>%
        arrange(date) %>%
        mutate(
          dvolume               = (diversion - release)        # dS = diversion - releases
          ) %>%
        ungroup() %>% 
        dplyr::relocate(
          structure, wdid, date,
          stage, volume, diversion, release,
          dvolume, 
          source
        )
      
      # add reservoir data to list of reservoirs
      reservoir_lst[[i]] <- res_data
      
      # remove data made during loop
      rm(res_data, flow,diversion, stage, release, volume)
      
}

# bind rows of all reservoirs
reservoirs <- bind_rows(reservoir_lst)

# ********************
# ---- CDSS gages ----
# ********************

# CDSS gages below + above reservoirs

# optional API token key
api_token <- "2fx%2B0sUzKbpOWeqkWzbU4BIIOtpwoVyE"

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

  # Get daily stream gage data
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
      flow   = round(sum(flow_af_day, na.rm = T), 3)          # Sum daily AF/day to get total AF of flow in each month (AF)      
    ) %>%
    ungroup() %>%
    mutate(
      date      = as.Date(paste0(year, "-", month, "-01")),   # wyear     = lfstat::water_year(date),
      structure = gage_structures$structure[i],
      abbrev    = gage_structures$abbrev[i],
      wdid      = gage_structures$wdid[i],
      source    = "CDSS"
    ) %>% 
    dplyr::select(-year, -month) %>% 
    dplyr::relocate(structure, wdid, abbrev, date, flow, source)
  
  gage_lst[[i]] <- gage_month
  
  rm(gage, gage_month)
}

cdss_gages <- bind_rows(gage_lst) 

# ************************************
# ---- CDSS Gage + reservoir data ----
# ************************************

# Join CDSS gages and reservoir data
reservoir_totals <- reservoirs %>% 
  left_join(
    dplyr::select(cdss_gages, structure, date, flow),
    by = c("structure", "date")
  ) %>% 
  rename(gage_outflow = flow) %>%
  mutate(
    qnatural   = gage_outflow + dvolume,                      # calculate inflows based on stream gage data
    ) %>% 
  dplyr::relocate(
    structure, wdid, date, stage, volume, diversion,
    release, dvolume, gage_outflow, qnatural, source
    ) %>% 
  group_by(date) %>%                                          # Add up diversions and releases across all reservoirs
  summarise(
    volume       = sum(volume, na.rm = T),
    diversion    = sum(diversion, na.rm = T),
    release      = sum(release, na.rm = T)
  ) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(
    dvolume   = diversion - release                           # calculate change in volume
    ) 

# *****************************************
# ---- Calculate daily Qnatural in CFS ----
# *****************************************

# Pineview model flow
pineview_model  <- readRDS("boatable_days/simulated_historical_pineview_flow.RDS")

# Generate daily dates using min/max dates in Pineview model 
days <- seq.Date(
              from = min(pineview_model$date),
              to   = max(pineview_model$date),
              by   = "day"
            ) %>% 
  tibble() %>% 
  setNames(c("date")) %>% 
  mutate(
    month   = lubridate::month(date),
    year    = lubridate::year(date),
    date_ym = as.Date(paste0(year, "-", month, "-01"))
  ) %>% 
  dplyr::select(date, date_ym)

# join days dataframe with monthly reservoir totals
reservoir_daily <-  left_join(
    days,
    reservoir_totals, 
    by = c("date_ym" = "date")
  ) %>% 
  group_by(date) %>% 
  summarise(
    days_in_month     = days_in_month(date),
    dvolume           =  (dvolume/days_in_month)*0.5042864       # calculate daily value per month, convert AF to CFS     
  ) %>% 
  ungroup() %>% 
  dplyr::select(-days_in_month)

# Join daily reservoir flow in CFS with Pineview model results
reservoir_pv_flow <- left_join(
  pineview_model,
  reservoir_daily,
  by = "date"
  ) %>% 
  mutate(
    sim_nat = flow - dvolume          # Simulate natural flows = Pineview flows - (Diversions - Releases)
    ) %>% 
  rename(pineview_flow = flow) 

# pivot data long for plotting
reservoir_pv_flow_long <- reservoir_pv_flow %>% 
  pivot_longer(cols = c(-date)) %>%
  mutate(
    name = factor(name, 
                  levels=c("pineview_flow", "sim_nat", "dvolume"))
  ) %>% 
  filter(date >= "2018-01-01", date <= "2020-01-01")

# pivot data long for plotting
reservoir_flow_plot <- 
  ggplot() +
    geom_line(data = reservoir_pv_flow_long, 
              aes(x = date, y = value, col = name, alpha = name),
              size = 1) +
  scale_alpha_manual(
    values = c("pineview_flow" = 1, "sim_nat" = 0.6, "dvolume" = 1),
    guide  = 'none') +
    labs(
      title    = "Reservoir flows and Pineview model flow",
      x        = "Date",
      y        = "Volume (CFS/day)",
      subtitle = "sim_nat   =  pineview_flow - (diversions + releases)\ndvolume  = diversions - releases",
      col      = "",
      alpha    = ""
    ) +
    theme_bw() +
    theme(
      axis.text      = element_text(size = 14),
      axis.title     = element_text(size = 16, face = "bold"),
      plot.title     = element_text(size = 20, face = "bold"),
      strip.text.x   = element_text(size = 14, color = "black",face = "bold"),
      strip.text.y   = element_text(size = 14, color = "black",face = "bold"),
      legend.text    = element_text(size = 14),
      plot.subtitle  = element_text(size = 16)
    ) +
    scale_x_date(date_labels="%b %y",date_breaks  ="3 month")


reservoir_flow_plot

# Export plot
ggsave(
  "plots/reservoirs/simulated_natural_flow_pineview_flow.png",
  plot   = reservoir_flow_plot,
  width  = 46,
  height = 28, 
  units  = "cm"
)

# ****************************************************************
# ---- Save Reservoir flows & Poudre Park modeled flow at RDS ----
# ****************************************************************

# save data to disk as RDS
path     <- here::here("reservoirs")
filename <- 'simulated_natural_flow_pineview_flow.rds'
logger::log_info(
  'saving simulated flow at Pineview with added natural flows from reservoirs as {paste0(path, "/", filename)}'
)

saveRDS(reservoir_pv_flow, paste0(path, "/", filename))

# ****************************************************
# ---- Water balance plot (Gage + reservoir data) ----
# ****************************************************

# Join CDSS gages and reservoir data
reservoir_gages <- reservoirs %>% 
  left_join(
    dplyr::select(cdss_gages, structure, date, flow),
    by = c("structure", "date")
  ) %>% 
  rename(gage_outflow = flow) %>%
  mutate(
    qnatural   = gage_outflow + dvolume,                      # calculate inflows based on stream gage data
  ) %>% 
  dplyr::relocate(
    structure, wdid, date, stage, volume, diversion,
    release, dvolume, gage_outflow, qnatural, source
  )

# plot monthly mean hydrographs for natural flow and observed flow on top of one another 
water_balance <- reservoir_gages %>% 
  dplyr::select(date, structure, gage_outflow, qnatural) %>% 
  pivot_longer(
    cols         = c(gage_outflow, qnatural)          # pivot data for plotting
  ) %>% 
  mutate(
    name = factor(name, 
                  levels=c("gage_outflow", "qnatural"))
  ) %>% 
  filter(date >= "2018-01-01", date <="2020-01-01")

water_balance_plot <-
  ggplot() + 
  geom_line(data = water_balance,
           aes(x = date, y = value, col = name), size = 1.5
  ) + 
    facet_grid(~structure) +
    labs(
      title    = "Natural vs. Observed flows",
      x        = "Date",
      y        = "Volume (AF)",
      subtitle = "qnatural = gage_outflow + diversion - releases",
      fill     = ""
    ) +
    theme_bw() +
    theme(
      axis.text      = element_text(size = 14),
      axis.title     = element_text(size = 16, face = "bold"),
      plot.title     = element_text(size = 20, face = "bold"),
      strip.text.x   = element_text(size = 14, color = "black",face = "bold"),
      strip.text.y   = element_text(size = 14, color = "black",face = "bold"),
      legend.text    = element_text(size = 14),
      plot.subtitle  = element_text(size = 16)
    ) +
  scale_x_date(date_labels="%b %y",date_breaks  ="12 month") +
  scale_y_continuous(breaks = seq(0, 20000, 2500))

water_balance_plot

# Export plot
ggsave(
  "plots/reservoirs/res_gage_water_balance_plot.png",
  plot   = water_balance_plot,
  width  = 36,
  height = 28, 
  units  = "cm"
)

# ***********************************
# ---- Managed vs. Natural flows ----
# ***********************************

# Initial attempt at calculating Managed & Natural flows

# ---- Natural flows ----
# Qnatural = In_j + (In_c - Out_j) + In_b + In_L

# (Out_c Out_j)
gage_flows <- reservoir_gages %>% 
  filter(structure %in% c("joe_wright", "chambers", "long_draw"))  %>%
  filter(date >=" 2018-09-01") %>%
  # na.omit() %>%
  pivot_wider(
    id_cols     = c(structure, date),
    names_from  = "structure",
    names_glue  = "{structure}_{.value}",
    values_from = c(gage_outflow, gage_inflow),
    values_fn   = mean
  ) %>% 
  ungroup() %>% 
  na.omit() %>%
  mutate(
    added_flow = chambers_gage_inflow - joe_wright_gage_outflow  # (In_c - Out_j) 
  ) 

# In_b
barnes_flows <- reservoir_gages %>% 
  filter(
    structure %in% c("barnes_meadow"),
    date >=" 2018-09-01"
    ) %>% 
  dplyr::select(date, inflow) %>% 
  rename(barnes_meadow_inflow = inflow)
 
# Calculate Qnatural  
natural_flows <- gage_flows %>% 
  left_join(
    barnes_flows,
    by = "date"
    ) %>% 
  mutate(
    qnatural = joe_wright_gage_inflow + added_flow + barnes_meadow_inflow + long_draw_gage_inflow
  ) %>% 
  rename(
    in_jw   = joe_wright_gage_inflow,
    out_jw  = joe_wright_gage_outflow,
    in_c    = chambers_gage_inflow,
    out_c   = chambers_gage_outflow,
    in_ld   = long_draw_gage_inflow,
    out_ld  = long_draw_gage_outflow,
    in_b    = barnes_meadow_inflow
    ) 
  
# pivot longer for plotting
natural_flows_long <- natural_flows %>% 
  dplyr::select(date, qnatural, in_jw, added_flow, in_b, in_ld) %>% 
  pivot_longer(cols = c(-date)) %>% 
  mutate(
    name =  factor(name, levels=c("in_jw", "added_flow", "in_b", "in_ld", "qnatural"))
  )

# Plot natural flows
natural_flows_plot <-
  ggplot() +
    geom_col(data = natural_flows_long,  aes(x = date, y = value, fill = name)) +
    # facet_wrap(name~.) +
    facet_grid(~name) +
  # facet_grid(name~.) +
    labs(
      title    = "Natural flows",
      subtitle = "qnatural = in_jw + added_flow + in_b + in_ld",
      x        = "Date",
      y        = "Volume (AF)",
      fill     = ""
    ) +
    theme_bw() +
    theme(
      axis.text      = element_text(size = 12),
      axis.title     = element_text(size = 16, face = "bold"),
      plot.title     = element_text(size = 20, face = "bold"),
      strip.text.x   = element_text(size = 14, color = "black",face = "bold"),
      strip.text.y   = element_text(size = 14, color = "black",face = "bold"),
      legend.text    = element_text(size = 12),
      plot.subtitle  = element_text(size = 14)
    ) +
  scale_x_date(date_labels="%b %y",date_breaks  ="6 month") +
  scale_y_continuous(breaks = seq(0, 30000, 5000))

natural_flows_plot

# Export plot
ggsave(
  "plots/reservoirs/natural_flows_plot.png",
  plot   = natural_flows_plot,
  width  = 50,
  height = 22,
  units  = "cm"
)

# ***********************
# ---- Managed flows ----
# ***********************
# Qmanaged = Out_c + Out_b + Out_L

# calculate Qmanaged
managed_flows <- reservoir_gages %>% 
  filter(structure %in% c("long_draw", "chambers", "barnes_meadow"))  %>%
  dplyr::select(date, structure, wdid, outflow, gage_outflow) %>% 
  pivot_wider(
    id_cols     = c(structure, date),
    names_from  = "structure",
    names_glue  = "{structure}_{.value}",
    values_from = c(outflow, gage_outflow),
    values_fn   = mean
  )  %>% 
  ungroup() %>%
  mutate(
    qmanaged      = chambers_outflow + barnes_meadow_outflow + long_draw_outflow,                   # (Out_c + Out_b + Out_L)
    qmanaged_gage = chambers_gage_outflow + barnes_meadow_outflow + long_draw_gage_outflow       # Outflows from stream gages at Chambers & Long Draw
  ) %>% 
  rename(
    out_c        = chambers_outflow,
    out_b        = barnes_meadow_outflow,
    out_ld       = long_draw_outflow,
    out_c_gage   = chambers_gage_outflow,
    out_ld_gage  = long_draw_gage_outflow
  ) 

# pivot longer for plotting
managed_flows_long <- managed_flows %>%
  dplyr::select(date, qmanaged, out_c, out_b, out_ld) %>% 
  pivot_longer(cols = c(-date))  %>% 
  filter(date >= "2018-09-01") %>% 
  mutate(
    name =  factor(name, levels=c("out_c", "out_b", "out_ld", "qmanaged"))
    )

# Plot managed flows
managed_flows_plot <- 
  ggplot() +
    geom_col(data = managed_flows_long,  aes(x = date, y = value, fill = name)) +
    # facet_wrap(~name) +
    facet_grid(~name) +
    labs(
      title    = "Managed flows",
      subtitle = "qmanaged = out_c + out_b + out_ld",
      x        = "Date",
      y        = "Volume (AF)",
      fill     = ""
    ) +
    theme_bw() +
    theme(
      axis.text      = element_text(size = 12),
      axis.title     = element_text(size = 16, face = "bold"),
      plot.title     = element_text(size = 20, face = "bold"),
      strip.text.x   = element_text(size = 14, color = "black",face = "bold"),
      strip.text.y   = element_text(size = 14, color = "black",face = "bold"),
      legend.text    = element_text(size = 12),
      plot.subtitle  = element_text(size = 14)
    )  +
  scale_x_date(date_labels="%b %y",date_breaks  ="6 month") +
  scale_y_continuous(
    limits = c(0, 30000),
    breaks = seq(0, 30000, 5000)
    )


managed_flows_plot


# pivot longer for plotting
managed_gage_flows_long <- managed_flows %>%
  dplyr::select(date, qmanaged_gage, out_c_gage, out_b, out_ld_gage) %>% 
  pivot_longer(cols = c(-date))  %>% 
  filter(date >= "2018-09-01") %>% 
  mutate(
    name =  factor(name, levels=c("out_c_gage", "out_b", "out_ld_gage", "qmanaged_gage"))
  )

# Plot managed flows using stream gage outflows
managed_gage_flows_plot <- 
  ggplot() +
  geom_col(data = managed_gage_flows_long,  aes(x = date, y = value, fill = name)) +
  # facet_wrap(~name) +
  facet_grid(~name) +
  labs(
    title    = "Managed flows (using stream gage outflows)",
    subtitle = "qmanaged_gage = out_c_gage + out_b + out_ld_gage",
    x        = "Date",
    y        = "Volume (AF)",
    fill     = ""
  ) +
  theme_bw() +
  theme(
    axis.text      = element_text(size = 12),
    axis.title     = element_text(size = 16, face = "bold"),
    plot.title     = element_text(size = 20, face = "bold"),
    strip.text.x   = element_text(size = 14, color = "black",face = "bold"),
    strip.text.y   = element_text(size = 14, color = "black",face = "bold"),
    legend.text    = element_text(size = 12),
    plot.subtitle  = element_text(size = 14)
  )  +
  scale_x_date(date_labels="%b %y",date_breaks  ="6 month") +
  scale_y_continuous(
    limits = c(0, 30000),
    breaks = seq(0, 30000, 5000)
  )


managed_gage_flows_plot

# Export plot
ggsave(
  "plots/reservoirs/managed_gage_flows_plot.png",
  plot   = managed_gage_flows_plot,
  width  = 50,
  height = 22,
  units  = "cm"
)

# *********************************
# ---- Natural + Managed flows ----
# *********************************

# QNatural vs. Qmanaged
qnat_qman <-   left_join(
                    dplyr::select(natural_flows, date, qnatural),
                    dplyr::select(managed_flows, date, qmanaged, qmanaged_gage), 
                    by = "date"
                    ) %>% 
  filter(date >= "2018-09-01") %>% 
  pivot_longer(cols = c(-date)) %>% 
  mutate(
    name = factor(name, levels=c("qnatural", "qmanaged", "qmanaged_gage"))
  )

# Plot managed flows
qnat_qman_plot <- 
  ggplot() +
    geom_col(data = qnat_qman,  aes(x = date, y = value, fill = name)) +
    # facet_wrap(~name) +
  facet_grid(~name) +
    # facet_grid(name~.) +
    labs(
      title    = "Natural vs. Managed Flows",
      subtitle = "qnatural                  = in_jw + (in_c - out_jw) + in_b + in_ld \nqmanaged              = out_c + out_b + out_ld \nqmanaged_gage   = out_c_gage + out_b + out_ld_gage",
      x        = "Date",
      y        = "Volume (AF)",
      fill     = ""
    ) +
    theme_bw() +
    theme(
      axis.text      = element_text(size = 12),
      axis.title     = element_text(size = 16, face = "bold"),
      plot.title     = element_text(size = 20, face = "bold"),
      strip.text.x   = element_text(size = 14, color = "black",face = "bold"),
      strip.text.y   = element_text(size = 14, color = "black",face = "bold"),
      legend.text    = element_text(size = 12),
      plot.subtitle  = element_text(size = 14)
    ) +
  scale_x_date(date_labels="%b %y",date_breaks  ="6 month") +
  scale_y_continuous(
    limits = c(0, 30000),
    breaks = seq(0, 30000, 5000)
  )

qnat_qman_plot

# Export plot
ggsave(
  "plots/reservoirs/natural_vs_managed_flows_plot.png",
  plot   = qnat_qman_plot,
  width  = 50,
  height = 22,
  units  = "cm"
)


# ***********************

# ********************
# ---- USGS gages ----
# ********************

above_jw_usgs_id <- "06746095"
below_jw_usgs_id <- "06746110"
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








