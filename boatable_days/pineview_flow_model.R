##------------------------------------------------------------------------------
##
## Script name: pineview_flow_model.R
##
## Purpose of script: Construct a model that can approximate historical flows
##  at Pineview, and other upper canyon locations
##
## Author: Adam N. Wlostowski & Angus Watters
##
## Date Created: 2021-12-31
##
## Copyright (c) Lynker, 2021
## Email: awlostowski@lynker.com
##
## ---------------------------
##
## Notes:
##
##------------------------------------------------------------------------------

remove(list = ls())  # clear all workspace variables
cat("\014")          # clear command line

## load packages
library(here)
library(tidyverse)
library(logger)

source(here::here('rating_curves', 'get_flow_utils.R'))
plot_path <- here::here('boatable_days','flow_comparison_plots')

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

# ====================================================
# ---- Read in flow data & compute daily averages ----

logger::log_info('Getting flow data necessary for the mass balance model...')

# get flow data for Poudre Canyon mouth
poudre.canyon.mouth <- GetCDSSStationFlow(
  site_abbrev = 'CLAFTCCO'
  ) %>% 
  mean_flow(., date)

# get flow data for Poudre Valley Canal
poudre.valley.canal <- getCDSSDiversionFlow(
  wdid        = '0300907'
  ) %>% 
  mean_flow(., date)

# get flow data for North Fork Poudre river
north.fork <- GetCDSSStationFlow(
  site_abbrev = 'CLANSECO'
  ) %>% 
  mean_flow(., date)

# get flow data for the North Poudre Supply Canal
poudre.supply.canal <- getCDSSDiversionFlow(
  wdid        = '0300905'
  ) %>% 
  mean_flow(., date)

# get flow data at Poudrepark <- TARGET DATA
poudre.park <- getOpenDataFlow(
  sensor_name = "Poudre Park"
  ) %>% 
  mean_flow(., date)


# =====================================================
# ---- flow comparison: Poudre Park & Canyon Mouth ----

# Join Poudre Park & Poudre Canyon mouth
pp_pcm <- poudre.park %>%
  inner_join(
    poudre.canyon.mouth, 
    by = 'date'
    ) %>%
  rename(
    flow_pp  = flow.x,
    flow_pcm = flow.y
  ) %>%
  mutate(flow_diff = flow_pp - flow_pcm)

# Key observations:
#
# - During early spring and early Autumn, flows at the Canyon Mouth are lower
#   than flows at Poudre Park. This is likely caused by diversions at the 
#   N. Poudre Supply Canal and Poudre Valley Canal
#
# - During Spring and early summer, flows at the Canyon Mouth are greater than
#   Poudre Park, likely due to flow contributions from the N. Fork (Seaman Res)
#

# Timeseries of flow at Poudre Canyon Mouth (red) & Poudre Park 
ggplot() +
  geom_point(data = pp_pcm, 
             aes(x = date, y = flow_pp, col = "Poudre Park"),
             alpha = 0.5) +
  geom_point(data = pp_pcm, 
             aes(x = date, y = flow_pcm, col = "Poudre Canyon Mouth"), 
             alpha = 0.5) +
  labs(
    title  = "Flow at Poudre Park and Poudre Canyon Mouth",
    y      = "Flow (cfs)",
    x      = "Date",
    colour = " "
  ) +
  scale_colour_manual(values = c("red", "black")) +
  scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  th
ggsave(paste0(plot_path,'/poudrepark_canyonmouth_timeseries.png'))

# Poudre park flow vs. Poudre Canyon mouth flow
ggplot() +
  geom_point(data = pp_pcm, 
             aes(x = flow_pcm, y = flow_pp),
             alpha = 0.5) +
  labs(
    title = "Flow at Poudre Park vs Poudre Canyon Mouth",
    y     = "Poudre Canyon Mouth flow (cfs)",
    x     = "Poudre Park flow (cfs)"
  ) +
  geom_abline(slope=1) +
  th
ggsave(paste0(plot_path,'/poudrepark_v_canyonmouth.png'))

# Difference in flows between Poudre Park and caynon Mouth
ggplot() +
  geom_point(data = pp_pcm, aes(x = date, y = flow_diff)) +
  labs(
    title  = "Flow difference: Poudre Park and Poudre Canyon Mouth",
    y      = "Flow (cfs)",
    x      = "Date",
    colour = " "
  ) +
  geom_hline(yintercept = 0) +
  ylim(-750, 750) +
  annotate('text', 
    x     = as.Date('2019-10-01'), y = 700, 
    label = "Poudre Park > Canyon Mouth",
    size  = 6
    ) +
  annotate('text', 
    x     = as.Date('2019-10-01'), 
    y     = -700, 
    label = "Canyon Mouth > Poudre Park",
    size  = 6
    ) +
  scale_x_date(date_breaks       = "4 months",
               date_minor_breaks = "1 month",
               date_labels       = "%Y-%m") +
  th
ggsave(paste0(plot_path,'/poudrepark_canyonmouth_diff.png'))


# ==========================================================
# ---- Assess managed flows - diversions and reservoirs ----
# ==========================================================

# Key observations:
#
# - The Poudre Supply Canal is the largest diversion between Poudre Park and
#   Canyon Mouth. It withraws water throughout the summer and autumn, with
#   the largest withdraw occuring in the late summer. 
#
# - The Poudre Valley Canal flows between spring and mid summer. Data are spotty
#
# - The North Fork exhibits a seasonal flow variation that peaks in late spring
#   and early summer - similar to the natural flow regime. Flows are curtailed
#   in the late summer through winter. 
#

ggplot() +
  geom_point(data = poudre.valley.canal, 
             aes(x = date, y = flow, col = 'Poudre Valley Canal')
             ) +
  geom_point(data = north.fork, 
             aes(x = date, y = flow, col = 'North Fork')
             ) +
  geom_point(data = poudre.supply.canal, 
             aes(x = date, y = flow, col = 'N. Poudre Supply Canal')
             ) +
  labs(
    title  = "Poudre River managed flows",
    y      = "Flow (cfs)",
    x      = "Date",
    colour = " "
  ) +
  scale_colour_manual(values = c("red", "gray", 'blue')) +
  ylim(0, 750) +
  scale_x_date(date_breaks       = "6 months",
               date_minor_breaks = "1 month",
               date_labels       = "%Y-%m",
               limits = c(min(poudre.park$date), 
                          as.Date('2020-10-01'))
               ) +
  th
ggsave(paste0(plot_path,'/managed_flows.png'))
  
# ==================================
# ---- Build and evaluate model ----
# ==================================

# assumptions and liberties taken
# - replace NAs with zeros @ N Fk, valley canal, and supply canal.
# - The N. Fork record only goes back to ~2004, so after that we are
#   effectively ignoring inflows from the N. Fork.
# - remove simulated flow values less than zero, which occur because
#   N. Fk flows are over estimated

poudre.park.model <- 
  poudre.canyon.mouth %>%
  left_join(north.fork,          by = 'date') %>%
  left_join(poudre.valley.canal, by = 'date') %>%
  left_join(poudre.supply.canal, by = 'date') %>%
  rename(
    canyon  = flow.x,
    northfk = flow.y,
    valley  = flow.x.x,
    supply  = flow.y.y
    ) %>%
  replace_na(list(valley = 0, supply = 0, northfk = 0 )) %>%
  mutate(
    model = canyon - northfk + supply + valley
    )

all.flows <- 
  poudre.park.model %>%
  inner_join(
    poudre.park, by = 'date'
    ) %>%
  rename(target = flow)

# some error statistics
RMSE_baseline <- (mean(abs(all.flows$canyon - all.flows$target)^2))^(0.5)
logger::log_info('RMSE (Canyon Mouth v. Poudre Park): {RMSE_baseline}')

RMSE_model    <- (mean(abs(all.flows$model  - all.flows$target)^2))^(0.5)
logger::log_info('RMSE (Model v. Poudre Park): {RMSE_model}')

MAE_baseline  <- mean(abs(all.flows$canyon  - all.flows$target))
logger::log_info('MAE (Canyon Mouth v. Poudre Park): {MAE_baseline}')

MAE_model     <- mean(abs(all.flows$model   - all.flows$target))
logger::log_info('MAE (Model v. Poudre Park): {MAE_model}')

# simulations v. observations
ggplot() +
  geom_point(data = all.flows, 
             aes(x = model, y = target, col = 'mass balance model'),
             alpha = 0.2
             ) +
  geom_point(data = all.flows, 
             aes(x = canyon, y = target, col = 'no model (Canyon flow)'),
             alpha = 0.2
             ) +
  labs(
    title  = "Flow model assessment",
    y      = "Observed flow @ Poudre Park (cfs)",
    x      = "Simulated flow @ Poudre Park (cfs)",
    colour = " "
  ) +
  scale_colour_manual(values = c("red", "black")) +
  geom_abline(slope=1) +
  th
ggsave(paste0(plot_path,'/simulated_flow_v_observed_poudrepark.png'))

# simulated and observed (Poudre Park) flow time series
ggplot() + 
  geom_point(data = poudre.park.model,
             aes(x = date, y = model, col = 'MODELED  Pouder Park flow'),
             alpha = 0.5
             ) +
  geom_point(data = poudre.park,
             aes(x = date, y = flow,  col = 'OBSERVED Poudre Park flow'),
             alpha = 0.5
             ) +
  labs(
    title  = "Model time series",
    y      = "Flow (cfs)",
    x      = "Date",
    colour = " "
  ) +
  xlim(min(poudre.park$date), max(poudre.park$date)) +
  scale_colour_manual(values = c("red", "black")) +
  scale_y_continuous(breaks = seq(0, 3000, by = 500),
                     limits = c(0, 3000)) +
  th
ggsave(paste0(plot_path,'/simulated_and_observed_timeseries_poudrepark.png'))

# ==============================================
# ---- sAVE Poudre Park modeled flow at RDS ----
# ==============================================

# save data to disk as RDS
path <- here::here("boatable_days")
filename <- 'simulated_historical_pineview_flow.RDS'
logger::log_info(
  'saving simulated flow at Pineview as {paste0(path, "/", filename)}'
)
pineview.model <- poudre.park.model %>%
  rename(flow = model) %>%
  select(date, flow)

saveRDS(pineview.model, paste0(path, "/", filename))
