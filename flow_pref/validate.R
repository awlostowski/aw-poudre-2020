remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(rstudioapi)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(cowplot); theme_set(theme_cowplot())

# Set working directory to source file location
source_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(source_path))

# load the nwm-simulated and observed streamflow data sets
load(file = "obs_flow.Rdata")
obs <- obs_flow_daily; rm(obs_flow_daily)
load(file = "sim_flow.Rdata")
sim <- flow; rm(flow)


#--------------------------------------------------------
# Monthly average obs & sim flows near the canyon mouth

# Simulations @ "POUDRE PARK PICNIC GOUND TO BELOW PINE VIEW FALLS", FeatureID == 2899363
sim_mouth <- filter(sim, featureID == 2899363) %>%
  mutate(month = month(time_utc),
         year = year(time_utc)) %>%
  group_by(year, month) %>%
  summarize(simulated_flow_cfs = mean(flow_cms) * 35.314666212661) # average monthly flow converted to cfs

# Observations @ "CACHE LA POUDRE RIV AT MO OF CN, NR FT COLLINS, CO", ID == 6752000
obs_mouth <- filter(obs, ID == 6752000) %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(year, month) %>%
  summarize(observed_flow_cfs = mean(flow_cfs))

#-------------------------------------------------------
# join monthly average flow data frames by common years and months
joined_flow <- left_join(sim_mouth, obs_mouth, by = c("year","month")) %>%
  mutate(date = as.Date(paste(year,month,"01", sep = "-")))

#------------------------------------------------------
# diagnositc plots

# plot simulated versus observed monthly average flows
p1 <- ggplot(joined_flow, aes(x = log(observed_flow_cfs), y = log(simulated_flow_cfs))) +
  geom_point() +
  labs(x = "ln Observed flow (cfs)",
       y = "ln Simulated flow (cfs)") +
  ggtitle("Cache La Poudre near mouth of canyon") +
  geom_abline(slope = 1)

print(p1)

# plot overlapping time series data
joined_flow_reshape <- joined_flow %>%
  gather(source, flow_cfs, simulated_flow_cfs:observed_flow_cfs)

p2 <- ggplot(joined_flow_reshape, aes(x = date, y = flow_cfs, color = source)) +
  geom_line() +
  labs(y = "Flow (cfs)")

print(p2)


