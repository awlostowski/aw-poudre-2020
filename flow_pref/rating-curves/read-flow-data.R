remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(here)
library(tidyverse)
library(lubridate)

# This script loads and formats observed and simulated flow data.

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Load and organize gauge flow observations
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#------------------------------Load from TSTool output---------------------------------------------
# daily average data obtained from TSTools
obs_flow = read.csv(here::here("data","gauge","poudre_gauge_data.csv"))

# Extract the gauge ID from the column headers
colnames(obs_flow) <- sub("\\X0", "", colnames(obs_flow))
colnames(obs_flow) <- sub("\\_Streamflow", "", colnames(obs_flow))

# convert timestamp from factor to Date
obs_flow$Date = as.Date(obs_flow$Date)
colnames(obs_flow)[1] <- "date"

# restructure gauge observations to a longer format
obs_flow <- obs_flow %>% 
  gather(ID,flow_cfs,2:17)

# remove NA values
I = which(is.na(obs_flow$flow_cfs))
obs_flow_daily = obs_flow[-I,]

# clear some variables
rm(obs_flow)

# load the inventory of gauges on the Poudre River 
inven <- read.csv(here::here("data","gauge","poudre_gauge_inventory.csv"))

# join guage names
obs_flow_tstool <- left_join(obs_flow_daily, inven[,c("ID", "Name.Description")], by = "ID")

# # ---------------------------- Analyze quantiles of canyon mounth gauge --------------------------
# obs_canyon_mouth <- filter(obs_flow_tstool, 
#                            Name.Description == "CACHE LA POUDRE RIV AT MO OF CN, NR FT COLLINS, CO") %>%
#   mutate(month = month(date)) %>%
#   filter(., month >= 3 & month <= 9)
# 
# q99 = quantile(obs_canyon_mouth$flow_cfs, 0.1)

#------------------------------Load from DWR file--------------------------------------------------

obs_flow <- read.table(file = here::here("data","gauge","CLANSECO_42120074145.txt"), sep = "\t",header = F, skip = 17,
                       col.names = c("ID", "timestamp", "flow_cfs")) %>%
  mutate(Name.Description = "N. FK. CACHE LA POUDRE RIVER BELOW SEAMAN RES.")

# re-organize columns
obs_flow <- obs_flow[c(2,1,3,4)]

# convert date from factor to date class & flow from factor to numeric
obs_flow$timestamp = as.Date(obs_flow$timestamp)
obs_flow$flow_cfs = as.numeric(as.character(obs_flow$flow_cfs))

# compute daily averages
obs_flow <- obs_flow %>%
  mutate(date = floor_date(timestamp)) %>%
  group_by(date) %>%
  summarize(ID = first(ID),
            flow_cfs = mean(flow_cfs, na.rm = T),
            Name.Description = first(Name.Description))

# remove sparse data before January 1, 2005
I = which(obs_flow$date < as.Date("2005-01-01"))
obs_flow <- obs_flow[-I,]

#------------------------------Bind observations together and save --------------------------------------------------
# bind N.Fk. data to 'obs_flow_tstool'
observations <- rbind(obs_flow_tstool,obs_flow)

# save the data locally
save(observations, file = here::here("flow_pref","rating-curves","obs_flow.Rdata"))


#---------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Load and organize nwm flow simulations
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#------------------------------Load and reshape NWM output--------------------------------------------------

# read nwm data from .csv file
dat <- read.csv(here::here("data","nwm","nwm_streamflow_cms_timeseries_1993_2017_retro.csv"))

# load NHD reach attributes and AW reach names
rchs <- read.csv(here::here("data","nwm","nhd_reach_attributes.csv"))

# gather data into a longer format
flow <- dat %>%
  gather("featureID", "flow_cms", 2:11)

# convert NHD featureID from string to numeric
flow$featureID = as.numeric(substring(flow$featureID,2,nchar(flow$featureID)))
rchs$featureID = as.numeric(rchs$featureID)

# convert date from factor to numeric
flow$time_utc = as.Date(substring(as.character(flow$time_utc),1,10))

# unit conversion: cubic meters per second to cubic feet per second
flow$flow_cms = flow$flow_cms * 35.314666212661
flow <- rename(flow, flow_cfs = flow_cms)

# join AW reach names and rename columns to align with 'observations'
simulations <- left_join(flow, rchs[,c("river_sect","featureID")], by = "featureID") %>%
  rename(date = time_utc,
         ID = featureID,
         Name.Description = river_sect)

#------------------------------Save simulated flows--------------------------------------------------
# save data
save(simulations, file = here::here("flow_pref","rating-curves","sim_flow.Rdata"))
