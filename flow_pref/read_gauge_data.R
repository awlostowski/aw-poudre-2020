remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(rstudioapi)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Set working directory to source file location
source_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(source_path))

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Load and organize gauge flow observations
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#--------------------------------------------------------------------------

# daily streamflow observations from key gauges used to assess rafting flows @ American Whitewater segments
setwd(dirname(source_path))
setwd("../data/gauge")
obs_flow = read.csv("poudre_gauge_data.csv")

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
inven <- read.csv("poudre_gauge_inventory.csv")

# join guage names
obs_flow_daily <- left_join(obs_flow_daily, inven[,c("ID", "Name.Description")], by = "ID")

# save flow re-shaped flow data as RData file
setwd(dirname(source_path))
save(obs_flow_daily, file = "obs_flow.Rdata")

# need to manually ADD North Fork below seaman (CLANSECO) - this is not included in the .TSTool grab for some reason. 
# same thing for La Poudre Pass below long draw reservoir (LAPLODCO)