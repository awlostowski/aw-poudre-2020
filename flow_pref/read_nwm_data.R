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

#----------------------------------------------------------
# Load Data

# change working directory
setwd("../data/nwm")

# read nwm data from .csv file
dat <- read.csv("nwm_streamflow_cms_timeseries_1993_2017_retro.csv")

# load NHD reach attributes and AW reach names
rchs <- read.csv("nhd_reach_attributes.csv")

#-----------------------------------------------------------
# reshape and massage data

# gather data into a longer format
flow <- dat %>%
  gather("featureID", "flow_cms", 2:11)

# convert NHD featureID from string to numeric
flow$featureID = as.numeric(substring(flow$featureID,2,nchar(flow$featureID)))
rchs$featureID = as.numeric(rchs$featureID)

# convert date from factor to numeric
flow$time_utc = as.Date(substring(as.character(flow$time_utc),1,10))

# join AW reach names
flow <- left_join(flow, rchs[,c("river_sect","featureID")], by = "featureID")

