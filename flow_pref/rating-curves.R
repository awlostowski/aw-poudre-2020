remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(rstudioapi)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(cowplot); theme_set(theme_cowplot())
library(hydroTSM)
library(RColorBrewer)

# Set working directory to source file location
source_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(source_path))

# load monthly flow data
load("simulated_monthly_flows_corrected.Rdata")

flow_hewlett <- sim_mly_corr_saveout %>% 
  filter(Name.Description == "MISHIWAKA INN TO POUDRE PARK") %>%
  mutate(year = year(date),
         month = month(date))
  

# load rock stage observations
setwd(dirname(source_path))
setwd("../data/rock-report")
stage <- read.csv("rock-report-stage-ft.csv")
stage$Date <- as.Date(stage$Date)

# quick compare rock stages and simulations at Hewlett Bridge

stage_hewlett <- stage %>%
  select(Date, Hewlett) %>%
  mutate(month = month(Date),
         year = year(Date)) %>%
  group_by(year, month) %>%
  summarize(stage = mean(Hewlett, na.rm = T))

stage_flow_hewlett <- stage_hewlett %>%
  left_join(flow_hewlett, by = c("year", "month"))

