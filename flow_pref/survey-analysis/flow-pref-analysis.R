remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(here)
library(tidyverse)
library(lubridate)

# This script will analyze tidy-d survey response data

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Load tidy survey response data
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load(file = here::here("flow_pref","survey-analysis","respondend-attributes.Rdata"))
load(file = here::here("flow_pref","survey-analysis","flow-pref-data.Rdata"))


#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Analyze the data
#
# 1) Calculate the average preference score for each flow bin
# 2) Calculate the Pottential for Conflict Index (PCI2) statistic
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

segment_name = "Big South"


results <- flowpref.dat %>%
  
  # Remove NAs and select a specific segment
  filter(is.na(preference.code) == 0 & segment.name == segment_name) %>%
  
  # calculate the average pref score and PCI2 statistic of each flow bin
  group_by(as.numeric(flow)) %>%
  summarize(pref.average = mean(preference.code),   # average preference
            d = sum(abs(dist(preference.code))),    # sum of the score distance vector
            m = sum(abs(dist(rep(c(-3,3),n())))),   # maximum possible sum of distance vector
            pci2 = d/m) %>%                         # PCI2 = d/m
  
  # rename a variable
  rename(flow = `as.numeric(flow)`)


#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Visualize results
#
# p1) Prefernece score against flow, PCI indicated by marker size.
#     all responses overlain as smaller markers
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


p1 <- ggplot() +
  geom_point(data = results, aes(x = flow, y = pref.average, size = pci2), color = 'blue') +
  geom_jitter(data = filter(flowpref.dat, segment.name == segment_name), aes(x = as.numeric(flow), y = preference.code), size = 1, alpha = 0.3) +
  labs(x = "Flow (cfs)",
       y = "Preference Score") +
  geom_hline(yintercept = 0)

print(p1)