remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(here)
library(tidyverse)
library(MASS)
library(scales)
library(lubridate)

# This script will analyze tidy-d survey response data

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Load tidy survey response data
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load(file = here::here("flow_pref","survey-analysis","example-development","cataract-canyon","respondend-attributes.Rdata"))
load(file = here::here("flow_pref","survey-analysis","example-development","cataract-canyon","flow-pref-data.Rdata"))

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Create a quick box and whisker plot to visualize results
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flowpref.median <- flowpref.dat %>%
  group_by(flow) %>%
  summarize(pref.average = mean(preference.code, na.rm = T),
            pref.std = sd(preference.code, na.rm = T))

p2 <- ggplot(flowpref.dat, aes(x = as.numeric(flow), y = preference.code)) +
  geom_jitter(width = 100, height = 0.5, alpha = 0.2) +
  geom_point(data = flowpref.median, aes(x = as.numeric(flow), y = pref.average), color = "red", size = 4) +
  labs(x = "Flow (cfs)",
       y = "Acceptability Score") +
  geom_hline(yintercept = 0)

print(p2)

# note: consider log-transforming and/or making scatter width vary with flow spacing. So, a much
# larger scatter width aroung higher flow responses, compared to low flow responses. 

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# potential for conflict index
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

results <- flowpref.dat %>%
  filter(is.na(preference.code) == 0) %>%
  group_by(as.numeric(flow)) %>%
  summarize(pref.average = mean(preference.code),
            d = sum(abs(dist(preference.code))),
            max = sum(abs(dist(rep(c(-3,3),n())))),
            pci2 = d/max) %>%
  rename(flow = `as.numeric(flow)`)

p3 <- ggplot(results, aes(x = flow, y = pref.average, size = pci2)) +
  geom_point() +
  labs(x = "Flow (cfs)",
       y = "Preference Score")

print(p3)