library(tidyverse)

path <- "C:/Users/angus/OneDrive/Desktop/lynker/AWW/data/"

# Poudre Rock Stage 
rock_report <- readRDS(paste0(path, "rock_report/stage_poudre_rock_report.rds"))

# Poudre park flow 
poudre_park_flow <- readRDS(paste0(path,  paste0(path, "poudre_park/poudre_park_flow.rds"))

# Monthly average and standard deviations of Pine View stage observations
stage_pv <- rock_report %>%
  mutate(
    year = year(date), 
    month = month(date)
    ) %>%
  group_by(year, month) %>%
  summarize(
    stage_av = mean(Pineview, na.rm = T), 
    stage_sd = sd(Pineview, na.rm = T)
    ) %>% 
  mutate(
    date = as.Date(paste0(year, "-", month, "-01"))
  ) 

# remove NA values
stage_pv <- stage_pv[-which(is.na(stage_pv$stage_sd)),]

# join monthly average stage data to upper canyon reaches
# stage_flow <- sim_mly_corr_saveout %>%
#   filter(Name.Description != "BELOW FILTER PLANT TO PICNIC ROCK ACCESS") %>%
#   mutate(year = year(date),
#          month = month(date)) %>%
#   inner_join(stage_pv, by = c("year", "month"))
