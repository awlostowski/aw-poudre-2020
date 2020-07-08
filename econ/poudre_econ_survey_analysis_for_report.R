# Script with analyses and plots for Poudre phase 1 report

# Keith Jennings
# kjennings@lynkertech.com
# 2020-07-09

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())


################################################################################
##############################  Poudre Canyon  #################################
################################################################################

# Import data
survey_can <- readRDS("private_data/respondent-attributes_20200630.RDS")
flow_pref <- readRDS("private_data/flow-pref-data_20200630.RDS")

################################################################################
# Compute the number of respondents per reach

# First identify in the flow preference data
# Whether a respondent gave flow pref values for a given reach
flow_pref_by_respondent <- flow_pref %>% 
  mutate(response = case_when(!is.na(preference) ~ 1,
                              TRUE ~ 0)) %>% 
  group_by(respondent_id, segment.name) %>% 
  summarise(response = case_when(sum(response) > 0 ~ 1,
                                 TRUE ~ 0))

# Create thresholds for each category
skill_thresh = "novice"
visit_thresh = "1 time a season"
confidence_thresh = "Not comfortable at all"

# Filter to respondent IDs above the thresholds
respondent.id.valid = survey_can %>% 
  filter(., skill != skill_thresh,
         visit_freq != visit_thresh,
         report_confidence != confidence_thresh) %>% 
  pull(., respondent_id)

# Add whether a respondent was included or not
flow_pref_by_respondent <- flow_pref_by_respondent %>% 
  mutate(valid = case_when(respondent_id %in% respondent.id.valid &
                             response == 1 ~ 1,
                           TRUE ~ 0))

# Calculate respondents per reach with and without invalid respondents
flow_pref_n_per_reach <- flow_pref_by_respondent %>% 
  group_by(segment.name) %>% 
  summarise(n_all = sum(response),
            n_val = sum(valid)) %>% 
  mutate(n_diff_tot = n_all - n_val,
         n_diff_pct = n_diff_tot / n_all * 100)
