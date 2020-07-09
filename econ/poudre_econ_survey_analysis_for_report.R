# Script with analyses and plots for Poudre phase 1 report

# Keith Jennings
# kjennings@lynkertech.com
# 2020-07-09

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(reshape2)


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


################################################################################
# Compute spending breakdown per category

# Import poudre economic survey 
econ_survey <- readRDS(file = "private_data/econ_survey_20200630.RDS")

# Summarize
survey_summary <- econ_survey %>% 
  filter(spend_total_no_equip < 450 & spend_total_no_equip > 0) %>% 
  select(respondent_id, starts_with("spend"), 
         -spend_equipment, -spend_total, -spend_total_no_equip) %>% 
  melt(id.vars = "respondent_id", value.name = "spend", variable.name = "category") %>% 
  mutate(category = str_replace(category, "spend_", "")) %>% 
  group_by(category) %>% 
  summarise(total = sum(spend, na.rm = T)) %>% 
  mutate(site = "CAN",
         percent = total / sum(total))

# Import PWP econ survey
econ_survey_pwp <- readRDS("private_data/pwp_survey_20200630.RDS")
  
# Break out by spending category
survey_summary_pwp <- econ_survey_pwp %>% 
  filter(spend_total_no_equip < 200 & spend_total_no_equip > 0) %>% 
  select(respondent_id, starts_with("spend"), 
         -spend_equip, -spend_total, -spend_total_no_equip) %>% 
  melt(id.vars = "respondent_id", value.name = "spend", variable.name = "category") %>% 
  mutate(category = str_replace(category, "spend_", "")) %>% 
  group_by(category) %>% 
  summarise(total = sum(spend, na.rm = T)) %>% 
  mutate(site = "PWP",
         percent = total / sum(total))

# Calculate the per-person, per-day expenditure
spend_can_av = mean(filter(econ_survey, spend_total_no_equip < 450 &
                             spend_total_no_equip > 0)$spend_total_no_equip)
spend_pwp_av = mean(filter(econ_survey_pwp, spend_total_no_equip < 200 &
                             spend_total_no_equip > 0)$spend_total_no_equip)

# Add these values to each survey
survey_summary$spend_av = spend_can_av
survey_summary_pwp$spend_av = spend_pwp_av

# Bind PWP and CAN
# And compute average daily spending per category
survey_summary <- bind_rows(survey_summary, survey_summary_pwp) %>% 
  mutate(spend_av_per_cat = spend_av * percent)


# Plot
spending_by_cat_plot <- 
  survey_summary %>% 
  ggplot(aes(site, spend_av_per_cat, fill = category)) + 
  geom_bar(position = "stack", stat = "identity", alpha = 0.7, color = "black") +
  labs(x = "Location",
       y = "Average daily spending per user ($)") +
  #scale_fill_brewer(palette="Dark2")
  scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                             "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                    name = "Category",
                    labels = str_to_title(unique(survey_summary$category))) +
  scale_x_discrete(labels = c("Poudre Canyon", "Whitewater Park"))
save_plot(plot = spending_by_cat_plot,
          filename = "plots/econ/spending_by_category.png")
