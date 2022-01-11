# Script for processing the Poudre River survey data

# Keith Jennings
# kjennings@lynkertech.com
# 2020-06-30

# Load packages
library(tidyverse)
library(reshape2)
library(here)

################################################################################
# Poudre Survey respondent attributes

# Import raw survey data and headers
haeder_file <- here::here('private_data','poudre_survey_headers.csv')
headers <- unlist(strsplit(readLines(haeder_file), ","))

survey_file <- here::here('private_data','poudre_survey_20220105.csv')
survey <- read_csv(survey_file, col_names = headers, trim_ws = TRUE )

# Remove all personal
# and unnecessary info
survey <- survey %>% 
  select(-(collector_id:name)) %>% 
  select(-(email:street_address))

# Remove the following respondent IDs
# 3 are test cases, and one gave offensive responses
respondent_ids_invalid <- c(11572572007,
                            11573246140,
                            11573682648,
                            11584994980)
survey <- survey %>% 
  filter(!respondent_id %in% respondent_ids_invalid)

# Refactor the city data to be homogenous
survey <- survey %>% 
  mutate(city = toupper(city)) %>% 
  mutate(city = case_when(city == "BOLDER" ~ "BOULDER",
                          city == "CB" ~ "CRESTED BUTTE",
                          city == "FT COLLINS" ~ "FORT COLLINS",
                          city == "FORT COLLINA" ~ "FORT COLLINS",
                          TRUE ~ city))

# Refactor the state data to be homogenous and use two-letter abbrev
survey <- survey %>% 
  mutate(state = toupper(state)) %>% 
  mutate(state = case_when(str_detect(string = state, pattern = "COLO")~ "CO",
                           str_detect(string = state, pattern = "RTH CAR")~ "NC",
                           str_detect(string = state, pattern = "OREG")~ "OR",
                           str_detect(string = state, pattern = "WYOM")~ "WY",
                           TRUE ~ state))

# Create a skill column that aggregates the multiple skill responses
# Assign all users an experience code
survey <- survey %>% 
  mutate(skill = case_when(!is.na(skill_novice) ~ "novice",
                           !is.na(skill_intermediate) ~ "intermediate",
                           !is.na(skill_advanced) ~ "advanced",
                           !is.na(skill_expert) ~ "expert"))

# Public/commercial
# Note: case_when evaluated in order
# There are no respondents who entered both commercial guide and guest,
# but there 18 respondents who entered both guide and public
# They are classified as "guide"
survey <- survey %>% 
  mutate(category = case_when(!is.na(cat_commercial_customer) ~ "customer",
                              !is.na(cat_commercial_guide) ~ "guide",
                              !is.na(cat_public) ~ "public"))


# Order factors:
# This way they appear in an order that makes sense when plotting
survey <- survey %>% 
  mutate(skill = factor(skill, 
                        levels = c("novice", 
                                   "intermediate",
                                   "advanced",
                                   "expert")),
         visit_freq = factor(visit_freq, 
                             levels = c("1 time a season",
                                        "2-5 times a season",
                                        "5-20 times a season",
                                        "20+ times a season",
                                        "50+ times a season")),
         report_confidence = factor(report_confidence, 
                                    levels = c("Not comfortable at all",
                                               "somewhat uncomfortable",
                                               "neutral",
                                               "somewhat comfortable",
                                               "very comfortable")),
         trip_length = factor(trip_length,
                              levels = c("0-5 miles",
                                         "5-10 miles",
                                         "10-20 miles",
                                         "20-50 miles",
                                         "50+ miles")))

# Add numeric values for visits and trip length
# These are abstractions based on the recorded categories
survey <- survey %>% 
  mutate(visit_n = case_when(as.integer(visit_freq) == 1 ~ 1,
                             as.integer(visit_freq) == 2 ~ 3.5,
                             as.integer(visit_freq) == 3 ~ 12.5,
                             as.integer(visit_freq) == 4 ~ 35,
                             as.integer(visit_freq) == 5 ~ 50),
         trip_length_miles = case_when(as.integer(trip_length) == 1 ~ 2.5,
                                       as.integer(trip_length) == 2 ~ 7.5,
                                       as.integer(trip_length) == 3 ~ 15,
                                       as.integer(trip_length) == 4 ~ 35,
                                       as.integer(trip_length) == 5 ~ 50))

# Change more_visits_pwp to a yes/no column
survey$more_visits_pwp <- ifelse(is.na(survey$more_visits_pwp),
                                 NA,
                                 ifelse(survey$more_visits_pwp == "No",
                                        "n",
                                        "y"))

# Export the respondent attributes
# Variables for now are similar to those exported by Adam in earlier version
survey %>% 
  select(respondent_id, city, state, zip_code, skill, skill_rapidclass, 
         category, visit_freq, report_confidence, more_visits_pwp, visit_n, 
         trip_length_miles) %>% 
  saveRDS(object = .,
          file = "private_data/respondent-attributes_20220105.RDS")


################################################################################
# Flow preference

# Import the reach codes for splitting out survey
reach_codes <- read_csv("private_data/poudre_survey_reach_codes.csv")

# Make dummy data frame
flowpref.dat <- data.frame()

# Loop through and format flow preference by reach
for(i in 1:length(reach_codes$code)){
  
  # Identify the reach code
  # reach name and flow or stage designation
  reach_code = as.character(reach_codes[i, "code"])
  reach_name = as.character(reach_codes[i, "name"])
  stage_or_flow = as.character(reach_codes[i, "stage_or_flow"])
  
  # Select only respondent id and columns matching reach code
  # And rename all columns by removing reach code prefix
  tmp <- survey %>% 
    select(respondent_id, starts_with(reach_code)) %>% 
    rename_all(funs(str_replace_all(., paste0(reach_code, "_"), "")))
  
  # Make a craft column from multiple columns
  tmp <- tmp %>% 
    mutate(craft = case_when(!is.na(craft_kayak) ~ "kayak",
                             !is.na(craft_raft_shredder) ~ "raft_shredder",
                             !is.na(craft_packraft_inflatable) ~ "packraft_inflatable",
                             !is.na(craft_canoe) ~ "canoe",
                             !is.na(craft_sup) ~ "sup",
                             !is.na(craft_other) ~ "other"))
  
  # Make data vertical
  tmp.melt <- tmp %>% 
    select(respondent_id, craft, starts_with("flow_pref")) %>% 
    melt(., value.name = "preference", variable.name = "flow", 
         id.vars = c("respondent_id", "craft")) %>% 
    mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                       str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                       preference == "Marginal" ~ 0,
                                       preference == "Moderately Acceptable" ~ 1,
                                       preference == "Acceptable" ~ 2), 
           segment.name = reach_name)
  
  # Convert values to flow or stage
  if(stage_or_flow == "flow"){
    tmp.melt <- tmp.melt %>% 
      mutate(flow = as.numeric(str_replace_all(flow, "flow_pref_", "")))
  } else {
    tmp.melt <- tmp.melt %>% 
      mutate(flow = as.numeric(str_replace_all(flow, "flow_pref_", "")) / 100)
  }
  
  # Bind to other data
  flowpref.dat <- bind_rows(flowpref.dat, tmp.melt)
  
}

# Export the flow preference data
saveRDS(object = flowpref.dat,
        file = "private_data/flow-pref-data_20220105.RDS")


################################################################################
# Econ data

# Function for removing all non-numeric strings but - and .
char_remove <- function(x) gsub("[^0-9\\.-]", "", x)
sep_string = "[^0-9\\.]"

# Selecting and formatting only the economic data
# This ugly bit of code is necessary to get the meaningful data (i.e. $ spent)
# out of the messy responses that include special characters, strings, etc.
econ_survey <- select(survey, respondent_id, starts_with("spend")) %>% 
  mutate_at(vars(starts_with("spend")), 
            funs(char_remove)) %>%  # remove all non-numeric chars but "." and "-"
  separate(spend_food, c("spend_food_lo", "spend_food_hi"), 
           sep = sep_string) %>% # split at all character vectors except "."
  separate(spend_gas, c("spend_gas_lo", "spend_gas_hi"), 
           sep = sep_string) %>% 
  separate(spend_lodging, c("spend_lodging_lo", "spend_lodging_hi"), 
           sep = sep_string) %>% 
  separate(spend_equipment, c("spend_equipment_lo", "spend_equipment_hi"), 
           sep = sep_string) %>% 
  separate(spend_clothing, c("spend_clothing_lo", "spend_clothing_hi"), 
           sep = sep_string) %>%  
  separate(spend_other, c("spend_other_lo", "spend_other_hi"), 
           sep = sep_string) %>% 
  mutate_all(as.numeric) # convert all strings to numeric

# Manually replace some values
# 11618383286	520 for gas lo (reads 5 to 20 USD)
# 11585058528	1500.151 for equip lo (should be just 1500)
# 11614506861	2000020 for equop hi (20,000 over 20 years >> change to 1000 as average)
# 11590054490 switch 200 to food hi 
econ_survey <- econ_survey %>% 
  mutate(spend_gas_lo = case_when(respondent_id == 11618383286 ~ 5,
                                  TRUE ~ spend_gas_lo),
         spend_gas_hi = case_when(respondent_id == 11618383286 ~ 20,
                                  TRUE ~ spend_gas_hi),
         spend_equipment_lo = case_when(respondent_id == 11585058528 ~ 1500,
                                    TRUE ~ spend_equipment_lo),
         spend_equipment_hi = case_when(respondent_id == 11614506861 ~ 1000,
                                       TRUE ~ spend_equipment_hi),
         spend_food_hi = case_when(respondent_id == 11590054490 ~ 200,
                                   TRUE ~ spend_food_hi),
         spend_other_lo = case_when(respondent_id == 11590054490 ~ 0,
                                    TRUE ~ spend_other_lo))

# Make means for lo and hi spending
econ_survey <- econ_survey %>% 
  rowwise() %>% 
  mutate_all(as.numeric) %>% 
  mutate(spend_food = mean(c(spend_food_lo, spend_food_hi), na.rm = T),
         spend_gas = mean(c(spend_gas_lo, spend_gas_hi), na.rm = T),
         spend_lodging = mean(c(spend_lodging_lo, spend_lodging_hi), na.rm = T),
         spend_equipment = mean(c(spend_equipment_lo, spend_equipment_hi), na.rm = T),
         spend_clothing = mean(c(spend_clothing_lo, spend_clothing_hi), na.rm = T),
         spend_other = mean(c(spend_other_lo, spend_other_hi), na.rm = T),
         spend_total = sum(c(spend_food, spend_gas, spend_lodging, 
                             spend_equipment, spend_clothing, spend_other), 
                           na.rm = T),
         spend_total_no_equip = sum(c(spend_food, spend_gas, spend_lodging, 
                                      spend_clothing, spend_other), 
                                    na.rm = T)) %>% 
  select(respondent_id, spend_food:spend_total_no_equip) %>% 
  ungroup()

# Export the econ data
saveRDS(object = econ_survey,
        file = "private_data/econ_survey_20220105.RDS")


# SUmmarize the survey data
# Break out by spending category
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

# Add margins
margins <- data.frame(category = c("clothing", "food", "gas", "lodging",
                                   "other", "souvenirs"),
                      margin = c(0.3785274, 0.2543902, 0.1418711, 1.0000000,
                                 0.3785274, 0.3785274))

# Join
survey_summary <- left_join(survey_summary, margins,
                            by = "category")

# Add total spent
spend_total = mean(filter(econ_survey, spend_total_no_equip < 450 &
                            spend_total_no_equip > 0)$spend_total_no_equip)

survey_summary <- survey_summary %>% 
  mutate(demand_change = percent * spend_total * margin)

################################################################################
# Plots
# MOVE TO DIFFERENT SCRIPT

# SKill breakdown
survey %>% 
  ggplot(aes(skill)) + 
  geom_bar(stat = "count", fill = "steelblue", color = "black") + 
  coord_flip() +
  labs(y = "Total Respondents",
       title = "Survey Respondents by Skill") +
  theme(axis.title.y = element_blank()) +
  scale_x_discrete(labels = c("Novice", "Intermediate", "Advanced", "Expert"))
