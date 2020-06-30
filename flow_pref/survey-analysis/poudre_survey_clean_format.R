# Script for processing the Poudre River survey data

# Keith Jennings
# kjennings@lynkertech.com
# 2020-06-30

# Load packages
library(tidyverse)
library(reshape2)

################################################################################
# Poudre Survey respondent attributes

# Import raw survey data and headers
headers <- unlist(strsplit(readLines("private_data/poudre_survey_headers.csv"), 
                           ","))
survey <- read_csv("private_data/poudre_survey_20200626.csv",
                   col_names = headers, trim_ws = TRUE )

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

# Export the respondent attributes
# Variables for now are similar to those exported by Adam in earlier version
survey %>% 
  select(respondent_id, city, state, zip_code, skill, skill_rapidclass, 
         category, visit_freq, report_confidence) %>% 
  saveRDS(object = .,
          file = "flow_pref/survey-analysis/respondent-attributes_20200630.RDS")


################################################################################
# Flow preference

# Import the reach codes for splitting out survey
reach_codes <- read_csv("private_data/poudre_survey_reach_codes.csv")

# Make dummy data frame
flowpref.dat <- data.frame()

# Loop through and format flow preference by reach
for(i in 1:length(reach_codes$code)){
  
  # Identify the reach code
  reach_code = as.character(reach_codes[i, "code"])
  reach_name = as.character(reach_codes[i, "name"])
  
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
    mutate(flow = as.numeric(str_replace_all(flow, "flow_pref_", "")),
           preference.code = case_when(preference == "Unacceptable" ~ -2,
                                       str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                       preference == "Marginal" ~ 0,
                                       preference == "Moderately Acceptable" ~ 1,
                                       preference == "Acceptable" ~ 2), 
           segment.name = reach_name)
  
  # Bind to other data
  flowpref.dat <- bind_rows(flowpref.dat, tmp.melt)
  
}

# Export the flow preference data
saveRDS(object = flowpref.dat,
        file = "flow_pref/survey-analysis/flow-pref-data_20200630.RDS")


