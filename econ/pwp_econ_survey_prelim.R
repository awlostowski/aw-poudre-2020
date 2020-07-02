# Script for analyzing preliminary survey data

# Keith Jennings
# kjennings@lynkertech.com
# 2020-06-15

# Load packages
library(tidyverse)
library(lubridate) # for parsing survey monkey datetimes
library(cowplot); theme_set(theme_cowplot())
library(reshape2)

###############################################################################
###############################  Import Data  #################################
###############################################################################

###############################################################################
# Import survey data
# https://www.surveymonkey.com/r/Poudre_Whitewater_Park

# Import headers
headers <- unlist(strsplit(readLines("private_data/pwp_survey_headers_20200626.csv"), 
                           ","))

# Import data
# Using read_csv and trim_ws allows non-responses to be classified as NA
survey <- read_csv("private_data/pwp_survey_20200626.csv",
                   skip = 2, col_names = headers, trim_ws = T)

# Parse the date columns
survey <- survey %>% 
  mutate_at(vars(contains("date")), list(~ mdy_hms(., tz = "US/Mountain"))) %>% 
  mutate(visit_date = as.Date(visit_datetime, tz = "US/Mountain"))

# Summarise watercraft into one column
# Add SUP and surfboard based on strings seen in the other response column
survey <- survey %>% 
  mutate(visit_watercraft_all = case_when(!is.na(visit_watercraft) ~ visit_watercraft,
                                          str_detect(toupper(visit_watercraft_other),
                                                     "SUP|PADDLE") ~ "SUP",
                                          str_detect(toupper(visit_watercraft_other),
                                                    "SURF") ~ "Surfboard"))

# Recategorize visit purposes
survey <- survey %>% 
  mutate(visit_purpose = case_when(is.na(visit_purpose) ~ "Other",
                                   visit_purpose == "Kayaking, boating, tubing, swimming, wading, or other water activity" ~ "Water activity",
                                   TRUE ~ visit_purpose))


# Make total spending columns
survey <- survey %>% 
  rowwise() %>% 
  mutate(spend_total = case_when(is.na(spend_food) & is.na(spend_gas) &
                                 is.na(spend_lodging) & is.na(spend_equip) &
                                 is.na(spend_clothing) & is.na(spend_souvenirs) &
                                 is.na(spend_other) ~ NA_real_,
                                 TRUE ~ sum(c(spend_food, spend_gas, spend_lodging, 
                                              spend_equip, spend_clothing, spend_souvenirs, 
                                              spend_other), 
                                            na.rm = T)),
         spend_total_no_equip = case_when(is.na(spend_food) & is.na(spend_gas) &
                                            is.na(spend_lodging) &
                                            is.na(spend_clothing) & is.na(spend_souvenirs) &
                                            is.na(spend_other) ~ NA_real_,
                                          TRUE ~ sum(c(spend_food, spend_gas, spend_lodging, 
                                                       spend_clothing, spend_souvenirs, 
                                                       spend_other), 
                                                     na.rm = T))) %>% 
  ungroup()
           

###############################################################################
# Make some plots

# Visit purpose
survey %>% 
  mutate(visit_purpose = fct_infreq(visit_purpose)) %>% 
  ggplot(aes(visit_purpose)) + 
  geom_bar(stat = "count", fill = "steelblue", color = "black") + 
  coord_flip() +
  labs(y = "Total Respondents",
       title = "Survey Respondents by Visit Purpose") +
  theme(axis.title.y = element_blank())


# Watercraft
survey %>% 
  filter(visit_watercraft_all %in% 
                    c("Kayak", "SUP", "Raft", "Tube", "Surfboard")) %>% 
  mutate(visit_watercraft_all = fct_infreq(visit_watercraft_all)) %>% 
  ggplot(aes(visit_watercraft_all)) + 
  geom_bar(stat = "count", fill = "steelblue", color = "black") + 
  coord_flip() +
  labs(y = "Total Respondents",
       title = "Survey Respondents by Watercraft") +
  theme(axis.title.y = element_blank())


###############################################################################
# A few analyses

# Avg trip length
mean(survey$trip_length_mi, na.rm = T)

# Avg trip expenditures
mean(survey$spend_total_no_equip, na.rm = T)

# Break out by spending category
survey_summary <- survey %>% 
  filter(spend_total_no_equip < 200 & spend_total_no_equip > 0) %>% 
  select(respondent_id, starts_with("spend"), 
         -spend_equip, -spend_total, -spend_total_no_equip) %>% 
  melt(id.vars = "respondent_id", value.name = "spend", variable.name = "category") %>% 
  mutate(category = str_replace(category, "spend_", "")) %>% 
  group_by(category) %>% 
  summarise(total = sum(spend, na.rm = T)) %>% 
  mutate(site = "PWP",
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
spend_total = mean(filter(survey, spend_total_no_equip < 200 &
                            spend_total_no_equip > 0)$spend_total_no_equip)

# Compute demand change per category taking margin into account
survey_summary <- survey_summary %>% 
  mutate(demand_change = percent * spend_total * margin)


###############################################################################
# Multipliers for impact estimates

# Import data
sales <- read.csv("econ/data/pwp_expenditures_margin.csv")
mults <- read.csv("econ/data/bea_retail_multipliers_final_demand_typei.csv")



###############################################################################
# Import Poudre gage data (for joining with flow ratings)

# Manual data entry
site_no = "06752260"
begin_date = "2019-10-01" # enter as YYYY-MM-DD format
end_date = "2020-06-15" # enter as YYYY-MM-DD format

# Download strings
dl1 = "https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&cb_00095=on&cb_00400=on&format=rdb&site_no="
dl2 = "&referred_module=sw&period=&begin_date="
dl3 = "&end_date="

# Download the data using the dl strings and the manual entries
flow <- paste0(dl1, site_no, dl2, begin_date, dl3, end_date) %>% 
  url(.) %>% 
  read.table(., skip = 34,
             col.names = c("agency", "site_no", "date", "flow", "qc")) %>% 
  mutate(date = as.Date(date))
  
###############################################################################
###############################  Import Data  #################################
###############################################################################



# Make a total expenditure column


# Concatenate visit purposes
# NA seems to correspond to "other"
