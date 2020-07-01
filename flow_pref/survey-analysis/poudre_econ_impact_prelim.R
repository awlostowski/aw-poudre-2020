#' ---
#' title: "Economic Impact of Commercial Rafting on the Poudre River"
#' author: "Lynker"
#' date: "2020-06-26"
#' output: github_document
#' ---
#'
#' This script analyzes the economic impact of commercial r5,116,999


# Import survey data
headers <- unlist(strsplit(readLines("private_data/poudre_survey_headers.csv"), 
                             ","))
survey <- read_csv("private_data/poudre_survey_20200626.csv",
                   col_names = headers, trim_ws = TRUE )


# Function for removing all non-numeric strings but - and .
char_remove <- function(x) gsub("[^0-9\\.-]", "", x)
sep_string = "[^0-9\\.]"

# Selecting and formatting only the economic data
# This ugly bit of code is necessary to get the meaningful data (i.e. $ spent)
# out of the messy responses that include special characters, strings, etc.
econ_survey <- select(survey, respondent_id, starts_with("spend")) %>% 
  mutate_at(vars(starts_with("spend")), funs(char_remove)) %>% 
  separate(spend_food, c("spend_food_lo", "spend_food_hi"), sep = sep_string) %>% 
  separate(spend_gas, c("spend_gas_lo", "spend_gas_hi"), sep = sep_string) %>% 
  separate(spend_lodging, c("spend_lodging_lo", "spend_lodging_hi"), sep = sep_string) %>% 
  separate(spend_equipment, c("spend_equipment_lo", "spend_equipment_hi"), sep = sep_string) %>% 
  separate(spend_clothing, c("spend_clothing_lo", "spend_clothing_hi"), sep = sep_string) %>%  
  separate(spend_other, c("spend_other_lo", "spend_other_hi"), sep = sep_string) %>% 
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



  mutate(spend_food = mean(c())
    spend_gas = mean(c(as.numeric(spend_gas_lo), as.numeric(spend_gas_hi)), na.rm = T))


test <- bind_cols(select(survey, spend_gas),
                  select(survey, spend_gas) %>% 
                    separate(spend_gas, c("spend_gas1", "spend_gas2", "spend_gas3")))
  
  mutate(spend1 = as.numeric(str_match(spend_food,"[0-9]+")))

