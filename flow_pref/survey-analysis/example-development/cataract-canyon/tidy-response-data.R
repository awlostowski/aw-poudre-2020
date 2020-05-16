remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(here)
library(tidyverse)
library(lubridate)

# This script will load and tidy survey response data

# import survey respinse data
data <- read.csv(here::here("data","survey_monkey","canyonlands-survey.csv"))

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# tidy respondant attributes data frame
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# skill level - novice, intermediate, advanced or expert
skill_level <- data[2:nrow(data), 12:15] %>%
  rename(novice = What.skill.level.paddler.would.you.classify.yourself.as.,
         intermediate = X,
         advanced = X.1,
         expert = X.2) %>%
  mutate(skill.novice = novice != "",
         skill.intermediate = intermediate != "",
         skill.advanced = advanced != "",
         skill.expert = expert != "") %>%
  select(skill.novice, skill.intermediate, skill.advanced, skill.expert)

# skill level by rapid classification
skill_class <- data.frame(skill.class = data[2:nrow(data),16])

# basic respondent identification attributes 
id_attributes <- data[2:nrow(data), 1:11] %>%
  rename(respondent.id = Respondent.ID,
         start.date = Start.Date,
         end.date = End.Date,
         ip.address = IP.Address,
         name = Your.name,
         craft.type = What.type.of.craft.do.you.predominately.use...check.only.one.) %>%
  select(respondent.id,start.date,end.date,ip.address,name,craft.type)

# contact and location information
contact_location <- data[2:nrow(data),20:25] %>%
  rename(email = Your.Email,
         phone = Your.Phone,
         street.address = Your.Street.Address,
         city = Your.City,
         state = Your.State,
         zip.code = Your.Zip.Code)

# user type - private, commercial guide, or commercial customer
user_type <- data[2:nrow(data),17:19] %>%
  rename(private = Would.you.characterize.yourself.as.a.private.or.commercial.boater.,
         commercial.guide = X.3,
         commercial.customer = X.4) %>%
  mutate(user.type.private = private != "",
         user.type.guide = commercial.guide != "",
         user.type.customer = commercial.customer != "") %>%
  select(user.type.private, user.type.guide, user.type.customer)

# outing frequency
frequency <- data.frame(trip.frequency = data[2:nrow(data),26])

# bind respondent attributes data together 
respondent.attributes <- cbind(id_attributes, 
                               contact_location, 
                               skill_class, 
                               skill_level, 
                               user_type, 
                               frequency)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# tidy flow preference data
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flowpref.dat <- data[1:nrow(data), 29:54]

colnames(flowpref.dat) = as.character(t(flowpref.dat[1,]))

flowpref.dat <- cbind(respondent.attributes$respondent.id, flowpref.dat[-1,]) %>%
  rename(id = `respondent.attributes$respondent.id`) %>%
  gather(flow, preference, `100`:`100000`) %>%
  mutate(preference.code = case_when(preference == "Totally Unacceptable" ~ -3,
                                    preference == "Moderately Unacceptable" ~ -2,
                                    preference == "Slightly Unacceptable" ~ -1,
                                    preference == "Martinal" ~ 0,
                                    preference == "Slightly Acceptable" ~ 1,
                                    preference == "Moderately Acceptable" ~ 2,
                                    preference == "Totally Acceptable" ~ 3))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# save data for later analysis
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save(respondent.attributes, file = here::here("flow_pref","survey-analysis","example-development","cataract-canyon","respondend-attributes.Rdata"))
save(flowpref.dat, file = here::here("flow_pref","survey-analysis","example-development","cataract-canyon","flow-pref-data.Rdata"))


  
