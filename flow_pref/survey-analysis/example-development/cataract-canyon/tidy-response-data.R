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
