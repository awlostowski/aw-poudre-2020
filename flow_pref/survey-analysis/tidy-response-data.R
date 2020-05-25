remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(here)
library(tidyverse)
library(lubridate)

# This script will load and tidy survey response data

# import survey respinse data
data <- read.csv(here::here("data","survey_monkey","poudre-survey-20200525.csv"))

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# tidy respondant attributes data frame
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# skill level - novice, intermediate, advanced or expert
# *************************************************************************

# skill level data are contained between cols 11 through 14
skill_level <- data[2:nrow(data), 11:14] %>%
  
  # appoint more intuitive column names
  rename(novice = "How.would.you.describe.yourÂ.skill.level.on.the.river.",
         intermediate = X,
         advanced = X.1,
         expert = X.2) %>%
  
  # logical interpretation of the data.
  mutate(skill.novice = novice != "",
         skill.intermediate = intermediate != "",
         skill.advanced = advanced != "",
         skill.expert = expert != "") %>%
  select(skill.novice, skill.intermediate, skill.advanced, skill.expert)     # !? R restart sometimes needed to get through this line. Library confusion?

# skill level by rapid classification
# *************************************************************************

# skill rapid classification data are contained in column 15
skill_class <- data.frame(skill.class = data[2:nrow(data),15])

# basic respondent identification attributes 
# *************************************************************************

# ID attributes contained in column 1 through 10
id_attributes <- data[2:nrow(data), 1:10] %>%
  
  # rename culumns to something more intuitive
  rename(respondent.id = Respondent.ID,
         start.date = Start.Date,
         end.date = End.Date,
         ip.address = IP.Address,
         name = Your.name) %>%
  
  # select and arrange columns columns
  select(respondent.id,start.date,end.date,ip.address,name)

# contact and location information
# *************************************************************************

# contact and location information contained in columns 19 through 24
contact_location <- data[2:nrow(data),19:24] %>%
  
  # rename columns to something more intuitive
  rename(email = Your.Email,
         phone = Your.Phone,
         street.address = Your.Street.Address,
         city = Your.City,
         state = Your.State,
         zip.code = Your.Zip.Code)

# user type - private, commercial guide, or commercial customer
# *************************************************************************

# user type information contained in columns 16 through 18
user_type <- data[2:nrow(data),16:18] %>%
  
  # rename columns to something more intuitive
  rename(private = "Would.you.characterize.yourself.as.aÂ.public.or.commercial.boater.",
         commercial.guide = X.3,
         commercial.customer = X.4) %>%
  mutate(user.type.private = private != "",
         user.type.guide = commercial.guide != "",
         user.type.customer = commercial.customer != "") %>%
  select(user.type.private, user.type.guide, user.type.customer)

# outing frequency
# *************************************************************************

# outing frequency response is in column 25
frequency <- data.frame(trip.frequency = data[2:nrow(data),25])

# cofidence of respondent in accurately assessing flow conditions
# *************************************************************************

# outing frequency response is in column 25
confidence <- data.frame(reporting.confidence = data[2:nrow(data),26])



# bind respondent attributes data together
# *************************************************************************

respondent.attributes <- cbind(id_attributes, 
                               contact_location, 
                               skill_class, 
                               skill_level, 
                               user_type, 
                               frequency,
                               confidence)

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# tidy flow preference data
#
# Grab and re-format flow preference response data
# This is uniquely done for each of the river segments surveyed
# Need to be careful in selecting the appropriate column idices and names!
# Bind responses from each segment together into one big long-formatted df
# 
# river segment-specific information: 
# 1) preferred craft type for the segment.
# 2) flow preference assessment 
# 3) a handful of open-ended questions
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--WHITEWATER PARK---------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *********************** WHITEWATER PARK  ********************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# creaft type
# *************************************************************************

# craft type response data for the Whitewater Park are in columns 28 through 33 
craft <- data[2:nrow(data), 28:33] %>%
  
  # appoint more intuitive column names
  rename(kayak = "What.is.your.preferred.craft.for.the.Fort.Collins.Whitewater.Park...Choose.one.",
         raft_shredder = X.5,
         packraft_inflatable_canoe = X.6,
         open_canoe = X.7,
         sup = X.8,
         other = X.9) %>%
  
  # convert data to logical, except `craft.other` which is a string 
  mutate(craft.kayak = kayak != "",
         craft.raft.shredder = raft_shredder != "",
         craft.opencanoe = open_canoe != "",
         craft.packraft.inflatecayak.canoe = packraft_inflatable_canoe != "",
         craft.sup = sup != "",
         craft.other = other) %>%
  
  # arrange columns
  select(craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other) %>%
  
  # add respondent ids to the data and rename
  cbind(respondent.attributes$respondent.id) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # arrange columns
  select(respondent.id, everything())

# flow preference
# *************************************************************************

# WWP flow preference responses are contained in columns 34 through 50
flowpref <- data[1:nrow(data), 34:50]

# change column names to the flow rates listed in te first row of flowpref
colnames(flowpref) = as.character(t(flowpref[1,]))

# add respondent id information, remove the first row that had contained column names
flowpref <- cbind(respondent.attributes$respondent.id, flowpref[-1,]) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # reshape into a long format, creating two new columns: flow & preference. 
  # Flow range surveeyd at WWP is 100 - 4000 cfs
  gather(flow, preference, `100`:`4000`) %>%
  
  # numeric coding of responses, ranging from -2 (Unacceptable) to +2 (Acceptable)
  mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                     str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                     preference == "Marginal" ~ 0,
                                     preference == "Moderately Acceptable" ~ 1,
                                     preference == "Acceptable" ~ 2)) %>%
  
  # add craft type data, joining by respondent id
  left_join(craft, by = "respondent.id") %>%
  
  # add the name of this segment
  mutate(segment.name = "Whitewater Park") %>%
  
  # rearrange columns
  select(respondent.id, segment.name, craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other, everything())

# rename for this segment
flowpref.wwp <- flowpref

#-- FILTER PLANT ----------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *********************** FILTER PLANT  ***********************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# creaft type
# *************************************************************************

# craft type response data for the filter plant segment are in columns 61 through 66 
craft <- data[2:nrow(data), 61:66] %>%
  
  rename(kayak = "What.is.your.preferred.craft.for.the.Poudre.from.Below.Filter.Plant.to.Picnic.Rock.Access...Choose.one.",
         raft_shredder = X.28,
         open_canoe = X.29,
         packraft_inflatable_canoe = X.30,
         sup = X.31,
         other = X.32) %>%
  
  mutate(craft.kayak = kayak != "",
         craft.raft.shredder = raft_shredder != "",
         craft.opencanoe = open_canoe != "",
         craft.packraft.inflatecayak.canoe = packraft_inflatable_canoe != "",
         craft.sup = sup != "",
         craft.other = other) %>%
  
  select(craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other) %>%
  
  cbind(respondent.attributes$respondent.id) %>%
  
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  select(respondent.id, everything())

# flow preference
# *************************************************************************

# Filter plant flow preference responses are contained in columns 67 through 87
flowpref <- data[1:nrow(data), 67:87]
colnames(flowpref) = as.character(t(flowpref[1,]))

flowpref <- cbind(respondent.attributes$respondent.id, flowpref[-1,]) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  gather(flow, preference, `100`:`4000`) %>%
  mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                     str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                     preference == "Marginal" ~ 0,
                                     preference == "Moderately Acceptable" ~ 1,
                                     preference == "Acceptable" ~ 2)) %>%
  
  left_join(craft, by = "respondent.id") %>%
  
  mutate(segment.name = "Filter Plant") %>%
  
  select(respondent.id, segment.name, craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other, everything())

# rename for this segment
flowpref.filter <- flowpref


#--BRIDGES ---------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *********************** BRIDGES  ****************************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# craft type
# *************************************************************************

# craft type response data for Bridges are in columns 96 through 101 
craft <- data[2:nrow(data), 96:101] %>%
  
  # appoint more intuitive column names
  rename(kayak = "What.is.your.preferred.craft.for.the.Poudre.from.Pine.View.Falls.to.Bridges.Take.out..Bridges....Choose.one.",
         raft_shredder = X.53,
         packraft_inflatable_canoe = X.54,
         open_canoe = X.55,
         sup = X.56,
         other = X.57) %>%
  
  # convert data to logical, except `craft.other` which is a string 
  mutate(craft.kayak = kayak != "",
         craft.raft.shredder = raft_shredder != "",
         craft.opencanoe = open_canoe != "",
         craft.packraft.inflatecayak.canoe = packraft_inflatable_canoe != "",
         craft.sup = sup != "",
         craft.other = other) %>%
  
  # arrange columns
  select(craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other) %>%
  
  # add respondent ids to the data and rename
  cbind(respondent.attributes$respondent.id) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # arrange columns
  select(respondent.id, everything())

# flow preference
# *************************************************************************

# Bridges flow preference responses are contained in columns 102 through 118
flowpref <- data[1:nrow(data), 102:118]

# change column names to the stage level listed in te first row of flowpref
colnames(flowpref) = as.character(t(flowpref[1,]))

# add respondent id information, remove the first row that had contained column names
flowpref <- cbind(respondent.attributes$respondent.id, flowpref[-1,]) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # reshape into a long format, creating two new columns: flow & preference. 
  # rock stage range surveeyd at Bridges is 0.25 - 5.5 ft
  gather(flow, preference, `0.25'`:`5.5'`) %>%
  
  # numeric coding of responses, ranging from -2 (Unacceptable) to +2 (Acceptable)
  mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                     str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                     preference == "Marginal" ~ 0,
                                     preference == "Moderately Acceptable" ~ 1,
                                     preference == "Acceptable" ~ 2)) %>%
  
  # add craft type data, joining by respondent id
  left_join(craft, by = "respondent.id") %>%
  
  # add the name of this segment
  mutate(segment.name = "Bridges") %>%
  
  # rearrange columns
  select(respondent.id, segment.name, craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other, everything())

# rename for this segment
flowpref.bridges <- flowpref


#--POUDRE PARK ------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *********************** POUDRE PARK  ************************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# craft type
# *************************************************************************

# craft type response data for Poudre Park are in columns 127 through 132 
craft <- data[2:nrow(data), 127:132] %>%
  
  # appoint more intuitive column names
  rename(kayak = colnames(data)[127],
         raft_shredder = colnames(data)[128],
         packraft_inflatable_canoe = colnames(data)[129],
         open_canoe = colnames(data)[130],
         sup = colnames(data)[131],
         other = colnames(data)[132]) %>%
  
  # convert data to logical, except `craft.other` which is a string 
  mutate(craft.kayak = kayak != "",
         craft.raft.shredder = raft_shredder != "",
         craft.opencanoe = open_canoe != "",
         craft.packraft.inflatecayak.canoe = packraft_inflatable_canoe != "",
         craft.sup = sup != "",
         craft.other = other) %>%
  
  # arrange columns
  select(craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other) %>%
  
  # add respondent ids to the data and rename
  cbind(respondent.attributes$respondent.id) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # arrange columns
  select(respondent.id, everything())

# flow preference
# *************************************************************************

# Poudre park flow (stage) preference responses are contained in columns 133 through 149
flowpref <- data[1:nrow(data), 133:149]

# change column names to the stage level listed in te first row of flowpref
colnames(flowpref) = as.character(t(flowpref[1,]))

# add respondent id information, remove the first row that had contained column names
flowpref <- cbind(respondent.attributes$respondent.id, flowpref[-1,]) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # reshape into a long format, creating two new columns: flow & preference. 
  # rock stage range surveeyd at Bridges is 0.25 - 5.5 ft
  gather(flow, preference, `0.25'`:`5.5'`) %>%
  
  # numeric coding of responses, ranging from -2 (Unacceptable) to +2 (Acceptable)
  mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                     str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                     preference == "Marginal" ~ 0,
                                     preference == "Moderately Acceptable" ~ 1,
                                     preference == "Acceptable" ~ 2)) %>%
  
  # add craft type data, joining by respondent id
  left_join(craft, by = "respondent.id") %>%
  
  # add the name of this segment
  mutate(segment.name = "Poudre Park") %>%
  
  # rearrange columns
  select(respondent.id, segment.name, craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other, everything())

# rename for this segment
flowpref.poudrepark <- flowpref
#- LoWER MISHAWAKA ------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *********************** LOWER MISHAWAKA  ********************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# craft type
# *************************************************************************

# craft type response data for Lower Mish are in columns 158 through 163 
craft <- data[2:nrow(data), 158:163] %>%
  
  # appoint more intuitive column names
  rename(kayak = colnames(data)[158],
         raft_shredder = colnames(data)[159],
         packraft_inflatable_canoe = colnames(data)[160],
         open_canoe = colnames(data)[161],
         sup = colnames(data)[162],
         other = colnames(data)[163]) %>%
  
  # convert data to logical, except `craft.other` which is a string 
  mutate(craft.kayak = kayak != "",
         craft.raft.shredder = raft_shredder != "",
         craft.opencanoe = open_canoe != "",
         craft.packraft.inflatecayak.canoe = packraft_inflatable_canoe != "",
         craft.sup = sup != "",
         craft.other = other) %>%
  
  # arrange columns
  select(craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other) %>%
  
  # add respondent ids to the data and rename
  cbind(respondent.attributes$respondent.id) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # arrange columns
  select(respondent.id, everything())

# flow preference
# *************************************************************************

# Lower Mishawaka flow (stage) preference responses are contained in columns 164 through 180
flowpref <- data[1:nrow(data), 164:180]

# change column names to the stage level listed in the first row of flowpref
colnames(flowpref) = as.character(t(flowpref[1,]))

# add respondent id information, remove the first row that had contained column names
flowpref <- cbind(respondent.attributes$respondent.id, flowpref[-1,]) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # reshape into a long format, creating two new columns: flow & preference. 
  # rock stage range surveyed is 0.25 - 5.5 ft
  gather(flow, preference, `0.25'`:`5.5'`) %>%
  
  # numeric coding of responses, ranging from -2 (Unacceptable) to +2 (Acceptable)
  mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                     str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                     preference == "Marginal" ~ 0,
                                     preference == "Moderately Acceptable" ~ 1,
                                     preference == "Acceptable" ~ 2)) %>%
  
  # add craft type data, joining by respondent id
  left_join(craft, by = "respondent.id") %>%
  
  # add the name of this segment
  mutate(segment.name = "Lower Mishawaka") %>%
  
  # rearrange columns
  select(respondent.id, segment.name, craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other, everything())

# rename for this segment
flowpref.lowermish <- flowpref

#- UPPER MISHAWAKA ------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *********************** UPPER MISHAWAKA  ********************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# craft type
# *************************************************************************

# craft type response data for Upper Mish are in columns 189 through 194 
craft <- data[2:nrow(data), 189:194] %>%
  
  # appoint more intuitive column names
  rename(kayak = colnames(data)[189],
         raft_shredder = colnames(data)[190],
         packraft_inflatable_canoe = colnames(data)[191],
         open_canoe = colnames(data)[192],
         sup = colnames(data)[193],
         other = colnames(data)[194]) %>%
  
  # convert data to logical, except `craft.other` which is a string 
  mutate(craft.kayak = kayak != "",
         craft.raft.shredder = raft_shredder != "",
         craft.opencanoe = open_canoe != "",
         craft.packraft.inflatecayak.canoe = packraft_inflatable_canoe != "",
         craft.sup = sup != "",
         craft.other = other) %>%
  
  # arrange columns
  select(craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other) %>%
  
  # add respondent ids to the data and rename
  cbind(respondent.attributes$respondent.id) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # arrange columns
  select(respondent.id, everything())

# flow preference
# *************************************************************************

# Upper Mishawaka flow (stage) preference responses are contained in columns 195 through 211
flowpref <- data[1:nrow(data), 195:211]

# change column names to the stage level listed in the first row of flowpref
colnames(flowpref) = as.character(t(flowpref[1,]))

# add respondent id information, remove the first row that had contained column names
flowpref <- cbind(respondent.attributes$respondent.id, flowpref[-1,]) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # reshape into a long format, creating two new columns: flow & preference. 
  # rock stage range surveyed is 0.25 - 5.5 ft
  gather(flow, preference, `0.25'`:`5.5'`) %>%
  
  # numeric coding of responses, ranging from -2 (Unacceptable) to +2 (Acceptable)
  mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                     str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                     preference == "Marginal" ~ 0,
                                     preference == "Moderately Acceptable" ~ 1,
                                     preference == "Acceptable" ~ 2)) %>%
  
  # add craft type data, joining by respondent id
  left_join(craft, by = "respondent.id") %>%
  
  # add the name of this segment
  mutate(segment.name = "Upper Mishawaka") %>%
  
  # rearrange columns
  select(respondent.id, segment.name, craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other, everything())

# rename for this segment
flowpref.uppermish <- flowpref
#- THE NARROWS ------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *********************** THE NARROWS  ************************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# craft type
# *************************************************************************

# craft type response data for The Narrows are in columns 220 through 225 
craft <- data[2:nrow(data), 220:225] %>%
  
  # appoint more intuitive column names
  rename(kayak = colnames(data)[220],
         raft_shredder = colnames(data)[221],
         packraft_inflatable_canoe = colnames(data)[222],
         open_canoe = colnames(data)[223],
         sup = colnames(data)[224],
         other = colnames(data)[225]) %>%
  
  # convert data to logical, except `craft.other` which is a string 
  mutate(craft.kayak = kayak != "",
         craft.raft.shredder = raft_shredder != "",
         craft.opencanoe = open_canoe != "",
         craft.packraft.inflatecayak.canoe = packraft_inflatable_canoe != "",
         craft.sup = sup != "",
         craft.other = other) %>%
  
  # arrange columns
  select(craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other) %>%
  
  # add respondent ids to the data and rename
  cbind(respondent.attributes$respondent.id) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # arrange columns
  select(respondent.id, everything())

# flow preference
# *************************************************************************

# The Narrows flow (stage) preference responses are contained in columns 226 through 242
flowpref <- data[1:nrow(data), 226:242]

# change column names to the stage level listed in the first row of flowpref
colnames(flowpref) = as.character(t(flowpref[1,]))

# add respondent id information, remove the first row that had contained column names
flowpref <- cbind(respondent.attributes$respondent.id, flowpref[-1,]) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # reshape into a long format, creating two new columns: flow & preference. 
  # rock stage range surveyed is 0.25 - 5.5 ft
  gather(flow, preference, `0.25'`:`5.5'`) %>%
  
  # numeric coding of responses, ranging from -2 (Unacceptable) to +2 (Acceptable)
  mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                     str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                     preference == "Marginal" ~ 0,
                                     preference == "Moderately Acceptable" ~ 1,
                                     preference == "Acceptable" ~ 2)) %>%
  
  # add craft type data, joining by respondent id
  left_join(craft, by = "respondent.id") %>%
  
  # add the name of this segment
  mutate(segment.name = "The Narrows") %>%
  
  # rearrange columns
  select(respondent.id, segment.name, craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other, everything())

# rename for this segment
flowpref.narrows <- flowpref
#- GRANDPA'S GORGE ------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *********************** GRANDPAS GORGE  *********************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# craft type
# *************************************************************************

# craft type response data for Grandpas Gorge are in columns 251 through 256 
craft <- data[2:nrow(data), 251:256] %>%
  
  # appoint more intuitive column names
  rename(kayak = colnames(data)[251],
         raft_shredder = colnames(data)[252],
         packraft_inflatable_canoe = colnames(data)[253],
         open_canoe = colnames(data)[254],
         sup = colnames(data)[255],
         other = colnames(data)[256]) %>%
  
  # convert data to logical, except `craft.other` which is a string 
  mutate(craft.kayak = kayak != "",
         craft.raft.shredder = raft_shredder != "",
         craft.opencanoe = open_canoe != "",
         craft.packraft.inflatecayak.canoe = packraft_inflatable_canoe != "",
         craft.sup = sup != "",
         craft.other = other) %>%
  
  # arrange columns
  select(craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other) %>%
  
  # add respondent ids to the data and rename
  cbind(respondent.attributes$respondent.id) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # arrange columns
  select(respondent.id, everything())

# flow preference
# *************************************************************************

# Grandpa's Gorge flow (stage) preference responses are contained in columns 257 through 273
flowpref <- data[1:nrow(data), 257:273]

# change column names to the stage level listed in the first row of flowpref
colnames(flowpref) = as.character(t(flowpref[1,]))

# add respondent id information, remove the first row that had contained column names
flowpref <- cbind(respondent.attributes$respondent.id, flowpref[-1,]) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # reshape into a long format, creating two new columns: flow & preference. 
  # rock stage range surveyed is 0.25 - 5.5 ft
  gather(flow, preference, `0.25'`:`5.5'`) %>%
  
  # numeric coding of responses, ranging from -2 (Unacceptable) to +2 (Acceptable)
  mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                     str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                     preference == "Marginal" ~ 0,
                                     preference == "Moderately Acceptable" ~ 1,
                                     preference == "Acceptable" ~ 2)) %>%
  
  # add craft type data, joining by respondent id
  left_join(craft, by = "respondent.id") %>%
  
  # add the name of this segment
  mutate(segment.name = "Grandpa's Gorge") %>%
  
  # rearrange columns
  select(respondent.id, segment.name, craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other, everything())

# rename for this segment
flowpref.grandpa <- flowpref

#- WHITE MILE RUN ------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *********************** WHITE MILE RUN  *********************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# craft type
# *************************************************************************

# craft type response data for White Mile Run are in columns 282 through 287 
craft <- data[2:nrow(data), 282:287] %>%
  
  # appoint more intuitive column names
  rename(kayak = colnames(data)[282],
         raft_shredder = colnames(data)[283],
         packraft_inflatable_canoe = colnames(data)[284],
         open_canoe = colnames(data)[285],
         sup = colnames(data)[286],
         other = colnames(data)[287]) %>%
  
  # convert data to logical, except `craft.other` which is a string 
  mutate(craft.kayak = kayak != "",
         craft.raft.shredder = raft_shredder != "",
         craft.opencanoe = open_canoe != "",
         craft.packraft.inflatecayak.canoe = packraft_inflatable_canoe != "",
         craft.sup = sup != "",
         craft.other = other) %>%
  
  # arrange columns
  select(craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other) %>%
  
  # add respondent ids to the data and rename
  cbind(respondent.attributes$respondent.id) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # arrange columns
  select(respondent.id, everything())

# flow preference
# *************************************************************************

# White Mile Run flow (stage) preference responses are contained in columns 288 through 304
flowpref <- data[1:nrow(data), 288:304]

# change column names to the stage level listed in the first row of flowpref
colnames(flowpref) = as.character(t(flowpref[1,]))

# add respondent id information, remove the first row that had contained column names
flowpref <- cbind(respondent.attributes$respondent.id, flowpref[-1,]) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # reshape into a long format, creating two new columns: flow & preference. 
  # rock stage range surveyed is 0.25 - 5.5 ft
  gather(flow, preference, `0.25'`:`5.5'`) %>%
  
  # numeric coding of responses, ranging from -2 (Unacceptable) to +2 (Acceptable)
  mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                     str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                     preference == "Marginal" ~ 0,
                                     preference == "Moderately Acceptable" ~ 1,
                                     preference == "Acceptable" ~ 2)) %>%
  
  # add craft type data, joining by respondent id
  left_join(craft, by = "respondent.id") %>%
  
  # add the name of this segment
  mutate(segment.name = "White Mile Run") %>%
  
  # rearrange columns
  select(respondent.id, segment.name, craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other, everything())

# rename for this segment
flowpref.whitemile <- flowpref

#- SPENCER HEIGHTS ------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *********************** SPENCER HEIGHTS  ********************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# craft type
# *************************************************************************

# craft type response data for Spencer Heights are in columns 313 through 318 
craft <- data[2:nrow(data), 313:318] %>%
  
  # appoint more intuitive column names
  rename(kayak = colnames(data)[313],
         raft_shredder = colnames(data)[314],
         packraft_inflatable_canoe = colnames(data)[315],
         open_canoe = colnames(data)[316],
         sup = colnames(data)[317],
         other = colnames(data)[318]) %>%
  
  # convert data to logical, except `craft.other` which is a string 
  mutate(craft.kayak = kayak != "",
         craft.raft.shredder = raft_shredder != "",
         craft.opencanoe = open_canoe != "",
         craft.packraft.inflatecayak.canoe = packraft_inflatable_canoe != "",
         craft.sup = sup != "",
         craft.other = other) %>%
  
  # arrange columns
  select(craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other) %>%
  
  # add respondent ids to the data and rename
  cbind(respondent.attributes$respondent.id) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # arrange columns
  select(respondent.id, everything())

# flow preference
# *************************************************************************

# Spencer Heights flow (stage) preference responses are contained in columns 319 through 355
flowpref <- data[1:nrow(data), 319:335]

# change column names to the stage level listed in the first row of flowpref
colnames(flowpref) = as.character(t(flowpref[1,]))

# add respondent id information, remove the first row that had contained column names
flowpref <- cbind(respondent.attributes$respondent.id, flowpref[-1,]) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # reshape into a long format, creating two new columns: flow & preference. 
  # rock stage range surveyed is 0.25 - 5.5 ft
  gather(flow, preference, `0.25'`:`5.5'`) %>%
  
  # numeric coding of responses, ranging from -2 (Unacceptable) to +2 (Acceptable)
  mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                     str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                     preference == "Marginal" ~ 0,
                                     preference == "Moderately Acceptable" ~ 1,
                                     preference == "Acceptable" ~ 2)) %>%
  
  # add craft type data, joining by respondent id
  left_join(craft, by = "respondent.id") %>%
  
  # add the name of this segment
  mutate(segment.name = "Spencer Heights") %>%
  
  # rearrange columns
  select(respondent.id, segment.name, craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other, everything())

# rename for this segment
flowpref.spencer <- flowpref

#- BIG SOUTH ------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# *********************** BIG SOUTH  **************************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# craft type
# *************************************************************************

# craft type response data for Big South are in columns 344 through 349 
craft <- data[2:nrow(data), 344:349] %>%
  
  # appoint more intuitive column names
  rename(kayak = colnames(data)[344],
         raft_shredder = colnames(data)[345],
         packraft_inflatable_canoe = colnames(data)[346],
         open_canoe = colnames(data)[347],
         sup = colnames(data)[348],
         other = colnames(data)[349]) %>%
  
  # convert data to logical, except `craft.other` which is a string 
  mutate(craft.kayak = kayak != "",
         craft.raft.shredder = raft_shredder != "",
         craft.opencanoe = open_canoe != "",
         craft.packraft.inflatecayak.canoe = packraft_inflatable_canoe != "",
         craft.sup = sup != "",
         craft.other = other) %>%
  
  # arrange columns
  select(craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other) %>%
  
  # add respondent ids to the data and rename
  cbind(respondent.attributes$respondent.id) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # arrange columns
  select(respondent.id, everything())

# flow preference
# *************************************************************************

# Big South flow (stage) preference responses are contained in columns 350 through 358
flowpref <- data[1:nrow(data), 350:358]

# change column names to the stage level listed in the first row of flowpref
colnames(flowpref) = as.character(t(flowpref[1,]))

# add respondent id information, remove the first row that had contained column names
flowpref <- cbind(respondent.attributes$respondent.id, flowpref[-1,]) %>%
  rename(respondent.id = `respondent.attributes$respondent.id`) %>%
  
  # reshape into a long format, creating two new columns: flow & preference. 
  # flow range 50 - 450 cfs
  gather(flow, preference, `50`:`450`) %>%
  
  # numeric coding of responses, ranging from -2 (Unacceptable) to +2 (Acceptable)
  mutate(preference.code = case_when(preference == "Unacceptable" ~ -2,
                                     str_detect(preference, c("Moderately","Unacceptable")) == 1 ~ -1,
                                     preference == "Marginal" ~ 0,
                                     preference == "Moderately Acceptable" ~ 1,
                                     preference == "Acceptable" ~ 2)) %>%
  
  # add craft type data, joining by respondent id
  left_join(craft, by = "respondent.id") %>%
  
  # add the name of this segment
  mutate(segment.name = "Big South") %>%
  
  # rearrange columns
  select(respondent.id, segment.name, craft.kayak, craft.raft.shredder, craft.opencanoe, craft.packraft.inflatecayak.canoe, craft.sup, craft.other, everything())

# rename for this segment
flowpref.bigsouth <- flowpref
#----------------------------------------------------------------------------------------------------
# **** STILL NEED TO organize data for other reaches higher in the canyon. 
flowpref.dat <- rbind(flowpref.wwp, 
                      flowpref.filter, 
                      flowpref.bridges, 
                      flowpref.poudrepark, 
                      flowpref.lowermish,
                      flowpref.uppermish, 
                      flowpref.narrows, 
                      flowpref.grandpa, 
                      flowpref.whitemile, 
                      flowpref.spencer, 
                      flowpref.bigsouth)

flowpref.dat$flow <- str_remove(flowpref.dat$flow, "'")

#-----------------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# save data for later analysis
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save(respondent.attributes, file = here::here("flow_pref","survey-analysis","respondend-attributes.Rdata"))
save(flowpref.dat, file = here::here("flow_pref","survey-analysis","flow-pref-data.Rdata"))

save(respondent.attributes, file = here::here("shiny","flow-pref-viz","data","respondend-attributes.Rdata"))
save(flowpref.dat, file = here::here("shiny","flow-pref-viz","data","flow-pref-data.Rdata"))

save.image(file = here::here("flow_pref","survey-analysis","tidy-response-workspace.Rdata"))
  
