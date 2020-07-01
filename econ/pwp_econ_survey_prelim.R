# Script for analyzing preliminary survey data

# Keith Jennings
# kjennings@lynkertech.com
# 2020-06-15

# Load packages
library(tidyverse)
library(lubridate) # for parsing survey monkey datetimes
library(cowplot); theme_set(theme_cowplot())

###############################################################################
###############################  Import Data  #################################
###############################################################################

###############################################################################
# Import survey data
# https://www.surveymonkey.com/r/Poudre_Whitewater_Park

# Import headers
headers <- unlist(strsplit(readLines("private_data/pwp_survey_headers.csv"), 
                           ","))

# Import data
survey <- read.csv("private_data/pwp_survey_20200612.csv",
                   skip = 2, header = FALSE, col.names = headers)

# Parse the date columns
survey <- survey %>% 
  mutate_at(vars(contains("date")), list(~ mdy_hms(., tz = "US/Mountain"))) %>% 
  mutate(visit_date = as.Date(visit_datetime, tz = "US/Mountain"))


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
