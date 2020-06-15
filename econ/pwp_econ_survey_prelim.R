# Script for analyzing preliminary survey data

# Keith Jennings
# kjennings@lynkertech.com
# 2020-06-15

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())

# Import headers
headers <- unlist(strsplit(readLines("private_data/pwp_survey_headers.csv"), 
                           ","))

# Import data
survey <- read.csv("private_data/pwp_survey_20200612.csv",
                   skip = 2, header = FALSE, col.names = headers)



