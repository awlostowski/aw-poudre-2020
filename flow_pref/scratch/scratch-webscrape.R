remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(rstudioapi)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(cowplot); theme_set(theme_cowplot())
library(stringr)
library(rvest)

# Set working directory to source file location
source_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(source_path))


# blog archives are organized by month and year.
page = seq(1,308,1)

# remove page 124
page = page[-which(page == 124 | page == 137 | page == 159 | page == 165 | page == 193)]


# loop over archive years
for (n in page) {
    
    # STEP 1: get htmp code from URL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
      # blog url, specific to year y and month m
      site_name <- paste("http://www.poudrerockreport.com/category/flows/page/", as.character(n), sep = "")
      
      # extract html data
      rock_report <- read_html(site_name)
    
    # STEP 2: extract pieces out of HTML using CSS selectors %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # NOTE: use SelectorGadget too l in Chrome to get CSS code
    
      # blog post titles, which contain Pine View stage observations
      title_content = rock_report %>%
        html_nodes(".entry-title-link") %>%
        html_text()
        
      # blog entry content text contains stage and flow observations at several sites
      entry_content = rock_report %>%
        html_nodes(".entry-content") %>%
        html_text()
      
      # timestamp of each blog post
      post_times = rock_report %>%
        html_nodes(".entry-time") %>%
        html_text() %>%
        str_remove(",")
      
      # if there is no post title, delete the content
    
    # STEP 3: Analyze blog entry contnent to get stage observations at Hewlett, Rustic, and Canyon Mouth %%%%%%%%%%%%%%%%%%%%%
    
      #**********************************************************************************************
      # HEWLETT **************
        
      # locates the string "Hewlett" and select a bunch of characters just after it. 
      J = str_locate(entry_content, pattern = "Hewlett") # locate
      hewlett_strings = str_sub(entry_content, J[,1], J[,2]+100) # select trailing 100 characters
      
      # break "Hewlett"-containing strings into chunks separated by spaces
      comps <- str_split(hewlett_strings, " ", simplify = T)
      
      # extract stage value from each post, loop over rows
      hewlett_stage = c()
      for (i in 1:nrow(comps)) {
        
        # find "Feet"
        I = which(str_detect(comps[i,], "Feet|ft.|Ft."))
        
        # select the string following and convert to numeric
        hewlett_stage[i] = as.numeric(comps[i,min(I)-1])
        
      }
      
      #**********************************************************************************************
      # RUSTIC **************
      
      # locates the string "Rustic" and select a bunch of characters just after it. 
      J = str_locate(entry_content, pattern = "Rustic") # locate
      rustic_strings = str_sub(entry_content, J[,1], J[,2]+100) # select trailing 100 characters
      
      # break "Rustic"-containing strings into chunks separated by spaces
      comps <- str_split(rustic_strings, " ", simplify = T)
      
      # extract stage value from each post, loop over rows
      rustic_stage = c()
      for (i in 1:nrow(comps)) {
        
        # find "Feet"
        I = which(str_detect(comps[i,], "Feet|ft.|Ft."))
        
        # select the string following and convert to numeric
        rustic_stage[i] = as.numeric(comps[i,min(I)-1])
        
      }
    
    # STEP 4: Analze post titles to get stage observations at Pine View %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      #**********************************************************************************************
      # PINE VIEW **************
      
      J = str_locate(title_content, pattern = "Pine View") # locate
      pine_strings = str_sub(title_content, J[,1], J[,2]+100) # select trailing 100 characters
      
      # break "Pine View"-containing strings into chunks separated by spaces
      comps <- str_split(pine_strings, " ", simplify = T)
      
      # extract stage value from each post title, loop over rows
      pine_stage = c()
      for (i in 1:nrow(comps)) {
        
        A = as.numeric(comps[i,]) # convert strings to numeric
        A = A[-which(is.na(A))]   # throw out NAs create by non-numeric classification
        A = A[-which(A > 10)]     # remove number greater than 10, which would correspond to a timestamp
        
        if (length(A) > 0) {
          pine_stage[i] = A
        } else {
          pine_stage[i] = NA
        }

      }
    
    # STEP 5: Convert post date from character to date class %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      date = as.Date(post_times, format = "%B %d %Y")
    
    # STEP 6: assemble data frame %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
      if (n == min(page)) {
        
        stage <- data.frame(date = date, 
                            Pineview = pine_stage,
                            Hewlett = hewlett_stage,
                            Rustic = rustic_stage)
        
      } else {
        
        tmp <- data.frame(date = date, 
                          Pineview = pine_stage,
                          Hewlett = hewlett_stage,
                          Rustic = rustic_stage)
        
        stage <- rbind(stage,tmp)
        
      }
    
      print(n)
    
}
  

# SAVE data ----------------------------------------
save(stage, file = "scractch_stage_dat.Rdata")


