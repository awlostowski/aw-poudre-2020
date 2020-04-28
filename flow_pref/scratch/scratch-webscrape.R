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

years = seq(2013,2019,1)
months = seq(4,9)


for (y in years) {
  
  for (m in months) {
    
    # STEP 1: scrape stage oservations for a single station, Hewlett Gulch Bridge ----------------------------
    # 1.1 get html code from url %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    site_name <- paste("http://www.poudrerockreport.com/",as.character(y),"/",as.character(m),"/", sep = "") 
    
    rock_report <- html(site_name)
    
    # 1.2 extract content by node %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # NOTE" use SelectorGadget tool in Chrome to get CSS code
    
    entry_content = rock_report %>%
      html_nodes(".entry-content") %>%
      html_text()
    
    # 1.2 analyze string content to find stage observations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # locates the string "Hewlett" and select a bunch of characters just after it. 
    
    J = str_locate(entry_content, pattern = "Hewlett") # locate
    hewlett_strings = str_sub(entry_content, J[,1], J[,2]+100) # select trailing 100 characters
    
    
    # break "Hewlett"-containing strings into chunks separated by spaces
    comps <- str_split(hewlett_strings, " ", simplify = T)
    
    # extract stage value from each post, loop over rows
    stage_value = c()
    for (i in 1:nrow(comps)) {
      
      # find "Feet"
      I = which(str_detect(comps[i,], "Feet|ft.|Ft."))
      
      # select the string following and convert to numeric
      stage_value[i] = as.numeric(comps[i,min(I)-1])
      
    }
    
    
    # STEP 2: scrape post dates ----------------------------
    # extract content entry time
    post_times = rock_report %>%
      html_nodes(".entry-time") %>%
      html_text() %>%
      str_remove(",")
    
    date = as.Date(post_times, format = "%B %d %Y")
    
    # STEP 3: assemble data frame -------------------------
    
    if (m == min(months) & y == min(years)) {
      
      stage <- data.frame(date = date, 
                          stage_ft = stage_value)
    } else if(m > min(months)) {
      
      tmp <- data.frame(date = date, 
                        stage_ft = stage_value)
      
      stage <- rbind(stage,tmp)
    }
    
    
    print(y)
    
  }
  
  
}

