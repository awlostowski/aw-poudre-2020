##------------------------------------------------------------------------------
##
## Script name: rock_stage_webscrape.R
##
## Purpose of script: Scrape Poudre Rock Report stage observations from blog
##                    posts, organize data into dataframe for later use.
##
## Authors: Adam Wlostowski & Angus Watters
##
## Date Created: 2018-04-11
##
## Copyright (c) Lynker, 2021
## Email: awlostowski@lynker.com
##
## ---------------------------
##
## Notes:
##   
##
##------------------------------------------------------------------------------

remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(here)
library(tidyverse)
library(lubridate)
library(rvest)
library(magrittr)
library(stringr)
library(stringi)
library(logger)

##------------------------------------------------------------------------------
## Function definitions

scrape_page <- function(n) {
  # 
  # Scrape stage observations at Pine View, Hewlett, and Rustic from RockReport
  # blog posts.
  #
  # Args:
  #   n (int): Rock Report page number to scrape
  #
  # Returns:
  #   DataFrame of stage observations and metadata
    
  logger::log_info("Scraping page {n}")

  site <- paste0(
    "http://www.poudrerockreport.com/category/flows/page/",
    as.character(n)
  )
  
  # identify articles (blog posts) on page n
  articles <- rvest::read_html(site) %>%
    rvest::html_elements("article")
  
  # extract post titles
  titles <- rvest::html_attrs(articles) %>%
    lapply(`[`, "aria-label") %>%
    unlist(use.names = FALSE)
  
  # extract post dates
  dates <- rvest::html_elements(articles, "time") %>%
    rvest::html_text2()
  
  # extract post categories
  categories <- rvest::html_elements(articles, ".entry-categories") %>%
    rvest::html_text2() %>%
    stringr::str_remove_all("Filed Under: ")
  
  # blog entry content text
  entry_content = rvest::read_html(site) %>%
    html_nodes(".entry-content") %>%
    html_text()
  
  #
  # ---- HEWLETT ----
  #
  
  # locates the string "Hewlett" and select 100 characters just after it. 
  J = str_locate(entry_content, pattern = "Hewlett")
  hewlett_strings = str_sub(entry_content, J[, 1], J[, 2]+100) 
  
  # break "Hewlett"-containing strings into chunks separated by spaces
  comps <- str_split(hewlett_strings, " ", simplify = T)
  
  # extract stage value from each post, loop over rows
  hewlett_stage = c()
  for (i in 1:nrow(comps)) {
    # find "Feet"
    I = which(str_detect(comps[i, ], "Feet|ft.|Ft."))
    
    # select the string following and convert to numeric
    hewlett_stage[i] = as.numeric(comps[i, min(I)-1])
  }
  
  #
  # ---- RUSTIC ----
  #
  
  # locates the string "Rustic" and select 100 characters just after it. 
  J = str_locate(entry_content, pattern = "Rustic")
  rustic_strings = str_sub(entry_content, J[,1], J[,2]+100) 
  
  # break "Rustic"-containing strings into chunks separated by spaces
  comps <- str_split(rustic_strings, " ", simplify = T)
  
  # extract stage value from each post, loop over rows
  rustic_stage = c()
  for (i in 1:nrow(comps)) {
    
    # find "Feet"
    I = which(str_detect(comps[i, ], "Feet|ft.|Ft."))
    
    # select the string following and convert to numeric
    rustic_stage[i] = as.numeric(comps[i, min(I)-1])
    
  }
  
  # remove special characters EXCEPT period
  title_content <- stringi::stri_replace_all_fixed(
                      titles, 
                      c("{", "}", "~", "[", "]", "(", ")","!", "@", "<", ">"),
                      c("", "", "", "", "","", "","", "", "", ""),
                      vectorize_all = FALSE
                      )
  
  pine_stage <- str_extract(title_content, "\\d+\\.*\\d*") %>% 
    as.numeric()
  
  # convert post date to Date class
  date = as.Date(dates, format = "%B %d, %Y")
  
  # pack scraped data into a dataframe
  data.frame(
      title        = titles,
      date         = date,
      Pineview     = pine_stage,
      Hewlett      = hewlett_stage,
      Rustic       = rustic_stage,
      page         = n,   
      categories   = categories
      ) %>% 
  mutate( 
      # extract 4 digit military time from title and convert to hour, minutes
      time = format( 
        as.POSIXct(
          sprintf("%04s", stri_extract_last(title, regex = "\\d{4}")),
          format  = "%H%M",
          origin  = "1970-01-01",
          tz      = "UTC"
          ), "%H:%M"
        )
      )
}

##------------------------------------------------------------------------------
## Executed statements

# Read rock-report home page
home_page <- read_html(
  "http://www.poudrerockreport.com/category/flows/page/1"
  )

# extract last available page number
last_page <- rvest::html_element(home_page, ".archive-pagination") %>%
  rvest::html_children() %>%
  rvest::html_text2() %>%
  stringr::str_replace_all("\\n", "") %>%
  stringr::str_extract("(?<=[[:punct:]]).*(?=(Next))") %>%
  as.integer()

# sequence of all available page numbers
pages <- seq_len(last_page)

# lapply scrape_page() function across each page
content <- lapply(
  X        = pages,
  FUN      = scrape_page
)

# Bind rows of all scraped pages 
full_content <- bind_rows(content)

# QA/QC scraped stage data
maxDepth = 10 # upper limit depth threshold, feet
full_content <- full_content %>%
  filter(Pineview <= maxDepth)
  
# save scraped data to local path
path <- here::here("data", "rock_report")
filename <- "stage_poudre_rock_report.RDS"
logger::log_info("saving scraped RockReport data to {path} as {filename}")
saveRDS(full_content, paste0(path, "/", filename))

# Plot scraped stage data at Pine View
rock_stage <- readRDS(paste0(path, "/", filename))

rock_stage <- rock_stage %>% 
  filter(is.na(time) == F) %>%
  mutate(datetime = lubridate::ymd_hm(paste(date, time)))

ggplot() +
  geom_point(data = rock_stage, aes(x = datetime, y = Pineview)) +
  labs( 
    title = "Pineview Stage from Poudre Rock Report",
    y = "Stage (ft)",
    x= "Date"
  ) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 14),
      axis.title  = element_text(size = 14)
    ) 


