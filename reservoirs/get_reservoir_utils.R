##------------------------------------------------------------------------------
##
## Script name: get_reservoir_utils.R
##
## Purpose of script: 
##  Contain utility functions for getting reservoir data from various sources and
##  returning similarly formatted tidy datasets.
## 
##
## Author: Angus Watters & Adam N. Wlostowski
##
## Date Created: 2022-02-01
##
## Copyright (c) Adam N. Wlostowski, 2021
## Email: awlostowski@lynker.com
## Email: awatters@lynker.com
##
## ---------------------------
##
## Notes:
##
##------------------------------------------------------------------------------

remove(list = ls())  # clear all workspace variables
cat("\014")          # clear command line

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

## load packages
library(here)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(logger)

##------------------------------------------------------------------------------
## Function definitions

#===========================================\

#===========================================
getCDSSDiversionFlow <- function(
                                wdid, 
                                data_type = "flow",
                                save.data = FALSE
                              ) {
  
   if(data_type == "flow") {
     

     # base URL for CDSS diversion records API 
     base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/"
     
     # maximum records per page
     pageSize = 50000
     
     # initialize empty dataframe to store data from multiple pages
     all_data = data.frame()
     
     # initialize pageInex
     pageIndex = 1
     
     # grab data while there are more pages of data to grab
     more_pages = T
     while (more_pages) {
       
       url <- paste0(base, 
                     "divrecday/?dateFormat=spaceSepToSeconds&",
                     "wcIdentifier=*Total+(Diversion)*&wdid=", wdid,
                     "&pageSize=", pageSize,
                     "&pageIndex=", pageIndex)
       
       logger::log_info(
         "Downloading WDID:{wdid} diversion flow data from CDSS API..."
       )
       
       # GET request to CDSS API
       tryCatch( 
         {
           cdss_api <- httr::GET(url) %>%
             content(as = "text") %>% 
             fromJSON() %>% 
             bind_rows() 
         },
         error = function(e) {
           logger::log_error(
             'An error was encountered when trying to download diversion flow data at WDID:{wdid}'
           )
           logger::log_error(
             'Perhaps the URL address is incorrect OR there are no data available.'
           )
           logger::log_error('Here is the URL address that was queried:')
           logger::log_error('{url}')
           logger::log_error('And, here is the original error message:')
           logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
           logger::log_error(message(e))
           stop()
         }
       )
       
       # Tidy data 
       structure_data <- cdss_api$ResultList %>%  
         dplyr::select(
           wdid,
           datestring = dataMeasDate,
           flow       = dataValue,
           unit       = measUnits
         ) %>% 
         mutate(
           datetime = lubridate::as_datetime(datestring),
           date     = lubridate::as_date(datestring),
           source   = 'CDSS'
         ) %>% 
         dplyr::select(wdid, datetime, date, flow, unit, source)
       
       # bind data from this page
       all_data <- rbind(all_data, structure_data)
       
       # determine if thre are additional pages of data to get.
       if (nrow(structure_data) < pageSize) {
         more_pages = FALSE
       } else {
         pageIndex = pageIndex + 1
       }
       
     }
     if (save.data) {
       
       # save data to disk as RDS
       path     <- here::here("data", "gauge")
       filename <- paste0("wdid_",wdid,
                          "_structure_flow.RDS")
       logger::log_info(
         'saving WDID:{wdid} diversion flow data to {path} as {filename}'
       )
       saveRDS(all_data, paste0(path, "/", filename))
       
     }
     
     return(as_tibble(all_data))
     
   } else if(data_type == "stage"){
     

     # base URL for CDSS diversion records API 
     base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/"
     
     # maximum records per page
     pageSize = 50000
     
     # initialize empty dataframe to store data from multiple pages
     all_data = data.frame()
     
     # initialize pageInex
     pageIndex = 1
     
     # grab data while there are more pages of data to grab
     more_pages = T
     while (more_pages) {
       
       url <- paste0(base, 
                     "stagevolume/?dateFormat=spaceSepToSeconds&wdid=", 
                     wdid, 
                     "&pageSize=",
                     pageSize,
                     "&pageIndex=", 
                     pageIndex
       )
       
       
       logger::log_info(
         "Downloading WDID:{wdid} stage data from CDSS API..."
       )
       
       # GET request to CDSS API
       tryCatch( 
         {
           cdss_api <- httr::GET(url) %>%
             content(as = "text") %>% 
             fromJSON() %>% 
             bind_rows() 
         },
         error = function(e) {
           logger::log_error(
             'An error was encountered when trying to download stage data at WDID:{wdid}'
           )
           logger::log_error(
             'Perhaps the URL address is incorrect OR there are no data available.'
           )
           logger::log_error('Here is the URL address that was queried:')
           logger::log_error('{url}')
           logger::log_error('And, here is the original error message:')
           logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
           logger::log_error(message(e))
           stop()
         }
       )
       
       # Tidy data 
       structure_data <- cdss_api$ResultList %>%  
         dplyr::select(
           wdid,
           datestring = dataMeasDate,
           stage
         ) %>% 
         mutate(
           datetime = lubridate::as_datetime(datestring),
           date     = lubridate::as_date(datestring),
           source   = 'CDSS'
         ) %>% 
         dplyr::select(wdid, datetime, date, stage, source)
       
       # bind data from this page
       all_data <- rbind(all_data, structure_data)
       
       # determine if thre are additional pages of data to get.
       if (nrow(structure_data) < pageSize) {
         more_pages = FALSE
       } else {
         pageIndex = pageIndex + 1
       }
       
     }
     
     if (save.data) {
       
       # save data to disk as RDS
       path     <- here::here("data", "gauge")
       filename <- paste0("wdid_",wdid,
                          "_structure_stage.RDS")
       logger::log_info(
         'saving WDID:{wdid} stage data to {path} as {filename}'
         )
       saveRDS(all_data, paste0(path, "/", filename))
       
     }
     
     return(as_tibble(all_data))
     
   } else if(data_type == "volume"){
     
     
     # base URL for CDSS diversion records API 
     base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/"
     
     # maximum records per page
     pageSize = 50000
     
     # initialize empty dataframe to store data from multiple pages
     all_data = data.frame()
     
     # initialize pageInex
     pageIndex = 1
     
     # grab data while there are more pages of data to grab
     more_pages = T
     while (more_pages) {
       
       url <- paste0(base, 
                     "stagevolume/?dateFormat=spaceSepToSeconds&wdid=", 
                     wdid, 
                     "&pageSize=",
                     pageSize,
                     "&pageIndex=", 
                     pageIndex
       )
       
       
       logger::log_info(
         "Downloading WDID:{wdid} volume data from CDSS API..."
       )
       
       # GET request to CDSS API
       tryCatch( 
         {
           cdss_api <- httr::GET(url) %>%
             content(as = "text") %>% 
             fromJSON() %>% 
             bind_rows() 
         },
         error = function(e) {
           logger::log_error(
             'An error was encountered when trying to download volume data at WDID:{wdid}'
           )
           logger::log_error(
             'Perhaps the URL address is incorrect OR there are no data available.'
           )
           logger::log_error('Here is the URL address that was queried:')
           logger::log_error('{url}')
           logger::log_error('And, here is the original error message:')
           logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
           logger::log_error(message(e))
           stop()
         }
       )
       
       # Tidy data 
       structure_data <- cdss_api$ResultList %>%  
         dplyr::select(
           wdid,
           datestring = dataMeasDate,
           volume
         ) %>% 
         mutate(
           datetime = lubridate::as_datetime(datestring),
           date     = lubridate::as_date(datestring),
           source   = 'CDSS'
         ) %>% 
         dplyr::select(wdid, datetime, date, volume, source)
       
       # bind data from this page
       all_data <- rbind(all_data, structure_data)
       
       # determine if thre are additional pages of data to get.
       if (nrow(structure_data) < pageSize) {
         more_pages = FALSE
       } else {
         pageIndex = pageIndex + 1
       }
       
     }
     
     if (save.data) {
       
       # save data to disk as RDS
       path     <- here::here("data", "gauge")
       filename <- paste0("wdid_",wdid,
                          "_structure_volume.RDS")
       logger::log_info(
         'saving WDID:{wdid} volume data to {path} as {filename}'
       )
       saveRDS(all_data, paste0(path, "/", filename))
       
     }
     
     return(as_tibble(all_data))
     
   } else if(data_type == "release"){
     

     # base URL for CDSS diversion records API 
     base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/"
     
     # maximum records per page
     pageSize = 50000
     
     # initialize empty dataframe to store data from multiple pages
     all_data = data.frame()
     
     # initialize pageInex
     pageIndex = 1
     
     # grab data while there are more pages of data to grab
     more_pages = T
     while (more_pages) {
       
         url <- paste0(base, 
                       "divrecday/?dateFormat=spaceSepToSeconds&",
                       "wcIdentifier=*Total+(Release)*&wdid=", wdid,
                       "&pageSize=", pageSize,
                       "&pageIndex=", pageIndex)
         
         logger::log_info(
           "Downloading WDID:{wdid} release data from CDSS API..."
         )
         
         # GET request to CDSS API
         tryCatch( 
           {
             cdss_api <- httr::GET(url) %>%
               content(as = "text") %>% 
               fromJSON() %>% 
               bind_rows() 
           },
           error = function(e) {
             logger::log_error(
               'An error was encountered when trying to download release data at WDID:{wdid}'
             )
             logger::log_error(
               'Perhaps the URL address is incorrect OR there are no data available.'
             )
             logger::log_error('Here is the URL address that was queried:')
             logger::log_error('{url}')
             logger::log_error('And, here is the original error message:')
             logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
             logger::log_error(message(e))
             stop()
           }
         )
         
         # Tidy data 
         structure_data <- cdss_api$ResultList %>%  
           dplyr::select(
             wdid,
             datestring = dataMeasDate,
             release    = dataValue,
             unit       = measUnits
           ) %>% 
           mutate(
             datetime = lubridate::as_datetime(datestring),
             date     = lubridate::as_date(datestring),
             source   = 'CDSS'
           ) %>% 
           dplyr::select(wdid, datetime, date, release, unit, source)
         
         # bind data from this page
         all_data <- rbind(all_data, structure_data)
         
         # determine if thre are additional pages of data to get.
         if (nrow(structure_data) < pageSize) {
           more_pages = FALSE
         } else {
           pageIndex = pageIndex + 1
         }
       
     }
     if (save.data) {
       
         # save data to disk as RDS
         path     <- here::here("data", "gauge")
         filename <- paste0("wdid_",wdid,
                            "_structure_release.RDS")
         logger::log_info(
           'saving WDID:{wdid} release data to {path} as {filename}'
         )
         saveRDS(all_data, paste0(path, "/", filename))
     }
     
     return(as_tibble(all_data))
     
   } else {
     
     logger::log_info('Invalid parameter input data_type, data_type must equal either "flow", "stage", "release')
     
   }
}

# Function to retrieve a list of telemetry stations from the CDSS API that contain useful information such as WDID, abbreviations, etc.
getStationInfo <- function() {
  url <- "https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrystation/?includeHistoric=true"
  
  # GET request to CDSS API
  cdss_api_stations <- httr::GET(url) %>%
    content(as = "text") %>%
    fromJSON() %>%
    bind_rows()
  
  structures <- cdss_api_stations$ResultList %>%
    janitor::clean_names() 
    # dplyr::select(station_name, wdid, gnis_id, usgs_station_id, abbrev, station_type, structure_type, stream_mile, parameter, stage, meas_value)
  
  return(structures)
  
}

# defunk function for pulling hourly telemetry station data, superceded by alterations to getCDSSDiversionFlow argument "data_type"
get_reservoirs <- function(abbreviation) {

  logger::log_info('reservoir {abbreviation}')
  

  # maximum records per page
  pageSize = 50000
  
  # initialize empty dataframe to store data from multiple pages
  all_data = data.frame()
  
  # initialize pageInex
  pageIndex = 1
  
  # grab data while there are more pages of data to grab
  more_pages = T
  
  # todays date to use as maximum date
  today_date <- gsub("-", "%2F", format(Sys.Date(), '%m-%d-%Y'))
  

  while (more_pages) {
# "    https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrytimeserieshour/?dateFormat=spaceSepToSeconds&abbrev=0604180A&endDate=02%2F03%2F2022&startDate=01%2F01%2F2015&pageSize=50000&pageIndex=1"
    url <- paste0("https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrytimeserieshour/?dateFormat=spaceSepToSeconds&abbrev=", 
                  abbreviation,
                  "&endDate=", today_date,
                  "&startDate=01%2F01%2F2000",
                  "&pageSize=", 
                  pageSize,
                  "&pageIndex=",
                  pageIndex
    )
    # url <- "https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrytimeserieshour/?dateFormat=spaceSepToSeconds&abbrev=0604180A&pageSize=50000&pageIndex=1"
    logger::log_info(
      "Downloading reservoir:{abbreviation} from CDSS API..."
    )
    
    # GET request to CDSS API
    tryCatch( 
      {
        cdss_api <- httr::GET(url) %>%
          content(as = "text") %>% 
          fromJSON() %>% 
          bind_rows() 
      },
      error = function(e) {
        logger::log_error(
          'An error was encountered when trying to download hourly timeseries at reservoir: {abbreviation}'
        )
        logger::log_error(
          'Perhaps the URL address is incorrect OR there are no data available.'
        )
        logger::log_error('Here is the URL address that was queried:')
        logger::log_error('{url}')
        logger::log_error('And, here is the orriginal error message:')
        logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
        logger::log_error(message(e))
        stop()
      }
    )
    
    # Tidy data 
    structure_data <- cdss_api$ResultList %>%  
      dplyr::select(
        abbrev,
        datestring = measDate,
        value      = measValue,
        unit       = measUnit
      ) %>% 
      mutate(
        datetime = lubridate::as_datetime(datestring),
        date     = lubridate::as_date(datestring),
        source   = 'CDSS'
      ) 
      # dplyr::select(wdid, datetime, date, flow, unit, source)
    
    # bind data from this page
    all_data <- rbind(all_data, structure_data)
    
    # determine if thre are additional pages of data to get.
    if (nrow(structure_data) < pageSize) {
      more_pages = FALSE
    } else {
      pageIndex = pageIndex + 1
    }
    
  }
  
  return(as_tibble(all_data))
}

