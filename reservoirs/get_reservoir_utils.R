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
                                timescale = "daily",
                                save.data = FALSE
                              ) {
  if(timescale == "daily") {
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
  } else if(timescale == "monthly") {
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
                        "divrecmonth/?dateFormat=spaceSepToSeconds&",
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
              date     = as.Date(paste0(datestring, "-01")),
              source   = 'CDSS'
            ) %>% 
            dplyr::select(wdid, date, flow, unit, source)
          
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
                             "_structure_flow_month.RDS")
          logger::log_info(
            'saving WDID:{wdid} monthly diversion flow data to {path} as {filename}'
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
                        "divrecmonth/?dateFormat=spaceSepToSeconds&",
                        "wcIdentifier=*Total+(Release)*&wdid=", wdid,
                        "&pageSize=", pageSize,
                        "&pageIndex=", pageIndex)
          
          logger::log_info(
            "Downloading WDID:{wdid} monthly release data from CDSS API..."
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
              date     = as.Date(paste0(datestring, "-01")),
              source   = 'CDSS'
            ) %>% 
            dplyr::select(wdid, date, release, unit, source)
          
          
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
                             "_structure_release_month.RDS")
          logger::log_info(
            'saving WDID:{wdid} release data to {path} as {filename}'
          )
          saveRDS(all_data, paste0(path, "/", filename))
        }
        
        return(as_tibble(all_data))
        
      } else {
        
        logger::log_info('Invalid parameter input data_type, data_type must equal either "flow", "stage", "release')
        
      }
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

#===========================================
GetCDSSStationFlow <- function(
  site_abbrev,
  start_date = '01-01-1800', 
  end_date   = '01-01-2050',
  timescale  = "hourly",
  save.data  = FALSE, 
  api_key    = NULL
) {
  
  # 
  # Get flow data via the the CDSS REST service. Return a tidy dataframe.
  # https://dwr.state.co.us/rest/get/help
  #
  # Args:
  #
  #  site_abbrev (character vector): telemetry site abbreviations.
  #     - North Fork Cache La Poudre Below Seaman Reservoir - 'CLANSECO'
  #     - Cache La Poudre at Canyon Mouth Near Fort Collins - 'CLAFTCCO'
  #
  #  start_date (character): Start perior of record (MM-DD-YYYY).
  #     - Defaults to '01-01-1800', thereby removing lower time limit
  #
  #  end_date (character): End period of record (MM-DD-YYYY)
  #     - Defaults to '01-01-2050', thereby removing upper time limit
  #
  # Returns:
  #   structure_data (tibble): tidy tibble of hourly flow record
  
  if(is.null(api_key)) {
      if(timescale == "hourly") {
    
        # IF API token is  NOT provided:
        
          # base URL for CDSS Telemetry station API 
          base <- paste0("https://dwr.state.co.us/Rest/GET/api/",
                         "v2/telemetrystations/telemetrytimeserieshour/")
          # maximum records per page
          pageSize = 50000
          
          # format start and end date for URL 
          start <- gsub("-", "%2F", start_date)
          end <- gsub("-", "%2F", end_date)
          
          # initialize empty dataframe to store data from multiple pages
          all_data = data.frame()
          
          # initialize pageInex
          pageIndex = 1
          
          # grab data while there are more pages of data to grab
          more_pages = T
          while (more_pages) {
            
            # create specific URL w/ WDID to call API
            flow_url <- paste0(
              base,
              "?dateFormat=spaceSepToSeconds&abbrev=", 
              paste(site_abbrev, collapse = '%2C+'), 
              "&endDate=", end, 
              "&includeThirdParty=true&parameter=DISCHRG", 
              "&startDate=", start,
              "&pageSize=", pageSize,
              "&pageIndex=", pageIndex)
            
            logger::log_info(
              "Downloading {site_abbrev} station flow data from CDSS API, page {pageIndex}..."
            )
            
            # GET request to CDSS API - catch standard errors, return custom
            tryCatch( 
              {
                cdss_api <- httr::GET(flow_url) %>%
                  content(as = "text") %>% 
                  jsonlite::fromJSON() %>% 
                  bind_rows() 
              },
              error = function(e) {
                logger::log_error(
                  'An error was encountered when trying to download flow data at {site_abbrev}'
                )
                logger::log_error(
                  'Perhaps the URL address is incorrect OR there are no data available.'
                )
                logger::log_error('Here is the URL address that was queried:')
                logger::log_error('{flow_url}')
                logger::log_error('And, here is the original error message:')
                logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
                logger::log_error(message(e))
                stop()
              }
            )
            
            # Tidy data 
            station_data <- cdss_api$ResultList %>% 
              dplyr::select(
                station    = abbrev,
                datestring = measDate,
                flow       = measValue,
                flow_unit  = measUnit
              ) %>% 
              mutate(
                datetime   = lubridate::as_datetime(datestring),
                date       = lubridate::as_date(datestring),
                hour       = lubridate::hour(datetime),
                source     = 'CDSS'
              ) %>% 
              dplyr::select(station, datetime, date, hour,
                            flow, flow_unit, source)
            
            # bind data from this page
            all_data <- rbind(all_data, station_data)
            
            # determine if thre are additional pages of data to get.
            if (nrow(station_data) < pageSize) {
              more_pages = FALSE
            } else {
              pageIndex = pageIndex + 1
            }
            
          }
          
          if (save.data) {
            
            # save data to disk as RDS
            path <- here::here("data", "gauge")
            filename <- paste0(site_abbrev,
                               "_station_flow.RDS")
            
            logger::log_info(
              'saving {site_abbrev} station flow data to {path} as {filename}'
            )
            saveRDS(all_data, paste0(path, "/", filename))
            
          }
          
          return(as_tibble(all_data))
      } else if(timescale == "daily") {
          # IF API token is  NOT provided:
          
          # base URL for CDSS Telemetry station API 
          base <- paste0("https://dwr.state.co.us/Rest/GET/api/",
                         "v2/telemetrystations/telemetrytimeseriesday/")
          # maximum records per page
          pageSize = 50000
          
          # format start and end date for URL 
          start <- gsub("-", "%2F", start_date)
          end <- gsub("-", "%2F", end_date)
          
          # initialize empty dataframe to store data from multiple pages
          all_data = data.frame()
          # site_abbrev <- "LAPLODCO"
          # initialize pageInex
          pageIndex = 1
          
          # grab data while there are more pages of data to grab
          more_pages = T
          while (more_pages) {
            
            # create specific URL w/ WDID to call API
            flow_url <- paste0(
              base,
              "?dateFormat=spaceSepToSeconds&abbrev=", 
              paste(site_abbrev, collapse = '%2C+'), 
              "&endDate=", end, 
              "&includeThirdParty=true&parameter=DISCHRG", 
              "&startDate=", start,
              "&pageSize=", pageSize,
              "&pageIndex=", pageIndex)
            
            logger::log_info(
              "Downloading {site_abbrev} station flow data from CDSS API, page {pageIndex}..."
            )
            
            # GET request to CDSS API - catch standard errors, return custom
            tryCatch( 
              {
                cdss_api <- httr::GET(flow_url) %>%
                  content(as = "text") %>% 
                  jsonlite::fromJSON() %>% 
                  bind_rows() 
              },
              error = function(e) {
                logger::log_error(
                  'An error was encountered when trying to download flow data at {site_abbrev}'
                )
                logger::log_error(
                  'Perhaps the URL address is incorrect OR there are no data available.'
                )
                logger::log_error('Here is the URL address that was queried:')
                logger::log_error('{flow_url}')
                logger::log_error('And, here is the original error message:')
                logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
                logger::log_error(message(e))
                stop()
              }
            )
            
            # Tidy data 
            station_data <- cdss_api$ResultList %>% 
              dplyr::select(
                station    = abbrev,
                datestring = measDate,
                flow       = measValue,
                flow_unit  = measUnit
              ) %>% 
              mutate(
                datetime   = lubridate::as_datetime(datestring),
                date       = lubridate::as_date(datestring),
                source     = 'CDSS'
              ) %>% 
              dplyr::select(station, datetime, date,
                            flow, flow_unit, source)
            
            # bind data from this page
            all_data <- rbind(all_data, station_data)
            
            # determine if thre are additional pages of data to get.
            if (nrow(station_data) < pageSize) {
              more_pages = FALSE
            } else {
              pageIndex = pageIndex + 1
            }
            
          }
          
          if (save.data) {
            
            # save data to disk as RDS
            path <- here::here("data", "gauge")
            filename <- paste0(site_abbrev,
                               "_station_flow.RDS")
            
            logger::log_info(
              'saving {site_abbrev} station flow data to {path} as {filename}'
            )
            saveRDS(all_data, paste0(path, "/", filename))
            
          }
          
          return(as_tibble(all_data))
      }
    
  }  else if(!is.null(api_key)) {
       if(timescale == "hourly") {
          # IF API token IS provided:
      
          # base URL for CDSS Telemetry station API 
          base <- paste0("https://dwr.state.co.us/Rest/GET/api/",
                         "v2/telemetrystations/telemetrytimeserieshour")
          
          # maximum records per page
          pageSize = 50000
          
          # format start and end date for URL 
          start <- gsub("-", "%2F", start_date)
          end <- gsub("-", "%2F", end_date)
          
          # initialize empty dataframe to store data from multiple pages
          all_data = data.frame()
          
          # initialize pageInex
          pageIndex = 1
          
          # grab data while there are more pages of data to grab
          more_pages = T
          while (more_pages) {
            
            # create specific URL w/ WDID to call API
            flow_url <- paste0(
              base,
              "/?dateFormat=spaceSepToSeconds&abbrev=", 
              paste(site_abbrev, collapse = '%2C+'), 
              "&endDate=", end, 
              "&includeThirdParty=true&parameter=DISCHRG", 
              "&startDate=", start,
              "&pageSize=", pageSize,
              "&pageIndex=", pageIndex, 
              "&apiKey=", api_key
              )
            
            logger::log_info(
              "Downloading {site_abbrev} station flow data from CDSS API, page {pageIndex}..."
            )
            
            # GET request to CDSS API - catch standard errors, return custom
            tryCatch( 
              {
                cdss_api <- httr::GET(flow_url) %>%
                  content(as = "text") %>% 
                  jsonlite::fromJSON() %>% 
                  bind_rows() 
              },
              error = function(e) {
                logger::log_error(
                  'An error was encountered when trying to download flow data at {site_abbrev}'
                )
                logger::log_error(
                  'Perhaps the URL address is incorrect OR there are no data available.'
                )
                logger::log_error('Here is the URL address that was queried:')
                logger::log_error('{flow_url}')
                logger::log_error('And, here is the original error message:')
                logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
                logger::log_error(message(e))
                stop()
              }
            )
            
            # Tidy data 
            station_data <- cdss_api$ResultList %>% 
              dplyr::select(
                station    = abbrev,
                datestring = measDate,
                flow       = measValue,
                flow_unit  = measUnit
              ) %>% 
              mutate(
                datetime   = lubridate::as_datetime(datestring),
                date       = lubridate::as_date(datestring),
                hour       = lubridate::hour(datetime),
                source     = 'CDSS'
              ) %>% 
              dplyr::select(station, datetime, date, hour,
                            flow, flow_unit, source)
            
            # bind data from this page
            all_data <- rbind(all_data, station_data)
            
            # determine if thre are additional pages of data to get.
            if (nrow(station_data) < pageSize) {
              more_pages = FALSE
            } else {
              pageIndex = pageIndex + 1
            }
            
          }
          
          if (save.data) {
            
            # save data to disk as RDS
            path <- here::here("data", "gauge")
            filename <- paste0(site_abbrev,
                               "_station_flow.RDS")
            
            logger::log_info(
              'saving {site_abbrev} station flow data to {path} as {filename}'
            )
            saveRDS(all_data, paste0(path, "/", filename))
            
          }
          
          return(as_tibble(all_data))
      } else if(timescale == "daily") {
            # IF API token IS provided:
            
            # base URL for CDSS Telemetry station API 
            base <- paste0("https://dwr.state.co.us/Rest/GET/api/",
                           "v2/telemetrystations/telemetrytimeseriesday/")
            
            # maximum records per page
            pageSize = 50000
            
            # format start and end date for URL 
            start <- gsub("-", "%2F", start_date)
            end <- gsub("-", "%2F", end_date)
            
            # initialize empty dataframe to store data from multiple pages
            all_data = data.frame()
            
            # initialize pageInex
            pageIndex = 1

            # grab data while there are more pages of data to grab
            more_pages = T
            while (more_pages) {
              
              # create specific URL w/ WDID to call API
              flow_url <- paste0(
                base,
                "?dateFormat=spaceSepToSeconds&abbrev=", 
                paste(site_abbrev, collapse = '%2C+'), 
                "&endDate=", end, 
                "&includeThirdParty=true&parameter=DISCHRG", 
                "&startDate=", start,
                "&pageSize=", pageSize,
                "&pageIndex=", pageIndex, 
                "&apiKey=", api_key
              )

              logger::log_info(
                "Downloading {site_abbrev} station flow data from CDSS API, page {pageIndex}..."
              )
              
              # GET request to CDSS API - catch standard errors, return custom
              tryCatch( 
                {
                  cdss_api <- httr::GET(flow_url) %>%
                    content(as = "text") %>% 
                    jsonlite::fromJSON() %>% 
                    bind_rows() 
                },
                error = function(e) {
                  logger::log_error(
                    'An error was encountered when trying to download flow data at {site_abbrev}'
                  )
                  logger::log_error(
                    'Perhaps the URL address is incorrect OR there are no data available.'
                  )
                  logger::log_error('Here is the URL address that was queried:')
                  logger::log_error('{flow_url}')
                  logger::log_error('And, here is the original error message:')
                  logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
                  logger::log_error(message(e))
                  stop()
                }
              )
              
              # Tidy data 
              station_data <- cdss_api$ResultList %>% 
                dplyr::select(
                  station    = abbrev,
                  datestring = measDate,
                  flow       = measValue,
                  flow_unit  = measUnit
                ) %>% 
                mutate(
                  datetime   = lubridate::as_datetime(datestring),
                  date       = lubridate::as_date(datestring),
                  source     = 'CDSS'
                ) %>% 
                dplyr::select(station, datetime, date,
                              flow, flow_unit, source)
              
              # bind data from this page
              all_data <- rbind(all_data, station_data)
              
              # determine if thre are additional pages of data to get.
              if (nrow(station_data) < pageSize) {
                more_pages = FALSE
              } else {
                pageIndex = pageIndex + 1
              }
              
            }
            
            if (save.data) {
              
              # save data to disk as RDS
              path <- here::here("data", "gauge")
              filename <- paste0(site_abbrev,
                                 "_station_flow.RDS")
              
              logger::log_info(
                'saving {site_abbrev} station flow data to {path} as {filename}'
              )
              saveRDS(all_data, paste0(path, "/", filename))
              
            }
            
            return(as_tibble(all_data))
        
      }
  }
}


