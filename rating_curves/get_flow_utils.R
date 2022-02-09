##------------------------------------------------------------------------------
##
## Script name: get_flow_utils.R
##
## Purpose of script: 
##  Contain utility functions for getting flow data from various sources and
##  returning similarly formatted tidy datasets.
## 
##
## Author: Angus Watters & Adam N. Wlostowski
##
## Date Created: 2021-12-30
##
## Copyright (c) Adam N. Wlostowski, 2021
## Email: awlostowski@lynker.com
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

#===========================================

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
          "Downloading WDID:{wdid} stage/volume data from CDSS API..."
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
              'An error was encountered when trying to download stage/volume data at WDID:{wdid}'
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
            stage,
            volume
          ) %>% 
          mutate(
            datetime = lubridate::as_datetime(datestring),
            date     = lubridate::as_date(datestring),
            source   = 'CDSS'
          ) %>% 
          dplyr::select(wdid, datetime, date, stage, volume, source)
        
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
          'saving WDID:{wdid} diversion stage/flow data to {path} as {filename}'
        )
        saveRDS(all_data, paste0(path, "/", filename))
      
    }
    
    return(as_tibble(all_data))
    
  } else {
    
    logger::log_info('Invalid parameter input data_type, data_type must equal either "flow" or "stage"')
    
  }
  
}


#===========================================
GetCDSSStationFlow <- function(
  site_abbrev,
  start_date = '01-01-1800', 
  end_date = '01-01-2050',
  save.data = FALSE
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
      "&parameter=DISCHRG", 
      "&startDate=", start,
      "&pageSize=", pageSize,
      "&pageIndex=", pageIndex,
      "&apiKey=zvb096GhomOSKSmpgbACBhDOCVUfs5P7")
    
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
        logger::log_error('And, here is the orriginal error message:')
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
  
}

#===========================================
getOpenDataFlow <- function(sensor_name, save.data = FALSE) {
  # 
  # Retrieve flow DataFrame from https://opendata.fcgov.com/ by sensor name
  #
  # Args:
  #   sensor_name (character): name of sensor for which data will be retrieved
  #     - 'Rustic'
  #     - 'Poudre Park'
  #
  # Returns:
  #   flow_data (tibble): tidy tibble of hourly flow record
  
  # full URL of City of Fort Collins OpenData site
  url <- "https://opendata.fcgov.com/resource/f5as-vvbj.json?sensor_name="
  
  # sensor-specific URL
  sensor_url <- paste0(url, gsub(" ", "%20", sensor_name))
  
  # retrieve data using the RSocrata library
  logger::log_info(
    "Downloading flow and stage data from opendata.fcgov.com at {sensor_name}"
  )
  flow_data_raw <- RSocrata::read.socrata(
    url = sensor_url
  )
  
  # take hourly average and tidy data
  flow_data <- flow_data_raw %>% 
    dplyr::select(sensor_name, timestamp, flow_cfs) %>%
    rename(datetime = timestamp, 
           station = sensor_name) %>%
    mutate(
      flow      = as.numeric(flow_cfs),
      date      = lubridate::date(datetime),
      hour      = lubridate::hour(datetime)
      ) %>%
    group_by(station, date, hour) %>%
    summarize(flow = mean(flow)) %>%
    mutate(
      datetime  = lubridate::ymd_h(paste(date, hour)),
      source    = 'Fort Collins OpenData',
      flow_unit = 'cfs'
      ) %>%
    dplyr::select(station, datetime, date, hour,
                  flow, flow_unit, source)
  
  if (save.data) {
    
    # save data to disk as RDS
    path <- here::here("data", "gauge")
    filename <- paste0(
      gsub(" ", "_", tolower(sensor_name)),
      "_flow.RDS"
    )
    
    logger::log_info(
      'saving {sensor_name} station flow data to {path} as {filename}'
    )
    saveRDS(flow_data, paste0(path, "/", filename))
    
  }
  
  return(flow_data)
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
##------------------------------------------------------------------------------
## Executed statements

# # Example function uses

# # get diversion record at Poudre Valley Canal
# struct_flow <- getCDSSDiversionFlow(wdid = '0300907', save.data = TRUE)

# # get flow record at Canyon Mouth gage (CLAFTCCO) from March 2019 onward
# station_flow <- GetCDSSStationFlow(
#   site_abbrev = 'CLAFTCCO',
#   start_date = '01-01-2020', 
#   end_date = '01-01-2021'
#   )

# # get flow record at Forc Collins Poudre Park gage
# foco_flow <- getOpenDataFlow("Poudre Park")
