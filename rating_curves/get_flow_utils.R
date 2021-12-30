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
## TODO - build exceptions to catch faulty URLs or no data returns
## TODO - return and process multiple pages of data. 1 pg limit of 50K records
## TODO - add start/end dates as input args to getCDSSDiversionData
##
##------------------------------------------------------------------------------

remove(list = ls())  # clear all workspace variables
cat("\014")          # clear command line

## load packages
library(here)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(logger)

##------------------------------------------------------------------------------
## Function definitions

getCDSSDiversionFlow <- function(wdid) {
  
  # 
  # Get daily diversion flow data from the CDSS REST service.
  #
  # Args:
  #   wdid (chr): unique structure identification code
  #
  # Returns:
  #   structure_data (tibble): tidy tibble of hourly flow record
  
  # base URL for CDSS diversion records API 
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/"

  url <- paste0(base, 
                "divrecday/?dateFormat=spaceSepToSeconds&",
                "wcIdentifier=*Total+(Diversion)*&wdid=", wdid)
  
  logger::log_info(
    "Downloading WDID:{wdid} diversion flow data from CDSS API..."
  )
  
  # GET request to CDSS API
  cdss_api <- httr::GET(url) %>%
    content(as = "text") %>% 
    fromJSON() %>% 
    bind_rows() 
  
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
  
  return(as_tibble(structure_data))
  
}

GetCDSSStationFlow <- function(
  site_abbrev,
  start_date = '01-01-1800', 
  end_date = '01-01-2050'
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
  #   station_data (tibble): tidy tibble of hourly flow record
  
  
  # base URL for CDSS Telemetry station API 
  base <- paste0("https://dwr.state.co.us/Rest/GET/api/",
                 "v2/telemetrystations/telemetrytimeserieshour")
  
  # format start and end date for URL 
  start <- gsub("-", "%2F", start_date)
  end <- gsub("-", "%2F", end_date)
  
  # create specific URL w/ WDID to call API
  flow_url <- paste0(base,
                "/?dateFormat=spaceSepToSeconds&abbrev=", 
                paste(site_abbrev, collapse = '%2C+'), 
                "&endDate=", end, 
                "&parameter=DISCHRG", 
                "&startDate=", start)
  
  logger::log_info(
    "Downloading {site_abbrev} station flow data from CDSS API..."
  )
  
  # GET request to CDSS API
  cdss_api <- httr::GET(flow_url) %>%
    content(as = "text") %>% 
    jsonlite::fromJSON() %>% 
    bind_rows() 
  
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
  
  return(as_tibble(station_data))
  
}

getOpenDataFlow <- function(sensor_name) {
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
  
  return(flow_data)
}

##------------------------------------------------------------------------------
## Executed statements

# # Example function uses
# 
# # get diversion record at Poudre Valley Canal
# struct_flow <- getCDSSDiversionFlow(wdid = '0300907')
# 
# # get flow record at Canyon Mouth gage (CLAFTCCO) from March 2019 onward
# station_flow <- GetCDSSStationFlow(
#   site_abbrev = 'CLAFTCCO',
#   start_date = '03-01-2019',
#   )
# 
# # get flow record at Forc Collins Poudre Park gage
# foco_flow <- getOpenDataFlow("Poudre Park")
