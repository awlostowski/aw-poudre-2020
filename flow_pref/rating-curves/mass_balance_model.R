library(tidyverse)
library(httr)
library(jsonlite)
library(logger)

# Function for calling CDSS API for Telemetry station data
get_station_data <- function(
                            type = c("telemetrytimeseriesraw", "telemetrytimeserieshour",
                                     "telemetrytimeseriesday", "telemetryratingtable"), 
                             abbrev,
                             param = c("DISCHRG", "GAGE_HT", "AIRTEMP"), 
                             start_date, 
                             end_date
                             ) {
  
  # base URL for CDSS Telemetry station API 
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/"
  
  # format start and end date for URL 
  start <- gsub("-", "%2F", start_date)
  end <- gsub("-", "%2F", end_date)
  
  # create specific URL w/ WDID to call API
  url <- paste0(base, type, "/?dateFormat=spaceSepToSeconds&abbrev=", abbrev, "&endDate=", end, "&parameter=", param, "&startDate=", start)
  
  # GET request to CDSS API
  cdss_api <- httr::GET(url) %>%
    content(as = "text") %>% 
    fromJSON() %>% 
    bind_rows() 
  
  # Tidy data 
  station_data <- cdss_api$ResultList %>% 
    dplyr::select(
            station  = abbrev,
            date     = measDate,
            parameter, 
            value    = measValue
                  ) %>% 
    mutate(
            time     = substr(date, 12, 19),
            date     = as.POSIXct(paste(as.character(substr(date, 0, 10)), time),
                                  format = "%Y-%m-%d %H:%M")
          ) %>% 
    dplyr::relocate(station, date, time, parameter, value)

}


# Function for calling CDSS API for Telemetry station data
get_structure_data <- function(
                              type = c("divrecday", "divrecmonth", "divrecyear", "stagevolume"),
                              wdid
                              ) {
  # base URL for CDSS diversion records API 
  base <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/"
  
  # create specific URL w/ WDID to call API
  url <- paste0(base, type, "/?dateFormat=spaceSepToSeconds&wcIdentifier=*Total+(Diversion)*&wdid=", wdid)
  
  # GET request to CDSS API
  cdss_api <- httr::GET(url) %>%
    content(as = "text") %>% 
    fromJSON() %>% 
    bind_rows() 
  
  # Tidy data 
  structure_data <- cdss_api$ResultList %>%  
    dplyr::select(
              wdid,
              date     = dataMeasDate,
              value    = dataValue,
              unit     = measUnits
                ) %>% 
    mutate(
             time     = substr(date, 12, 19),
             date     = as.POSIXct(paste(as.character(substr(date, 0, 10)), time),  
                                   format = "%Y-%m-%d %H:%M")
             ) %>% 
    dplyr::relocate(wdid, date, time, unit, value)
  
}

# type = "divrecday"
# wdid = "0300907"

# get flow data for Poudre Canyon mouth
poudre_canyon_mouth <- get_station_data(
                                        type        = "telemetrytimeserieshour",
                                        abbrev      = "CLAFTCCO",
                                        param       = "DISCHRG",
                                        start_date  = "03-22-2019",
                                        end_date    = "12-29-2021"
                                        # start_date  = "03-22-2019",
                                        # end_date    = "04-22-2019"
                                        )


# get flow data at Poudre Park 
sensor_name <-  "Poudre Park"
poudre_park <- get_flow_data(sensor_name = sensor_name)

poudre_park2 <- poudre_park %>%  
  mutate(
    time  =   format(
                    round(
                      strptime(paste("2001-01-01", time), format="%Y-%m-%d %H:%M"), 
                          units="hours"), 
                    format="%H:%M"
                    ),
     date  =  as.POSIXct(paste(as.character(date), time),  format = "%Y-%m-%d %H:%M")
         ) %>% 
  group_by(date) %>% 
  summarize(
    flow_cfs = mean(flow_cfs, na.rm = T)
  )




url <- "https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrytimeserieshour/?abbrev=CLAFTCCO&endDate=11%2F01%2F2021&parameter=DISCHRG&startDate=10%2F01%2F2021"
cdss_api <- httr::GET(url) %>%
  content(as = "text") %>% 
  fromJSON()

cdss_api

df <- bind_rows(cdss_api) 
df2 <- df$ResultList
url <- "https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrytimeserieshour/?abbrev=CLAFTCCO"


