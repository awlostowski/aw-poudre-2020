##------------------------------------------------------------------------------
##
## Script name: build_rating_curve.R
##
## Purpose of script: 
##  Develop a power-law rating relationship between flow at Poudre Park and stage 
##  at Pineview. This relationship will be used to transform the stage values 
##  used in preference surveys to flow values.
##
## Author: Adam N. Wlostowski & Angus Watters
##
## Date Created: 2021-12-28
##
## Copyright (c) Adam N. Wlostowski, 2021
## Email: awlostowski@lynker.com
##
## ---------------------------
##
## Notes:
## - rock_stage_webscrape.R and get_FoCo_OpenData_flow.R need to be run first
## - we use a power-law model to fit stage-flow observations
## - the rating model object is saved to aw-poudre-2020/rating_curves as RDS
##   
##------------------------------------------------------------------------------

remove(list = ls())  # clear all workspace variables
cat("\014")          # clear command line

## load packages
library(here)
library(tidyverse)
library(minpack.lm)
library(nlstools)
library(rootSolve) 
library(stringr)
library(logger)

##------------------------------------------------------------------------------
## Function definitions
CheckLoad <- function(path, filename) {
  # 
  # Check to see if RDS datasets exist, if so - load, if not - throw error
  #
  # Args:
  #   path (character)    : full path do folder containing data
  #   filename (character): name of file to be loaded
  #
  # Returns:
  #   loaded.data (Data)  : Dataset loaded from RDS
  
  if (file.exists(paste0(path, "/", filename))) {
    # read-in data
    logger::log_info("Reading data from {filename}")
    loaded.data <- readRDS(paste0(path, "/", filename))
    
  } else {
    logger::log_error("{filename} not found!")
    logger::log_error(
      "Run rock_stage_webscrape.R to build Pineview stage record."
    )
    logger::log_error(
      "Run get_FoCo_OpenData_flow.R to build tidy Poudre Park flow record."
    )
    stop()
  }
  
  return(loaded.data)
}

##------------------------------------------------------------------------------
## Executed statements

# =======================================
# Read-in stage and flow data

# Read in Rock Report stage observations at Pineview
rockreport.path     <- here::here("data","rock_report")
rockreport.filename <- "stage_poudre_rock_report.RDS"
rock.stage          <- CheckLoad(rockreport.path, rockreport.filename)
  
# Read in gage observations at Poudre Park
gagedata.path     <- here::here("data","gauge")
gagedata.filename <- "poudre_park_flow.RDS"
gage.flow <- CheckLoad(gagedata.path, gagedata.filename)

# =======================================
# Align datetime conventions of each dataset and join

# specify stage observation date and time
stage <- rock.stage %>%
  mutate(
    time = case_when(
      str_detect(title, regex('noon', ignore_case = T)) == TRUE ~ "12:00",
      str_detect(title, regex('noon', ignore_case = T)) == FALSE ~ time
    )
  ) %>%
  mutate(
    time = format(
      round(
        strptime(paste("2001-01-01", time), format="%Y-%m-%d %H:%M"), 
            units="hours"), 
      format="%H:%M")) %>% 
  filter(is.na(time) == F) %>% 
  mutate(date = as.POSIXct(paste(as.character(date), time),
                           format = "%Y-%m-%d %H:%M"))

# round flow time to nearest hour, make date variable full datetime, 
# and average flow observations within the same hour
flow <- gage.flow %>% 
  mutate(time = format(
            round(
              strptime(
                paste("2001-01-01", gage.flow$time),
                format="%Y-%m-%d %H:%M"
                ), 
              units="hours"
              ), 
            format="%H:%M"),
         date = as.POSIXct(
           paste(as.character(date), time),
           format = "%Y-%m-%d %H:%M"
           )
         ) %>% 
  group_by(date) %>% 
  summarize(
    flow_cfs = mean(flow_cfs, na.rm = T)
  )

# Join observed gage flow and RockReport stage observations 
rating.data <- inner_join(stage, flow, by = "date") %>% 
  rename(stage = Pineview, flow = flow_cfs) %>%
  select(date, stage, flow)

logger::log_info(
  "there are {nrow(rating.data)} flow/stage observations in the rating data"
  )

# =======================================
# Develop rating relationship with power-law model

# Fit a nonlinear, power-law, regression model to rating data
m <- minpack.lm::nlsLM(
  flow ~ a + (c*(stage^f)), 
  data = rating.data, 
  start = list(a = 100, c = 200, f = 1.4)
  )

# save regression model object
path     <- here::here("rating_curves")
filename <- "pineview_rating_model.RDS"
logger::log_info("saving Pineview rating model at {path} as {filename}")
saveRDS(m, paste0(path, "/", filename))

# extract regression model confidence intervals
cis <- as.data.frame(confint(m))

# build a dataframe of the fitted stage-flow relationship
fit <- data.frame(x = seq(0.25,6,0.001)) %>%
  mutate(flow     = predict(m, newdata = list(stage = x))) %>%
  mutate(flow_upr = cis$`97.5%`[1] + (cis$`97.5%`[2] * (x^cis$`97.5%`[3]))) %>%
  mutate(flow_lwr = cis$`2.5%`[1] + (cis$`2.5%`[2] * (x^cis$`2.5%`[3])))

# =======================================
# Plot and save rating curve

pl_plot <- ggplot(rating.data, aes(x = stage, y = flow)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_line(data = fit, aes(x = x, y = flow),  size = 1.5, color = "red") +
  labs(
    y = "Flow @ Pine View (cfs)", 
    x = "Rock Report stage @ Pine View (ft)",
    title = "Poudre Rock Rating Curve - Power Law Model "
    ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title  = element_text(size = 14)
  ) 

print(pl_plot)
path <- here::here("rating_curves")
logger::log_info("saving a plot of Pineview rating curve at {path}")
ggsave(paste0(path, "/pineview_rating.png"))

# =======================================
# Evaluate relationship between flows at Poudre Park and Rustic

rustic.filename <- "rustic_flow.RDS"
rustic.flow <- CheckLoad(gagedata.path, rustic.filename)

# take hourly average of flow data at Rustic
flow.rustic.dly <- rustic.flow %>% 
  group_by(date) %>% 
  summarize(
    flow_cfs = mean(flow_cfs, na.rm = T)
  )

flow.poudre.dly <- gage.flow %>%
  group_by(date) %>%
  summarize(
    flow_cfs = mean(flow_cfs, na.rm = T)
  )

# join Poudre Park and Rustic flows
site.compare <- inner_join(flow.rustic.dly, flow.poudre.dly, by = "date") %>% 
  rename(Rustic = flow_cfs.x, Poudre.Park = flow_cfs.y) %>%
  select(date, Poudre.Park, Rustic) %>%
  mutate(month = month(date))

compare.plot <- ggplot() +
  geom_point(data = site.compare, aes(x = Poudre.Park, y = Rustic, color = date)) +
  labs( 
    y = "Flow at Rustic (cfs)",
    x= "Flow at Poudre Park (cfs)"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title  = element_text(size = 14)
  )
print(compare.plot)
