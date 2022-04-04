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

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

## load packages
library(here)
library(tidyverse)
library(minpack.lm)
library(nlstools)
library(rootSolve) 
library(stringr)
library(logger)
library(segmented)

source(here::here('rating_curves', 'get_flow_utils.R'))

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
    logger::log_warn("{filename} not found!")
    loaded.data = F
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
stage          <- CheckLoad(rockreport.path, rockreport.filename)
if (stage == FALSE) {
  
  #*****
  # TODO(awlostowski): automatically call web-scraping.
  #*****
  
  logger::error('Please run rock_stage_webscrape.R to get stage data')
  stop()
}
  
# Read in gage observations at Poudre Park
gagedata.path     <- here::here("data","gauge")
gagedata.filename <- "poudre_park_flow.RDS"
flow <- CheckLoad(gagedata.path, gagedata.filename)
if (flow == FALSE) {
  logger::log_info(
    'Attempting to get Poudre Park flow data from FoCo OpenData...'
    )
  flow <- getOpenDataFlow("Poudre Park", save.data = T)
}

# =======================================
# Join observed gage flow and RockReport stage observations 

rating.data <- inner_join(stage, flow, by = "datetime") %>% 
  rename(stage = pineview, flow = flow) %>%
  dplyr::select(datetime, stage, flow)

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
    y = "Flow @ Poudre Park gauge (cfs)", 
    x = "Stage @ Pineview Access (ft)"
    ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title  = element_text(size = 14)
  ) 

print(pl_plot)
path <- here::here("rating_curves")
logger::log_info("saving a plot of Pineview rating curve at {path}")
ggsave(paste0(path, "/pineview_rating.png"), plot = pl_plot)

# =======================================
# Develop rating relationship with piecewise regression model

# remove zeros 
rating_data_rm_zero <- rating.data %>% 
  filter(stage > 0)

# simple linear regression
lm <- lm(flow ~ stage, rating_data_rm_zero)

# Segmented linear regression model w/ break at 2
segment_fit <- segmented::segmented(lm, fixed.psi = 1)


# save Logarithmic regression model object
path     <- here::here("rating_curves")
filename <- "piecewise_pineview_rating_model.RDS"
logger::log_info("saving Piecewise linear regression Pineview rating model at {path} as {filename}")
saveRDS(segment_fit, paste0(path, "/", filename))

# =======================================
# Plot and save piecewise rating curve plot

fit_segment <- rating_data_rm_zero %>% 
  mutate(
    fitted = segment_fit$fitted.values
  )

seg_lm_plot <- 
  ggplot() +
    geom_point(data = fit_segment, aes(x = stage, y = flow), alpha = 0.6, size = 1.5) +
    geom_line(data = fit_segment, aes(x = stage, y = fitted),  size = 1.5, color = "red") +
    scale_y_continuous(breaks = seq(0, 4000, 250)) +
    labs(
      y = "Flow @ Poudre Park gauge (cfs)", 
      x = "Stage @ Pineview Access (ft)"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 14),
      axis.title  = element_text(size = 14)
    ) 

seg_lm_plot

path <- here::here("rating_curves")
logger::log_info("saving a plot of Pineview rating curve at {path}")
ggsave(paste0(path, "/piecewise_pineview_rating.png"), plot = seg_lm_plot)


# # =======================================
# # Evaluate relationship between flows at Poudre Park and Rustic
# 
# rustic.filename <- "rustic_flow.RDS"
# rustic.flow <- CheckLoad(gagedata.path, rustic.filename)
# 
# # take hourly average of flow data at Rustic
# flow.rustic.dly <- rustic.flow %>% 
#   group_by(date) %>% 
#   summarize(
#     flow_cfs = mean(flow_cfs, na.rm = T)
#   )
# 
# flow.poudre.dly <- gage.flow %>%
#   group_by(date) %>%
#   summarize(
#     flow_cfs = mean(flow_cfs, na.rm = T)
#   )
# 
# # join Poudre Park and Rustic flows
# site.compare <- inner_join(flow.rustic.dly, flow.poudre.dly, by = "date") %>% 
#   rename(Rustic = flow_cfs.x, Poudre.Park = flow_cfs.y) %>%
#   select(date, Poudre.Park, Rustic) %>%
#   mutate(month = month(date))
# 
# compare.plot <- ggplot() +
#   geom_point(data = site.compare, aes(x = Poudre.Park, y = Rustic, color = date)) +
#   labs( 
#     y = "Flow at Rustic (cfs)",
#     x= "Flow at Poudre Park (cfs)"
#   ) +
#   theme_bw() +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.title  = element_text(size = 14)
#   )
# print(compare.plot)
