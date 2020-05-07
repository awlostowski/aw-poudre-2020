remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(here)
library(tidyverse)
library(lubridate)
library(cowplot); theme_set(theme_cowplot())
library(broom)

# load monthly flow data. Corrected NWM simulations
load(here::here("flow_pref","rating-curves","corrected_sim_flow.Rdata"))

# load rock stage observations
load(here::here("flow_pref","rating-curves","stage_data.Rdata"))

# remove erronious data point at Rustic. Stage should not be ~150 ft!
stage$Rustic[which(stage$Rustic > 50)] = NA


# Monthly average and standard deviations of Pine View stage observations
stage_pv <- stage %>%
  mutate(year = year(date), 
         month = month(date)) %>%
  group_by(year, month) %>%
  summarize(stage_av = mean(Pineview, na.rm = T), 
            stage_sd = sd(Pineview, na.rm = T))

# remove NA values
stage_pv <- stage_pv[-which(is.na(stage_pv$stage_sd)),]

# join monthly average stage data to upper canyon reaches
stage_flow <- sim_mly_corr_saveout %>%
  filter(Name.Description != "BELOW FILTER PLANT TO PICNIC ROCK ACCESS") %>%
  mutate(year = year(date),
         month = month(date)) %>%
  inner_join(stage_pv, by = c("year", "month"))

#plot rating relationships for each reach
p <- ggplot(stage_flow, aes(stage_av, flow_cfs)) + 
  geom_point() +
  geom_errorbarh(aes(xmax = stage_av + stage_sd, xmin = stage_av - stage_sd)) +
  facet_wrap(~Name.Description) +
  labs(x = "Rock stage (ft)",
       y = "Monthly average flow (cfs)")

print(p)

# Generate stage-discharge rating relationships at each site. Stage from Pine View
site = unique(stage_flow$Name.Description)

for (i in 1:length(site)) {
  
  # isolate stage and flow data for this site
  dat <- filter(stage_flow, Name.Description == site[i])
  
  # fit a log regression model to the observations
  m <- lm(stage_av ~ log(flow_cfs), 
       data = dat)
  
  # extract model coefficients and goodness of fit statisic
  gof <- glance(m) %>%
    mutate(Name.Description = site[i])
  
  # predict rock stage across range of flows
  df_pred <- data.frame(flow_cfs = seq(floor(min(dat$flow_cfs, na.rm = T)), floor(max(dat$flow_cfs, na.rm = T)), 1))
  df_pred$stage = predict(m, newdata = df_pred)
  df_pred$Name.Description = site[i]
  
  # assemble data frames
  if (i == 1) {
    
    curve_fits <- gof
    rating_stage_predictions <- df_pred
    
  } else {
    
    tmp_curve_fits <- gof
    curve_fits <- rbind(curve_fits, tmp_curve_fits)
    
    tmp_rating_stage_predictions <- df_pred
    rating_stage_predictions <- rbind(rating_stage_predictions, tmp_rating_stage_predictions)
  }
  
}

# # load flow quantiles and find corresponding flow level
# setwd("../")
# quants <- read.csv("predicted_flow_quantiles.csv")
# 
# quants <- quants %>%
#   gather(quantile, flow, q10:q99) %>%
#   filter(Name.Description != "BELOW FILTER PLANT TO PICNIC ROCK ACCESS") %>%
#   arrange(Name.Description) %>%
#   mutate(flow_cfs = floor(flow)) %>%
#   left_join(rating_stage_predictions, by = c("Name.Description", "flow_cfs"))
# 
# # save-out quantile and stage data
# setwd(dirname(source_path))
# 
# write.csv(quants, file = "quantile_stages.csv")
