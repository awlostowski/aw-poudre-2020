library(tidyverse)
library(minpack.lm)
library(nlstools)
library(rootSolve) 


path <- "C:/Users/angus/OneDrive/Desktop/lynker/AWW/data/"

# Read in Poudre Rock Stage
# Use scrape_page() function & loop on rock-stage-webscrape.R to pull most recent data from poudrerockreport.com
rock_report <- readRDS(paste0(path, "rock_report/stage_poudre_rock_report.rds"))

# Read in Poudre park flow 
poudre_park_flow <- readRDS(paste0(path, "poudre_park/poudre_park_flow.rds"))

# Pull data from Fort Collins site using get_flow_data()
# sensor_name      <-  "Poudre Park"
# poudre_park_flow <- get_flow_data(sensor_name = sensor_name)

library(stringr)
library(unglue)
unglue::unglue_vec(
  stage$title, 
  "{}at {x} pm{}")

# Monthly average and standard deviations of Pine View stage observations
stage <- rock_report %>%
  mutate(
    time = case_when(
      str_detect(title, regex('noon', ignore_case = T)) == TRUE ~ "12:00",
      str_detect(title, regex('noon', ignore_case = T)) == FALSE ~ time
    )
   # pm = unglue::unglue_vec(
   #    title, 
   #    patterns = c("{}@ {x}pm{}", "{}@ {x}am{}", "{}@ {x} pm{}", "{}@ {x} am{}",
   #                 "{x}pm{}", "{x}am{}", "{x} pm{}", "{x} am{}")
   #    )
  ) %>%
  mutate(
    time = format(
      round(
        strptime(paste("2001-01-01", time), format="%Y-%m-%d %H:%M"), 
            units="hours"), 
      format="%H:%M")) %>% 
  filter(is.na(time) == F) %>% 
  mutate(date = as.POSIXct(paste(as.character(date), time),  format = "%Y-%m-%d %H:%M"))

# Data from Fort Collins Poudre Park site
flow <- poudre_park_flow %>% 
  mutate(time = format(
            round(strptime(paste("2001-01-01", poudre_park_flow$time), format="%Y-%m-%d %H:%M"), 
            units="hours"), 
            format="%H:%M"),
         date = as.POSIXct(paste(as.character(date), time),  format = "%Y-%m-%d %H:%M")
         ) %>% 
  group_by(date) %>% 
  summarize(
    flow_cfs = mean(flow_cfs, na.rm = T)
  )

# Join observed Flow w/ observed Stage 

rating <- inner_join(stage, flow, by = "date") %>% 
  filter(Pineview <= 20) %>% 
  rename(rock_stage = Pineview, flow = flow_cfs)

# =======================================

# Power law model fit
m <- nlsLM(
  flow ~ a + (c*(rock_stage^f)), 
  data = rating, 
  start = list(a = 1, c = 1, f = 1.1)
  )

cis <- as.data.frame(confint(m))

fit <- data.frame(x = seq(0.25,6,0.001)) %>%
  mutate(flow = predict(m, newdata = list(rock_stage = x))) %>%
  mutate(flow_upr = cis$`97.5%`[1] + (cis$`97.5%`[2] * (x^cis$`97.5%`[3]))) %>%
  mutate(flow_lwr = cis$`2.5%`[1] + (cis$`2.5%`[2] * (x^cis$`2.5%`[3])))

pl_plot <- ggplot(rating, aes(x = rock_stage, y = flow)) +
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


# =======================================
ggplot() +
  geom_point(data = rating, aes(x = Pineview, y = flow_cfs)) +
  labs( 
    title = "Rock Report Rating Curve at Pineview",
    y = "Flow (cfs)",
    x= "Pineview Stage (ft)"
    ) +
  theme_bw() +
  theme(
    axis.text   = element_text(size = 14),
    axis.title  = element_text(size = 14)
        ) 

mod <- lm(Pineview~log(flow_cfs), data = rating)
10^mod$model[2]
summary(mod)
ggplot() +
  # geom_point(data = rating, aes(x = Pineview, y = flow_cfs)) +
  geom_line(data = fit, aes(x = fitted, y = flow_cfs)) 

fit <- rating %>% 
  mutate(
    fitted = mod$fitted.values
    )


# join monthly average stage data to upper canyon reaches
# rating <- sim_mly_corr_saveout %>%
#   filter(Name.Description != "BELOW FILTER PLANT TO PICNIC ROCK ACCESS") %>%
#   mutate(year = year(date),
#          month = month(date)) %>%
#   inner_join(stage_pv, by = c("year", "month"))
