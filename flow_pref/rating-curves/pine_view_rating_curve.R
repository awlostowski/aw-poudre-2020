remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(here)
library(tidyverse)
library(lubridate)

# *********** read data ************
# ========================================

stage <- readRDS(here::here("flow_pref","rating-curves","stage_data_2007_2021.RDS")) %>%
  select(date, Pineview) %>%
  rename(rock_stage = Pineview) %>%
  filter(!is.na(rock_stage) & rock_stage > 0)

flow <- readRDS(here::here("flow_pref","rating-curves","pine_view_model.RDS")) %>%
  select(date, pine_view) %>%
  rename(flow = pine_view) %>%
  filter(!is.na(flow) & flow > 0)

# =======================================

rating <- stage %>%
  inner_join(flow, by = "date")


# =======================================

# stage bins from flow pref survey
stage_bin_edges <- seq(0.125,5.625, by = 0.25)
stage_bin_centers <- data.frame(bin = seq(1,22),
                                bin_center = seq(0.25,5.5, by = 0.25))

rating_bin <- rating %>%
  mutate(bin = cut(rock_stage, stage_bins, right = F, labels = F)) %>%
  left_join(stage_bin_centers, by = "bin")

p <- ggplot(rating_bin, aes(x = bin_center, y = flow, group = bin_center))+ 
  geom_boxplot(fill = "blue", alpha = 0.4, outlier.size = 0.1) +
  geom_jitter(width = 0.05, size = 0.1) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  theme_classic() +
  ylab("Flow @ Pine View (cfs)") +
  xlab("Rock Report stage @ Pine View (ft)")

print(p)

rating_summary <- rating_bin %>%
  rename(rock_stage_ft = bin_center) %>%
  group_by(rock_stage_ft) %>%
  summarize(av_flow_cfs = mean(flow, na.rm = T),
            med_flow_cfs = median(flow, na.rm = T),
            sd_flow_cfs = sd(flow, na.rm = T))
