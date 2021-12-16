remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(here)
library(tidyverse)
library(lubridate)
library(minpack.lm)
library(nlstools)
library(rootSolve) # for computing flow preference thresholds
library(cowplot); theme_set(theme_cowplot())

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

flow_stage <- ggplot(filter(flow, date >= as.Date("2008-1-01")), aes(x = date, y = flow)) +
  geom_line() +
  geom_point(data = filter(stage, date >= as.Date("2008-1-01")), aes(x = date, y = rock_stage*500), color = "red") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Estimated Flow at Pine View (cfs)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./500, name="Pine View Rock Report stage (ft)")
  ) +
  theme_classic()

print(flow_stage)

# =======================================

rating <- stage %>%
  inner_join(flow, by = "date")

# =======================================
# Power law model fit
m <- nlsLM(flow ~ a + (c*(rock_stage^f)), data = rating, start = list(a = 1, c = 1, f = 1.1))

cis <- as.data.frame(confint(m))

fit <- data.frame(x = seq(0.25,6,0.001)) %>%
  mutate(flow = predict(m, newdata = list(rock_stage = x))) %>%
  mutate(flow_upr = cis$`97.5%`[1] + (cis$`97.5%`[2] * (x^cis$`97.5%`[3]))) %>%
  mutate(flow_lwr = cis$`2.5%`[1] + (cis$`2.5%`[2] * (x^cis$`2.5%`[3])))

pl_plot <- ggplot(rating, aes(x = rock_stage, y = flow)) +
  geom_point(alpha = 0.6, size = 1.2) +
  geom_line(data = fit, aes(x = x, y = flow), color = "red") +
  theme_classic() +
  ylab("Flow @ Pine View (cfs)") +
  xlab("Rock Report stage @ Pine View (ft)")

print(pl_plot)


# =======================================

# stage bins from flow pref survey
stage_bin_edges <- seq(0.125,5.625, by = 0.25)
stage_bin_centers <- data.frame(bin = seq(1,22),
                                bin_center = seq(0.25,5.5, by = 0.25))

rating_bin <- rating %>%
  mutate(bin = cut(rock_stage, stage_bin_edges, right = F, labels = F)) %>%
  left_join(stage_bin_centers, by = "bin")

p <- ggplot(rating_bin, aes(x = bin_center, y = flow, group = bin_center))+ 
  geom_boxplot(fill = "blue", alpha = 0.4, outlier.size = 0.1) +
  geom_jitter(width = 0.05, size = 0.1) +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="red", fill="red") +
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

# ==========================================

flow_pref_stage <- read.csv(here::here("private_data", "flow_pref_by_reach.csv")) %>%
  filter(flow < 6) %>%
  rename(stage = flow) %>%
  left_join(fit, by = c("stage" = "x")) %>%
  select(segment, stage, flow, pref.average, pci2, n_obs)
  

flow_pref_nostage <- read.csv(here::here("private_data", "flow_pref_by_reach.csv")) %>%
  filter(flow > 6) %>%
  mutate(stage = NA_real_) %>%
  select(segment, stage, flow, pref.average, pci2, n_obs)

flow_pref <- flow_pref_stage %>%
  rbind(flow_pref_nostage)

# wite flow pref data to csv
write.csv(filter(flow_pref, n_obs > 1), file = "plots/flow_pref_updated_2021/flow_pref_results.csv", row.names = F )

# Plot
for (s in unique(flow_pref$segment)) {
  
  dat <- filter(flow_pref, segment == s & n_obs > 1)
  
  # Calculate where flow/stage crosses zero (defines flow acceptability)
  flow_thresh = uniroot.all(approxfun(dat$flow, dat$pref.average), 
                            interval = range(dat$flow))
  
  # If there's only 1 zero crossing min = acceptability threshold
  # and max = max flow
  # If there's two, then max = second crossing
  if(length(flow_thresh) == 1){
    flow_thresh_min = flow_thresh
    flow_thresh_max = max(dat$flow)
  } else {
    flow_thresh_min = flow_thresh[1]
    flow_thresh_max = flow_thresh[2]
  }
  
  
  flow_pref_plot <- 
    ggplot() +
    geom_point(data = filter(flow_pref, n_obs > 2, segment == s), 
               aes(x = flow, y = pref.average, size = pci2), 
               color = 'blue') +
    geom_hline(yintercept = 0) +
    geom_segment(aes(x = flow_thresh_min, xend = flow_thresh_max, y = 0, yend = 0), 
                 color = "black",
                 arrow = arrow(ends = "both"),
                 lwd = 1.5) +
    scale_radius(name = expression(PCI[2]), range = c(1,6)) +
    labs(x = "Flow (cfs)",
         y = "Preference Score",
         title = paste0(s, " Flow Preference Curve")) +
    annotate(geom = "text", y = -0.3, x = mean(c(flow_thresh_min, flow_thresh_max)),
             label = paste0("Acceptable Flow Range\n",
                            signif(flow_thresh_min, 2),
                            " cfs to ",
                            signif(flow_thresh_max, 2),
                            " cfs"))
  
  if(s == "Big South"){
    flow_pref_plot <- flow_pref_plot +
      labs(x = "Flow at La Poudre Pass Blw Long Draw (cfs)")
  } else if(s == "Filter Plant"){
    
    flow_pref_plot <- flow_pref_plot +
      labs(x = "Flow at Canyon Mouth (cfs)")
    
  } else if(s == "Poudre Whitewater Park") {
    
    flow_pref_plot <- flow_pref_plot +
      labs(x = "Flow at Fort Collins (cfs)")
    
  } else {
    
    flow_pref_plot <- flow_pref_plot +
      labs(x = "Flow at Pine View (cfs)")
    
  }
  
  print(flow_pref_plot)
  
  # Export plot
  segment_name2 = str_replace_all(s, pattern = " ", replacement = "")
  save_plot(filename = paste0("plots/flow_pref_updated_2021/flow_pref_",
                              segment_name2,
                              "_av_only_annotated.png"),
            plot = flow_pref_plot,
            base_width = 7)
  
  a = 2
  
  
}


  
  
