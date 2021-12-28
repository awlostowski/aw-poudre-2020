# Script for filtering and plotting survey data for flow preference analysis

# Keith Jennings
# kjennings@lynkertech.com
# 2020-06-19

# Load packages
library(tidyverse)
library(here)
library(cowplot); theme_set(theme_cowplot())
library(rootSolve) # for computing flow preference thresholds

# Import data
# Note .RData files are in gitignore, so these must be on your local machine
# load(file = here("flow_pref", "survey-analysis", "respondend-attributes.Rdata"))
# load(file = here("flow_pref", "survey-analysis", "flow-pref-data.Rdata"))
respondent.attributes <-
  readRDS(file = here::here("private_data", "respondent-attributes_20200630.RDS"))
flowpref.dat <- 
  readRDS(file = here::here("private_data", "flow-pref-data_20200630.RDS"))

# Commented code moved to poudre_survey_clean_format
# Can be deleted
#  # Convert flow to numeric
# flowpref.dat <- flowpref.dat %>% 
#   mutate(flow = as.numeric(flow))
# # Assign all users an experience code
# respondent.attributes <- respondent.attributes %>% 
#   mutate(skill = case_when(skill.novice == T ~ "novice",
#                            skill.intermediate == T ~ "intermed",
#                            skill.advanced == T ~ "advanced",
#                            skill.expert == T ~ "expert"))

# Next a list of unqualified respondent IDs based on:
# 1) skill level (remove novice)
# 2) trip frequency (remove 1 trip per season)
# 3) reporting confidence (remove Not comfortable at all)

# Create thresholds for each category
skill_thresh = "novice"
visit_thresh = "1 time a season"
confidence_thresh = "Not comfortable at all"

# Filter to respondent IDs above the thresholds
respondent.id.valid = respondent.attributes %>% 
  filter(., skill != skill_thresh,
         visit_freq != visit_thresh,
         report_confidence != confidence_thresh) %>% 
  pull(., respondent_id)

# Make a vector of segment names
segments <- unique(flowpref.dat$segment.name)

# Calculate the range between maximum and minimum preference code values
# These are used in calculating the PCI2 value below
# These can be entered by hand per survey, but they are done programatically here
pref_range = max(flowpref.dat$preference.code, na.rm = T) - 
              min(flowpref.dat$preference.code, na.rm = T)

# Create dummy data frame for storing average flow preference and PCI2
# values per segment and flow level
flow_pref_summary <- data.frame()

# Loop through each segment
# calculate flow preference
# and plot the data
for(i in 1:length(segments)){
  
  # Identify segment
  segment_name = segments[i]
  segment_name2 = str_replace_all(segment_name, pattern = " ", replacement = "")
  
  # Create temporary data frame for the segment data
  flow.dat.tmp <- flowpref.dat %>%
        filter(!is.na(preference.code) & 
             segment.name == segment_name &
             respondent_id %in% respondent.id.valid)
  
  # Summarize the data for calculation of PCI2
  # m is the denominator of PCI2
  results <- flow.dat.tmp %>%
    group_by(flow) %>%
    summarize(pref.average = mean(preference.code),   # average preference
              n_obs = length(flow),                   # count # of observations
              m = ifelse(n_obs %% 2 == 0,
                         (pref_range * (n_obs^2))/ 2,
                         (pref_range * ((n_obs^2) - 1))/ 2)) # max distance vector

  # Summarize the data by flow and preference code
  # This will be used to compute distance sum below (the numerator of PCI2)
  flow.summary.tmp <- flow.dat.tmp %>% 
    group_by(flow, preference.code) %>% 
    summarize(n_obs = n())
  
  # Calculate the distance sum as in Vaske et al. (2010)
  # Here, distance is only computed for preference codes of opposite signs
  # (i.e., zero is not considered)
  distance_sum <-
    bind_rows(
      full_join(filter(flow.summary.tmp, preference.code == 2),
                filter(flow.summary.tmp, preference.code == -2),
                by = "flow"),
      full_join(filter(flow.summary.tmp, preference.code == 1),
                filter(flow.summary.tmp, preference.code == -2),
                by = "flow"),
      full_join(filter(flow.summary.tmp, preference.code == 2),
                filter(flow.summary.tmp, preference.code == -1),
                by = "flow"),
      full_join(filter(flow.summary.tmp, preference.code == 1),
                filter(flow.summary.tmp, preference.code == -1),
                by = "flow")      
    ) %>% 
    group_by(flow) %>% 
    summarize(d = sum(2 * n_obs.x * n_obs.y * 
                        (preference.code.x - preference.code.y),
                      na.rm = T))
  
  # Bind the d and m data and compute PCI2 (d/m)
  results <- left_join(results, distance_sum,
                       by = "flow") %>% 
    mutate(pci2 = d/m)
  
  # Filter results so that n_obs > 5
  results <- filter(results, n_obs > 5)

  # Calculate where flow/stage crosses zero (defines flow acceptability)
  flow_thresh = uniroot.all(approxfun(results$flow, results$pref.average), 
                            interval = range(results$flow))
  
  # If there's only 1 zero crossing min = acceptability threshold
  # and max = max flow
  # If there's two, then max = second crossing
  if(length(flow_thresh) == 1){
    flow_thresh_min = flow_thresh
    flow_thresh_max = max(results$flow)
  } else {
    flow_thresh_min = flow_thresh[1]
    flow_thresh_max = flow_thresh[2]
  }
  
  # Plot
  flow_pref_plot <- 
    ggplot() +
    # geom_jitter(data = filter(flowpref.dat, segment.name == segment_name), 
    #             aes(x = as.numeric(flow), 
    #                 y = preference.code), 
    #             size = 1, alpha = 0.3) +
    geom_point(data = filter(results, n_obs > 2), 
               aes(x = flow, y = pref.average, size = pci2), 
               color = 'blue') +
    geom_hline(yintercept = 0) +
    scale_radius(name = expression(PCI[2]), range = c(1,6)) +
    geom_segment(aes(x = flow_thresh_min, xend = flow_thresh_max, y = 0, yend = 0), 
                 color = "black",
                 arrow = arrow(ends = "both"),
                 lwd = 1.5) 
          # scale_radius provides more visually intuitive point scaling
          # than scale_size
  
  # Add axis labels and titles
  # Plus acceptable flow/stage range
  # Some reaches have stage values, others have flow
  # So use if-else to assign correct label
  if(max(results$flow, na.rm = T) < 100){
    flow_pref_plot <- flow_pref_plot +
      labs(x = "Stage (ft.)",
           y = "Preference Score",
           title = paste0(segment_name, " Stage Preference Curve")) +
      annotate(geom = "text", y = -0.3, x = mean(c(flow_thresh_min, flow_thresh_max)),
               label = paste0("Acceptable Stage Range\n",
                              signif(flow_thresh_min, 2),
                              " ft. to ",
                              signif(flow_thresh_max, 2),
                              " ft."))
  }else{
    flow_pref_plot <- flow_pref_plot +
      labs(x = "Flow (cfs)",
           y = "Preference Score",
           title = paste0(segment_name, " Flow Preference Curve"))  +
      annotate(geom = "text", y = -0.3, x = mean(c(flow_thresh_min, flow_thresh_max)),
               label = paste0("Acceptable Flow Range\n",
                              signif(flow_thresh_min, 3),
                              " cfs to ",
                              signif(flow_thresh_max, 3),
                              " cfs"))
    }

  # Export plot
  save_plot(filename = paste0("plots/flow_pref/flow_pref_",
                              segment_name2,
                              "_av_only_annotated.png"),
            plot = flow_pref_plot,
            base_width = 7)
  
  # Add summary data to data frame
  flow_pref_summary <- bind_rows(flow_pref_summary,
                                 select(results, flow, n_obs, pref.average, pci2) %>% 
                                   mutate(segment = segment_name))
  
}

# Export the flow preference data
write.csv(x = flow_pref_summary,
          file = "private_data/flow_pref_by_reach.csv", 
          row.names = F, quote = F)
