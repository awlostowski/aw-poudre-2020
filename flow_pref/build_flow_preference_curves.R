##------------------------------------------------------------------------------
##
## Script name: build_flow_preference_curves.R
##
## Purpose of script:
##  Generate flow preference curves for boating reaches along the Poudre River
##
## Author: Adam N. Wlostowski & Keith Jennings
##
## Date Created: 2021-12-28
##
## Copyright (c) Adam N. Wlostowski, 2021
## Email: awlostowski@lynker.com
##
## ---------------------------
##
## Notes:
##
##  - This script is a revision of poudre_flow_pref_filter_plot.R
##
##  - Stage - preference relationships are transformed to flow - pref
##    using the rating curved constructed by /rating_curves/build_rating_curve.R
##
##  - respondent-attributes_20200630.RDS and flow-pref-data_20200630.RDS are
##    created by /flow_pref/survey-analysis/poudre_survey_clean_format.R
##
##  - if no rating curve data are available, then the analysis will default to
##    a stage-preference analysis at sites where surveys were developed in terms
##    of stage
##
##------------------------------------------------------------------------------

remove(list = ls())  # clear all workspace variables
cat("\014")          # clear command line

## load packages
library(here)
library(tidyverse)
library(logger)
library(cowplot); theme_set(theme_cowplot())
library(rootSolve) # for computing flow preference thresholds

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

##------------------------------------------------------------------------------
## Function definitions

LabelString <- function(thresh_min, thresh_max, max_plot, is.stage = FALSE) {
  
  if (is.stage == TRUE & !is.na(thresh_max)) {
    
    label.string <- paste0(
      "Acceptable Stage Range\n",
      signif(flow_thresh_min, 2),
      " ft. to",
      signif(flow_thresh_max, 2),
      " ft."
      ) 
    
  } else if (is.stage == TRUE & is.na(thresh_max)) {
    
    label.string <- paste0(
      "Acceptable Stage Range\n",
      signif(flow_thresh_min, 2),
      " ft. and above"
    ) 
    
  } else if (is.stage == FALSE & !is.na(thresh_max)) {

    label.string <- paste0(
      "Acceptable Flow Range\n",
      signif(flow_thresh_min, 2),
      " cfs to",
      signif(flow_thresh_max, 2),
      " cfs"
    ) 
    
  } else {
    
    label.string <- paste0(
      "Acceptable Flow Range\n",
      signif(flow_thresh_min, 2),
      " cfs. and above"
    ) 
    
  }

  return(label.string)
  
}

##------------------------------------------------------------------------------
## Executed statements



#===========================
# Load tidy'd survey data

attribute.file <- here::here(
  "private_data", 
  "respondent-attributes_20220105.RDS"
  # "respondent-attributes_20200630.RDS"
  )
flowpref.file  <- here::here(
  "private_data", 
  "flow-pref-data_20220105.RDS"
  # "flow-pref-data_20200630.RDS"
  )

# Import data
if (file.exists(attribute.file) & file.exists(attribute.file)) {
  respondent.attributes <-readRDS(file = attribute.file)
  flowpref.dat          <- readRDS(file = flowpref.file)
} else {
  logger::log_error(
    'could not locate respondent attribute or flow pref data'
    )
  stop()
}

#===========================
# convert Pineview Rock stage values to flow, using rating curve

# filename and path to rating curve model object
rating.file <- here::here("rating_curves","piecewise_pineview_rating_model.RDS")

# stage "flow" threshold
stage.thresh = 5.5

# load rating curve model
m <- readRDS(rating.file)


if (file.exists(rating.file)) {
  
  logger::log_info(
    "found rating curve model - creating flow-preference curves at all sites"
    )
  
  # load rating curve model
  m <- readRDS(rating.file)
  
  # use model to convert stage to flow
  flowpref.dat <- flowpref.dat %>%
    mutate(
    stage = case_when(
      flow <= stage.thresh            ~ flow,                           # use rating model to convert stage values to flow values
      TRUE                            ~ NA_real_
                     )
          )
    
  flowpref.dat$flow_transform <- predict(m, newdata = flowpref.dat)
  
  flowpref.dat <- flowpref.dat %>%
    mutate(
      flow_transform = case_when(                            
        is.na(flow_transform) == TRUE               ~ flow,                           # convert stage values between 5.5 and 10 to 5.5 
        is.na(flow_transform) == FALSE              ~ flow_transform
      )
    ) %>% 
    select(-flow) %>%
    rename(flow = flow_transform)
    
  
} else {
  
  logger::log_info("rating curve model NOT found!")
  logger::log_info("creating stage-preference curves at upper canyon sites")
  
  flowpref.dat <- flowpref.dat %>%
    mutate(
      stage = if_else(
        flow <= stage.thresh,
        flow,
        NA_real_
      ),
      flow_transform = if_else(
        flow <= stage.thresh,
        NA_real_,
        flow
      )
    ) %>%
    select(-flow) %>%
    rename(flow = flow_transform)
  
}

#===========================
# Mask unqualified respondents based on:
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

#===========================
# Calculate PCI2 scores and generate flow pref curves at each site

# Make a vector of segment names
segments <- unique(flowpref.dat$segment.name)

# Calculate the range between maximum and minimum preference code values
# These are used in calculating the PCI2 value below.
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
    group_by(flow, stage) %>%
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
  if(all(is.na(results$flow)) == TRUE) {
    flow_thresh = uniroot.all(approxfun(results$stage, results$pref.average), 
                              interval = range(results$stage))
  } else {
    flow_thresh = uniroot.all(approxfun(results$flow, results$pref.average), 
                              interval = range(results$flow))
  }
  # If there's only 1 zero crossing min = acceptability threshold
  # and max = max flow
  # If there's two, then max = second crossing
  if(length(flow_thresh) == 1){
    flow_thresh_min = flow_thresh
#    flow_thresh_max = max(results$flow)
    flow_thresh_max = NA
    flow_thresh_max_plot = max(results$flow)
  } else {
    flow_thresh_min = flow_thresh[1]
    flow_thresh_max = flow_thresh[2]
    flow_thresh_max_plot = flow_thresh[2]
  }
  
  # Plot
  flow_pref_plot <- 
    ggplot() +
    
    # Plot average flow prefernce scores as circle markers, where radius
    # is scaled to PCI2 score. 
    
    # geom_jitter(data = filter(flowpref.dat, segment.name == segment_name), 
    #             aes(x = as.numeric(flow), 
    #                 y = preference.code), 
    #             size = 1, alpha = 0.3) +
    geom_point(data = filter(results, n_obs > 2), 
               aes(x = flow, y = pref.average, size = pci2), 
               color = 'blue') +
    geom_hline(yintercept = 0) +
    scale_radius(name = expression(PCI[2]), range = c(1,6)) +
    geom_segment(aes(x = flow_thresh_min, xend = flow_thresh_max_plot, y = 0, yend = 0), 
                 color = "black",
                 arrow = arrow(ends = "both"),
                 lwd = 1.5) +
    scale_x_continuous(limits = c(0, max(results$flow))) +
    theme(legend.position = "left")
  # Add axis labels and titles
  # Plus acceptable flow/stage range
  # Some reaches have stage values, others have flow
  # So use if-else to assign correct label
  
  if(all(is.na(results$flow)) == TRUE){
    
    logger::log_info("Creating STAGE-preference at {segment_name}")
    
    flow_pref_plot <- flow_pref_plot +
      labs(x = "Stage (ft.)",
           y = "Preference Score",
           title = paste0(segment_name, " Stage Preference Curve")) +
      annotate(geom = "text", y = -0.3, x = mean(c(flow_thresh_min, flow_thresh_max_plot)),
               label = LabelString(
                 flow_thresh_min, 
                 flow_thresh_max, 
                 flow_thresh_max_plot, 
                 is.stage = TRUE
                 )
               )
  }else{
    
    logger::log_info("Creating FLOW-preference at {segment_name}")
    
    flow_pref_plot <- flow_pref_plot +
      labs(x = "Flow (cfs)",
           y = "Preference Score",
           title = paste0(segment_name, " Flow Preference Curve"))  +
      annotate(geom = "text", y = -0.3, x = mean(c(flow_thresh_min, flow_thresh_max_plot)),
               label = LabelString(
                 flow_thresh_min, 
                 flow_thresh_max, 
                 flow_thresh_max_plot
                 )
               )
  }
  
  # Open ended survey response (Boating flow preference data)
  respondent_flow_pref <- flowpref.dat %>%
      filter(segment.name == segment_name) %>%
      dplyr::select(-flow, -stage) %>% 
      pivot_longer(
        cols      = c(flow_min_craft:flow_max_craft), 
        names_to  = "respondent_var", 
        values_to = "respondent_val") %>%
      mutate(
        respondent_val = case_when(                            
          respondent_val > 5.5 & respondent_val <= 10 ~ 5.5,                            # convert stage values between 5.5 and 10 to 5.5 
          TRUE                                        ~ respondent_val
        ),  
        stage = case_when(
          respondent_val <= stage.thresh  ~ respondent_val,                             # create stage column for flow values <= 5.5  
          TRUE                            ~ NA_real_
        )
      )
  # Piecewise regression model to predict flow from stage values
  respondent_flow_pref$flow_transform <-  predict(m, newdata = respondent_flow_pref)    # use model to convert stage to flow
  
  respondent_flow_pref <- respondent_flow_pref %>%
      mutate(
        flow_transform = case_when(                            
          is.na(flow_transform) == TRUE               ~ respondent_val,                 # combine stage-flow converted data with actual flow values
          is.na(flow_transform) == FALSE              ~ flow_transform
        )
      ) %>% 
      rename(flow = flow_transform) %>%
      filter(flow <= flow_thresh_max_plot) %>%                                          # remove flow values greater than the max values used in flow preference curve
      mutate(
        respondent_var = factor(
          respondent_var,                                                               # reorder factors for graphing
          levels = c("flow_min_craft", "flow_best_technical",
                     "flow_min_acceptable", "flow_best_average",
                     "flow_best_challenge", "flow_max_craft")
        )
      )
  
  # Boating preference box plot
  boating_preference_plot <- 
      ggplot() +
        geom_boxplot(
          data = respondent_flow_pref, 
          aes(
            x = flow, 
            y = respondent_var)
          ) +
        labs(
           x     = "Flow (cfs)",
           y     = "Boating preferences",
           title = paste0(segment_name, " Boating Preferences")
           ) +
    scale_x_continuous(limits = c(0, max(results$flow)))
  
  # plot Boating preference and flow preference on same plot and save
  boat_flow_pref_plots <- 
      cowplot::plot_grid(
                        boating_preference_plot, 
                        flow_pref_plot,
                        ncol  = 1,
                        align = "v",
                        axis  = "l"
                        )
  # Export plot
  save_plot(filename = paste0("plots/flow_pref/flow_pref_",
                              segment_name2,
                              "_annotated.png"),
            plot = boat_flow_pref_plots,
            base_width = 10,
            base_height = 10
            )
  
  # Add summary data to data frame
  flow_pref_summary <- bind_rows(flow_pref_summary,
                                 select(results, flow, stage, n_obs, pref.average, pci2) %>% 
                                   mutate(segment = segment_name,
                                          minimally.acceptable.flow = flow_thresh_min,
                                          maximally.acceptable.flow = flow_thresh_max))
  
}

#===========================
# Export the flow preference data

write.csv(x = flow_pref_summary,
          file = "private_data/flow_pref_by_reach.csv", 
          row.names = F, quote = F)
