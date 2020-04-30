# Script for running example economic analysis
# for the Poudre Whitewater Parl

# Keith Jennings
# kjennings@lynkertech.com
# 2020-04-29

# Load packages 
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())

# Import data
sales <- read.csv("econ/data/pwp_expenditures_margin.csv")
mults <- read.csv("econ/data/bea_retail_multipliers_final_demand_typei.csv")

# Total sales figures from Loomis & McTernan (2011)
lo_res = 118744
lo_vis = 176021
hi_res = 336223
hi_vis = 409115

# Convert expenditure amounts to percentages
sales <- sales %>% 
  mutate(exp_res_pct = exp_res / sum(exp_res) * 100,
         exp_vis_pct = exp_vis / sum(exp_vis) * 100)

# Compute high and low total per-category spending for residents and visitors
# Then compute the revenue based on spending times margin
sales <- sales %>% 
  mutate(exp_res_tot_lo = exp_res_pct/100 * lo_res,
         exp_res_tot_hi = exp_res_pct/100 * hi_res,
         exp_vis_tot_lo = exp_vis_pct/100 * lo_vis,
         exp_vis_tot_hi = exp_vis_pct/100 * hi_vis) %>% 
  mutate(rev_res_tot_lo = exp_res_tot_lo * margin,
         rev_res_tot_hi = exp_res_tot_hi * margin,
         rev_vis_tot_lo = exp_vis_tot_lo * margin,
         rev_vis_tot_hi = exp_vis_tot_hi * margin)

# Sum the resident and visitor revenue by low/high scenario
revenue <- data.frame(scenario = c("low", "high"),
                      rev_tot = c(sum(sales$rev_res_tot_lo + 
                                        sales$rev_vis_tot_lo),
                                  sum(sales$rev_res_tot_hi + 
                                        sales$rev_vis_tot_hi)))

# Use final-demand multipliers to calculate output, value add, and jobs
revenue <- revenue %>% 
  mutate(output = rev_tot * filter(mults, type == "output")$multiplier,
         value_add = rev_tot * filter(mults, type == "value_add")$multiplier,
         jobs = rev_tot / 1e6 * filter(mults, type == "employment")$multiplier)

# Print table
revenue

#     scenario   rev_tot    output    value_add     jobs
# 1      low    97075.73    132158.9  81902.79    1.445361
# 2     high    244495.43   332856.1  206280.80   3.640292
# 
