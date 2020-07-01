# Script for converting boatable days PNG plots to animated gifs

# Keith Jennings
# 2020-06-15
# kjennings@lynkertech.com

# Code will only work if ImageMagick is loaded on system

# Load package
library(tidyverse) # needed for pipe

################################################################################
# Add white backgrounds to all PNGs

# Identify plot path
plot_path <- "plots/boatable_days/raw/"

# List plots with transparent backgrounds
plots_transparent <- list.files(path = plot_path, 
                                pattern = "*.png")


# Loop through files and add white backgrounds
for(i in 1:length(plots_transparent)){
  old_plot <- plots_transparent[i]
  new_plot <- paste0(tools::file_path_sans_ext(old_plot),
                     "_wht.png")
  system(paste0("convert -flatten ",
                plot_path,
                old_plot, " ",
                plot_path,
                new_plot))
}

################################################################################
# Make gifs

# Add second plot path
plot_path2 <- "plots/boatable_days/"

# Make gif for canyon gage
paste0("convert -delay 200 ",
       plot_path,
       "*can_wht.png ",
       plot_path2,
       "observed_impact_canyon.gif") %>% 
  system(.)

# Make gif for fort collins gage
paste0("convert -delay 200 ",
       plot_path,
       "*fcg_wht.png ",
       plot_path2,
       "observed_impact_foco.gif") %>% 
  system(.)

# Make combined gif
paste0("convert -delay 200 ",
      plot_path,
      "*wht.png ",
      plot_path2,
      "observed_impact_all.gif") %>% 
  system(.)
