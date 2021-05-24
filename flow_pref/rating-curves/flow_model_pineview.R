remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(here)
library(tidyverse)
library(lubridate)

# *********** read gauge data ************
# ========================================

# Canyon Mouth
canyon_mouth <- read.csv(here::here("data","gauge","gauge_data_2021","canyon_mouth.csv")) %>%
  mutate(site = "canyon_mouth",
         date = as.Date(datetime)) %>%
  rename(flow = `X17702_00060_00003`) %>%
  select(site, date, flow)
 
# Poudre Valley canal
canal <- read.csv(here::here("data","gauge","gauge_data_2021","canal.csv")) %>%
  mutate(site = "canal",
         date = as.Date(Date)) %>%
  rename(flow = Amount) %>%
  select(site, date, flow)

# Munroe canal
# ! remove outlier canal flows greater than 300 cfs. There is a pretty obvious 
# error in the data in fall of 2020.
munroe <- read.csv(here::here("data","gauge","gauge_data_2021","munroe.csv")) %>%
  mutate(site = "munroe",
         date = as.Date(Date.Time, "%m/%d/%Y %H:%M")) %>%
  rename(flow = Value) %>%
  select(site, date, flow) %>%
  # outlier removal, here
  mutate(flow = if_else(flow > 300, NA_real_, flow))

# North Fork below Seaman Reservoir
north_fork <- read.csv(here::here("data","gauge","gauge_data_2021","north_fork.csv")) %>%
  mutate(site = "north_fork",
         date = as.Date(Date.Time, "%m/%d/%Y %H:%M")) %>%
  rename(flow = DISCHRG.Value) %>%
  select(site, date, flow)

# ****** Gap-fill the Munroe Canal record *******
# ===============================================

# annual average daily flow series at munroe
munroe_av <- munroe %>%
  mutate(doy = yday(date)) %>%
  group_by(doy) %>%
  summarise(flow_av = mean(flow, na.rm = T))

# make munroe record as long as Canyon mouth, fill with NAs where no data
munroe_gap <- canyon_mouth %>%
  rbind(munroe) %>%
  pivot_wider(names_from = site, values_from = flow) %>%
  select(date, munroe) %>%
  mutate(doy = yday(date)) 

# fill NA values with daily average flows
i = 1
munroe_gap_filled <- data.frame()
for (d in munroe_gap$doy) {
  
  if (is.na(munroe_gap$munroe[i])) {
    
    fill_val = munroe_av$flow_av[which(munroe_av$doy == d)]
    
  } else {
    
    fill_val = munroe_gap$munroe[i]
    
  }
  
  tmp <- data.frame(date = munroe_gap$date[i],
                    site = "munroe",
                    flow = fill_val)
   
  munroe_gap_filled <- rbind(munroe_gap_filled, tmp)
  
   i = i+1
}

# ********* Build model of flows at Pine View
# ===============================================
  
# join discharge data
flow_data <- canyon_mouth %>%
  rbind(canal) %>%
  rbind(north_fork) %>%
  rbind(munroe_gap_filled) %>%
  
  # pivot wider, a column for each site
  pivot_wider(names_from = site, values_from = flow) %>%
  
  # fill missing canal dates with zeros - assume no diversion if no data
  mutate(canal = replace_na(canal, 0)) %>%
  
  # daily mass balance model:
  mutate(pine_view = canyon_mouth + canal + munroe - north_fork) %>%
  
  # replace negative flow estimates
  mutate(pine_view = if_else(pine_view < 0, NA_real_, pine_view)) %>%
  
  # remove NA valued Pine View flow estimates
  mutate(na.check = is.na(pine_view)) %>%
  filter(na.check == F)

# Plot simulated flow at Pine View
p1 <- ggplot(flow_data, aes(x = date, y = pine_view)) + geom_point()
print(p1)

saveRDS(flow_data, file = here::here("flow_pref","rating-curves","pine_view_model.RDS"))


