remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(rstudioapi)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(cowplot); theme_set(theme_cowplot())
library(hydroTSM)
library(RColorBrewer)

# Set working directory to source file location
source_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(source_path))

# this script will compare simulated flows to observations, then apply a bias correction to simulated flow duration curves

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Load simulated and observed flow data
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#------------------------------Load data ---------------------------------------------
# load the nwm-simulated and observed streamflow data sets
load(file = "obs_flow.Rdata")
load(file = "sim_flow.Rdata")

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Estimate monthly flows above the N. Fk. concfluence by subtracting N. Fk flows
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#------------------------------ monthly flows above confluence ---------------------------------------------

# monthly averages, diff canyon mouth and n fk, reshape and clean 
obs_mly_abnfk <- observations %>%
  group_by(year(date),
           month(date),
           ID) %>%
  summarize(date = first(date),
            flow_cfs = mean(flow_cfs, na.rm = T)) %>% # monthly average flow
  spread(ID,flow_cfs) %>%
  select(date,'6752000', CLANSECO) %>%                # isolate observations from the canyon mouth and north fork
  mutate(ABNFK = `6752000` - CLANSECO) %>%            # subtract north fork flows from canyon mouth flows, to estimate flows above confluence
  ungroup()

# reshape and filter data within the boating season (April - October)
obs_mly_abnfk <- gather(obs_mly_abnfk, ID, flow_cfs, `6752000`:ABNFK) %>%
  filter(ID == "ABNFK" & is.na(flow_cfs) == F & month(date) >= 4 & month(date) <= 10, flow_cfs > 0)

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Compare monthly average simulations against monthly average observations
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# monthly average flows at nhd ID 2899311, a.k.a PINEVIEW FALLS TO BRIDGES TAKE-OUT
sim_mly_abnfk<- filter(simulations, ID == "2899311") %>%
  group_by(year(date),
           month(date),
           ID) %>%
  summarize(date = first(date),
            flow_cfs = mean(flow_cfs, na.rm = T))

# join observations above the n. fk.
sim_obs_mly_abnfk <- sim_mly_abnfk[c(1,2,4,3,5)] %>%
  ungroup() %>%
  rbind(obs_mly_abnfk) %>%
  spread(ID, flow_cfs) %>%
  filter(month(date) >= 4 & month(date) <= 10 & year(date) >= 2005)

# plot simulated against observed monthly flows
p1 <- ggplot(sim_obs_mly_abnfk, aes(x = `ABNFK`, y = `2899311`)) +
  geom_point() +
  labs(y = "Simulations @ PINEVIEW to BRIDGES (cfs)",
       x = "Observations above N. Fk. confluence (cfs)") +
  geom_abline(slope = 1)

print(p1)

# plot monthly flows over the average water year
df_p2 <- sim_obs_mly_abnfk %>%
  gather(ID, flow_cfs, `2899311`:ABNFK) 

p2 <- ggplot(df_p2, aes(x = as.factor(month(date)), y = flow_cfs, fill = ID)) +
  geom_boxplot()

print(p2)

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# bias correction with a linear model
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# create a linear model that explains simulated flows (ID == 2899311) as a function of observed (ID == ABNFK)
lm.sim_obs <- lm(`2899311` ~ ABNFK, sim_obs_mly_abnfk)

# adjust simulated monthly flows using the slope and intercept terms from the linear model
sim_mly_corr <- simulations %>%
  group_by(year(date),
           month(date),
           ID) %>%
  summarize(date = first(date),
            flow_cfs = mean(flow_cfs, na.rm = T)) %>%
  mutate(corr = (flow_cfs - lm.sim_obs$coefficients[1])/lm.sim_obs$coefficients[2]) %>% # flow correction operation w/ lm parameters
  ungroup() %>%
  filter(corr > 0) %>%  # remove any negative flows introduced during the correction process
  select(date, ID, corr) %>%  
  rename(flow_cfs = corr) %>%
  spread(ID, flow_cfs)

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Construct flow duration curves from simulated flows, using corrected data
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# use hydroTSM to develop exceedance probabilities
flow_duration <- fdc(sim_mly_corr[ ,2:11])

flow_duration <- as.data.frame(flow_duration) %>%
  gather(ID,exprob) %>%
  cbind(
    gather(sim_mly_corr,ID,flow_cfs,2:11)[,c("flow_cfs")]
    ) %>%
  filter(is.na(flow_cfs) == F & is.na(exprob) == F)

# plot ensemble of flow duration curves
p4 <- ggplot(flow_duration, aes(x = exprob, y = flow_cfs, color = ID)) +
  geom_line() +
  scale_y_continuous(trans='log10')
  labs(x = "Exceedance Probability", 
       y = "Flow (cfs)")

print(p4)

#--------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Calculate quantile flow for each segment
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# calculate flow quantiles
flow_quants <- filter(sim_mly_corr, month(date) >= 4 & month(date) <= 10) %>%
  gather(ID, flow_cfs, 2:11) %>%
  filter(is.na(flow_cfs) == F) %>%
  group_by(ID) %>%
  summarize(q10 = quantile(flow_cfs,probs = 0.1),
            q25 = quantile(flow_cfs,probs = 0.25),
            q50 = quantile(flow_cfs,probs = 0.5),
            q75 = quantile(flow_cfs,probs = 0.75),
            q90  = quantile(flow_cfs,probs = 0.90)
            ) %>%
  ungroup()

# change ID from character to numeric for joining purposes
flow_quants$ID = as.numeric(flow_quants$ID)

# join reach names 
flow_quants <- left_join(flow_quants,unique(simulations[,c("ID","Name.Description")]), by = "ID") %>%
  arrange(ID)

# explort to .csv file
setwd(dirname(source_path))
write.csv(flow_quants, file = "predicted_flow_quantiles.csv")



