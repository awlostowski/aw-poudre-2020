# Load packages
library(cowplot)
theme_set(theme_cowplot(12))
library(tidyverse)
library(dataRetrieval)
library(readxl)
library(lubridate)
library(scales)
library(epitools)

# FLOW DATA -----------------------------------------------------------------
# CLAFTCO = 1; CLAFORCO = 2

### CACHE LA POUDRE AT CANYON MOUTH NEAR FORT COLLINS (CLAFTCCO)
siteNumber1 <- "06752000" 
CLAFTCCO <- readNWISsite(siteNumber1)
parameterCd <- "00060" #ID for discharge info
USGS1 <- readNWISdv(siteNumber1,parameterCd,
                    "1980-01-01","2020-06-08")
USGS1 <- na.omit(USGS1) # Omit NA
USGS1 <- USGS1 %>% 
  rename(discharge_cfs = X_00060_00003)
USGS1 <- USGS1[3:4] # Collect only date & discharge

# Data after 2007 since USGS recorded only until 2007
CODWS <- read.csv("/Users/nayounghur/Desktop/CLAFTCOdata.csv") # Data downloaded from CODWR website
CODWS <- na.omit(CODWS) # Omit NA
CODWS <- CODWS %>% 
  rename(discharge_cfs = DISCHRG.Value, Date = DateTime)
CODWS <- CODWS %>% 
  filter(discharge_cfs!= 0) # Remove 0s b/c was empty cells
CODWS$Date <- ymd(CODWS$Date) # Make from character to date
CODWS <- CODWS[2:3] # Collect only date & discharge

# COMBINE data as Discharge1
Discharge1 <- full_join(CODWS, USGS1, by=c("discharge_cfs","Date"))
Discharge1 <- Discharge1[!duplicated(Discharge1[c('Date')]),] # Delete duplicates due to overlap of data

# MONTHLY
monthly_discharge1 <- Discharge1 %>%
  # Add a new column of months to the data frame
  mutate(month = month(Date)) %>%
  # Group the data by month b/c we want to calculate monthly averages
  group_by(month) %>%
  # Calculate the mean discharge value for each month group.
  summarize(av_dischm1 = mean(discharge_cfs))

# ANNUAL    
annual_discharge1 <- Discharge1 %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarize(av_dischyr1 = mean(discharge_cfs))

# MONTHLY PLOT
CLAFTCCOgraphMon <- ggplot(data = monthly_discharge1, aes(x = month, y = av_dischm1)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1, 12, 1), lim = c(1, 12)) +
  labs(title = "Canyon Gage Monthly Discharge", y="Average discharge (cfs)", x = "Month")

# print(CLAFTCCOgraphMon)

# ANNUAL PLOT
CLAFTCCOgraphAnn <- ggplot(data = annual_discharge1, aes(x = year, y = av_dischyr1)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1980, 2020, 10), lim = c(1980, 2020)) +
  labs(title = "Canyon Gage Annual Discharge", y ="Average discharge (cfs)", x = "Year")

# print(CLAFTCCOgraphAnn)



#CACHE LA POUDRE RIVER AT FORT COLLINS, CO (CLAFORCO)
siteNumber2 <- "06752260" 
CLAFORCO <- readNWISsite(siteNumber2)
parameterCd <- "00060" #ID for discharge info
USGS2 <- readNWISdv(siteNumber2,parameterCd,
                           "1980-01-01","2020-06-08")
USGS2 <- na.omit(USGS2) # Omit NA
USGS2 <- USGS2 %>% 
  rename(discharge_cfs = X_00060_00003)
Discharge2 <- USGS2[3:4] # Collect only date & discharge

# MONTHLY
monthly_discharge2 <- Discharge2 %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  summarize(av_dischm2 = mean(discharge_cfs))

# ANNUAL    
annual_discharge2 <- Discharge2 %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarize(av_dischyr2 = mean(discharge_cfs))

# MONTHLY PLOT
CLAFORCOgraphMon <- ggplot(data = monthly_discharge2, aes(x = month, y = av_dischm2)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1, 12, 1), lim = c(1, 12)) +
  labs(title = "Fort Collins Monthly Discharge", y ="Average discharge (cfs)", x = "Month")

# print(CLAFORCOgraphMon)

# ANNUAL PLOT
CLAFORCOgraphAnn <- ggplot(data = annual_discharge2, aes(x = year, y = av_dischyr2)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1980, 2020, 10), lim = c(1980, 2020)) +
  labs(title = "Fort Collins Annual Discharge", y ="Average discharge (cfs)", x = "Year")

# print(CLAFORCOgraphAnn) 

# FEIS CHART into CFS -----------------------------------------------------------------
FEISValues <- read_excel("Desktop/FEISValues.xlsx")
FEISValues$Month <- c(11,12,01,02,03,04,05,06,07,08,09,10) # Make months into values
FEISValues <- FEISValues[,-2] # Remove the second column (# of diversions)
FEISValues$DaysinMonth <- c(30,31,31,28,31,30,31,30,31,31,30,31) # Based on the Gregorian calendar

#AF/day to CF/s conv = (43560/86400) cfs
CFSconv <- (43560/86400)

FEISValues <- FEISValues %>% 
mutate(FEISValues,
       Min_dayratecfs = (Min/DaysinMonth)*CFSconv,
       Max_dayratecfs = (Max/DaysinMonth)*CFSconv,
       Avg_dayratecfs = (Avg/DaysinMonth)*CFSconv)
FEISValuesUSE <- FEISValues[,-(2:5)]

# To convert df into csv:
# write.csv(FEISValuesUSE,"Desktop/FEISUSE.csv", row.names = FALSE)

# CHANGE IN FLOWS ---------------------------------------------------------------

## CLAFTCOO
# MONTHLY change
  C_Mon_Discharge1 <- Discharge1 %>% # Create new df to display impacts
  mutate(month = month(Date)) %>%
  group_by(month)
# Subtract values by month
C_Mon_Discharge1 <- C_Mon_Discharge1 %>% 
  mutate(avgdischarge_applied = case_when(month == 4 ~ (discharge_cfs-35.29167),
                                          month == 5 ~ (discharge_cfs-174.01882),
                                          month == 6 ~ (discharge_cfs-294.09722),
                                          month == 7 ~ (discharge_cfs-84.56989),
                                          month == 8 ~ (discharge_cfs-27.64785),
                                          TRUE ~ as.numeric(discharge_cfs)))
# Calculate mean discharges based on FEIS impacts
  C_monthly_discharge1 <- C_Mon_Discharge1 %>% 
  summarize(C_av_avg_dischm1 = mean(avgdischarge_applied))

# Repeat with ANNUAL change
  C_Ann_Discharge1 <- C_Mon_Discharge1 %>% 
    mutate(year = year(Date)) %>%
    group_by(year)
  C_annual_discharge1 <- C_Ann_Discharge1 %>% 
    summarize(C_av_avg_dischyr1 = mean(avgdischarge_applied))

# PLOT average impacts
# MONTHLY PLOT
C_CLAFTCCOgraphMon <- ggplot(data = monthly_discharge1, aes(x = month, y = av_dischm1)) + geom_line(color = "salmon2") + geom_point(color = "salmon2") +
  geom_line(data = C_monthly_discharge1, aes(x = month, y = C_av_avg_dischm1), color = "gray59") +
  geom_point(data = C_monthly_discharge1, aes(x = month, y = C_av_avg_dischm1), color = "gray59", shape = 15) + 
  scale_x_continuous(breaks = seq(1, 12, 1), lim = c(1, 12)) +
  scale_y_continuous(breaks = seq(0, 1700, 500), lim = c(0, 1700)) +
    labs(title = "Canyon Gage Monthly Discharge", y="Average discharge (cfs)", x = "Month")

print(C_CLAFTCCOgraphMon)

# ANNUAL PLOT
C_CLAFTCCOgraphAnn <- ggplot(data = annual_discharge1, aes(x = year, y = av_dischyr1)) + geom_line(color = "salmon2") + geom_point(color = "salmon2") +
  geom_line(data = C_annual_discharge1, aes(x = year, y = C_av_avg_dischyr1), color = "gray59") +
  geom_point(data = C_annual_discharge1, aes(x = year, y = C_av_avg_dischyr1), color = "gray59", shape = 15) +
  scale_x_continuous(breaks = seq(1980, 2020, 10), lim = c(1980, 2020)) +
  scale_y_continuous(breaks = seq(0, 1000, 250), lim = c(0, 1000)) +
  labs(title = "Canyon Gage Annual Discharge", y="Average discharge (cfs)", x = "Year")

print(C_CLAFTCCOgraphAnn)


# MONTHLY % changes (historical vs. post-NISP)
CLAFTCCO_monchange <- left_join(monthly_discharge1, C_monthly_discharge1)
CLAFTCCO_monchange <- CLAFTCCO_monchange %>% 
  mutate(prtchange = (C_av_avg_dischm1 - av_dischm1)/av_dischm1 * 100)



## CLAFORCO
# MONTHLY change
C_Mon_Discharge2 <- Discharge2 %>%
  mutate(month = month(Date)) %>%
  group_by(month)
# Subtract values by month
C_Mon_Discharge2 <- C_Mon_Discharge2 %>% 
  mutate(avgdischarge_applied = case_when(month == 4 ~ (discharge_cfs-35.29167),
                                          month == 5 ~ (discharge_cfs-174.01882),
                                          month == 6 ~ (discharge_cfs-294.09722),
                                          month == 7 ~ (discharge_cfs-84.56989),
                                          month == 8 ~ (discharge_cfs-27.64785),
                                          TRUE ~ as.numeric(discharge_cfs)))
# Calculate mean discharges based on FEIS impacts
C_monthly_discharge2 <- C_Mon_Discharge2 %>% 
  summarize(C_av_avg_dischm2 = mean(avgdischarge_applied))

# Repeat with ANNUAL change
C_Ann_Discharge2 <- C_Mon_Discharge2 %>% 
  mutate(year = year(Date)) %>%
  group_by(year)
C_annual_discharge2 <- C_Ann_Discharge2 %>% 
  summarize(C_av_avg_dischyr2 = mean(avgdischarge_applied))

# PLOT average impacts
# MONTHLY PLOT
C_CLAFORCOgraphMon <- ggplot(data = monthly_discharge2, aes(x = month, y = av_dischm2)) + geom_line(color = "skyblue2") + geom_point(color = "skyblue2") +
  geom_line(data = C_monthly_discharge2, aes(x = month, y = C_av_avg_dischm2), color = "slategray") +
  geom_point(data = C_monthly_discharge2, aes(x = month, y = C_av_avg_dischm2), color = "slategray", shape = 15) + 
  scale_x_continuous(breaks = seq(1, 12, 1), lim = c(1, 12)) +
  scale_y_continuous(breaks = seq(0, 1700, 500), lim = c(0, 1700)) +
  labs(title = "Fort Collins Gage Monthly Discharge", y="Average discharge (cfs)", x = "Month")

print(C_CLAFORCOgraphMon)

# ANNUAL PLOT
C_CLAFORCOgraphAnn <- ggplot(data = annual_discharge2, aes(x = year, y = av_dischyr2)) + geom_line(color = "skyblue2") + geom_point(color = "skyblue2") +
  geom_line(data = C_annual_discharge2, aes(x = year, y = C_av_avg_dischyr2), color = "slategray") +
  geom_point(data = C_annual_discharge2, aes(x = year, y = C_av_avg_dischyr2), color = "slategray", shape = 15) + 
  scale_x_continuous(breaks = seq(1980, 2020, 10), lim = c(1980, 2020)) +
  scale_y_continuous(breaks = seq(0, 1000, 250), lim = c(0, 1000)) +
  labs(title = "Fort Collins Gage Annual Discharge", y="Average discharge (cfs)", x = "Year")

print(C_CLAFORCOgraphAnn)


# MONTHLY % changes (historical vs. post-NISP)
CLAFORCO_monchange <- left_join(monthly_discharge2, C_monthly_discharge2)
CLAFORCO_monchange <- CLAFORCO_monchange %>% 
  mutate(prtchange = ((C_av_avg_dischm2 - av_dischm2)/av_dischm2) * 100)

# BOATABLE DAYS --------------------------------------------------
# BASED ON SURVEY RESULTS:
## COUNT HOW MANY DAYS > 529.166670 cfs for CLAFTCCO
##                     > 365.217391 cfs for CLAFORCO

# CLAFTCCO
# Post-NISP average diversions
CountAVG1 <- C_Mon_Discharge1[which(C_Mon_Discharge1$avgdischarge_applied >= 529.166670),]
CountAVG1 <- CountAVG1 %>%
  mutate(year = year(Date),
         logic = 1) %>%
  group_by(year) %>% 
  summarize(BoatableDaysAvgImpact = sum(logic))

# Business as usual (BAU = historical)
CountBAU1 <- Discharge1[which(Discharge1$discharge_cfs >= 529.166670),] 
CountBAU1 <- CountBAU1 %>%
  mutate(year = year(Date),
         logic = 1) %>%
  group_by(year) %>% 
  summarize(BoatableDaysNoImpact = sum(logic))


# PLOT density of boatable days
Count_Density1 <- ggplot (data = CountAVG1, aes(BoatableDaysAvgImpact)) + geom_density(color = "gray59") +
  geom_density(data = CountBAU1, aes(BoatableDaysNoImpact), color = "salmon2") +
  scale_x_continuous(breaks = seq(0, 120, 20), lim = c(0, 120)) +
  labs(y = "Density", x = "Boatable Days")

print(Count_Density1)

# Individual bar graphs
# POST-NISP PLOT
Count_barAVG1 <- ggplot (data = CountAVG1, aes(x = year, y = BoatableDaysAvgImpact)) + 
  geom_bar(stat = "identity", fill = "gray59") +
  scale_y_continuous(breaks = seq(0, 140, 20), lim = c(0, 140)) +
  labs(y = "Boatable Days", x = "Year")

print(Count_barAVG1)

# HISTORICAL PLOT
Count_barBAU1 <- ggplot (data = CountBAU1, aes(x = year, y = BoatableDaysNoImpact)) + 
  geom_bar(stat = "identity", fill = "salmon2") +
  scale_y_continuous(breaks = seq(0, 140, 20), lim = c(0, 140)) +
  labs(y = "Boatable Days", x = "Year")

print(Count_barBAU1)

# STACKED BAR PLOT
  # Make compiled df
    Count_CLAFTCCO <- left_join(CountBAU1, CountAVG1, by = "year")
      Count_CLAFTCCO[is.na(Count_CLAFTCCO)] <- 0 # Make N/A values = 0
    Count_CLAFTCCO <- Count_CLAFTCCO %>% 
      mutate(a = abs(BoatableDaysAvgImpact-BoatableDaysNoImpact)) %>% # Find difference of boatable days
      select(year, BoatableDaysAvgImpact, a)
    
    dfCount_CLAFTCCO <- Count_CLAFTCCO %>% 
      gather(Impact, BoatableDays, 2:3)
  # PLOT
    Bar_CLAFTCCO <- ggplot(data = dfCount_CLAFTCCO, aes(x = year, y = BoatableDays, fill = Impact)) +
      geom_bar(stat = "identity", position = "stack") + 
      scale_fill_manual(values=c("tomato2", "gray59"), 
                        name="",
                        breaks=c("a", "BoatableDaysAvgImpact"),
                        labels=c("Loss of days", "Post-NISP")) +
      scale_y_continuous(breaks = seq(0, 140, 20), lim = c(0, 140)) +
      theme(legend.position="bottom") +
      labs(y = "Boatable Days", x = "Year")
    
    print(Bar_CLAFTCCO)
    
# BOXPLOT
    Count_CLAFTCCO <- left_join(CountBAU1, CountAVG1, by = "year") 
    Count_CLAFTCCO[is.na(Count_CLAFTCCO)] <- 0 # Make N/A values = 0
    Count_CLAFTCCO <- Count_CLAFTCCO %>% 
      mutate(a = abs(BoatableDaysAvgImpact-BoatableDaysNoImpact)) %>% 
      select(year, a)
  # PLOT
    Box_CLAFTCCO <- ggplot(data = Count_CLAFTCCO, aes(x = year, y = a)) + geom_boxplot() +
      labs(y = "Boatable Days", x = "Year") +
      scale_y_continuous(breaks = seq(0, 30, 5), lim = c(0, 30)) +
      scale_x_continuous(breaks = seq(1980, 2020, 10), lim = c(1980, 2020))
    
    print(Box_CLAFTCCO)
  


## CLAFORCO
# Post-NISP average diversions
CountAVG2 <- C_Mon_Discharge2[which(C_Mon_Discharge2$avgdischarge_applied >= 365.217391),]
CountAVG2 <- CountAVG2 %>%
  mutate(year = year(Date),
         logic = 1) %>%
  group_by(year) %>% 
  summarize(BoatableDaysAvgImpact = sum(logic))

# Business as usual (BAU = historical)
CountBAU2 <- Discharge2[which(Discharge2$discharge_cfs >= 365.217391),]
CountBAU2 <- CountBAU2 %>%
  mutate(year = year(Date),
         logic = 1) %>%
  group_by(year) %>% 
  summarize(BoatableDaysNoImpact = sum(logic))

# PLOT density of boatable days
Count_Density2 <- ggplot (data = CountAVG2, aes(BoatableDaysAvgImpact)) + geom_density(color = "slategray") +
  geom_density(data = CountBAU2, aes(BoatableDaysNoImpact), color = "skyblue2") +
  scale_x_continuous(breaks = seq(0, 140, 20), lim = c(0, 140)) +
  labs(y = "Density", x = "Boatable Days")

print(Count_Density2)

# Individual bar graphs
# POST-NISP PLOT
Count_barAVG2 <- ggplot (data = CountAVG2, aes(x = year, y = BoatableDaysAvgImpact)) + 
  geom_bar(stat = "identity", fill = "slategray") +
  scale_y_continuous(breaks = seq(0, 140, 20), lim = c(0, 140)) +
  labs(y = "Boatable Days", x = "Year")

print(Count_barAVG2)

# HISTORICAL PLOT
Count_barBAU2 <- ggplot (data = CountBAU2, aes(x = year, y = BoatableDaysNoImpact)) + 
  geom_bar(stat = "identity", fill = "skyblue2") +
  scale_y_continuous(breaks = seq(0, 140, 20), lim = c(0, 140)) +
  labs(y = "Boatable Days", x = "Year")

print(Count_barBAU2)

# STQACKED BAR PLOT
  # Make compiled df
    Count_CLAFORCO <- left_join(CountBAU2, CountAVG2, by = "year")
      Count_CLAFORCO[is.na(Count_CLAFORCO)] <- 0 # Make N/A values = 0
    Count_CLAFORCO <- Count_CLAFORCO %>% 
      mutate(a = abs(BoatableDaysAvgImpact-BoatableDaysNoImpact)) %>% 
      select(year, a, BoatableDaysAvgImpact)

    dfCount_CLAFORCO <- Count_CLAFORCO %>% 
      gather(Impact, BoatableDays, 2:3)
  # PLOT
    Bar_CLAFORCO <- ggplot(data = dfCount_CLAFORCO, aes(x = year, y = BoatableDays, fill = Impact)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values=c("tomato2", "slategray"), 
                        name="",
                        breaks=c("a", "BoatableDaysAvgImpact"),
                        labels=c("Loss of days", "Post-NISP")) +
      scale_y_continuous(breaks = seq(0, 140, 20), lim = c(0, 140)) +
      theme(legend.position="bottom") +
      labs(y = "Boatable Days", x = "Year")
    
    print(Bar_CLAFORCO)
    

# BOXPLOT
    Count_CLAFORCO <- left_join(CountBAU2, CountAVG2, by = "year")
    Count_CLAFORCO[is.na(Count_CLAFORCO)] <- 0 # Make N/A values = 0
    Count_CLAFORCO <- Count_CLAFORCO %>% 
      mutate(a = abs(BoatableDaysAvgImpact-BoatableDaysNoImpact)) %>% 
      select(year, a)
  # PLOT
    Box_CLAFORCO <- ggplot(data = Count_CLAFORCO, aes(x = year, y = a)) + geom_boxplot() +
      labs(y = "Boatable Days", x = "Year") +
      scale_y_continuous(breaks = seq(0, 30, 5), lim = c(0, 30)) +
      scale_x_continuous(breaks = seq(1980, 2020, 10), lim = c(1980, 2020))
   
     print(Box_CLAFORCO)