# What are the best times and days of the week to minimise delays each year?
# Reading in files:
df_2004 <- read.csv("2004.csv.bz2")
df_2005 <- read.csv("2005.csv.bz2")
df_2006 <- read.csv("2006.csv.bz2")
df_2007 <- read.csv("2007.csv.bz2")
df_2008 <- read.csv("2008.csv.bz2")
airports <- read.csv("airports.csv")
carriers <- read.csv("carriers.csv")
planedata <- read.csv("plane-data.csv")
variabledescriptions <- read.csv("variable-descriptions.csv")

library(tidyverse)
library(dplyr)

# Understanding the data:
str(df_2004)
variabledescriptions

# Creating new variable - TotalDelay:
#Carrierdelay + Weatherdelay + Nasdelay + Securitydelay + Lateaircraftdelay

# using mutate while considering NaN values (set = 0) 
df_2004 <- df_2004 %>% 
  mutate(df_2004, TotalDelay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
    ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
    ifelse(is.na(NASDelay), 0 , NASDelay) + 
    ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
    ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay)) 
                  
df_2005 <- df_2005 %>% 
  mutate(df_2005, TotalDelay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
    ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
    ifelse(is.na(NASDelay), 0 , NASDelay) + 
    ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
    ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay)) 
    
df_2006 <- df_2006 %>% 
  mutate(df_2006, TotalDelay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
    ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
    ifelse(is.na(NASDelay), 0 , NASDelay) + 
    ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
    ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay)) 
    
df_2007 <- df_2007 %>% 
  mutate(df_2007, TotalDelay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
    ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
    ifelse(is.na(NASDelay), 0 , NASDelay) + 
    ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
    ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay)) 
    
df_2008 <- df_2008 %>% 
  mutate(df_2008, TotalDelay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
    ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
    ifelse(is.na(NASDelay), 0 , NASDelay) + 
    ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
    ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay)) 
    
# Best times to minimise delay
# Unique CRSDepTime count
length(unique(df_2004$CRSDepTime)) # Note $
length(unique(df_2005$CRSDepTime))
length(unique(df_2006$CRSDepTime))
length(unique(df_2007$CRSDepTime))
length(unique(df_2008$CRSDepTime))

# Average delay in terms of CRSDepTime # doesnt make sense?
avg_timedelay_2004 <- df_2004 %>% 
  group_by(CRSDepTime) %>% 
  summarise(avg_timedelay_04 = mean(TotalDelay, na.rm = TRUE))
avg_timedelay_2005 <- df_2005 %>% 
  group_by(CRSDepTime) %>% 
  summarise(avg_timedelay_05 = mean(TotalDelay, na.rm = TRUE))
avg_timedelay_2006 <- df_2006 %>% 
  group_by(CRSDepTime) %>% 
  summarise(avg_timedelay_06 = mean(TotalDelay, na.rm = TRUE))
avg_timedelay_2007 <- df_2007 %>% 
  group_by(CRSDepTime) %>% 
  summarise(avg_timedelay_07 = mean(TotalDelay, na.rm = TRUE))
avg_timedelay_2008 <- df_2008 %>% 
  group_by(CRSDepTime) %>% 
  summarise(avg_timedelay_08 = mean(TotalDelay, na.rm = TRUE))

# Average delays in each year
avg_yeardelay_2004 <-mean(df_2004$TotalDelay, na.rm = TRUE)
avg_yeardelay_2005 <-mean(df_2005$TotalDelay, na.rm = TRUE)
avg_yeardelay_2006 <-mean(df_2006$TotalDelay, na.rm = TRUE)
avg_yeardelay_2007 <-mean(df_2007$TotalDelay, na.rm = TRUE)
avg_yeardelay_2008 <-mean(df_2008$TotalDelay, na.rm = TRUE)
avg_delay_yearly <- list(avg_yeardelay_2004,
                      avg_yeardelay_2005,
                      avg_yeardelay_2006,
                      avg_yeardelay_2007,
                      avg_yeardelay_2008)
print(avg_delay_yearly)

# How are flights scheduled in a week
# visualisation 
plot(df_2004$DayOfMonth,df_2004$CRSDepTime, type = 'l')

# ---------------

# Best days to minimise delay 
# visualisation 
library(ggplot2)

ggplot(df_2004, aes(x = DayOfWeek, y = TotalDelay))+
geom_boxplot(
  outlier.colour = "red",
  outlier.size = 1.5,
  notch = FALSE,
) +
labs(title = "Best days to minimise delay",
     x = "Days of the Week (Mon = 1, Sun = 7",
     y = "Total Delay (min)")

# Average delay in terms of DayOfWeek 
# visualisation 
avg_dailyelay_2004 <- df_2004 %>% 
  group_by(DayOfWeek) %>% 
  summarise(avg_dailydelay_04 = mean(TotalDelay, na.rm = TRUE))
avg_dailyelay_2005 <- df_2005 %>% 
  group_by(DayOfWeek) %>% 
  summarise(avg_dailydelay_05 = mean(TotalDelay, na.rm = TRUE))
avg_dailyelay_2006 <- df_2006 %>% 
  group_by(DayOfWeek) %>% 
  summarise(avg_dailydelay_06 = mean(TotalDelay, na.rm = TRUE))
avg_dailyelay_2007 <- df_2007 %>% 
  group_by(DayOfWeek) %>% 
  summarise(avg_dailydelay_07 = mean(TotalDelay, na.rm = TRUE))
avg_dailyelay_2008 <- df_2008 %>% 
  group_by(DayOfWeek) %>% 
  summarise(avg_dailydelay_08 = mean(TotalDelay, na.rm = TRUE))

ggplot(avg_dailyelay_2004) + geom_bar(aes(x = cut, colour = cut, fill = cut))

