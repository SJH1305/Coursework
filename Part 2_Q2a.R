# Part 2, Q2a, What are the best times and days of the week to minimise delays each year?
# Reading in relevant files:
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
library(ggplot2)
library(skimr)

# Understanding the data:
str(df_2004)
variabledescriptions

# Delays in each year
## Creating new variable - TotalDelay:
## Carrierdelay + Weatherdelay + Nasdelay + Securitydelay + Lateaircraftdelay

## using mutate while considering NaN values (set = 0) 
df_2004 <- df_2004 %>% 
  mutate(df_2004, Delay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
    ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
    ifelse(is.na(NASDelay), 0 , NASDelay) + 
    ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
    ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay)) 
                  
df_2005 <- df_2005 %>% 
  mutate(df_2005, Delay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
    ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
    ifelse(is.na(NASDelay), 0 , NASDelay) + 
    ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
    ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay)) 
    
df_2006 <- df_2006 %>% 
  mutate(df_2006, Delay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
    ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
    ifelse(is.na(NASDelay), 0 , NASDelay) + 
    ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
    ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay)) 
    
df_2007 <- df_2007 %>% 
  mutate(df_2007, Delay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
    ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
    ifelse(is.na(NASDelay), 0 , NASDelay) + 
    ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
    ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay)) 
    
df_2008 <- df_2008 %>% 
  mutate(df_2008, Delay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
    ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
    ifelse(is.na(NASDelay), 0 , NASDelay) + 
    ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
    ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay)) 

## Average delay for each year
avgdelay_years <- c(
  "2004" = mean(df_2004$Delay, na.rm = FALSE),
  "2005" = mean(df_2005$Delay, na.rm = FALSE),
  "2006" = mean(df_2006$Delay, na.rm = FALSE),
  "2007" = mean(df_2007$Delay, na.rm = FALSE),
  "2008" = mean(df_2008$Delay, na.rm = FALSE)
)

print (avgdelay_years)
barplot1 <- barplot (avgdelay_years, main = "Average delay per year", ylab = "Average delay")
ggsave("Barplot - Yearly avg_delay.png")
    
# Best times to minimise delay
## Count of number of uniquely scheduled flight times
length(unique(df_2004$CRSDepTime)) 
length(unique(df_2005$CRSDepTime))
length(unique(df_2006$CRSDepTime))
length(unique(df_2007$CRSDepTime))
length(unique(df_2008$CRSDepTime))

## Visualisation - 4 time segments 
# 2004
df_2004 <- df_2004 %>% filter(!is.na(CRSDepTime))

df_2004 <- df_2004 %>% 
  mutate(Time_segment = case_when(
    CRSDepTime>=0 & CRSDepTime<600 ~ "12AM-6AM",
    CRSDepTime>=600 & CRSDepTime<1200 ~ "6AM-12PM",
    CRSDepTime>=1200 & CRSDepTime<1800 ~ "12PM-6PM",
    CRSDepTime>=1800 & CRSDepTime<2359 ~ "6PM-12AM"
  ))

ggplot(df_2004, aes(x = Time_segment, y = Delay)) +
  geom_boxplot(fill = "lightblue")+
  geom_jitter(alpha = 0.2, color = "orange") +
  labs(title = "Delays throughout the day - 2004",
       x = "Segments of Day",
       y = "Delay (min)")
ggsave("Boxplot(times)_2004.png")

#2005
df_2005 <- df_2005 %>% filter(!is.na(CRSDepTime))

df_2005 <- df_2005 %>% 
  mutate(Time_segment = case_when(
    CRSDepTime>=0 & CRSDepTime<600 ~ "12AM-6AM",
    CRSDepTime>=600 & CRSDepTime<1200 ~ "6AM-12PM",
    CRSDepTime>=1200 & CRSDepTime<1800 ~ "12PM-6PM",
    CRSDepTime>=1800 & CRSDepTime<2359 ~ "6PM-12AM"
  ))

ggplot(df_2005, aes(x = Time_segment, y = Delay)) +
  geom_boxplot(fill = "lightblue")+
  geom_jitter(alpha = 0.2, color = "orange") +
  labs(title = "Delays throughout the day - 2005",
       x = "Segments of Day",
       y = "Delay (min)")
ggsave("Boxplot(times)_2005.png")

#2006
df_2006 <- df_2006 %>% filter(!is.na(CRSDepTime))

df_2006 <- df_2006 %>% 
  mutate(Time_segment = case_when(
    CRSDepTime>=0 & CRSDepTime<600 ~ "12AM-6AM",
    CRSDepTime>=600 & CRSDepTime<1200 ~ "6AM-12PM",
    CRSDepTime>=1200 & CRSDepTime<1800 ~ "12PM-6PM",
    CRSDepTime>=1800 & CRSDepTime<2359 ~ "6PM-12AM"
  ))

ggplot(df_2006, aes(x = Time_segment, y = Delay)) +
  geom_boxplot(fill = "lightblue")+
  geom_jitter(alpha = 0.2, color = "orange") +
  labs(title = "Delays throughout the day - 2006",
       x = "Segments of Day",
       y = "Delay (min)")
ggsave("Boxplot(times)_2006.png")

#2007
df_2007 <- df_2007 %>% filter(!is.na(CRSDepTime))

df_2007 <- df_2007 %>% 
  mutate(Time_segment = case_when(
    CRSDepTime>=0 & CRSDepTime<600 ~ "12AM-6AM",
    CRSDepTime>=600 & CRSDepTime<1200 ~ "6AM-12PM",
    CRSDepTime>=1200 & CRSDepTime<1800 ~ "12PM-6PM",
    CRSDepTime>=1800 & CRSDepTime<2359 ~ "6PM-12AM"
  ))

ggplot(df_2007, aes(x = Time_segment, y = Delay)) +
  geom_boxplot(fill = "lightblue")+
  geom_jitter(alpha = 0.2, color = "orange") +
  labs(title = "Delays throughout the day - 2007",
       x = "Segments of Day",
       y = "Delay (min)")
ggsave("Boxplot(times)_2007.png")

#2008
df_2008 <- df_2008 %>% filter(!is.na(CRSDepTime))

df_2008 <- df_2008 %>% 
  mutate(Time_segment = case_when(
    CRSDepTime>=0 & CRSDepTime<600 ~ "12AM-6AM",
    CRSDepTime>=600 & CRSDepTime<1200 ~ "6AM-12PM",
    CRSDepTime>=1200 & CRSDepTime<1800 ~ "12PM-6PM",
    CRSDepTime>=1800 & CRSDepTime<2359 ~ "6PM-12AM"
  ))

ggplot(df_2008, aes(x = Time_segment, y = Delay)) +
  geom_boxplot(fill = "lightblue")+
  geom_jitter(alpha = 0.2, color = "orange") +
  labs(title = "Delays throughout the day - 2008",
       x = "Segments of Day",
       y = "Delay (min)")
ggsave("Boxplot(times)_2008.png")

## Average delay in terms of scheduled departures 
avg_timedelay_2004 <- df_2004 %>% 
  group_by(CRSDepTime) %>% 
  summarise(avg_timedelay_04 = mean(Delay, na.rm = TRUE))
avg_timedelay_2004 <- avg_timedelay_2004 %>% 
  rename(AvgDelay = avg_timedelay_04) %>% mutate(Year = 2004)

avg_timedelay_2005 <- df_2005 %>% 
  group_by(CRSDepTime) %>% 
  summarise(avg_timedelay_05 = mean(Delay, na.rm = TRUE))
avg_timedelay_2006 <- df_2006 %>% 
  group_by(CRSDepTime) %>% 
  summarise(avg_timedelay_06 = mean(Delay, na.rm = TRUE))
avg_timedelay_2007 <- df_2007 %>% 
  group_by(CRSDepTime) %>% 
  summarise(avg_timedelay_07 = mean(Delay, na.rm = TRUE))
avg_timedelay_2008 <- df_2008 %>% 
  group_by(CRSDepTime) %>% 
  summarise(avg_timedelay_08 = mean(Delay, na.rm = TRUE))

avg_timedelay_2005 <- avg_timedelay_2005 %>% 
  rename(AvgDelay = avg_timedelay_05) %>% mutate(Year = 2005)
avg_timedelay_2006 <- avg_timedelay_2006 %>% 
  rename(AvgDelay = avg_timedelay_06) %>% mutate(Year = 2006)
avg_timedelay_2007 <- avg_timedelay_2007 %>% 
  rename(AvgDelay = avg_timedelay_07) %>% mutate(Year = 2007)
avg_timedelay_2008 <- avg_timedelay_2008 %>% 
  rename(AvgDelay = avg_timedelay_08) %>% mutate(Year = 2008)

avg_timedelay_all <- bind_rows(avg_timedelay_2004,avg_timedelay_2005,
                               avg_timedelay_2006,avg_timedelay_2007,avg_timedelay_2008) 
print(avg_timedelay_all)

# Histogram 

library(ggplot2)
ggplot(avg_timedelay_2004, aes(x = AvgDelay)) +
  geom_histogram(fill = "darkcyan")
ggsave("Histogram(Avgdelay_CRSDep04)_04.png")

ggplot(avg_timedelay_2005, aes(x = AvgDelay)) +
  geom_histogram(fill = "cadetblue")
ggsave("Histogram(Avgdelay_CRSDep05)_05.png")

ggplot(avg_timedelay_2006, aes(x = AvgDelay)) +
  geom_histogram(fill = "darkslategrey")
ggsave("Histogram(Avgdelay_CRSDep06)_06.png")

ggplot(avg_timedelay_2007, aes(x = AvgDelay)) +
  geom_histogram(fill = "steelblue")
ggsave("Histogram(Avgdelay_CRSDep07)_07.png")

ggplot(avg_timedelay_2008, aes(x = AvgDelay)) +
  geom_histogram(fill = "slateblue")
ggsave("Histogram(Avgdelay_CRSDep08)_08.png")

# -----------------------------------------------------
# Best days to minimise delay 

## Average delay in terms of days of the week 

avg_dailydelay_2004 <- df_2004 %>% 
  group_by(DayOfWeek) %>% 
  summarise(avg_dailydelay_04 = mean(Delay, na.rm = TRUE))
avg_dailydelay_2005 <- df_2005 %>% 
  group_by(DayOfWeek) %>% 
  summarise(avg_dailydelay_05 = mean(Delay, na.rm = TRUE))
avg_dailydelay_2006 <- df_2006 %>% 
  group_by(DayOfWeek) %>% 
  summarise(avg_dailydelay_06 = mean(Delay, na.rm = TRUE))
avg_dailydelay_2007 <- df_2007 %>% 
  group_by(DayOfWeek) %>% 
  summarise(avg_dailydelay_07 = mean(Delay, na.rm = TRUE))
avg_dailydelay_2008 <- df_2008 %>% 
  group_by(DayOfWeek) %>% 
  summarise(avg_dailydelay_08 = mean(Delay, na.rm = TRUE))

avgdailydelay_all <- list(avg_dailydelay_2004, avg_dailydelay_2005,avg_dailydelay_2006,
                          avg_dailydelay_2007,avg_dailydelay_2008)
print(avgdailydelay_all)

## barplot 
ggplot(avg_dailydelay_2004) + 
  geom_bar(aes(x = factor(DayOfWeek), y = avg_dailydelay_04, fill = factor(DayOfWeek)), stat = "identity") +
labs(x = "Days (1 = Mon, 7 = Sun)",
     y = "Average Delay (min)")
ggsave("Bar plot - avg_delay,days.png")
