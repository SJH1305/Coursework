# Q2b - Evaluate whether older planes suffer more delays on a year-to-year basis.
library(tidyverse)
library(dplyr)
library(ggplot2)
library(skimr)

# Understanding the data:
str(planedata)
str(df_2004)

# planes in plane data appear more than once in each df
df_2004 %>% 
  count(TailNum) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1)

# Identifying and cross referencing flights in 'planedata' with each df
## This is because planes data contains all known planes, we only want
# relevant planes for each year
## using filter
actuallyflew_2004 <- df_2004 %>% filter(TailNum %in% planedata$tailnum) %>% 
  left_join(planedata, by = c("TailNum" = "tailnum"))
actuallyflew_2005 <- df_2005 %>% filter(TailNum %in% planedata$tailnum) %>% 
  left_join(planedata, by = c("TailNum" = "tailnum"))
actuallyflew_2006 <- df_2006 %>% filter(TailNum %in% planedata$tailnum) %>% 
  left_join(planedata, by = c("TailNum" = "tailnum"))
actuallyflew_2007 <- df_2007 %>% filter(TailNum %in% planedata$tailnum) %>% 
  left_join(planedata, by = c("TailNum" = "tailnum"))
actuallyflew_2008 <- df_2008 %>% filter(TailNum %in% planedata$tailnum) %>% 
  left_join(planedata, by = c("TailNum" = "tailnum"))

## Creating age variable 
actuallyflew_2004 <- actuallyflew_2004 %>% 
  filter(!is.na(year) & year != 0) %>% 
  mutate(year = as.numeric(year), age = 2004 - year) 
actuallyflew_2005 <- actuallyflew_2005 %>% 
  filter(!is.na(year) & year != 0) %>% 
  mutate(year = as.numeric(year),age = 2005 - year)
actuallyflew_2006 <- actuallyflew_2006 %>% 
  filter(!is.na(year) & year != 0) %>% 
  mutate(year = as.numeric(year),age = 2006 - year)
actuallyflew_2007 <- actuallyflew_2007 %>% 
  filter(!is.na(year) & year != 0) %>% 
  mutate(year = as.numeric(year),age = 2007 - year)
actuallyflew_2008 <- actuallyflew_2008 %>% 
  filter(!is.na(year) & year != 0) %>% 
  mutate(year = as.numeric(year),age = 2008 - year)

# 'Delay' is already added

#Exclude 0 and 00000 flight numbers
remove_2004 <- df_2004 %>% 
  filter(!TailNum %in% c("0","00000")) 

remove_2005 <- df_2005 %>% 
  filter(!TailNum %in% c("0","00000"))

remove_2006 <- df_2006 %>% 
  filter(!TailNum %in% c("0","00000"))
                  
remove_2007 <- df_2007 %>% 
  filter(!TailNum %in% c("0","00000"))

remove_2008 <- df_2008 %>% 
  filter(!TailNum %in% c("0","00000"))

# getting avg delay of each individual plane based on their total no. of flights
idv_avgdelay_04 <- actuallyflew_2004 %>% 
  group_by(TailNum) %>% 
  summarise(Avg_idv_Delay = mean(Delay, na.rm=TRUE), .groups = "drop")
idv_avgdelay_05 <- actuallyflew_2005 %>% 
  group_by(TailNum) %>% 
  summarise(Avg_idv_Delay = mean(Delay, na.rm=TRUE), .groups = "drop")
idv_avgdelay_06 <- actuallyflew_2006 %>% 
  group_by(TailNum) %>% 
  summarise(Avg_idv_Delay = mean(Delay, na.rm=TRUE), .groups = "drop")
idv_avgdelay_07 <- actuallyflew_2007 %>% 
  group_by(TailNum) %>% 
  summarise(Avg_idv_Delay = mean(Delay, na.rm=TRUE), .groups = "drop")
idv_avgdelay_08 <- actuallyflew_2008 %>% 
  group_by(TailNum) %>% 
  summarise(Avg_idv_Delay = mean(Delay, na.rm=TRUE), .groups = "drop")

# adding this as a column in actuallyflew_200X
actuallyflew_2004 <- actuallyflew_2004 %>% 
  left_join(idv_avgdelay_04, by = "TailNum")
actuallyflew_2005 <- actuallyflew_2005 %>% 
  left_join(idv_avgdelay_05, by = "TailNum")
actuallyflew_2006 <- actuallyflew_2006 %>% 
  left_join(idv_avgdelay_06, by = "TailNum")
actuallyflew_2007 <- actuallyflew_2007 %>% 
  left_join(idv_avgdelay_07, by = "TailNum")
actuallyflew_2008 <- actuallyflew_2008 %>% 
  left_join(idv_avgdelay_08, by = "TailNum")

# Distribution of delayed flights - Avg delay of individual planes & age 
ggplot(actuallyflew_2004,aes(x = Avg_idv_Delay)) +
  geom_histogram(aes(y = ..density..))
labs(title = "Distribution of individual avg.delays and Age(2004)",
     x = "Average delay per plane(min)",
     y = "F.Density")
ggsave("Histogram - Individual avg.delay (2004).png")

ggplot(actuallyflew_2005,aes(x = Avg_idv_Delay)) +
  geom_histogram(aes(y = ..density..))
labs(title = "Distribution of individual avg.delays and Age(2005)",
     x = "Average delay per plane(min)",
     y = "F.Density")
ggsave("Histogram - Individual avg.delay (2005).png")

ggplot(actuallyflew_2006,aes(x = Avg_idv_Delay)) +
  geom_histogram(aes(y = ..density..))
labs(title = "Distribution of individual avg.delays and Age(2006)",
     x = "Average delay per plane(min)",
     y = "F.Density")
ggsave("Histogram - Individual avg.delay (2006).png")

ggplot(actuallyflew_2007,aes(x = Avg_idv_Delay)) +
  geom_histogram(aes(y = ..density..))
labs(title = "Distribution of individual avg.delays and Age(2007)",
     x = "Average delay per plane(min)",
     y = "F.Density")
ggsave("Histogram - Individual avg.delay (2007).png")

ggplot(actuallyflew_2008,aes(x = Avg_idv_Delay)) +
  geom_histogram(aes(y = ..density..))
labs(title = "Distribution of individual avg.delays and Age(2008)",
     x = "Average delay per plane(min)",
     y = "F.Density")
ggsave("Histogram - Individual avg.delay (2008).png")

# Distribution of delayed flights - Avg delay & plane age 
avgdelay_age_2004 <- actuallyflew_2004 %>% 
  group_by(age) %>% 
  summarise(Delay = mean(Delay, na.rm = TRUE))

ggplot(avgdelay_age_2004, aes(x = age, y = Delay)) +
  geom_line(color = "steelblue") +
  labs(title = "Line plot of Plane age and Average Delays(2004)",
       x = "Plane age (years)",
       y = "Average Delay (min)")
ggsave("Lineplot - Plane age and Average Delays(2004).png")

avgdelay_age_2005 <- actuallyflew_2005 %>% 
  group_by(age) %>% 
  summarise(Delay = mean(Delay, na.rm = TRUE))

ggplot(avgdelay_age_2005, aes(x = age, y = Delay)) +
  geom_line(color = "steelblue") +
  labs(title = "Line plot of Plane age and Average Delays(2005)",
       x = "Plane age (years)",
       y = "Average Delay (min)")
ggsave("Lineplot - Plane age and Average Delays(2005).png")

avgdelay_age_2006 <- actuallyflew_2006 %>% 
  group_by(age) %>% 
  summarise(Delay = mean(Delay, na.rm = TRUE))

ggplot(avgdelay_age_2006, aes(x = age, y = Delay)) +
  geom_line(color = "steelblue") +
  labs(title = "Line plot of Plane age and Average Delays(2006)",
       x = "Plane age (years)",
       y = "Average Delay (min)")
ggsave("Lineplot - Plane age and Average Delays(2006).png")

avgdelay_age_2007 <- actuallyflew_2007 %>% 
  group_by(age) %>% 
  summarise(Delay = mean(Delay, na.rm = TRUE))

ggplot(avgdelay_age_2007, aes(x = age, y = Delay)) +
  geom_line(color = "steelblue") +
  labs(title = "Line plot of Plane age and Average Delays(2007)",
       x = "Plane age (years)",
       y = "Average Delay (min)")
ggsave("Lineplot - Plane age and Average Delays(2007).png")

avgdelay_age_2008 <- actuallyflew_2008 %>% 
  group_by(age) %>% 
  summarise(Delay = mean(Delay, na.rm = TRUE))

ggplot(avgdelay_age_2008, aes(x = age, y = Delay)) +
  geom_line(color = "steelblue") +
  labs(title = "Line plot of Plane age and Average Delays(2008)",
       x = "Plane age (years)",
       y = "Average Delay (min)")
ggsave("Lineplot - Plane age and Average Delays(2008).png")

# Distribution of non-delay flights - Histogram 
# Adding binary column - on_time (Yes = 0, No = 1), Delay = 0
actuallyflew_2004 <- actuallyflew_2004 %>% 
  mutate(on_time = ifelse(Delay == 0,1,0))
ontime_2004 <- actuallyflew_2004 %>% 
  select(TailNum,Delay, on_time) %>% 
  filter(on_time == 1)

actuallyflew_2005 <- actuallyflew_2005 %>% 
  mutate(on_time = ifelse(Delay == 0,1,0))
ontime_2005 <- actuallyflew_2005 %>% 
  select(TailNum,Delay, on_time) %>% 
  filter(on_time == 1)

actuallyflew_2006 <- actuallyflew_2006 %>% 
  mutate(on_time = ifelse(Delay == 0,1,0))
ontime_2006 <- actuallyflew_2006 %>% 
  select(TailNum,Delay, on_time) %>% 
  filter(on_time == 1)

actuallyflew_2007 <- actuallyflew_2007 %>% 
  mutate(on_time = ifelse(Delay == 0,1,0))
ontime_2007 <- actuallyflew_2007 %>% 
  select(TailNum,Delay, on_time) %>% 
  filter(on_time == 1)

actuallyflew_2008 <- actuallyflew_2008 %>% 
  mutate(on_time = ifelse(Delay == 0,1,0))
ontime_2008 <- actuallyflew_2008 %>% 
  select(TailNum,Delay, on_time) %>% 
  filter(on_time == 1)

ggplot(ontime_2004, aes(x = age)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "steelblue") +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2004)",
       x = "F.Density",
       y = "Age")
ggsave("Histogram - Non delayed flights,2004.png")

ggplot(ontime_2005, aes(x = age)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "steelblue") +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2005)",
       x = "F.Density",
       y = "Age")
ggsave("Histogram - Non delayed flights,2005.png")

ggplot(ontime_2006, aes(x = age)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "steelblue") +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2006)",
       x = "F.Density",
       y = "Age")
ggsave("Histogram - Non delayed flights,2006.png")

ggplot(ontime_2007, aes(x = age)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "steelblue") +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2007)",
       x = "F.Density",
       y = "Age")
ggsave("Histogram - Non delayed flights,2007.png")

ggplot(ontime_2008, aes(x = age)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "steelblue") +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2008)",
       x = "F.Density",
       y = "Age")
ggsave("Histogram - Non delayed flights,2008.png")

# Distribution of Aircraft age
ggplot(actuallyflew_2004, aes(x = age))+
  geom_histogram(aes(y = ..density..)) +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2004)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - age distribution per year (2004).png")

ggplot(actuallyflew_2005, aes(x = age))+
  geom_histogram(aes(y = ..density..)) +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2005)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - age distribution per year (2005).png")

ggplot(actuallyflew_2006, aes(x = age))+
  geom_histogram(aes(y = ..density..)) +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2006)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - age distribution per year (2006).png")

ggplot(actuallyflew_2007, aes(x = age))+
  geom_histogram(aes(y = ..density..)) +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2007)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - age distribution per year (2007).png")

ggplot(actuallyflew_2008, aes(x = age))+
  geom_histogram(aes(y = ..density..)) +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2008)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - age distribution per year (2008).png")

# Why 'issue date' was not chosen
negativeage_count04 <- sum(actuallyflew_2004$age <0, na.rm = TRUE)
negativeage_count05 <- sum(actuallyflew_2005$age <0, na.rm = TRUE)
negativeage_count06 <- sum(actuallyflew_2006$age <0, na.rm = TRUE)
negativeage_count07 <- sum(actuallyflew_2007$age <0, na.rm = TRUE)
negativeage_count08 <- sum(actuallyflew_2008$age <0, na.rm = TRUE)

actuallyflew_2004$issue_date <- as.Date(actuallyflew_2004$issue_date)
actuallyflew_2005$issue_date <- as.Date(actuallyflew_2005$issue_date)
actuallyflew_2006$issue_date <- as.Date(actuallyflew_2006$issue_date)
actuallyflew_2007$issue_date <- as.Date(actuallyflew_2007$issue_date)
actuallyflew_2008$issue_date <- as.Date(actuallyflew_2008$issue_date)

actuallyflew_2004$age_id <- 2004 - as.numeric(format(actuallyflew_2004$issue_date, "%Y"))
actuallyflew_2005$age_id <- 2005 - as.numeric(format(actuallyflew_2005$issue_date, "%Y"))
actuallyflew_2006$age_id <- 2006 - as.numeric(format(actuallyflew_2006$issue_date, "%Y"))
actuallyflew_2007$age_id <- 2007 - as.numeric(format(actuallyflew_2007$issue_date, "%Y"))
actuallyflew_2008$age_id <- 2008 - as.numeric(format(actuallyflew_2008$issue_date, "%Y"))

negativeage_count_id04 <- sum(actuallyflew_2004$age_id < 0, na.rm = TRUE)
negativeage_count_id05 <- sum(actuallyflew_2005$age_id < 0, na.rm = TRUE)
negativeage_count_id06 <- sum(actuallyflew_2006$age_id < 0, na.rm = TRUE)
negativeage_count_id07 <- sum(actuallyflew_2007$age_id < 0, na.rm = TRUE)
negativeage_count_id08 <- sum(actuallyflew_2008$age_id < 0, na.rm = TRUE)

print("NegativeCounts based on 'issue date':",
      negativeage_count_id04, ",",
      negativeage_count_id05, ",",
      negativeage_count_id06, ",",
      negativeage_count_id07, ",",
      negativeage_count_id08, "\n")

print("NegativeCounts based on 'year'",
      negativeage_count04, ",",
      negativeage_count05, ",",
      negativeage_count06, ",",
      negativeage_count07, ",",
      negativeage_count08, "\n")









