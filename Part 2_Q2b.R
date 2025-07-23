
# Understanding the data:
str(planedata)
str(df_2004)

# Identifying and cross referencing flights in 'planedata' with each df
# using filter
actuallyflew_2004 <- planedata %>% filter(tailnum %in% df_2004$TailNum)
actuallyflew_2005 <- planedata %>% filter(tailnum %in% df_2005$TailNum)
actuallyflew_2006 <- planedata %>% filter(tailnum %in% df_2006$TailNum)
actuallyflew_2007 <- planedata %>% filter(tailnum %in% df_2007$TailNum)
actuallyflew_2008 <- planedata %>% filter(tailnum %in% df_2008$TailNum)

# Creating age variable 
actuallyflew_2004 <- actuallyflew_2004 %>% 
  mutate(year = as.numeric(year), age = 2004 - year)
actuallyflew_2005 <- actuallyflew_2005 %>% 
  mutate(year = as.numeric(year),age = 2005 - year)
actuallyflew_2006 <- actuallyflew_2006 %>% 
  mutate(year = as.numeric(year),age = 2006 - year)
actuallyflew_2007 <- actuallyflew_2007 %>% 
  mutate(year = as.numeric(year),age = 2007 - year)
actuallyflew_2008 <- actuallyflew_2008 %>% 
  mutate(year = as.numeric(year),age = 2008 - year)

# counting non-numeric years

# Why issue date wasn't used

# visualising age based on year and total delays 
