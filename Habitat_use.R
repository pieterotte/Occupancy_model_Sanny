#### Calculate detection rate ####
## Based on script from wilCo-Analysis chapter 10 

## For correlation plots (like in charlie's thesis):
#1: aantal dagen running time per camera uitrekenen 
#2: aantal observaties per soort per camera uitrekenen 
#3: observaties delen door aantal dagen running time 
#4: in tabel zetten en cor.test() gebruiken? of over een kaartje heen plotten 
#5: hiermee is te zien of the aanwezigheid van de ene predator, de aanwezigheid van de ander beïnvloed

rm(list=ls())

# load packages
list.of.packages <- c("kableExtra", "tidyr", "ggplot2", "gridExtra", "lme4", "dplyr", "Hmsc", "jtools", "lubridate", "corrplot", "MuMIn")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# create total observations dataset with location, days of running time and species 
# load observation data and cam running time dataframes
predator_obs <- read.csv("predator_observations.csv")
downtime_long <- read.csv("dataframes in R/downtime_long.csv")

# select species, count and locationName from predator_obs 
predator_sum <- predator_obs %>%
  dplyr::select(locationName, commonName, count)

# Group by locationName and commonName, then summarize the count into a new column called 'sum'
predator_sum <- predator_sum %>%
  group_by(locationName, commonName) %>%
  summarize(sum = sum(count)) %>%
  ungroup()
# convert to wide format 
predator_wide <- predator_sum %>%
  pivot_wider(names_from = commonName, values_from = sum, values_fill = 0)

# create dataframe containing running time per camera in days 

# first sum downtime in days per cam
downtime_long <- downtime_long %>%
  mutate(detected, down = 1)
downtime_cam <- downtime_long %>%
  group_by(locationName) %>%
  summarize(sum = sum(down)) %>%
  ungroup()

running_time <- data.frame(
  locationName = sprintf("SK%02d", 1:30), 
  days = rep(74, 30)
)

# add two dataframes together 
cam_days <- merge(running_time, downtime_cam, by = "locationName", all.x = TRUE)
cam_days$sum[is.na(cam_days$sum)] <- 0

cam_days <- cam_days %>%
  mutate(running_days = days-sum) %>%
  select(locationName, running_days)

# now add running_time to the predator_wide df
total_obs <- merge(predator_wide, cam_days, by= "locationName")
