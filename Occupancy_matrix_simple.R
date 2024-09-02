rm(list=ls())

## make occupancy matrix including NA's 
#Load Packages
list.of.packages <- c(
  "leaflet",       # creates interactive maps
  "plotly",        # creates interactive plots   
  "kableExtra",    # Creates interactive tables 
  "tidyr",         # A package for data manipulation
  "dplyr",         # A package for data manipulation
  "viridis",       # Generates colors for plots  
  "corrplot",      # Plots pairwise correlations
  "lubridate",     # Easy manipulation of date objects
  "taxize",        # Package to check taxonomy 
  "sf",            # Package for spatial data analysis 
  "ggplot2",
  "readr")            
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


predator_obs <- read.csv("predator_observations.csv")
cam_downtime <- read.csv2("Cam_downtime.csv")

## Start by making a dataframe including camera downtime 
# Convert the Date column to a proper date format
cam_downtime$start_downtime <- as.Date(cam_downtime$start_downtime, format = "%d-%m-%Y")
cam_downtime$end_downtime <- as.Date(cam_downtime$end_downtime, format = "%d-%m-%Y")

# Expand the data frame to long format by each date within the downtime range
downtime_long <- cam_downtime %>%
  rowwise() %>%
  mutate(date.start = list(seq(from = start_downtime, to = end_downtime, by = "day"))) %>%
  unnest(date.start)
# Add column specifying downtime and select necessary columns
downtime_long <- downtime_long %>% 
  mutate(detected = "down") %>%
  select(locationName, date.start, detected)
# convert it to numeric (make NA's)
# downtime_long$detected <- as.numeric(downtime_long$detected)


# Create a dataframe with all combinations of locations and all dates
# define locations
locations <- unique(predator_obs$locationName)
locations <- sort(locations)
# Define the start and end dates
start_date <- as.Date("2023-03-26")
end_date <- as.Date("2023-06-07")

# Create a sequence of dates from start_date to end_date
all_dates <- seq(from = start_date, to = end_date, by = "day")
# create a grid with all combinations of locations and dates
date_location_grid <- expand.grid(locationName = locations, date.start = all_dates, detected = 0)

## create dataframe containing camera observations (in binary form)

# Ensure unique combinations of locationName and date.start
species_obs <- predator_obs %>%
  dplyr::select(locationName, date.start) %>%
  distinct() %>%
  mutate(detected = 1)
# Check for duplicates
duplicates <- species_obs %>%
  group_by(locationName, date.start) %>%
  filter(n() > 1)
if(nrow(duplicates) > 0) {
  print("Duplicates found:")
  print(duplicates)
} else {
  print("No duplicates found")
}

species_obs$date.start <- as.Date(species_obs$date.start)


## merge all 3 dataframes into 1 
occu_d <- merge(species_obs, downtime_long, by = c("locationName", "date.start"))

occu_d <- downtime_long %>%
  left_join(species_obs, by = c("locationName", "date.start"))

combined_df <- date_location_grid %>%
  full_join(species_obs, by = c("locationName", "date.start")) %>%
  full_join(downtime_long, by = c("locationName", "date.start"))

combined_df <- combined_df %>%
  mutate(detected = coalesce(detected.x, detected.y, detected)) %>%
  select(locationName, date.start, detected)
# Create the occupancy matrix

occupancy_matrix <- data_unique %>%
  complete(locationName, date.start, fill = list(detected = 0)) %>%
  spread(key = date.start, value = detected, fill = 0)
# print 
print(occupancy_matrix)

 


