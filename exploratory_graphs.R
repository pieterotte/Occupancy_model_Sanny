## Script om een aantal overzichtelijke grafieken te maken ##
rm(list=ls())

# load packages
list.of.packages <- c("kableExtra", "tidyr", "ggplot2", "gridExtra", "lme4", "dplyr", "Hmsc", "jtools", "lubridate", "corrplot", "MuMIn")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

total_obs <- read.csv("dataframes in R/total_obs.csv")

# make a bar graph with total observation per species 
species_totals <- total_obs %>%
  summarise(
    Domestic_Cat = sum(Domestic.Cat),
    European_Polecat = sum(European.Polecat),
    Small_Mustelid = sum(Small.Mustelid, Weasel, Stoat), #do we add polecat or not?
    Weasel = sum(Weasel),
    Stoat = sum(Stoat),
  #  Mustelid = sum(Mustelid, Small.Mustelid, Weasel, Stoat, Marten, European.Polecat),
    Marten = sum(Marten)
  )

# Convert to long format for ggplot
species_totals_long <- species_totals %>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "Total_Observations")

# Create the bar graph
ggplot(species_totals_long, aes(x = Species, y = Total_Observations)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Total Observations per Species", x = "Species", y = "Total Observations") +
  theme_minimal()

# Make map with # observations per species located at camera locations 
# add latitude and longitude of the camera's 

depl <- read.csv("depl.csv")
locations <- depl %>%
  select(locationName, latitude, longitude) %>%
  distinct(locationName, latitude, longitude, .keep_all = TRUE)

total_obs <- merge(total_obs, locations, by= "locationName")

# make map with dot size depending on number of observations 
# first for all species 

# Calculate total observations per location
data_with_totals <- total_obs %>%
  rowwise() %>%
  mutate(total_observations = sum(c_across(Domestic.Cat:Marten)))

# Create a basic map with ggplot2
ggplot(data_with_totals, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = total_observations), color = "blue", alpha = 0.7) +
  labs(title = "Total Observations per Location",
       x = "Longitude", y = "Latitude", size = "Total Observations") +
  theme_minimal() +
  coord_fixed()

## put a map as background 
library(leaflet)

# Create an interactive map with leaflet
leaflet(data_with_totals) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, radius = ~sqrt(total_observations),
                   popup = ~paste("Location:", locationName, "<br>",
                                  "Total Observations:", total_observations),
                   color = "blue", fillOpacity = 0.7)

# could be done for each species 

## Create map of temporal activity for each species 