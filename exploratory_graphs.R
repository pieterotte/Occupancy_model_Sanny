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
    Marten = sum(Marten),
    European_Polecat = sum(European.Polecat),
    Small_Mustelid = sum(Small.Mustelid), #do we add polecat or not?
    Weasel = sum(Weasel),
    Stoat = sum(Stoat),
  #  Mustelid = sum(Mustelid, Small.Mustelid, Weasel, Stoat, Marten, European.Polecat),
    )

# Convert to long format for ggplot
species_totals_long <- species_totals %>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "Total_Observations")

# Create the bar graph
ggplot(species_totals_long, aes(x = Species, y = Total_Observations, fill=Species)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Observations per Species", x = "Species", y = "Total Observations") +
  theme_minimal()

# make same map but leave out the small mustelids
species_totals2 <- species_totals %>%
  select(-Small_Mustelid) %>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "Total_Observations")
#create bar plot again 
ggplot(species_totals2, aes(x = Species, y = Total_Observations, fill=Species)) +
  geom_bar(stat = "identity") +
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

### MAKE bargraph for proportion of stations visited
print(total_obs)
# Convert the data from wide to long format
df_long <- total_obs %>%
  select(-running_days, -longitude, -latitude, -Mustelid) %>% #, -Small_Mustelid
  pivot_longer(cols = -locationName, names_to = "Species", values_to = "Observations")

print(df_long)
# Calculate the proportion of locations visited per species (non-zero values)
species_proportions <- df_long %>%
  mutate(Observations = ifelse(Observations > 1, 1, Observations)) %>%
  group_by(Species) %>%
  summarise(Proportion_Locations = sum(Observations > 0) / n())

# Create the bar plot
ggplot(species_proportions, aes(x = Species, y = Proportion_Locations, fill = Species)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Locations Visited per Species", x = "Species", y = "Proportion of Locations") +
  theme_minimal()
