#### Occupancy matrix script Sanny ####
rm(list=ls())

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

## Load in predators observation data 
predator_obs <- read.csv("predator_observations.csv") 

# Generate a complete date sequence to include all dates of the study period 
start_date <- as.Date(min(predator_obs$date.start)) #convert to date and pick 1st date 
end_date <- as.Date(max(predator_obs$date.start)) # convert to date and pick last date 
all_dates <- seq.Date(from = start_date, to = end_date, by = "day") ## 74 objects, so every day is included!


## add column with common names and merge felis and martes 
# first make dataframe with common names 

## this was already done in the prep script so not necessary anymore 

#species_table <- data.frame(
#scientificName = c("Mustela nivalis", "Mustela erminea", "Mustela putorius", "Felis catus", "Felis", "Martes martes/foina","Martes", "Mustelidae", "Mustela"),
#commonName = c("Weasel", "Stoat", "European Polecat", "Domestic Cat","Domestic Cat", "Marten","Marten", "Mustelid", "Small Mustelid" ))

# join by scientific name 
# predator_obs <- predator_obs %>% 
# left_join(species_table, by = "scientificName")

# Create a unique identifier for each sampling period (e.g., day)
predator_obs <- predator_obs %>%
  mutate(period = paste(locationName, date.start, sep = "_"))

# Ensure unique combinations of locationName and date.start
data_unique <- predator_obs %>%
  dplyr::select(locationName, date.start) %>%
  distinct() %>%
  mutate(detected = 1)
# Check for duplicates
duplicates <- data_unique %>%
  group_by(locationName, date.start) %>%
  filter(n() > 1)
if(nrow(duplicates) > 0) {
  print("Duplicates found:")
  print(duplicates)
} else {
  print("No duplicates found")
}

# Create the occupancy matrix

occupancy_matrix <- data_unique %>%
  complete(locationName, date.start, fill = list(detected = 0)) %>%
  spread(key = date.start, value = detected, fill = 0)
# print 
print(occupancy_matrix)

## visualize occupancy matrix with HEATMAP 
# Melt the matrix for ggplot2
occupancy_melt_all <- reshape2::melt(occupancy_matrix, id.vars = "locationName", variable.name = "date.start", value.name = "detected")
ggplot(occupancy_melt_all, aes(x = date.start, y = locationName, fill = factor(detected))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "grey", "1" = "blue"), name = "Detection") +
  labs(title = paste("Occupancy Matrix for all species")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### Make occupancy matrices for each species ####

# Create a list to store occupancy matrices for each species
occupancy_matrices <- list()

# Create a dataframe with all combinations of locations and all dates
locations <- unique(predator_obs$locationName)
date_location_grid <- expand.grid(locationName = locations, date.start = all_dates)


## ensure unique list for date, location and Species name 
# Ensure unique combinations of locationName, date.start, and commonName (so martes and felis are grouped)
predator_observations_unique <- predator_obs %>%
  dplyr::select(locationName, date.start, commonName) %>%
  distinct() %>%
  mutate(detected = 1)

# Ensure date.start is Date type in both dataframes
predator_observations_unique <- predator_observations_unique %>%
  mutate(date.start = as.Date(date.start))

date_location_grid <- date_location_grid %>%
  mutate(date.start = as.Date(date.start))

# Verify the structure 
str(date_location_grid$date.start)
str(predator_observations_unique$date.start)

# Generate complete date-location grid with all possible dates for each location
full_data <- date_location_grid %>%
  left_join(predator_observations_unique, by = c("locationName", "date.start")) %>%
  mutate(detected = ifelse(is.na(detected), 0, detected))

# Get the list of unique species
species_list <- unique(predator_observations_unique$commonName)

# Loop over each species to create an occupancy matrix
for(species in species_list) {
  species_data <- predator_observations_unique %>%
    filter(commonName == species) %>%
    dplyr::select(locationName, date.start, detected)
  
  # Merge with the complete date-location grid
  complete_data <- date_location_grid %>%
    left_join(species_data, by = c("locationName", "date.start")) %>%
    mutate(detected = ifelse(is.na(detected), 0, detected))
  
  occupancy_matrix <- complete_data %>%
    spread(key = date.start, value = detected, fill = 0)
  # occupancy_matrix <- species_data %>%
  #  complete(locationName, date.start, fill = list(detected = 0)) %>%
  # spread(key = date.start, value = detected, fill = 0)
  
  # Add the occupancy matrix to the list
  occupancy_matrices[[species]] <- occupancy_matrix
}

# Check one of the occupancy matrices
head(occupancy_matrices[[species_list[1]]])

### Visualize plots

# Melt the occupancy matrix for visualization
occupancy_melt <- occupancy_matrices[[species_list[6]]] %>%
  pivot_longer(cols = -locationName, names_to = "date.start", values_to = "detected")

# Create the occupancy matrix plot
ggplot(occupancy_melt, aes(x = date.start, y = locationName, fill = factor(detected))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "grey", "1" = "blue"), name = "Detection") +
  labs(title = paste("Occupancy Matrix for", species_list[6]), x = "Date", y = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Change number in [] from 1:7 to create matrix for different species 

# Create occupancy matrices for each species
occupancy_matrices <- list()
species_list <- unique(predator_observations_unique$commonName)

for (species in species_list) {
  species_data <- full_data %>%
    filter(commonName == species | is.na(commonName)) %>%
    dplyr::select(locationName, date.start, detected)
  
  occupancy_matrix <- species_data %>%
    spread(key = date.start, value = detected, fill = 0)
  
  # Add the occupancy matrix to the list
  occupancy_matrices[[species]] <- occupancy_matrix
}

# Check one of the occupancy matrices
head(occupancy_matrices[[species_list[1]]])

# Visualize the occupancy matrix for one species
occupancy_melt <- occupancy_matrices[[species_list[1]]] %>%
  pivot_longer(cols = -locationName, names_to = "date.start", values_to = "detected")

# Convert date.start to Date type for proper plotting
occupancy_melt$date.start <- as.Date(occupancy_melt$date.start)

# Create the occupancy matrix plot
ggplot(occupancy_melt, aes(x = date.start, y = locationName, fill = factor(detected))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "grey", "1" = "blue"), name = "Detection") +
  labs(title = paste("Occupancy Matrix for", species_list[1]), x = "Date", y = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Repeat for other species by changing the index in species_list
