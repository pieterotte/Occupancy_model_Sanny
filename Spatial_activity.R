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
list.of.packages <- c("kableExtra", "tidyr", "ggplot2", "gridExtra", "lme4", "dplyr", "Hmsc", "jtools", "lubridate", "corrplot", "MuMIn", "reshape", "cowplot", "ggmap", "osmdata", "sf")

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

# Change species names so they will be easier to work with  
colnames(predator_wide) <- c("locationName", "Domestic.Cat", "European.Polecat", "Small.Mustelid", "Weasel", "Stoat", "Mustelid", "Marten")
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

# save df
write.csv(total_obs, file = "dataframes in R/total_obs.csv", row.names = F)

## Calculate capture rate for each species 
capture_rate_df <- total_obs %>%
  mutate(across(c("Domestic.Cat", "European.Polecat", "Small.Mustelid", "Weasel", 
                  "Stoat", "Mustelid", "Marten"),
                ~ .x / running_days)) 

print(capture_rate_df)
#plot the relationship between raw counts and detection rate 
plot(total_obs$Domestic.Cat ~ capture_rate_df$Domestic.Cat,
     las=1, pch=19, 
     ylab="Number of independent records", 
     xlab="Capture rate per day")

## Single species models 
# uses simple linear models 
# determine interesting covariates


#### test for correlation between all species ####
# Select only the predator columns
predators_cor <- capture_rate_df %>%
  select(Domestic.Cat, Marten, European.Polecat, Stoat, Weasel)

# Compute a correlation matrix
cor_matrix <- cor(predators_cor)

# Convert the correlation matrix into a long format
cor_data <- reshape::melt(cor_matrix)

# Create a heatmap of the correlation matrix
ggplot(cor_data, aes(x = X1, y = X2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap of Predator Observations",
       x = "Species", y = "Species")

# TEST CORRELATION BETWEEN ALL SPECIES 
# determine whether the presence of one species influences the presence of other species

## CAT VS SMALLER PREDATORS
# test correlation between domestic cat and marten 
cor.test(capture_rate_df$Domestic.Cat, capture_rate_df$Marten)
# Scatter plot to show the correlation between Domestic Cats and Martens 
ggplot(capture_rate_df, aes(x =Domestic.Cat, y = Marten)) +
  geom_point(color = "black", size = 3, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear regression line
  labs(title = "Correlation Between Domestic Cat and Marten Observations",
       x = "Domestic Cat Observations",
       y = "Marten Observations") +
  theme_minimal()

# test correlation between Domestic cat and European Polecat 
cor.test(capture_rate_df$Domestic.Cat, capture_rate_df$European.Polecat)
# Scatter plot to show the correlation between Domestic Cats and European Polecat 
ggplot(capture_rate_df, aes(x = Domestic.Cat, y = European.Polecat)) +
  geom_point(color = "black", size = 3, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear regression line
  labs(title = "Correlation Between Domestic Cat and European Polecat Observations",
       x = "Domestic Cat Observations",
       y = "European Polecat Observations") +
  theme_minimal()

# test correlation between Domestic cat and Stoat
cor.test(capture_rate_df$Domestic.Cat, capture_rate_df$Stoat)
# Scatter plot to show the correlation between Domestic Cats and Stoat 
ggplot(capture_rate_df, aes(x = Domestic.Cat, y = Stoat)) +
  geom_point(color = "black", size = 3, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear regression line
  labs(title = "Correlation Between Domestic Cat and Stoat Observations",
       x = "Domestic Cat Observations",
       y = "Stoat Observations") +
  theme_minimal()

# test correlation between Domestic cat and Weasel
cor.test(capture_rate_df$Domestic.Cat, capture_rate_df$Weasel)
# Scatter plot to show the correlation between Domestic Cats and Weasel 
ggplot(capture_rate_df, aes(x = Domestic.Cat, y = Weasel)) +
  geom_point(color = "black", size = 3, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear regression line
  labs(title = "Correlation Between Domestic Cat and Weasel Observations",
       x = "Domestic Cat Observations",
       y = "Weasel Observations") +
  theme_minimal()

## MARTEN VS SMALLER PREDATORS
# test correlation between Marten and Polecat
cor.test(capture_rate_df$Marten, capture_rate_df$European.Polecat)
# Scatter plot to show the correlation between Domestic Cats and Weasel 
ggplot(capture_rate_df, aes(x = Marten, y = European.Polecat)) +
  geom_point(color = "black", size = 3, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear regression line
  labs(title = "Correlation Between Marten and European Polecat Observations",
       x = "Marten Observations",
       y = "European Polecat Observations") +
  theme_minimal()

# test correlation between Marten and Stoat
cor.test(capture_rate_df$Marten, capture_rate_df$Stoat)
# Scatter plot to show the correlation between Marten and Stoat 
ggplot(capture_rate_df, aes(x = Marten, y = Stoat)) +
  geom_point(color = "black", size = 3, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear regression line
  labs(title = "Correlation Between Marten and Stoat Observations",
       x = "Marten Observations",
       y = "Stoat Observations") +
  theme_minimal()

# test correlation between Marten and Weasel
cor.test(capture_rate_df$Marten, capture_rate_df$Weasel)
# Scatter plot to show the correlation between Marten and Weasel 
ggplot(capture_rate_df, aes(x = Marten, y = Weasel)) +
  geom_point(color = "black", size = 3, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear regression line
  labs(title = "Correlation Between Marten and Weasel Observations",
       x = "Marten Observations",
       y = "Weasel Observations") +
  theme_minimal()

## POLECAT vs smaller predator 
# test correlation between Polecat and Stoat
cor.test(capture_rate_df$European.Polecat, capture_rate_df$Stoat)
# Scatter plot to show the correlation between Polecat and Stoat 
ggplot(capture_rate_df, aes(x = European.Polecat, y = Stoat)) +
  geom_point(color = "black", size = 3, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear regression line
  labs(title = "Correlation Between European polecat and Stoat Observations",
       x = "Polecat Observations",
       y = "Stoat Observations") +
  theme_minimal()

# test correlation between Polecat and Weasel
cor.test(capture_rate_df$European.Polecat, capture_rate_df$Weasel)
# Scatter plot to show the correlation between Polecat and Weasel
ggplot(capture_rate_df, aes(x = European.Polecat, y = Weasel)) +
  geom_point(color = "black", size = 3, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear regression line
  labs(title = "Correlation Between European polecat and Weasel Observations",
       x = "Polecat Observations",
       y = "Weasel Observations") +
  theme_minimal()

# STOAT vs smaller predator
# test correlation between stoat and Weasel
cor.test(capture_rate_df$Stoat, capture_rate_df$Weasel)
# Scatter plot to show the correlation between Stoat and Weasel
ggplot(capture_rate_df, aes(x = Stoat, y = Weasel)) +
  geom_point(color = "black", size = 3, alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear regression line
  labs(title = "Correlation Between Stoat and Weasel Observations",
       x = "Stoat Observations",
       y = "Weasel Observations") +
  theme_minimal()

#### Calculate the Sorensen similarity Index ####
# Create a binary matrix for species presence/absence
presence_absence <- total_obs %>%
  select(Domestic.Cat, Marten, European.Polecat, Stoat, Weasel) %>%
  mutate_all(~ ifelse(. > 0, 1, 0))

# Function to calculate Sørensen Similarity Index between two species
sorensen_similarity <- function(x, y) {
  aij <- sum(x == 1 & y == 1)  # Sites where both species are present
  bij <- sum(x == 1 & y == 0)  # Sites where only the first species is present
  cij <- sum(x == 0 & y == 1)  # Sites where only the second species is present
  
  # Calculate Sørensen Similarity Index
  ssi <- 2 * aij / (2 * aij + bij + cij)
  return(ssi)
}

# Calculate the Sørensen index for each pair of species
species <- colnames(presence_absence)
sorensen_matrix <- matrix(NA, ncol = length(species), nrow = length(species))
colnames(sorensen_matrix) <- species
rownames(sorensen_matrix) <- species

# Loop through each pair of species and calculate the index
for (i in 1:length(species)) {
  for (j in 1:length(species)) {
    sorensen_matrix[i, j] <- sorensen_similarity(presence_absence[[i]], presence_absence[[j]])
  }
}
print(sorensen_matrix)

# visualize the matrix in a heatmap 
# Melt the matrix into long format for ggplot
sorensen_data <- melt(sorensen_matrix)

# Create a heatmap of the Sørensen similarity index
ggplot(sorensen_data, aes(x = X1, y = X2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0, 1), space = "Lab", 
                       name = "SSI") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Sørensen Similarity Index Between Species",
       x = "Species", y = "Species")




#### Create map with pie charts #### 

# Make map with # observations per species located at camera locations 
# add latitude and longitude of the camera's 

depl <- read.csv("depl.csv")
locations <- depl %>%
  select(locationName, latitude, longitude) %>%
  distinct(locationName, latitude, longitude, .keep_all = TRUE)
# merge with capture rate data
capture_rate_df <- merge(capture_rate_df, locations, by= "locationName")

## create pie chart for each location 
# Function to create pie charts for each location
create_pie_chart <- function(capture_rate_df) {
  # Create a dataframe for the pie chart
  pie_data <- data.frame(
    species = c("Domestic.Cat", "European.Polecat", "Weasel", "Stoat", "Marten"),
    values = c(capture_rate_df$Domestic.Cat, capture_rate_df$European.Polecat, capture_rate_df$Weasel, capture_rate_df$Stoat, capture_rate_df$Marten)
  )
  
  # Plot the pie chart
  p <- ggplot(pie_data, aes(x = "", y = values, fill = species)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(legend.position = "none")
  
  # return the pie chart as grob
  ggplotGrob(p)
}

# Base map (a simple blank map or any geographic boundary map can be added)
base_map <- ggplot(capture_rate_df, aes(x = longitude, y = latitude)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Total Observations per Location",
       x = "Longitude", y = "Latitude", size = "Total Observations") +
  theme_minimal() +
  coord_fixed()
base_map

# Add pie charts at respective coordinates
for (i in 1:nrow(capture_rate_df)) {
  pie_grob <- create_pie_chart(capture_rate_df[i, ])
  
  # Add the pie chart at the specified location
  base_map <- base_map + annotation_custom(
    grob = pie_grob,
    xmin = capture_rate_df$longitude[i] - 0.0005,  # Fixed values for each location
    xmax = capture_rate_df$longitude[i] + 0.0005,
    ymin = capture_rate_df$latitude[i] - 0.0005,
    ymax = capture_rate_df$latitude[i] + 0.0005
  )
}

# Plot the map with pie charts
print(base_map)


## Add a legend to the plot 
# Create a dummy data frame for the legend
dummy_data <- data.frame(
  species = c("Domestic.Cat","European.Polecat", "Weasel", "Stoat", "Marten"),
  values = c(1, 1, 1, 1, 1)  # Equal values just to generate the pie chart for the legend
)

# Create a dummy pie chart for the legend
dummy_pie <- ggplot(dummy_data, aes(x = "", y = values, fill = species)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "right")

# Extract the legend from the dummy plot
legend <- get_legend(dummy_pie)

# combine main map with pie charts and the legend 
final_plot <- cowplot::plot_grid(base_map, legend, ncol = 2, rel_widths = c(3, 1))
print(final_plot)



## make map for soarremoarre area 
# Define the bounding box using your coordinates
bbox <- c(left = 5.850, bottom = 53.050, right = 5.890, top = 53.070)

# Get the map using OpenStreetMap as the source
map <- get_map(location = bbox, source = "osm", zoom = 14)  # Adjust zoom as needed

# Plot the map as a background
ggmap(map) +
  labs(title = "Map of the Specified Area") +
  theme_minimal()


## try again
# Define bounding box for Soarremoarre, Friesland, Netherlands
bbox <- c(5.850, 53.050, 5.890, 53.070)  # left, bottom, right, top
# Get OSM data for highways (roads) within the bounding box
osm_data <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()
# Plot the OpenStreetMap data (roads)
ggplot() +
  geom_sf(data = osm_data$osm_lines, color = "black", size = 0.3) +
  labs(title = "Map of Soarremoarre, Friesland (Roads)") +
  theme_minimal()


## Try my own way
SM23_map <- ggplot() +
  borders(xlim = c(5.850, 5.890), ylim = c(53.050, 53.070), fill = "gray90") +
  coord_fixed() + 
  theme_minimal()
SM23_map
##################################################

## create pie chart for each location 
# Function to create pie charts for each location
create_pie_chart <- function(capture_rate_df) {
  # Create a dataframe for the pie chart
  pie_data <- data.frame(
    species = c("Domestic.Cat", "European.Polecat", "Weasel", "Stoat", "Marten"),
    values = c(capture_rate_df$Domestic.Cat, capture_rate_df$European.Polecat, capture_rate_df$Weasel, capture_rate_df$Stoat, capture_rate_df$Marten)
  )
  
  # Plot the pie chart
  p <- ggplot(pie_data, aes(x = "", y = values, fill = species)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(legend.position = "none")
  
  # return the pie chart as grob
  ggplotGrob(p)
}

# Define bounding box for Soarremoarre, Friesland, Netherlands
bbox <- c(5.855, 53.050, 5.888, 53.065)  # left, bottom, right, top
# Get OSM data for highways (roads) within the bounding box
osm_data <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()
# Plot the OpenStreetMap data (roads)

# Base map (a simple blank map or any geographic boundary map can be added)
base_map <- ggplot() +
  geom_sf(data = osm_data$osm_lines, color = "black", size = 0.3) +
  labs(title = "Map of Soarremoarre, Friesland (Roads)") +
  theme_minimal()
base_map

# Add pie charts at respective coordinates
for (i in 1:nrow(capture_rate_df)) {
  pie_grob <- create_pie_chart(capture_rate_df[i, ])
  
  # Add the pie chart at the specified location
  base_map <- base_map + annotation_custom(
    grob = pie_grob,
    xmin = capture_rate_df$longitude[i] - 0.001,  # Fixed values for each location
    xmax = capture_rate_df$longitude[i] + 0.001,
    ymin = capture_rate_df$latitude[i] - 0.001,
    ymax = capture_rate_df$latitude[i] + 0.001
  )
}

# Plot the map with pie charts
print(base_map)
