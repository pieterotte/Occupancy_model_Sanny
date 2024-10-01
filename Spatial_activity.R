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

#### LINEAR MODELS based on capture rates ####

## CAT vs smaller predator

## CAT VS Beech marten  
# x is predicted based on y in lm(y ~ x, data= data)
# Test Linear model of correlations 
lm.cat_marten <- lm(Domestic.Cat ~ Marten, data= capture_rate_df)
summary(lm.cat_marten)
anova(lm.cat_marten)
confint(lm.cat_marten)
# Visualize the linear relationship
ggplot(capture_rate_df, aes(x = Marten, y = Domestic.Cat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Model",
       x = "Beech Marten", y = "Domestic cat")

## CAT vs Polecat 
# make linear model 
lm.cat_polecat <- lm(Domestic.Cat ~ European.Polecat, data= capture_rate_df)
summary(lm.cat_polecat)
# test assumptions 
par(mfrow = c(2,2))
plot(lm.cat_polecat)
# check statistical significance of the model 
anova(lm.cat_polecat)
confint(lm.cat_polecat, level = 0.95)

## CAT Vs Stoat 
# make linear model 
lm.cat_stoat <- lm(Domestic.Cat ~ Stoat, data= capture_rate_df)
summary(lm.cat_stoat)
# test assumptions 
par(mfrow = c(2,2))
plot(lm.cat_stoat) ## assumptions met quite okay?

# check statistical significance of the model 
anova(lm.cat_stoat)
confint(lm.cat_stoat, level = 0.95)

## CAT vs Weasel 
lm.cat_weasel <- lm(Domestic.Cat ~ Weasel, data= capture_rate_df)
summary(lm.cat_weasel)
# test assumptions 
par(mfrow = c(2,2))
plot(lm.cat_weasel) ## assumptions met quite okay?

# check statistical significance of the model 
anova(lm.cat_weasel)
confint(lm.cat_weasel, level = 0.95)

## MARTEN vs smaller predator 
## Marten vs polecat 
# make linear model 
lm.marten_polecat <- lm(Marten ~ European.Polecat, data= capture_rate_df)
summary(lm.marten_polecat)
# test assumptions 
par(mfrow = c(2,2))
plot(lm.marten_polecat)
# check statistical significance of the model 
anova(lm.marten_polecat)
confint(lm.marten_polecat, level = 0.95)

## Marten vs stoat 
# make linear model 
lm.marten_stoat <- lm(Marten ~ Stoat, data= capture_rate_df)
summary(lm.marten_stoat)
# test assumptions 
par(mfrow = c(2,2))
plot(lm.marten_stoat)
# check statistical significance of the model 
anova(lm.marten_stoat)
confint(lm.marten_stoat, level = 0.95)

## SIGNIFICANCE
# Visualize the linear relationship
ggplot(capture_rate_df, aes(x = Marten, y = Stoat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Model",
       x = "Beech Marten", y ="Stoat")

## Big outlier (SK19)
# try model again with outlier removed
capture_rate_df2 <- capture_rate_df[-19, ]
# model with no outlier 
lm.marten_stoat <- lm(Marten ~ Stoat, data= capture_rate_df2)
summary(lm.marten_stoat)
# test assumptions 
par(mfrow = c(2,2))
plot(lm.marten_stoat)
# check statistical significance of the model 
anova(lm.marten_stoat)
confint(lm.marten_stoat, level = 0.95)
# Visualize the linear relationship
ggplot(capture_rate_df2, aes(x = Marten, y = Stoat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Model no outlier",
       x = "Beech Marten", y ="Stoat")+
  coord_cartesian(ylim = c(0.0, 0.5))

## MARTEN VS WEASEL 
# make linear model 
lm.marten_weasel <- lm(Weasel ~ Marten, data= capture_rate_df)
summary(lm.marten_weasel)
# test assumptions 
par(mfrow = c(2,2))
plot(lm.marten_weasel)
# check statistical signifance of the model 
anova(lm.marten_weasel)
confint(lm.marten_weasel, level = 0.95)

# Visualize the linear relationship
ggplot(capture_rate_df, aes(x = Marten, y = Weasel)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Model",
       x = "Beech Marten", y ="Weasel")
## Outlier, remove this from dataset? --> SK19

# try model again with outlier removed
capture_rate_df2 <- capture_rate_df[-19, ]
# run model again, without ourlier SK19
lm.marten_weasel_no <- lm(Weasel ~ Marten, data= capture_rate_df2)
summary(lm.marten_weasel_no)
# test assumptions 
par(mfrow = c(2,2))
plot(lm.marten_weasel_no)
# check statistical signifance of the model 
anova(lm.marten_weasel_no)
confint(lm.marten_weasel_no, level = 0.95)

# Visualize the linear relationship
ggplot(capture_rate_df2, aes(x = Marten, y = Weasel)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Model no outlier",
       x = "Beech Marten", y ="Weasel") +
  coord_cartesian(ylim = c(0.0, 0.2))

## POLECAT vs stoat 
# make linear model 
lm.polecat_stoat <- lm(European.Polecat ~ Stoat, data= capture_rate_df)
summary(lm.polecat_stoat)
# test assumptions 
par(mfrow = c(2,2))
plot(lm.polecat_stoat)
# check statistical significance of the model 
anova(lm.polecat_stoat)
confint(lm.polecat_stoat, level = 0.95)

# Visualize the linear relationship
ggplot(capture_rate_df, aes(x = European.Polecat, y = Stoat)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Model",
       x = "European Polecat", y ="Stoat")

## POLECAT vs weasel
# make linear model 
lm.polecat_weasel <- lm(European.Polecat ~ Weasel, data= capture_rate_df)
summary(lm.polecat_weasel)
# test assumptions 
par(mfrow = c(2,2))
plot(lm.polecat_weasel)
# check statistical significance of the model 
anova(lm.polecat_weasel)
confint(lm.polecat_weasel, level = 0.95)

# Visualize the linear relationship
ggplot(capture_rate_df, aes(x = European.Polecat, y = Weasel)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Model",
       x = "European Polecat", y ="Weasel")

# STOAT vs weasel 
lm.stoat_weasel <- lm(Stoat ~ Weasel, data= capture_rate_df)
summary(lm.stoat_weasel)
# test assumptions 
par(mfrow = c(2,2))
plot(lm.stoat_weasel)
# check statistical significance of the model 
anova(lm.stoat_weasel)
confint(lm.stoat_weasel, level = 0.95)

# Visualize the linear relationship
ggplot(capture_rate_df, aes(x = Stoat, y = Weasel)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Model",
       x = "Stoat", y ="Weasel")


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
  scale_fill_gradient2(low = "#006D77", high = "#7B2D26", mid = "white", 
                       midpoint = 0.5, limit = c(0, 1), space = "Lab", 
                       name = "SSI") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        plot.background = element_rect(fill ="#FEFAE0", color= NA)) +
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

## make map for soarremoarre area 
# Using shape file 
sf_path <- "C:/Users/sanny/OneDrive/Documenten/Research project 1/SM_gewas"
SM_polygon <- st_read(sf_path)
# change CRS from amersfoort to lon/lat 
st_crs(SM_polygon) # check current CRS
# Transform the CRS from EPSG:28992 (Amersfoort) to EPSG:4326 (WGS84 - Latitude/Longitude)
SM_polygon <- st_transform(SM_polygon, crs = 4326)
# Check the new CRS
st_crs(SM_polygon)

# plot shapefile 
ggplot(data = SM_polygon) +
  geom_sf() + 
  labs(title = "Soarremoarre Polygons") +
  theme_minimal()

#### create pie chart for each location ####
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
       x = "longitude", y = "latitude", size = "Total Observations") +
  theme_minimal() +
  coord_fixed()
base_map

# Add pie charts at respective coordinates
for (i in 1:nrow(capture_rate_df)) {
  pie_grob <- create_pie_chart(capture_rate_df[i, ])
  
  # Add the pie chart at the specified location
  base_map <- base_map + annotation_custom(
    grob = pie_grob,
    xmin = capture_rate_df$longitude[i] - 0.0007,  # Fixed values for each location
    xmax = capture_rate_df$longitude[i] + 0.0007,
    ymin = capture_rate_df$latitude[i] - 0.0007,
    ymax = capture_rate_df$latitude[i] + 0.0007
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







##################################################

library(grid)
if (st_crs(SM_polygon)$epsg != 4326) {
  SM_polygon <- st_transform(SM_polygon, crs = 4326)
}
# Convert capture_rate_df to an sf object if it's not already
capture_rate_sf <- st_as_sf(capture_rate_df, coords = c("longitude", "latitude"), crs = 4326)

















## create pie chart for each location 
# Function to create pie charts for each location
create_pie_chart <- function(capture_rate_df) {
  # Create a dataframe for the pie chart
  pie_data <- data.frame(
    species = c("Domestic.Cat", "European.Polecat", "Weasel", "Stoat", "Marten"),
    values = c(capture_rate_df$Domestic.Cat, capture_rate_df$European.Polecat, capture_rate_df$Weasel, capture_rate_df$Stoat, capture_rate_df$Marten)
  )
 
   # Define custom colors for each species
  custom_colors <- c("Domestic.Cat" = "#BC6C25", 
                     "Marten" = "#606C38", 
                     "European.Polecat" = "#006D77",
                     "Stoat" = "#64403E", 
                     "Weasel" = "#95190C")  
  
  # Plot the pie chart
  p <- ggplot(pie_data, aes(x = "", y = values, fill = species)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = custom_colors) +
    theme_void() +
    theme(legend.position = "none")
  
  # return the pie chart as grob
  ggplotGrob(p)
}

base_map <- ggplot(data = SM_polygon) +
  geom_sf(fill = "grey", color = "black", alpha = 0.5) + 
  labs(title = "Species composition per camera location",
       x = "longitude", y = "latitude", size = "Total Observations") +
  theme_minimal() #+ 
 # theme(plot.background = element_rect(fill= "#FEFAE0", color= NA))

# Add pie charts at respective coordinates
for (i in 1:nrow(capture_rate_df)) {
  pie_grob <- create_pie_chart(capture_rate_df[i, ])
  # Manually extract longitude and latitude for each pie chart
  lon <- capture_rate_df$longitude[i]
  lat <- capture_rate_df$latitude[i]
  
  # Add the pie chart at the specified location
  base_map <- base_map + annotation_custom(
    grob = pie_grob,
    xmin = lon - 0.001,  # Fixed values for each location
    xmax = lon + 0.001,
    ymin = lat - 0.001,
    ymax = lat + 0.001
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
  scale_fill_manual(values = custom_colors) + 
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "topright")
        #, 
        #legend.background = element_rect(fill= "#FEFAE0", color= NA))


# Extract the legend from the dummy plot
legend <- get_legend(dummy_pie)

# combine main map with pie charts and the legend 
final_plot <- cowplot::plot_grid(base_map, legend, ncol = 2, rel_widths = c(3, 1))
print(final_plot)

