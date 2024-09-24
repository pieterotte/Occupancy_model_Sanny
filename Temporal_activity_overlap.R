#### Temporal activity using the overlap package ####
rm(list=ls())
# install.packages("overlap")
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
library("overlap")

## convert times to radians, where 0 and 2pi represent midnight. pi represents midday 
times <- c(1, 2, 15, 18, 21)  # Example times in hours
rad_times <- (times / 24) * 2 * pi  # Convert to radians

## calculate overlap coefficient with "overlapEst()"
times_group1 <- c(1, 3, 6, 9)  # Example times for group 1 in hours
times_group2 <- c(12, 15, 18, 21)  # Example times for group 2 in hours

rad_group1 <- (times_group1 / 24) * 2 * pi
rad_group2 <- (times_group2 / 24) * 2 * pi

overlap_coefficient <- overlapEst(rad_group1, rad_group2)
print(overlap_coefficient)

## visualize activity patterns 
overlapPlot(rad_group1, rad_group2)

## calculate bootstrap confidence intervals to add robustness?? 
set.seed(123)  # For reproducibility
bootstrap_results <- bootEst(rad_group1, rad_group2, R = 1000)
print(bootstrap_results)

#### Try with real data ####

## Load in predators observation data 
predator_obs <- read.csv("predator_observations.csv", header = T) 

## convert time to numeric hours 
# Convert "HH:MM" to hours, then to radians
predator_obs$time.start <- hour(hms(predator_obs$time.start)) + minute(hms(predator_obs$time.start)) / 60

## Now, make a column with radian times 
predator_obs <- predator_obs %>%
  mutate(radiantime = (predator_obs$time.start / 24)* 2 * pi)


## seperate data for each species 
# Subset the data by species
cat_times <- predator_obs$radiantime[predator_obs$commonName == "Domestic Cat"]
stoat_times <- predator_obs$radiantime[predator_obs$commonName == "Stoat"]
weasel_times <- predator_obs$radiantime[predator_obs$commonName == "Weasel"]
polecat_times <- predator_obs$radiantime[predator_obs$commonName == "European Polecat"]
marten_times <- predator_obs$radiantime[predator_obs$commonName == "Marten"]
Mustelid_times <- predator_obs$radiantime[predator_obs$commonName == "Small Mustelid"]

## Calculate the overlap between 2 species 
library(overlap)

### CAT VS other predators 

## Cat and Marten 
# Calculate overlap
overlap_coefficient <- overlapEst(cat_times, marten_times)
print(overlap_coefficient)
## plot with overlapPlot
overlapPlot(cat_times, marten_times, linecol = c("red", "#582f0e"), main = "Domestic Cat and Marten") 
legend("topleft", legend = c("Domestic cat", "Marten"), col = c("red", "#582f0e"), lty = 1, cex = 0.8)


## Cat and Polecat
overlap_coefficient <- overlapEst(cat_times, polecat_times)
print(overlap_coefficient)
## plot with overlapPlot
overlapPlot(cat_times, polecat_times, linecol = c("red", "black"), main = "Domestic Cat and European Polecat") 
legend("topleft", legend = c("Domestic cat", "European Polecat"), col = c("red", "black"), lty = 1, cex = 0.8)


## Cat and Stoat
overlap_coefficient <- overlapEst(cat_times, stoat_times)
print(overlap_coefficient)
## or with overlapPlot
overlapPlot(cat_times, stoat_times, linecol = c("red", "#bc6c25"), main = "Domestic Cat and Stoat") 
legend("center", legend = c("Domestic cat", "Stoat"), col = c("red", "#bc6c25"), lty = 1, cex = 0.8)

## Cat and Weasel 
overlap_coefficient <- overlapEst(cat_times, weasel_times)
print(overlap_coefficient)
## plot with overlapPlot
overlapPlot(cat_times, weasel_times, linecol = c("red", "darkgreen"), main = "Domestic Cat and Weasel") 
legend("topleft", legend = c("Domestic cat", "Weasel"), col = c("red", "darkgreen"), lty = 1, cex = 0.8)


## Cat and small mustelids 
overlap_coefficient <- overlapEst(cat_times, Mustelid_times)
print(overlap_coefficient)
## plot with overlapPlot
overlapPlot(cat_times, Mustelid_times, linecol = c("red", "blue"), main = "Domestic Cat and Small Mustelids") 
legend("topleft", legend = c("Domestic cat", "Small Mustelids"), col = c("red", "blue"), lty = 1, cex = 0.8)


### MARTEN vs other predators 
## marten and polecat 
overlap_coefficient <- overlapEst(marten_times, polecat_times)
print(overlap_coefficient)
## plot with overlapPlot
overlapPlot(marten_times, polecat_times, linecol = c("brown", "black"), main = "Marten and European Polecat") 
legend("topleft", legend = c("Marten", "European polecat"), col = c("brown", "black"), lty = 1, cex = 0.8)

## marten and stoat
overlap_coefficient <- overlapEst(marten_times, stoat_times)
print(overlap_coefficient)
## plot with overlapPlot
overlapPlot(marten_times, stoat_times, linecol = c("brown", "orange"), main = "Marten and Stoat") 
legend("topleft", legend = c("Marten", "Stoat"), col = c("brown", "orange"), lty = 1, cex = 0.8)

## marten and weasel 
overlap_coefficient <- overlapEst(marten_times, weasel_times)
print(overlap_coefficient)
## plot with overlapPlot
overlapPlot(marten_times, weasel_times, linecol = c("brown", "darkgreen"), main = "Marten and Weasel") 
legend("topleft", legend = c("Marten", "Weasel"), col = c("brown", "darkgreen"), lty = 1, cex = 0.8)

### POLECAT vs other predators 
## polecat and stoat 
overlap_coefficient <- overlapEst(polecat_times, stoat_times)
print(overlap_coefficient)
## plot with overlapPlot
overlapPlot(polecat_times, stoat_times, linecol = c("black", "darkorange"), main = "European polecat and Stoat") 
legend("topleft", legend = c("European polecat", "Stoat"), col = c("black", "darkorange"), lty = 1, cex = 0.8)

## polecat and weasel 
overlap_coefficient <- overlapEst(polecat_times, weasel_times)
print(overlap_coefficient)
## plot with overlapPlot
overlapPlot(polecat_times, weasel_times, linecol = c("black", "darkgreen"), main = "European polecat and Weasel") 
legend("topleft", legend = c("European polecat", "Weasel"), col = c("black", "darkgreen"), lty = 1, cex = 0.8)

### STOAT vs other predators 
## weasel and stoat 
overlap_coefficient <- overlapEst(stoat_times, weasel_times)
print(overlap_coefficient)
## or with overlapPlot
overlapPlot(stoat_times, weasel_times, linecol = c("darkorange", "darkgreen"), main = "Stoat and Weasel") 
legend("topleft", legend = c("Stoat", "Weasel"), col = c("darkorange", "darkgreen"), lty = 1, cex = 0.8)

## Make a plot with all 5 species groups 
# Rename species using case_when
predator_obs <- predator_obs %>%
  mutate(commonName = case_when(
    commonName == "Domestic Cat" ~ "Domestic.Cat",
    commonName == "European Polecat" ~ "European.Polecat",
    commonName == "Small Mustelid" ~ "Small.Mustelid",
    commonName == "Weasel" ~ "Weasel",
    commonName == "Stoat" ~ "Stoat",
    commonName == "Mustelid" ~ "Unknown.Mustelid",
    commonName == "Marten" ~ "Marten",
    TRUE ~ commonName  # Keep the original name if not matched
  ))

# filter out the higher taxa groups, so everything is species level
all_pred <- predator_obs %>%
  select(commonName, radiantime) %>%
  filter(!commonName %in% c("Small.Mustelid", "Unknown.Mustelid"))

# use Kernel density estimate 
#install.packages("circular")
library(circular)
# Function to convert time in radians to circular kernel density estimates
estimate_activity <- function(all_pred) {
  density_data <- density.circular(circular(all_pred$radiantime), bw = 10) # Adjust bandwidth (bw) as needed
  return(data.frame(time = density_data$x, density = density_data$y, species = all_pred$commonName[1]))
}

# Apply the KDE function for each species and combine them
kde_data <- do.call(rbind, lapply(split(all_pred, all_pred$commonName), estimate_activity))

# View the estimated KDE data
head(kde_data)

# Plot the kernel density estimates for each species
ggplot(kde_data, aes(x = time, y = density, color = species)) +
  geom_line(size = 1) +  # Line thickness
  scale_x_continuous(breaks = seq(0, 2 * pi, length.out = 13),
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", "10:00", "12:00", 
                                "14:00", "16:00", "18:00", "20:00", "22:00", "00:00")) +
  labs(title = "Temporal Activity of Species", x = "Time of Day", y = "Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Domestic.Cat" = "red", "Marten" = "brown", 
                                "European.Polecat" = "black", "Stoat" = "orange", 
                                "Weasel" = "darkgreen")) + 
  annotate("rect", xmin = 0, xmax = 1.65719, ymin = 0, ymax = Inf, fill = "grey", alpha = 0.3) +  # Nighttime shading # sunset at 21:05 (mean time)
  annotate("rect", xmin = 5.49778, xmax = 2*pi, ymin = 0, ymax = Inf, fill = "grey", alpha = 0.3) # sunrise at 6:20 (mean time)

#alternative option with dotted lines for sunrise and sunset + night hours as shading (like in Tsunoda et al., 2020)
# Plot the kernel density estimates for each species
ggplot(kde_data, aes(x = time, y = density, color = species)) +
  geom_line(size = 1) +  # Line thickness
  scale_x_continuous(breaks = seq(0, 2 * pi, by=pi/6), # breaks at every 2 hours (by 13 works too)
                     labels = c("00:00", "02:00", "04:00", "06:00", "08:00", "10:00", "12:00", 
                                "14:00", "16:00", "18:00", "20:00", "22:00", "00:00")) +
  labs(title = "Temporal Activity of Species", x = "Time of Day", y = "Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Domestic.Cat" = "red", "Marten" = "brown", 
                                "European.Polecat" = "black", "Stoat" = "orange", 
                                "Weasel" = "darkgreen")) + 
  annotate("rect", xmin = 0, xmax = ((5.73 / 24) * 2 * pi), ymin = 0, ymax = Inf, fill = "grey", alpha = 0.3) +  # Nighttime shading # sunset at 21:05 (mean time) so dark at 21:41 (36 min dusk)
  annotate("rect", xmin = ((21.683 / 24) * 2 * pi), xmax = 2*pi, ymin = 0, ymax = Inf, fill = "grey", alpha = 0.3) + # sunrise at 6:20 (mean time), so dark until 5:44 (36 min dawn)
  geom_vline(xintercept = ((7.5 / 24) * 2 * pi), linetype= "dotted", color = "black") +
  geom_vline(xintercept = ((5.36 / 24) * 2 * pi), linetype= "dotted", color = "black") +
  geom_vline(xintercept = ((20.05 / 24) * 2 * pi), linetype= "dotted", color = "black") + 
  geom_vline(xintercept = ((21.93 / 24) * 2 * pi), linetype= "dotted", color = "black") 


#Kruskal Wallis test for mean temporal overlap coefficients for each pairing with domestic cat 
# create df with Dhat1 values for species compared to domestic cats 

overlap_cat <- data.frame(
  species = c("Marten", "European.Polecat", "Stoat", "Weasel", "Small.Mustelid"), 
  overlap_co1 = c(0.6948606, 0.6908695, 0.6908695, 0.8296566, 0.8270280)
)

# Perform Kruskal-Wallis Test
kruskal_test <- kruskal.test(overlap_co1 ~ species, data = overlap_cat)

# View the test result
kruskal_test ## p-value is well above 0.05


# Add statistical groupings manually based on the Dunn test result
# For example, assume the following letters are the groupings:
overlap_cat$grouping <- c("a", "b", "b", "c", "c")

# Create the plot
ggplot(overlap_cat, aes(x = species, y = overlap_co1)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = overlap_co1 - 0.05, ymax = overlap_co1 + 0.05), width = 0.2) +  # Add error bars manually
#  geom_text(aes(label = grouping, y = overlap_co1 + 0.07), size = 5) +  # Add statistical letters
  labs(title = "Mean Temporal Overlap Coefficients by Species",
       x = "Species",
       y = "Temporal overlap w/ domestic cat") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

