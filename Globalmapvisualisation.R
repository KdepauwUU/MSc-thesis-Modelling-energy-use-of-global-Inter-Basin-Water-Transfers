######################################################################################################################################
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# Step 1: Load and process vector files
load_vector_data <- function() {
  # Path to vector files
  Vector_folder <- "C:/Users/2816954/OneDrive - Universiteit Utrecht/THESIS/data/WTMP_vectors_modified"
  Vector_filelist <- list.files(path = Vector_folder, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  # Create list to store vector data
  vector_list <- list()
  
  for (vector_file in Vector_filelist) {
    vector_data <- st_read(vector_file, quiet = TRUE)           # Read vector file
    vector_data <- st_transform(vector_data, crs = 4326)        # Transform to WGS 84 (EPSG:4326)
    vector_data$Name <- sub("\\.shp$", "", basename(vector_file)) # Extract name from filename
    
    # Ensure consistent column names (keep only geometry and Name column)
    vector_data <- vector_data[, c("geometry", "Name")]
    
    vector_list[[length(vector_list) + 1]] <- vector_data       # Add to list
  }
  
  # Combine all vectors into a single sf object
  all_vectors <- do.call(rbind, vector_list)
  
  # Calculate centroids for labeling
  centroids <- st_centroid(all_vectors)
  
  return(list(vectors = all_vectors, centroids = centroids))
}

vector_data <- load_vector_data()

# Step 2: Load a world map for context
world <- ne_countries(scale = "medium", returnclass = "sf")

# Step 3: Create plots
# Global map
plot_global <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +                     # World map background
  geom_sf(data = vector_data$vectors, aes(geometry = geometry),      # Vector lines
          color = "black", size = 2) +
  theme_minimal() +
  labs(title = "Global map of Inter-Basin Water Transfer (IBWT) megaprojects",
       x = "Longitude", y = "Latitude")

# Zoomed-in regions
plot_north_america <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +
  geom_sf(data = vector_data$vectors, aes(geometry = geometry),
          color = "black", size = 2) +
  coord_sf(xlim = c(-170, -50), ylim = c(10, 80)) +  # North America
  theme_minimal() +
  labs(title = "North America")

plot_europe_north_africa <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +
  geom_sf(data = vector_data$vectors, aes(geometry = geometry),
          color = "black", size = 2) +
  coord_sf(xlim = c(-30, 60), ylim = c(20, 70)) +  # Europe and North Africa
  theme_minimal() +
  labs(title = "Europe and North Africa")

plot_china_india <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +
  geom_sf(data = vector_data$vectors, aes(geometry = geometry),
          color = "black", size = 2) +
  coord_sf(xlim = c(60, 140), ylim = c(5, 50)) +  # China and India
  theme_minimal() +
  labs(title = "China and India")

# Combine plots using patchwork
combined_plot <- (plot_global + plot_north_america) / (plot_europe_north_africa + plot_china_india)

# Display the combined plot
print(plot_global)
print(plot_north_america)
print(plot_europe_north_africa)
print(plot_china_india)
print(combined_plot)
######################################################################################################################################
# Load the IBWT dataset
ibwt_dataset <- IBWT_megaproject_dataset  # Adjust the path
colnames(ibwt_dataset) <- tolower(colnames(ibwt_dataset))         # Standardize column names

# Merge centroids with IBWT dataset
centroids_with_naturalheightgain <- vector_data$centroids %>%
  left_join(ibwt_dataset, by = c("Name" = "transfer_name")) %>%    # Join datasets by matching Transfer_Name
  mutate(total_natural_height_gain = as.numeric(total_natural_height_gain))       # Ensure numeric column

# World map with centroids sized by total distance
plot_centroids_with_naturalheightgain <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +                     # World map background
  geom_sf(data = centroids_with_naturalheightgain, aes(geometry = geometry, color = total_natural_height_gain),
          size = 4, alpha = 0.7) +                          # Centroids sized by distance
  scale_color_viridis_c(option = "H", name = "total natural height gain (m)") + # Control point size range
  theme_minimal() +
  labs(title = "Global Map of IBWT Projects",
       subtitle = "Centroids sized by total natural height gain (m)",
       x = "Longitude", y = "Latitude")

# Save plot
print(plot_centroids_with_naturalheightgain)
######################################################################################################################################
# Merge the vector data with the IBWT dataset
vector_data_with_distance <- vector_data$vectors %>%
  left_join(ibwt_dataset, by = c("Name" = "transfer_name")) %>%    # Join datasets by matching Transfer_Name
  mutate(total_distance_km = as.numeric(total_distance_km))       # Ensure numeric column

# World map with vector lines colored by total distance
plot_vectors_with_distance <- ggplot(data = world) +
  geom_sf(fill = "gray50", color = "white") +                     # World map background
  geom_sf(data = vector_data_with_distance, aes(geometry = geometry, color = total_distance_km),
          size = 8) +                                             # Vectors colored by distance
  scale_color_viridis_c(option = "H", name = "Distance (km)") +    # Use Viridis for color scale
  theme_minimal() +
  labs(title = "Global Map of IBWT Projects",
       subtitle = "Vectors colored by total distance (km)",
       x = "Longitude", y = "Latitude")

# Save plot
print(plot_vectors_with_distance)
######################################################################################################################################
# Define boundaries for categories
boundaries <- c(0, 10000, 50000, 100000, 200000, 300000, 400000, Inf)  # Adjust these thresholds as needed
labels <- c("0-10,000", "10,000-50,000", "50,000-100,000", "100,000-200,000", "200,000-300,000", "300,000-400,000", "400,000+")

# Categorize total_infrastructure_points_per_transfer into bins
centroids_with_naturalheightgain <- centroids_with_naturalheightgain %>%
  mutate(category = cut(total_natural_height_gain, breaks = boundaries, labels = labels))

# World map with centroids categorized by infrastructure points
plot_centroids_with_naturalheightgain <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +                     # World map background
  geom_sf(data = centroids_with_naturalheightgain, 
          aes(geometry = geometry, color = category), 
          size = 4, alpha = 1) +                                # Centroids colored by categories
  scale_color_viridis_d(option = "C", direction = -1,                            # Use Viridis for discrete categories
                        name = "total natural height gain (m)") +
  theme_minimal() +
  labs(title = "Global Map of IBWT Projects",
       subtitle = "Centroids categorized by total natural height gain (m)",
       x = "Longitude", y = "Latitude")

# Save plot
print(plot_centroids_with_naturalheightgain)
######################################################################################################################################
# Load the IBWT dataset
ibwt_dataset <- IBWT_megaproject_dataset  # Adjust the path
colnames(ibwt_dataset) <- tolower(colnames(ibwt_dataset))         # Standardize column names

# Merge centroids with IBWT dataset
centroids_with_naturalheightloss <- vector_data$centroids %>%
  left_join(ibwt_dataset, by = c("Name" = "transfer_name")) %>%    # Join datasets by matching Transfer_Name
  mutate(total_natural_height_loss = as.numeric(total_natural_height_loss))       # Ensure numeric column

# Define boundaries for categories
boundaries <- c(-Inf, -400000, -300000, -200000, -100000, -50000, -10000, 0)  # Adjust these thresholds as needed
labels <- c("-400,000+", "-300,000 - -400,000", "-200,000 - -300,000", "-100,000 - -200,000", "-50,000 - -100,000", "-10,000 - -50,000", "0 - -10,000")

# Categorize total_infrastructure_points_per_transfer into bins
centroids_with_naturalheightloss <- centroids_with_naturalheightloss %>%
  mutate(category = cut(total_natural_height_loss, breaks = boundaries, labels = labels))

# World map with centroids categorized by infrastructure points
plot_centroids_with_naturalheightloss <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +                     # World map background
  geom_sf(data = centroids_with_naturalheightloss, 
          aes(geometry = geometry, color = category), 
          size = 4, alpha = 1) +                                # Centroids colored by categories
  scale_color_viridis_d(option = "G", direction = 1,                            # Use Viridis for discrete categories
                        name = "total natural height loss (m)") +
  theme_minimal() +
  labs(title = "Global Map of IBWT Projects",
       subtitle = "Centroids categorized by total natural height loss (m)",
       x = "Longitude", y = "Latitude")

# Save plot
print(plot_centroids_with_naturalheightloss)
######################################################################################################################################
# Load the IBWT dataset
ibwt_dataset <- IBWT_megaproject_dataset  # Adjust the path
colnames(ibwt_dataset) <- tolower(colnames(ibwt_dataset))         # Standardize column names

# Merge centroids with IBWT dataset
centroids_with_heightgaininfra <- vector_data$centroids %>%
  left_join(ibwt_dataset, by = c("Name" = "transfer_name")) %>%    # Join datasets by matching Transfer_Name
  mutate(total_height_gain_with_infra = as.numeric(total_height_gain_with_infra)) 

boundaries <- c(0, 50, 100, 250, 500, 750, 1000, Inf)  # Adjust these thresholds as needed
labels <- c("0-50", "50-100", "100-250", "250-500", "500-750", "750-1,000", "1,000+")

# Categorize total_infrastructure_points_per_transfer into bins
centroids_with_heightgaininfra <- centroids_with_heightgaininfra %>%
  filter(!is.na(total_height_gain_with_infra)) %>%  # Remove NA values before categorization
  mutate(category = cut(total_height_gain_with_infra, breaks = boundaries, labels = labels, right = FALSE))  # Left-closed intervals


# World map with centroids categorized by infrastructure points
plot_centroids_with_heightgaininfra <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +                     # World map background
  geom_sf(data = centroids_with_heightgaininfra, 
          aes(geometry = geometry, color = category), 
          size = 4, alpha = 1) +                                # Centroids colored by categories
  scale_color_viridis_d(option = "C", direction = -1,                            # Use Viridis for discrete categories
                        name = "total height gain with infrastructure (m)") +
  theme_minimal() +
  labs(title = "Global Map of IBWT Projects",
       subtitle = "Centroids categorized by total height gain with infrastructure (m)",
       x = "Longitude", y = "Latitude")

# Save plot
print(plot_centroids_with_heightgaininfra)
######################################################################################################################################
# Load the IBWT dataset
ibwt_dataset <- IBWT_megaproject_dataset  # Adjust the path
colnames(ibwt_dataset) <- tolower(colnames(ibwt_dataset))  # Standardize column names

# Merge centroids with IBWT dataset
centroids_with_heightlossinfra <- vector_data$centroids %>%
  left_join(ibwt_dataset, by = c("Name" = "transfer_name")) %>%  
  mutate(total_height_loss_with_infra = as.numeric(total_height_loss_with_infra))  # Ensure numeric column

# Define boundaries and labels
boundaries <- c(-Inf, -1000, -750, -500, -250, -100, -50, 0)  
labels <- c("-1,000+", "-1,000 - -750", "-750 - -500", "-500 - -250", "-250 - -100", "-100 - -50", "-50 - 0")

# Filter out NA values before categorization
centroids_with_heightlossinfra <- centroids_with_heightlossinfra %>%
  filter(!is.na(total_height_loss_with_infra)) %>%  # Remove NA values
  mutate(category = cut(total_height_loss_with_infra, 
                        breaks = boundaries, 
                        labels = labels, 
                        right = FALSE))  # Left-closed intervals

# Ensure no NA values remain in the category column
centroids_with_heightlossinfra <- centroids_with_heightlossinfra %>%
  filter(!is.na(category))  

# World map with centroids categorized by height loss with infrastructure
plot_centroids_with_heightlossinfra <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +                     
  geom_sf(data = centroids_with_heightlossinfra, 
          aes(geometry = geometry, color = category), 
          size = 4, alpha = 1) +                                
  scale_color_viridis_d(option = "G", direction = 1,  
                        name = "Total height loss with infrastructure (m)",
                        na.translate = FALSE) +  # Ensure NA is not shown in legend
  theme_minimal() +
  labs(title = "Global Map of IBWT Projects",
       subtitle = "Centroids categorized by total height loss with infrastructure (m)",
       x = "Longitude", y = "Latitude")

# Save plot
print(plot_centroids_with_heightlossinfra)
######################################################################################################################################
composite_figure2 <- (plot_centroids_with_naturalheightgain + plot_centroids_with_naturalheightloss) / (plot_centroids_with_heightgaininfra + plot_centroids_with_heightlossinfra)
print(composite_figure2)
######################################################################################################################################
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the IBWT dataset
ibwt_dataset <- IBWT_megaproject_dataset  
colnames(ibwt_dataset) <- tolower(colnames(ibwt_dataset))  

# Merge centroids with IBWT dataset
centroids_with_data <- vector_data$centroids %>%
  left_join(ibwt_dataset, by = c("Name" = "transfer_name")) %>%  
  mutate(
    total_natural_height_gain = as.numeric(total_natural_height_gain),
    total_natural_height_loss = as.numeric(total_natural_height_loss),
    total_height_gain_with_infra = as.numeric(total_height_gain_with_infra),
    total_height_loss_with_infra = as.numeric(total_height_loss_with_infra)
  )

# Group data by country and summarize gains & losses
country_summary <- centroids_with_data %>%
  group_by(country) %>%
  summarise(
    natural_gain = sum(total_natural_height_gain, na.rm = TRUE),
    natural_loss = -sum(total_natural_height_loss, na.rm = TRUE),  # Convert to positive for plotting
    infra_gain = sum(total_height_gain_with_infra, na.rm = TRUE),
    infra_loss = -sum(total_height_loss_with_infra, na.rm = TRUE)  # Convert to positive for plotting
  ) %>%
  pivot_longer(cols = c(natural_gain, natural_loss, infra_gain, infra_loss), 
               names_to = "type", values_to = "height")

# Assign colors
country_summary$type <- factor(country_summary$type, 
                               levels = c("natural_gain", "natural_loss", "infra_gain", "infra_loss"),
                               labels = c("Natural Gain", "Natural Loss", "Infra Gain", "Infra Loss"))

# Plot 1: Natural Elevation Gain & Loss
plot_natural <- ggplot(filter(country_summary, type %in% c("Natural Gain", "Natural Loss")), 
                       aes(x = reorder(country, height), y = height, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Natural Gain" = "blue", "Natural Loss" = "red")) +
  labs(title = "Natural Elevation Gain & Loss per Country",
       x = "Country", y = "Height Change (m)", fill = "Type") +
  coord_flip() +
  theme_minimal()

# Plot 2: Infrastructure Elevation Gain & Loss
plot_infra <- ggplot(filter(country_summary, type %in% c("Infra Gain", "Infra Loss")), 
                     aes(x = reorder(country, height), y = height, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Infra Gain" = "green", "Infra Loss" = "orange")) +
  labs(title = "Infrastructure Elevation Gain & Loss per Country",
       x = "Country", y = "Height Change (m)", fill = "Type") +
  coord_flip() +
  theme_minimal()

# Display plots
composite_figure3 <- (plot_natural + plot_infra)
print(plot_natural)
print(plot_infra)
print(composite_figure3)
######################################################################################################################################
# Define the datasets corresponding to each resolution
Spain_exceldata <- SPA_TASEGWT2_elevation   # ArcGIS
Spain_downsample <- NISSUE                  # Downsample factor 5
Spain_normal <- Natural_Spain_Tagus_Segura_Transfer  # Downsample factor 1

# Function to calculate elevation gain and loss
calculate_elevation_change <- function(elevation_data, column_name) {
  elevations <- elevation_data[[column_name]]  # Extract elevation values
  elevation_diff <- diff(elevations, na.rm = TRUE)  # Compute differences between consecutive points
  elevation_gain <- sum(elevation_diff[elevation_diff > 0], na.rm = TRUE)  # Sum of positive differences
  elevation_loss <- sum(elevation_diff[elevation_diff < 0], na.rm = TRUE)  # Sum of negative differences
  return(c(elevation_gain, elevation_loss))
}

# Compute elevation gain and loss for each dataset
arcgis_changes <- calculate_elevation_change(Spain_exceldata, "Z")   # Adjust column name if needed
downsample5_changes <- calculate_elevation_change(Spain_downsample, "elevation")
downsample1_changes <- calculate_elevation_change(Spain_normal, "elevation")

# Combine results into a single data frame
data_combined <- data.frame(
  resolution = c("ArcGIS", "Downsample factor 5", "Downsample factor 1"),
  elevation_gain = c(arcgis_changes[1], downsample5_changes[1], downsample1_changes[1]),
  elevation_loss = c(arcgis_changes[2], downsample5_changes[2], downsample1_changes[2])
)

# Print the combined dataset
print(data_combined)

# Reorder factor levels so "Downsample factor 1" appears last
data_combined$resolution <- factor(data_combined$resolution, 
                                   levels = c("ArcGIS", "Downsample factor 5", "Downsample factor 1"))

# Melt data for ggplot
data_long <- melt(data_combined, id.vars = "resolution", variable.name = "type", value.name = "elevation")

# Plot
ggplot(data_long, aes(x = resolution, y = elevation, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +  # Grouped bars
  scale_fill_manual(values = c("elevation_gain" = "darkgreen", "elevation_loss" = "red")) +  # Custom colors
  theme_minimal() +
  labs(
    title = "Elevation Gain and Loss by Resolution",
    x = "Resolution Type",
    y = "Elevation (m)",
    fill = "Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
######################################################################################################################################
# Prepare the data (filter for non-NA values of energy consumption and distance)
plot_naturalenergyvsdistance <- IBWT_megaproject_dataset %>%
  filter(!is.na(Natural_energy_consumption) & !is.na(total_distance_natural_profile_in_km))

# Create the scatter plot for Energy Consumption vs Distance
plot_naturalenergyvsdistance <- ggplot(plot_naturalenergyvsdistance, aes(x = total_distance_natural_profile_in_km, y = Natural_energy_consumption, color = country)) +
  geom_point(size = 3) +
  labs(
    title = "Natural Energy Consumption vs Distance",
    x = "Distance",
    y = "Natural Energy Consumption (kWh)") +
  theme_minimal()

# Prepare the data (filter for non-NA values of energy consumption and height gain)
plot_naturalenergyvsheightgain <- IBWT_megaproject_dataset %>%
  filter(!is.na(Natural_energy_consumption) & !is.na(total_natural_height_gain))

# Create the scatter plot for Energy Consumption vs height gain
plot_naturalenergyvsheightgain <- ggplot(plot_naturalenergyvsheightgain, aes(x = total_natural_height_gain, y = Natural_energy_consumption, color = country)) +
  geom_point(size = 3) +
  labs(
    title = "Natural Energy Consumption vs height gain",
    x = "height gain",
    y = "Natural Energy Consumption (kWh)") +
  theme_minimal()
  
# Prepare the data (filter for non-NA values of energy consumption and water_discharge_level)
plot_naturalenergyvswaterdischarge <- IBWT_megaproject_dataset %>%
  filter(!is.na(Natural_energy_consumption) & !is.na(water_discharge_level))

# Create the scatter plot for Energy Consumption vs water_discharge_level
plot_naturalenergyvswaterdischarge <- ggplot(plot_naturalenergyvswaterdischarge, aes(x = water_discharge_level, y = Natural_energy_consumption, color = country)) +
  geom_point(size = 3) +
  labs(
    title = "Natural Energy Consumption vs water_discharge_level ",
    x = "water_discharge_level",
    y = "Natural Energy Consumption (kWh)") +
  theme_minimal()
 
######################################################################################################################################

# Prepare the data (filter for non-NA values of energy consumption and distance)
plot_infraenergyvsdistance <- IBWT_megaproject_dataset %>%
  filter(!is.na(total_energy_consumption_with_infra) & !is.na(total_distance_in_km_with_infra))

# Create the scatter plot for Energy Consumption vs Distance
plot_infraenergyvsdistance <- ggplot(plot_infraenergyvsdistance, aes(x = total_distance_in_km_with_infra, y = total_energy_consumption_with_infra, color = country)) +
  geom_point(size = 3) +
  labs(
    title = "Energy Consumption vs Distance",
    x = "Distance",
    y = "Energy Consumption (kWh)",
    color = "Country Name"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "right") +
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.box = "vertical") +
  theme(legend.box.just = "center") +
  theme(legend.key.height = unit(0.5, "cm")) +
  theme(legend.box.margin = margin(0, 0, 0, 15))

# Prepare the data (filter for non-NA values of energy consumption and height gain)
plot_infraenergyvsheightgain <- IBWT_megaproject_dataset %>%
  filter(!is.na(total_energy_consumption_with_infra) & !is.na(total_height_gain_with_infra))

# Create the scatter plot for Energy Consumption vs height gain
plot_infraenergyvsheightgain <- ggplot(plot_infraenergyvsheightgain, aes(x = total_height_gain_with_infra, y = total_energy_consumption_with_infra, color = country)) +
  geom_point(size = 3) +
  labs(
    title = "Energy Consumption vs height gain",
    x = "height gain",
    y = "Energy Consumption (kWh)",
    color = "Country Name"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "right") +
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.box = "vertical") +
  theme(legend.box.just = "center") +
  theme(legend.key.height = unit(0.5, "cm")) +
  theme(legend.box.margin = margin(0, 0, 0, 15))

# Prepare the data (filter for non-NA values of energy consumption and water_discharge_level)
plot_infraenergyvswaterdischarge <- IBWT_megaproject_dataset %>%
  filter(!is.na(total_energy_consumption_with_infra) & !is.na(water_discharge_level))

# Create the scatter plot for Energy Consumption vs water_discharge_level
plot_infraenergyvswaterdischarge <- ggplot(plot_infraenergyvswaterdischarge, aes(x = water_discharge_level, y = total_energy_consumption_with_infra, color = country)) +
  geom_point(size = 3) +
  labs(
    title = "Energy Consumption vs water_discharge_level ",
    x = "water_discharge_level",
    y = "Energy Consumption (kWh)",
    color = "Country Name"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "right") +
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.box = "vertical") +
  theme(legend.box.just = "center") +
  theme(legend.key.height = unit(0.5, "cm")) +
  theme(legend.box.margin = margin(0, 0, 0, 15))

# Remove legend from each plot
plot_naturalenergyvsdistance <- plot_naturalenergyvsdistance + theme(legend.position = "none")
plot_naturalenergyvsheightgain <- plot_naturalenergyvsheightgain + theme(legend.position = "none")
plot_naturalenergyvswaterdischarge <- plot_naturalenergyvswaterdischarge + theme(legend.position = "none")

composite_figure4 <- (plot_naturalenergyvsdistance + plot_infraenergyvsdistance) / (plot_naturalenergyvsheightgain + plot_infraenergyvsheightgain) / (plot_naturalenergyvswaterdischarge + plot_infraenergyvswaterdischarge)
print(composite_figure4)
######################################################################################################################################
# Load the IBWT dataset
ibwt_dataset <- IBWT_megaproject_dataset  # Adjust the path
colnames(ibwt_dataset) <- tolower(colnames(ibwt_dataset))         # Standardize column names

# Merge centroids with IBWT dataset
centroids_with_energyconsumption <- vector_data$centroids %>%
  left_join(ibwt_dataset, by = c("Name" = "transfer_name")) %>%    # Join datasets by matching Transfer_Name
  mutate(Natural_energy_consumption = as.numeric(Natural_energy_consumption)) 

boundaries <- c(0, 5000, 10000, 25000, 50000, 75000, 100000, Inf)  # Adjust these thresholds as needed
labels <- c("0-5,000", "5,000-10,000", "10,000-25,000", "25,000-50,000", "50,000-75,000", "75,000-100,000", "100,000+")

# Categorize `Natural_energy_consumption` into bins (use the fixed boundary values)
centroids_with_energyconsumption <- centroids_with_energyconsumption %>%
  filter(!is.na(Natural_energy_consumption)) %>%
  mutate(category = cut(Natural_energy_consumption, 
                        breaks = boundaries, 
                        labels = labels, 
                        right = FALSE))

# World map with centroids categorized by energy consumption categories
plot_centroids_with_energyconsumption <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +                     # World map background
  geom_sf(data = centroids_with_energyconsumption, 
          aes(geometry = geometry, color = category), 
          size = 4, alpha = 1) +                                # Centroids colored by categories
  scale_color_viridis_d(option = "H", direction = -1,                            # Use discrete Viridis scale
                        name = "Energy Consumption Category (kWh)") +
  theme_minimal() +
  labs(title = "Global Map of IBWT Projects",
       subtitle = "Centroids categorized by total Natural energy consumption (kWh)",
       x = "Longitude", y = "Latitude")

# Save plot
print(plot_centroids_with_energyconsumption)
######################################################################################################################################
# Load the IBWT dataset
ibwt_dataset <- IBWT_megaproject_dataset  # Adjust the path
colnames(ibwt_dataset) <- tolower(colnames(ibwt_dataset))         # Standardize column names

# Merge centroids with IBWT dataset
centroids_with_infraenergyconsumption <- vector_data$centroids %>%
  left_join(ibwt_dataset, by = c("Name" = "transfer_name")) %>%    # Join datasets by matching Transfer_Name
  mutate(total_energy_consumption_with_infra = as.numeric(total_energy_consumption_with_infra)) 

boundaries <- c(0, 50, 100, 250, 500, 750, 1000, Inf)  # Adjust these thresholds as needed
labels <- c("0-50", "50-100", "100-250", "250-500", "500-750", "750-1,000", "1,000+")

# Categorize `total_energy_consumption_with_infra` into bins (use the fixed boundary values)
centroids_with_infraenergyconsumption <- centroids_with_infraenergyconsumption %>%
  filter(!is.na(total_energy_consumption_with_infra)) %>%
  mutate(category = cut(total_energy_consumption_with_infra, 
                        breaks = boundaries, 
                        labels = labels, 
                        right = FALSE))

# World map with centroids categorized by energy consumption categories
plot_centroids_with_infraenergyconsumption <- ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +                     # World map background
  geom_sf(data = centroids_with_infraenergyconsumption, 
          aes(geometry = geometry, color = category), 
          size = 4, alpha = 1) +                                # Centroids colored by categories
  scale_color_viridis_d(option = "H", direction = -1,                            # Use discrete Viridis scale
                        name = "Energy Consumption Category (kWh)") +
  theme_minimal() +
  labs(title = "Global Map of IBWT Projects",
       subtitle = "Centroids categorized by total energy consumption (kWh)",
       x = "Longitude", y = "Latitude")

# Save plot
print(plot_centroids_with_infraenergyconsumption)

Composite_figure5 <- (plot_centroids_with_energyconsumption) / (plot_centroids_with_infraenergyconsumption)
print(Composite_figure5)
 