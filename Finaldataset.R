# MSc Research Modelling energy use of global Inter basin water Transfer Megaprojects
# Author: Karsten De Pauw
# student number: 2816954
# contact info: k.depauw@students.uu.nl

# 1st supervisor: Prof. Michelle van Vliet
# 2nd Supervisor: Michele Magni MSc

# MSc programme: Earth Surface and Water
# Faculty of Geosciences
# Department of Physical Geography
# Utrecht University

# This R-script is developed with the assistance of ChatGPT. The assistance included:
# - combining datasets for Inter-Basin Water transfers
# - Calculating and visualising elevation profiles and distances
# - Implementing interactive plots with Shiny app and ggplot

# R-script to generate a new database, append water discharge levels and calculate energy consumption
#####################################################################################################################################################
# Prepare Your Environment in R
#setwd(Z:/R studio - Thesis/Rstudio/output) #not necessary right now as already loaded
#Load required libraries
#library(dplyr) #for data manipulation
#library(readr) #for readin txt files

# Read and combine text files
IBWT_heightprofile_with_infra_folder <- "U:/R studio - Thesis/Rstudio/output/IBWT height profile with infrastructure"  # Path to IBWT infra files
IBWT_infra_fileslist <- list.files(path = IBWT_heightprofile_with_infra_folder, pattern = "\\.txt$", full.names = TRUE)
names(IBWT_infra_fileslist) <- sub("^IBWT_", "", sub("\\.txt$", "", basename(IBWT_infra_fileslist)))

IBWT_natural_heightprofile_folder <- "U:/R studio - Thesis/Rstudio/output/Natural Height profiles"  # Path to natural files
IBWT_natural_fileslist <- list.files(path = IBWT_natural_heightprofile_folder, pattern = "\\.txt$", full.names = TRUE)
names(IBWT_natural_fileslist) <- sub("^Natural_", "", sub("\\.txt$", "", basename(IBWT_natural_fileslist)))

# Constants for calculations
original_resolution <- 30  # Original DEM resolution in meters
downsample_factor <- 1     # Downsample factor
dfMaxLength <- 500         # Segmentation factor

# Initialize the dataset
IBWT_megaproject_dataset <- data.frame(
  Transfer_Name = NA,  # Empty character vector
  Country = NA,       # NA as character
  Country_region = NA,
  Country_code = NA,
  Water_discharge_level = NA, # NA as numeric
  Total_elevation_points_per_natural_transfer = NA,
  Total_distance_natural_profile_in_km = NA,
  Total_natural_height_gain = NA,
  Total_natural_height_loss = NA,
  Total_elevation_points_per_Transfer_with_infra = NA,
  Total_distance_in_km_with_infra = NA,
  Total_height_gain_with_infra = NA,
  Total_height_loss_with_infra = NA,
  Natural_energy_consumption = NA,
  Total_energy_consumption_with_infra = NA,
  stringsAsFactors = FALSE
)

# Process both natural and infra files based on Transfer_Name
all_transfer_names <- unique(c(names(IBWT_natural_fileslist), names(IBWT_infra_fileslist)))

for (transfer_name in all_transfer_names) {
  # Process natural file
  natural_file_path <- IBWT_natural_fileslist[transfer_name]
  if (!is.na(natural_file_path)) {
    natural_data <- read.table(natural_file_path, header = TRUE)
    natural_elevation_points <- nrow(natural_data)
    points_per_segment <- dfMaxLength / (original_resolution * downsample_factor)
    natural_total_distance_meters <- natural_elevation_points * (dfMaxLength / points_per_segment)
    natural_total_distance_km <- natural_total_distance_meters / 1000
  } else {
    natural_elevation_points <- NA
    natural_total_distance_km <- NA
  }
  
  # Process infra file
  infra_file_path <- IBWT_infra_fileslist[transfer_name]
  if (!is.na(infra_file_path)) {
    infra_data <- read.table(infra_file_path, header = TRUE)
    infra_elevation_points <- nrow(infra_data)
    points_per_segment <- dfMaxLength / (original_resolution * downsample_factor)
    infra_total_distance_meters <- infra_elevation_points * (dfMaxLength / points_per_segment)
    infra_total_distance_km <- infra_total_distance_meters / 1000
  } else {
    infra_elevation_points <- NA
    infra_total_distance_km <- NA
  }
  
  # Add a new row to the dataset
  IBWT_megaproject_dataset <- rbind(
    IBWT_megaproject_dataset,
    data.frame(
      Transfer_Name = transfer_name,
      Country = NA,
      Country_region = NA,
      Country_code = NA,
      Water_discharge_level = NA,
      Total_elevation_points_per_natural_transfer = natural_elevation_points,
      Total_distance_natural_profile_in_km = natural_total_distance_km,
      Total_natural_height_gain = NA,
      Total_natural_height_loss = NA,
      Total_elevation_points_per_Transfer_with_infra = infra_elevation_points,
      Total_distance_in_km_with_infra = infra_total_distance_km,
      Total_height_gain_with_infra = NA,
      Total_height_loss_with_infra = NA,
      Natural_energy_consumption = NA,
      Total_energy_consumption_with_infra = NA,
      stringsAsFactors = FALSE
    )
  )
}

# View the resulting dataset
print(IBWT_megaproject_dataset)
###################################################################################################################################################
# Assign water discharge level based on Transfer_Name using case_when
{
IBWT_megaproject_dataset <- IBWT_megaproject_dataset %>% 
  mutate(Water_discharge_level = case_when(
    Transfer_Name == "Australia_Goldfields_Water_Supply_Scheme" ~ "1.04",
    Transfer_Name == "Australia_Snowy_River_Scheme_Murray" ~ "33.68",
    Transfer_Name == "Australia_Snowy_River_Scheme_Murrumbidgee" ~ "31.90",
    Transfer_Name == "Canada_Churchill_Falls_Kanairktok" ~ "0",
    Transfer_Name == "Canada_Churchill_Falls_Naskaupi" ~ "0",
    Transfer_Name == "Canada_Churchill_River_Diversion" ~ "850.00",
    Transfer_Name == "Canada_James_Bay_Project_Eastmain_Diversion" ~ "1600",
    Transfer_Name == "Canada_James_Bay_Project_LaForge_Diversion" ~ "1260",
    Transfer_Name == "Canada_Long_Lake_Diversion_Kenogami" ~ "38.79408",
    Transfer_Name == "Canada_Long_Lake_Diversion_Ogoki" ~ "121.1961",
    Transfer_Name == "Canada_Nechako_Kemano_Kitimat" ~ "184.06",
    Transfer_Name == "Canada_Saint_Joseph_Lake_Diversion" ~ "86.64955",
    Transfer_Name == "Chile_Teno_Chimbarongo_Canal" ~ "65",
    Transfer_Name == "China_Irtysh_Karamay_Urumqi_Canal_East" ~ "25.35047",
    Transfer_Name == "China_Irtysh_Karamay_Urumqi_Canal_West" ~ "24.08295",
    Transfer_Name == "China_South_North_Transfer_Central_Route" ~ "405.975",
    Transfer_Name == "China_South_North_Transfer_Eastern_Route" ~ "242.8327329",
    Transfer_Name == "China_Tarim_River_Restoration_Project" ~ "24.07",
    Transfer_Name == "India_Indira_Gandhi_Canal" ~ "284.0837075",
    Transfer_Name == "India_Periyar_Vaigai_Irrigation_Project" ~ "40.75",
    Transfer_Name == "India_Telugu_Ganga_Project" ~ "31.54",
    Transfer_Name == "Israel_National_Water_Carrier" ~ "15.85",
    Transfer_Name == "Kazakhstan_Irtysh_Karaganda_Canal" ~ "75",
    Transfer_Name == "Libya_Great_Manmade_River_Phase_FOUR" ~ "21.88",
    Transfer_Name == "Libya_Great_Manmade_River_Phase_ONE" ~ "22.18",
    Transfer_Name == "Libya_Great_Manmade_River_Phase_THREE" ~ "21.88",
    Transfer_Name == "Libya_Great_Manmade_River_Phase_TWO" ~ "28.52",
    Transfer_Name == "Mexico_Cutzamala_reproject_system" ~ "14.9",
    Transfer_Name == "Mexico_Cutzamala_system" ~ "14.9",
    Transfer_Name == "SA_Orange_River_Transfer_Scheme" ~ "23.95",
    Transfer_Name == "Spain_Tagus_Segura_Transfer" ~ "16.16",
    Transfer_Name == "Ukraine_North_Crimean_Canal" ~ "20",
    Transfer_Name == "USA_All_American_Canal" ~ "2.73",
    Transfer_Name == "USA_California_State_Water_Coastal_Branch_Project" ~ "123.33",
    Transfer_Name == "USA_California_State_Water_Project" ~ "123.33",
    Transfer_Name == "USA_Central_Arizona_Project" ~ "31.13",
    Transfer_Name == "USA_Central_Valley_Project_North" ~ "272",
    Transfer_Name == "USA_Central_Valley_Project_South" ~ "272",
    Transfer_Name == "USA_Colorado_River_Aqueduct" ~ "45",
    Transfer_Name == "USA_Delaware_Aqueduct_Bypass_Tunnel" ~ "34.88",
    Transfer_Name == "USA_Lake_Michigan_Chicago_Diversion" ~ "90.6",
    Transfer_Name == "USA_Los_Angeles_Aqueduct_First" ~ "14.7",
    Transfer_Name == "USA_Los_Angeles_Aqueduct_Second" ~ "8.2"
  ))
}
IBWT_megaproject_dataset$Water_discharge_level <- as.numeric(IBWT_megaproject_dataset$Water_discharge_level)
#########################################################################################################################################################
# Remove duplicate columns resulting from a previous join
IBWT_megaproject_dataset <- IBWT_megaproject_dataset %>%
  select(-Country_region.y) %>%          # Remove the duplicate column if it exists
  rename(Country_region = Country_region.x)  # Rename the original column

# Update the Country_region column
IBWT_megaproject_dataset <- IBWT_megaproject_dataset %>%
  rowwise() %>%
  mutate(Country_region = ifelse(
    Transfer_Name %in% all_transfers_df3$Transfer_Name,
    all_transfers_df3$Country_region[match(Transfer_Name, all_transfers_df3$Transfer_Name)],
    Country_region
  )) %>%
  ungroup()
# Extract country names from `Country_region`
IBWT_megaproject_dataset <- IBWT_megaproject_dataset %>%
  mutate(
    Country = sub("_.*", "", Country_region),                          # Extract country name from `Country_region`
    Country_code = toupper(substr(Country, 1, 3))                      # Use first three letters as abbreviation
  ) %>%
  mutate(
    # Specifically handle Chile and modify its code to 'CHL'
    Country_code = ifelse(Country == "Chile", "CHL", Country_code)
  )
############################################################################################################
# Loop through each transfer in IBWT_megaproject_dataset
for (i in 1:nrow(IBWT_megaproject_dataset)) {
  
  transfer_name <- IBWT_megaproject_dataset$Transfer_Name[i]   # Extract transfer name from dataset
  
  # Find the corresponding natural height profile file
  infra_file_path <- file.path(IBWT_heightprofile_with_infra_folder, paste0("IBWT_", transfer_name, ".txt"))
  
  # Check if the file exists
  if (file.exists(infra_file_path)) {
    # Read the elevation data from the file
    file_data <- read.table(infra_file_path, header = TRUE)  # Modify this based on actual data format
    
    # Assuming elevation data is stored in a column named 'elevation' or similar
    elevations <- file_data$infra_elevation  # Adjust the column name as needed
    
    # Calculate elevation gain and loss
    elevation_diff <- c(NA, diff(elevations, na.rm = TRUE))  # Difference between consecutive elevation points
    total_height_gain <- sum(elevation_diff[elevation_diff > 0], na.rm = TRUE)  # Positive differences (gain)
    total_height_loss <- sum(elevation_diff[elevation_diff < 0], na.rm = TRUE)  # Negative differences (loss)
    
    # Update the dataset with the calculated values
    IBWT_megaproject_dataset$Total_height_gain_with_infra[i] <- total_height_gain
    IBWT_megaproject_dataset$Total_height_loss_with_infra[i] <- total_height_loss
  } else {
    # If no file found, fill in NAs for the calculated columns
    IBWT_megaproject_dataset$Total_height_gain_with_infra[i] <- NA
    IBWT_megaproject_dataset$Total_height_loss_with_infra[i] <- NA
  }
}
natural_profile <- NISSUE1 
file_data <- read(natural_profile, header = TRUE)
print(natural_profile)
elevations <- natural_profile$elevation  # Adjust the column name as needed

# Calculate elevation gain and loss
elevation_diff <- c(NA, diff(elevations, na.rm = TRUE))  # Difference between consecutive elevation points
total_height_gain <- sum(elevation_diff[elevation_diff > 0], na.rm = TRUE)  # Positive differences (gain)
total_height_loss <- sum(elevation_diff[elevation_diff < 0], na.rm = TRUE)  # Negative differences (loss)
print(total_height_gain)
print(elevation_diff)


plot_profile <- ggplot(file_data, aes(x = distance)) +
  geom_line(aes(y = elevation), color = "blue", alpha = 0.5) +  # Original data
  labs(title = paste("Height profile of", vector_short), 
       x = "Distance (points)", 
       y = "Height (m)") +
  theme_minimal()

print(plot_profile)
save(IBWT_megaproject_dataset, file = "Rstudio/output/IBWT_megaproject_dataset.csv")
############################################################################################################
# Remove the NA row from the dataset (created during initialization)
IBWT_megaproject_dataset <- IBWT_megaproject_dataset[!is.na(IBWT_megaproject_dataset$Transfer_Name), ]

# Load ggplot2
library(ggplot2)

# Filter and prepare the dataset
plot_data <- IBWT_megaproject_dataset %>%
  filter(!is.na(Total_distance_natural_profile_in_km)) %>%
  filter(!is.na(Country))  # Ensure Country is available for coloring

# Create the bar plot
ggplot(IBWT_megaproject_dataset, aes(x = Total_infrastructure_points_per_transfer, y = Transfer_Name, fill = Country)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Infrastructure points per Transfer",
    x = "Transfer Name",
    y = "Amount of infrastructure points",
    fill = "Country"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load ggplot2
library(ggplot2)

# Filter and prepare the dataset
plot_data <- IBWT_megaproject_dataset %>%
  filter(!is.na(Total_distance_natural_profile_in_km) & !is.na(Total_natural_height_gain))

# Create the scatter plot
ggplot(IBWT_megaproject_dataset, aes(x = Total_distance_in_km_with_infra, y = Total_infrastructure_points_per_transfer, label = Country)) +
  geom_point(aes(color = Country), size = 3) +
  theme_minimal() +
  labs(
    title = "Distance vs total infrastructure points per Country",
    x = "Distance (km)",
    y = "Amount of infrastructure points per Country",
    color = "Country name"
  )
############################################################################################################
calculate_energy_consumption <- function(h, Q, efficiency = 0.4, time_period_hours = 1) {
  g <- 9.81  # Gravitational acceleration (m/s^2)
  rho <- 1000  # Density of water (kg/m^3)
  delta_t <- time_period_hours * 3600  # Time period in seconds
  
  # Fix: Use any() instead of ||
  if (any(is.na(h)) || any(is.na(Q))) {
    return(NA)
  }
  
  E <- ((g * h) * (rho * Q * delta_t)) / (3.6e9 * efficiency)
  
  return(round(E, 2))
}

# Loop through each transfer
for (i in 1:nrow(IBWT_megaproject_dataset)) {
  
  transfer_name <- IBWT_megaproject_dataset$Transfer_Name[i]
  
  infra_file_path <- file.path(IBWT_heightprofile_with_infra_folder, paste0("IBWT_", transfer_name, ".txt"))
  
  if (file.exists(infra_file_path)) {
    file_data <- read.table(infra_file_path, header = TRUE)
    
    # Use single-row values instead of entire column
    h <- IBWT_megaproject_dataset$Total_height_gain_with_infra[i]
    Q <- IBWT_megaproject_dataset$Water_discharge_level[i]
    
    infra_energy_consumption <- calculate_energy_consumption(h, Q)
    
    IBWT_megaproject_dataset$Total_energy_consumption_with_infra[i] <- infra_energy_consumption
  } else {
    IBWT_megaproject_dataset$Total_energy_consumption_with_infra[i] <- NA
  }
}

############################################################################################################
# Prepare the data (filter for non-NA values of energy consumption and distance)
plot_data <- IBWT_megaproject_dataset %>%
  filter(!is.na(Natural_energy_consumption) & !is.na(Total_distance_natural_profile_in_km))

# Create the scatter plot for Energy Consumption vs Distance
# Create the scatter plot for Energy Consumption vs Distance
ggplot(plot_data, aes(x = Natural_energy_consumption, y = Total_natural_height_gain, color = Transfer_Name)) +
  geom_point(size = 3) +
  labs(
    title = "Energy Consumption vs Natural heigth gain per Transfer",
    x = "Energy Consumption (kWh)",
    y = "Natural height gain (m)",
    color = "Transfer Name"
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
############################################################################################################
IBWT_megaproject_dataset$Total_infrastructure_points_per_transfer <- NA

# Loop through each transfer in the dataset
for (i in 1:nrow(IBWT_megaproject_dataset)) {
  
  transfer_name <- IBWT_megaproject_dataset$Transfer_Name[i]  # Extract transfer name from dataset
  
  # Find the corresponding infrastructure file
  infra_file_path <- file.path(IBWT_heightprofile_with_infra_folder, paste0("IBWT_", transfer_name, ".txt"))
  
  # Check if the file exists
  if (file.exists(infra_file_path)) {
    # Read the infrastructure data file
    file_data <- read.table(infra_file_path, header = TRUE)  # Adjust parameters based on file format
    
    # Count rows where infra_id has a value (not NA)
    total_infra_points <- sum(!is.na(file_data$infra_id))  # Count rows with non-NA values in infra_id
    
    # Update the dataset with the calculated value
    IBWT_megaproject_dataset$Total_infrastructure_points_per_transfer[i] <- total_infra_points
  } else {
    # If no file found, fill in NA for the calculated column
    IBWT_megaproject_dataset$Total_infrastructure_points_per_transfer[i] <- NA
  }
}
############################################################################################################
ggplot(IBWT_megaproject_dataset, aes(x = Water_discharge_level)) +
  geom_point(aes(x = Total_distance_natural_profile_in_km, y = Total_natural_height_gain, color = Natural_energy_consumption, size = Water_discharge_level), alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.title.y.right = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray"),
        legend.position = "right") +
  labs(
    title = "Energy Consumption and Water discharge levels vs Total natural height gain and distance",
    subtitle = "Each point represents a transfer, with color indicating energy consumption and size indicating water discharge level (m3/s)",
    x = "Total distance (km)",
    y = "Total natural elevation gain (m)",
    color = "Energy consumption (kWh)",
    size = "Water Discharge Level (m3/s)"
  )
