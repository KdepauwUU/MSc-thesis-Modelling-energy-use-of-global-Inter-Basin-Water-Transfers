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

# R-script to generate automatically a new database, append water discharge levels and calculate energy consumption

# Load required libraries
library(sf)
library(terra)
library(ggplot2)

#######################################################################################################################

# Bulk transfer database function
bulk_generate_transfer_database <- function(downsample_factor = 1) {
  # Initialize an empty list to store transfer data entries
  transfer_database <- list()
  
  # Load DEM and vector files
  ASTER_DEM_folder <- "C:/users/2816954/OneDrive - Universiteit Utrecht/THESIS/data/ASTER_dem"
  Vector_folder <- "C:/users/2816954/OneDrive - Universiteit Utrecht/THESIS/data/WTMP_vectors_modified"
  
  # Generate lists of all DEM and vector files
  ASTER_filelist <- list.files(path = ASTER_DEM_folder, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  names(ASTER_filelist) <- sub("^ASTER_", "", sub("\\.tif$", "", basename(ASTER_filelist)))
  
  Vector_filelist <- list.files(path = Vector_folder, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  names(Vector_filelist) <- sub("\\.shp$", "", basename(Vector_filelist))
  
  # Loop through each vector file
  for (vector_short in names(Vector_filelist)) {
    cat("Processing vector:", vector_short, "\n")
    
    # Load the vector data
    vector_data <- st_read(Vector_filelist[vector_short])
    
    # Check if the vector has a CRS
    if (is.na(st_crs(vector_data))) {
      cat("Skipping vector:", vector_short, "as it has no CRS.\n")
      next
    }
    
    # Loop through each DEM file
    for (dem_short in names(ASTER_filelist)) {
      cat("Trying DEM:", dem_short, "\n")
      
      # Generate the height profile
      tryCatch({
        profile_df <- generate_height_profile(dem_short, vector_short)
        profile_infra_df <- IBWT_height_profile(dem_short, vector_short)
        # Calculate cumulative height gain and loss
        result <- calculate_cumulative_height_gain_loss(profile_df)
        
        # Create a data frame for this transfer
        transfer_entry <- data.frame(
          Transfer_Name = vector_short,
          Country_region = dem_short,
          Total_elevation_points_per_Transfer  = nrow(profile_df),  # Total points in profile
          Total_Height_Gain = result$cumulative_height_gain,
          Total_Height_Loss = result$cumulative_height_loss
        )
        
        # Add the entry to the database list
        transfer_database[[paste(dem_short, vector_short, sep = "_")]] <- transfer_entry
        
        cat("Successfully added transfer for", vector_short, "with DEM:", dem_short, "\n")
        
      }, error = function(e) {
        cat("Error while processing DEM:", dem_short, "and vector:", vector_short, "-", e$message, "\n")
      })
    }
  }
  
  # Combine all data into one data frame
  if (length(transfer_database) > 0) {
    transfer_database_df2 <- do.call(rbind, transfer_database)
    rownames(transfer_database_df) <- NULL  # Remove row names
  } else {
    transfer_database_df <- data.frame()  # Return an empty data frame if no matches were found
  }
  
  return(transfer_database_df2)
}

# Run the function to generate the database
all_transfers_df2 <- bulk_generate_transfer_database(downsample_factor = 5)

# Check the number of transfers added
print(paste("Total transfers added:", nrow(all_transfers_df)))
print(all_transfers_df)  # Print the resulting transfer database
warnings()

# Converting total distance of transfers (in points) to total distance in km in database
{
# Calculate downsampled resolution
original_resolution <- 30  # Original DEM resolution in meters
downsample_factor <- 1 # downsample factor used in generate_height_profile function
downsampled_resolution <- original_resolution * downsample_factor  # After down sampling

# Calculate the number of points per segment
dfMaxLength <- 500 # segmentation factor used in generate_height_profile function
points_per_segment <- dfMaxLength / downsampled_resolution

# Total number of points in the profile
n_points <- all_transfers_df3$Total_elevation_points_per_Transfer

# Calculate total distance in meters
total_distance_meters <- n_points * (dfMaxLength / points_per_segment)

# Convert to kilometers
total_distance_km <- total_distance_meters / 1000
}
# Add the total distance in the transfer entry
all_transfers_df3$Total_Distance_in_km <- total_distance_km

# If Water_discharge_level is a vector of values, you can assign it directly:
all_transfers_df3$Water_discharge_level <- NA  # Initialize the column

# Assign water discharge level based on Transfer_Name using case_when
{
all_transfers_df3 <- all_transfers_df3 %>% 
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
all_transfers_df3$Water_discharge_level <- as.numeric(all_transfers_df3$Water_discharge_level)

# Extract country names from `Country_region`
{all_transfers_df3 <- all_transfers_df3 %>%
  mutate(
    Country = sub("_.*", "", Country_region),                  # Extract country name from `Country_region`
    Country_code = toupper(substr(Country, 1, 3))              # Use first three letters as abbreviation
  ) %>%
  select(Transfer_Name, Country, Country_region, Country_code, Total_elevation_points_per_Transfer , Total_Distance_in_km, Total_Height_Gain, Total_Height_Loss, Water_discharge_level)
}

#################################################################################################################
# Output the transfer entry
print(head(all_transfers_df1))

# Save database for further use
save(all_transfers_df3, file = "Rstudio/output/IBWT_newDatabase2")

###############################################################################################################

# function to calculate energy  consumption
# Define the function to calculate energy consumption
calculate_energy_consumption <- function(transfer_name, efficiency = 0.8, time_period_hours = 1) {
  # Constants
  g <- 9.81  # Gravitational acceleration (m/s^2)
  rho <- 1000  # Density of water (kg/m^3)
  delta_t <- time_period_hours * 3600  # Time period in seconds
  
  # Filter the specific transfer based on the name
  transfer <- all_transfers_df3 %>% filter(Transfer_Name == transfer_name)
  
  # Check if transfer exists in the data
  if (nrow(transfer) == 0) {
    stop("Transfer name not found in the database.")
  }
  
  # Convert Water_discharge_level and Total_Height_Gain to numeric if needed
  h <- as.numeric(transfer$Total_Height_Gain)
  Q <- as.numeric(transfer$Water_discharge_level)  # Discharge rate (m^3/s)
  
  # Check if h or Q is NA after conversion
  if (is.na(h) || is.na(Q)) {
    stop("Height gain or water discharge level is missing or non-numeric for the specified transfer.")
  }
  
  # Calculate energy consumption (in kWh)
  E <- ((g * h) * (rho * Q * delta_t)) / (3.6e9 * efficiency)
  
  # Round the result to 2 decimal places
  E <- round(E, 2)
  
  # Return the result
  return(E)
}

# Example usage
# Calculate energy for a specific transfer name, e.g., "Australia_Goldfields_Water_Supply_Scheme"
energy_consumption <- calculate_energy_consumption("Australia_Goldfields_Water_Supply_Scheme", efficiency = 0.85, time_period_hours = 1)
print(paste("Energy consumption for the transfer:", energy_consumption, "MWh"))

# Apply energy calculation to each row
all_transfers_df3 <- all_transfers_df3 %>%
  rowwise() %>%
  mutate(Energy_Consumption = calculate_energy_consumption(Transfer_Name, efficiency = 0.8, time_period_hours = 1))
#######################################################################################################################################
generate_height_profile_with_energy <- function(transfer_name, all_transfers_df, efficiency = 0.85, g = 9.81, rho = 1000) {
  
  # Retrieve the corresponding water discharge level and Country_region (dem_short)
  transfer_info <- all_transfers_df[all_transfers_df3$Transfer_Name == transfer_name, ]
  
  if (nrow(transfer_info) == 0) {
    stop("Transfer name not found in the dataset.")
  }
  
  # Extract the necessary values
  water_discharge_level <- transfer_info$Water_discharge_level  # m³/s
  dem_short <- transfer_info$Country_region  # Corresponding to dem_short
  total_distance_km <- transfer_info$Total_Distance_in_km  # Total distance in km
  
  # Now generate the height profile for the given transfer
  profile_df <- generate_height_profile(dem_short = dem_short, vector_short = transfer_name)  # Adjust function as needed
  
  # Check if the profile was successfully created and contains the necessary columns
  if (is.null(profile_df) || nrow(profile_df) == 0 || !all(c("elevation") %in% colnames(profile_df))) {
    stop("Failed to generate height profile for the transfer or missing necessary columns.")
  }
  
  # Filter out rows with NA values in 'elevatie' and 'height_gain' columns
  profile_df <- profile_df[!is.na(profile_df$elevation), ]
  
  # Calculate incremental height gains
  profile_df$height_gain <- c(0, diff(profile_df$elevation))
  profile_df$height_gain[profile_df$height_gain < 0] <- 0  # Only positive gains
  
  # Ensure that the 'height_gain' column is properly calculated and populated
  if (nrow(profile_df) == 0 || sum(is.na(profile_df$height_gain)) > 0) {
    stop("Error calculating height gain.")
  }
  
  # Calculate incremental energy consumption per segment (in MWh)
  profile_df$Energy_Consumption <- (g * profile_df$height_gain * rho * water_discharge_level * 3600) / (3.6e9 * efficiency)
  
  # Cumulative energy consumption
  profile_df$Cumulative_Energy <- cumsum(profile_df$Energy_Consumption)
  
  # Calculate total distance based on the length of profile
  profile_df$Distance_km <- seq(0, total_distance_km, length.out = nrow(profile_df))
  
  # Ensure the data is filtered to avoid NA values in Energy_Consumption
  profile_df <- profile_df[!is.na(profile_df$Energy_Consumption), ]
  
  # Find the maximum value of both height and cumulative energy for scaling purposes
  max_elevation <- max(profile_df$elevation, na.rm = TRUE)
  max_energy <- max(profile_df$Cumulative_Energy, na.rm = TRUE)
  
  # Identify points with maximum energy consumption
  max_energy_points <- profile_df %>% filter(Energy_Consumption == max(profile_df$Energy_Consumption, na.rm = TRUE))
  
  # Plot using ggplot2
  ggplot(profile_df, aes(x = Distance_km)) +
    # Plot height profile on the primary y-axis
    geom_line(aes(y = elevation), color = "blue", alpha = 0.6, size = 1) +
    
    # Plot cumulative energy consumption on the secondary y-axis (scaled)
    geom_line(aes(y = Cumulative_Energy * max_elevation / max_energy), 
              color = "green", size = 1, linetype = "dashed") +
    
    # Highlight maximum energy consumption points
    geom_point(data = max_energy_points, aes(y = Energy_Consumption * max_elevation / max_energy),
               color = "orange", size = 3, shape = 18) +
    geom_text(data = max_energy_points, aes(y = Energy_Consumption * max_elevation / max_energy, 
                                            label = round(Energy_Consumption, 2)),
              vjust = -1, color = "orange", size = 3) +
    
    # Labels and theme adjustments
    labs(title = paste("Height Profile with Cumulative Energy Consumption:", transfer_name),
         subtitle = paste("Water Discharge Level:", water_discharge_level, "m³/s"),
         x = "Distance (km)", 
         y = "Height (m)") +
    
    # Secondary y-axis for Cumulative Energy Consumption
    scale_y_continuous(
      sec.axis = sec_axis(~ . * max_energy / max_elevation, name = "Cumulative Energy (MWh)")
    ) +
    
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Example call to the function:
generate_height_profile_with_energy(transfer_name = "Spain_Tagus_Segura_Transfer", 
                                    all_transfers_df = all_transfers_df3)
