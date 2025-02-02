generate_height_profile <- function(dem_short, vector_short, downsample_factor = 1) {
  # Clear unused memory
  gc()
  
  # Find DEM and vector paths
  files <- find_files(dem_short, vector_short)
  dem_path <- files$dem_path
  vector_path <- files$vector_path
  
  # Check if the files exist
  if (length(dem_path) == 0) {
    stop("No DEM file found with this name.")
  }
  if (length(vector_path) == 0) {
    stop("No vector file found with this name.")
  }
  
  # Load DEM raster and vector line
  dem_raster <- rast(dem_path)
  vector_line <- tryCatch({
    st_read(vector_path)
  }, error = function(e) {
    stop("Error loading vector file: ", e$message)
  })
  
  # Ensure both layers have the same CRS
  if (st_crs(vector_line) != crs(dem_raster)) {
    vector_line <- st_transform(vector_line, crs(dem_raster))
  }
  
  # Check if the vector line contains data
  if (nrow(vector_line) == 0) {
    stop("Vector line has no data or could not be loaded correctly.")
  }
  
  # Crop the DEM to the vector's bounding box and downsample
  dem_cropped <- tryCatch({
    crop(dem_raster, st_bbox(vector_line))
  }, error = function(e) {
    stop("Error during DEM cropping: ", e$message)
  })
  dem_downsampled <- aggregate(dem_cropped, fact = downsample_factor)  # Downsample by given factor
  
  # Split long vector lines into smaller segments
  vector_segments <- st_segmentize(vector_line, dfMaxLength = 500)  # Adjust dfMaxLength if needed (length of segment 500 meters)
  # smaller length --> more points and more detail
  # larger length --> fewer points and smoother profiles
  
  # Initialize a list to store extracted elevation data
  elevation_data <- list()
  
  # Extract elevation data segment-by-segment
  for (i in seq_along(vector_segments)) {
    try({
      segment <- vector_segments[i, ]
      segment_data <- terra::extract(dem_downsampled, vect(segment), along = TRUE)
      elevation_data[[i]] <- segment_data
    }, silent = TRUE)
  }
  
  # Combine elevation data from all segments
  extracted_data <- do.call(rbind, elevation_data)
  
  # Check column name for elevation
  elev_column <- if ("lyr.1" %in% names(extracted_data)) "lyr.1" else names(extracted_data)[2]
  
  # Create a data frame for the height profile
  profile_df <- data.frame(
    distance = -seq_len(nrow(extracted_data)),  # Sequence for distance (individual points)
    elevation = extracted_data[[elev_column]]  # Extract the height values
  )
  
  # Debug: check if profile exists and is correctly filled
  if (is.null(profile_df) || nrow(profile_df) == 0) {
    stop("profile_df was not correctly created.")
  }
  cat("profile_df successfully created with", nrow(profile_df), "rows.\n")
  
  
  # Plot the height profile
  plot_profile <- ggplot(profile_df, aes(x = distance)) +
    geom_line(aes(y = elevation), color = "blue", alpha = 0.5) +  # Original data
    labs(title = paste("Height profile of", vector_short), 
         x = "Distance (points)", 
         y = "Height (m)") +
    theme_minimal()
  
  print(plot_profile)
  
  # Return profile data frame for further calculations
  print(profile_df)
  # Save profile_df as a global variable for later use
  assign(".Last_profile_df", profile_df, envir = .GlobalEnv)
}




# Example call
# Uncomment the following line to test the function
NISSUE <- generate_height_profile("spain", "Spain_Tagus_Segura_Transfer", downsample_factor = 5)
NISSUE1 <- generate_height_profile("Australia_East", "Australia_Snowy_River_Scheme_Murray", downsample_factor = 1)
NISSUE2 <- generate_height_profile("Australia_East", "Australia_Snowy_River_Scheme_Murrumbidgee", downsample_factor = 1)
NISSUE3 <- generate_height_profile("Australia_West", "Australia_Goldfields_Water_Supply_Scheme", downsample_factor = 1)
#NISSUE4 <- generate_height_profile("Canada_Labrador", "Canada_Churchill_Falls_Kanairktok", downsample_factor = 1)
#NISSUE5 <- generate_height_profile("Canada_Labrador", "Canada_Churchill_Falls_Naskaupi", downsample_factor = 1)
#NISSUE6 <- generate_height_profile("Canada_Manitoba", "Canada_Churchill_River_Diversion", downsample_factor = 1)
NISSUE7 <- generate_height_profile("Canada_Quebec", "Canada_James_Bay_Project_Eastmain_Diversion", downsample_factor = 1)
#NISSUE8 <- generate_height_profile("Canada_Quebec", "Canada_James_Bay_Project_LaForge_Diversion", downsample_factor = 1)
#NISSUE9 <- generate_height_profile("Canada_Ontario", "Canada_Long_Lake_Diversion_Kenogami", downsample_factor = 1)
#NISSUE10 <- generate_height_profile("Canada_Ontario", "Canada_Long_Lake_Diversion_Ogoki", downsample_factor = 1)
NISSUE11 <- generate_height_profile("Canada_BC", "Canada_Nechako_Kemano_Kitimat", downsample_factor = 1)
#NISSUE12 <- generate_height_profile("Canada_Ontario", "Canada_Saint_Joseph_Lake_Diversion", downsample_factor = 1)
NISSUE13 <- generate_height_profile("Chile", "Chile_Teno_Chimbarongo_Canal", downsample_factor = 1)
#NISSUE14 <- generate_height_profile("China_West", "China_Irtysh_Karamay_Urumqi_Canal_East", downsample_factor = 1)
#NISSUE15 <- generate_height_profile("China_West", "China_Irtysh_Karamay_Urumqi_Canal_West", downsample_factor = 1)
#NISSUE16 <- generate_height_profile("China_East", "China_South_North_Transfer_Central_Route", downsample_factor = 1)
#NISSUE17 <- generate_height_profile("China_East", "China_South_North_Transfer_Eastern_Route", downsample_factor = 1)
#NISSUE18 <- generate_height_profile("China_West", "China_Tarim_River_Restoration_Project", downsample_factor = 1)
#NISSUE19 <- generate_height_profile("India_North", "India_Indira_Gandhi_Canal", downsample_factor = 1)
#NISSUE20 <- generate_height_profile("India_South", "India_Periyar_Vaigai_Irrigation_Project", downsample_factor = 1)
#NISSUE21 <- generate_height_profile("India_South", "India_Telugu_Ganga_Project", downsample_factor = 1)
#NISSUE22 <- generate_height_profile("Israel", "Israel_National_Water_Carrier", downsample_factor = 1)
#NISSUE23 <- generate_height_profile("Kazakhstan", "Kazakhstan_Irtysh_Karaganda_Canal", downsample_factor = 1)
#NISSUE24 <- generate_height_profile("Libya", "Libya_Great_Manmade_River_Phase_FOUR", downsample_factor = 1)
#NISSUE25 <- generate_height_profile("Libya", "Libya_Great_Manmade_River_Phase_ONE", downsample_factor = 1)
#NISSUE26 <- generate_height_profile("Libya", "Libya_Great_Manmade_River_Phase_THREE", downsample_factor = 1)
#NISSUE27 <- generate_height_profile("Libya", "Libya_Great_Manmade_River_Phase_TWO", downsample_factor = 1)
#NISSUE28 <- generate_height_profile("Mexico", "Mexico_Cutzamala_reproject_system", downsample_factor = 1)
#NISSUE29 <- generate_height_profile("Mexico", "Mexico_Cutzamala_system", downsample_factor = 1)
#NISSUE30 <- generate_height_profile("SouthAfrica", "SA_Orange_River_Transfer_Scheme", downsample_factor = 1)
#NISSUE31 <- generate_height_profile("Ukraine", "Ukraine_North_Crimean_Canal", downsample_factor = 1)
#NISSUE32 <- generate_height_profile("USA_CA_AZ", "USA_All_American_Canal", downsample_factor = 1)
#NISSUE33 <- generate_height_profile("USA_CA_AZ", "USA_California_State_Water_Project", downsample_factor = 1)
#NISSUE34 <- generate_height_profile("USA_CA_AZ", "USA_California_State_Water_Coastal_Branch_Project", downsample_factor = 1)
#NISSUE35 <- generate_height_profile("USA_CA_AZ", "USA_Central_Arizona_Project", downsample_factor = 1)
#NISSUE36 <- generate_height_profile("USA_CA_AZ", "USA_Central_Valley_Project_North", downsample_factor = 1)
#NISSUE37 <- generate_height_profile("USA_CA_AZ", "USA_Central_Valley_Project_South", downsample_factor = 1)
#NISSUE38 <- generate_height_profile("USA_CA_AZ", "USA_Colorado_River_Aqueduct", downsample_factor = 1)
#NISSUE39 <- generate_height_profile("USA_CA_AZ", "USA_Los_Angeles_Aqueduct_First", downsample_factor = 1)
#NISSUE40 <- generate_height_profile("USA_CA_AZ", "USA_Los_Angeles_Aqueduct_Second", downsample_factor = 1)
#NISSUE41 <- generate_height_profile("USA_NY", "USA_Delaware_Aqueduct_Bypass_Tunnel", downsample_factor = 1)
NISSUE42 <- generate_height_profile("USA_IL", "USA_Lake_Michigan_Chicago_Diversion", downsample_factor = 1)

#write.table(NISSUE, file = "Rstudio/output/Natural_Spain_Tagus_Segura_Transfer.txt", sep = "\t", row.names = FALSE)
write.table(NISSUE1, file = "Rstudio/output/Natural_Australia_Snowy_River_Scheme_Murray.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE2, file = "Rstudio/output/Natural_Australia_Snowy_River_Scheme_Murrumbidgee.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE3, file = "Rstudio/output/Natural_Australia_Goldfields_Water_Supply_Scheme.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE4, file = "Rstudio/output/Natural_Canada_Churchill_Falls_Kanairktok.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE5, file = "Rstudio/output/Natural_Canada_Churchill_Falls_Naskaupi.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE6, file = "Rstudio/output/Natural_Canada_Churchill_River_Diversion.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE7, file = "Rstudio/output/Natural_Canada_James_Bay_Project_Eastmain_Diversion.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE8, file = "Rstudio/output/Natural_Canada_James_Bay_Project_LaForge_Diversion.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE9, file = "Rstudio/output/Natural_Canada_Long_Lake_Diversion_Kenogami.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE10, file = "Rstudio/output/Natural_Canada_Long_Lake_Diversion_Ogoki.txt", sep = "\t", row.names = FALSE)
write.table(NISSUE11, file = "Rstudio/output/Natural_Canada_Nechako_Kemano_Kitimat.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE12, file = "Rstudio/output/Natural_Canada_Saint_Joseph_Lake_Diversion.txt", sep = "\t", row.names = FALSE)
write.table(NISSUE13, file = "Rstudio/output/Natural_Chile_Teno_Chimbarongo_Canal.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE14, file = "Rstudio/output/Natural_China_Irtysh_Karamay_Urumqi_Canal_East.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE15, file = "Rstudio/output/Natural_China_Irtysh_Karamay_Urumqi_Canal_West.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE16, file = "Rstudio/output/Natural_China_South_North_Transfer_Central_Route.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE17, file = "Rstudio/output/Natural_China_South_North_Transfer_Eastern_Route.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE18, file = "Rstudio/output/Natural_China_Tarim_River_Restoration_Project.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE19, file = "Rstudio/output/Natural_India_Indira_Gandhi_Canal.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE20, file = "Rstudio/output/Natural_India_Periyar_Vaigai_Irrigation_Project.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE21, file = "Rstudio/output/Natural_India_Telugu_Ganga_Project.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE22, file = "Rstudio/output/Natural_Israel_National_Water_Carrier.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE23, file = "Rstudio/output/Natural_Kazakhstan_Irtysh_Karaganda_Canal.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE24, file = "Rstudio/output/Natural_Libya_Great_Manmade_River_Phase_FOUR.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE25, file = "Rstudio/output/Natural_Libya_Great_Manmade_River_Phase_ONE.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE26, file = "Rstudio/output/Natural_Libya_Great_Manmade_River_Phase_THREE.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE27, file = "Rstudio/output/Natural_Libya_Great_Manmade_River_Phase_TWO.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE28, file = "Rstudio/output/Natural_Mexico_Cutzamala_reproject_system.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE29, file = "Rstudio/output/Natural_Mexico_Cutzamala_system.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE30, file = "Rstudio/output/Natural_SA_Orange_River_Transfer_Scheme.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE31, file = "Rstudio/output/Natural_Ukraine_North_Crimean_Canal.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE32, file = "Rstudio/output/Natural_USA_All_American_Canal.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE33, file = "Rstudio/output/Natural_USA_California_State_Water_Project.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE34, file = "Rstudio/output/Natural_USA_California_State_Water_Coastal_Branch_Project.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE35, file = "Rstudio/output/Natural_USA_Central_Arizona_Project.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE36, file = "Rstudio/output/Natural_USA_Central_Valley_Project_North.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE37, file = "Rstudio/output/Natural_USA_Central_Valley_Project_South.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE38, file = "Rstudio/output/Natural_USA_Colorado_River_Aqueduct.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE39, file = "Rstudio/output/Natural_USA_Los_Angeles_Aqueduct_First.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE40, file = "Rstudio/output/Natural_USA_Los_Angeles_Aqueduct_Second.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE41, file = "Rstudio/output/Natural_USA_Delaware_Aqueduct_Bypass_Tunnel.txt", sep = "\t", row.names = FALSE)
#write.table(NISSUE42, file = "Rstudio/output/Natural_USA_Lake_Michigan_Chicago_Diversion.txt", sep = "\t", row.names = FALSE)
