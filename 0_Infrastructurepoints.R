##################################################################################################################################################################
# Function to find files for DEM and infrastructure points #### THIS WORKS!!
find_infra_files <- function(dem_short, infra_short) {
  # DEM folder and file search
  ASTER_DEM_folder <- "C:/users/karst/OneDrive - Universiteit Utrecht/THESIS/data/ASTER_dem"
  ASTER_filelist <- list.files(path = ASTER_DEM_folder, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  
  # Filter DEM files by name
  dem_path <- ASTER_filelist[grep(dem_short, basename(ASTER_filelist), ignore.case = TRUE)]
  
  # Infrastructure folder and file search
  Infra_folder <- "C:/users/karst/OneDrive - Universiteit Utrecht/THESIS/data/Infrastructure_vectors_categorized"
  Infra_filelist <- list.files(path = Infra_folder, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  # Filter infrastructure files by name
  infra_path <- Infra_filelist[grep(infra_short, basename(Infra_filelist), ignore.case = TRUE)]
  
  # Return file paths
  return(list(dem_path = dem_path, infra_path = infra_path))
}

# Function to generate scatterplot for elevation profile based on infrastructure points
generate_infra_scatterplot <- function(dem_short, infra_short) {
  # Find files
  files <- find_infra_files(dem_short, infra_short)
  dem_path <- files$dem_path
  infra_path <- files$infra_path
  
  # Ensure files exist
  if (length(dem_path) == 0) stop("No DEM file found with this name.")
  if (length(infra_path) == 0) stop("No infrastructure file found with this name.")
  
  # Load DEM raster and infrastructure points
  dem_raster <- rast(dem_path)
  infra_points <- tryCatch({
    st_read(infra_path)
  }, error = function(e) {
    stop("Error loading infrastructure file: ", e$message)
  })
  
  # Check geometry type
  if (!all(st_geometry_type(infra_points) %in% c("POINT", "MULTIPOINT"))) {
    stop("The infrastructure data does not contain points.")
  }
  
  # Ensure CRS alignment
  if (st_crs(infra_points) != crs(dem_raster)) {
    infra_points <- st_transform(infra_points, crs(dem_raster))
  }
  
  # Extract elevation data for points
  extracted_data <- terra::extract(dem_raster, vect(infra_points), along = FALSE)
  
  # Check for valid extraction
  if (is.null(extracted_data) || nrow(extracted_data) == 0 || all(is.na(extracted_data[[1]]))) {
    stop("No valid elevation data extracted for points.")
  }
  
  # Combine with original point data for coordinates
  coords <- st_coordinates(infra_points)
  profile_df <- data.frame(
    Id = seq_len(nrow(coords)),
    X = coords[, "X"],    # X coordinates (Longitude)
    Y = coords[, "Y"],    # Y coordinates (Latitude)
    elevation = extracted_data[[2]],  # Elevation values
    Category = infra_points$Category)
  
  # Debugging: ensure no NA elevations
  profile_df <- profile_df[!is.na(profile_df$elevation), ]
  if (nrow(profile_df) == 0) stop("No valid points remain after filtering.")
  
  # Generate scatterplot
  plot_profile <- ggplot(profile_df, aes(x = Id, y = elevation, color = as.factor(infra_points$Category))) +
    geom_point(size = 3, shape = 15) +  # Scatter points
    labs(
      title = paste("Infrastructure points of:", infra_short),
      x = "Longitude (X Coordinate)",
      y = "Latitude (Y Coordinate)",
      color = "Category"
    ) +
    theme_minimal() +
    scale_color_discrete()  # Ensure discrete color scale
  
  print(plot_profile)
  print(profile_df)
  # Return profile data frame for further analysis
  return(profile_df)
}

# Example usage
infra_elevation_profile <- generate_infra_scatterplot("Australia_East", "Infrastructure_Australia_Snowy_River_Scheme_Murrumbidgee")
################################################################################################################################################
plot_points_and_lines <- function(dem_short, vector_short, infra_short) {
  # Find files
  files <- find_files(dem_short, vector_short)
  infra_files <- find_infra_files(dem_short, infra_short)
  
  vector_path <- files$vector_path
  infra_path <- infra_files$infra_path
  
  # Ensure files exist
  if (length(vector_path) == 0) stop("No vector file found with this name.")
  if (length(infra_path) == 0) stop("No infrastructure file found with this name.")
  
  # Load vector lines and infrastructure points
  vector_line <- tryCatch({
    st_read(vector_path)
  }, error = function(e) {
    stop("Error loading vector file: ", e$message)
  })
  
  infra_points <- tryCatch({
    st_read(infra_path)
  }, error = function(e) {
    stop("Error loading infrastructure file: ", e$message)
  })
  
  # Ensure CRS alignment
  if (st_crs(vector_line) != st_crs(infra_points)) {
    infra_points <- st_transform(infra_points, st_crs(vector_line))
  }
  
  # Plot points and lines together
  map_plot <- ggplot() +
    # Add vector lines
    geom_sf(data = vector_line, color = "blue", size = 1, alpha = 0.5) +
    # Add infrastructure points
    geom_sf(data = infra_points, aes(color = as.factor(infra_points$Category)), size = 3, alpha = 0.7) +
    # Add labels and title
    labs(
      title = paste("IBWT with Real Coordinates and infrastructure points:", vector_short),
      x = "Longitude (X Coordinate)",
      y = "Latitude (Y Coordinate)",
      color = "Category"
    ) +
    theme_minimal() +
    scale_color_discrete()  # Ensure discrete color scale
  
  print(map_plot)
}   ##### THIS WORKS!!!

# Example Usage
plot_points_and_lines("Australia_East", "Australia_Snowy_River_Scheme_Murrumbidgee", "Infrastructure_Australia_Snowy_River_Scheme_Murrumbidgee")
#########################################################################################################################################
plot_dem_points_and_lines <- function(dem_short, vector_short, infra_short) {
  # Find files
  files <- find_files(dem_short, vector_short)
  infra_files <- find_infra_files(dem_short, infra_short)
  
  dem_path <- files$dem_path
  vector_path <- files$vector_path
  infra_path <- infra_files$infra_path
  
  # Ensure files exist
  if (length(dem_path) == 0) stop("No DEM file found with this name.")
  if (length(vector_path) == 0) stop("No vector file found with this name.")
  if (length(infra_path) == 0) stop("No infrastructure file found with this name.")
  
  # Load DEM raster, vector lines, and infrastructure points
  dem_raster <- rast(dem_path)
  
  vector_line <- tryCatch({
    st_read(vector_path)
  }, error = function(e) {
    stop("Error loading vector file: ", e$message)
  })
  
  infra_points <- tryCatch({
    st_read(infra_path)
  }, error = function(e) {
    stop("Error loading infrastructure file: ", e$message)
  })
  
  # Ensure CRS alignment
  if (st_crs(vector_line) != crs(dem_raster)) {
    vector_line <- st_transform(vector_line, crs(dem_raster))
  }
  
  if (st_crs(infra_points) != crs(dem_raster)) {
    infra_points <- st_transform(infra_points, crs(dem_raster))
  }
  
  # Convert DEM raster to a data frame for ggplot
  dem_df <- as.data.frame(dem_raster, xy = TRUE, na.rm = TRUE)  # Convert raster to data frame
  
  # Debugging: Check the column names of dem_df
  cat("Column names in DEM data frame:\n", names(dem_df), "\n")
  
  # Find the correct column for elevation (assume it's the 3rd column if unnamed)
  elevation_column <- names(dem_df)[3]  # Adjust index if needed based on column names
  
  # Create the plot
  map_plot <- ggplot() +
    # Add DEM raster
    geom_raster(data = dem_df, aes(x = x, y = y, fill = .data[[elevation_column]])) +
    scale_fill_viridis_c(name = "Elevation (m)") +
    # Add vector lines
    geom_sf(data = vector_line, color = "blue", size = 1, alpha = 0.7) +
    # Add infrastructure points
    geom_sf(data = infra_points, aes(color = as.factor(infra_points$Category)), size = 3, alpha = 0.8) +
    # Add labels and title
    labs(title = paste("DEM with Infrastructure Points, and Vector Lines of", vector_short),
         x = "Longitude (X Coordinate)",
         y = "Latitude (Y Coordinate)",
         color = "Category"
    ) +
    theme_minimal() +
    scale_color_discrete() + # Ensure discrete color scale
    theme(legend.position = "right")
  
  print(map_plot)
}##### THIS WORKS!!!!

# Example Usage
plot_dem_points_and_lines("Australia_East", "Australia_Snowy_River_Scheme_Murrumbidgee", "Infrastructure_Australia_Snowy_River_Scheme_Murrumbidgee")
#################################################################################################################################################################
generate_elevations_for_infrastructure <- function(dem_short, vector_short, infra_short) {
  
  # Find files
  files <- find_files(dem_short, vector_short)
  infra_files <- find_infra_files(dem_short, infra_short)
  
  dem_path <- files$dem_path
  vector_path <- files$vector_path
  infra_path <- infra_files$infra_path
  
  # Ensure files exist
  if (length(dem_path) == 0) stop("No DEM file found with this name.")
  if (length(vector_path) == 0) stop("No vector file found with this name.")
  if (length(infra_path) == 0) stop("No infrastructure file found with this name.")
  
  # Load DEM raster, vector lines, and infrastructure points
  dem_raster <- rast(dem_path)
  
  vector_line <- tryCatch({
    st_read(vector_path)
  }, error = function(e) {
    stop("Error loading vector file: ", e$message)
  })
  
  infra_points <- tryCatch({
    st_read(infra_path)
  }, error = function(e) {
    stop("Error loading infrastructure file: ", e$message)
  })
  
  # Check if 'infra_points' loaded successfully
  if (exists("infra_points")) {
    str(infra_points)
  } else {
    stop("Infrastructure points not loaded. Please check the file path and format.")
  }
  
  # Ensure CRS alignment
  if (st_crs(vector_line) != crs(dem_raster)) {
    vector_line <- st_transform(vector_line, crs(dem_raster))
  }
  
  if (st_crs(infra_points) != crs(dem_raster)) {
    infra_points <- st_transform(infra_points, crs(dem_raster))
  }
  
  # Check and create 'id' column if necessary
  if (!"id" %in% colnames(infra_points)) {
    infra_points$id <- 1:nrow(infra_points)  # Assign unique IDs if not present
  }
  
  # Convert DEM raster to a data frame for ggplot
  dem_df <- as.data.frame(dem_raster, xy = TRUE, na.rm = TRUE)  # Convert raster to data frame
  
  # Debugging: Check the column names of dem_df
  cat("Column names in DEM data frame:\n", names(dem_df), "\n")
  
  # Find the correct column for elevation (assume it's the 3rd column if unnamed)
  elevation_column <- names(dem_df)[3]  # Adjust index if needed based on column names
  
  # Extract elevation values for infrastructure points by matching coordinates
  infra_coords <- st_coordinates(infra_points)
  
  # Initialize an empty list to store the elevations
  infra_elevations <- numeric(nrow(infra_points))
  
  # Loop over each infrastructure point and match to DEM coordinates
  for (i in 1:nrow(infra_points)) {
    x <- infra_coords[i, 1]
    y <- infra_coords[i, 2]
    
    # Find the closest point in the DEM raster
    nearest_dem_point <- which.min((dem_df$x - x)^2 + (dem_df$y - y)^2)
    
    # Get the corresponding elevation
    infra_elevations[i] <- dem_df[[elevation_column]][nearest_dem_point]
  }
  
  # Add the elevations to the infrastructure points data frame
  infra_points$elevation <- infra_elevations
  
  # Create a data frame to show the results
  elevations_df <- data.frame(
    infra_id = as.numeric(infra_points$id),  # assuming 'id' is the identifier for each infrastructure point
    subject = infra_points$subject,
    x = infra_coords[, 1],
    y = infra_coords[, 2],
    elevation = infra_points$elevation
  )
  #elevations_df$infra_id <- seq_len(nrow(elevations_df))
  # Return the result
  return(elevations_df)
} ### THIS WORKS!!!

# Example usage:
elevations1 <- generate_elevations_for_infrastructure("Australia_West", "Australia_Goldfields_Water_Supply_Scheme", "Infrastructure_Australia_Goldfields_Water_Supply_Scheme")

#Plot infra points on elevation line only
{
# Check if elevations_df exists and contains data
if (!exists("elevations1") || nrow(elevations1) == 0) {
  stop("The elevations_df object is missing or empty. Please ensure it contains data.")
}

# Generate the plot
ggplot(data = elevations1, aes(x = infra_id)) +
  # Add the elevation profile as a line
  geom_line(aes(y = elevation), color = "blue", size = 0.8, alpha = 0.7) +
  # Add the infrastructure points as green dots
  geom_point(aes(y = elevation), color = "green", size = 3) +
  # Label the plot
  labs(
    title = "Height Profile with Infrastructure Points",
    x = "Distance Along Line (m)",
    y = "Elevation (m)"
  ) +
  # Adjust theme for better visibility
  theme_minimal(base_size = 14)
}

########################################################################################################################################################
inspect_infra_files <- function(infra_short) {
  # Infrastructure folder and file search
  Infra_folder <- "C:/users/2816954/OneDrive - Universiteit Utrecht/THESIS/data/Infrastructure_vectors_categorized"
  Infra_filelist <- list.files(path = Infra_folder, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  # Filter infrastructure files by name
  infra_path <- Infra_filelist[grep(infra_short, basename(Infra_filelist), ignore.case = TRUE)]
  
  # Ensure the file exists
  if (length(infra_path) == 0) {
    stop("No infrastructure file found with the given name.")
  }
  
  # Load the infrastructure shapefile
  infra_data <- tryCatch({
    st_read(infra_path)
  }, error = function(e) {
    stop("Error loading infrastructure file: ", e$message)
  })
  
  # Print the geometry types and attributes of the infrastructure data
  cat("Geometry types in the infrastructure data:\n")
  print(st_geometry_type(infra_data))
  
  cat("\nAttributes (column names) of the infrastructure data:\n")
  print(names(infra_data))
  
  # Return file paths and data attributes
  return(list(infra_path = infra_path, infra_attributes = infra_data))
}

# Example usage
infra_info_df <- inspect_infra_files("Infrastructure_Australia_Snowy_River_Scheme_Murrumbidgee")

# Access and inspect attributes
print(infra_info$infra_attributes)  # View the loaded infrastructure data
###################################################################################################################################################################
