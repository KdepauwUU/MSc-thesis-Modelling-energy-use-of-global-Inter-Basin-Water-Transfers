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

# creating faster path to raster and vector lines through R

########################
# Necessary packages:  #
# terra                #
# sf                   #
# ggplot2              #
# zoo                  #
########################

# Loading path of Raster data --> allows you to check whether the data is correct for next steps
{
ASTER_DEM_folder <- "C:/users/karst/OneDrive - Universiteit Utrecht/THESIS/data/ASTER_dem"               # path to directory where ASTER files are stored
ASTER_filelist <- list.files(path = ASTER_DEM_folder, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)    # creating list of all ASTER files
names(ASTER_filelist) <- sub("^ASTER_", "", sub("\\.tif$", "", basename(ASTER_filelist)))          # creating shorter names for each ASTER DEM for further use
}

# Control if lists are correct
print(ASTER_filelist)

# Loading path of vector data --> allows you to check whether the data is correct for next steps
{
Vector_folder <- "C:/users/karst/OneDrive - Universiteit Utrecht/THESIS/data/WTMP_vectors_modified"               # path to directory where Vector lines of WTMP are stored
Vector_filelist <- list.files(path = Vector_folder, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)   # creating list of all vector files
names(Vector_filelist) <- sub("\\.shp$", "", basename(Vector_filelist))        # creating shorter names for each vector line for further usage
}
# Control if lists are correct
print(Vector_filelist)


# The following functions allows you to visualize DEM and vector data separately or combined.
{
# Raster tif files with terra through a function
plot_DEM <- function(dem_name) {
  dem_path <- ASTER_filelist[grep(dem_name, names(ASTER_filelist), ignore.case = TRUE)]         # creates a path for the DEM via previous file list and ignore case sensitivity
  
  if (length(dem_path) == 1) {
    DEM_raster <-rast(dem_path)
    plot(DEM_raster, main = paste("ASTER DEM of", dem_name))                                    # plots the DEM if name is correct
  } else if (length(dem_path) > 1){
    message("More than one file found with this name, Please specify a more unique name.")      
  } else {
    message("No DEM found with this name.")
  }
}

plot_DEM("Libya")

# function of vector lines
plot_vector_lines <- function(vector_name) {
  vector_path <- Vector_filelist[grep(vector_name, names(Vector_filelist), ignore.case = TRUE)]
  
  if (length(vector_path) == 1) {
    vector_lines <- st_read(vector_path)
    plot(st_geometry(vector_lines), main = paste("Vector lines of", vector_name), col = "blue", lwd = 2)
  } else if (length(vector_path) > 1){
    message("More than one vector file found. Please specify with a more unique name.")
  } else {
    message("No vector file found with this name.")
  }
}

plot_vector_lines("Libya_Great_Manmade_River_Phase_ONE")

# Function to plot Vector over Raster
plot_Vector_over_Raster <- function(dem_short, vector_short) {
  # find the files
  file_paths <- find_files(dem_short, vector_short)
  
  dem_path <- file_paths$dem_path
  vector_path <- file_paths$vector_path
  
  if (length(dem_path) == 0) {
    stop("No DEM-file found")
  }
  
  if (length(vector_path) == 0) {
    stop("No vector-file found")
  }
  
  # Load  DEM and vector data
  DEM_raster <- rast(dem_path)
  vector_data <- st_read(vector_path)
  DEM_raster <- crop(DEM_raster, st_bbox(vector_data))
  dem_lowres <- aggregate(DEM_raster, fact = 1)  # Factor parameter can lower resolution for laptops with lower cpu
  # Zet raster om naar een dataframe voor ggplot2
  dem_df <- as.data.frame(dem_lowres, xy = TRUE)
  colnames(dem_df) <- c("x", "y", "elevation")
  
  # Plot DEM met ggplot2
  ggplot() +
    geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +
    geom_sf(data = vector_data, color = "red", size = 1) +
    labs(title = paste("DEM of", dem_short), fill = "Elevation") +
    theme_minimal()
}

plot_Vector_over_Raster("Libya", "Libya_Great_Manmade_River_Phase_ONE")
}


# Function to find the proper DEM and Vector lines.  # load function --> when running "generate_height_profile" function this is activated.
find_files <- function(dem_short, vector_short) {
  # Searching the DEM file
  ASTER_DEM_folder <- "C:/users/karst/OneDrive - Universiteit Utrecht/THESIS/data/ASTER_dem"  # pad naar de map waar ASTER-bestanden zijn opgeslagen
  ASTER_filelist <- list.files(path = ASTER_DEM_folder, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)  # lijst van alle ASTER-bestanden
  
  # Filter DEM-files based of input shortened name
  dem_path <- ASTER_filelist[grep(dem_short, basename(ASTER_filelist), ignore.case = TRUE)]
  
  # Searching the vector file
  Vector_folder <- "C:/users/karst/OneDrive - Universiteit Utrecht/THESIS/data/WTMP_vectors_modified"  # pad naar de map waar vectorlijnen zijn opgeslagen
  Vector_filelist <- list.files(path = Vector_folder, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)  # lijst van alle vectorbestanden
  
  # Filter vector-files base of input shortened name
  vector_path <- Vector_filelist[grep(vector_short, basename(Vector_filelist), ignore.case = TRUE)]
  
  # Checking whether correct files are found and returns values
  if (length(dem_path) == 0) {
    message("No DEM-file found with this name.")
  }
  
  if (length(vector_path) == 0) {
    message("No vector-file found with this name.")
  }
  
  return(list(dem_path = dem_path, vector_path = vector_path))
}

# generate height profile for IBWT transfers function with DEM downsampling and vector line splitting
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
    distance = seq_len(nrow(extracted_data)),  # Sequence for distance (individual points)
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

# Example: generate the height profile with cropping, downsampling, and segmenting
NATURAL_ISSUE2 <- generate_height_profile("Canada_Ontario", "Canada_Saint_Joseph_Lake_Diversion", downsample_factor = 1)
save(NATURAL_ISSUE2, file = "Rstudio/output/Natural_IBWT_Canada_Churchill_River_Diversion")

# Calculate cumulative height gain and loss function
calculate_cumulative_height_gain_loss <- function(profile_df) {
  if (is.null(.Last_profile_df)) {
    stop("Input dataframe needs to contain a 'elevation'column.")
  }
  profile_df <- get(".Last_profile_df", envir = .GlobalEnv)
  # calculate height difference between points
  profile_df$heightdifference <- c(NA, diff(profile_df$elevation, na.rm = TRUE))
  
  # calculate height gain and loss
  cumulative_height_gain <- sum(profile_df$heightdifference[profile_df$heightdifference > 0], na.rm = TRUE)
  cumulative_height_loss <- sum(abs(profile_df$heightdifference[profile_df$heightdifference < 0]), na.rm = TRUE)
  
  #Results shown as a list
  return(list(
    cumulative_height_gain = cumulative_height_gain,
    cumulative_height_loss = cumulative_height_loss
  ))
} 

# Calculate height gain and loss
result <- calculate_cumulative_height_gain_loss(profile_df)
print(result)