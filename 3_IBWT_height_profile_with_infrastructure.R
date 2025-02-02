IBWT_Height_profile <- function(dem_short, vector_short, infra_short) {
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
  # Split long vector lines into smaller segments
  vector_segments <- st_segmentize(vector_line, dfMaxLength = 500)
  # Extract elevation data from the DEM for vector segments
  elevation_data <- list()
  for (i in seq_len(nrow(vector_segments))) {
    segment <- vector_segments[i, ]
    segment_data <- tryCatch({
      terra::extract(dem_raster, vect(segment), along = TRUE, xy = TRUE) #DO NOT FORGET XY!!!!!
    }, error = function(e) {
      cat("Extraction failed for segment", i, "\n")
      NULL
    })
    if (!is.null(segment_data)) {
      elevation_data[[i]] <- segment_data
    }
  }
  extracted_data <- do.call(rbind, elevation_data)
  if (is.null(extracted_data) || nrow(extracted_data) == 0) {
    stop("No elevation data extracted. Check your DEM and vector data alignment.")
  }
  elev_column <- if ("lyr.1" %in% names(extracted_data)) "lyr.1" else names(extracted_data)[2]

  # Create profile_df
  profile_df <- data.frame(
    Vector_id = seq_len(nrow(extracted_data)),
    elevation = extracted_data[[elev_column]],
    vector_x = extracted_data$x,
    vector_y = extracted_data$y,
    infra_id = NA,
    Original_infra_id = NA,
    infra_elevation = NA,
    infra_x = NA,
    infra_y = NA,
    infra_Category = NA
  )
  # Initialize counters
  matched_infra_points <- 0
  unmatched_infra_points <- 0
  # Match coordinates exactly
  for (i in seq_len(nrow(infra_points))) {
    infra_point <- infra_points[i, c("POINT_X", "POINT_Y")]  # Extract only X and Y coordinates
    exact_match <- which(extracted_data$x == infra_point$POINT_X &
                           extracted_data$y == infra_point$POINT_Y)
    if (length(exact_match) > 0) {
      matched_index <- which.min(distances)  # This line seems suspicious; distances are not defined here
      profile_df$infra_id[exact_match] <- matched_index
      profile_df$Original_infra_id[exact_match] <- i
      profile_df$infra_elevation[exact_match] <- profile_df$elevation[exact_match]
      profile_df$infra_x[exact_match] <- infra_point$POINT_X
      profile_df$infra_y[exact_match] <- infra_point$POINT_Y
      profile_df$infra_Category[exact_match] <- infra_points$Category[i]
      matched_infra_points <- matched_infra_points + 1
    } else {
      # Handle unmatched points
      new_elevation <- terra::extract(dem_raster, matrix(c(infra_point$POINT_X, infra_point$POINT_Y), ncol = 2, byrow = TRUE))
      new_elevation_value <- if (!is.null(new_elevation)) new_elevation[[1]] else NA
      new_row <- data.frame(
        Vector_id = NA,  # Will be recalculated
        elevation = new_elevation_value,
        infra_id = NA,
        Original_infra_id = i,
        infra_elevation = new_elevation_value,
        infra_x = infra_point$POINT_X,
        infra_y = infra_point$POINT_Y,
        infra_Category = infra_points$Category[i],
        vector_x = infra_point$POINT_X,
        vector_y = infra_point$POINT_Y
      )
      profile_df <- rbind(profile_df, new_row)
      unmatched_infra_points <- unmatched_infra_points + 1
    }
  }
  
  # Calculate Euclidean distance function
  calculate_distance <- function(row, reference) {
    sqrt((as.numeric(row['vector_x']) - as.numeric(reference['vector_x']))^2 +
           (as.numeric(row['vector_y']) - as.numeric(reference['vector_y']))^2)
  }
  # Reference point (first row)
  reference_point <- profile_df[1, ]
  
  # Apply the function to calculate distance for each row
  profile_df$distance <- apply(profile_df, 1, function(row) calculate_distance(row, reference_point))
  profile_df <- profile_df[order(profile_df$distance), ]
  
  # View the dataframe with the distance column
  profile_df$Vector_id <- seq_len(nrow(profile_df))  # Recalculate IDs
  profile_df$infra_id <- ifelse(!is.na(profile_df$infra_x) & profile_df$infra_x != "",
                                profile_df$Vector_id,
                                NA)
  
  profile_df <- profile_df %>%
    tidyr::fill(infra_elevation, infra_Category, .direction = "down")
  rownames(profile_df) <- NULL
  # Print summary
  cat("Matched infra_points: ", matched_infra_points, "\n")
  cat("Unmatched infra_points added: ", unmatched_infra_points, "\n")
  # Create plot
  plot <- ggplot() +
    geom_line(data = profile_df, aes(x = Vector_id, y = elevation, linetype = "Elevation Profile"), 
              color = "blue", alpha = 0.25) +
    geom_line(data = profile_df, aes(x = infra_id, y = elevation, linetype = "Infrastructure Profile"), 
              color = "red", size = 1) +
    geom_point(data = profile_df, 
               aes(x = infra_id, y = elevation, color = factor(infra_Category))) +
    labs(
      title = paste("IBWT elevation profiles of:", vector_short),
      x = "Elevation points",
      y = "Elevation in meters (m)",
      color = "Category of Infrastructure",
      linetype = "Profile Type"
    ) +
    scale_color_manual(
      values = c("1" = "green", "2" = "orange", "3" = "purple", "4" = "cyan", "5" = "coral", "6" = "deepskyblue", "7" = "red", "8" = "deeppink2", "9" = "chartreuse", "10" = "aquamarine", "11" = "brown"), # Customize colors
      labels = c("1" = "Pumping station", "2" = "Tunnel", "3" = "Aqueduct", "4" = "Power station", "5" = "Pipeline", "6" = "Starting reservoir", "7" = "End reservoir", "8" = "Intermediate reservoir", "9" = "weir", "10" = "Dam", "11" = "Other infrastructure") # Add descriptive labels
    ) +
    scale_linetype_manual(
      values = c("Elevation Profile" = "solid", "Infrastructure Profile" = "dashed"), # Customize line styles
      labels = c("Elevation Profile" = "Elevation Profile", "Infrastructure Profile" = "Infrastructure Profile") # Optional, redundant here
    ) +
    theme_minimal()
  
  print(plot)
  print(infra_points)
  return(profile_df)
}

  

# Example call
# Uncomment the following line to test the function
ISSUE <- IBWT_Height_profile("spain", "Spain_Tagus_Segura_Transfer", "Infrastructure_Spain_Tagus_Segura_Transfer")
ISSUE1 <- IBWT_Height_profile("Australia_East", "Australia_Snowy_River_Scheme_Murray", "Infrastructure_Australia_Snowy_River_Scheme_Murray")
#ISSUE2 <- IBWT_Height_profile("Australia_East", "Australia_Snowy_River_Scheme_Murrumbidgee", "Infrastructure_Australia_Snowy_River_Scheme_Murrumbidgee")
#ISSUE3 <- IBWT_Height_profile("Australia_West", "Australia_Goldfields_Water_Supply_Scheme", "Infrastructure_Australia_Goldfields_Water_Supply_Scheme")
#ISSUE4 <- IBWT_Height_profile("Canada_Labrador", "Canada_Churchill_Falls_Kanairktok", "Infrastructure_Canada_Churchill_Falls_Kanairktok")
#ISSUE5 <- IBWT_Height_profile("Canada_Labrador", "Canada_Churchill_Falls_Naskaupi", "Infrastructure_Canada_Churchill_Falls_Naskaupi")
#ISSUE6 <- IBWT_Height_profile("Canada_Manitoba", "Canada_Churchill_River_Diversion", "Infrastructure_Canada_Churchill_River_Diversion")
ISSUE7 <- IBWT_Height_profile("Canada_Quebec", "Canada_James_Bay_Project_Eastmain_Diversion", "Infrastructure_Canada_James_Bay_Project_Eastmain_Diversion")
ISSUE8 <- IBWT_Height_profile("Canada_Quebec", "Canada_James_Bay_Project_LaForge_Diversion", "Infrastructure_Canada_James_Bay_Project_LaForge_Diversion")
ISSUE9 <- IBWT_Height_profile("Canada_Ontario", "Canada_Long_Lake_Diversion_Kenogami", "Infrastructure_Canada_Long_Lake_Diversion_Kenogami")
#ISSUE10 <- IBWT_Height_profile("Canada_Ontario", "Canada_Long_Lake_Diversion_Ogoki", "Infrastructure_Canada_Long_Lake_Diversion_Kenogami")
#ISSUE11 <- IBWT_Height_profile("Canada_BC", "Canada_Nechako_Kemano_Kitimat", "Infrastructure_Canada_Nechako_Kemano_Kitimat")
#ISSUE12 <- IBWT_Height_profile("Canada_Ontario", "Canada_Saint_Joseph_Lake_Diversion", "Infrastructure_Canada_Saint_Joseph_Lake_Diversion")
#ISSUE13 <- IBWT_Height_profile("Chile", "Chile_Teno_Chimbarongo_Canal", "Infrastructure_Chile_Teno_Chimbarongo_Canal")
#ISSUE14 <- IBWT_Height_profile("China_West", "China_Irtysh_Karamay_Urumqi_Canal_East", "Infrastructure_China_Irtysh_Karamay_Urumqi_Canal_East")
#ISSUE15 <- IBWT_Height_profile("China_West", "China_Irtysh_Karamay_Urumqi_Canal_West", "Infrastructure_China_Irtysh_Karamay_Urumqi_Canal_West")
#ISSUE16 <- IBWT_Height_profile("China_East", "China_South_North_Transfer_Central_Route", "Infrastructure_China_South_North_Transfer_Central_Route")
#ISSUE17 <- IBWT_Height_profile("China_East", "China_South_North_Transfer_Eastern_Route", "Infrastructure_China_South_North_Transfer_Eastern_Route")
#ISSUE18 <- IBWT_Height_profile("China_West", "China_Tarim_River_Restoration_Project", "Infrastructure_China_Tarim_River_Restoration_Project")
#ISSUE19 <- IBWT_Height_profile("India_North", "India_Indira_Gandhi_Canal", "Infrastructure_India_Indira_Gandhi_Canal")
#ISSUE20 <- IBWT_Height_profile("India_South", "India_Periyar_Vaigai_Irrigation_Project", "Infrastructure_India_Periyar_Vaigai_Irrigation_Project")
#ISSUE21 <- IBWT_Height_profile("India_South", "India_Telugu_Ganga_Project", "Infrastructure_India_Telugu_Ganga_Project")
ISSUE22 <- IBWT_Height_profile("Israel", "Israel_National_Water_Carrier", "Infrastructure_Israel_National_Water_Carrier")
#ISSUE23 <- IBWT_Height_profile("Kazakhstan", "Kazakhstan_Irtysh_Karaganda_Canal", "Infrastructure_Kazakhstan_Irtysh_Karaganda_Canal")
#ISSUE24 <- IBWT_Height_profile("Libya", "Libya_Great_Manmade_River_Phase_FOUR", "Infrastructure_Libya_Great_Manmade_River_Phase_FOUR")
#ISSUE25 <- IBWT_Height_profile("Libya", "Libya_Great_Manmade_River_Phase_ONE", "Infrastructure_Libya_Great_Manmade_River_Phase_ONE")
#ISSUE26 <- IBWT_Height_profile("Libya", "Libya_Great_Manmade_River_Phase_THREE", "Infrastructure_Libya_Great_Manmade_River_Phase_THREE")
#ISSUE27 <- IBWT_Height_profile("Libya", "Libya_Great_Manmade_River_Phase_TWO", "Infrastructure_Libya_Great_Manmade_River_Phase_TWO")
#ISSUE28 <- IBWT_Height_profile("Mexico", "Mexico_Cutzamala_reproject_system", "Infrastructure_Mexico_Cutzamala_reproject_system")
#ISSUE29 <- IBWT_Height_profile("Mexico", "Mexico_Cutzamala_system", "Infrastructure_Mexico_Cutzamala_system")
#ISSUE30 <- IBWT_Height_profile("SouthAfrica", "SA_Orange_River_Transfer_Scheme", "Infrastructure_SA_Orange_River_Transfer_Scheme")
#ISSUE31 <- IBWT_Height_profile("Ukraine", "Ukraine_North_Crimean_Canal", "Infrastructure_Ukraine_North_Crimean_Canal")
#ISSUE32 <- IBWT_Height_profile("USA_CA_AZ", "USA_All_American_Canal", "Infrastructure_USA_All_American_Canal")
ISSUE33 <- IBWT_Height_profile("USA_CA_AZ", "USA_California_State_Water_Project", "Infrastructure_USA_California_State_Water_Project")
#ISSUE34 <- IBWT_Height_profile("USA_CA_AZ", "USA_California_State_Water_Coastal_Branch_Project", "Infrastructure_USA_California_State_Water_Coastal_Branch_Project")
#ISSUE35 <- IBWT_Height_profile("USA_CA_AZ", "USA_Central_Arizona_Project", "Infrastructure_USA_Central_Arizona_Project")
#ISSUE36 <- IBWT_Height_profile("USA_CA_AZ", "USA_Central_Valley_Project_North", "Infrastructure_USA_Central_Valley_Project_North")
#ISSUE37 <- IBWT_Height_profile("USA_CA_AZ", "USA_Central_Valley_Project_South", "Infrastructure_USA_Central_Valley_Project_South")
#ISSUE38 <- IBWT_Height_profile("USA_CA_AZ", "USA_Colorado_River_Aqueduct", "Infrastructure_USA_Colorado_River_Aqueduct")
#ISSUE39 <- IBWT_Height_profile("USA_CA_AZ", "USA_Los_Angeles_Aqueduct_First", "Infrastructure_USA_Los_Angeles_Aqueduct_First")
#ISSUE40 <- IBWT_Height_profile("USA_CA_AZ", "USA_Los_Angeles_Aqueduct_Second", "Infrastructure_USA_Los_Angeles_Aqueduct_Second")
#ISSUE41 <- IBWT_Height_profile("USA_NY", "USA_Delaware_Aqueduct_Bypass_Tunnel", "Infrastructure_USA_Delaware_Aqueduct_Bypass_Tunnel")
#ISSUE42 <- IBWT_Height_profile("USA_IL", "USA_Lake_Michigan_Chicago_Diversion", "Infrastructure_USA_Lake_Michigan_Chicago_Diversion")

#write.table(ISSUE, file = "Rstudio/output/IBWT_Spain_Tagus_Segura_Transfer.txt", sep = "\t", row.names = FALSE)
write.table(ISSUE1, file = "Rstudio/output/IBWT_Australia_Snowy_River_Scheme_Murray.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE2, file = "Rstudio/output/IBWT_Australia_Snowy_River_Scheme_Murrumbidgee.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE3, file = "Rstudio/output/IBWT_Australia_Goldfields_Water_Supply_Scheme.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE6, file = "Rstudio/output/IBWT_Canada_Churchill_River_Diversion.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE7, file = "Rstudio/output/IBWT_Canada_James_Bay_Project_Eastmain_Diversion.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE8, file = "Rstudio/output/IBWT_Canada_James_Bay_Project_LaForge_Diversion.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE9, file = "Rstudio/output/IBWT_Canada_Long_Lake_Diversion_Kenogami.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE10, file = "Rstudio/output/IBWT_Canada_Long_Lake_Diversion_Ogoki.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE11, file = "Rstudio/output/IBWT_Canada_Nechako_Kemano_Kitimat.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE12, file = "Rstudio/output/IBWT_Canada_Saint_Joseph_Lake_Diversion.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE13, file = "Rstudio/output/IBWT_Chile_Teno_Chimbarongo_Canal.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE14, file = "Rstudio/output/IBWT_China_Irtysh_Karamay_Urumqi_Canal_East.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE15, file = "Rstudio/output/IBWT_China_Irtysh_Karamay_Urumqi_Canal_West.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE16, file = "Rstudio/output/IBWT_China_South_North_Transfer_Central_Route.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE17, file = "Rstudio/output/IBWT_China_South_North_Transfer_Eastern_Route.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE18, file = "Rstudio/output/IBWT_China_Tarim_River_Restoration_Project.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE19, file = "Rstudio/output/IBWT_India_Indira_Gandhi_Canal.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE20, file = "Rstudio/output/IBWT_India_Periyar_Vaigai_Irrigation_Project.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE21, file = "Rstudio/output/IBWT_India_Telugu_Ganga_Project.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE22, file = "Rstudio/output/IBWT_Israel_National_Water_Carrier.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE23, file = "Rstudio/output/IBWT_Kazakhstan_Irtysh_Karaganda_Canal.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE24, file = "Rstudio/output/IBWT_Libya_Great_Manmade_River_Phase_FOUR.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE25, file = "Rstudio/output/IBWT_Libya_Great_Manmade_River_Phase_ONE.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE26, file = "Rstudio/output/IBWT_Libya_Great_Manmade_River_Phase_THREE.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE27, file = "Rstudio/output/IBWT_Libya_Great_Manmade_River_Phase_TWO.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE28, file = "Rstudio/output/IBWT_Mexico_Cutzamala_reproject_system.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE29, file = "Rstudio/output/IBWT_Mexico_Cutzamala_system.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE30, file = "Rstudio/output/IBWT_SA_Orange_River_Transfer_Scheme.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE31, file = "Rstudio/output/IBWT_Ukraine_North_Crimean_Canal.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE32, file = "Rstudio/output/IBWT_USA_All_American_Canal.txt", sep = "\t", row.names = FALSE)
write.table(ISSUE33, file = "Rstudio/output/IBWT_USA_California_State_Water_Project.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE34, file = "Rstudio/output/IBWT_USA_California_State_Water_Coastal_Branch_Project.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE35, file = "Rstudio/output/IBWT_USA_Central_Arizona_Project.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE36, file = "Rstudio/output/IBWT_USA_Central_Valley_Project_North.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE37, file = "Rstudio/output/IBWT_USA_Central_Valley_Project_South.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE38, file = "Rstudio/output/IBWT_USA_Colorado_River_Aqueduct.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE39, file = "Rstudio/output/IBWT_USA_Los_Angeles_Aqueduct_First.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE40, file = "Rstudio/output/IBWT_USA_Los_Angeles_Aqueduct_Second.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE41, file = "Rstudio/output/IBWT_USA_Delaware_Aqueduct_Bypass_Tunnel.txt", sep = "\t", row.names = FALSE)
#write.table(ISSUE42, file = "Rstudio/output/IBWT_USA_Lake_Michigan_Chicago_Diversion.txt", sep = "\t", row.names = FALSE)
