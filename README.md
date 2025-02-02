# MSc-thesis-Modelling-energy-use-of-global-Inter-Basin-Water-Transfers
This repository contains every script and data that was produced during my MSc thesis.

MSc Research Modelling Energy Use of Global Inter-Basin Water Transfer (IBWT) Megaprojects
Author
Name: Karsten De Pauw
Student Number: 2816954
Contact Info: k.depauw@students.uu.nl

Supervisors
1st Supervisor: Prof. Michelle van Vliet
2nd Supervisor: Michele Magni MSc

Programme Information
MSc Programme: Earth Surface and Water
Faculty: Faculty of Geosciences
Department: Department of Physical Geography
University: Utrecht University
Description

This project involves modelling the energy use of global Inter-Basin Water Transfer (IBWT) megaprojects. The R scripts provided are used to generate databases, calculate energy consumption, and visualize elevation profiles and distances for these projects.

Scripts Overview

0. R_rasterdata.R
Purpose: Handles raster data processing and analysis for IBWT projects.
Usage: This script includes functions to load, manipulate, and analyze raster data, such as DEM (Digital Elevation Model) data, for use in IBWT projects.
Main Functions:
load_raster_data()
process_raster_data()
analyze_raster_data()

1. R_Infrastructurepoints.R
Purpose: Analyzes and visualizes infrastructure data for IBWT projects.
Usage: This script defines functions to create plots and perform analyses on infrastructure points data.
Main Functions:
find_infra_files()
generate_infra_scatterplot()
inspect_infra_files()

2. Natural_height_profile.R
Purpose: Processes DEM raster data and vector data to generate natural height profiles.
Usage: This script defines a function to load and process DEM and vector data, ensuring CRS alignment and extracting elevation data.
Main Functions:
load_data()
generate_height_profile()

3. IBWT_height_profile_with_infrastructure.R
Purpose: Processes DEM raster data and vector data to generate height profiles with infrastructure points.
Usage: This script defines a function to load and process DEM and vector data, ensuring CRS alignment and extracting elevation data.
Main Functions:
load_data()
IBWT_Height_profile()

4. R_generatingNEWDatabase.R
Purpose: Generates a new database, appends water discharge levels, and calculates energy consumption for multiple IBWT projects.
Usage: This script reads DEM and vector files, processes them, and generates height profiles with energy consumption calculations.
Main Functions:
bulk_generate_transfer_database()
generate_height_profile_with_energy()

5. Finaldataset.R
Purpose: Generates a new database, appends water discharge levels, and calculates energy consumption.
Usage: This script reads and combines text files containing height profile data, performs calculations, and creates visualizations using ggplot2.
Main Functions:
load_data()
calculate_energy_consumption()
create_visualizations()

6. Globalmapvisualisation.R
Purpose: Visualizes global IBWT projects on a world map.
Usage: This script categorizes the centroids of the projects based on various metrics and uses ggplot2 and sf for visualization.
Main Functions:
categorize_data()
create_world_map()

