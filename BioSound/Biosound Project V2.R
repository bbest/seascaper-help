# Updated SeascapeR functions to support custom sf path and 8 day data resolution 
# Created by Jared Stephens 10/08/2023

# install.packages("mapview")
# install.packages("rgdal", repos = "https://R-Forge.R-project.org")
# install.packages("devtools")
# devtools::session_info()

# remotes::install_github("marinebon/seascapeR", force=TRUE)
library(librarian)
library(seascapeR)
library(sf)
library(glue)
library(mapview)
library(dplyr)

# Set directory and define the custom shapefile path
# NOTE: Put "" for the custom_sf_path variable if you don't have a custom shapefile to load
# NOTE: You must run the run the custom_sf_path to complete the function even if you don't have a custom shapefile
# ~/OSA/Seascape Project/Marathon_FL_shapefile/Marathon_FL.shp
# LaJolla_MPA/LaJolla_MPA.shp
dir_data <- setwd("H:/BioSound/Shapefiles/KeyWest_FL/KeyWest_0.55deg") # Set directory path
custom_sf_path <- "H:/BioSound/Shapefiles/KeyWest_FL/KeyWest_0.55deg/KeyWest_SeascapeR.shp" # Set shapefile path
sanctuary <- "mbnms" # Or you can set the sanctuary code

ss_start_date <- "2020-01-01" # Adjust desired start date
ss_end_date <- "2020-04-01" # Adjust desired end date

# This is the updated function that reads shapefiles
# It originally read the "sanctuary" code and retrieve the shapefile information from NOAA Sanctuaries GIS 
# Found at https://sanctuaries.noaa.gov/library/imast_gis.html
# Now this function can search the custom shapefile path provided or read the standard sanctuary code
get_sanctuary_ply <- function(dir_ply, custom_sf_path = NULL, sanctuary = NULL) {
  if (!is.null(custom_sf_path) && file.exists(custom_sf_path)) {
    # Use the custom shapefile path provided by the user
    ply <- sf::read_sf(custom_sf_path) %>%
      st_transform(4326)  # Reproject to EPSG 4326 (WGS84)
  } else if (!is.null(sanctuary)) {
    # Use get_url_ply to retrieve polygon geometry for the sanctuary code
    ply <- get_url_ply(sanctuary = sanctuary, dir_ply = dir_ply) %>%
      st_transform(4326)  # Reproject to EPSG 4326 (WGS84)
  } else {
    stop("No custom shapefile path provided and no sanctuary code specified.")
  }
  
  return(ply)
}

# Run the function to retrieve the shapefile data
ply <- get_sanctuary_ply(dir_ply = "data/ply", custom_sf_path = custom_sf_path, sanctuary = sanctuary)


############# Don't run this naming code below if you're using a sanctuary code!!! ###########
# Have user input shapefile name if there isn't one
if ("NAME" %in% colnames(ply) && !is.null(ply$NAME[1])) {
  ply_name <- ply$NAME[1] # Read the first title of the shapefile
} else {
  if ("NAME" %in% colnames(ply)) {
    stop("No custom shapefile path provided.")
  } else {
    # Ask user to input the name for the 'NAME' column
    cat("Please enter the name for this shapefile: ")
    name <- readline()
    # Create the 'NAME' column with the provided name
    ply$NAME <- name
    ply_name <- name
  }
}

# Filter just the name and polygon data
# If you're using a sanctuary code then you don't need to use this, it will give you an error!!!
ply <- ply %>%
  select(NAME, geometry)
ply

# The LaJolla shapefile has two features
# If need be, this code below filters between the two features
#ply <- ply %>%
#  filter(OBJECTID != 89)


# Fetch Seascape data with 8-day resolution using seascapeR package
ss_dataset <- "global_8day"
ss_var <- "CLASS"

# Fetch Seascape data using seascapeR package
ss_info <- get_ss_info(dataset = ss_dataset)
ss_info

# Visualize the ss_info
map_ss_wms(ss_info, ply, ss_var = ss_var)


# Get SeaScape grids within the polygon for the specified date range
# Check the map generated above to ensure there is ss data for the range you're measuring
grds <- get_ss_grds(  
  ss_info, 
  ply, 
  ss_var    = ss_var, 
  date_beg  = ss_start_date, 
  date_end  = ss_end_date,
  dir_tif   = NULL,
  verbose = TRUE)

# Get the first grid, a raster layer in the raster stack grds
grd <- raster::raster(grds, 1)

# Map SeaScape grid
# NOTE: this function creates an error saying there's an unused argument
map_ss_grd(grd)

# Set the dataframe name
ts_csv = glue::glue("{dir_data}/{ply_name}_{ss_dataset}_{ss_var}.csv")

# Construct the ss dataframe
tbl <- sum_ss_grds_to_ts(grds, ts_csv = ts_csv)
tbl

# Save the data frame to a CSV file
write.csv(tbl, file = ts_csv, row.names = FALSE)

# plot SeaScape time series
plot_ss_ts(tbl, show_legend = "always")

