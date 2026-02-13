# Preprocess DBPM data for fast loading in Shiny app
# This script converts zarr data to rasterized sf objects ready for plotting
# Run this once to create preprocessed data files

library(tidyverse)
library(sf)
library(stars)
library(terra)

# Check if Rarr is available
if (!requireNamespace("Rarr", quietly = TRUE)) {
  message("Installing Rarr package...")
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  BiocManager::install("Rarr")
}
library(Rarr)

message("Loading DBPM data from zarr file...")

# Path to zarr store
zarr_path <- "mean_tot-exp-bio_all_regions_simask_1deg_1961-2010.zarr"

# Load coordinate arrays
lat <- as.vector(read_zarr_array(file.path(zarr_path, "lat")))
lon <- as.vector(read_zarr_array(file.path(zarr_path, "lon")))

# Load data for each FAO region
fao_regions <- c("fao-48", "fao-58", "fao-88")
region_names <- c("Atlantic, Antarctic", "Indian Ocean, Antarctic", "Pacific, Antarctic")

# Create list to store preprocessed data
dbpm_sf_list <- list()

for (i in seq_along(fao_regions)) {
  message("\nProcessing ", fao_regions[i], " (", region_names[i], ")...")
  
  # Load biomass data
  bio_data <- read_zarr_array(file.path(zarr_path, fao_regions[i]))
  
  # Create data frame with coordinates
  coords <- expand.grid(lat = lat, lon = lon)
  coords$biomass <- as.vector(t(bio_data))  # transpose to match coordinate order
  coords$region <- region_names[i]
  coords$fao_region <- fao_regions[i]
  
  # Convert longitude from 0-360 to -180-180 format
  coords$lon <- ifelse(coords$lon > 180, coords$lon - 360, coords$lon)
  
  # Remove NA values
  coords <- coords[!is.na(coords$biomass), ]
  
  message("  Data points: ", nrow(coords))
  message("  Lon range: ", min(coords$lon), " to ", max(coords$lon))
  message("  Lat range: ", min(coords$lat), " to ", max(coords$lat))
  message("  Biomass range: ", round(min(coords$biomass), 2), " to ", round(max(coords$biomass), 2))
  
  # Keep as simple point data to avoid polygon projection issues
  message("  Converting to sf points...")
  
  # Create sf point object directly from coordinates
  bio_sf <- st_as_sf(
    coords, 
    coords = c("lon", "lat"), 
    crs = 4326,
    remove = FALSE
  )
  
  # Add region info and tooltip
  bio_sf <- bio_sf %>%
    mutate(
      region = region_names[i],
      fao_region = fao_regions[i],
      tooltip = paste0(
        "Region: ", region_names[i], "\n",
        "Lon: ", round(lon, 1), "°, Lat: ", round(lat, 1), "°\n",
        "Biomass: ", round(biomass, 2), " tonnes/km²"
      )
    )
  
  message("  SF point object created with ", nrow(bio_sf), " points")
  
  dbpm_sf_list[[i]] <- bio_sf
}

# Combine all regions
dbpm_biomass_sf <- do.call(rbind, dbpm_sf_list)

# Create test plot to verify data - using base R image()
message("\n" , rep("=", 60))
message("Creating test plot using image()...")
message(rep("=", 60))

# Use viridisLite for colors (installed with ggplot2)
library(viridisLite)

# Create plot using base R image() function
png("dbpm_test_plot.png", width = 16, height = 6, units = "in", res = 150)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 5))

for (region in unique(dbpm_biomass_sf$region)) {
  message("  Plotting region: ", region)
  
  # Get data for this region
  region_data <- dbpm_biomass_sf[dbpm_biomass_sf$region == region, ]
  coords <- st_coordinates(region_data)
  
  # Get unique sorted coordinates
  unique_lon <- sort(unique(coords[,1]))
  unique_lat <- sort(unique(coords[,2]))
  
  # Create matrix (initialize with NA)
  mat <- matrix(NA, nrow = length(unique_lat), ncol = length(unique_lon))
  
  # Fill matrix with biomass values
  for (i in 1:nrow(region_data)) {
    lon_idx <- which(unique_lon == coords[i,1])
    lat_idx <- which(unique_lat == coords[i,2])
    mat[lat_idx, lon_idx] <- region_data$biomass[i]
  }
  
  # Plot using image() with log-transformed colors
  image(unique_lon, unique_lat, t(mat), 
        col = viridis(100),
        xlab = "Longitude", ylab = "Latitude",
        main = region,
        zlim = log10(range(dbpm_biomass_sf$biomass, na.rm = TRUE)))
  
  # Add color scale info in subtitle
  mtext(paste0("Biomass (log10 tonnes/km²): ", 
               round(min(dbpm_biomass_sf$biomass, na.rm = TRUE), 2), " - ",
               round(max(dbpm_biomass_sf$biomass, na.rm = TRUE), 2)),
        side = 3, line = 0.5, cex = 0.7)
}

# Add main title
mtext("DBPM Biomass Test Plot (1961-2010 mean)", 
      side = 3, line = -1, outer = TRUE, cex = 1.2, font = 2)

dev.off()
message("Test plot saved as: dbpm_test_plot.png")
message("Please check this plot to verify the data looks correct!")

# Save as RDS file
output_file <- "dbpm_biomass_sf.rds"
saveRDS(dbpm_biomass_sf, output_file)

message("\n" , rep("=", 60))
message("Preprocessing complete!")
message(rep("=", 60))
message("Output file: ", output_file)
message("File size: ", round(file.size(output_file) / 1024^2, 2), " MB")
message("Total cells: ", nrow(dbpm_biomass_sf))
message("Regions: ", paste(unique(dbpm_biomass_sf$region), collapse = ", "))
message("\nYou can now use this RDS file in your Shiny app for instant loading!")
message("The data is already in sf format ready for plotting.")
