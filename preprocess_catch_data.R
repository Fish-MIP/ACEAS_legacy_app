# Test script to preprocess catch data and create a test plot
# This will help determine if rendering issues are data-specific

library(tidyverse)
library(sf)
library(viridisLite)

# Check if Rarr is available
if (!requireNamespace("Rarr", quietly = TRUE)) {
  message("Installing Rarr package...")
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  BiocManager::install("Rarr")
}
library(Rarr)

message("Loading catch data from zarr file...")

# Path to zarr store
zarr_path <- "mean_catch_all_regions_simask_1deg_1961-2010.zarr"

# Load coordinate arrays
lat <- as.vector(read_zarr_array(file.path(zarr_path, "lat")))
lon <- as.vector(read_zarr_array(file.path(zarr_path, "lon")))

# Load data for FAO-48 only (Atlantic, Antarctic) as a test
message("\nProcessing fao-48 (Atlantic, Antarctic)...")

# Load catch data
catch_data <- read_zarr_array(file.path(zarr_path, "fao-48"))

# Create data frame with coordinates
coords <- expand.grid(lat = lat, lon = lon)
coords$catch <- as.vector(t(catch_data))  # transpose to match coordinate order

# Convert longitude from 0-360 to -180-180 format
coords$lon <- ifelse(coords$lon > 180, coords$lon - 360, coords$lon)

# Remove NA values
coords <- coords[!is.na(coords$catch), ]

message("  Data points: ", nrow(coords))
message("  Lon range: ", min(coords$lon), " to ", max(coords$lon))
message("  Lat range: ", min(coords$lat), " to ", max(coords$lat))
message("  Catch range: ", round(min(coords$catch), 2), " to ", round(max(coords$catch), 2))

# Create test plot using image()
message("\nCreating test plot using image()...")

# Get unique sorted coordinates
unique_lon <- sort(unique(coords$lon))
unique_lat <- sort(unique(coords$lat))

# Create matrix (initialize with NA)
mat <- matrix(NA, nrow = length(unique_lat), ncol = length(unique_lon))

# Fill matrix with catch values
for (i in 1:nrow(coords)) {
  lon_idx <- which(unique_lon == coords$lon[i])
  lat_idx <- which(unique_lat == coords$lat[i])
  mat[lat_idx, lon_idx] <- coords$catch[i]
}

# Plot using base R image()
png("catch_test_plot.png", width = 8, height = 6, units = "in", res = 150)
par(mar = c(4, 4, 3, 2))

# Use sqrt transformation instead of log for data with zeros
image(unique_lon, unique_lat, t(mat), 
      col = viridis(100),
      xlab = "Longitude", ylab = "Latitude",
      main = "FAO-48 Mean Catch Test Plot (1961-2010)")

mtext(paste0("Catch (tonnes): ", 
             round(min(coords$catch, na.rm = TRUE), 3), " - ",
             round(max(coords$catch, na.rm = TRUE), 3)),
      side = 3, line = 0.5, cex = 0.9)

dev.off()

message("Test plot saved as: catch_test_plot.png")
message("\nDone!")
