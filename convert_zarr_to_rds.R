# Convert zarr files to RDS format for R
# This script uses reticulate to read zarr files and saves them as RDS
# Run this once to convert the data, then you won't need Python dependencies in the main app

library(reticulate)
library(tidyverse)

# Import Python zarr library
zarr <- import("zarr")

# Path to zarr store
zarr_path <- "mean_tot-exp-bio_all_regions_simask_1deg_1961-2010.zarr"

# Load coordinate arrays
zarr_store <- zarr$open(zarr_path, mode = "r")
lat <- as.vector(zarr_store$lat$get())
lon <- as.vector(zarr_store$lon$get())

# Load data for each FAO region and convert to data frame
fao_regions <- c("fao-48", "fao-58", "fao-88")
region_names <- c("Atlantic, Antarctic", "Indian Ocean, Antarctic", "Pacific, Antarctic")

dbpm_data_list <- list()

for (i in seq_along(fao_regions)) {
  message("Processing ", fao_regions[i], " (", region_names[i], ")...")
  
  # Load biomass data
  bio_data <- zarr_store[[fao_regions[i]]]$get()
  
  # Create data frame with coordinates
  coords <- expand.grid(lat = lat, lon = lon)
  coords$biomass <- as.vector(t(bio_data))  # transpose to match coordinate order
  coords$region <- region_names[i]
  coords$fao_region <- fao_regions[i]
  
  # Remove NA values
  coords <- coords[!is.na(coords$biomass), ]
  
  dbpm_data_list[[i]] <- coords
}

# Combine all regions
dbpm_biomass_data <- do.call(rbind, dbpm_data_list)

# Save as RDS file
output_file <- "dbpm_biomass_data.rds"
saveRDS(dbpm_biomass_data, output_file)

message("\nConversion complete!")
message("Saved DBPM biomass data to: ", output_file)
message("Number of data points: ", nrow(dbpm_biomass_data))
message("Regions included: ", paste(unique(dbpm_biomass_data$region), collapse = ", "))
message("\nYou can now use this RDS file in your Shiny app without Python dependencies.")
