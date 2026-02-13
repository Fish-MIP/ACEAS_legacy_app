# Debug script to examine zarr data structure
library(Rarr)

message("Examining zarr structure...")

zarr_path <- "mean_tot-exp-bio_all_regions_simask_1deg_1961-2010.zarr"

# Load coordinate arrays
lat <- as.vector(read_zarr_array(file.path(zarr_path, "lat")))
lon <- as.vector(read_zarr_array(file.path(zarr_path, "lon")))

# Load FAO-48 data
bio_data <- read_zarr_array(file.path(zarr_path, "fao-48"))

message("\nCoordinate info:")
message("  lat length: ", length(lat))
message("  lon length: ", length(lon))
message("  lat range: ", min(lat), " to ", max(lat))
message("  lon range: ", min(lon), " to ", max(lon))

message("\nBio data array info:")
message("  Dimensions: ", paste(dim(bio_data), collapse = " x "))
message("  Class: ", class(bio_data))

message("\nChecking dimension order...")
message("  Expected if [lat, lon]: ", length(lat), " x ", length(lon))
message("  Expected if [lon, lat]: ", length(lon), " x ", length(lat))
message("  Actual dimensions: ", paste(dim(bio_data), collapse = " x "))

# Check a few values
message("\nSample values from bio_data:")
message("  [1,1]: ", bio_data[1,1])
message("  [1,2]: ", bio_data[1,2])
message("  [2,1]: ", bio_data[2,1])

# Try without transpose
message("\nTesting coordinate matching WITHOUT transpose...")
coords_no_t <- expand.grid(lat = lat, lon = lon)
coords_no_t$biomass <- as.vector(bio_data)  # NO transpose
coords_no_t <- coords_no_t[!is.na(coords_no_t$biomass), ]
message("  Non-NA cells: ", nrow(coords_no_t))

# Try with transpose
message("\nTesting coordinate matching WITH transpose...")
coords_with_t <- expand.grid(lat = lat, lon = lon)
coords_with_t$biomass <- as.vector(t(bio_data))  # WITH transpose
coords_with_t <- coords_with_t[!is.na(coords_with_t$biomass), ]
message("  Non-NA cells: ", nrow(coords_with_t))

message("\nDone!")
