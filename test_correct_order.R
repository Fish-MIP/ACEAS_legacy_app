# Test script with correct coordinate ordering
library(Rarr)
library(viridisLite)

message("Loading data with correct coordinate order...")

zarr_path <- "mean_tot-exp-bio_all_regions_simask_1deg_1961-2010.zarr"

# Load coordinates
lat <- as.vector(read_zarr_array(file.path(zarr_path, "lat")))
lon <- as.vector(read_zarr_array(file.path(zarr_path, "lon")))

# Load FAO-48 data
bio_data <- read_zarr_array(file.path(zarr_path, "fao-48"))

message("Data dimensions: ", paste(dim(bio_data), collapse = " x "))

# IMPORTANT: bio_data is [lat, lon], but expand.grid creates [lon varying fastest]
# So we need to match this correctly

# Create coordinate grid - expand.grid varies FIRST argument fastest
# So expand.grid(lon, lat) gives us the right order to match as.vector(bio_data)
coords <- expand.grid(lon = lon, lat = lat)  # lon varies first
coords$biomass <- as.vector(bio_data)  # NO transpose needed

# Convert longitude from 0-360 to -180-180
coords$lon <- ifelse(coords$lon > 180, coords$lon - 360, coords$lon)

# Remove NA values
coords <- coords[!is.na(coords$biomass), ]

message("Non-NA cells: ", nrow(coords))
message("Lon range: ", min(coords$lon), " to ", max(coords$lon))
message("Lat range: ", min(coords$lat), " to ", max(coords$lat))

# Create matrix for plotting
unique_lon <- sort(unique(coords$lon))
unique_lat <- sort(unique(coords$lat))

mat <- matrix(NA, nrow = length(unique_lat), ncol = length(unique_lon))

for (i in 1:nrow(coords)) {
  lon_idx <- which(unique_lon == coords$lon[i])
  lat_idx <- which(unique_lat == coords$lat[i])
  mat[lat_idx, lon_idx] <- coords$biomass[i]
}

# Plot
png("correct_order_test.png", width = 8, height = 6, units = "in", res = 150)
par(mar = c(4, 4, 3, 2))

image(unique_lon, unique_lat, t(mat), 
      col = viridis(100),
      xlab = "Longitude", ylab = "Latitude",
      main = "Test with Correct Coordinate Order")

dev.off()

message("Plot saved as: correct_order_test.png")
