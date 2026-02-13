library(CCAMLRGIS)
library(sf)
library(tidyverse)
library(stars)
library(data.table)
library(terra)
library(cmocean)
library(rnaturalearthdata)
# Read and combine fishmip datasets
fl_ls <- list.files("c:/Users/greendb/OneDrive - University of Tasmania/Documents/ACEAS_showcase/", pattern = "*.csv", full.names = T)
# jb files: 
#fl_ls <- list.files(pattern = "*.csv", full.names = T)
proj_dat <- rbindlist(lapply(fl_ls, fread))
# 

# Create spatrast/grid from dataframe
tmplt <- rast(ext(-180,180,-78,-45), resolution = 1, crs = crs("+init=epsg:4326")) # first template grid at 1deg res
proj_rast <- rasterize(proj_dat[,2:3], tmplt, values = proj_dat$mean_change, fun = mean) # rasterise, 1 val per grid cell, so mean is ok
proj_stars <- st_as_stars(proj_rast) # turn into stars grid object for easier plotting in ggplot
proj_sf <- st_as_sf(proj_stars, merge = F) # then turn into polygons that can be projecetd better
proj_sf$mean <- ifelse(proj_sf$mean>50,50,proj_sf$mean) # cap values at 50, following Kieran's paper

# Get CCAMLR areas and EEZs and continents
CCAMLR_Areas <- load_ASDs() # grab statistical areas
EEZs <- load_EEZs() # grab eezs
ANT <- load_Coastline() # get antarctic polygon
wrld <- countries50 # get the rest of the world

# plotting circumpolar
proj_map <- ggplot() +
  geom_sf(data = proj_sf, aes(fill = mean), colour = NA) +
  scale_fill_cmocean(name = "curl", limits = c(-50,50)) +
  geom_sf(data = CCAMLR_Areas, fill = NA, colour = "grey60", lwd = 1) +
  geom_sf(data = EEZs, fill = NA, colour = "orange", lwd = 1) +
  
  geom_sf(data = ANT, fill = "grey99", colour = "grey60") +
  geom_sf(data = wrld, fill = "grey60", colour = "grey60") +
  coord_sf(crs = "+proj=ortho +lat_0=-90 +lon_0=0",
           ylim = c(-4000000, 4000000),
           xlim = c(-4000000, 4100000   )) +
  theme_bw()

ggsave(filename = "c:/Users/greendb/OneDrive - University of Tasmania/Documents/ACEAS_showcase/FishMIP_proj_CCAMLR_domain.png", proj_map, dpi = 300)
