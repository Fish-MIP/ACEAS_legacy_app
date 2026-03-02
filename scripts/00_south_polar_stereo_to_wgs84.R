## Transforming South Polar Stereographic shapefile into WGS84
# Solving issue with crossing of international dateline
# Author: Denisse Fierro Arcos
# Date: 2026-02-26


# Loading libraries -------------------------------------------------------
library(dplyr)
library(sf)
library(CCAMLRGIS)
library(ggplot2)
library(lwgeom)

# MPA GPZiii crosses the international dateline. We will split this area in
# two polygons to avoid issues when extracting data
ccamlr_mpas <- load_MPAs()

# First we will create two lines to split the GPZiii polygon across date line
split_line_west <- st_linestring(
  matrix(c(-194.9399964576886, -111692.2771416911,
           -6709.7212147825985, -3844383.169605953), ncol = 2, 
         byrow = TRUE)) |> 
  st_sfc(crs = st_crs(ccamlr_mpas))

split_line_east <- st_linestring(
  matrix(c(194.9399964576886, 111692.2771416911,
           6709.7212147825985, -3844383.169605953), ncol = 2, 
         byrow = TRUE)) |> 
  st_sfc(crs = st_crs(ccamlr_mpas))

# We now extract the MPA of interest
gpz_iii <- ccamlr_mpas |> 
  filter(GAR_Short_Label == "GPZiii")

# We can plot both layers to ensure they overlap
ggplot()+
  geom_sf(data = gpz_iii)+
  geom_sf(data = split_line_east, color = "red")+
  geom_sf(data = split_line_west, color = "blue")


# Splitting MPA -----------------------------------------------------------
# Needs to be split twice to ensure east and west sides do not cross the 
# international dateline
gpz_iii_east <- st_split(st_geometry(gpz_iii), split_line_east) |> 
  # Turning into multipolygon
  st_collection_extract("POLYGON") 

gpz_iii_east <- st_sf(id = 1:length(gpz_iii_east), 
                    geometry = st_sfc(gpz_iii_east)) |> 
  select(!id) |> 
  mutate(mpa = "GPZiii", side = "east", .before = geometry)

gpz_iii_west <- st_split(st_geometry(gpz_iii), split_line_west) |> 
  # Turning into multipolygon
  st_collection_extract("POLYGON") 

gpz_iii_west <- st_sf(id = 1:length(gpz_iii_west), 
                      geometry = st_sfc(gpz_iii_west)) |> 
  select(!id) |> 
  mutate(mpa = "GPZiii", side = "west", .before = geometry)

# Merging both sides, but keeping separate polygon for east and west
gpz_iii_shp <- gpz_iii_east[2,] |> 
  bind_rows(gpz_iii_west[1,]) |> 
  st_transform(crs = 4326) |> 
  select(!side)

# Check the result before proceeding
gpz_iii_shp |> 
  ggplot()+geom_sf()


# Replacing original MPA polygon with multipolygon ------------------------
ccamlr_mpas_fixed <- ccamlr_mpas |> 
  select(GAR_Short_Label) |> 
  rename(mpa = GAR_Short_Label) |> 
  filter(mpa != "GPZiii") |> 
  st_transform(crs = 4326) |> 
  bind_rows(gpz_iii_shp)

# Plotting fixed shapefile
ccamlr_mpas_fixed |> 
  ggplot()+
  geom_sf(aes(fill = mpa))

# Saving to disk
ccamlr_mpas_fixed |> 
  write_sf("data/map_layers/ccamlr_mpas_wgs84.shp")



