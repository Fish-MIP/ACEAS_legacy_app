## Creating masks for GFDL-MOM6-COBALT2 ocean model that includes regions
# shown in the shiny app (CCAMLR main and sub regions, EEZs, and MPAs)
# Author: Denisse Fierro Arcos
# Last updated: 2026-05-15


# Loading libraries -------------------------------------------------------
library(dplyr)
library(sf)
library(CCAMLRGIS)
library(ggplot2)
library(stringr)


# Loading shapefiles with regional data -----------------------------------
# CRS WGS 84
fao <- read_sf(
  "/rd/gem/private/shared_resources/FAO_shapefiles/FAO_MajorAreas.shp") |> 
  filter(str_detect(NAME_EN, "Antarctic")) |> 
  select(F_CODE, OCEAN)

ccamlr_mpas_fixed <- read_sf("data/map_layers/ccamlr_mpas_wgs84.shp")

# South Pole Stereographic
ccamlr_areas <- load_ASDs() |> 
  select(GAR_Name, GAR_Short_Label)

eez <- read_sf("data/map_layers/ccamlr_eez_macq_epsg6932.shp")


# Loading GFDL masks ------------------------------------------------------
mask_1deg <- read.csv(
  file.path("/rd/gem/private/shared_resources/grid_cell_area_ESMs/isimip3a",
            "gfdl-mom6-cobalt2_areacello_60arcmin_global_fixed.csv")) |> 
  filter(y <= -40)

mask_025deg <- read.csv(
  file.path("/rd/gem/private/shared_resources/grid_cell_area_ESMs/isimip3a",
            "gfdl-mom6-cobalt2_areacello_15arcmin_global_fixed.csv")) |> 
  filter(y <= -40)



# Adding regional data ----------------------------------------------------
# 1 degree
mask_1deg_so <- mask_1deg |> 
  st_as_sf(coords = c("x", "y"), crs = st_crs(4326), remove = F) |> 
  st_join(ccamlr_mpas_fixed) |> 
  st_join(fao) |> 
  # Resolving minor discrepancies
  mutate(F_CODE = case_when(is.na(F_CODE) & (x < -70 | x > 150) & y <= -60 ~ 
                              "88", 
                            is.na(F_CODE) & (x > -70 & x < 30) & y <= -60 ~ 
                              "48",
                            is.na(F_CODE) & (x > 30 & x < 150) & y <= -60 ~ 
                              "58",
                            .default = F_CODE),
         mpa_code = case_when(is.na(mpa_code) & (x > 163 & x < 168) & 
                                (y < -60 & y > -63) ~ 2,
                              .default = mpa_code),
         mpa = case_when(is.na(mpa) & mpa_code == 2 ~ "GPZii",
                         .default = mpa),
         OCEAN = case_when(is.na(OCEAN) & F_CODE == "88" ~ "Pacific",
                           is.na(OCEAN) & F_CODE == "48" ~ "Atlantic",
                           is.na(OCEAN) & F_CODE == "58" ~ "Indian",
                           .default = OCEAN))|> 
  rename("fao_code" = "F_CODE", "fao" = "OCEAN") |> 
  st_transform(st_crs(ccamlr_areas)) |> 
  st_join(ccamlr_areas) |> 
  st_join(eez) |> 
  mutate(GAR_Short_Label = 
           case_when(is.na(GAR_Short_Label) & (x > -61 & x < -20) & y < -64 ~ 
                       "485", .default = GAR_Short_Label),
         GAR_Name = case_when(is.na(GAR_Name) & GAR_Short_Label == "485" ~ 
                                "Subarea 48.5", .default = GAR_Name)) |> 
  rename("subreg" = "GAR_Short_Label", "subreg_name" = "GAR_Name")

# Saving result
mask_1deg_so |> 
  st_drop_geometry() |> 
  write.csv(
    file.path("/rd/gem/private/shared_resources/SouthernOceanMasks",
              "gfdl-mom6-cobalt2_areacello_60arcmin_southern-ocean_fixed.csv"), 
    row.names = F)


# 025 degree
mask_025deg_so <- mask_025deg |> 
  st_as_sf(coords = c("x", "y"), crs = st_crs(4326), remove = F) |> 
  st_join(ccamlr_mpas_fixed) |> 
  st_join(fao) |> 
  # Resolving minor discrepancies
  mutate(F_CODE = case_when(is.na(F_CODE) & (x < -70 | x > 150) & y <= -60 ~ 
                              "88", 
                            is.na(F_CODE) & (x > -70 & x < 30) & y <= -60 ~ 
                              "48",
                            is.na(F_CODE) & (x > 30 & x < 150) & y <= -60 ~ 
                              "58",
                            .default = F_CODE),
         OCEAN = case_when(is.na(OCEAN) & F_CODE == "88" ~ "Pacific",
                           is.na(OCEAN) & F_CODE == "48" ~ "Atlantic",
                           is.na(OCEAN) & F_CODE == "58" ~ "Indian",
                           .default = OCEAN))|> 
  rename("fao_code" = "F_CODE", "fao" = "OCEAN") |> 
  st_transform(st_crs(ccamlr_areas)) |> 
  st_join(ccamlr_areas) |> 
  st_join(eez) |> 
  mutate(GAR_Short_Label = 
           case_when(is.na(GAR_Short_Label) & (x > -61 & x < -20) & y < -64 ~ 
                       "485", 
                     is.na(GAR_Short_Label) & (x > -70 & x < -50) & y < -60 ~ 
                       "481", .default = GAR_Short_Label),
         GAR_Name = case_when(is.na(GAR_Name) & GAR_Short_Label == "485" ~ 
                                "Subarea 48.5", 
                              is.na(GAR_Name) & GAR_Short_Label == "481" ~ 
                                "Subarea 48.1", .default = GAR_Name)) |> 
  rename("subreg" = "GAR_Short_Label", "subreg_name" = "GAR_Name")

# Saving result
mask_025deg_so |> 
  st_drop_geometry() |> 
  write.csv(
    file.path("/rd/gem/private/shared_resources/SouthernOceanMasks",
              "gfdl-mom6-cobalt2_areacello_15arcmin_southern-ocean_fixed.csv"), 
    row.names = F)
