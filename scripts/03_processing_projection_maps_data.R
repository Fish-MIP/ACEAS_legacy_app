## Processing ensemble outputs to create maps of projected biomass change
# Author: Denisse Fierro Arcos
# Date: 2026-02-26


# Loading libraries -------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(sf)
library(terra)


# Loading MEM ensemble output file ----------------------------------------
maps_data <- read_csv(
  "data/ensemble_perc_change_fish_bio_all-ssp_mid-end-century_all-reg.csv")

# Rasterising sample data
sample_grid <- maps_data |> 
  distinct(longitude, latitude, value = 1) |> 
  rast(type = "xyz", crs = "epsg:4326")


### ----------------------------------------------------------------------
# Creating raster layers
create_maps_data <- function(maps_data, grouping){
  if(!is.null(grouping)){
    reg_split <- maps_data |> 
      drop_na(!!sym(grouping)) |> 
      group_by(decade, scenario, !!sym(grouping)) |> 
      group_split()
  }else{
    reg_split <- maps_data |> 
      drop_na(fao) |> 
      group_by(decade, scenario) |> 
      group_split()
  }
  
  maps_ras <- reg_split |>
    map(\(x) select(x, longitude, latitude, mean_change, sd_change, cv_mask,
                    cv) |>
          st_as_sf(coords = c("longitude", "latitude"), crs = 4326))
  
  maps_ras_df <- maps_ras |> 
    map(\(x) rasterize(x, sample_grid, field = c("mean_change", "sd_change", 
                                                 "cv_mask", "cv")) |> 
          st_as_stars() |>
          st_as_sf(merge = F) |>
          mutate(tooltip = paste0("Mean change: ", round(mean_change, 1),
                                  "%\n", "SD: ±", round(sd_change, 1), "%\n",
                                  "CV: ", round(cv, 2))) |>
          st_transform(crs = "+proj=ortho +lat_0=-90 +lon_0=0") |>
          select(mean_change, cv_mask, tooltip))
  
  maps_sf <- reg_split |> 
    map(\(x) select(x, longitude, latitude, cv_mask) |>
          drop_na(cv_mask) |> 
          st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
          st_transform(crs = "+proj=ortho +lat_0=-90 +lon_0=0"))
  
  if(!is.null(grouping)){
    fn_out <- reg_split |>
      map(\(x) distinct(x, !!sym(grouping), decade, scenario) |>
            mutate(fn_mean =
                     paste0("mean-cvmask_ensemble_perc_change_",
                            str_replace_all(str_to_lower(!!sym(grouping)),
                                            " ", "-"), "_", decade, "_",
                            scenario, ".shp"),
                   fn_shp = str_replace(fn_mean, "^mean-cvmask", "cv"))) |>
      bind_rows()
  }else{
    fn_out <- reg_split |>
      map(\(x) distinct(x, decade, scenario) |>
            mutate(fn_mean =
                     paste0("mean-cvmask_ensemble_perc_change_southern-ocean_",
                            decade, "_", scenario, ".shp"),
                   fn_shp = str_replace(fn_mean, "^mean-cvmask", "cv"))) |>
      bind_rows()
  }
  
  
  out_folder <- "data/projection_maps"
  if(!dir.exists(out_folder)){
    dir.create(out_folder)
  }
  
  for(i in 1:nrow(fn_out)){
    maps_ras_df[[i]] |> 
      rename(mean = mean_change) |> 
      write_sf(file.path(out_folder, fn_out$fn_mean[i]))
    maps_sf[[i]] |> 
      write_sf(file.path(out_folder, fn_out$fn_shp[i]))
  }
}


# Applying function to all groupings --------------------------------------
create_maps_data(maps_data, "fao")
create_maps_data(maps_data, "mpa")
create_maps_data(maps_data, "subregion")
create_maps_data(maps_data, "eez")
# Creates panantarctic datasets
create_maps_data(maps_data, NULL)

