## Processing Marine Protected Areas shapefile for the Southern Ocean prior to 
# data extraction
# Author: Denisse Fierro Arcos
# Date: 2026-02-26


# Loading libraries -------------------------------------------------------
library(dplyr)
library(sf)
library(terra)
library(CCAMLRGIS)
# library(lwgeom)
library(ggplot2)


# Loading MEM ensemble output file ----------------------------------------
maps_data <- read_csv("data/ensemble_perc_change_fish_bio_all-ssp_mid-end-century_all-reg.csv")

# Rasterising sample data
sample_grid <- maps_data |> 
  distinct(longitude, latitude, value = 1) |> 
  rast(type = "xyz", crs = "epsg:4326") #|> 
  # project("epsg:6932")

# Loading MPA shapefile ---------------------------------------------------
# # ccamlr_mpas <- read_sf("data/map_layers/ccamlr_mpas.shp")
# # 
# # ccamlr_areas <- load_ASDs() |> 
# #   select(GAR_Short_Label) |> 
# #   rename(subregion = GAR_Short_Label)
# # 
# 
# # Defining functions to rasterise sample grid -----------------------------
# shp_to_raster <- function(shp, raster, grouping){
#   rast_shp <- rasterize(shp, raster, field = 1)
#   name_rast <- shp |> 
#     distinct(!!sym(grouping)) |> 
#     pull()
#   names(rast_shp) <- name_rast
#   longnames(rast_shp) <- paste0(name_rast, "_", grouping)
#   return(rast_shp)
# }
# 
# transform_shp_to_raster <- function(shp, raster, grouping, folder){
#   shp_group <- shp |> 
#     group_by(!!sym(grouping)) |> 
#     group_split() |> 
#     map(\(x) shp_to_raster(x, raster, grouping = grouping))
#   
#   if(!dir.exists(folder)){
#     dir.create(folder)
#   }
#   
#   for(i in 1:length(shp_group)){
#     ras_i <- shp_group[[i]]
#     writeCDF(ras_i, file.path(folder, paste0("ccamlr_", grouping, "_",
#                                              names(ras_i), "_ras.nc")),
#              varname = names(ras_i), overwrite = T)
#   }
# }
# 
# 
# # Applying functions transforming shp to raster ---------------------------
# transform_shp_to_raster(ccamlr_mpas, sample_grid, "mpa", "data/region_masks")
# transform_shp_to_raster(ccamlr_areas, sample_grid, "subregion", 
#                         "data/region_masks")
# 
# 
# # Extracting MEM outputs using raster masks -------------------------------
# # Loading raster masks
# ccamlr_mpas_ras <- list.files("data/region_masks/", pattern = "_mpa_", 
#                               full.names = T) |> 
#   rast()
# 
# 
# 
# # Creating raster layers
# create_maps_data <- function(maps_data, grouping){
#   reg_split <- maps_data |>
#       group_by(decade, scenario) |> 
#       group_split()
#   
#   maps_ras_df <- reg_split |> 
#     map(\(x) select(x, longitude, latitude, mean_change, sd_change, cv_mask, 
#                     cv) |> 
#           rast(type = "xyz", crs = "epsg:4326")) #|> 
#           # project("epsg:6932"))
#   
#   fn_out <- reg_split |> 
#     map(\(x) distinct(x, decade, scenario) |>
#           mutate(fn_mean = 
#                    paste0("mean-cvmask_ensemble_perc_change_", #GROUP-HERE_", 
#                           decade, "_", scenario, ".nc"))) |> #,
#                  # fn_shp = str_replace(fn_mean, "^mean-cvmask", "cv"))) |> 
#     bind_rows()
#   
#   
#   for(j in 1:length(maps_ras_df)){
#     ras_i <- maps_ras_df[[j]]
#     fnames <- fn_out$fn_mean[j]
#     writeCDF(ras_i, file.path("data", fnames), #varname = names(ras_i),
#              overwrite = T)
#     }
#     
#     # for(i in 1:length(ccamlr_mpas_ras)){
#     #   mask <- ccamlr_mpas_ras[[i]]
#     #   area_name <- str_replace_all(str_to_lower(names(mask)), " ", "-")
#     #   fout <- file.path(out_folder, 
#     #                     str_replace(fnames, "_GROUP-HERE_", 
#     #                                 paste0("_", area_name, "_")))
#     #   ras_dec_sec_area <- (ras_dec_sec*mask) 
#     #   
#     #   poly_dec_sec_area <- ras_dec_sec_area |> 
#     #     st_as_stars() |> 
#     #     st_as_sf(merge = F) |> 
#     #     mutate(tooltip = paste0("Mean change: ", round(mean_change, 1),
#     #                             "%\n", "SD: ±", round(sd_change, 1), "%\n", 
#     #                             "CV: ", round(cv, 2))) |> 
#     #     select(mean_change, cv_mask, tooltip) |> 
#     #     rename(mean = mean_change) 
#     #   
#     #   poly_dec_sec_area |> 
#     #     write_sf(fout)
#     #   
#     #   pt_dec_sec_area <- ras_dec_sec_area$cv_mask |> 
#     #     as.data.frame(xy = T) |> 
#     #     drop_na(cv_mask) |> 
#     #     st_as_sf(coords = c("x", "y"), crs = 6932)
#     #     
#     # }}
#   
#       
#         
# 
#   plot(ras_dec_sec$cv_mask)
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
  
  create_maps_data(maps_data, "region_name")
  create_maps_data(maps_data, "subregion")
  create_maps_data(maps_data, "mpa")
  # Creates panantarctic datasets
  create_maps_data(maps_data, NULL)














# MPA GPZiii crosses the international dateline. We will split this area in
# two polygons to avoid issues when extracting data

# First we will create a line to split the GPZiii polygon 
split_line_west <- st_linestring(
  matrix(c(-194.9399964576886, -111692.2771416911,
           -6709.7212147825985, -3844383.169605953), ncol = 2, 
         byrow = TRUE)) |> 
  st_sfc(crs = st_crs(ccamlr_areas))

split_line_east <- st_linestring(
  matrix(c(194.9399964576886, 111692.2771416911,
           6709.7212147825985, -3844383.169605953), ncol = 2, 
         byrow = TRUE)) |> 
  st_sfc(crs = st_crs(ccamlr_areas))

# We now extract the MPA of interest
gpz_iii <- ccamlr_mpas |> 
  filter(mpa == "GPZiii")

# We can plot both layers to ensure they overlap
ggplot()+
  geom_sf(data = gpz_iii)+
  geom_sf(data = split_line, color = "red")


# Splitting MPA -----------------------------------------------------------
gpz_iii_sp <- lwgeom::st_split(st_geometry(gpz_iii), split_line) |> 
  # Turning into multipolygon
  st_collection_extract("POLYGON") 

gpz_iii_sp <- st_sf(id = 1:length(gpz_iii_sp), 
                    geometry = st_sfc(gpz_iii_sp)) |> 
  select(!id) |> 
  mutate(mpa = "GPZiii", .before = geometry)
  

gpz_iii_wgs <- read_csv("data/map_layers/coords_gpziii.csv") |> 
  st_as_sf(coords = c("X", "Y"), crs = 4326) |> 
  group_by(poly) |> 
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON")


# Replacing original MPA polygon with multipolygon ------------------------
ccamlr_mpas_fixed <- ccamlr_mpas |> 
  filter(mpa != "GPZiii") |> 
  st_transform(crs = 4326) |> 
  bind_rows(gpz_iii_wgs)

# Plotting fixed shapefile
ccmalr_mpas_fixed |> 
  ggplot()+
  geom_sf(aes(fill = mpa))

# Saving to disk
ccmalr_mpas_fixed |> 
  write_sf("data/map_layers/southern_ocean_mpas.shp")






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
      group_by(decade, scenario) |> 
      group_split()
  }
  
  maps_ras_df <- reg_split |> 
    map(\(x) select(x, longitude, latitude, mean_change, sd_change, cv_mask, 
                    cv) |> 
          rast(type = "xyz", crs = "epsg:4326") |> 
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

create_maps_data(maps_data, "region_name")
create_maps_data(maps_data, "subregion")
create_maps_data(maps_data, "mpa")
# Creates panantarctic datasets
create_maps_data(maps_data, NULL)




