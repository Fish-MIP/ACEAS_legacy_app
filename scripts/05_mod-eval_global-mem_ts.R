# Calculating time series from global MEM outputs
# Author: Denisse Fierro Arcos
# Date last update: 2026-05-12


# Loading libraries -------------------------------------------------------
library(terra)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(arrow)
# library(tictoc)
# library(ggplot2)

# Regridding DBEM outputs from 0.5 to 1 deg -------------------------------
dbem_files <- list.files("/rd/gem/private/fishmip_outputs/ISIMIP3a/global/DBEM",
                         pattern = "gfdl.*_30arc.*_tc[_|b].*", recursive = T, 
                         full.names = T)

# Loading 1 degree grid 
grid_1deg <- list.files(
  "/rd/gem/private/shared_resources/grid_cell_area_ESMs/isimip3a", 
  pattern = "60arcmin.*nc$", full.names = T) |> 
  rast()

# Turning sample grid into land mask
grid_1deg[!is.na(grid_1deg)] <- 1

# Defining function to regrid and apply land mask
regrid_land_mask <- function(file_path_input, target_grid, land_mask, ...){
  fn <- file_path_input |> 
    str_replace("30arcmin", "60arcmin")
  ras <- rast(file_path_input)
  ras_reg <- resample(ras, target_grid, ...)*land_mask
  writeCDF(ras_reg, fn, overwrite = T)
}

dbem_files |> 
  map(\(x) regrid_land_mask(x, grid_1deg, grid_1deg, method = "bilinear"))


# Find relevant raster files (tc and tcb) ---------------------------------
raster_files <- list.files("/rd/gem/private/fishmip_outputs/ISIMIP3a/global",
                           pattern = "gfdl.*_[def|60a].*_tc[_|b].*", 
                           recursive = T, full.names = T)


# Define output folder ----------------------------------------------------
out_folder <- "/rd/gem/private/fishmip_outputs/ISIMIP3a/regional/SouthernOcean"


# Extract data for the Southern Ocean -------------------------------------
so_ext <- ext(-180, 180, -80, -40)

# Function extracting raster data using extent and saving as csv file
crop_ras_df <- function(file_path_input, output_folder, crop_ext, reg_name,
                        annual = TRUE){
  # Inputs:
  # - file_path_input (character) Full path to location of input raster file
  # - output_folder (character) Path to folder directory where csv files will 
  # be stored
  # - crop_ext (SpatExtent/SpatRaster/SpatVector) Bounding box, raster or 
  # vector used to crop raster file
  # - reg_name (character) Name of region name to be used in file naming. The 
  # word "global" will be replaced by this region name
  # - annual (boolean) Default is TRUE. If set to TRUE, yearly summaries of the
  # extracted data will be saved. If set to FALSE, data will only be extracted 
  # and aggregation will be performed.
  
  if(!dir.exists(output_folder)){
    dir.create(output_folder)
  }
  
  # Change name to be annual
  reg_name <- str_to_lower(reg_name) |> 
    str_replace_all(" ", "-") 
  
  # Create full path to save cropped data
  fn <- file.path(output_folder, basename(file_path_input)) |> 
    str_replace("_global_", paste0("_", reg_name, "_")) |> 
    str_replace(".nc", ".parquet")
  
  if(annual){
    fn <- fn |> 
      str_replace(paste0(reg_name, "_monthly_"), 
                  paste0(reg_name, "_annual_"))
  }
  
  # Get variable name from file name
  var <- str_extract(fn, paste0("_.*[arcmin|default]_(.*)_", reg_name), 
                     group = 1)
  
  # Load raster and crop it
  ras <- rast(file_path_input) |> 
    crop(crop_ext) |> 
    as.data.frame(xy = T, time = T) |> 
    pivot_longer(!x:y, names_to = "time", values_to = var) |> 
    mutate(time = as_date(time))
  
  if(annual){
    ras <- ras |> 
      mutate(year = year(time)) 
    # TCB - Total Consumer Biomass will be averaged per grid cell per year
    if(var == "tcb"){
      ras <- ras |> 
        summarise(!!sym(var) := mean(!!sym(var), na.rm = T), 
                  .by = c(x, y, year))
      # TC - Total Catches will be summed per grid cell per year
      }else if(var == "tc"){
        ras <- ras |> 
          summarise(!!sym(var) := sum(!!sym(var), na.rm = T), 
                    .by = c(x, y, year))
    }
  }
  
  # Save cropped data as parquet
  ras |>
    write_parquet(fn)
}

# Applying function to all raster files
raster_files |> 
  map(\(x) crop_ras_df(x, out_folder, so_ext, "southern-ocean"))








# Define folders containing relevant data ---------------------------------
mask_files <- list.files(
  "/rd/gem/private/shared_resources/grid_cell_area_ESMs/isimip3a", 
  pattern = "csv$", full.names = T)


mem_files <- list.files(out_folder, pattern = "[monthl|annual]", full.names = T)


# Loading masks -----------------------------------------------------------
mask <- read.csv(mask_files[2]) |> 
  filter(y <= -40 & y >= -80)
  
mem_df <- fread(mem_files[1]) |> 
  left_join(mask, by = c("x", "y")) |> 
  group_by(time) |> 
  summarise(mean_tc = weighted.mean(tc, cellareao))




mask |> 
  ggplot(aes(x, y, fill = cellareao))+geom_tile()




eco <- list.files("/rd/gem/private/fishmip_outputs/ISIMIP3a/global/EcoOcean/",
           pattern = "default_t[d|p]c_*", full.names = T,
           recursive = T)

tdc <- rast(eco[1]) |> 
  crop(so_ext)

tpc <- rast(eco[2]) |> 
  crop(so_ext)


test <- tdc+tpc
plot(test[[1]])


tic()
ras <- rast(file.path(base_dir, "boats_gfdl-mom6-cobalt2_obsclim_nat_default_tcb_global_monthly_1961_2010.nc")) |> 
  crop(so_ext) |> 
  as.data.frame(xy = T, time = T)
toc()

tic()
ras_df <- rast(file.path(base_dir, "boats_gfdl-mom6-cobalt2_obsclim_nat_default_tcb_global_monthly_1961_2010.nc")) |> 
  as.data.frame(xy = T, time = T) |> 
  filter(y <= -40 & y >= -80)
toc()




ras_weighted <- weighted.mean(ras, mask, na.rm = T)


plot(mask)

plot(crop(mask, ext(-180, 180, -80, -40)))

plot(crop(ras[[1]], ext(-180, 180, -80, -40)))
