# Calculating time series from global MEM outputs
# Author: Denisse Fierro Arcos
# Date last update: 2026-05-13


# Loading libraries -------------------------------------------------------
library(terra)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(arrow)
library(ggplot2)


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



# Calculating total catches (tc) from EcoOcean outputs --------------------
td_eo_files <- list.files(
  "/rd/gem/private/fishmip_outputs/ISIMIP3a/global/EcoOcean",
  pattern = "gfdl.*[soc|nat]_[def|60a].*_tdc.*", recursive = T, 
  full.names = T)

tp_eo_files <- list.files(
  "/rd/gem/private/fishmip_outputs/ISIMIP3a/global/EcoOcean",
  pattern = "gfdl.*[soc|nat]_[def|60a].*_tpc.*", recursive = T, 
  full.names = T)


# Function calculating "Total Catch Density" (tc) from pelagic and demersal
# catches
tc_calc <- function(tdc_file, tpc_file, save_tc = T){
  # Inputs:
  # - tdc_file (character) Full path to location of raster file containing
  # total demersal catches
  # - tpc_file (character) Full path to location of raster file containing
  # total pelagic catches
  # - save_tc (boolean) Default is TRUE. If set to TRUE, total catch density
  # will be saved to the same directory where total demersal catches are stored.
  # If set to FALSE, total catches will not be saved, just returned.
  
  tdc <- rast(tdc_file)
  tpc <- rast(tpc_file)
  tc <- tdc+tpc
  
  # Save outputs in the same folders as inputs
  if(save_tc){
    writeCDF(tc, str_replace(tdc_file, "_tdc_", "_tc_"), overwrite = T,
             varname = "tc", longname = "Total Catch Density", unit = "g m-2")
  }else{
    varnames(tc) <- "tc"
    longnames(tc) <- "Total Catch Density"
    names(tc) <- names(tc) |> 
      str_replace_all("tdc_", "tc_")
    return(tc)
  }
}

tc_calc(str_subset(td_eo_files, "60arc"), str_subset(tp_eo_files, "60arc"))
tc_calc(str_subset(td_eo_files, "default"), str_subset(tp_eo_files, "default"))


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
    crop(crop_ext) 
  
  ras_unit <- unique(units(ras))
  
  ras <- ras |> 
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
  
  # Adding units to data frame
  ras <- ras |> 
    mutate(unit = ras_unit)
  
  # Save cropped data as parquet
  ras |>
    write_parquet(fn)
}

# Applying function to all raster files
raster_files |> 
  map(\(x) crop_ras_df(x, out_folder, so_ext, "southern-ocean"))



# Calculating time series -------------------------------------------------
## Loading mask files containing regions of interest ----------------------
mask_1deg <- read_csv_arrow(list.files(
  "/rd/gem/private/shared_resources/SouthernOceanMasks", 
  pattern = "gfdl-mom6.*60arcmin.*csv$", full.names = T)) |> 
  mutate(so = ifelse(!is.na(ccamlr_name), "Southern Ocean", NA))

mask_025deg <- read_csv_arrow(list.files(
  "/rd/gem/private/shared_resources/SouthernOceanMasks", 
  pattern = "gfdl-mom6.*15arcmin.*csv$", full.names = T)) |> 
  mutate(so = ifelse(!is.na(ccamlr_name), "Southern Ocean", NA))


## Getting list of files containing global MEM outputs --------------------
mem_files <- list.files(out_folder, pattern = "southern-ocean_annual", 
                        full.names = T)

#Define groups to be processed
grouping <- c("ccamlr_name", "mpa", "ccamlr_sub_name", "eez", "so")

#Define output folder
out_folder <- "/rd/gem/public/fishmip/aceas_legacy/global_mems"
if(!dir.exists(out_folder)){
  dir.create(out_folder)
}

#Looping through each FishMIP model
out_ts <- file.path(out_folder, "timeseries")
if(!dir.exists(out_ts)){
  dir.create(out_ts)
}

for(m in mem_files){
  fn <- basename(m)
  for(group in grouping){
    #Select correct area of grid cell and keep only information for grid cells
    #for the selected grouping only 
    if(str_detect(m, "60arcmin")){
      area_grid <- mask_1deg |> 
        select(x, y, cellareao, !!sym(group)) |> 
        drop_na(!!sym(group))
      res <- "1deg"
      print(paste0("processing '", fn, "' file, using 1 degree mask"))
    }else if(str_detect(m, "default")){
      area_grid <- mask_025deg |> 
        select(x, y, cellareao, !!sym(group)) |> 
        drop_na(!!sym(group))
      res <- "025deg"
      print(paste0("processing '", fn, "' file, using 0.25 degree mask"))
    }
    
    varname <- str_extract(fn, "[n|t]_([a-z]+)_southern", group = 1)
    model_name <- str_extract(fn, "(^[a-z]+)_gfdl", group = 1)
    
    df_model <- read_parquet(m) |> 
      mutate(mem_name = model_name) |> 
      left_join(area_grid, by = c("x", "y")) |> 
      drop_na(!!sym(group)) |> 
      #Calculate weighted mean for output variable
      summarise(mean_val = weighted.mean(!!sym(varname), cellareao, na.rm = T), 
                std_val = sd(!!sym(varname), na.rm = T),
                .by = c(mem_name, year, !!sym(group), unit)) |> 
      #Calculate percentage change
      mutate(variable = varname, resolution = res)
    
    fout <- file.path(out_ts, str_replace(fn, "_southern-ocean_annual_", 
                                          paste0("_", group, "_annual-ts_")))
    
    df_model |> 
      write_parquet(fout)
  }
}


# Creating a single file for each simulation ------------------------------
nat_df <- list.files(out_ts, pattern = str_c("nat.*annual-ts_"), 
                     full.names = T) |> 
  map(\(x) read_parquet(x)) |> 
  bind_rows()

nat_df |> 
  write_parquet(file.path(
    out_folder, 
    "gfdl-mom6-cobalt2_obsclim_nat_all-regs_yearly_perc_bio_change.parquet"))

histsoc_df <- list.files(out_ts, pattern = str_c("histsoc.*annual-ts_"), 
                         full.names = T) |> 
  map(\(x) read_parquet(x)) |> 
  bind_rows()

histsoc_df |> 
  write_parquet(file.path(
    out_folder, 
    "gfdl-mom6-cobalt2_obsclim_histsoc_all-regs_yearly_perc_bio_change.parquet"))



