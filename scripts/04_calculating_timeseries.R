## Calculating time series from ensemble results
# Author: Denisse Fierro Arcos
# Date: 2026-02-25


# Loading libraries -------------------------------------------------------
library(readr)
library(dplyr)
library(data.table)
library(stringr)
library(purrr)
library(sf)
library(CCAMLRGIS)


# CCAMLR spatial management -----------------------------------------------

# Load CCAMLR datasets to extract info about spatial management areas
ccamlr_areas <- load_ASDs() |> 
  select(GAR_Short_Label) |> 
  rename(subregion = GAR_Short_Label)

# Loading edited EEZ layer including Macquarie Island
eez <- read_sf("data/map_layers/ccamlr_eez_macq_epsg6932.shp")

ccamlr_mpas <- read_sf("data/map_layers/ccamlr_mpas_wgs84.shp") |> 
  select(!mpa_code)


# Load FAO outputs for use in app -----------------------------------------
base_folder <- "/rd/gem/private/users/camillan"
out_folder <- file.path(base_folder, "FAO_Report")


#Getting GFDL grid area
area_gfdl_df <- list.files(out_folder, "gfdl-esm4_60arcmin_global_FAO.csv",
                            recursive = T, full.names = T) |> 
  read_csv(col_select = !ID_mrgd) |> 
  filter(y <= -40) |> 
  mutate(fao = case_when(str_detect(NAME_EN, "Antarctic") ~ 
                           str_remove(str_remove(NAME_EN, ", Antarctic"), 
                                      " Ocean"), T ~ NA)) |> 
  select(!NAME_EN) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE)

#Getting IPSL grid area
area_ipsl_df <- list.files(out_folder, "ipsl-cm6a_60arcmin_global_FAO.csv",
                            recursive = T, full.names = T) |> 
  read_csv(col_select = !ID_mrgd) |> 
  filter(y <= -40) |> 
  mutate(fao = case_when(str_detect(NAME_EN, "Antarctic") ~ 
                           str_remove(str_remove(NAME_EN, ", Antarctic"), 
                                      " Ocean"), T ~ NA)) |> 
  select(!NAME_EN) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE)


# Merging information about CCAMLR areas
area_gfdl_df <- area_gfdl_df |> 
  st_join(ccamlr_mpas) |> 
  st_transform(st_crs(ccamlr_areas)) |> 
  st_join(ccamlr_areas) |> 
  st_join(eez) |> 
  st_drop_geometry() |> 
  mutate(so = ifelse(!is.na(fao), "Southern Ocean", NA))

area_ipsl_df <- area_ipsl_df |> 
  st_join(ccamlr_mpas) |> 
  st_transform(st_crs(ccamlr_areas)) |> 
  st_join(ccamlr_areas) |> 
  st_join(eez) |> 
  st_drop_geometry() |> 
  mutate(so = ifelse(!is.na(fao), "Southern Ocean", NA))


#Getting a list of files containing biomass data
global_files <- list.files(
  file.path(base_folder, "Extract_tcblog10_Data/Output/sumSize_annual",
            "sizeConsidered10g_100kg"),
  pattern = "2024-06-03.*global_10g-100kg.rds", full.names = T, recursive = T)


#Getting a list of MEMs and forcing ESMs 
members <- str_extract(global_files, "annual_(.*)_(h|s)", group = 1) |> 
  unique()

#Define groups to be processed
grouping <- c("fao", "mpa", "subregion", "eez", "so")

#Define output folder
out_folder <- "data/mem_outputs"
if(!dir.exists(out_folder)){
  dir.create(out_folder)
}


#Looping through each FishMIP model
for(m in members){
  for(group in grouping){
    #Select correct area of grid cell and keep only information for grid cells
    #for the selected grouping 
    if(str_detect(m, "ipsl")){
      area_grid <- area_ipsl_df |> 
        select(x, y, cell_area, !!sym(group)) |> 
        drop_na(!!sym(group))
      print(paste0("processing '", m, "' file, using ipsl mask"))
    }else if(str_detect(m, "gfdl")){
      area_grid <- area_gfdl_df |> 
        select(x, y, cell_area, !!sym(group)) |> 
        drop_na(!!sym(group))
      print(paste0("processing '", m, "' file, using gfdl mask"))
    }
    
    #Load all data available for a single FishMIP model
    df_model <- str_subset(global_files, m) |>
      map(~readRDS(.)) |>
      map_df(~bind_rows(.)) |>
      #Add area for each grid cell and within area of interest
      left_join(area_grid, by = join_by(x, y)) |>
      #Calculate weighted mean for biomass per ensemble member per AOI
      group_by(year, mem, esm, scenario, !!sym(group)) |> 
      summarise(mean_bio = weighted.mean(biomass, cell_area, na.rm = T)) |> 
      #Remove rows outside country boundaries
      drop_na(!!sym(group)) 
  
    #Calculating reference period
    ref_bio <- df_model |> 
      ungroup() |> 
      filter(year >= 2005 & year <= 2014) |> 
      group_by(!!sym(group)) |> 
      #Calculate mean per ensemble member
      summarise(ref_bio = mean(mean_bio, na.rm = T))
    
    #Add reference period to weighted mean data frame
    df_model <- df_model |> 
      ungroup() |> 
      left_join(ref_bio, join_by(!!sym(group))) |> 
      #Calculate percentage change
      mutate(perc_change = ((mean_bio-ref_bio)/ref_bio)*100)
    
    #Create name to save file
    f_out <- file.path(out_folder, str_c(m, "_yearly_perc_bio_change_", group, 
                                         "_region.csv"))
    
    #Saving results for each model
    df_model |>
      fwrite(f_out)
  }
}


## Calculating mean ensemble percentage change in biomass per selected group
for(group in grouping){
  bio_data <- list.files(out_folder, pattern = str_c("_yearly_perc_bio_change_",
                                                     group, "_region.csv"),
                         full.names = T, recursive = T) |> 
    map(~fread(.)) |> 
    bind_rows() |> 
    group_by(year, scenario, !!sym(group)) |> 
    #Calculate summary statistics per year, scenario and FAO region
    summarise(min_change = min(perc_change, na.rm = T),
              mean_change = mean(perc_change, na.rm = T),
              median_change = median(perc_change, na.rm = T),
              max_change = max(perc_change, na.rm = T),
              sd_change = sd(perc_change, na.rm = T)) |> 
    #Rounding values to 2 decimal places
    mutate(across(where(is.double), ~round(.x, 2)),
           tooltip = paste0("Year: ", year, "\n",
                            "Mean change: ", round(mean_change, 1), "%\n",
                            "SD: ±", round(sd_change, 1), "%"))
  
  #Create name to save file
  f_out <- file.path("data/ensemble_outputs", 
                     str_c("ensemble_perc_bio_change_", group, "_region.csv"))
  
  #Saving results for each model
  bio_data |>
    write_csv(f_out)
}


list.files("data/ensemble_outputs", pattern = "^ensemble_perc_bio", 
           full.names = T) |> 
  map(fread) |> 
  bind_rows() |> 
  write_csv(
    file.path("data/ensemble_outputs",
              "ensemble_perc_change_fish_bio_ts_all-ssp_all-reg_1950-2100.csv"))

