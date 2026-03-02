## Processing ensemble outputs
# Author: Denisse Fierro Arcos
# Date: 2026-02-18


# Loading libraries -------------------------------------------------------
library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(stringr)
library(purrr)
library(sf)
library(CCAMLRGIS)


# Prepare datasets for use in app -----------------------------------------
# Using FAO report outputs
fao_data <- file.path("/rd/gem/private/users/camillan/FAO_Report/ensemble",
                      "ensemble_perc_bio_change_data_map_tiles.csv")

maps_data <- read_csv(fao_data, col_select = c(longitude:sd_change, NAME_EN)) |> 
  # Filtering Southern Ocean 
  filter(latitude <= -40) |> 
  # Editing names of FAO statistical areas
  mutate(fao = case_when(str_detect(NAME_EN, "Antarctic") ~ 
                           str_remove(str_remove(NAME_EN, ", Antarctic"), 
                                      " Ocean"), T ~ NA)) |> 
  select(!NAME_EN) |> 
  #Calculate Coefficient of variation (indicator of high uncertainty)
  mutate(cv = ifelse(mean_change != 0, sd_change/abs(mean_change), NA),
         cv_mask = ifelse(cv > 1, 1, NA), .before = fao)

# Load CCAMLR datasets to extract info about spatial management areas
ccamlr_areas <- load_ASDs() |> 
  select(GAR_Short_Label) |> 
  rename(subregion = GAR_Short_Label)

eez <- load_EEZs() |> 
  mutate(eez = str_remove_all(str_remove(GAR_Name, "EEZ "), " \\(.*"), 
         .before = geometry) |> 
  select(eez)

ccamlr_mpas <- read_sf("data/map_layers/ccamlr_mpas_wgs84.shp") |> 
  select(!mpa_code)

# Merging information about CCAMLR areas
maps_grid <- maps_data |> 
  distinct(longitude, latitude) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) |> 
  st_join(ccamlr_mpas) |> 
  st_transform(st_crs(ccamlr_areas)) |> 
  st_join(ccamlr_areas) |> 
  st_join(eez) |> 
  st_drop_geometry()

# Adding to original data
maps_data <- maps_data |> 
  left_join(maps_grid, by = c("longitude", "latitude"))

# Saving data frame with CCAMLR management areas data
maps_data |>
  write_csv(
    "data/ensemble_perc_change_fish_bio_all-ssp_mid-end-century_all-reg.csv")


### Create summary statistics table ---------------------------------------
summary_stats <- maps_data |>
  pivot_longer(fao:eez, names_to = "area_type", values_to = "area_name") |> 
  group_by(area_name, scenario, decade) |>
  summarise(mean_change = mean(mean_change, na.rm = TRUE),
            min_change = min(min_change, na.rm = TRUE),
            max_change = max(max_change, na.rm = TRUE),
            median_change = median(median_change, na.rm = TRUE),
            sd_change = mean(sd_change, na.rm = TRUE), n_cells = n(), 
            .groups = "drop") |>
  arrange(scenario, decade, area_name)

# Saving data frame with summary statistics
summary_stats |>
  write_csv("data/ensemble_perc_change_summ-stats_all-ssp_mid-end-century_all-reg.csv")


## Temporal data ----------------------------------------------------------
# Load all timeseries files (exclude files with spatial data)
ts_data <- list.files("data/ensemble_outputs", 
                       pattern = "^mean_ensemble_perc_change_fish_bio.*",
                       full.names = TRUE) |> 
  map(fread) |> 
  bind_rows() |> 
  mutate(region_name = str_remove(name, ", .*")) |> 
  mutate(region_name = str_remove(region_name, " Ocean")) |> 
  select(!c(continent, fao_official, name)) |>
  mutate(tooltip = paste0(
    "Year: ", year, "\n",
    "Mean change: ", round(mean_change, 1), "%\n",
    "SD: ±", round(sd_change, 1), "%"
  ))
  
ts_data |> 
  write_csv("data/mean_ensemble_perc_change_fish_bio_timeseries_all-ssp_all-reg_1950-2100.csv")
