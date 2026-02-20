## Processing ensemble outputs
# Author: Denisse Fierro Arcos
# Date: 2026-02-18


# Loading libraries -------------------------------------------------------
library(readr)
library(dplyr)
library(data.table)
library(stringr)
library(purrr)
library(sf)
library(CCAMLRGIS)


# Prepare datasets for use in app -----------------------------------------

## Spatial data -----------------------------------------------------------
# Load all spatial data files (exclude timeseries files)
maps_data <- list.files("data/ensemble_outputs/", pattern = "^ensemble",
                        full.names = TRUE) |>
  map(fread) |>
  bind_rows() |> 
  select(!c(rowid, name_merge)) |> 
  mutate(region_name = str_remove(region_name, ", .*")) |> 
  mutate(region_name = str_remove(region_name, " Ocean")) |> 
  #Calculate Coefficient of variation (indicator of high uncertainty)
  mutate(cv = ifelse(mean_change != 0, sd_change/abs(mean_change), NA),
         # Add tooltip for interactive maps
         tooltip = paste0(
           "Lon: ", round(longitude, 2), "°, Lat: ", round(latitude, 2), "°\n",
           "Mean change: ", round(mean_change, 1), "%\n",
           "Range: [", round(min_change, 1), ", ", round(max_change, 1), "]%\n",
           "SD: ±", round(sd_change, 1), "%\n",
           "CV: ", round(cv, 2)))

# Extract unique coordinate pairs in dataset above
coords <- maps_data |> 
  distinct(longitude, latitude) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)


# Load CCAMLR datasets to extract info about spatial management areas
ccamlr_areas <- load_ASDs() |> 
  select(GAR_Short_Label) |> 
  rename(subregion = GAR_Short_Label) |> 
  st_transform(st_crs(coords))
  
ccmalr_mpas <- load_MPAs()|> 
  select(GAR_Short_Label) |> 
  rename(mpa = GAR_Short_Label) |> 
  st_transform(st_crs(coords))

# Join CCMALR datasets to unique coordinates
coords <- coords |> 
  st_join(ccamlr_areas) |> 
  st_join(ccmalr_mpas) |> 
  st_drop_geometry()

maps_data <- maps_data |> 
  left_join(coords, by = c("longitude", "latitude"))

# Saving data frame with CCAMLR management areas data
maps_data |>
  write_csv("data/ensemble_perc_change_fish_bio_all-ssp_mid-end-century_all-reg.csv")


### Create summary statistics table ---------------------------------------
summary_stats <- maps_data |>
  group_by(region_name, scenario, decade) |>
  summarise(
    mean_change = mean(mean_change, na.rm = TRUE),
    min_change = min(min_change, na.rm = TRUE),
    max_change = max(max_change, na.rm = TRUE),
    median_change = median(median_change, na.rm = TRUE),
    sd_change = mean(sd_change, na.rm = TRUE),
    n_cells = n(),
    .groups = "drop"
  ) |>
  arrange(scenario, decade, region_name)

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
