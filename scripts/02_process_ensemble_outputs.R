## Processing ensemble outputs
# Author: Denisse Fierro Arcos
# Date: 2026-02-18

# Load all spatial data files (exclude timeseries files)
spatial_files <- list.files("data/ensemble_outputs/", full.names = TRUE)

# Read and combine all spatial data
maps_data <- spatial_files |>
  map(fread) |>
  bind_rows() |> 
  select(!c(rowid, name_merge)) |> 
  mutate(region_name = str_remove(region_name, ", .*")) |> 
  mutate(region_name = str_remove(region_name, " Ocean")) |> 
  # Add tooltip for interactive maps
  mutate(tooltip = paste0(
    "Region: ", region_name, "\n",
    "Lon: ", round(longitude, 2), "°, Lat: ", round(latitude, 2), "°\n",
    "Mean change: ", round(mean_change, 1), "%\n",
    "Range: [", round(min_change, 1), ", ", round(max_change, 1), "]%\n",
    "SD: ±", round(sd_change, 1), "%"))

maps_data |> 
  write_csv("data/ensemble_perc_change_fish_bio_all-ssp_mid-end-century_all-reg.csv")
