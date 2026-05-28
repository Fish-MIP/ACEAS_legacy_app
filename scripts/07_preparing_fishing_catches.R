# Preparing observed fishing catches 
# Author: Denisse Fierro Arcos
# Date last update: 2026-05-26


# Loading libraries -------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(arrow)


# Loading GFDL masks ------------------------------------------------------
# 1 deg
mask_1deg  <- file.path(
  "/rd/gem/private/shared_resources/SouthernOceanMasks",
  "gfdl-mom6-cobalt2_areacello_60arcmin_southern-ocean_fixed.csv") |> 
  read_csv_arrow() 

area_1deg_fao <- mask_1deg |> 
  drop_na(fao_code) |> 
  summarise(area_m2 = sum(cellareao, na.rm = T), .by = c(fao, fao_code)) |> 
  mutate(resolution = "1deg")

area_1deg_eez <- mask_1deg |> 
  drop_na(eez) |> 
  summarise(area_m2 = sum(cellareao, na.rm = T), .by = eez) |> 
  mutate(resolution = "1deg")

area_1deg_subreg <- mask_1deg |> 
  drop_na(subreg) |> 
  summarise(area_m2 = sum(cellareao, na.rm = T), .by = subreg) |> 
  mutate(resolution = "1deg")

area_1deg_so <- mask_1deg |> 
  drop_na(fao_code) |> 
  summarise(area_m2 = sum(cellareao, na.rm = T)) |> 
  mutate(so = "Southern Ocean", resolution = "1deg")

# 0.25 deg
mask_025deg <- file.path(
  "/rd/gem/private/shared_resources/SouthernOceanMasks",
  "gfdl-mom6-cobalt2_areacello_15arcmin_southern-ocean_fixed.csv") |> 
  read_csv_arrow() 

area_025deg_fao <- mask_025deg |> 
  drop_na(fao_code) |> 
  summarise(area_m2 = sum(cellareao, na.rm = T), .by = c(fao, fao_code)) |> 
  mutate(resolution = "025deg")

area_025deg_eez <- mask_025deg |> 
  drop_na(eez) |> 
  summarise(area_m2 = sum(cellareao, na.rm = T), .by = eez) |> 
  mutate(resolution = "025deg")

area_025deg_subreg <- mask_025deg |> 
  drop_na(subreg) |> 
  summarise(area_m2 = sum(cellareao, na.rm = T), .by = subreg) |> 
  mutate(resolution = "025deg")

area_025deg_so <- mask_025deg |> 
  drop_na(fao_code) |> 
  summarise(area_m2 = sum(cellareao, na.rm = T)) |> 
  mutate(so = "Southern Ocean", resolution = "025deg")

# CCAMLR area totals
area_both_fao <- area_025deg_fao |> 
  bind_rows(area_1deg_fao) 

# EEZ area totals
area_both_eez <- area_025deg_eez |> 
  bind_rows(area_1deg_eez) 

# CCAMLR subregion totals
area_both_subreg <- area_025deg_subreg |> 
  bind_rows(area_1deg_subreg)

# Southern Ocean totals
area_both_so <- area_025deg_so |> 
  bind_rows(area_1deg_so)


# Preparing Watson catch data ---------------------------------------------
catch_watson <- read_csv_arrow(
  "data/observed_catches/catch_histsoc_1869_2017_EEZ_addFAO.csv",
  col_select = c(Year, fao_area, Reported, IUU)) |> 
  filter(fao_area %in% c(48, 88, 58) & Year >= 1961 & Year <= 2010) |> 
  mutate(catch_tonnes = Reported+IUU) |> 
  summarise(catch_tonnes = sum(catch_tonnes, na.rm = T), 
            .by = c(Year, fao_area)) |> 
  rename(year = Year, fao_code = fao_area) |> 
  left_join(area_both_fao, by = "fao_code") |> 
  mutate(catch_g_m2 = (catch_tonnes*1e6)/area_m2)

catch_watson_so <- catch_watson |> 
  summarise(catch_g = sum(catch_tonnes, na.rm = T)*1e6,
            .by = c(year, resolution)) |> 
  left_join(area_both_so, by = "resolution") |> 
  mutate(catch_g_m2 = catch_g/area_m2) |>
  select(year, so, catch_g_m2, resolution)

catch_watson <- catch_watson |> 
  select(year, fao, catch_g_m2, resolution) |> 
  bind_rows(catch_watson_so) |> 
  mutate(source = "IMAS")

# Preparing Pauly catch data ----------------------------------------------
catch_pauly_fao <- list.files("data/observed_catches", "SAU FAO",
                              full.names = T) |> 
  map(\(x) read.csv(x)) |> 
  bind_rows() |> 
  filter(year >= 1961 & year <= 2010 & catch_type != "Discards") |> 
  summarise(catch_g = sum(tonnes, na.rm = T)*1e6, .by = c(year, area_name)) |> 
  mutate(area_name = str_remove_all(area_name, ", Antarctic"),
         area_name = str_remove_all(area_name, " Ocean")) |> 
  rename(fao = area_name) |> 
  left_join(area_both_fao, by = "fao") |> 
  mutate(catch_g_m2 = catch_g/area_m2) 

catch_pauly_so <- catch_pauly_fao |> 
  summarise(catch_g = sum(catch_g, na.rm = T), .by = c(year, resolution)) |> 
  left_join(area_both_so, by = "resolution") |> 
  mutate(catch_g_m2 = catch_g/area_m2) |>
  select(year, so, catch_g_m2, resolution)

catch_pauly_fao <- catch_pauly_fao |> 
  select(year, fao, catch_g_m2, resolution) |> 
  bind_rows(catch_pauly_so) |> 
  mutate(source = "SAUP")

catch_pauly_eez <- list.files("data/observed_catches", "SAU EEZ",
                              full.names = T) |> 
  map(\(x) read.csv(x)) |> 
  bind_rows() |> 
  filter(year >= 1961 & year <= 2010 & catch_type != "Discards") |> 
  summarise(catch_g = sum(tonnes, na.rm = T)*1e6, .by = c(year, area_name)) |> 
  mutate(area_name = str_remove_all(area_name, " \\(.+\\)"),
         area_name = str_replace_all(area_name, " \\& ", " Island and "),
         area_name = ifelse(str_detect(area_name, "Macq"),
                            str_replace_all(area_name, "Isl\\.", "Island"),
                            str_replace_all(area_name, "Isl\\.", "Islands")),
         source = "SAUP") |> 
  rename(eez = area_name) |> 
  left_join(area_both_eez, by = "eez") |> 
  mutate(catch_g_m2 = catch_g/area_m2) |> 
  select(year, eez, catch_g_m2, resolution, source)

catch_pauly <- catch_pauly_eez |> 
  bind_rows(catch_pauly_fao)

# Preparing CCAMLR catch data ---------------------------------------------
catch_ccamlr <- read_csv_arrow("data/observed_catches/Catch.csv") |> 
  filter(year >= 1961 & year <= 2010) |> 
  summarise(catch_g = sum(greenweight_caught_tonne, na.rm = T)*1e6,
            .by = c(year, asd_code)) |> 
  mutate(fao_code = as.numeric(str_extract(asd_code, "\\d{2}"))) |> 
  rename(subreg = asd_code)

catch_ccamlr_sub <- catch_ccamlr |> 
  left_join(area_both_subreg, by = "subreg") |> 
  drop_na(area_m2) |> 
  mutate(catch_g_m2 = catch_g/area_m2) |> 
  select(year, subreg, catch_g_m2, resolution)
  
catch_ccamlr_fao <- catch_ccamlr |> 
  summarise(catch_g = sum(catch_g, na.rm = T),
            .by = c(year, fao_code)) |> 
  left_join(area_both_fao, by = "fao_code") |> 
  mutate(catch_g_m2 = catch_g/area_m2) |> 
  select(year, fao, catch_g_m2, resolution)
            
catch_ccamlr_so <- catch_ccamlr |> 
  summarise(catch_g = sum(catch_g, na.rm = T), .by = year) |> 
  mutate(so = "Southern Ocean") |> 
  left_join(area_both_so, by = "so") |> 
  mutate(catch_g_m2 = catch_g/area_m2) |> 
  select(year, so, catch_g_m2, resolution)

catch_ccamlr <- catch_ccamlr_fao |> 
  bind_rows(catch_ccamlr_sub) |> 
  bind_rows(catch_ccamlr_so) |> 
  mutate(source = "CCAMLR")


# Combine all fishing catches sources -------------------------------------
catch_all <- catch_ccamlr |> 
  bind_rows(catch_pauly) |> 
  bind_rows(catch_watson)


# Saving combined catches -------------------------------------------------
catch_all |> 
  write_parquet(file.path("/rd/gem/public/fishmip/aceas_legacy",
                          "observed_catches_all_sources.parquet"))

