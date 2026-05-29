# Preparing Mizer data
# Author: Denisse Fierro Arcos
# Date last update: 2026-05-28


# Loading libraries -------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(readr)


# Load mizer outputs ------------------------------------------------------
reg_folder <- "/rd/gem/public/fishmip/aceas_legacy/regional_mems/timeseries"

biomass <- list.files(reg_folder, "mizer.*bsp.*", full.names = T) |> 
  read_csv()

date_correction <- data.frame(
  year_modelled = seq(as_date("1841-01-01"), as_date("2010-12-31"), 
                      by = "years")) |> 
  mutate(n_days = as.numeric(year_modelled-as_date("1841-01-01"))) |> 
  bind_cols(distinct(biomass, time))

biomass <- biomass |> 
  left_join(date_correction, by = "time") |> 
  mutate(year = year(as_date(n_days, origin = "1841-01-01"))) |> 
  filter(year >= 1961) |> 
  select(year, species:q95) |> 
  mutate(unit = "g m-2",
         variable = "tcb",
         long_var = "Biomass by species/functional group")


catches <- list.files(reg_folder, "mizer.*csp.*", full.names = T) |> 
  read_csv() |> 
  left_join(date_correction, by = "time") |> 
  mutate(year = year(as_date(n_days, origin = "1841-01-01"))) |> 
  filter(year >= 1961) |> 
  select(year, species:q95) |> 
  mutate(unit = "g m-2 yr-1",
         variable = "tc",
         long_var = "Catch (landings) by species/functional group")

mizer_data <- biomass |> 
  bind_rows(catches)

mizer_data |> 
  write_parquet(file.path(
    "/rd/gem/public/fishmip/aceas_legacy/regional_mems",
    "mizer_gfdl-mom6-cobalt2_obsclim_histsoc_default_catches-biomass.parquet"))


lab_y <- str_c(unique(biomass$long_var), " (", unique(biomass$unit), ")")

biomass |> 
  ggplot(aes(year))+
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = species), alpha = 0.3)+
  geom_line(aes(y = median, color = species))+
  facet_wrap(~species, scales = "free_y")+
  theme_bw()+
  labs(y = lab_y)+
  theme(legend.position = "none", axis.title.x = element_blank())



lab_y <- str_c(unique(catches$long_var), " (", unique(catches$unit), ")")

catches |> 
  ggplot(aes(year))+
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = species), alpha = 0.3)+
  geom_line(aes(y = median, color = species))+
  facet_wrap(~species, scales = "free_y")+
  theme_bw()+
  labs(y = lab_y)+
  theme(legend.position = "none", axis.title.x = element_blank())
