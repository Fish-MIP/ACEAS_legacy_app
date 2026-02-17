#!/usr/bin/env python3

# Converting DBPM zarr files into csv files
# Authors: Denisse Fierro Arcos
# Date last updated: 2026-02-17

# Loading libraries
import xarray as xr
import pandas as pd
import numpy as np

# Loading zarr file
ds = xr.open_zarr('../data/mean_tot-exp-bio_all_regions_simask_1deg_1961-2010.zarr/')

# Transforming zarr file to csv
df = ds.to_dataframe().dropna(how = 'all').reset_index()
df = pd.melt(df, id_vars = ['lat', 'lon'], 
             value_vars = ['fao-48', 'fao-58', 'fao-88'],
             var_name = 'region', value_name = 'biomass').dropna()
df['tooltip'] = df.apply(lambda x:
                         f'Lon: {round(x.lon, 2)}°, Lat: {round(x.lat, 2)}°\nBiomass: {round(x.biomass, 2)} tonnes/km²\nRegion:{x.region}',
                         axis = 1)

# Saving csv
df.to_csv('../data/dbpm_biomass_clean_1deg_1961-2010.csv', index = False)