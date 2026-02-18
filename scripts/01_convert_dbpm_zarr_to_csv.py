#!/usr/bin/env python3

# Converting DBPM zarr files into csv files
# Authors: Denisse Fierro Arcos
# Date last updated: 2026-02-17

# Loading libraries
import xarray as xr
import pandas as pd
import numpy as np
from glob import glob

# Getting a list of DBPM output files
dbpm_zarr = glob('../data/dbpm*.zarr')

# Loop through each file
for f in dbpm_zarr:
    # Loading zarr file
    ds = xr.open_zarr(f)
    # Identify variable store in each file
    if 'catch' in f:
        val_colname = 'catches'
    elif 'bio' in f:
        val_colname = 'biomass'
    
    # Transforming zarr file to csv
    df = ds.to_dataframe().dropna(how = 'all').reset_index()
    
    # Reorganise data
    df = pd.melt(df, id_vars = ['lat', 'lon'], 
                 # All columns starting with FAO
                 value_vars = [f for f in df.columns.tolist() if 'fao' in f],
                 var_name = 'region', value_name = val_colname).dropna()
    df['tooltip'] = df.apply(lambda x:
                             f'Lon: {round(x.lon, 2)}°, Lat: {round(x.lat, 2)}°\n{val_colname.capitalize()}: {round(x[val_colname], 2)} tonnes/km²\nRegion:{x.region}',
                             axis = 1)
    
    # Create name for csv file
    fout = f.replace('zarr', 'csv')
    
    # Saving csv
    df.to_csv(fout, index = False)