#!/bin/bash

#PBS -P vf71
#PBS -q normalbw
#PBS -l ncpus=8
#PBS -l mem=60GB
#PBS -l walltime=00:10:00
#PBS -l storage=gdata/vf71+gdata/xp65
#PBS -M lilian.fierroarcos@utas.edu.au
#PBS -m abe
#PBS -l wd

module use /g/data/xp65/public/modules
module load conda/analysis3-26.01
python3 01_convert_zarr_to_csv.py
