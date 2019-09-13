#!/bin/bash

# Plot only the 18Z runs using NCL
wrffn=$(ls wrfout_d03*18_00_00)
dirname=$(pwd)
result="${dirname%"${dirname##*[!/]}"}" # extglob-free multi-trailing-/ trim
result="${result##*/}" # keep only the name of directory

for ff in $wrffn
do
    sed -e "s;WRFDATA;"$ff";" -e "s;IMGTITLE;"$result";" overlayloop_t2_wind10.ncl > overlay_t2_wind10_plotter.ncl
    ncl overlay_t2_wind10_plotter.ncl
    rm overlay_t2_wind10_plotter.ncl
done
