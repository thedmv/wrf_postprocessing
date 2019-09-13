import matplotlib
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
import scipy.io.netcdf as nc
import numpy as np

# Get wrfout name
ncflist = ['../wrfout_d03_2016-07-13_17:00:00', 
           '../wrfout_d03_2016-07-14_17:00:00',
           '../wrfout_d03_2016-07-18_17:00:00',
           '../wrfout_d03_2016-07-20_17:00:00']
for filename in ncflist:
    
    wrfobj = nc.netcdf_file(filename, 'r')
    var = np.squeeze(wrfobj.variables['T2'].data)
    lon = np.squeeze(wrfobj.variables['XLONG'][:])
    lat = np.squeeze(wrfobj.variables['XLAT'][:])

    # To plot over a map, we first must create a map instance.
    nudgelat = 0.22
    mp = Basemap(llcrnrlon = np.min(lon),
                 llcrnrlat = np.min(lat) + nudgelat,
                 urcrnrlon = np.max(lon),
                 urcrnrlat = np.max(lat) - nudgelat,
                 projection = 'lcc',
                 rsphere = (6378137.00,6356752.3142),
                 lat_1 = wrfobj.TRUELAT1, 
                 lat_2 = wrfobj.TRUELAT2, 
                lat_0 = wrfobj.CEN_LAT,
                lon_0 = wrfobj.STAND_LON)

    # Create a new figure
    fig1 = plt.figure()

    # Now we can plot out data into the map instance.
    mp.pcolormesh(lon, lat, var, latlon=True, vmin = 295, vmax = 315)
    
    # Load the shapefile 
    shp = mp.readshapefile('dtl_counties_northeast', 'counties')

    # There are many colormaps available.
    plt.set_cmap('RdBu_r')

    # We can also add a colorbar
    mp.colorbar()
    datetitle = filename.split('_')[2] + '_' + filename.split('_')[3].split(":")[0] + "00"
    figname = './wrfvar_t2' + "__" + datetitle
    plt.title('T2 (K) ' + datetitle)
    fig1.savefig(figname, dpi = 250)

    # Close netcdf obj
    wrfobj.close()


