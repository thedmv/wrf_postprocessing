import matplotlib
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
import scipy.io.netcdf as nc
import numpy as np
import glob


# Get wrfout list; for 14th (EDT) look for July 13 20:00 to July 14 19:00
ncflist = glob.glob('../wrfout_d03_2012-07-13_[2][0123]*') + glob.glob('../wrfout_d03_2012-07-13_[01][0-9]*')

# Get map details from the first file
wrfobj = nc.netcdf_file(ncflist[0], 'r')
lon = np.squeeze(wrfobj.variables['XLONG'].data) # LONGITUDE
lat = np.squeeze(wrfobj.variables['XLAT'].data)  # LATITUDE

# Sum the CM_AC_URB3D {W/m^2} variable for every grid point
ncfobjs = nc.netcdf_file('../wrfinput_d03', 'r')
var = np.array(np.squeeze(ncfobjs.variables['BUILD_HEIGHT'].data)) # VARIABLE

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
mp.pcolormesh(lon, lat, var, latlon=True)
    
# Load the shapefile 
shp = mp.readshapefile('dtl_counties_northeast', 'counties')

# There are many colormaps available.
plt.set_cmap('Greens')

# We can also add a colorbar
mp.colorbar()
#datetitle = filename.split('_')[2] + '_' + filename.split('_')[3].split(":")[0] + "00"
datetitle = "2012-07-14"
figname = './wrfvar_BUILD_HEIGHT' + "__" + datetitle
plt.title('Building Heights (m) ' + datetitle)
fig1.savefig(figname, dpi = 250)

# Close netcdf obj
wrfobj.close()


