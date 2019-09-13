# source("./ccnymwr_functions.R")

#### Parameters ####

# Time and location parameters
CCNY = c(40.821519, -73.948184)
BRONX = c(40.872481, -73.893522)
QUEENS = c(40.734335, -73.815856)
STATENISLAND = c(40.604014, -74.148499)
locations = list(CCNY, BRONX, QUEENS, STATENISLAND)
tz = "UTC"

# Final heights for interpolations
# ccnylidarhts = 
nymesolidarhts = c(seq(100, 1000, 25), seq(1050, 7000, 50)) # NYMesoNet Windcube heights
ccnymwrheights = c(seq(0, 500, 50), seq(600, 2000, 100), seq(2250, 10000, 250)) + 65 # Microwave Radiometer height
bronmwrheights = c(seq(0, 500, 50), seq(600, 2000, 100), seq(2250, 10000, 250)) + 59.31 
queemwrheights = c(seq(0, 500, 50), seq(600, 2000, 100), seq(2250, 10000, 250)) + 52.89
stismwrheights = c(seq(0, 500, 50), seq(600, 2000, 100), seq(2250, 10000, 250)) + 34.43
ccnywlheights = seq(50, 2975, 25) + 65 

# 2019-06-05
# Date.Time issues: Use the START_DATE global attribute along with the XTIME variable to determine the forecast length.
#                   Use the filenames of the wrfout_d0* files to get the Date.Time or "valid times"
#                   This means that the Load05 script needs to be in the same location as the wrfout_d0* scripts.

#### Functions ####

wrfvarinterp <- function(wrfvar, heights.out) {
    # This function interpolates the WRF vertical columns to selected heights.
    # wrfvar        - data frame of the wrf data created by wrf2df functions that calculate
    #                 the vertical profile at a specific point from the WRF variable.
    # heights.out   - vector of heights that the WRF profile is to be interpolated to.
    
    wrfvar.interp = list()
    
    # Get the wrf heights
    wrfhts = char2height2(names(wrfvar %>% select(-(Date.Time:sec))))
    for (roww in 1:dim(wrfvar)[1]) {
        wrfvar.interp[[roww]] = approx(x = wrfhts, 
                                       y = wrfvar[roww, ] %>% select(-(Date.Time:sec)), 
                                       xout = heights.out)$y
    }
    # No interpolation given for the first MWR level at 0m (AIL)
    wrfvar.interp = as.data.frame(do.call(rbind, wrfvar.interp))
    heights.out.names = paste0("X", heights.out)
    names(wrfvar.interp) = heights.out.names
    wrfvar.interp = cbind(wrfvar.interp, select(wrfvar, Date.Time:sec)) %>% select(Date.Time:sec, heights.out.names)
}

wrf2df_qmix <- function(file_q, file_znu, poi, tz, nc.dt) {
    # Get the netcdf ID's for the temperature and the heights
    ncid = nc_open(file_q) # Dimensions are (XLAT, XLON, level, time)
    
    # Get times and locations of the WRF output.
    #nc.date.time = as.POSIXct(x = ncvar_get(ncid, "XTIME") * 60, 
    #                          tz = "UTC", 
    #                          origin = ncatt_get(ncid, 0)$START_DATE)
    nc.date.time = nc.dt
    attr(nc.date.time, "tzone") = tz
    nc.xlat = ncvar_get(ncid, "XLAT")
    nc.xlon = ncvar_get(ncid, "XLONG")
    
    # Find the indices of the WRF matrix of the closest point to the point of interest
    closest = getIndex(poi[1], poi[2], nc.xlat, nc.xlon)
    
    # Calculate Potential temperature
    nc.qmix = data.frame(t((ncvar_get(ncid, "QVAPOR") * 1000)[closest[1], closest[2], , ]))
    colw = names(nc.qmix)
    nc.qmix$Date.Time = nc.date.time
    nc.qmix = dateTimeCol(nc.qmix) %>% select(Date.Time, year:sec, colw)
    nc_close(ncid) # Close the temperature netCDF
    
    # Get the vertical levels
    ncid = nc_open(file_znu)
    nc.hts = round(eta_to_hts(ncvar_get(ncid, "ZNU")[, 1]), 0)
    nc_close(ncid)
    
    # Rename columns with heights of the vertical levels
    height.names = paste0("X", nc.hts) # First level is 0 m
    
    names(nc.qmix) = c(names(nc.qmix %>% select(Date.Time:sec)), height.names)
    
    return(nc.qmix)
}

wrf2df_theta <- function(file_t, file_znu, poi, tz, nc.dt) {
    # Get the netcdf ID's for the temperature and the heights
    ncid = nc_open(file_t) # Dimensions are (XLAT, XLON, level, time)
    
    # Get times and locations of the WRF output.
    #nc.date.time = as.POSIXct(x = ncvar_get(ncid, "XTIME") * 60, 
    #                          tz = "UTC", 
    #                          origin = ncatt_get(ncid, 0)$START_DATE)
    nc.date.time = nc.dt
    attr(nc.date.time, "tzone") = tz
    nc.xlat = ncvar_get(ncid, "XLAT")
    nc.xlon = ncvar_get(ncid, "XLONG")
    
    # Find the indices of the WRF matrix of the closest point to the point of interest
    closest = getIndex(poi[1], poi[2], nc.xlat, nc.xlon)
    
    # Calculate Potential temperature
    nc.theta = data.frame(t((ncvar_get(ncid, "T") + 300)[closest[1], closest[2], , ]))
    colw = names(nc.theta)
    nc.theta$Date.Time = nc.date.time
    nc.theta = dateTimeCol(nc.theta) %>% select(Date.Time, year:sec, colw)
    nc_close(ncid) # Close the temperature netCDF
    
    # Get the vertical levels
    ncid = nc_open(file_znu)
    nc.hts = round(eta_to_hts(ncvar_get(ncid, "ZNU")[, 1]), 0)
    nc_close(ncid)
    
    # Rename columns with heights of the vertical levels
    height.names = paste0("X", nc.hts) # First level is 0 m
    
    names(nc.theta) = c(names(nc.theta %>% select(Date.Time:sec)), height.names)
    
    return(nc.theta)
}

wrf2df_metwind <- function(file_wind, file_t, file_znu, poi, tz, u_v, nc.dt) {
    # Get the netcdf ID's for the temperature and the heights
    # u_v  which component of the wind to get, 1 for u, 2 for v
    ncid = nc_open(file_wind) # Dimensions are (XLAT, XLON, level, time)
    
    # Get the times and coordinates from the temperature variable. # FIX THIS!!!!!
    ncidt = nc_open(file_t)
    # Get times and locations of the WRF output.
    #nc.date.time = as.POSIXct(x = ncvar_get(ncidt, "XTIME") * 60, 
    #                          tz = "UTC", 
    #                          origin = ncatt_get(ncidt, 0)$START_DATE)
    nc.date.time = nc.dt
    attr(nc.date.time, "tzone") = tz
    nc.xlat = ncvar_get(ncidt, "XLAT")
    nc.xlon = ncvar_get(ncidt, "XLONG")
    
    # Find the indices of the WRF matrix of the closest point to the point of interest
    closest = getIndex(poi[1], poi[2], nc.xlat, nc.xlon)
    
    # Get winds
    nc.wind = data.frame(t((ncvar_get(ncid, "uvmet"))[closest[1], closest[2], , , u_v])) # (XLAT, XLONG, lev, Time, u(1)/v(2))
    colw = names(nc.wind)
    nc.wind$Date.Time = nc.date.time
    nc.wind = dateTimeCol(nc.wind) %>% select(Date.Time, year:sec, colw)
    nc_close(ncid) # Close the temperature netCDF
    
    # Get the vertical levels
    ncid = nc_open(file_znu)
    nc.hts = round(eta_to_hts(ncvar_get(ncid, "ZNU")[, 1]), 0)
    nc_close(ncid)
    
    # Rename columns with heights of the vertical levels
    height.names = paste0("X", nc.hts) # First level is 0 m
    names(nc.wind) = c(names(nc.wind %>% select(Date.Time:sec)), height.names)
    
    return(nc.wind)
}

wrf2df_metwind2 <- function(file_wind, file_znu, poi, tz, u_v, nc.dt) {
    # Get the netcdf ID's for the temperature and the heights
    # u_v  which component of the wind to get, 1 for u, 2 for v
    ncid = nc_open(file_wind) # Dimensions are (XLAT, XLON, level, time)

    # Get times and locations of the WRF output.
    #nc.date.time = as.POSIXct(x = ncvar_get(ncid, "XTIME") * 60,
    #                          tz = "UTC",
    #                          origin = ncatt_get(ncid, 0)$START_DATE)
    nc.date.time = nc.dt
    attr(nc.date.time, "tzone") = tz
    nc.xlat = ncvar_get(ncid, "XLAT")[, , 1]
    nc.xlon = ncvar_get(ncid, "XLONG")[, , 1] 

    # Find the indices of the WRF matrix of the closest point to the point of interest
    closest = getIndex(poi[1], poi[2], nc.xlat, nc.xlon)

    # Get winds
    nc.wind = data.frame(t((ncvar_get(ncid, "uvmet"))[closest[1], closest[2], , , u_v])) # (XLAT, XLONG, lev, Time, u(1)/v(2))
    colw = names(nc.wind)
    nc.wind$Date.Time = nc.date.time
    nc.wind = dateTimeCol(nc.wind) %>% select(Date.Time, year:sec, colw)
    nc_close(ncid) # Close the uvmet netCDF

    # Get the vertical levels
    ncid = nc_open(file_znu)
    nc.hts = round(eta_to_hts(ncvar_get(ncid, "ZNU")[, 1]), 0)
    nc_close(ncid)

    # Rename columns with heights of the vertical levels
    height.names = paste0("X", nc.hts) # First level is 0 m
    names(nc.wind) = c(names(nc.wind %>% select(Date.Time:sec)), height.names)

    return(nc.wind)
}

wrf2df_vertwind <- function(file_wind, file_znw, poi, tz, nc.dt) {
    # Get the netcdf ID's for the temperature and the heights
    # u_v  which component of the wind to get, 1 for u, 2 for v
    ncid = nc_open(file_wind) # Dimensions are (XLAT, XLON, level, time)
    
    # Get times and locations of the WRF output.
    #nc.date.time = as.POSIXct(x = ncvar_get(ncid, "XTIME") * 60, 
    #                          tz = "UTC", 
    #                          origin = ncatt_get(ncid, 0)$START_DATE)
    nc.date.time = nc.dt
    attr(nc.date.time, "tzone") = tz
    nc.xlat = ncvar_get(ncid, "XLAT")
    nc.xlon = ncvar_get(ncid, "XLONG")
    
    # Find the indices of the WRF matrix of the closest point to the point of interest
    closest = getIndex(poi[1], poi[2], nc.xlat, nc.xlon)
    
    # Get winds
    nc.wind = data.frame(t((ncvar_get(ncid, "W"))[closest[1], closest[2], , ])) # (XLAT, XLONG, lev, Time)
    colw = names(nc.wind)
    nc.wind$Date.Time = nc.date.time
    nc.wind = dateTimeCol(nc.wind) %>% select(Date.Time, year:sec, colw)
    nc_close(ncid) # Close the temperature netCDF
    
    # Get the vertical levels
    ncid = nc_open(file_znw)
    nc.hts = round(eta_to_hts(ncvar_get(ncid, "ZNW")[, 1]), 0)
    nc_close(ncid)
    
    # Rename columns with heights of the vertical levels
    height.names = paste0("X", nc.hts)
    names(nc.wind) = c(names(nc.wind %>% select(Date.Time:sec)), height.names)
    
    return(nc.wind)
}

wrfdf_wswd <- function(wrfdf_u, wrfdf_v, ws_or_wd) {
    # This function performs the operations necessary to get the:
    # 1. Wind speed,
    # 2. Wind direction,
    # 
    # wrfdf_u and wrfdf_v are data frames of the WRF variables "U" and "V" that have columns: Date.Time, year
    # mon, day, hour, min, sec, and the respective heights "X0000..." where "0000..."
    # refers to the height.
    
    # Calculate the wind speed
    if(ws_or_wd == "wspd") {
        wspd = cbind(wrfdf_u %>% select(Date.Time:sec), sqrt((wrfdf_u %>% select(-(Date.Time:sec))) ^ 2 + (wrfdf_v %>% select(-(Date.Time:sec))) ^ 2))
        return(wspd)
    } else if( ws_or_wd == "wdir") {
        wdir = cbind(wrfdf_u %>% select(Date.Time:sec),
                     270 - (atan2(as.matrix(wrfdf_v %>% select(-(Date.Time:sec))), 
                                  as.matrix(wrfdf_u %>% select(-(Date.Time:sec))) ) * 180/pi))
        return(wdir)
    } else {
        print("Invalid input")
    }
}

wrfscalar_hw2018 = function(file_t, file_q, file_znu, poi, tz, targethts, nc.dt) {
    # This function is a wrapper fo rcreating WRF potential temperature and water vapor
    # mixing ratio.
    
    # Extract the weather variables
    nc.theta = wrf2df_theta(file_t, file_znu, poi, tz, nc.dt)
    nc.qmix = wrf2df_qmix(file_q, file_znu, poi, tz, nc.dt)
    
    # Interpolate to heights of the CCNY Microwave Radiometer
    tt = wrfvarinterp(nc.theta, targethts)
    qq = wrfvarinterp(nc.qmix, targethts)
    
    wrfoutsc = list(th = tt, qmix = qq)
    
    return(wrfoutsc)
}

wrfwinds_hw2018b = function(file_wind, file_vertwind, file_znu, file_znw, poi, tz, targethts, nc.dt) {
    # This function is a wrapper for creating WRF wind dataframes (u, v, w, horizontal wspd, and wind direction)
    # Input: Filenames for the different netcdfs that contain the variables of interest.
    # Extract the rotated winds horizontal winds and the vertical winds

    nc.uwind = wrf2df_metwind2(file_wind, file_znu, poi, tz, 1, nc.dt)
    nc.vwind = wrf2df_metwind2(file_wind, file_znu, poi, tz, 2, nc.dt)
    nc.wwind = wrf2df_vertwind(file_vertwind, file_znw, poi, tz, nc.dt)
    # wrfwindhts = char2height2(names(nc.uwind %>% select(-(Date.Time:sec))))
    # Interpolate to heights of the NYMesoNet wind lidar - BRONX
    uu = wrfvarinterp(nc.uwind, targethts)
    vv = wrfvarinterp(nc.vwind, targethts)
    ww = wrfvarinterp(nc.wwind, targethts)
    wspd = wrfdf_wswd(uu, vv, "wspd") # Convert the uu and vv to wind speed
    wdir = wrfdf_wswd(uu, vv, "wdir") # Convert the uu and vv to wind direction

    wrfout = list(uu = uu, vv = vv, ww = ww, wspd = wspd, wdir = wdir)
    return(wrfout)
}

wrfwinds_hw2018 = function(file_wind, file_t, file_vertwind, file_znu, file_znw, poi, tz, targethts, nc.dt) {
    # This function is a wrapper for creating WRF wind dataframes (u, v, w, horizontal wspd, and wind direction)
    # Input: Filenames for the different netcdfs that contain the variables of interest.
    # Extract the rotated winds horizontal winds and the vertical winds
    
    nc.uwind = wrf2df_metwind(file_wind, file_t, file_znu, poi, tz, 1, nc.dt)
    nc.vwind = wrf2df_metwind(file_wind, file_t, file_znu, poi, tz, 2, nc.dt)
    nc.wwind = wrf2df_vertwind(file_vertwind, file_znw, poi, tz, nc.dt)
    # wrfwindhts = char2height2(names(nc.uwind %>% select(-(Date.Time:sec))))
    # Interpolate to heights of the NYMesoNet wind lidar - BRONX
    uu = wrfvarinterp(nc.uwind, targethts)
    vv = wrfvarinterp(nc.vwind, targethts)
    ww = wrfvarinterp(nc.wwind, targethts)
    wspd = wrfdf_wswd(uu, vv, "wspd") # Convert the uu and vv to wind speed
    wdir = wrfdf_wswd(uu, vv, "wdir") # Convert the uu and vv to wind direction
    
    wrfout = list(uu = uu, vv = vv, ww = ww, wspd = wspd, wdir = wdir)
    return(wrfout)
}
