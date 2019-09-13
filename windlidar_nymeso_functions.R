source("/mnt/lfs3/projects/wrfruc/dmv.ccny/POSTPROC/ccnymwr_functions.R")
source("/mnt/lfs3/projects/wrfruc/dmv.ccny/POSTPROC/wrf_functions.R")
suppressPackageStartupMessages(library(ncdf4))
suppressPackageStartupMessages(library(stringr))

## TODO 2019-01-18 Add height of each individual station
# Heights for the different Wind Lidars; heights in [m] above sea level.
nymesolidarhts = c(seq(100, 1000, 25), seq(1050, 7000, 50)) # NYMesoNet Windcube heights
nymesolidarhts_bronx = nymesolidarhts + 59.31
nymesolidarhts_queens = nymesolidarhts + 52.89
nymesolidarhts_statenisland = nymesolidarhts + 34.43

read.nymesowindlidar <- function(ncfile, loc, tz) {
    # This function reads the netcdf files from NY MEsonet stations in Bronx, Queens, and Staten Island.
    # ncf - the filename of the netcdf
    # loc - location of the instrument of interest. Choose: "BX", "QU", or "SI".
    #       Each netcdf contains groups, and the 'loc' input specifies the name of
    #       the group within the netcdf.
    
    # The output is a list with two elements: windspeed data frame and a wind direction data frame.
    
    # Set the location of the instrument. also sets the groupname within the netcdf that
    # is required to call the desired variables.
    
    # HERE WE ALSO LIMIT THE HEIGHT LEVEL TO 3000m. WE DO NOT USE MEASUREMENTS ABOVE 3000m
    # TO SAVE MEMORY.
    if (loc == "BX") {
        groupname = "c6_s16"
        # dheight = 59.31
    } else if(loc == "QU") {
        groupname = "c6_s19"
        # dheight = 52.89
    } else if(loc == "SI") {
        groupname = "c10_s20"
        # dheight = 34.43
    } else {
        print("Invalid location.")
    }
    dheight = 0
    # Open the netcdf file
    ncf = nc_open(ncfile)
    # Use the groupname to define a wrapper to call each variable
    vname = function(varname, grp = groupname) {return(paste0(grp, "/", varname))}
    
    # Dimension variables
    datex = paste0(str_extract_all(ncatt_get(ncf, vname("time"))$units, '[:digit:]')[[1]], collapse = '')
    datex = paste0(str_sub(datex, 1, 4), "-", str_sub(datex, 5, 6), "-", str_sub(datex, 7, 8))
    timex = as.POSIXct(ncvar_get(ncf, vname("time"))/1000, origin = datex, tz = "UTC")
    attr(timex, "tzone") = tz
    rangex = ncvar_get(ncf, vname("range")) + dheight # height range (range) + elevation
    
    # Configuration variables
    elevang = ncvar_get(ncf, vname("elevation")) %>% signif(digits = 2) # Elevation angle (time)
    azimang = ncvar_get(ncf, vname("azimuth")) %>% signif(digits = 2) %>% round(digits = 0) # azimuth angle (time)
    
    # Wind variables
    wspd = as.data.frame(t(ncvar_get(ncf, vname("velocity")))) # transpose so dimensions are time x height
    names(wspd) = paste0("X", rangex)
    wdir = as.data.frame(t(ncvar_get(ncf, vname("direction"))))
    names(wdir) = paste0("X", rangex)
    uu = as.data.frame(t(ncvar_get(ncf, vname("u"))))
    names(uu) = paste0("X", rangex)
    vv = as.data.frame(t(ncvar_get(ncf, vname("v"))))
    names(vv) = paste0("X", rangex)
    ww = as.data.frame(t(ncvar_get(ncf, vname("w"))))
    names(ww) = paste0("X", rangex)
    
    # Quality flag variabe; dimensions are time x height
    qualf = as.data.frame(t(ncvar_get(ncf, vname("reconstruction_status"))))
    names(qualf) = paste0("X", rangex)
    
    # Add configuration data to wind data
    wspd$elevang = elevang
    wspd$azimang = azimang
    wspd$Date.Time = timex
    
    wdir$elevang = elevang
    wdir$azimang = azimang
    wdir$Date.Time= timex
    
    uu$elevang = elevang
    uu$azimang = azimang
    uu$Date.Time = timex
    
    vv$elevang = elevang
    vv$azimang = azimang
    vv$Date.Time = timex
    
    ww$elevang = elevang
    ww$azimang = azimang
    ww$Date.Time = timex
    
    qualf$elevang = elevang
    qualf$elevang = azimang
    qualf$Date.Time = timex
    
    # Add the TimeOfDay columns
    wspd = wspd %>% dateTimeCol()
    wdir = wdir %>% dateTimeCol()
    uu = uu %>% dateTimeCol()
    vv = vv %>% dateTimeCol()
    ww = ww %>% dateTimeCol()
    qualf = qualf %>% dateTimeCol()
    
    # Consolidate the configurations for the station
    wspd$angpairs = paste0(elevang, "-", azimang)
    wspd$angpairs[wspd$angpairs %in% c("75-0")] = "75-360"
    wspd$angpairs[wspd$angpairs %in% c("90-180", "90-360")] = "90-0"
    
    wdir$angpairs = paste0(elevang, "-", azimang)
    wdir$angpairs[wdir$angpairs %in% c("75-0")] = "75-360"
    wdir$angpairs[wdir$angpairs %in% c("90-180", "90-360")] = "90-0"
    
    uu$angpairs = paste0(elevang, "-", azimang)
    uu$angpairs[uu$angpairs %in% c("75-0")] = "75-360"
    uu$angpairs[uu$angpairs %in% c("90-180", "90-360")] = "90-0"
    
    vv$angpairs = paste0(elevang, "-", azimang)
    vv$angpairs[vv$angpairs %in% c("75-0")] = "75-360"
    vv$angpairs[vv$angpairs %in% c("90-180", "90-360")] = "90-0"
    
    ww$angpairs = paste0(elevang, "-", azimang)
    ww$angpairs[ww$angpairs %in% c("75-0")] = "75-360"
    ww$angpairs[ww$angpairs %in% c("90-180", "90-360")] = "90-0"
    
    qualf$angpairs = paste0(elevang, "-", azimang)
    qualf$angpairs[qualf$angpairs %in% c("75-0")] = "75-360"
    qualf$angpairs[qualf$angpairs %in% c("90-180", "90-360")] = "90-0"
    
    # Rearrange Columns
    # THIS IS WHERE WE LIMIT THE HEIGHT LEVEL
    wspd = wspd %>% select(Date.Time:angpairs, X100:X3000)
    wdir = wdir %>% select(Date.Time:angpairs, X100:X3000)
    uu = uu %>% select(Date.Time:angpairs, X100:X3000)
    vv = vv %>% select(Date.Time:angpairs, X100:X3000)
    ww = ww %>% select(Date.Time:angpairs, X100:X3000)
    qualf = qualf %>% select(Date.Time:angpairs, X100:X3000)
    
    winds = list(wspd = wspd, wdir = wdir, uu = uu, vv = vv, ww = ww, qualf = qualf)
    nc_close(ncf)
    return(winds)
}

read.nymesowindlidar.qc <- function(ncfile, loc, tz) {
    # This is a wrapper for `read.nymesowindlidar` where the data is quality controlled after it is read.
    # It uses the same inputs as `read.nymesowindlidar` and does additional steps for quality control.
    
    # Read the data
    winds = read.nymesowindlidar(ncfile, loc, tz)
    
    # Load the data individually
    wspd = winds$wspd
    wdir = winds$wdir
    uu = winds$uu
    vv = winds$vv
    ww = winds$ww
    qualf = winds$qualf
    
    # Quality Control matrix
    col.for.qc = grep("X", names(qualf)) # index of the data columns
    qc.matrix = qualf[, col.for.qc]
    qc.matrix[qc.matrix == 0] = NaN
    
    # Create quality controlled data columns
    wspd[, col.for.qc] = wspd[, col.for.qc] * qc.matrix
    wdir[, col.for.qc] = wdir[, col.for.qc] * qc.matrix
    uu[, col.for.qc] = uu[, col.for.qc] * qc.matrix
    vv[, col.for.qc] = vv[, col.for.qc] * qc.matrix
    ww[, col.for.qc] = ww[, col.for.qc] * qc.matrix
    qualf[, col.for.qc] = qualf[, col.for.qc] * qc.matrix
    
    # Save the variables back into a list; similar to the read.nymesowindlidar function
    winds = list(wspd = wspd, wdir = wdir, uu = uu, vv = vv, ww = ww, qualf = qualf)
}

read.nymesowindlidar.qc.batch <- function(file_batch, wlloc, tz, 
                                          takeagg = FALSE, aggtime = "1 hour", aggfun = "mean",
										  angpairs = "90-0") {
    # This function was created to read multiple wind lidar files.
    # This is a wrapper for the `read.nymesowindlidar.qc` function.
    # takeagg - T/F - take time-aggregate of the data 
    # aggtime - if takeagg = T, this is the value for how much of an aggregate to take; default is "1 hour"
    # aggfun  - if takeagg = T, this assigns the function to use for the aggreagation; default is "mean"
    # Create temporary file to hold all the components of an output from
    # read.nymesowindlidar.qc .
    temp_uu = list()
    temp_vv = list()
    temp_ww = list()
    temp_wspd = list()
    temp_wdir = list()
    temp_qualf = list()
    count = 1
    
    # Loop through each file in the batch
    for(wlf in file_batch) {
        temp = read.nymesowindlidar.qc(wlf, wlloc, tz)
        temp_uu[[count]] = temp$uu
        temp_vv[[count]] = temp$vv
        temp_ww[[count]] = temp$ww
        temp_wspd[[count]] = temp$wspd
        temp_wdir[[count]] = temp$wdir
        temp_qualf[[count]] = temp$qualf
        count = count + 1
    }
    
    # Combine into a list. Each component of the list is now a 
    # data frame containing all the dates in the `file_batch`.
    if (takeagg == FALSE) {
        nymesowl = list(uu = do.call("rbind", temp_uu),
                        vv = do.call("rbind", temp_vv),
                        ww = do.call("rbind", temp_ww),
                        wspd = do.call("rbind", temp_wspd),
                        wdir = do.call("rbind", temp_wdir),
                        qualf = do.call("rbind", temp_qualf))
    } else if (takeagg == TRUE) {
        nymesowl = list(uu = do.call("rbind", temp_uu) %>% filter(angpairs == angpairs) %>% select(-c(elevang, azimang, angpairs)) %>% timeavg.nymesowindlidar(aggtime, aggfun),
                        vv = do.call("rbind", temp_vv) %>% filter(angpairs == angpairs) %>% select(-c(elevang, azimang, angpairs)) %>% timeavg.nymesowindlidar(aggtime, aggfun),
                        ww = do.call("rbind", temp_ww) %>% filter(angpairs == angpairs) %>% select(-c(elevang, azimang, angpairs)) %>% timeavg.nymesowindlidar(aggtime, aggfun))
        nymesowl$wspd = wrfdf_wswd(nymesowl$uu, nymesowl$vv, "wspd")
        nymesowl$wdir = wrfdf_wswd(nymesowl$uu, nymesowl$vv, "wdir")
        
        # no qualf for this
    } else {print("Invalid thing you did!!!")}
    
    return(nymesowl)
}

plotme <- function() {
    # For quickly plotting using the filled.contour3 function.
    # Here is a sample code of what needs to be defined in order to use it:
    ## xva = wspd15 %>% select(Date.Time:sec)
    ## yva = char2height2(names(wspd15 %>% select(X100:X2500))) / 1000
    ## zlimits = c(0, 20)
    ## zva = squish(as.matrix(wspd15 %>% select(X100:X2500)), range = zlimits)
    ## colpal = blue.to.red
    ## clevels = seq(zlimits[1], zlimits[2], 2)
    plot.new()
    par(new = "TRUE", plt = c(0.15, 0.85, 0.18, 0.95), las = 1, cex.axis = 1.5)
    # windsnow = uwindsnow # TODO: FIX THIS
    fc_xlim = c(as.POSIXct(paste0(unique(xva$year), 
                                  sprintf("%02d", unique(xva$mon)), #pad single digit with 0's
                                  sprintf("%02d", unique(xva$day)), 
                                  "-", "00:00"), 
                           format = "%Y%m%d-%H:%M"),
                as.POSIXct(paste0(unique(xva$year), 
                                  sprintf("%02d", unique(xva$mon)), 
                                  sprintf("%02d", unique(xva$day)),
                                  "-", "23:59"), 
                           format = "%Y%m%d-%H:%M"))
    fc_xlim = c(head(fc_xlim, 1), tail(fc_xlim, 1)) # If time contains minutes from previous day, take the two extreme dates as the xlims
    filled.contour3(x = xva$Date.Time, y = yva, z = zva, 
                    color.palette = colpal, 
                    xlim = fc_xlim,
                    ylim = c(0, 2),
                    zlim = zlimits,
                    levels = clevels,
                    plot.axes = {
                        # points(x = xva$Date.Time, y = cloudz, col = 'white', pch = 20);
                        # points(x = xtime, y = clvars$cloudz[, 2], col = 'cyan', pch = 20);
                        # points(x = xtime, y = clvars$cloudz[, 3], col = 'purple', pch = 20);
                        axis.POSIXct(side = 1, x = xva$Date.Time); axis(2)
                    },
                    xlab = paste("Time (LST):", paste0(unique(xva$year), 
                                                       sprintf("%02d", unique(xva$mon)), 
                                                       sprintf("%02d", unique(xva$day)))),
                    ylab = NULL
    )
    # par(xpd = NA)
    #     contour(x = xva, y = yva, z = zva, 
    #                     col = "black",
    #                     xlim = fc_xlim,
    #                     ylim = c(0, 2),
    #                     zlim = zlimits,
    #             levels = clevels,
    #             drawlabels = F,
    #             add = T
    #                     )
    par(new = "TRUE", plt = c(0.86, 0.90, 0.18, 0.95), las = 1, cex.axis = 1.5)
    filled.legend(x = as.vector(xva$Date.Time), y = as.vector(yva), z = as.matrix(zva), 
                  color.palette = colpal, 
                  xlim = fc_xlim,
                  ylim = c(0, 2),
                  zlim = zlimits,
                  levels = clevels)
}

timeavg.nymesowindlidar <- function(wlvar, agg.time, statistic) {
    # This function perfomrs the time 'statistics' on the data as well as setting
    # up the rest of the columns.
	
	# 1. For the timeAverage to work we need to change the Date.Time column name
	names(wlvar)[names(wlvar) == "Date.Time"] = "date"
	
	# 2. Save the heights in the variables
	timenames = c("date", "year", "mon", "day", "hour", "min", "sec")
	htsnames = names(wlvar)[names(wlvar) %w/o% timenames]
	
	# 4. Calculate the time average
    wlvar.aggregate = timeAverage(wlvar %>% select(date, htsnames), avg.time = agg.time, statistic =  statistic)
    names(wlvar.aggregate)[names(wlvar.aggregate) == "date"] = "Date.Time"
    wlvar.aggregate = wlvar.aggregate %>% dateTimeCol() %>% select(Date.Time, year:sec, htsnames)
    
    return(wlvar.aggregate)
}

timeavg.nymesowindlidar2 <- function(wlvar, agg.time, statistic) {
    # This function performs the time 'statistics' on the data as well as setting
    # up the rest of the columns. The difference from the other function is the addition
    # of the 'angpairs' column.
    
    # 1. For the timeAverage to work we need to change the Date.Time column name
    names(wlvar)[names(wlvar) == "Date.Time"] = "date"
    
    # 2. Save the heights in the variables
    timenames = c("date", "year", "mon", "day", "hour", "min", "sec", "angpairs")
    htsnames = names(wlvar)[names(wlvar) %w/o% timenames]
    
    # 4. Calculate the time average
    wlvar.aggregate = timeAverage(wlvar %>% select(date, htsnames), avg.time = agg.time, statistic =  statistic)
    names(wlvar.aggregate)[names(wlvar.aggregate) == "date"] = "Date.Time"
    wlvar.aggregate = wlvar.aggregate %>% dateTimeCol() %>% select(Date.Time, year:angpairs, htsnames)
    
    return(wlvar.aggregate)
}

xtimelimits <- function(xva, day, tz) {
    # This function computes the two element vector used to define the limits
    # on the date-time for data that has data over a 24-hour period.
    # xva - This a data frame with columns of Date.Time (a POSIXct class object),
    #       year, mon, day, hour, min, sec columns. The columns can be generated just
    #       Date.Time by using the function 'dateTimeCol'.
    # fc_xlim - This is the x axis limits.
    
    startdate = paste0(unique(xva$year), 
                       sprintf("%02d", unique(xva$mon)), #pad single digit with 0's
                       # sprintf("%02d", day),
                       sprintf("%02d", unique(xva$day)), # Need this for plotter_quiver_obs.R
                       "-", "00:00")
    enddate = paste0(unique(xva$year), 
                     sprintf("%02d", unique(xva$mon)),
                     # sprintf("%02d", day),
                     sprintf("%02d", unique(xva$day)), # Need this for plotter_quiver_obs.R
                     "-", "23:59")
    xtlim = c(as.POSIXct(startdate, format = "%Y%m%d-%H:%M", tz = tz),
              as.POSIXct(enddate,   format = "%Y%m%d-%H:%M", tz = tz))
    xtlim = c(head(xtlim, 1), tail(xtlim, 1)) # If time contains minutes from previous day, take the two extreme dates as the xlims
    attr(xtlim, "tzone") <- tz
    return(xtlim)
}
