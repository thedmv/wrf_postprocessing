#### Libraries ####
suppressPackageStartupMessages(library(openair)) # for timeAverage function
suppressPackageStartupMessages(library(dplyr))

#### Functions ####
"%w/o%" <- function(x, y) !x %in% y


theta.mwr = function(mwrtv) {
    # This function calculates the potential temperature from a data frame
    # of temperature from the microwave radiometer profile.
    
    # Potential Temperature
    datetimecols = c("Date.Time", "year", "mon", "day", "hour", "min", "sec")
    cols = !(names(mwrtv) %in% datetimecols)
    hts = heightsdf.mwr(mwrtv)
    mwrth = mwrtv
    mwrth[, cols] = mwrtv[, cols] * 0
    mwrth[, cols] = Th(temp = mwrtv[, cols], z = hts[, cols])
    return(mwrth)
}

rmix.mwr = function(mwrtv, mwrrh) {
    # This function calculates the mixing ratio from a data frame of 
    # relative humidity from the microwave radiometer.
    # Units: g/kg
    
    datetimecols = c("Date.Time", "year", "mon", "day", "hour", "min", "sec")
    cols = !(names(mwrrh) %in% datetimecols)
    hts = heightsdf.mwr(mwrrh)
    pres = hts
    pres[, cols] = pressure(hts[, cols])
    mwrrmix = mwrrh
    mwrrmix[, cols] = mwrrmix[, cols] * 0
    mwrrmix[, cols] = mixratio(mwrrh[, cols], mixratio_sat(pres[, cols], pres_sat(mwrtv[, cols])))
    mwrrmix[, cols] = mwrrmix[, cols] * 1000 # unit change to g/kg
    return(mwrrmix)
}

heightsdf.mwr = function(mwrdf) {
    # This function creates a data frame of the heights from the MWR.
    # This funtion is used as an intermediate step in calculating theta and rmix.
    
    # Vector of columns we won't use
    datetimecols = c("Date.Time", "year", "mon", "day", "hour", "min", "sec")
    cols = !(names(mwrdf) %in% datetimecols)
    hts = mwrdf
    hts[, cols] = hts[, cols] * 0
    hts[, cols] = rep(char2height2(names(hts)[cols]), each = nrow(hts[, cols]))
    return(hts)
}

timeavg.mwr = function(mwrdf, agg.time = "1 hour", statistic = "mean") {
    # This function performs a time average on the MWR profile data.
    # The mwrdf needs to be a data frame of either the temperature, water vapor,
    # liquid water OR relative humidity profiles.
    
    # 1. For timeAverage to work we need to change the Date.Time column name
    names(mwrdf)[names(mwrdf) == "Date.Time"] = "date"
    
    # 2. Save the heights in the variables
    timenames = c("date", "year", "mon", "day", "hour", "min", "sec")
    htsnames = names(mwrdf)[names(mwrdf) %w/o% timenames]
    
    # 3. Calculate the time average
    mwrdf = timeAverage(mwrdf %>% select(date, htsnames), avg.time = agg.time, statistic =  statistic)
    names(mwrdf)[names(mwrdf) == "date"] = "Date.Time"
    mwrdf = mwrdf %>% dateTimeCol() %>% select(Date.Time, year:sec, htsnames)
    
    return(mwrdf)
}

getIndex = function(Station.lat, Station.lon, Ulat, Ulon){
    # This function calculates the index of the closest grid point
    # to the Latitude and Longitude coordinated supplied.
    # Station.lat, Station.lon - point of interest
    # Ulat, Ulan - user latitude and longitude coordinates from which to find the closest point
    
    Lat1 = matrix(which(abs(Ulat - Station.lat) <= 0.01, arr.ind = T), ncol = 2)
    Lon1 = matrix(which(abs(Ulon - Station.lon) <= 0.01, arr.ind = T), ncol = 2)
    
    a = which(Lat1[, 1] <= max(Lon1[, 1]) & Lat1[, 1] >= min(Lon1[, 1]))
    Lat1[a, ]
    
    # Only the indices of the closest point remain
    # Let's calculte the distance
    distance = rep(0, dim(Lat1[a, ])[1])
    for(i in 1:dim(Lat1[a,])[1]){
        distance[i]=sqrt((Ulon[Lat1[a,][i,1],Lat1[a,][i,2]]-Station.lon)^2+((Ulat[Lat1[a,][i,1],Lat1[a,][i,2]]-Station.lat)^2))
    }
    
    #MinimumDistance
    Index=which(distance==min(distance))
    
    Ind1=Lat1[a,][Index,1]
    Ind2=Lat1[a,][Index,2]
    
    return(c(Ind1,Ind2))
    
}

eta_to_hts <- function(eta, p_top = 1000, p_surf = 101300) {
    # Eta levels to heights in m
    
    # 1. Convert eta levels to pressure levels
    preslvl = p_top + eta*(p_surf - p_top)
    
    # 2. Convert Pressure levels to height
    H = 8500 # scale height
    heightlvl = - H * log(preslvl/p_surf)
    
    return(heightlvl)
}

read.mwrlv2 <- function(lv2file, tz = "EST") {
    # Given a MP-3000A Level2 file name this function performs the
    # lv2surface function on it. It then performs changes similar to
    # read.mwrprofile. 'tz' is the time zone desired; it first assumes
    # the time zone is 'UTC'
    # Functions needed: lv2surface, dateTimeCol, lv2vertprf, lv2scalars
    
    # Read the surface variable
    sf = lv2surface(lv2file)
    names(sf)[1] = "Date.Time"
    sf$Date.Time = as.POSIXct(sf$Date.Time, tz = "UTC", format = "%m/%d/%y %H:%M:%S")
    attributes(sf$Date.Time)$tzone = tz
    sf = dateTimeCol(sf)
    
    # Read the vertical profiles
    tv = lv2vertprf(lv2file, "TV")
    wv = lv2vertprf(lv2file, "WV")
    lw = lv2vertprf(lv2file, "LW")
    rh = lv2vertprf(lv2file, "RH")
    
    names(tv)[1] = "Date.Time"
    tv$Date.Time = as.POSIXct(tv$Date.Time, tz = "UTC", format = "%m/%d/%y %H:%M:%S")
    attributes(tv$Date.Time)$tzone = tz
    tv = dateTimeCol(tv)
    
    names(wv)[1] = "Date.Time"
    wv$Date.Time = as.POSIXct(wv$Date.Time, tz = "UTC", format = "%m/%d/%y %H:%M:%S")
    attributes(wv$Date.Time)$tzone = tz
    wv = dateTimeCol(wv)
    
    names(lw)[1] = "Date.Time"
    lw$Date.Time = as.POSIXct(lw$Date.Time, tz = "UTC", format = "%m/%d/%y %H:%M:%S")
    attributes(lw$Date.Time)$tzone = tz
    lw = dateTimeCol(lw)
    
    names(rh)[1] = "Date.Time"
    rh$Date.Time = as.POSIXct(rh$Date.Time, tz = "UTC", format = "%m/%d/%y %H:%M:%S")
    attributes(rh$Date.Time)$tzone = tz
    rh = dateTimeCol(rh)
    
    # read the scalar variables
    sc = lv2scalars(lv2file)
    names(sc)[1] = "Date.Time"
    sc$Date.Time = as.POSIXct(sc$Date.Time, tz = "UTC", format = "%m/%d/%y %H:%M:%S")
    attributes(sc$Date.Time)$tzone = tz
    sc = dateTimeCol(sc)
    
    # Return a list with all the variables
    mwr2lv2 = list(sf = sf, tv = tv, wv = wv, lw = lw, rh = rh, sc = sc)
    return(mwr2lv2)
}


read.mwrlv2_list <- function(lv2list, tz = "EST", 
                             mwrheights = c(seq(0, 500, 50), seq(600, 2000, 100), seq(2250, 10000, 250))) {
    # This function reads a list of lv2 MWR data. This is a wrapper for the read.mwrlv2 function.
    # lv2list - character vector where each element is a unique path of a lv2 file.
    # mwrheights - these are the levels of the microwave radiometer with the elevation offset.
    #              A list of examples can be found in the ./wrf_functions.R script.
    #              If given, these values overwrite the heights from the lv2 data.
    # OUTPUT: R list where each element is a data frame. Each data frame corresponds to the surface variables,
    #         the 4 profile outputs, and the integrated scalar values.
    
    sf = list()
    tv = list()
    wv = list()
    lw = list()
    rh = list()
    sc = list()
    
    cnt = 1
    
    # Loop through every file and get the data
    for (ff in lv2list) {
        mwr = read.mwrlv2(ff, tz = tz)
        sf[[cnt]] = mwr$sf
        tv[[cnt]] = mwr$tv
        wv[[cnt]] = mwr$wv
        lw[[cnt]] = mwr$lw
        rh[[cnt]] = mwr$rh
        sc[[cnt]] = mwr$sc
        cnt = cnt + 1
    }
    
    # Combine the data in each R list; add dateTime Columns; rearrange columns
    sf = do.call(rbind, sf) %>% dateTimeCol() %>% select(Date.Time, year:sec, `Tamb(K)`:Rain)
    tv = do.call(rbind, tv) %>% dateTimeCol() %>% select(Date.Time, year:sec, X0:X10000)
    wv = do.call(rbind, wv) %>% dateTimeCol() %>% select(Date.Time, year:sec, X0:X10000)
    lw = do.call(rbind, lw) %>% dateTimeCol() %>% select(Date.Time, year:sec, X0:X10000)
    rh = do.call(rbind, rh) %>% dateTimeCol() %>% select(Date.Time, year:sec, X0:X10000)
    sc = do.call(rbind, sc) %>% dateTimeCol() %>% select(Date.Time, year:sec, `Int. Vapor(cm)`:`Cloud Base(km)`)
    
    # Change the heights for each variable
    timenames = c("Date.Time", "year", "mon", "day", "hour", "min", "sec")
    names(tv) =  c(timenames, paste0("X", mwrheights))
    names(wv) =  c(timenames, paste0("X", mwrheights))
    names(lw) =  c(timenames, paste0("X", mwrheights))
    names(rh) =  c(timenames, paste0("X", mwrheights))
    
    # Return the data in a list of the 6 data frames
    mwr = list(sf = sf, tv = tv, wv = wv, lw = lw, rh = rh, sc = sc)
    return(mwr)
}

char2height2 <- function(mychar){
    # Convert heights from characters to numbers vector, i.e. from character "X100" to numeric 100.
    #   mychar... string
    mychar = substring(mychar, 2) # Remove first character
    mychar_num = as.numeric(mychar) # Heights vector is now a numeric vector 
    return(mychar_num)
}

# Read the integrated scalar data
lv2scalars <- function(lv2file) {
    # Given a MP-3000A Level2 file name this function performs the
    # necessary operations to make it into a usable data frame where only
    # relevant columns are used, and 
    # certain column values had to be coerced from a 'char' to a 'double'
    # This function separates out the scalar data.
    
    lv2df = read.csv(file = lv2file, header = FALSE, stringsAsFactors = FALSE) # level2 file
    scalcol = c(2, 4:6) # relevant scalar data columns
    lv2dfscal = lv2df[lv2df[, 3] == 301, scalcol] # Scalar Data = 301
    names(lv2dfscal) <- lv2df[lv2df[, 3] == 300, scalcol] # Scalar Data header = 200
    lv2dfscal[,2:4] <- apply(lv2dfscal[,2:4], 2, as.numeric)
    return(lv2dfscal)
}

# Vertical profiles
lv2vertprf <- function(lv2file, varname) {
    # Given a MP-3000A Level2 file name this function performs the
    # necessary operations to make it into a usable data frame where only
    # relevant columns are used, colnames changed from km to meters, and 
    # certain column values had to be coerced from a 'char' to a 'double'
    # This function seperates out the vertical profile for a given 
    # variable (var).
    
    lv2df = read.csv(file = lv2file, header = FALSE, stringsAsFactors = FALSE) # level2 file
    vertcol = c(2, 5:62) # relevant columns
    header = lv2df[lv2df[, 3] == 400, vertcol] # Vector retrieval header = 400
    
    if (varname == "TV") { # Temperature Variable (K)
        lv2dfvert = lv2df[lv2df[, 3] == 401, vertcol]
        names(lv2dfvert) <- header
        names(lv2dfvert)[2:59] <- paste("X", as.numeric(names(lv2dfvert[, 2:59]))*1000, sep = "")
        lv2dfvert[, 2:59] <- apply(lv2dfvert[, 2:59], 2, as.numeric)
    }
    
    if (varname == "WV") { # Water Vapor Density (g/m3)
        lv2dfvert = lv2df[lv2df[, 3] == 402, vertcol]
        names(lv2dfvert) <- header
        names(lv2dfvert)[2:59] <- paste("X", as.numeric(names(lv2dfvert[, 2:59]))*1000, sep = "")
        lv2dfvert[, 2:59] <- apply(lv2dfvert[, 2:59], 2, as.numeric)
    }
    
    if (varname == "LW") { # Liquid Water Density (g/m3)
        lv2dfvert = lv2df[lv2df[, 3] == 403, vertcol]
        names(lv2dfvert) <- header
        names(lv2dfvert)[2:59] <- paste("X", as.numeric(names(lv2dfvert[, 2:59]))*1000, sep = "")
        lv2dfvert[, 2:59] <- apply(lv2dfvert[, 2:59], 2, as.numeric)
    }
    
    if (varname == "RH") { # Relative Humidity (%)
        lv2dfvert = lv2df[lv2df[, 3] == 404, vertcol]
        names(lv2dfvert) <- header
        names(lv2dfvert)[2:59] <- paste("X", as.numeric(names(lv2dfvert[, 2:59]))*1000, sep = "")
        lv2dfvert[, 2:59] <- apply(lv2dfvert[, 2:59], 2, as.numeric)
    }
    
    return(lv2dfvert)
}

lv2surface <- function(lv2file) {
    # Given a MP-3000A Level2 file name this function performs the
    # necessary operations to make it into a usable data frame where only
    # relevant columns are used, and
    # certain column values had to be coerced from a 'char' to a 'double'
    # This function separates out the surface data.
    
    lv2df = read.csv(file = lv2file, header = FALSE, stringsAsFactors = FALSE) # level2 file
    surfcol = c(2, 4:8) # relevant surface data columns
    lv2dfsurf = lv2df[lv2df[, 3] == 201, surfcol] # Surface Data = 201
    names(lv2dfsurf) <- lv2df[lv2df[, 3] == 200, surfcol] # Surface Data header = 200
    lv2dfsurf[,2:6] <- apply(lv2dfsurf[,2:6], 2, as.numeric)
    return(lv2dfsurf)
}

# Color Palettes
rgb.palette = colorRampPalette(c("purple", "blue","green", "gray", "orange", "red"), space = "rgb")
brown.to.green = colorRampPalette(c("brown", "yellow", "green"), space = "rgb")
blue.to.red = colorRampPalette(c("blue", "white", "red"), space = "rgb")
blue.to.red.rev = colorRampPalette(c("red", "white", "blue"), space = "rgb")
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
jet.colors.rev <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))
div.pal = colorRampPalette(rev(c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'))) # usefule for vertical winds
div2.rev <- colorRampPalette(rev(c("#26456E", "#408CBB", "#C9C9C9", "#E85B0C", "#7B3014"))) # useful for mixing ratio

# Heights for the microwave radiometer
ccnymwrheights = c(seq(0, 500, 50), seq(600, 2000, 100), seq(2250, 10000, 250)) + 65 # Microwave Radiometer height

# Add columns for date and time
dateTimeCol <- function(vdata) {
    # This function creates columns of year, month, day, hour, minute, and second
    # for a variable that has a date-time column in POSIXct. This function uses
    # a conversion to POSIXlt in order to create the columns.
    # The date-time column must be named "Date.Time"
    
    # vdata is a data frame with a column named 'Date.Time' of POSIXct class.
    
    # This function returns the same data frame with additional columns of year,
    # month, day, hour, minute, and second.
    
    # Check if it is a POSIXct
    if (class(vdata$Date.Time)[1] == class(as.POSIXct(Sys.time()))[1]) {
        # Convert to POSIXlt from POSIXct.
        vdata$Date.Time <- as.POSIXlt(vdata$Date.Time)
    }
    # Number of years since 1900
    vdata$year <- vdata$Date.Time$year + 1900 
    # Months are 0-11
    vdata$mon <- vdata$Date.Time$mon + 1 
    vdata$day <- vdata$Date.Time$mday
    vdata$hour <- vdata$Date.Time$hour
    vdata$min <- vdata$Date.Time$min
    vdata$sec <- vdata$Date.Time$sec
    
    # Convert the date time object back to a POSIXct
    vdata$Date.Time <- as.POSIXct(vdata$Date.Time)
    return(vdata)
}

changeTZ <- function(vdata, tz) {
    # This function changes the time zone of the data. 
    # It couples with the `dateTimeCol` function to recreate
    # the columns for year, mon, day, hour, min and sec.
    # The data should already have Date.Time and the columns mentioned.
    # NOTE: Need to use data.frame because R then thinks that newtz is
    #       a vector, which means that I can't add columns.
    
    newtz = data.frame(Date.Time = vdata$Date.Time) 
    attr(newtz$Date.Time, "tzone") <- tz
    vdata_new = cbind(newtz %>% dateTimeCol(),
                      vdata %>% select(-c(Date.Time:sec)))
    return(vdata_new)
}

# Filled Contour Plot function
filled.contour3 <-
    function (x = seq(0, 1, length.out = nrow(z)),
              y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
              ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
              levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
              col = color.palette(length(levels) - 1), plot.title, plot.axes, 
              key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
              axes = TRUE, frame.plot = axes,mar, ...) 
{
        # modification by Ian Taylor of the filled.contour function
        # to remove the key and facilitate overplotting with contour()
        # further modified by Carey McGilliard and Bridget Ferris
        # to allow multiple plots on one page
        
        if (missing(z)) {
            if (!missing(x)) {
                if (is.list(x)) {
                    z <- x$z
                    y <- x$y
                    x <- x$x
                }
                else {
                    z <- x
                    x <- seq.int(0, 1, length.out = nrow(z))
                }
            }
            else stop("no 'z' matrix specified")
        }
        else if (is.list(x)) {
            y <- x$y
            x <- x$x
        }
        if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
            stop("increasing 'x' and 'y' values expected")
        # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
        # on.exit(par(par.orig))
        # w <- (3 + mar.orig[2]) * par("csi") * 2.54
        # par(las = las)
        # mar <- mar.orig
        plot.new()
        # par(mar=mar)
        plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
        if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
            stop("no proper 'z' matrix specified")
        if (!is.double(z)) 
            storage.mode(z) <- "double"
        .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                        col = col)
        if (missing(plot.axes)) {
            if (axes) {
                title(main = "", xlab = "", ylab = "")
                Axis(x, side = 1)
                Axis(y, side = 2)
            }
        }
        else plot.axes
        if (frame.plot) 
            box()
        if (missing(plot.title)) 
            title(...)
        else plot.title
        invisible()
    }

filled.legend <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, length.out = ncol(z)), z, 
                           xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
                           levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
                           col = color.palette(length(levels) - 1), plot.title, plot.axes, 
                           key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
                           axes = TRUE, frame.plot = axes, ...) 
{
    # modification of filled.contour by Carey McGilliard and Bridget Ferris
    # designed to just plot the legend
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    #  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    #  on.exit(par(par.orig))
    #  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    #layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    #  par(las = las)
    #  mar <- mar.orig
    #  mar[4L] <- mar[2L]
    #  mar[2L] <- 1
    #  par(mar = mar)
    # plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
        if (axes) 
            axis(4)
    }
    else key.axes
    box()
    
    #
    #    if (!missing(key.title)) 
    #        key.title
    #    mar <- mar.orig
    #    mar[4L] <- 1
    #    par(mar = mar)
    #    plot.new()
    #    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    #    if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L) 
    #        stop("no proper 'z' matrix specified")
    #    if (!is.double(z)) 
    #        storage.mode(z) <- "double"
    #    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
    #        col = col))
    #    if (missing(plot.axes)) {
    #        if (axes) {
    #            title(main = "", xlab = "", ylab = "")
    #            Axis(x, side = 1)
    #            Axis(y, side = 2)
    #        }
    #    }
    #    else plot.axes
    #    if (frame.plot) 
    #        box()
    #    if (missing(plot.title)) 
    #        title(...)
    #    else plot.title
    #    invisible()
    #}
}


args.filled.contour <- function(fcdata, datacolumns, colpal, pfunction = F, ylimits, 
                                zlimits = c(280, 300), maintitle) {
    args.fc = list()
    args.fc$x = fcdata$Date.Time
    args.fc$y = char2height2(names(fcdata[, datacolumns])) / 1000
    if (pfunction == T) {
        args.fc$z = pmin(pmax(as.matrix(fcdata[, datacolumns]), 
                              min(as.vector(as.matrix(fcdata[, datacolumns]))), zlimits[1]), #pmaxval
                         max(as.vector(as.matrix(fcdata[, datacolumns]))), zlimits[2]) #pminval
        args.fc$levels = seq(zlimits[1], zlimits[2], 2)
    } else {
        args.fc$z = as.matrix(fcdata[, datacolumns])
    }
    args.fc$xlim = c(min(fcdata$Date.Time), max(fcdata$Date.Time))
    args.fc$ylim = ylimits
    args.fc$color.palette = colpal
    args.fc$main = maintitle
    args.fc$axes = FALSE
    return(args.fc)
}

# Contour arguments (if needed)
args.contour <- function(fcdata, datacolumns, pfunction = F, ylimits, 
                         zlimits = c(280, 300), maintitle) {
    args.c = list()
    args.c$x = fcdata$Date.Time
    args.c$y = char2height2(names(fcdata[, datacolumns])) / 1000
    if (pfunction == T) {
        args.c$z = pmin(pmax(as.matrix(fcdata[, datacolumns]), 
                             min(as.vector(as.matrix(fcdata[, datacolumns]))), zlimits[1]), #pmaxval
                        max(as.vector(as.matrix(fcdata[, datacolumns]))), zlimits[2]) #pminval
        args.c$levels = seq(zlimits[1], zlimits[2], 2)
    } else {
        args.c$z = as.matrix(fcdata[, datacolumns])
    }
    args.fc$xlim = c(min(fcdata$Date.Time), max(fcdata$Date.Time))
    args.c$ylim = ylimits
    args.c$col = NA
    args.c$drawlabels = FALSE
    args.c$add = T
    return(args.c)
}

# Wrapper for filled.contour3 and filled.legend
plot.fc <- function(args.fc, args.c) {
    # Filled Contour plot
    plot.new()
    par(new = "TRUE", plt = c(0.15, 0.85, 0.15, 0.87), las = 1, cex.axis = 1.5)
    do.call("filled.contour3", args.fc) 
    axis(side = 1,
         at = seq(min(args.fc$x), max(args.fc$x), by = "hour"),
         labels = as.character(
             format(seq(min(args.fc$x), max(args.fc$x), by = "hour"), format = "%H")),
         cex.axis = 1,
         tck = -0.025)
    axis(side = 2)
    # do.call(contour, args.c)
    par(xpd = NA)
    text(x = median(args.fc$x), y = -0.27, labels = "Hours (EST)", cex = 1.5)
    text(x = min(args.fc$x) - 10000, y = 1, labels = "Height (km)", cex = 1.5, srt = 90)
    par(new = "TRUE", plt = c(0.86, 0.88, 0.15, 0.87), las = 1, cex.axis = 1.5)
    args.fc$axes = TRUE # for legend 
    do.call("filled.legend", args.fc)
    axis(side = 3, outer = T)
    # suppressWarnings(par(par.df))
}

# Quick plotting for a day
plotme.day <- function() {
    # For quickly plotting using the filled.contour3 function. Make sure the data is a single day
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
                           format = "%Y%m%d-%H:%M", tz = "UTC"),
                as.POSIXct(paste0(unique(xva$year), 
                                  sprintf("%02d", unique(xva$mon)), 
                                  sprintf("%02d", unique(xva$day)),
                                  "-", "23:59"), 
                           format = "%Y%m%d-%H:%M", tz = "UTC"))
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

plotme.event <- function() {
    # For quickly plotting using the filled.contour3 function.
    # For short events that lasts multiple days
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
    # fc_xlim = c(as.POSIXct(paste0(unique(xva$year),
    #                               sprintf("%02d", unique(xva$mon)), #pad single digit with 0's
    #                               sprintf("%02d", unique(xva$day)),
    #                               "-", "00:00"),
    #                        format = "%Y%m%d-%H:%M"),
    #             as.POSIXct(paste0(unique(xva$year),
    #                               sprintf("%02d", unique(xva$mon)),
    #                               sprintf("%02d", unique(xva$day)),
    #                               "-", "23:59"),
    #                        format = "%Y%m%d-%H:%M"))
    fc_xlim = c(min(xva$Date.Time), max(xva$Date.Time)) # If time contains minutes from previous day, take the two extreme dates as the xlims
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
                    xlab = paste("Time (UTC):", min(xva$Date.Time), "to", max(xva$Date.Time)),
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

plotme.day.shiny <- function(xva, yva, zva, clevels, colpal) {
    # For quickly plotting using the filled.contour3 function. Make sure the data is a single day
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
                           format = "%Y%m%d-%H:%M", tz = "UTC"),
                as.POSIXct(paste0(unique(xva$year), 
                                  sprintf("%02d", unique(xva$mon)), 
                                  sprintf("%02d", unique(xva$day)),
                                  "-", "23:59"), 
                           format = "%Y%m%d-%H:%M", tz = "UTC"))
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

# Plot multiple png files to a pdf
# https://jonkimanalyze.wordpress.com/2014/07/24/r-compile-png-files-into-pdf/
merge.png.pdf <- function(pdfFile, pngFiles, deletePngFiles=FALSE) {
    
    #### Package Install ####
    pngPackageExists <- suppressPackageStartupMessages(require ("png"))
    
    if ( !pngPackageExists ) {
        install.packages ("png")
        suppressPackageStartupMessages(library ("png"))
        
    }
    suppressPackageStartupMessages(library(grid))
    #########################
    
    pdf(pdfFile)
    
    n <- length(pngFiles)
    
    for( i in 1:n) {
        
        pngFile <- pngFiles[i]
        
        pngRaster <- readPNG(pngFile)
        
        grid.raster(pngRaster, width = unit(0.8, "npc"), height = unit(0.8, "npc"))
        
        if (i < n) plot.new()
        
    }
    
    dev.off()
    
    if (deletePngFiles) {
        
        unlink(pngFiles)
    }
    
}

# Derived Variables
# Potential Temperature
Th <- function(temp, z) {
    # Potential Temperature: 
    # temp - Temperature, K
    # z - Height, meters
    h = 8500 # Scale Height    
    theta = temp*exp(0.286*z/h)
    return(theta)
}

# Water Vapor saturation pressure
pres_sat <- function(temp){
    # Saturation Pressure in Pa
    #   temp... local temperature, K
    p_sat = 617.08*exp((17.2694 * (temp - 273.16))/(temp - 35.86))
    return(p_sat)
}

# Mixing ration at saturation
mixratio_sat <- function(p, p_sat){
    # Saturation Mixing Ratio
    #   p...local pressure, Pa
    #   p_sat... saturation pressure, Pa
    r_sat = 0.622 * p_sat/(p - p_sat)
    return(r_sat)
}

# Mixing Ratio
mixratio <- function(rh, r_sat){
    # Local Mixing Ratio
    #   rh... relative humidity in percentage (%)
    #   r_sat... Saturation Mixing Ratio
    r = rh * 1/100 * r_sat
    return(r)
}

# Pressure; using exponential profile
pressure <- function(z){ 
    # Pressure of Air in Atmosphere, 
    #   z...height, in m
    P_o = 101325 # Pa, surface pressure
    H = 8500 # m, scale height of the Earth
    P = P_o*exp(-z/H)
    return(P)
}