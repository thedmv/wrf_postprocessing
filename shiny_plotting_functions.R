# Plotting functions for Shiny apps

plotme.day.shiny.default <- function(xva, yva, zva, clevels, colpal) {
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
    par(new = "TRUE", plt = c(0.86, 0.90, 0.18, 0.95), las = 1, cex.axis = 1.5)
    filled.legend(x = as.vector(xva$Date.Time), y = as.vector(yva), z = as.matrix(zva), 
                  color.palette = colpal, 
                  xlim = fc_xlim,
                  ylim = c(0, 2),
                  zlim = zlimits,
                  levels = clevels)
}

plotme.day.shiny.left <- function(xva, yva, zva, clevels, colpal) {
    # For quickly plotting using the filled.contour3 function. Make sure the data is a single day
    # Here is a sample code of what needs to be defined in order to use it:
    ## xva = wspd15 %>% select(Date.Time:sec)
    ## yva = char2height2(names(wspd15 %>% select(X100:X2500))) / 1000
    ## zlimits = c(0, 20)
    ## zva = squish(as.matrix(wspd15 %>% select(X100:X2500)), range = zlimits)
    ## colpal = blue.to.red
    ## clevels = seq(zlimits[1], zlimits[2], 2)
    plot.new()
    par(new = "TRUE", las = 1, plt = c(0.15, 0.98, 0.18, 0.95), cex.axis = 1.3)
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
                        axis.POSIXct(side = 1, x = xva$Date.Time); 
                        axis(side = 2)
                    },
                    plot.title = {
                        title(xlab = paste("Date:", paste0(unique(xva$year), sprintf("%02d", unique(xva$mon)), sprintf("%02d", unique(xva$day)))),
                              cex.lab = 1.3);
                        title(ylab = "Height (km)", cex.lab = 1.3)
                    }
    )
    # par(new = "TRUE", plt = c(0.86, 0.90, 0.18, 0.95), las = 1, cex.axis = 1.5)
    # filled.legend(x = as.vector(xva$Date.Time), y = as.vector(yva), z = as.matrix(zva), 
    #               color.palette = colpal, 
    #               xlim = fc_xlim,
    #               ylim = c(0, 2),
    #               zlim = zlimits,
    #               levels = clevels)
}

plotme.day.shiny.right <- function(xva, yva, zva, clevels, colpal, vartitle) {
    # For quickly plotting using the filled.contour3 function. Make sure the data is a single day
    # Here is a sample code of what needs to be defined in order to use it:
    ## xva = wspd15 %>% select(Date.Time:sec)
    ## yva = char2height2(names(wspd15 %>% select(X100:X2500))) / 1000
    ## zlimits = c(0, 20)
    ## zva = squish(as.matrix(wspd15 %>% select(X100:X2500)), range = zlimits)
    ## colpal = blue.to.red
    ## clevels = seq(zlimits[1], zlimits[2], 2)
    plot.new()
    par(new = "TRUE", las = 1, plt = c(0.02, 0.85, 0.18, 0.95), cex.axis = 1.3)
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
                    main = "test",
                    plot.axes = {
                        # points(x = xva$Date.Time, y = cloudz, col = 'white', pch = 20);
                        # points(x = xtime, y = clvars$cloudz[, 2], col = 'cyan', pch = 20);
                        # points(x = xtime, y = clvars$cloudz[, 3], col = 'purple', pch = 20);
                        axis.POSIXct(side = 1, x = xva$Date.Time); 
                        axis(2, tick = TRUE, labels = FALSE)
                    },
                    plot.title = {
                        title(xlab = vartitle, cex.lab = 1.3);
                        title(ylab = NULL)
                    }
    )
    par(new = "TRUE", plt = c(0.86, 0.90, 0.18, 0.95), las = 1, cex.axis = 1.3)
    filled.legend(x = as.vector(xva$Date.Time), y = as.vector(yva), z = as.matrix(zva),
                  color.palette = colpal,
                  xlim = fc_xlim,
                  ylim = c(0, 2),
                  zlim = zlimits,
                  levels = clevels)
}

xtimelimits.shiny <- function(xva, day, tz) {
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