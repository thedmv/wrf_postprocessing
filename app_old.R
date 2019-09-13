###############################
# Shiny Web App: CCNY_Profilers
#
###############################
# Load the libraries
rm(list = ls())
options(warn = -1)
# setwd("C:/Users/melev/Desktop/FORCE/SummerCampaign2017/Shiny_OBSvWRF_Profilers/")
source("call_libraries.r")
source("ccnymwr_functions.R")
# source("C:/Users/melev/Desktop/FORCE/WindLidar_NYC/windlidar_nymeso_functions.R")
source("wrf_functions.R")
source("shiny_plotting_functions.R")

# Load the observation data.
tz = "UTC"
load("mwr_ccny15min.data")
load("wl_ccny15min.data")
obsdf1 = ccnymwrprof.timeavg$th
attributes(obsdf1$Date.Time)$tzone = tz
obsdf1$Dates = as.Date(obsdf1$Date.Time)
obsdf2 = ccnymwrprof.timeavg$rmix
attributes(obsdf2$Date.Time)$tzone = tz
obsdf2$Dates = as.Date(obsdf2$Date.Time)
obsdf3 = ccnywl15min$wspd
attributes(obsdf2$Date.Time)$tzone = tz
obsdf3$Dates = as.Date(obsdf3$Date.Time)
obsdf4 = ccnywl15min$wdir
attributes(obsdf4$Date.Time)$tzone = tz
obsdf4$Dates = as.Date(obsdf4$Date.Time)
obsdf5 = ccnywl15min$vesp
attributes(obsdf5$Date.Time)$tzone = tz
obsdf5$Dates = as.Date(obsdf5$Date.Time)

# Load the WRF Data
load("wrf_ccny.data")
wrfdf1 = wrfccny$th
attributes(wrfdf1$Date.Time)$tzone = tz
wrfdf1$Dates = as.Date(wrfdf1$Date.Time)
wrfdf2 = wrfccny$qmix
attributes(wrfdf2$Date.Time)$tzone = tz
wrfdf2$Dates = as.Date(wrfdf2$Date.Time)
wrfdf3 = wrfccny$wspd
attributes(wrfdf3$Date.Time)$tzone = tz
wrfdf3$Dates = as.Date(wrfdf3$Date.Time)
wrfdf4 = wrfccny$wdir
attributes(wrfdf4$Date.Time)$tzone = tz
wrfdf4$Dates = as.Date(wrfdf4$Date.Time)
wrfdf5 = wrfccny$ww
attributes(wrfdf5$Date.Time)$tzone = tz
wrfdf5$Dates = as.Date(wrfdf5$Date.Time)

# Load data separately for wind vectors
obsuu = ccnywl15min$uu
attributes(obsuu$Date.Time)$tzone = tz
obsuu$Dates = as.Date(obsuu$Date.Time)
obsvv = ccnywl15min$vv
attributes(obsvv$Date.Time)$tzone = tz
obsvv$Dates = as.Date(obsvv$Date.Time)
obsdf6 = ccnywl15min$wspd
attributes(obsdf6$Date.Time)$tzone = tz
obsdf6$Dates = as.Date(obsdf6$Date.Time)

wrfuu = wrfccny$uu
attributes(wrfuu$Date.Time)$tzone = tz
wrfuu$Dates = as.Date(wrfuu$Date.Time)
wrfvv = wrfccny$vv
attributes(wrfvv$Date.Time)$tzone = tz
wrfvv$Dates = as.Date(wrfvv$Date.Time)
wrfdf6 = wrfccny$wspd
attributes(wrfdf6$Date.Time)$tzone = tz
wrfdf6$Dates = as.Date(wrfdf6$Date.Time)


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
    titlePanel("CCNY 15-min ObsTime Avg. - WRF Hourly Outputs"),
    
   
   # Sidebar with a slider input for number of bins 
   # Use WRF dates since they are less
    sidebarLayout(
        fluidRow(column(12, 
                        sidebarPanel(
                            sliderInput("dates", paste0("Dates (Times in ", tz, ")"),  
                                        min = unique(obsdf1$Dates)[1],
                                        max = unique(obsdf1$Dates)[8], 
                                        value = as.Date("2018-07-02", tz = tz),
                                        timeFormat = "%Y-%m-%d",
                                        ticks =  FALSE)
                        ) ) ),
      # Show a plot of the generated distribution
      mainPanel(
          # Potential Temperature Row
          fluidRow(
              column(6, style='padding:0px; margin-right:0em',
                     plotOutput(outputId = "obsfc1") ) ,
              column(6, style='padding:0px; margin-right:0em',
                     plotOutput(outputId = "wrffc1") ) ),
          # Mixing Ratio Row
          fluidRow(
              column(6, style='padding:0px; margin-right:0em',
                     plotOutput(outputId = "obsfc2") ),
              column(6, style='padding:0px; margin-right:0em',
                     plotOutput(outputId = "wrffc2") ) ),

          # Wind Speed Row
          fluidRow(
              column(6, style='padding:0px; margin-right:0em',
                     plotOutput(outputId = "obsfc3") ),
              column(6, style='padding:0px; margin-right:0em',
                     plotOutput(outputId = "wrffc3") ) ),

          # Wind Direction Row
          fluidRow(
              column(6, style='padding:0px; margin-right:0em',
                     plotOutput(outputId = "obsfc4") ),
              column(6, style='padding:0px; margin-right:0em',
                     plotOutput(outputId = "wrffc4") ) ),

          # Vertical Wind Speed
          fluidRow(
              column(6, style='padding:0px; margin-right:0em',
                     plotOutput(outputId = "obsfc5") ),
              column(6, style='padding:0px; margin-right:0em',
                     plotOutput(outputId = "wrffc5") ) ),

          # Wind Vector Plots
          plotOutput(outputId = "windvecobs"),
          plotOutput(outputId = "windvecwrf")
         )
      )
)

tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)
# Server Logic for Plots
server <- function(input, output) {
    
    # Potential Temperature Output
    output$obsfc1 <- renderPlot({
       
       select.date = input$dates
       wvar = obsdf1
       xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
       yva = (wvar %>% select(X65:X2065) %>% names() %>% char2height2()) / 1000
       zva = wvar %>% filter(Dates == select.date) %>% select(X65:X2065) %>% as.matrix()
       zlimits = c(290, 320)
       zva = squish(as.matrix(zva), range = zlimits)
       colpal = blue.to.red
       clevels = seq(zlimits[1], zlimits[2], 2)
       plotme.day.shiny.left(xva, yva, zva, clevels, colpal)
   })
    
    output$wrffc1 <- renderPlot({
        
        select.date = input$dates
        wvar = wrfdf1
        xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
        yva = (wvar %>% select(X65:X2065) %>% names() %>% char2height2()) / 1000
        zva = wvar %>% filter(Dates == select.date) %>% select(X65:X2065) %>% as.matrix()
        zlimits = c(290, 320)
        zva = squish(as.matrix(zva), range = zlimits)
        colpal = blue.to.red
        clevels = seq(zlimits[1], zlimits[2], 2)
        plotme.day.shiny.right(xva, yva, zva, clevels, colpal, vartitle = "Potential Temperature (K)")
    })
   
    output$obsfc2 <- renderPlot({
       
       select.date = input$dates
       wvar = obsdf2
       xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
       yva = (wvar %>% select(X65:X2065) %>% names() %>% char2height2()) / 1000
       zva = wvar %>% filter(Dates == select.date) %>% select(X65:X2065) %>% as.matrix()
       zlimits = c(0, 20)
       zva = squish(as.matrix(zva), range = zlimits)
       colpal = div2.rev
       clevels = seq(zlimits[1], zlimits[2], 2)
       plotme.day.shiny.left(xva, yva, zva, clevels, colpal)
   })
    
    output$wrffc2 <- renderPlot({
        
        select.date = input$dates
        wvar = wrfdf2
        xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
        yva = (wvar %>% select(X65:X2065) %>% names() %>% char2height2()) / 1000
        zva = wvar %>% filter(Dates == select.date) %>% select(X65:X2065) %>% as.matrix()
        zlimits = c(0, 20)
        zva = squish(as.matrix(zva), range = zlimits)
        colpal = div2.rev
        clevels = seq(zlimits[1], zlimits[2], 2)
        plotme.day.shiny.right(xva, yva, zva, clevels, colpal, vartitle = "Mixing Ratio (g/kg)")
    })
   
    output$obsfc3 <- renderPlot({
       
       select.date = input$dates
       wvar = obsdf3
       xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
       yva = (wvar %>% select(X115:X2015) %>% names() %>% char2height2()) / 1000
       zva = wvar %>% filter(Dates == select.date) %>% select(X115:X2015) %>% as.matrix()
       zlimits = c(0, 15)
       zva = squish(as.matrix(zva), range = zlimits)
       colpal = viridis
       clevels = seq(zlimits[1], zlimits[2], 2)
       plotme.day.shiny.left(xva, yva, zva, clevels, colpal)
   })
    
    output$wrffc3 <- renderPlot({
        
        select.date = input$dates
        wvar = wrfdf3
        xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
        yva = (wvar %>% select(X115:X2015) %>% names() %>% char2height2()) / 1000
        zva = wvar %>% filter(Dates == select.date) %>% select(X115:X2015) %>% as.matrix()
        zlimits = c(0, 15)
        zva = squish(as.matrix(zva), range = zlimits)
        colpal = viridis
        clevels = seq(zlimits[1], zlimits[2], 2)
        plotme.day.shiny.right(xva, yva, zva, clevels, colpal, vartitle = "Horizontal Wind Speed (m/s)")
    })
   
    output$obsfc4 <- renderPlot({
       
       select.date = input$dates
       wvar = obsdf4
       xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
       yva = (wvar %>% select(X115:X2015) %>% names() %>% char2height2()) / 1000
       zva = wvar %>% filter(Dates == select.date) %>% select(X115:X2015) %>% as.matrix()
       zlimits = c(0, 360)
       zva = squish(as.matrix(zva), range = zlimits)
       colpal = jet.colors
       clevels = seq(zlimits[1], zlimits[2], 22.5)
       plotme.day.shiny.left(xva, yva, zva, clevels, colpal)
   })
    
    output$wrffc4 <- renderPlot({
        
        select.date = input$dates
        wvar = wrfdf4
        xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
        yva = (wvar %>% select(X115:X2015) %>% names() %>% char2height2()) / 1000
        zva = wvar %>% filter(Dates == select.date) %>% select(X115:X2015) %>% as.matrix()
        zlimits = c(0, 360)
        zva = squish(as.matrix(zva), range = zlimits)
        colpal = jet.colors
        clevels = seq(zlimits[1], zlimits[2], 22.5)
        plotme.day.shiny.right(xva, yva, zva, clevels, colpal, vartitle = "Horizontal Wind Direction (deg)")
    })
   
    output$obsfc5 <- renderPlot({
       
       select.date = input$dates
       wvar = obsdf5
       xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
       yva = (wvar %>% select(X115:X2015) %>% names() %>% char2height2()) / 1000
       zva = wvar %>% filter(Dates == select.date) %>% select(X115:X2015) %>% as.matrix()
       zlimits = c(-2, 2)
       zva = squish(as.matrix(zva), range = zlimits)
       colpal = div.pal
       clevels = seq(zlimits[1], zlimits[2], 0.2)
       plotme.day.shiny.left(xva, yva, zva, clevels, colpal)
   })
    
    output$wrffc5 <- renderPlot({
        
        select.date = input$dates
        wvar = wrfdf5
        xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
        yva = (wvar %>% select(X115:X2015) %>% names() %>% char2height2()) / 1000
        zva = wvar %>% filter(Dates == select.date) %>% select(X115:X2015) %>% as.matrix()
        zlimits = c(-2, 2)
        zva = squish(as.matrix(zva), range = zlimits)
        colpal = div.pal
        clevels = seq(zlimits[1], zlimits[2], 0.2)
        plotme.day.shiny.right(xva, yva, zva, clevels, colpal, vartitle = "Vertical Wind Speed (m/s) (+up)")
    })
    
    output$windvecobs <- renderPlot({
        
        select.date = input$dates %>% as.character()
        wvar = obsdf6 %>% filter(Dates == select.date)
        zlimits = c(0, 15)
        uu = obsuu %>% filter(Dates == select.date)
        vv = obsvv %>% filter(Dates == select.date)
        
        # Plotting parameters
        datelabel = paste0("Date: ", select.date)
        uva = uu %>% select(X115:X2015) %>% as.matrix()
        vva = vv %>% select(X115:X2015) %>% as.matrix()
        xva = uu %>% select(Date.Time:sec)
        yva = char2height2(names(uu %>% select(X115:X2015))) / 1000 
        zva = squish(as.matrix(wvar %>% select(X115:X2015)), range = zlimits)
        fc_xlim = xtimelimits.shiny(xva, tz = tz) # x-axis limits.
        
        # Produce the quiver plot;
        # Note that we need to trick the plotter to thinking that the times are
        # in UTC instead of the tz we set above
        par(new = "TRUE", las = 1, cex.axis = 1.3, mai = c(1.0, 0.9, 0.2, 0.2))
            image2D(x = xva$Date.Time %>% as.character() %>% as.POSIXct(tz = "UTC"), 
                    y = yva,
                    z = zva,
                    xlim = fc_xlim,
                    zlim = zlimits,
                    axes = FALSE,
                    xlab = datelabel,
                    alpha = 0.85,
                    col = viridis(7),
                    ylab = "Height (km)", NAcol = "white",
                    clab = "WSPD (m/s)")
            quiver2D(u = uva, 
                     v = vva,
                     x = as.vector(xva$Date.Time %>% as.character() %>% as.POSIXct(tz = "UTC")),
                     y = yva,
                     by = c(4, 2),
                     type = "simple",
                     NAcol = "white",
                     arr.max = 0.45,
                     arr.min = 0.13,
                     add = T)
        axis.POSIXct(1, x = xva$Date.Time, at = seq(fc_xlim[1], fc_xlim[2], by = "hour"), format = "%H:%M")
        axis(2)
        
    })
    
    output$windvecwrf <- renderPlot({
        
        select.date = input$dates
        wvar = wrfdf6 %>% filter(Dates == select.date)
        zlimits = c(0, 15)
        uu = wrfuu %>% filter(Dates == select.date)
        vv = wrfvv %>% filter(Dates == select.date)
        # Plotting parameters
        datelabel = paste0("Date: ", select.date)
        uva = uu %>% select(X115:X2015) %>% as.matrix()
        vva = vv %>% select(X115:X2015) %>% as.matrix()
        xva = uu %>% select(Date.Time:sec)
        yva = char2height2(names(uu %>% select(X115:X2015))) / 1000 
        zva = squish(as.matrix(wvar %>% select(X115:X2015)), range = zlimits)
        fc_xlim = xtimelimits.shiny(xva, tz = tz) # x-axis limits.
        
        
        # Produce the quiver plot
        par(new = "TRUE", las = 1, cex.axis = 1.3, mai = c(1.0, 0.9, 0.2, 0.2))
            image2D(x = xva$Date.Time, 
                    y = yva,
                    z = zva,
                    xlim = fc_xlim,
                    zlim = zlimits,
                    axes = FALSE,
                    xlab = datelabel,
                    alpha = 0.85,
                    col = viridis(7),
                    ylab = "Height (km)", NAcol = "white",
                    clab = "WSPD (m/s)")
            quiver2D(u = uva, 
                     v = vva,
                     x = as.vector(xva$Date.Time),
                     y = yva,
                     by = c(1, 2),
                     type = "simple",
                     NAcol = "white",
                     arr.max = 0.45,
                     arr.min = 0.13,
                     add = T)
        axis.POSIXct(1, x = xva$Date.Time, at = seq(fc_xlim[1], fc_xlim[2], by = "hour"), format = "%H:%M")
        axis(2)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

