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
source("windlidar_nymeso_functions.R")
source("wrf_functions.R")
source("shiny_plotting_functions.R")
tz = "UTC"
# Load the observation data.
dates_of_interest = seq.POSIXt(from = as.POSIXct("2018-06-30", tz = tz), 
                               to   = as.POSIXct("2018-07-03", tz = tz), 
                               by   = "1 day")
select.location <- function(loc) {
  tz = "UTC"
  datalocation = list()
  datalocation$CCNY = c("mwr_ccny15min.data", "wl_ccny15min.data", "wrf_ccny.data")
  datalocation$Bronx = c("mwr_bronx15min.data", "wl_bronx15min.data", "wrf_bronx.data")
  datalocation$Queens = c("mwr_queens15min.data", "wl_queens15min.data", "wrf_queens.data")
  datalocation$StatenIsland = c("mwr_staten_island15min.data", "wl_staten_island15min.data", "wrf_statenisland.data")
  if (loc == "CCNY") {
    for(ff in datalocation[[loc]]) {
      load(ff)
    }
    tmpobsmwr = ccnymwrprof.timeavg
    tmpobswl  = ccnywl15min
    tmpwrf    = wrfccny
  }
  if (loc == "Bronx") {
    for(ff in datalocation[[loc]]) {
      load(ff)
    }
    tmpobsmwr = bronmwrprof
    tmpobswl  = nymesowl_bx15min
    tmpwrf    = wrfbx
  }
  if (loc == "Queens") {
    for(ff in datalocation[[loc]]) {
      load(ff)
    }
    tmpobsmwr = queensmwrprof
    tmpobswl  = nymesowl_qu15min
    tmpwrf    = wrfqu
  }
  if (loc == "StatenIsland") {
    for(ff in datalocation[[loc]]) {
      load(ff)
    }
    tmpobsmwr = stismwrprof
    tmpobswl  = nymesowl_si15min
    tmpwrf    = wrfsi
  }
  return(list(tmpobsmwr = tmpobsmwr, tmpobswl = tmpobswl, tmpwrf = tmpwrf))
}

select.data <- function(tmpdflist) {
  # Load observation data frames
  obsdf1 = tmpdflist$tmpobsmwr$th
  obsdf2 = tmpdflist$tmpobsmwr$rmix
  obsdf3 = tmpdflist$tmpobswl$wspd
  obsdf4 = tmpdflist$tmpobswl$wdir
  obsdf5 = tmpdflist$tmpobswl$ww
  obsdf6 = tmpdflist$tmpobswl$wspd
  obsuu = tmpdflist$tmpobswl$uu
  obsvv = tmpdflist$tmpobswl$vv
  
  # Load WRF data frames
  wrfdf1 = tmpdflist$tmpwrf$th
  wrfdf2 = tmpdflist$tmpwrf$qmix
  wrfdf3 = tmpdflist$tmpwrf$wspd
  wrfdf4 = tmpdflist$tmpwrf$wdir
  wrfdf5 = tmpdflist$tmpwrf$ww
  wrfdf6 = tmpdflist$tmpwrf$wspd
  wrfuu = tmpdflist$tmpwrf$uu
  wrfvv = tmpdflist$tmpwrf$vv
  
  # Apply the desired time zone
  attributes(obsdf1$Date.Time)$tzone = tz
  attributes(obsdf2$Date.Time)$tzone = tz
  attributes(obsdf2$Date.Time)$tzone = tz
  attributes(obsdf4$Date.Time)$tzone = tz
  attributes(obsdf5$Date.Time)$tzone = tz
  attributes(obsdf6$Date.Time)$tzone = tz
  attributes(obsuu$Date.Time)$tzone = tz
  attributes(obsvv$Date.Time)$tzone = tz
  
  attributes(wrfdf1$Date.Time)$tzone = tz
  attributes(wrfdf2$Date.Time)$tzone = tz
  attributes(wrfdf3$Date.Time)$tzone = tz
  attributes(wrfdf4$Date.Time)$tzone = tz
  attributes(wrfdf5$Date.Time)$tzone = tz
  attributes(wrfdf6$Date.Time)$tzone = tz
  attributes(wrfuu$Date.Time)$tzone = tz
  attributes(wrfvv$Date.Time)$tzone = tz
  
  # Create a Dates column from the Date.Time columns
  obsdf1$Dates = as.Date(obsdf1$Date.Time)
  obsdf2$Dates = as.Date(obsdf2$Date.Time)
  obsdf3$Dates = as.Date(obsdf3$Date.Time)
  obsdf4$Dates = as.Date(obsdf4$Date.Time)
  obsdf5$Dates = as.Date(obsdf5$Date.Time)
  obsdf6$Dates = as.Date(obsdf6$Date.Time)
  obsuu$Dates = as.Date(obsuu$Date.Time)
  obsvv$Dates = as.Date(obsvv$Date.Time)
  
  wrfdf1$Dates = as.Date(wrfdf1$Date.Time)
  wrfdf2$Dates = as.Date(wrfdf2$Date.Time)
  wrfdf3$Dates = as.Date(wrfdf3$Date.Time)
  wrfdf4$Dates = as.Date(wrfdf4$Date.Time)
  wrfdf5$Dates = as.Date(wrfdf5$Date.Time)
  wrfdf6$Dates = as.Date(wrfdf6$Date.Time)
  wrfuu$Dates = as.Date(wrfuu$Date.Time)
  wrfvv$Dates = as.Date(wrfvv$Date.Time)
 
  return(list(obsdf1 = obsdf1, obsdf2 = obsdf2, obsdf3 = obsdf3, 
              obsdf4 = obsdf4, obsdf5 = obsdf5, obsdf6 = obsdf6,
              obsuu = obsuu, obsvv = obsvv, 
              wrfdf1 = wrfdf1, wrfdf2 = wrfdf2, wrfdf3 = wrfdf3, 
              wrfdf4 = wrfdf4, wrfdf5 = wrfdf5, wrfdf6 = wrfdf6,
              wrfuu = wrfuu, wrfvv = wrfvv)) 
}

select.data.xhehights <- function(loc) {
  if (loc == "CCNY") {
    mwr_ht1 = "X65"
    mwr_ht2 = "X2065"
    wl_ht1  = "X115"
    wl_ht2  = "X2015"
  }
  if (loc == "Bronx") {
    mwr_ht1 = "X59.31" 
    mwr_ht2 = "X2059.31"
    wl_ht1  = "X159.31"
    wl_ht2  = "X2109.31"
  }
  if (loc == "Queens") {
    mwr_ht1 = "X52.89"
    mwr_ht2 = "X2052.89"
    wl_ht1  = "X152.89"
    wl_ht2  = "X2152.89"
  }
  if (loc == "StatenIsland") {
    mwr_ht1 = "X34.43"
    mwr_ht2 = "X2034.43"
    wl_ht1  = "X134.43"
    wl_ht2  = "X2134.43"
  }
  
  return(c(mwr_ht1, mwr_ht2, wl_ht1, wl_ht2))
}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
    titlePanel("15-min ObsTime Avg. - UrbanJoe1, Urban BEPBEM WRF Hourly Outputs"),
    
   
   # Sidebar with a slider input for number of bins 
   # Use WRF dates since they are less
    sidebarLayout(
      
      fluidRow(column(6, 
                      sidebarPanel(
                        sliderInput("dates", paste0("Dates (Times in ", tz, ")"),  
                                    min = as.Date(min(unique(dates_of_interest))),
                                    max = as.Date(max(unique(dates_of_interest))), 
                                    value = as.Date("2018-07-02", tz = tz),
                                    timeFormat = "%Y-%m-%d",
                                    ticks =  FALSE)
                      ) ),
               column(6,
                      sidebarPanel(
                        selectInput(inputId = "type", label = strong("Location"),
                                    choices = c("CCNY", "Bronx", "Queens", "StatenIsland"),
                                    selected = "CCNY")
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
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates
      wvar = wvarlist$obsdf1
      xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
      yva = (wvar %>% select(xhts[1]:xhts[2]) %>% names() %>% char2height2()) / 1000
      zva = wvar %>% filter(Dates == select.date) %>% select(xhts[1]:xhts[2]) %>% as.matrix()
      zlimits = c(290, 320)
      zva = squish(as.matrix(zva), range = zlimits)
      colpal = blue.to.red
      clevels = seq(zlimits[1], zlimits[2], 2)
      plotme.day.shiny.left(xva, yva, zva, clevels, colpal)
   })
    
    output$wrffc1 <- renderPlot({
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates
      wvar = wvarlist$wrfdf1
      xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
      yva = (wvar %>% select(xhts[1]:xhts[2]) %>% names() %>% char2height2()) / 1000
      zva = wvar %>% filter(Dates == select.date) %>% select(xhts[1]:xhts[2]) %>% as.matrix()
      zlimits = c(290, 320)
      zva = squish(as.matrix(zva), range = zlimits)
      colpal = blue.to.red
      clevels = seq(zlimits[1], zlimits[2], 2)
      plotme.day.shiny.right(xva, yva, zva, clevels, colpal, vartitle = "Potential Temperature (K)")
    })
   
    output$obsfc2 <- renderPlot({
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates
      wvar = wvarlist$obsdf2
      xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
      yva = (wvar %>% select(xhts[1]:xhts[2]) %>% names() %>% char2height2()) / 1000
      zva = wvar %>% filter(Dates == select.date) %>% select(xhts[1]:xhts[2]) %>% as.matrix()
      zlimits = c(0, 20)
      zva = squish(as.matrix(zva), range = zlimits)
      colpal = div2.rev
      clevels = seq(zlimits[1], zlimits[2], 2)
      plotme.day.shiny.left(xva, yva, zva, clevels, colpal)
   })
    
    output$wrffc2 <- renderPlot({
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates
      wvar = wvarlist$wrfdf2
      xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
      yva = (wvar %>% select(xhts[1]:xhts[2]) %>% names() %>% char2height2()) / 1000
      zva = wvar %>% filter(Dates == select.date) %>% select(xhts[1]:xhts[2]) %>% as.matrix()
      zlimits = c(0, 20)
      zva = squish(as.matrix(zva), range = zlimits)
      colpal = div2.rev
      clevels = seq(zlimits[1], zlimits[2], 2)
      plotme.day.shiny.right(xva, yva, zva, clevels, colpal, vartitle = "Mixing Ratio (g/kg)")
    })
   
    output$obsfc3 <- renderPlot({
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates
      wvar = wvarlist$obsdf3
      xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
      yva = (wvar %>% select(xhts[3]:xhts[4]) %>% names() %>% char2height2()) / 1000
      zva = wvar %>% filter(Dates == select.date) %>% select(xhts[3]:xhts[4]) %>% as.matrix()
      zlimits = c(0, 15)
      zva = squish(as.matrix(zva), range = zlimits)
      colpal = viridis
      clevels = seq(zlimits[1], zlimits[2], 2)
      plotme.day.shiny.left(xva, yva, zva, clevels, colpal)
   })
    
    output$wrffc3 <- renderPlot({
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates
      wvar = wvarlist$wrfdf3
      xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
      yva = (wvar %>% select(xhts[3]:xhts[4]) %>% names() %>% char2height2()) / 1000
      zva = wvar %>% filter(Dates == select.date) %>% select(xhts[3]:xhts[4]) %>% as.matrix()
      zlimits = c(0, 15)
      zva = squish(as.matrix(zva), range = zlimits)
      colpal = viridis
      clevels = seq(zlimits[1], zlimits[2], 2)
      plotme.day.shiny.right(xva, yva, zva, clevels, colpal, vartitle = "Horizontal Wind Speed (m/s)")
    })
   
    output$obsfc4 <- renderPlot({
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates
      wvar = wvarlist$obsdf4
      xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
      yva = (wvar %>% select(xhts[3]:xhts[4]) %>% names() %>% char2height2()) / 1000
      zva = wvar %>% filter(Dates == select.date) %>% select(xhts[3]:xhts[4]) %>% as.matrix()
      zlimits = c(0, 360)
      zva = squish(as.matrix(zva), range = zlimits)
      colpal = jet.colors
      clevels = seq(zlimits[1], zlimits[2], 22.5)
      plotme.day.shiny.left(xva, yva, zva, clevels, colpal)
   })
    
    output$wrffc4 <- renderPlot({
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates
      wvar = wvarlist$wrfdf4
      xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
      yva = (wvar %>% select(xhts[3]:xhts[4]) %>% names() %>% char2height2()) / 1000
      zva = wvar %>% filter(Dates == select.date) %>% select(xhts[3]:xhts[4]) %>% as.matrix()
      zlimits = c(0, 360)
      zva = squish(as.matrix(zva), range = zlimits)
      colpal = jet.colors
      clevels = seq(zlimits[1], zlimits[2], 22.5)
      plotme.day.shiny.right(xva, yva, zva, clevels, colpal, vartitle = "Horizontal Wind Direction (deg)")
    })
   
    output$obsfc5 <- renderPlot({
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates
      wvar = wvarlist$obsdf5
      xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
      yva = (wvar %>% select(xhts[3]:xhts[4]) %>% names() %>% char2height2()) / 1000
      zva = wvar %>% filter(Dates == select.date) %>% select(xhts[3]:xhts[4]) %>% as.matrix()
      zlimits = c(-2, 2)
      zva = squish(as.matrix(zva), range = zlimits)
      colpal = div.pal
      clevels = seq(zlimits[1], zlimits[2], 0.2)
      plotme.day.shiny.left(xva, yva, zva, clevels, colpal)
   })
    
    output$wrffc5 <- renderPlot({
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates
      wvar = wvarlist$wrfdf5
      xva = wvar %>% filter(Dates == select.date) %>% select(Date.Time:sec)
      yva = (wvar %>% select(xhts[3]:xhts[4]) %>% names() %>% char2height2()) / 1000
      zva = wvar %>% filter(Dates == select.date) %>% select(xhts[3]:xhts[4]) %>% as.matrix()
      zlimits = c(-2, 2)
      zva = squish(as.matrix(zva), range = zlimits)
      colpal = div.pal
      clevels = seq(zlimits[1], zlimits[2], 0.2)
      plotme.day.shiny.right(xva, yva, zva, clevels, colpal, vartitle = "Vertical Wind Speed (m/s) (+up)")
    })
    
    output$windvecobs <- renderPlot({
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates %>% as.character()
      wvar = wvarlist$obsdf6 %>% filter(Dates == select.date)
      zlimits = c(0, 15)
      uu = wvarlist$obsuu %>% filter(Dates == select.date)
      vv = wvarlist$obsvv %>% filter(Dates == select.date)
      
      # Plotting parameters
      datelabel = paste0("Date: ", select.date)
      uva = uu %>% select(xhts[3]:xhts[4]) %>% as.matrix()
      vva = vv %>% select(xhts[3]:xhts[4]) %>% as.matrix()
      xva = uu %>% select(Date.Time:sec)
      yva = char2height2(names(uu %>% select(xhts[3]:xhts[4]))) / 1000 
      zva = squish(as.matrix(wvar %>% select(xhts[3]:xhts[4])), range = zlimits)
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
                  alpha = 0.95,
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
      data_at_loc = select.location(input$type)
      xhts = select.data.xhehights(input$type)
      wvarlist = select.data(data_at_loc)
      select.date = input$dates
      wvar = wvarlist$wrfdf6 %>% filter(Dates == select.date)
      zlimits = c(0, 15)
      uu = wvarlist$wrfuu %>% filter(Dates == select.date)
      vv = wvarlist$wrfvv %>% filter(Dates == select.date)
      # Plotting parameters
      datelabel = paste0("Date: ", select.date)
      uva = uu %>% select(xhts[3]:xhts[4]) %>% as.matrix()
      vva = vv %>% select(xhts[3]:xhts[4]) %>% as.matrix()
      xva = uu %>% select(Date.Time:sec)
      yva = char2height2(names(uu %>% select(xhts[3]:xhts[4]))) / 1000 
      zva = squish(as.matrix(wvar %>% select(xhts[3]:xhts[4])), range = zlimits)
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
                  alpha = 0.95,
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

