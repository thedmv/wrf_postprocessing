# Initial libraries
suppressPackageStartupMessages(library(ncdf4))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggquiver))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(circular)) # for circular histogram
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(zoo)) # for the rollapply function
suppressPackageStartupMessages(library(openair)) # for timeAverage function
suppressPackageStartupMessages(library(circular)) # for averaging wind direction
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(scales)) #for squish function
suppressPackageStartupMessages(library(OceanView)) # for quiver2D
# suppressPackageStartupMessages(library(Metrics)) # for rmse
suppressPackageStartupMessages(library(hydroGOF)) # for rmse
suppressPackageStartupMessages(library(directlabels)) # for labels on RMSE ggplots

# Function for wind direction calculation
wd_dif = function(sim, obs) {
    # This function calculates the difference between a simulated and an observed
    # wind direction. paper: PA Jimenez & J Dudhia - https://doi.org/10.1175/JAMC-D-12-0266.1
    
    # Quote from paper: "This definition assigns positive (negative) differences if 
    # the WRF direction is rotated clockwise (counterclockwise) with respect to the
    # observed records. The values of del_d are therefore in the range [-180, 180]"
    
    # 1. Calculate the difference
    del_diff = sim - obs
    
    # 2. Condition 2 adjustment del_diff > 180, make del_diff = del_diff - 360
    for (height in seq_along(del_diff)) {
        adjust = del_diff[[height]]
        adjust[adjust > 180 & !is.na(adjust)] = adjust[adjust > 180 & !is.na(adjust)] - 360 
        del_diff[[height]] = adjust
    }
    
    # 3. Condition 3 adjustment del_diff < -180, make del_diff = del_diff + 360
    
    for (height in seq_along(del_diff)) {
        adjust = del_diff[[height]]
        adjust[adjust < -180 & !is.na(adjust)] = adjust[adjust < -180 & !is.na(adjust)] + 360
        del_diff[[height]] = adjust
    }
    
    return(del_diff)
}

wd_dif_rmse = function(sim, obs) {
    # Calculates the RMSE from the difference input.
    del_diff = wd_dif(sim, obs)
    rmse_val = list()
    
    for(col in seq_along(del_diff)) {
        vec = del_diff[[col]]
        rmse_val[[col]] = sqrt(sum(vec^2)/length(as.vector(vec)))
    }
    rmse_val = do.call("rbind", rmse_val)
    rownames(rmse_val) = names(del_diff)
    
    return(rmse_val)
}
