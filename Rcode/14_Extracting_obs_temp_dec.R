
#------------------------------------------------------------------------------------------#
################# Extracting mean temp December from observed station data ###################
#------------------------------------------------------------------------------------------#

# Since, we have already calculated mean monthly temps of worldclim data and added it to the data frame All_chill.
# Now we will also do the same for our real observed data for all stations and then compare the results for bias correction.
# The month of focus is December. It means I will calculate mean of all Decembers from 1980:2020

library(stringr) #needed for functions to extract the stations name from the file names
library(ggplot2)
library(ggrepel) #needed to avoid overlap of labels
library(tidyverse)

# read stations (Station_AT_List)
temp1 <- Station_AT_List

# In order to get temps of all Decembers and then calculate the mean lets define min and max years 
min_year <- 1980
max_year <- 2020

# Define a function for calculating mean
calc_tmean <- function(df, month, max_year, min_year){
  #subset the data frame
  sub1 <- subset(df, Year >= min_year & Year <= max_year & Month == month)
  return(c(mean((sub1$Tmax + sub1$Tmin)/2, na.rm = T),mean(sub1$Tmin, na.rm = T),mean(sub1$Tmax, na.rm = T)))
}

# Define a function for standard deviation
calc_tmean_sd <- function(df, month, max_year, min_year){
  #subset the data frame
  sub1 <- subset(df, Year >= min_year & Year <= max_year & Month == month)
  return(c(sd((sub1$Tmax + sub1$Tmin)/2, na.rm = T),sd(sub1$Tmin, na.rm = T),sd(sub1$Tmax, na.rm = T)))
}

# For first station, calculate mean temperature and sd of mean temp of December
t_dec <- calc_tmean(temp1$Adraskan, 12, 2020,1980)
t_dec_sd <- calc_tmean_sd(temp1$Adraskan, 12, 2020,1980)

# create an empty data frame
temp_dec <- data.frame(Station_ID = character(),
                       obs_avg_temp_dec = numeric(),
                       obs_tmin_dec = numeric(),
                       obs_tmax_dec = numeric(),
                       sd_obs_avg_temp_dec = numeric(),
                       sd_obs_tmin_dec = numeric(),
                       sd_obs_tmax_dec = numeric(),
                       stringsAsFactors = FALSE)

# Put the calculated values in a data frame
temp_dec_values <- data.frame(Station_ID = Stations_Names[1],
                              obs_avg_temp_dec = t_dec[1],
                              obs_tmin_dec = t_dec[2],
                              obs_tmax_dec = t_dec[3],
                              sd_obs_avg_temp_dec = t_dec_sd[1],
                              sd_obs_tmin_dec = t_dec_sd[2],
                              sd_obs_tmax_dec = t_dec_sd[3],
                              stringsAsFactors = FALSE)

# Combine the calculated values into the empty data frame
temp_dec <- rbind(temp_dec, temp_dec_values)



# Apply to other stations in a loop: calculate mean monthly data of December for all station from 1980:2020

#create an empty data frame
temp_dec <- data.frame(Station_ID = character(),
                       obs_avg_temp_dec = numeric(),
                       obs_tmin_dec = numeric(),
                       obs_tmax_dec = numeric(),
                       sd_obs_avg_temp_dec = numeric(),
                       sd_obs_tmin_dec = numeric(),
                       sd_obs_tmax_dec = numeric(),
                       stringsAsFactors = FALSE)

for (i in 1: length(Stations_Names)) {
  station_data <- temp1[[i]]
  
  #calculate mean temperature and sd of mean temp of December
  t_dec <- calc_tmean(station_data, 12, 2020,1980)
  t_dec_sd <- calc_tmean_sd(station_data, 12, 2020,1980)
  
  
  # Create a row with the calculated values
  temp_dec_values <- data.frame(Station_ID = Stations_Names[i],
                                obs_avg_temp_dec = t_dec[1],
                                obs_tmin_dec = t_dec[2],
                                obs_tmax_dec = t_dec[3],
                                sd_obs_avg_temp_dec = t_dec_sd[1],
                                sd_obs_tmin_dec = t_dec_sd[2],
                                sd_obs_tmax_dec = t_dec_sd[3],
                                stringsAsFactors = FALSE)
  
  temp_dec <- rbind(temp_dec, temp_dec_values)
  
}

#round the data because we dont need so many digits
temp_dec[,-1] <- round(temp_dec[,-1],digits = 2)

#read the All_chill and add this as well

# merge
All_chill <- merge(All_chill, temp_dec, by = 'Station_ID')

#update the All_chill data frame
write.csv(All_chill, "D:/Rdata/Chill_quantification/SWC/All_chill.csv", row.names = FALSE)


