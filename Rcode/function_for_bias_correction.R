require(dplyr)
require(chillR)
#Loading the stations data 
Weather_Obs_List <-  chillR::load_temperature_scenarios("Stations_Obs", prefix = "")
Weather_Sim_List <- chillR::load_temperature_scenarios("Stations_Sim", prefix = "")

#Since the files names are not there, we used list.file to show the names
fnames <- list.files("Stations_Obs")
#The names are split by _ however there are still three characters now we assign it to 
#each name using purrr function which works as a loop.
Weather_Names <- str_split(fnames, pattern = "_") %>% 
  purrr::map_chr(1)

#assign the list of names to the data set
names(Weather_Obs_List) <- Weather_Names
names(Weather_Sim_List) <- Weather_Names

#writing a loop
for(i in 1:length(Weather_Obs_List)){
  
  Weather_Sim <- Weather_Sim_List[[i]]
  Weather_Obs <- Weather_Obs_List[[i]]
  
  rm(Historic_BC,Recent_Obs,weather_obs,weather_obs_list,weather_sim,weather_sim_list)
  colnames(Weather_Sim) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
  
  colnames(Weather_Obs)[6] <- "Tmin_Obs"
  colnames(Weather_Obs)[7] <- "Tmax_Obs"
  
  start_date <- as.Date(Weather_Obs$Date[1])
  end_date <- as.Date(Weather_Obs$Date[nrow(Weather_Obs)])
  
  Weather_Sim <- Weather_Sim[as.Date(Weather_Sim$Date) >=  start_date & as.Date(Weather_Sim$Date) <=  end_date,]
  
  #Merging the two Obs and Sim to ease plotting
  Weather_Obs_Sim <- merge(Weather_Obs, Weather_Sim, by = c("Date"))
  
  #Choosing the columns needed
  Weather_Obs_Sim <- Weather_Obs_Sim[c("Date", "Year", "Month", "Day", "Tmin_Obs", 
                                         "Tmax_Obs", "Tmin_Sim", "Tmax_Sim")]
  
  #Plotting
  require(ggplot2)
  # Plotting Tmin for both Obs and Sim 
  Weather_Plot_Tmin <- ggplot(data = Weather_Obs_Sim, aes(x = Tmin_Obs, y = Tmin_Sim)) + 
    geom_point(alpha = 0.2)+
    geom_abline(slope = 1, linetype = "dashed") +
    xlab("Tmin Observed") + 
    ylab("Tmin Simulated") + 
    scale_y_continuous(limits = c(-20,40)) +
    scale_x_continuous(limits = c(-20,40)) +
    facet_wrap( ~ format(x = as.Date(Date))) +
    facet_wrap( ~ format(as.Date(Date), "%Y"))
  
  ggsave(Weather_Plot_Tmin <- paste0("Plots/", Weather_Names[i],"_Plot_Tmin.jpeg"))
  
  # Plotting Adraskan Tmax for both Obs and Sim 
  Weather_Plot_Tmax <- ggplot(data = Weather_Obs_Sim, aes(x = Tmax_Obs[[i]], y = Tmax_Sim[[i]])) + 
    geom_point(alpha = 0.2)+
    geom_abline(slope = 1, linetype = "dashed") +
    xlab("Tmax Observed") + 
    ylab("Tmax Simulated") + 
    scale_y_continuous(limits = c(-20,40)) +
    scale_x_continuous(limits = c(-20,40)) +
    facet_wrap( ~ format(x = as.Date(Date))) +
    facet_wrap( ~ format(as.Date(Date), "%Y"))
  
  ggsave(Weather_Plot_Tmax <- paste0("Plots/", Weather_Names[i], "_Plot_Tmax.jpeg"))
  
  #Merging the two plots Tmin and Tmax
  Weather_Plot_Tmin_Tmax <- Weather_Plot_Tmin + Weather_Plot_Tmax
  
  ggsave(Weather_Plot_Tmin_Tmax <- paste0("Plots/", Weather_Names[i], "_Plot_Tmin_Tmax"))
  #fname <- paste0("Plots/", weather_names[i],"_Plot_Tmax.jpeg")
  
  
  # Calculating the difference between Obs and Sim data for stations (2008:2020)
  library(tidyverse)
  require(lubridate)
  require(kableExtra)
  # Since we do a linear bias correction we do it based on the months of each year using lubridate function
  Weather_Obs_Sim$Month <- lubridate::month(Weather_Obs_Sim$Date)
  
  # Now we calculate the difference and other statistical indicators for Tmin and Tmax
  Weather_Statistics <- Weather_Obs_Sim %>% 
    group_by(Month) %>% 
    # For Tmin
    summarise(Mean_Bias_Tmin = mean(Tmin_Obs-Tmin_Sim),
              SD_Tmin=sd(Tmin_Obs-Tmin_Sim),
              #RMSEP_Tmin=RMSEP(Tmin_Sim,Tmin_Obs),
              #RPIQ_Tmin=RPIQ(Tmin_Sim,Tmin_Obs),
              
              #For Tmax
              Mean_Bias_Tmax = mean(Tmax_Obs-Tmax_Sim),
              SD_Tmax=sd(Tmax_Obs-Tmax_Sim))
  #RMSEP_Tmax=RMSEP(Tmax_Sim,Tmax_Obs),
  #RPIQ_Tmax=RPIQ(Tmax_Sim,Tmax_Obs))
  
  kable(Weather_Statistics[i]) %>%
    kable_styling("striped", position = "left", font_size = 10)

  # Comparison between available temperature data 
  
  #Now merging these statistical indicators into the original dataframe to ease bias correction in the next functions
  Weather_Obs_Sim <- merge(Weather_Obs_Sim, Weather_Statistics, by = "Month")
  
  #Bias correction
  
  #Now I will do bias correction for Tmin and Tmax in new columns as Tmin_BC and Tmax_BC:  
  # Tmin 
  Weather_Obs_Sim$TminBC <- Weather_Obs_Sim$Tmin_Sim + Weather_Obs_Sim$Mean_Bias_Tmin
  
  # Tmax
  Weather_Obs_Sim$TmaxBC <- Weather_Obs_Sim$Tmax_Sim + Weather_Obs_Sim$Mean_Bias_Tmax

  
  # Again calculating the statistical indicators for Tmin and Tmax after bias correction
  
  Weather_Statistics_BC <- Weather_Obs_Sim %>% 
    group_by(Month) %>% 
    
    # For Tmin
    summarise(#Mean_Bias_Tmin = mean(Tmin_Obs-TminBC),
      #SD_Tmin=sd(Tmin_Obs-TminBC),
      RMSEP_Tmin=RMSEP(Tmin_Sim,Tmin_Obs),
      RMSEP_TminBC=RMSEP(TminBC, Tmin_Obs),
      RPIQ_Tmin=RPIQ(Tmin_Sim,Tmin_Obs),
      RPIQ_TminBC=RPIQ(TminBC, Tmin_Obs),
      
      
      #For Tmax
      #Mean_Bias_Tmax = mean(Tmax_Obs-TmaxBC),
      #SD_Tmax=sd(Tmax_Obs-TmaxBC),
      RMSEP_Tmax=RMSEP(Tmax_Sim,Tmax_Obs),
      RMSEP_TmaxBC=RMSEP(TmaxBC, Tmax_Obs),
      RPIQ_Tmax=RPIQ(Tmax_Sim,Tmax_Obs),
      RPIQ_TmaxBC=RPIQ(TmaxBC, Tmax_Obs))
  
  
  
  
  kable(Weather_Statistics_BC) %>%
    kable_styling("striped", position = "left", font_size = 10)
  
  # Calculating the overall statistics
  
  mean(Weather_Obs_Sim$Tmin_Obs - Weather_Obs_Sim$Tmin_Sim)
  mean(Weather_Obs_Sim$Tmin_Obs - Weather_Obs_Sim$TminBC)
  
  sd(Weather_Obs_Sim$Tmin_Obs - Weather_Obs_Sim$Tmin_Sim)
  sd(Weather_Obs_Sim$Tmin_Obs - Weather_Obs_Sim$TminBC)
  
  RMSEP(Weather_Obs_Sim$Tmin_Sim, Weather_Obs_Sim$Tmin_Obs)
  RMSEP(Weather_Obs_Sim$TminBC, Weather_Obs_Sim$Tmin_Obs)
  
  RPIQ(Weather_Obs_Sim$Tmin_Sim, Weather_Obs_Sim$Tmin_Obs)
  RPIQ(Weather_Obs_Sim$TminBC, Weather_Obs_Sim$Tmin_Obs)
  
  # Bias correction is completed for the period with available observed data, I am saving it
  
  write.csv(paste0(Weather_Obs_Sim, "Stations_Obs_Sim/", Weather_Names[i], "_Obs_Sim.csv"))
  write.csv(paste0(Weather_Statistics_BC, "Stations_Obs_Sim/", Weather_Names[i], "_Statistics_BC.csv"))
  
  
  # Bias correction of historical temperature data (with no observed data)
  
  Weather_Simulated <- read.csv("D:/Rdata/Stations_Sim/Weather_Simulated.csv")
  
  #Renaming the column names of Simulated stations
  colnames(Weather_Simulated) <- c("Date", "Tmin_Sim", "Tmax_Sim")
   
  #Extract the desired time series from simulated stations (1980-2008 in most cases)
  start_date2 <- "1980-01-01"
  Weather_Simulated <- Weather_Simulated[as.Date(Weather_Simulated$Date) ==  as.Date(start_date) & as.Date(Weather_Simulated$Date) ==  start_date,]
  
  #Bias correction
  Weather_Simulated$Month <- lubridate::month(Weather_Simulated$Date)
  
  Weather_Simulated <- merge(Weather_Simulated, Weather_Statistics, by = "Month")
  
  #Now I will do bias correction for Tmin and Tmax in new columns as Tmin_BC and Tmax_BC:  
  # Tmin
    Weather_Simulated$TminBC <- Weather_Simulated$Tmin_Sim + Weather_Simulated$Mean_Bias_Tmin
    
  # Tmax
    Weather_Simulated$TmaxBC <- Weather_Simulated$Tmax_Sim + Weather_Simulated$Mean_Bias_Tmax
    
    #Merging the two temperature series (Observed with bias corrected)
    
    #1 Cleaning recent (Obs)
    #Choosing the wanted columns
    Recent_Obs <- Weather_Obs_Sim[,c("Date", "TminBC", "TmaxBC")]
    
    #Renaming the column names
    colnames(Recent_Obs)[2] <- "Tmin"
    colnames(Recent_Obs)[3] <- "Tmax"

    #2 Cleaning historic (BC)
    #Choosing the wanted columns
    Historic_BC <- Weather_Simulated[,c("Date", "TminBC", "TmaxBC")]
    
    #Renaming the column names
    colnames(Historic_BC)[2] <- "Tmin"
    colnames(Historic_BC)[3] <- "Tmax"

    # Merging the two data frame by stacking on the top of each other- both rows should have same columns
    Weather_AT <- rbind(Historic_BC, Recent_Obs)
    
    rownames(Weather_AT) <- 1:nrow(Weather_AT)
    print(Weather_AT)
    
    write.csv(paste0(Weather_AT, "Stations_Obs_Sim/", Weather_Names[i], "_AT.csv"))
    
    
}
