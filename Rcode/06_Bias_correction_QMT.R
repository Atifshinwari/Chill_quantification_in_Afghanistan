#Loading the stations data 
devtools::install_github(c("SantanderMetGroup/transformeR", "SantanderMetGroup/downscaleR"))
require(downscaleR)
require(kableExtra)
require(zoo)
require(latticeExtra)
require(QMTap)
require(chillR)
require(ggplot2)
require(dplyr)
require(stringr)
require(lubridate)

#------------------------------------------------------------------------------------------#
# 01: Looping the QDM for all remaining stations
#------------------------------------------------------------------------------------------#

#Loading the stations data
StationQMT_Obs_List <-  chillR::load_temperature_scenarios("D:/Rdata/QMT/Stations_Obs_QMT", prefix = "")
StationQMT_Sim_List <- chillR::load_temperature_scenarios("D:/Rdata/QMT/Stations_Sim_QMT", prefix = "")
# Again loading simulated because I will need it for later on use
StationQMT_Sim_His_List <- chillR::load_temperature_scenarios("D:/Rdata/QMT/Stations_Sim_QMT", prefix = "")

filenamesQMT <- list.files("D:/Rdata/QMT/Stations_Obs_QMT")
# The names are split by '_', however there are still three characters now we assign it to 
# each name using purrr function which works as a loop.
StationQMT_Names <- str_split(filenamesQMT, pattern = "_") %>% 
  purrr::map_chr(1)

# assign the list of names to the data set
names(StationQMT_Obs_List) <- StationQMT_Names
names(StationQMT_Sim_List) <- StationQMT_Names
names(StationQMT_Sim_His_List) <- StationQMT_Names

StationQMT_Statistics_List <- list()

pb = txtProgressBar(min = 0, max = length(StationQMT_Obs_List), initial = 0, style = 3) 

for(i in 1:length(StationQMT_Obs_List)){
  
  StationQMT_Obs <- StationQMT_Obs_List[[i]]
  StationQMT_Sim <- StationQMT_Sim_List[[i]]
  StationQMT_Sim_His <- StationQMT_Sim_His_List[[i]]
  
  StationQMT_Obs <- as.data.frame(StationQMT_Obs)
  StationQMT_Sim <- as.data.frame(StationQMT_Sim)
  StationQMT_Sim_His <- as.data.frame(StationQMT_Sim_His)
  
  colnames(StationQMT_Sim) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
  StationQMT_Sim$Tmin_Sim <- as.numeric(StationQMT_Sim$Tmin_Sim)
  StationQMT_Sim$Tmax_Sim <- as.numeric(StationQMT_Sim$Tmax_Sim)
  
  colnames(StationQMT_Obs)[6] <- "Tmin_Obs"
  colnames(StationQMT_Obs)[7] <- "Tmax_Obs"
  StationQMT_Obs$Tmin_Obs <- as.numeric(StationQMT_Obs$Tmin_Obs)
  StationQMT_Obs$Tmax_Obs <- as.numeric(StationQMT_Obs$Tmax_Obs)
  
  # test which date format we have
  
  candidate1 <- lubridate::ymd(StationQMT_Obs$Date[1])
  candidate2 <- lubridate::dmy(StationQMT_Obs$Date[1])
  #candidate3 <- lubridate::dmy_hms(Weather_Obs$Date[1])
  
  #take the candidate which is not an NA
  if(is.na(candidate1) == FALSE & is.na(candidate2) == TRUE){
    start_date <- candidate1
    end_date <- lubridate::ymd(StationQMT_Obs$Date[nrow(StationQMT_Obs)])
    StationQMT_Obs$Date <- lubridate::ymd(StationQMT_Obs$Date)
    
    
  } else if(is.na(candidate1) == TRUE & is.na(candidate2) == FALSE){
    start_date <- candidate2
    end_date <- lubridate::dmy(StationQMT_Obs$Date[nrow(StationQMT_Obs)])
    StationQMT_Obs$Date <- lubridate::dmy(StationQMT_Obs$Date)
  }
  
  
  StationQMT_Sim <- StationQMT_Sim[lubridate::ymd(StationQMT_Sim$Date) >=  start_date & lubridate::ymd(StationQMT_Sim$Date) <=  end_date,]
  
  # I will make sure that weather_sim$Date is in a date format
  # otherwise merging does not work
  StationQMT_Sim$Date <- lubridate::ymd(StationQMT_Sim$Date)
  
  # Historic
  colnames(StationQMT_Sim_His) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
  StationQMT_Sim_His$Tmin_Sim <- as.numeric(StationQMT_Sim_His$Tmin_Sim)
  StationQMT_Sim_His$Tmax_Sim <- as.numeric(StationQMT_Sim_His$Tmax_Sim)
  
  start_date2 <- as.Date("1980-01-01")
  StationQMT_Sim_His <- StationQMT_Sim_His[as.Date(StationQMT_Sim_His$Date) >=  as.Date(start_date2) & as.Date(StationQMT_Sim_His$Date) <  start_date,]
  
  
  # Define the observed and modeled data for quantile mapping
  Obs_Tmin <- StationQMT_Obs$Tmin_Obs
  Sim_Tim <- StationQMT_Sim$Tmin_Sim
  Hist_Tmin <- StationQMT_Sim_His$Tmin_Sim
  
  # Tmin QMT for calibration period
  QMTapFittingTmin <- qmap::fitQmapQUANT(obs = Obs_Tmin, mod = Sim_Tim, wet.day = FALSE, qstep=0.01,
                                        nboot = 1)
  QMTapTminL <- qmap::doQmapQUANT(x = Sim_Tim, fobj = QMTapFittingTmin, type = "tricub")
  
  Station_BC_TminL <- data.frame(QMTapTminL)
  colnames(Station_BC_TminL) <-  c("Station_BC_Tmin_L")
  
  # Tmax QMT for calibration period
  Obs_Tmax <- StationQMT_Obs$Tmax_Obs
  Sim_Tmax <- StationQMT_Sim$Tmax_Sim
  Hist_Tmax <- StationQMT_Sim_His$Tmax_Sim
  
  QMTapFitting <- qmap::fitQmapQUANT(obs = Obs_Tmax, mod = Sim_Tmax, wet.day = FALSE, qstep=0.01,
                                    nboot = 1)
  QMTapTmaxL <- qmap::doQmapQUANT(x = Sim_Tmax, fobj = QMTapFitting, type = "tricub")
  
  Station_BC_TmaxL <- data.frame(QMTapTmaxL)
  colnames(Station_BC_TmaxL) <-  c("Station_BC_Tmax_L")
  
  StationQMT_Obs_Sim <- cbind(StationQMT_Obs, StationQMT_Sim, Station_BC_TminL, Station_BC_TmaxL)
  StationQMT_Obs_Sim <- StationQMT_Obs_Sim[c("Date", "Year", "Month", "Day", "Tmin_Obs", "Tmax_Obs", "Tmin_Sim", "Tmax_Sim", "Station_BC_Tmin_L", "Station_BC_Tmax_L")]
  
  #Statistics
  # For Tmin
  StationQMT_Stats_Tmin <- StationQMT_Obs_Sim %>% 
    summarise(Mean_Bias_Tmin = mean(Tmin_Obs-Tmin_Sim),
              #Mean_Bias_TminBC = mean(Tmin_Obs-QDM_Tmin),
              SD_Tmin=sd(Tmin_Obs-Tmin_Sim),
              SD_TminBC=sd(Tmin_Obs-Station_BC_Tmin_L),
              RMSEP_Tmin=RMSEP(Tmin_Sim,Tmin_Obs),
              RMSEP_TminBC=RMSEP(Station_BC_Tmin_L, Tmin_Obs),
              RPIQ_Tmin=RPIQ(Tmin_Sim,Tmin_Obs),
              RPIQ_TminBC=RPIQ(Station_BC_Tmin_L, Tmin_Obs))
  
  #For Tmax
  StationQMT_Stats_Tmax <- StationQMT_Obs_Sim %>% 
    summarise(Mean_Bias_Tmax = mean(Tmax_Obs-Tmax_Sim),
              #Mean_Bias_TmaxBC = mean(Tmax_Obs-QDM_Tmax),
              SD_Tmax=sd(Tmax_Obs-Tmax_Sim),
              SD_TmaxBC=sd(Tmax_Obs-Station_BC_Tmax_L),
              RMSEP_Tmax=RMSEP(Tmax_Sim,Tmax_Obs),
              RMSEP_TmaxBC=RMSEP(Station_BC_Tmax_L, Tmax_Obs),
              RPIQ_Tmax=RPIQ(Tmax_Sim,Tmax_Obs),
              RPIQ_TmaxBC=RPIQ(Station_BC_Tmax_L, Tmax_Obs))
  
  StationQMT_average_statistics <- cbind(StationQMT_Stats_Tmin, StationQMT_Stats_Tmax)
  
  StationQMT_Statistics_List[[i]] <- StationQMT_average_statistics
  
  write.csv(StationQMT_average_statistics, file = paste0("D:/Rdata/QMT/Stations_Obs_Sim_QMT/Calib_", StationQMT_Names[i], "_Statistics.csv"), row.names = FALSE)
  
  #Here I would like to save the bias corrected calibration period
  write.csv(StationQMT_Obs_Sim, paste0("D:/Rdata/QMT/Stations_Obs_Sim_QMT/Recent_", StationQMT_Names[i], "_BC_QMT.csv"))
  
  # Plotting
  
  # Plotting Calibration Period
  StationQMT_Plot_Tmin <- ggplot(StationQMT_Obs_Sim, aes(x = Date, y = StationQMT_Obs_Sim$Tmin_Obs, group = 1)) + 
    geom_line(lwd=0.3) + 
    xlab("Time series of temperature data") + 
    ylab("Daily minimum temperature (°C)") + 
    #scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*700)]) +
    geom_line(data = StationQMT_Obs_Sim, aes(x = Date, y = StationQMT_Obs_Sim$Tmin_Sim), col="blue",lwd=0.3) +
    geom_line(data = StationQMT_Obs_Sim, aes(x = Date, y = StationQMT_Obs_Sim$Station_BC_Tmin_L), col="red",lwd=0.3) + 
    #scale_y_continuous(limits = c(-30,45)) + 
    expand_limits(y = c(min(StationQMT_Obs_Sim$Tmin_Obs), max(StationQMT_Obs_Sim$Tmin_Obs))) +
    facet_wrap(~ format(Date, "%Y"), scales = "free_x")
  
  StationQMT_Plot_Tmin <- StationQMT_Plot_Tmin + scale_x_date(date_labels = "%b")
  
  ggsave(plot = StationQMT_Plot_Tmin, filename = paste0("D:/Rdata/QMT/Plots_QMT/", StationQMT_Names[i],"_Plot_TminQMT.jpeg"), height = 13, width = 24, units = "cm" )
  
  
  StationQMT_Plot_Tmax <- ggplot(StationQMT_Obs_Sim, aes(x = Date, y = StationQMT_Obs_Sim$Tmax_Obs, group = 1)) + 
    geom_line(lwd=0.3) + 
    xlab("Time series of temperature data") + 
    ylab("Daily maximum temperature (°C)") + 
    #scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*700)]) +
    geom_line(data = StationQMT_Obs_Sim, aes(x = Date, y = StationQMT_Obs_Sim$Tmax_Sim), col="blue",lwd=0.3) +
    geom_line(data = StationQMT_Obs_Sim, aes(x = Date, y = StationQMT_Obs_Sim$Station_BC_Tmax_L), col="red",lwd=0.3) + 
    #scale_y_continuous(limits = c(-30,45)) + 
    expand_limits(y = c(min(StationQMT_Obs_Sim$Tmax_Obs), max(StationQMT_Obs_Sim$Tmax_Obs))) +
    facet_wrap(~ format(Date, "%Y"), scales = "free_x")
  
  StationQMT_Plot_Tmax <- StationQMT_Plot_Tmax + scale_x_date(date_labels = "%b")
  
  ggsave(plot = StationQMT_Plot_Tmax, filename = paste0("D:/Rdata/QMT/Plots_QMT/", StationQMT_Names[i],"_Plot_TmaxQMT.jpeg"), height = 13, width = 24, units = "cm" )
  
  # Plotting via color code methods:
  # ggplot(data = StationQMT_Obs_Sim, aes(x = StationQMT_Obs_Sim$Tmin_Obs)) +
  #   geom_point(aes(y = StationQMT_Obs_Sim$Tmin_Sim, color = "Modeled"), size = 2) +
  #   geom_point(aes(y = StationQMT_Obs_Sim$Station_BC_Tmin_L, color = "Bias Corrected"), size = 2, shape = 1) +
  #   geom_point(aes(y = StationQMT_Obs_Sim$Tmin_Obs, color = "Observed"), size = 2, shape = 3) +
  #   scale_color_manual(values = c("blue", "red", "black"), labels = c("Modeled", "Bias Corrected", "Observed")) +
  #   labs(x = "Observed Temperature", y = "Temperature") +
  #   theme_bw()
  
  # create the heatmap
  Date <- StationQMT_Obs_Sim$Date
  Year <- StationQMT_Obs_Sim$Year
  Month <- StationQMT_Obs_Sim$Month
  Tmin_Obs <-  StationQMT_Obs_Sim$Tmin_Obs
  Tmin_Sim <-  StationQMT_Obs_Sim$Tmin_Sim
  Tmin_BC <- StationQMT_Obs_Sim$Station_BC_Tmin_L
  df_Tmin <- data.frame(Date, Year, Month, Tmin_Obs, Tmin_BC, Tmin_Sim)
  
  # convert data to long format
  ggQMT_Tmin <- melt(df_Tmin, id.vars = c("Date", "Year", "Month"))

  ggQMT_Tmin <- ggplot(ggQMT_Tmin, aes(x = Month, y = variable, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white",  high = "red")+
    ggtitle("Tmin")+
    xlab(NULL)+
    ylab(NULL)
    # geom_rect(aes(xmin = min(Month) - 0.5, xmax = max(Month) + 0.5, ymin = 0.5, ymax = 0.5), 
    #           fill = NA, color = "White") +
    # geom_rect(aes(xmin = min(Month) - 0.5, xmax = max(Month) + 0.5, ymin = 1.5, ymax = 1.5), 
    #           fill = NA, color = "White") +
    # geom_rect(aes(xmin = min(Month) - 0.5, xmax = max(Month) + 0.5, ymin = 2.5, ymax = 2.5), 
   #           fill = NA, color = "white")    
    #facet_wrap(~ Year)

  ggQMT_Tmin <- ggQMT_Tmin + theme(plot.title = element_text(hjust = 0.5))
  
  #Tmax
  Tmax_Obs <-  StationQMT_Obs_Sim$Tmax_Obs
  Tmax_Sim <-  StationQMT_Obs_Sim$Tmax_Sim
  Tmax_BC <- StationQMT_Obs_Sim$Station_BC_Tmax_L
  df_Tmax <- data.frame(Date, Year, Month, Tmax_Obs, Tmax_BC, Tmax_Sim)
  
  # convert data to long format
  ggQMT_Tmax <- melt(df_Tmax, id.vars = c("Date", "Year", "Month"))

  ggQMT_Tmax <- ggplot(ggQMT_Tmax, aes(x = Month, y = variable, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white",  high = "red")+
    ggtitle("Tmax")+
    xlab(NULL)+
    ylab(NULL)
  
  ggQMT_Tmax <- ggQMT_Tmax + theme(plot.title = element_text(hjust = 0.5))
  
    #coord_fixed(ratio = 150)
  
  require(gridExtra)
  ggQMT_Both <- grid.arrange(ggQMT_Tmin, ggQMT_Tmax, ncol = 2, left = "Observed, Simulated and Bias-Corrected Temperature (°C)", 
                             bottom = "Month-Based Temperature Time Series")
  

  ggsave(plot = ggQMT_Both, filename = paste0("D:/Rdata/QMT/Plots_QMT/", StationQMT_Names[i],"_ggQMT.jpeg"), height = 13, width = 24, units = "cm" )
  
  
  # Doing bias correction of historic data
  
  #Tmin
  QMTapFittingTmin <- qmap::fitQmapQUANT(obs = Obs_Tmin, mod = Sim_Tim, wet.day = FALSE, qstep=0.01,
                                        nboot = 1)
  QMTapTminL_Hist <- qmap::doQmapQUANT(x = Hist_Tmin, fobj = QMTapFittingTmin, type = "tricub")
  
  QMT_BC_Hist_TminL <- data.frame(QMTapTminL_Hist)
  
  colnames(QMT_BC_Hist_TminL) <-  c("QMT_BC_Hist_Tmin_L")
  
  #Tmax
  QMTapFittingTmax <- qmap::fitQmapQUANT(obs = Obs_Tmax, mod = Sim_Tmax, wet.day = FALSE, qstep=0.01,
                                        nboot = 1)
  QMTapTmaxL_Hist <- qmap::doQmapQUANT(x = Hist_Tmax, fobj = QMTapFittingTmax, type = "tricub")
  
  QMT_BC_Hist_TmaxL <- data.frame(QMTapTmaxL_Hist)
  
  colnames(QMT_BC_Hist_TmaxL) <-  c("QMT_BC_Hist_Tmax_L")
  
  #Bias corrected historic
  StationQMT_BC_Hist <- cbind(StationQMT_Sim_His, QMT_BC_Hist_TminL, QMT_BC_Hist_TmaxL)
  
  #Merging the two temperature series (Observed with bias corrected)
  
  #1 Cleaning recent (Obs)
  #Choosing the wanted columns
  RecentQMT_Obs <- StationQMT_Obs_Sim[,c("Date", "Tmin_Obs", "Tmax_Obs")]
  
  #Renaming the column names
  colnames(RecentQMT_Obs)[2] <- "Tmin"
  colnames(RecentQMT_Obs)[3] <- "Tmax"
  
  #2 Cleaning historic (BC)
  #Choosing the wanted columns
  HistoricQMT_BC <- StationQMT_BC_Hist[,c("Date", "QMT_BC_Hist_Tmin_L", "QMT_BC_Hist_Tmax_L")]
  
  #Renaming the column names
  colnames(HistoricQMT_BC)[2] <- "Tmin"
  colnames(HistoricQMT_BC)[3] <- "Tmax"
  
  RecentQMT_Obs$Date <- as.character(RecentQMT_Obs$Date)
  
  QMT_Historic_BC <- rbind(HistoricQMT_BC, RecentQMT_Obs)
  
  #Weather_AT <- QMT_Historic_BC %>% 
  #arrange(Date)
  
  write.csv(QMT_Historic_BC, paste0("D:/Rdata/QMT/Stations_Obs_Sim_QMT/Hist_", StationQMT_Names[i], "_BC_QMT.csv"))
  
  setTxtProgressBar(pb,i)
  
}

#------------------------------------------------------------------------------------------#
# 02: Combining all the average statistics of 51 stations into a single table
#------------------------------------------------------------------------------------------#

#Loading the stations data 
All_Statistics_QMT <-  chillR::load_temperature_scenarios("D:/Rdata/QMT/Stations_Obs_Sim_QMT", prefix = "Calib_")

#Since the files names are not there, we used list.file to show the names
#filenamesQMT <- list.files("D:/Rdata/QMT/Stations_Obs_Sim_QMT", pattern = "Calib_", all.files = FALSE)

#Station_Names <- str_split(filenamesQMT, pattern = "_") %>% 
# purrr::map_chr(2)

names(All_Statistics_QMT) <- StationQMT_Names

#library(dplyr)

# merge all the data frames in the list into one data frame
All_Statistics_QMT <- bind_rows(All_Statistics_QMT)

# create a new column with the station names
All_Statistics_QMT$Station_Name <- StationQMT_Names

# rearrange the columns so that the new column is at the start
All_Statistics_QMT <- All_Statistics_QMT[, c("Station_Name", colnames(All_Statistics_QMT))]

# remove the ".csv" extension from the station names
#All_Statistics$Station_Name <- gsub(".csv", "", All_Statistics$Station_Name)

# Remove the last columns 
All_Statistics_QMT <- All_Statistics_QMT[-c(16)]

kable(All_Statistics_QMT, digits = 3, caption = "Mean statistics using Quantile Mapping method") %>%
  kable_styling("striped", position = "left", font_size = 10)

write.csv(All_Statistics_QMT, "D:/Rdata/QMT/Stations_Obs_Sim_QMT/All_Statistics_QMT.csv")