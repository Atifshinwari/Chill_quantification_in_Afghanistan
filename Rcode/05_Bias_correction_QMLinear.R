#Loading the stations data 
devtools::install_github(c("SantanderMetGroup/transformeR", "SantanderMetGroup/downscaleR"))
require(downscaleR)
require(kableExtra)
require(zoo)
require(latticeExtra)
require(qmap)
require(chillR)
require(ggplot2)
require(dplyr)
require(stringr)

#------------------------------------------------------------------------------------------#
# 01: Application of QDM for one station: Adraskan
#------------------------------------------------------------------------------------------#

Adraskan_Obs <- read.csv("D:/Rdata/QM/Stations_Obs_QM/Adraskan_AT_Obs.csv")
Adraskan_Sim <- read.csv("D:/Rdata/QM/Stations_Sim_QM/Adraskan_Simulated.csv")
Adraskan_Sim_His <- read.csv("D:/Rdata/QM/Stations_Sim_QM/Adraskan_Simulated.csv")

colnames(Adraskan_Sim) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
Adraskan_Sim$Tmin_Sim <- as.numeric(Adraskan_Sim$Tmin_Sim)
Adraskan_Sim$Tmax_Sim <- as.numeric(Adraskan_Sim$Tmax_Sim)

colnames(Adraskan_Obs)[6] <- "Tmin_Obs"
colnames(Adraskan_Obs)[7] <- "Tmax_Obs"
Adraskan_Obs$Tmin_Obs <- as.numeric(Adraskan_Obs$Tmin_Obs)
Adraskan_Obs$Tmax_Obs <- as.numeric(Adraskan_Obs$Tmax_Obs)

candidate1 <- lubridate::ymd(Adraskan_Obs$Date[1])
candidate2 <- lubridate::dmy(Adraskan_Obs$Date[1])
#candidate3 <- lubridate::dmy_hms(Weather_Obs$Date[1])

#take the candidate which is not an NA
if(is.na(candidate1) == FALSE & is.na(candidate2) == TRUE){
  start_date <- candidate1
  end_date <- lubridate::ymd(Adraskan_Obs$Date[nrow(Adraskan_Obs)])
  Adraskan_Obs$Date <- lubridate::ymd(Adraskan_Obs$Date)
  
  
} else if(is.na(candidate1) == TRUE & is.na(candidate2) == FALSE){
  start_date <- candidate2
  end_date <- lubridate::dmy(Adraskan_Obs$Date[nrow(Adraskan_Obs)])
  Adraskan_Obs$Date <- lubridate::dmy(Adraskan_Obs$Date)
}


Adraskan_Sim <- Adraskan_Sim[lubridate::ymd(Adraskan_Sim$Date) >=  start_date & lubridate::ymd(Adraskan_Sim$Date) <=  end_date,]

# I will make sure that weather_sim$Date is in a date format
# otherwise merging does not work
Adraskan_Sim$Date <- lubridate::ymd(Adraskan_Sim$Date)

#start_date <- as.Date(Adraskan_Obs$Date[1])
#end_date <- as.Date(Adraskan_Obs$Date[nrow(Adraskan_Obs)])

#Adraskan_Sim <- Adraskan_Sim[as.Date(Adraskan_Sim$Date) >=  start_date & as.Date(Adraskan_Sim$Date) <=  end_date,]

# Adraskan_Obs <- Adraskan_Obs[c("Tmin_Obs", "Tmax_Obs")]
# Adraskan_Sim <- Adraskan_Sim[c("Tmin_Sim", "Tmax_Sim")]

Adraskan_Obs_Sim <- merge(Adraskan_Obs, Adraskan_Sim, by = c("Date"))
Adraskan_Obs_Sim <- Adraskan_Obs_Sim[c("Date", "Year", "Month", "Day", "Tmin_Obs", 
                                       "Tmax_Obs", "Tmin_Sim", "Tmax_Sim")]

# Historic data
colnames(Adraskan_Sim_His) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
Adraskan_Sim_His$Tmin_Sim <- as.numeric(Adraskan_Sim_His$Tmin_Sim)
Adraskan_Sim_His$Tmax_Sim <- as.numeric(Adraskan_Sim_His$Tmax_Sim)

start_date2 <- as.Date("1980-01-01")
Adraskan_Sim_His <- Adraskan_Sim_His[as.Date(Adraskan_Sim_His$Date) >=  as.Date(start_date2) & as.Date(Adraskan_Sim_His$Date) <  start_date,]

# install.packages("qmaptools", repos = "http://cran.us.r-project.org")

# Define the observed and modeled data
Obs_Tmin <- Adraskan_Obs$Tmin_Obs
Sim_Tim <- Adraskan_Sim$Tmin_Sim
Hist_Tmin <- Adraskan_Sim_His$Tmin_Sim

#Tmin
QmapFittingTmin <- qmap::fitQmapQUANT(obs = Obs_Tmin, mod = Sim_Tim, wet.day = FALSE, qstep=0.01,
                   nboot = 1)
QmapTminL <- qmap::doQmapQUANT(x = Sim_Tim, fobj = QmapFittingTmin, type = "linear")
#QmapTminT <- qmap::doQmapQUANT(x = Sim_Tim, fobj = QmapFittingTmin, type = "tricub")

Station_BC_TminL <- data.frame(QmapTminL)
#Station_BC_TminT <- data.frame(QmapTminT)
colnames(Station_BC_TminL) <-  c("Station_BC_Tmin_L")
#colnames(Station_BC_TminT) <-  c("Station_BC_Tmin_T")


#Tmax
Obs_Tmax <- Adraskan_Obs$Tmax_Obs
Sim_Tmax <- Adraskan_Sim$Tmax_Sim
Hist_Tmax <- Adraskan_Sim_His$Tmax_Sim

QmapFitting <- qmap::fitQmapQUANT(obs = Obs_Tmax, mod = Sim_Tmax, wet.day = FALSE, qstep=0.01,
                                  nboot = 1)
QmapTmaxL <- qmap::doQmapQUANT(x = Sim_Tmax, fobj = QmapFitting, type = "linear")

Station_BC_TmaxL <- data.frame(QmapTmaxL)
#Station_BC_TmaxT <- data.frame(QmapTmaxT)
colnames(Station_BC_TmaxL) <-  c("Station_BC_Tmax_L")
#colnames(Station_BC_TmaxT) <-  c("Station_BC_Tmax_T")
Adraskan_Obs <- cbind(Adraskan_Obs, Adraskan_Sim, Station_BC_TminL, Station_BC_TmaxL)
Adraskan_Obs <- Adraskan_Obs[c("Date", "Year", "Month", "Day", "Tmin_Obs", "Tmax_Obs", "Station_BC_Tmin_L", "Station_BC_Tmax_L",  "Tmin_Sim", "Tmax_Sim")]

# For Tmin
Adraskan_Stats_Tmin <- Adraskan_Obs %>% 
  summarise(Mean_Bias_Tmin = mean(Tmin_Obs-Tmin_Sim),
            #Mean_Bias_TminBC = mean(Tmin_Obs-QDM_Tmin),
            SD_Tmin=sd(Tmin_Obs-Tmin_Sim),
            SD_TminBC=sd(Tmin_Obs-Station_BC_Tmin_L),
            RMSEP_Tmin=RMSEP(Tmin_Sim,Tmin_Obs),
            RMSEP_TminBC=RMSEP(Station_BC_Tmin_L, Tmin_Obs),
            RPIQ_Tmin=RPIQ(Tmin_Sim,Tmin_Obs),
            RPIQ_TminBC=RPIQ(Station_BC_Tmin_L, Tmin_Obs))

#For Tmax
Adraskan_Stats_Tmax <- Adraskan_Obs %>% 
  summarise(Mean_Bias_Tmax = mean(Tmax_Obs-Tmax_Sim),
            #Mean_Bias_TmaxBC = mean(Tmax_Obs-QDM_Tmax),
            SD_Tmax=sd(Tmax_Obs-Tmax_Sim),
            SD_TmaxBC=sd(Tmax_Obs-Station_BC_Tmax_L),
            RMSEP_Tmax=RMSEP(Tmax_Sim,Tmax_Obs),
            RMSEP_TmaxBC=RMSEP(Station_BC_Tmax_L, Tmax_Obs),
            RPIQ_Tmax=RPIQ(Tmax_Sim,Tmax_Obs),
            RPIQ_TmaxBC=RPIQ(Station_BC_Tmax_L, Tmax_Obs))

Adraskan_average_statistics <- cbind(Adraskan_Stats_Tmin, Adraskan_Stats_Tmax)


# Plot temperature columns using ggplot

  Adraskan_Plot_Tmin <- ggplot(Adraskan_Obs, aes(x = Date, y = Adraskan_Obs$Tmin_Obs, group = 1)) + 
  geom_line(lwd=0.3) + 
  xlab("Time series of temperature data") + 
  ylab("Daily minimum temperature (°C)") + 
  #scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*700)]) +
  geom_line(data = Adraskan_Obs, aes(x = Date, y = Adraskan_Obs$Tmin_Sim), col="blue",lwd=0.3) +
  geom_line(data = Adraskan_Obs, aes(x = Date, y = Adraskan_Obs$Station_BC_Tmin_L), col="red",lwd=0.3) + 
    #scale_y_continuous(limits = c(-30,45)) + 
    expand_limits(y = c(min(Adraskan_Obs$Tmin_Obs), max(Adraskan_Obs$Tmin_Obs))) +
  facet_wrap(~ format(Date, "%Y"), scales = "free_x")
  
  Adraskan_Plot_Tmin + scale_x_date(date_labels = "%b")

ggsave("D:/Rdata/QM/Plots_QM/Adraskan_Plot_Tmin.png",width = 24,height = 13, units = "cm",dpi = 600)
  
  Adraskan_Plot_Tmax <- ggplot(Adraskan_Obs, aes(x = Date, y = Adraskan_Obs$Tmax_Obs, group = 1)) + 
    geom_line(lwd=0.3) + 
    xlab("Time series of temperature data") + 
    ylab("Daily maximum temperature (°C)") + 
    #scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*700)]) +
    geom_line(data = Adraskan_Obs, aes(x = Date, y = Adraskan_Obs$Tmax_Sim), col="blue",lwd=0.3) +
    geom_line(data = Adraskan_Obs, aes(x = Date, y = Adraskan_Obs$Station_BC_Tmax_L), col="red",lwd=0.3) + 
    #scale_y_continuous(limits = c(-30,45)) + 
    expand_limits(y = c(min(Adraskan_Obs$Tmax_Obs), max(Adraskan_Obs$Tmax_Obs))) +
    facet_wrap(~ format(Date, "%Y"), scales = "free_x")
  
  Adraskan_Plot_Tmax + scale_x_date(date_labels = "%b")
  
ggsave("D:/Rdata/QM/Plots_QM/Adraskan_Plot_Tmax.png",width = 24,height = 13, units = "cm",dpi = 600)
  
# Doing bias correction of historic data

#Tmin
AdraskanFittingTmin <- qmap::fitQmapQUANT(obs = Obs_Tmin, mod = Sim_Tim, wet.day = FALSE, qstep=0.01,
                                      nboot = 1)
Adraskan_Hist_TminL <- qmap::doQmapQUANT(x = Hist_Tmin, fobj = AdraskanFittingTmin, type = "linear")

Adraskan_Hist_TminL <- data.frame(Adraskan_Hist_TminL)

colnames(Adraskan_Hist_TminL) <-  c("Adraskan_Hist_Tmin_L")

#Tmax
AdraskanFittingTmax <- qmap::fitQmapQUANT(obs = Obs_Tmax, mod = Sim_Tmax, wet.day = FALSE, qstep=0.01,
                                      nboot = 1)
Adraskan_Hist_TmaxL <- qmap::doQmapQUANT(x = Hist_Tmax, fobj = AdraskanFittingTmax, type = "linear")

Adraskan_Hist_TmaxL <- data.frame(Adraskan_Hist_TmaxL)

colnames(Adraskan_Hist_TmaxL) <-  c("Adraskan_Hist_Tmax_L")

#Bias corrected historic
AdraskanQM_BC_Hist <- cbind(Adraskan_Sim_His, Adraskan_Hist_TminL, Adraskan_Hist_TmaxL)

#Merging the two temperature series (Observed with bias corrected)

#1 Cleaning recent (Obs)
#Choosing the wanted columns
Adraskan_RecentQM_Obs <- Adraskan_Obs[,c("Date", "Tmin_Obs", "Tmax_Obs")]

#Renaming the column names
colnames(Adraskan_RecentQM_Obs)[2] <- "Tmin"
colnames(Adraskan_RecentQM_Obs)[3] <- "Tmax"

#2 Cleaning historic (BC)
#Choosing the wanted columns
Adraskan_HistoricQM_BC <- AdraskanQM_BC_Hist[,c("Date", "Adraskan_Hist_Tmin_L", "Adraskan_Hist_Tmax_L")]

#Renaming the column names
colnames(Adraskan_HistoricQM_BC)[2] <- "Tmin"
colnames(Adraskan_HistoricQM_BC)[3] <- "Tmax"

Adraskan_RecentQM_Obs$Date <- as.character(Adraskan_RecentQM_Obs$Date)

Adraskan_Historic_BC <- rbind(Adraskan_HistoricQM_BC, Adraskan_RecentQM_Obs)

write.csv(Adraskan_Historic_BC, "D:/Rdata/QM/Stations_Obs_Sim_QM/Adraskan_Historic_BC.csv")


#------------------------------------------------------------------------------------------#
# 02: Looping the QDM for all remaining stations
#------------------------------------------------------------------------------------------#

#Loading the stations data
StationQM_Obs_List <-  chillR::load_temperature_scenarios("D:/Rdata/QM/Stations_Obs_QM", prefix = "")
StationQM_Sim_List <- chillR::load_temperature_scenarios("D:/Rdata/QM/Stations_Sim_QM", prefix = "")
# Again loading simulated because I will need it for later on use
StationQM_Sim_His_List <- chillR::load_temperature_scenarios("D:/Rdata/QM/Stations_Sim_QM", prefix = "")

filenamesQM <- list.files("D:/Rdata/QM/Stations_Obs_QM")
#The names are split by '_', however there are still three characters now we assign it to 
#each name using purrr function which works as a loop.
StationQM_Names <- str_split(filenamesQM, pattern = "_") %>% 
  purrr::map_chr(1)

#assign the list of names to the data set
names(StationQM_Obs_List) <- StationQM_Names
names(StationQM_Sim_List) <- StationQM_Names
names(StationQM_Sim_His_List) <- StationQM_Names

StationQM_Statistics_List <- list()

pb = txtProgressBar(min = 0, max = length(StationQM_Obs_List), initial = 0, style = 3) 

for(i in 1:length(StationQM_Obs_List)){
  
  StationQM_Obs <- StationQM_Obs_List[[i]]
  StationQM_Sim <- StationQM_Sim_List[[i]]
  StationQM_Sim_His <- StationQM_Sim_His_List[[i]]
  
  StationQM_Obs <- as.data.frame(StationQM_Obs)
  StationQM_Sim <- as.data.frame(StationQM_Sim)
  StationQM_Sim_His <- as.data.frame(StationQM_Sim_His)
  
  colnames(StationQM_Sim) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
  StationQM_Sim$Tmin_Sim <- as.numeric(StationQM_Sim$Tmin_Sim)
  StationQM_Sim$Tmax_Sim <- as.numeric(StationQM_Sim$Tmax_Sim)
  
  colnames(StationQM_Obs)[6] <- "Tmin_Obs"
  colnames(StationQM_Obs)[7] <- "Tmax_Obs"
  StationQM_Obs$Tmin_Obs <- as.numeric(StationQM_Obs$Tmin_Obs)
  StationQM_Obs$Tmax_Obs <- as.numeric(StationQM_Obs$Tmax_Obs)
  
  # test which date format we have
  
  candidate1 <- lubridate::ymd(StationQM_Obs$Date[1])
  candidate2 <- lubridate::dmy(StationQM_Obs$Date[1])
  #candidate3 <- lubridate::dmy_hms(Weather_Obs$Date[1])
  
  # take the candidate which is not an NA
  if(is.na(candidate1) == FALSE & is.na(candidate2) == TRUE){
    start_date <- candidate1
    end_date <- lubridate::ymd(StationQM_Obs$Date[nrow(StationQM_Obs)])
    StationQM_Obs$Date <- lubridate::ymd(StationQM_Obs$Date)
    
    
  } else if(is.na(candidate1) == TRUE & is.na(candidate2) == FALSE){
    start_date <- candidate2
    end_date <- lubridate::dmy(StationQM_Obs$Date[nrow(StationQM_Obs)])
    StationQM_Obs$Date <- lubridate::dmy(StationQM_Obs$Date)
  }
  
  
  StationQM_Sim <- StationQM_Sim[lubridate::ymd(StationQM_Sim$Date) >=  start_date & lubridate::ymd(StationQM_Sim$Date) <=  end_date,]
  
  # I will make sure that weather_sim$Date is in a date format
  #otherwise merging does not work
  StationQM_Sim$Date <- lubridate::ymd(StationQM_Sim$Date)
  
  # Historic
  colnames(StationQM_Sim_His) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
  StationQM_Sim_His$Tmin_Sim <- as.numeric(StationQM_Sim_His$Tmin_Sim)
  StationQM_Sim_His$Tmax_Sim <- as.numeric(StationQM_Sim_His$Tmax_Sim)
  
  start_date2 <- as.Date("1980-01-01")
  StationQM_Sim_His <- StationQM_Sim_His[as.Date(StationQM_Sim_His$Date) >=  as.Date(start_date2) & as.Date(StationQM_Sim_His$Date) <  start_date,]
  

  # Define the observed and modeled data for quantile mapping
  Obs_Tmin <- StationQM_Obs$Tmin_Obs
  Sim_Tim <- StationQM_Sim$Tmin_Sim
  Hist_Tmin <- StationQM_Sim_His$Tmin_Sim
  
# Tmin QM for calibration period
  QmapFittingTmin <- qmap::fitQmapQUANT(obs = Obs_Tmin, mod = Sim_Tim, wet.day = FALSE, qstep=0.01,
                                        nboot = 1)
  QmapTminL <- qmap::doQmapQUANT(x = Sim_Tim, fobj = QmapFittingTmin, type = "linear")

  Station_BC_TminL <- data.frame(QmapTminL)
  colnames(Station_BC_TminL) <-  c("Station_BC_Tmin_L")

# Tmax QM for calibration period
  Obs_Tmax <- StationQM_Obs$Tmax_Obs
  Sim_Tmax <- StationQM_Sim$Tmax_Sim
  Hist_Tmax <- StationQM_Sim_His$Tmax_Sim
  
  QmapFitting <- qmap::fitQmapQUANT(obs = Obs_Tmax, mod = Sim_Tmax, wet.day = FALSE, qstep=0.01,
                                    nboot = 1)
  QmapTmaxL <- qmap::doQmapQUANT(x = Sim_Tmax, fobj = QmapFitting, type = "linear")

  Station_BC_TmaxL <- data.frame(QmapTmaxL)
  colnames(Station_BC_TmaxL) <-  c("Station_BC_Tmax_L")
  
  StationQM_Obs_Sim <- cbind(StationQM_Obs, StationQM_Sim, Station_BC_TminL, Station_BC_TmaxL)
  StationQM_Obs_Sim <- StationQM_Obs_Sim[c("Date", "Year", "Month", "Day", "Tmin_Obs", "Tmax_Obs", "Tmin_Sim", "Tmax_Sim", "Station_BC_Tmin_L", "Station_BC_Tmax_L")]
  
  # Statistics
  # For Tmin
  StationQM_Stats_Tmin <- StationQM_Obs_Sim %>% 
    summarise(Mean_Bias_Tmin = mean(Tmin_Obs-Tmin_Sim),
              #Mean_Bias_TminBC = mean(Tmin_Obs-QDM_Tmin),
              SD_Tmin=sd(Tmin_Obs-Tmin_Sim),
              SD_TminBC=sd(Tmin_Obs-Station_BC_Tmin_L),
              RMSEP_Tmin=RMSEP(Tmin_Sim,Tmin_Obs),
              RMSEP_TminBC=RMSEP(Station_BC_Tmin_L, Tmin_Obs),
              RPIQ_Tmin=RPIQ(Tmin_Sim,Tmin_Obs),
              RPIQ_TminBC=RPIQ(Station_BC_Tmin_L, Tmin_Obs))
  
  # For Tmax
  StationQM_Stats_Tmax <- StationQM_Obs_Sim %>% 
    summarise(Mean_Bias_Tmax = mean(Tmax_Obs-Tmax_Sim),
              #Mean_Bias_TmaxBC = mean(Tmax_Obs-QDM_Tmax),
              SD_Tmax=sd(Tmax_Obs-Tmax_Sim),
              SD_TmaxBC=sd(Tmax_Obs-Station_BC_Tmax_L),
              RMSEP_Tmax=RMSEP(Tmax_Sim,Tmax_Obs),
              RMSEP_TmaxBC=RMSEP(Station_BC_Tmax_L, Tmax_Obs),
              RPIQ_Tmax=RPIQ(Tmax_Sim,Tmax_Obs),
              RPIQ_TmaxBC=RPIQ(Station_BC_Tmax_L, Tmax_Obs))
  
  StationQM_average_statistics <- cbind(StationQM_Stats_Tmin, StationQM_Stats_Tmax)
  
  StationQM_Statistics_List[[i]] <- StationQM_average_statistics
  
  write.csv(StationQM_average_statistics, file = paste0("D:/Rdata/QM/Stations_Obs_Sim_QM/Calib_", StationQM_Names[i], "_Statistics.csv"), row.names = FALSE)
  
  # Saving the bias corrected calibration period
  write.csv(StationQM_Obs_Sim, paste0("D:/Rdata/QM/Stations_Obs_Sim_QM/Recent_", StationQM_Names[i], "QML_BC.csv"))
  
  
  # Plotting
  # Plotting Calibration Period
  StationQM_Plot_Tmin <- ggplot(StationQM_Obs_Sim, aes(x = Date, y = StationQM_Obs_Sim$Tmin_Obs, group = 1)) + 
    geom_line(lwd=0.3) + 
    xlab("Time series of temperature data") + 
    ylab("Daily minimum temperature (°C)") + 
    #scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*700)]) +
    geom_line(data = StationQM_Obs_Sim, aes(x = Date, y = StationQM_Obs_Sim$Tmin_Sim), col="blue",lwd=0.3) +
    geom_line(data = StationQM_Obs_Sim, aes(x = Date, y = StationQM_Obs_Sim$Station_BC_Tmin_L), col="red",lwd=0.3) + 
    #scale_y_continuous(limits = c(-30,45)) + 
    expand_limits(y = c(min(StationQM_Obs_Sim$Tmin_Obs), max(StationQM_Obs_Sim$Tmin_Obs))) +
    facet_wrap(~ format(Date, "%Y"), scales = "free_x")
  
  StationQM_Plot_Tmin <- StationQM_Plot_Tmin + scale_x_date(date_labels = "%b")
  
  ggsave(plot = StationQM_Plot_Tmin, filename = paste0("D:/Rdata/QM/Plots_QM/", StationQM_Names[i],"_Plot_Tmin.jpeg"), height = 13, width = 24, units = "cm" )
  

  StationQM_Plot_Tmax <- ggplot(StationQM_Obs_Sim, aes(x = Date, y = StationQM_Obs_Sim$Tmax_Obs, group = 1)) + 
    geom_line(lwd=0.3) + 
    xlab("Time series of temperature data") + 
    ylab("Daily maximum temperature (°C)") + 
    #scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*700)]) +
    geom_line(data = StationQM_Obs_Sim, aes(x = Date, y = StationQM_Obs_Sim$Tmax_Sim), col="blue",lwd=0.3) +
    geom_line(data = StationQM_Obs_Sim, aes(x = Date, y = StationQM_Obs_Sim$Station_BC_Tmax_L), col="red",lwd=0.3) + 
    #scale_y_continuous(limits = c(-30,45)) + 
    expand_limits(y = c(min(StationQM_Obs_Sim$Tmax_Obs), max(StationQM_Obs_Sim$Tmax_Obs))) +
    facet_wrap(~ format(Date, "%Y"), scales = "free_x")
  
  StationQM_Plot_Tmax <- StationQM_Plot_Tmax + scale_x_date(date_labels = "%b")
  
  ggsave(plot = StationQM_Plot_Tmax, filename = paste0("D:/Rdata/QM/Plots_QM/", StationQM_Names[i],"_Plot_Tmax.jpeg"), height = 13, width = 24, units = "cm" )
  
  # create the heatmap
  Date <- StationQM_Obs_Sim$Date
  Year <- StationQM_Obs_Sim$Year
  Month <- StationQM_Obs_Sim$Month
  Tmin_Obs <-  StationQM_Obs_Sim$Tmin_Obs
  Tmin_Sim <-  StationQM_Obs_Sim$Tmin_Sim
  Tmin_BC <- StationQM_Obs_Sim$Station_BC_Tmin_L
  df_TminQML <- data.frame(Date, Year, Month, Tmin_Obs, Tmin_BC, Tmin_Sim)
  
  # convert data to long format
  ggQML_Tmin <- melt(df_TminQML, id.vars = c("Date", "Year", "Month"))
  
  ggQML_Tmin <- ggplot(ggQML_Tmin, aes(x = Month, y = variable, fill = value)) +
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
  
  ggQML_Tmin <- ggQML_Tmin + theme(plot.title = element_text(hjust = 0.5))
  
  #Tmax
  Tmax_Obs <-  StationQM_Obs_Sim$Tmax_Obs
  Tmax_Sim <-  StationQM_Obs_Sim$Tmax_Sim
  Tmax_BC <- StationQM_Obs_Sim$Station_BC_Tmax_L
  df_TmaxQML <- data.frame(Date, Year, Month, Tmax_Obs, Tmax_BC, Tmax_Sim)
  
  # convert data to long format
  ggQML_Tmax <- melt(df_TmaxQML, id.vars = c("Date", "Year", "Month"))
  
  ggQML_Tmax <- ggplot(ggQML_Tmax, aes(x = Month, y = variable, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white",  high = "red")+
    ggtitle("Tmax")+
    xlab(NULL)+
    ylab(NULL)
  
  ggQML_Tmax <- ggQML_Tmax + theme(plot.title = element_text(hjust = 0.5))
  
  #coord_fixed(ratio = 150)
  
  require(gridExtra)
  ggQML_Both <- grid.arrange(ggQML_Tmin, ggQML_Tmax, ncol = 2, left = "Observed, Simulated and Bias-Corrected Temperature (°C)", 
                             bottom = "Month-Based Temperature Time Series")
  
  
  ggsave(plot = ggQML_Both, filename = paste0("D:/Rdata/QM/Plots_QM/", StationQM_Names[i],"_ggQML.jpeg"), height = 13, width = 24, units = "cm" )
  
  # Doing bias correction of historic data
  
  #Tmin
  QmapFittingTmin <- qmap::fitQmapQUANT(obs = Obs_Tmin, mod = Sim_Tim, wet.day = FALSE, qstep=0.01,
                                        nboot = 1)
  QmapTminL_Hist <- qmap::doQmapQUANT(x = Hist_Tmin, fobj = QmapFittingTmin, type = "linear")

  QM_BC_Hist_TminL <- data.frame(QmapTminL_Hist)

  colnames(QM_BC_Hist_TminL) <-  c("QM_BC_Hist_Tmin_L")

  #Tmax
  QmapFittingTmax <- qmap::fitQmapQUANT(obs = Obs_Tmax, mod = Sim_Tmax, wet.day = FALSE, qstep=0.01,
                                    nboot = 1)
  QmapTmaxL_Hist <- qmap::doQmapQUANT(x = Hist_Tmax, fobj = QmapFittingTmax, type = "linear")

  QM_BC_Hist_TmaxL <- data.frame(QmapTmaxL_Hist)
  
  colnames(QM_BC_Hist_TmaxL) <-  c("QM_BC_Hist_Tmax_L")

  #Bias corrected historic
  StationQM_BC_Hist <- cbind(StationQM_Sim_His, QM_BC_Hist_TminL, QM_BC_Hist_TmaxL)

  #Merging the two temperature series (Observed with bias corrected)
  
  #1 Cleaning recent (Obs)
  #Choosing the wanted columns
  RecentQM_Obs <- StationQM_Obs_Sim[,c("Date", "Tmin_Obs", "Tmax_Obs")]
  
  #Renaming the column names
  colnames(RecentQM_Obs)[2] <- "Tmin"
  colnames(RecentQM_Obs)[3] <- "Tmax"
  
  #2 Cleaning historic (BC)
  #Choosing the wanted columns
  HistoricQM_BC <- StationQM_BC_Hist[,c("Date", "QM_BC_Hist_Tmin_L", "QM_BC_Hist_Tmax_L")]
  
  #Renaming the column names
  colnames(HistoricQM_BC)[2] <- "Tmin"
  colnames(HistoricQM_BC)[3] <- "Tmax"
 
  RecentQM_Obs$Date <- as.character(RecentQM_Obs$Date)

  QM_Historic_BC <- rbind(HistoricQM_BC, RecentQM_Obs)
  
  #Weather_AT <- QM_Historic_BC %>% 
    #arrange(Date)
  
  write.csv(QM_Historic_BC, paste0("D:/Rdata/QM/Stations_Obs_Sim_QM/Hist_", StationQM_Names[i], "_BC.csv"))

  setTxtProgressBar(pb,i)
  
}



#------------------------------------------------------------------------------------------#
# 03: Combining all the average statistics of 51 stations into a single table
#------------------------------------------------------------------------------------------#

#Loading the stations data 
All_Statistics_QM <-  chillR::load_temperature_scenarios("D:/Rdata/QM/Stations_Obs_Sim_QM", prefix = "Calib_")

#Since the files names are not there, we used list.file to show the names
#filenamesQM <- list.files("D:/Rdata/QM/Stations_Obs_Sim_QM", pattern = "Calib_", all.files = FALSE)

#Station_Names <- str_split(filenamesQM, pattern = "_") %>% 
 # purrr::map_chr(2)

names(All_Statistics_QM) <- StationQM_Names

#library(dplyr)

# merge all the data frames in the list into one data frame
All_Statistics_QM <- bind_rows(All_Statistics_QM)

# create a new column with the station names
All_Statistics_QM$Station_Name <- StationQM_Names

# rearrange the columns so that the new column is at the start
All_Statistics_QM <- All_Statistics_QM[, c("Station_Name", colnames(All_Statistics_QM))]
    
# remove the ".csv" extension from the station names
#All_Statistics$Station_Name <- gsub(".csv", "", All_Statistics$Station_Name)

# Remove the last columns 
All_Statistics_QM <- All_Statistics_QM[-c(16)]

kable(All_Statistics_QM, digits = 3, caption = "Mean statistics using Quantile Mapping method") %>%
  kable_styling("striped", position = "left", font_size = 10)

write.csv(All_Statistics_QM, "D:/Rdata/QM/Stations_Obs_Sim_QM/All_Statistics_QM.csv")