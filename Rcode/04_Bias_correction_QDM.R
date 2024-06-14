#Loading the stations data 
require(ggplot2)
require(lubridate)
require(tidyverse)
require(chillR)
require(kableExtra)
require(Metrics)
require(hydroGOF)
require(MBC)

#------------------------------------------------------------------------------------------#
# 01: Application of QDM for one station: Adraskan
#------------------------------------------------------------------------------------------#

Adraskan_Obs <- read.csv("D:/Rdata/QDM/Stations_Obs/Adraskan_AT_Obs.csv")
Adraskan_Sim <- read.csv("D:/Rdata/QDM/Stations_Sim/Adraskan_Simulated.csv")

colnames(Adraskan_Sim) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
Adraskan_Sim$Tmin_Sim <- as.numeric(Adraskan_Sim$Tmin_Sim)
Adraskan_Sim$Tmax_Sim <- as.numeric(Adraskan_Sim$Tmax_Sim)

colnames(Adraskan_Obs)[6] <- "Tmin_Obs"
colnames(Adraskan_Obs)[7] <- "Tmax_Obs"
Adraskan_Obs$Tmin_Obs <- as.numeric(Adraskan_Obs$Tmin_Obs)
Adraskan_Obs$Tmax_Obs <- as.numeric(Adraskan_Obs$Tmax_Obs)

start_date <- as.Date(Adraskan_Obs$Date[1])
end_date <- as.Date(Adraskan_Obs$Date[nrow(Adraskan_Obs)])

Adraskan_Sim <- Adraskan_Sim[as.Date(Adraskan_Sim$Date) >=  start_date & as.Date(Adraskan_Sim$Date) <=  end_date,]

# Adraskan_Obs <- Adraskan_Obs[c("Tmin_Obs", "Tmax_Obs")]
# Adraskan_Sim <- Adraskan_Sim[c("Tmin_Sim", "Tmax_Sim")]

Adraskan_Obs_Sim <- merge(Adraskan_Obs, Adraskan_Sim, by = c("Date"))
Adraskan_Obs_Sim <- Adraskan_Obs_Sim[c("Date", "Year", "Month", "Day", "Tmin_Obs", 
                                     "Tmax_Obs", "Tmin_Sim", "Tmax_Sim")]
  
Adraskan_Sim_His <- read.csv("D:/Rdata/QDM/Stations_Sim/Adraskan_Simulated.csv")
colnames(Adraskan_Sim_His) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
Adraskan_Sim_His$Tmin_Sim <- as.numeric(Adraskan_Sim_His$Tmin_Sim)
Adraskan_Sim_His$Tmax_Sim <- as.numeric(Adraskan_Sim_His$Tmax_Sim)

start_date2 <- as.Date("1980-01-01")
Adraskan_Sim_His <- Adraskan_Sim_His[as.Date(Adraskan_Sim_His$Date) >=  as.Date(start_date2) & as.Date(Adraskan_Sim_His$Date) <  start_date,]

# Adraskan_Sim_His <- Adraskan_Sim_His[c("Tmin_Sim", "Tmax_Sim")]

QDM_Adraskan_Tmin <- MBC::QDM(o.c = Adraskan_Obs$Tmin_Obs, m.c = Adraskan_Sim$Tmin_Sim, m.p = Adraskan_Sim_His$Tmin_Sim, ratio=FALSE, trace=0.05, trace.calc=0.5*trace,
                              jitter.factor=0, n.tau=NULL, ratio.max=2,
                              ratio.max.trace=10*trace, ECBC=FALSE, ties='first',
                              subsample=NULL, pp.type=7)
QDM_Adraskan_Tmax <- MBC::QDM(o.c = Adraskan_Obs$Tmax_Obs, m.c = Adraskan_Sim$Tmax_Sim, m.p = Adraskan_Sim_His$Tmax_Sim, ratio=FALSE, trace=0.05, trace.calc=0.5*trace,
                              jitter.factor=0, n.tau=NULL, ratio.max=2,
                              ratio.max.trace=10*trace, ECBC=FALSE, ties='first',
                              subsample=NULL, pp.type=7)


# Combining all calibration period
Adraskan_BC_Cal <- cbind(Adraskan_Obs_Sim, QDM_Adraskan_Tmin$mhat.c)
Adraskan_BC_Cal <- cbind(Adraskan_BC_Cal, QDM_Adraskan_Tmax$mhat.c)
colnames(Adraskan_BC_Cal)[9] <- "QDM_Tmin"
colnames(Adraskan_BC_Cal)[10] <- "QDM_Tmax"

# Combining all historic period
Adraskan_BC_His <- cbind(Adraskan_Sim_His, QDM_Adraskan_Tmin$mhat.p)
Adraskan_BC_His <- cbind(Adraskan_BC_His, QDM_Adraskan_Tmax$mhat.p)
colnames(Adraskan_BC_His)[4] <- "QDM_Tmin_His"
colnames(Adraskan_BC_His)[5] <- "QDM_Tmax_His"

# Plotting

# Plotting Calibration Period
Adraskan_TminPlot_Cal <- ggplot(data = Adraskan_BC_Cal, aes(x = Tmin_Obs, y = Tmin_Sim)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TminPlot_Cal)

ggsave("D:/Rdata/QDM/QDM_Plots/Adraskan_TminPlot_Cal.png",width = 20,height = 10, units = "cm",dpi = 600)

Adraskan_TmaxPlot_Cal <- ggplot(data = Adraskan_BC_Cal, aes(x = Tmax_Obs, y = Tmax_Sim)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TmaxPlot_Cal)

ggsave("D:/Rdata/QDM/QDM_Plots/Adraskan_TmaxPlot_Cal.png",width = 20,height = 10, units = "cm",dpi = 600)

# Plotting Bias Corrected Calibration Period
Adraskan_TminPlot_CalBC <- ggplot(data = Adraskan_BC_Cal, aes(x = Tmin_Obs, y = QDM_Tmin)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TminPlot_CalBC)

ggsave("D:/Rdata/QDM/QDM_Plots/Adraskan_TminPlot_CalBC.png",width = 20,height = 10, units = "cm",dpi = 600)

Adraskan_TmaxPlot_CalBC <- ggplot(data = Adraskan_BC_Cal, aes(x = Tmax_Obs, y = QDM_Tmax)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TmaxPlot_CalBC)

ggsave("D:/Rdata/QDM/QDM_Plots/Adraskan_TmaxPlot_CalBC.png",width = 20,height = 10, units = "cm",dpi = 600)

# Plotting Bias Corrected Historic Period
Adraskan_TminPlot_HisBC <- Adraskan_BC_His %>%
  mutate(Year = lubridate::year(Date)) %>% 
  ggplot(aes(x = Tmin_Sim, y = QDM_Tmin_His)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Simulated") + 
  ylab("Tmin Bias Corrected") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TminPlot_HisBC)

ggsave("D:/Rdata/QDM/QDM_Plots/Adraskan_TminPlot_HisBC.png",width = 20,height = 10, units = "cm",dpi = 600)

Adraskan_TmaxPlot_HisBC <- Adraskan_BC_His %>%
  mutate(Year = lubridate::year(Date)) %>% 
  ggplot(aes(x = Tmax_Sim, y = QDM_Tmax_His)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TmaxPlot_HisBC)

ggsave("D:/Rdata/QDM/QDM_Plots/Adraskan_TmaxPlot_HisBC.png",width = 20,height = 10, units = "cm",dpi = 600)

#Calculating overall statistical indicators

# For Tmin
Adraskan_Statistics_QDM_Tmin <- Adraskan_BC_Cal %>% 
  summarise(Mean_Bias_Tmin = mean(Tmin_Obs-Tmin_Sim),
            Mean_Bias_TminBC = mean(Tmin_Obs-QDM_Tmin),
            SD_Tmin=sd(Tmin_Obs-Tmin_Sim),
            SD_TminBC=sd(Tmin_Obs-QDM_Tmin),
            RMSEP_Tmin=RMSEP(Tmin_Sim,Tmin_Obs, na.rm=TRUE),
            RMSEP_TminBC=RMSEP(QDM_Tmin, Tmin_Obs, na.rm=TRUE),
            RPIQ_Tmin=RPIQ(Tmin_Sim,Tmin_Obs,na.rm=TRUE),
            RPIQ_TminBC=RPIQ(QDM_Tmin, Tmin_Obs, na.rm=TRUE))

#For Tmax
Adraskan_Statistics_QDM_Tmax <- Adraskan_BC_Cal %>% 
  summarise(Mean_Bias_Tmax = mean(Tmax_Obs-Tmax_Sim),
            Mean_Bias_TmaxBC = mean(Tmax_Obs-QDM_Tmax),
            SD_Tmax=sd(Tmax_Obs-Tmax_Sim),
            SD_TmaxBC=sd(Tmax_Obs-QDM_Tmax),
            RMSEP_Tmax=RMSEP(Tmax_Sim,Tmax_Obs, na.rm=TRUE),
            RMSEP_TmaxBC=RMSEP(QDM_Tmax, Tmax_Obs, na.rm=TRUE),
            RPIQ_Tmax=RPIQ(Tmax_Sim,Tmax_Obs, na.rm=TRUE),
            RPIQ_TmaxBC=RPIQ(QDM_Tmax, Tmax_Obs, na.rm=TRUE))

Adraskan_Average_Statistics <- cbind(Adraskan_Statistics_QDM_Tmin, Adraskan_Statistics_QDM_Tmax)

kable(Adraskan_Average_Statistics, digits = 4, caption = "Summary of the average statistics") %>%
  kable_styling("striped", position = "left", font_size = 10)


#------------------------------------------------------------------------------------------#
# 02: Looping the QDM for all remaining stations
#------------------------------------------------------------------------------------------#

#Loading the stations data 
#Loading the stations data 
Station_Obs_List <-  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs", prefix = "")
Station_Sim_List <- chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Sim", prefix = "")
# Again loading simulated because I will need it for later on use
Station_Sim_His_List <- chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Sim", prefix = "")

filenames <- list.files("D:/Rdata/QDM/Stations_Obs")
#The names are split by _ however there are still three characters now we assign it to 
#each name using purrr function which works as a loop.
Station_Names <- str_split(filenames, pattern = "_") %>% 
  purrr::map_chr(1)

#assign the list of names to the data set
names(Station_Obs_List) <- Station_Names
names(Station_Sim_List) <- Station_Names
names(Station_Sim_His_List) <- Station_Names

Station_Statistics_List <- list()

pb = txtProgressBar(min = 0, max = length(Station_Obs_List), initial = 0, style = 3) 

for(i in 1:length(Station_Obs_List)){
  
  Station_Obs <- Station_Obs_List[[i]]
  Station_Sim <- Station_Sim_List[[i]]
  Station_Sim_His <- Station_Sim_His_List[[i]]
  
colnames(Station_Sim) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
Station_Sim$Tmin_Sim <- as.numeric(Station_Sim$Tmin_Sim)
Station_Sim$Tmax_Sim <- as.numeric(Station_Sim$Tmax_Sim)

colnames(Station_Obs)[6] <- "Tmin_Obs"
colnames(Station_Obs)[7] <- "Tmax_Obs"
Station_Obs$Tmin_Obs <- as.numeric(Station_Obs$Tmin_Obs)
Station_Obs$Tmax_Obs <- as.numeric(Station_Obs$Tmax_Obs)

#test which date format we have

candidate1 <- lubridate::ymd(Station_Obs$Date[1])
candidate2 <- lubridate::dmy(Station_Obs$Date[1])
#candidate3 <- lubridate::dmy_hms(Weather_Obs$Date[1])

#take the candidate which is not an NA
if(is.na(candidate1) == FALSE & is.na(candidate2) == TRUE){
  start_date <- candidate1
  end_date <- lubridate::ymd(Station_Obs$Date[nrow(Station_Obs)])
  Station_Obs$Date <- lubridate::ymd(Station_Obs$Date)
  
  
} else if(is.na(candidate1) == TRUE & is.na(candidate2) == FALSE){
  start_date <- candidate2
  end_date <- lubridate::dmy(Station_Obs$Date[nrow(Station_Obs)])
  Station_Obs$Date <- lubridate::dmy(Station_Obs$Date)
}


Station_Sim <- Station_Sim[lubridate::ymd(Station_Sim$Date) >=  start_date & lubridate::ymd(Station_Sim$Date) <=  end_date,]

#make sure that weather_sim$Date is in a date format
#otherwise merging does not work
Station_Sim$Date <- lubridate::ymd(Station_Sim$Date)


# start_date <- as.Date(Station_Obs$Date[1])
# end_date <- as.Date(Station_Obs$Date[nrow(Station_Obs)])
# Station_Sim <- Station_Sim[as.Date(Station_Sim$Date) >=  start_date & as.Date(Station_Sim$Date) <=  end_date,]

# Adraskan_Obs <- Adraskan_Obs[c("Tmin_Obs", "Tmax_Obs")]
# Adraskan_Sim <- Adraskan_Sim[c("Tmin_Sim", "Tmax_Sim")]

Station_Obs_Sim <- merge(Station_Obs, Station_Sim, by = c("Date"))
Station_Obs_Sim <- Station_Obs_Sim[c("Date", "Year", "Month", "Day", "Tmin_Obs", 
                                       "Tmax_Obs", "Tmin_Sim", "Tmax_Sim")]

#Adraskan_Sim_His <- read.csv("D:/Rdata/QDM/Stations_Sim/Adraskan_Simulated.csv")
colnames(Station_Sim_His) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
Station_Sim_His$Tmin_Sim <- as.numeric(Station_Sim_His$Tmin_Sim)
Station_Sim_His$Tmax_Sim <- as.numeric(Station_Sim_His$Tmax_Sim)

start_date2 <- as.Date("1980-01-01")
Station_Sim_His <- Station_Sim_His[as.Date(Station_Sim_His$Date) >=  as.Date(start_date2) & as.Date(Station_Sim_His$Date) <=  start_date,]

# Delete the last row which is from 2008
Station_Sim_His <- Station_Sim_His[-nrow(Station_Sim_His),]

# Adraskan_Sim_His <- Adraskan_Sim_His[c("Tmin_Sim", "Tmax_Sim")]

QDM_Station_Tmin <- MBC::QDM(o.c = Station_Obs$Tmin_Obs, m.c = Station_Sim$Tmin_Sim, m.p = Station_Sim_His$Tmin_Sim, ratio=FALSE, trace=0.05, trace.calc=0.5*trace,
                              jitter.factor=0, n.tau=NULL, ratio.max=2,
                              ratio.max.trace=10*trace, ECBC=FALSE, ties='first',
                              subsample=NULL, pp.type=7)
QDM_Station_Tmax <- MBC::QDM(o.c = Station_Obs$Tmax_Obs, m.c = Station_Sim$Tmax_Sim, m.p = Station_Sim_His$Tmax_Sim, ratio=FALSE, trace=0.05, trace.calc=0.5*trace,
                              jitter.factor=0, n.tau=NULL, ratio.max=2,
                              ratio.max.trace=10*trace, ECBC=FALSE, ties='first',
                              subsample=NULL, pp.type=7)


# Combining all calibration period
Station_BC_Cal <- cbind(Station_Obs_Sim, QDM_Station_Tmin$mhat.c)
Station_BC_Cal <- cbind(Station_BC_Cal, QDM_Station_Tmax$mhat.c)
colnames(Station_BC_Cal)[9] <- "QDM_Tmin"
colnames(Station_BC_Cal)[10] <- "QDM_Tmax"

##write.csv(Station_BC_Cal, paste0("D:/Rdata/QDM/Stations_Obs_Sim/Calib_", Station_Names[i], "_QDM.csv"), row.names = FALSE)


# Combining all historic period
Station_BC_His <- cbind(Station_Sim_His, QDM_Station_Tmin$mhat.p)
Station_BC_His <- cbind(Station_BC_His, QDM_Station_Tmax$mhat.p)
colnames(Station_BC_His)[4] <- "QDM_Tmin_His"
colnames(Station_BC_His)[5] <- "QDM_Tmax_His"

# Plotting

# Plotting Calibration Period
Station_TminPlot_Cal <- ggplot(data = Station_BC_Cal, aes(x = Tmin_Obs, y = Tmin_Sim)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
#print(Station_TminPlot_Cal)

##ggsave(plot = Station_TminPlot_Cal, filename = paste0("D:/Rdata/QDM/QDM_Plots/", Station_Names[i],"_TminPlot_Cal_WithBias.jpeg"), height = 10, width = 15, units = "cm" )

Station_TmaxPlot_Cal <- ggplot(data = Station_BC_Cal, aes(x = Tmax_Obs, y = Tmax_Sim)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
#print(Station_TmaxPlot_Cal)

##ggsave(plot = Station_TmaxPlot_Cal, filename = paste0("D:/Rdata/QDM/QDM_Plots/", Station_Names[i], "_TmaxPlot_Cal_WithBias.jpeg"), height = 10, width = 15, units = "cm" )

# Plotting Bias Corrected Calibration Period
# Station_TminPlot_CalBC <- ggplot(data = Station_BC_Cal, aes(x = Tmin_Obs, y = QDM_Tmin)) +
#   geom_point(alpha = 0.2) +
#   geom_abline(slope = 1, linetype = "dashed") +
#   xlab("Tmin Observed") + 
#   ylab("Tmin Simulated") + 
#   scale_y_continuous(limits = c(-30,45)) +
#   scale_x_continuous(limits = c(-30,45)) +
#   #facet_wrap( ~ format(x = as.Date(Date))) +
#   #facet_wrap( ~ format(as.Date(Date), "%Y"))
#   facet_wrap(~Year)
# #print(Station_TminPlot_CalBC)
# 
# ggsave(plot = Station_TminPlot_CalBC, filename = paste0("D:/Rdata/QDM/QDM_Plots/", Station_Names[i],"_TminPlot_CalBC.jpeg"), height = 10, width = 15, units = "cm" )
# 
# 
# Station_TmaxPlot_CalBC <- ggplot(data = Station_BC_Cal, aes(x = Tmax_Obs, y = QDM_Tmax)) +
#   geom_point(alpha = 0.2) +
#   geom_abline(slope = 1, linetype = "dashed") +
#   xlab("Tmin Observed") + 
#   ylab("Tmin Simulated") + 
#   scale_y_continuous(limits = c(-30,45)) +
#   scale_x_continuous(limits = c(-30,45)) +
#   #facet_wrap( ~ format(x = as.Date(Date))) +
#   #facet_wrap( ~ format(as.Date(Date), "%Y"))
#   facet_wrap(~Year)
# #print(Station_TmaxPlot_CalBC)
# 
# ggsave(plot = Station_TmaxPlot_CalBC, filename = paste0("D:/Rdata/QDM/QDM_Plots/", Station_Names[i], "_TmaxPlot_CalBC.jpeg"), height = 10, width = 15, units = "cm" )

#Tmin
Station_PlotBC_Tmin <- ggplot(Station_BC_Cal, aes(x = Date, y = Tmin_Obs, group = 1)) + 
  geom_line(aes(colour = "Observed"),lwd=0.3) + 
  xlab("Time series of temperature data") + 
  ylab("Daily minimum temperature (°C)") + 
  #scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*700)]) +
  geom_line(data = Station_BC_Cal, aes(x = Date, y = Tmin_Sim, colour = "Simulated"),lwd=0.3) +
  geom_line(data = Station_BC_Cal, aes(x = Date, y = QDM_Tmin, colour = "Bias Corrected"),lwd=0.3) + 
  #scale_y_continuous(limits = c(-30,45)) + 
  expand_limits(y = c(min(Station_BC_Cal$Tmin_Obs), max(Station_BC_Cal$Tmin_Obs))) +
  facet_wrap(~ format(Date, "%Y"), scales = "free_x") +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(name = "Temperature Data\nTypes", values = c("Observed" = "darkblue", "Simulated" = "red", "Bias Corrected" = "orange"))

ggsave(plot = Station_PlotBC_Tmin, filename = paste0("D:/Rdata/QDM/QDM_Plots/", Station_Names[i], "_Tmin_BC.jpeg"), height = 13, width = 24, units = "cm" )

#Tmax

Station_PlotBC_Tmax <- ggplot(Station_BC_Cal, aes(x = Date, y = Tmax_Obs, group = 1)) + 
  geom_line(aes(colour = "Observed"),lwd=0.3) + 
  xlab("Time series of temperature data") + 
  ylab("Daily maximum temperature (°C)") + 
  #scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*700)]) +
  geom_line(data = Station_BC_Cal, aes(x = Date, y = Tmax_Sim, colour = "Simulated"),lwd=0.3) +
  geom_line(data = Station_BC_Cal, aes(x = Date, y = QDM_Tmax, colour = "Bias Corrected"),lwd=0.3) + 
  #scale_y_continuous(limits = c(-30,45)) + 
  expand_limits(y = c(min(Station_BC_Cal$Tmax_Obs), max(Station_BC_Cal$Tmax_Obs))) +
  facet_wrap(~ format(Date, "%Y"), scales = "free_x")+
  scale_x_date(date_labels = "%b") +
  scale_color_manual(name = "Temperature Data\nTypes", values = c("Observed" = "darkblue", "Simulated" = "red", "Bias Corrected" = "orange"))


#Station_PlotBC_Tmax <- Station_PlotBC_Tmax + scale_x_date(date_labels = "%b")

ggsave(plot = Station_PlotBC_Tmax, filename = paste0("D:/Rdata/QDM/QDM_Plots/", Station_Names[i], "_Tmax_BC.jpeg"), height = 13, width = 24, units = "cm" )

#require(hydroGOF)
#Calculating overall statistical indicators
# For Tmin
Station_Statistics_QDM_Tmin <- Station_BC_Cal %>% 
  summarise(Mean_Bias_Tmin = mean(Tmin_Obs-Tmin_Sim),
            #Mean_Bias_TminBC = mean(Tmin_Obs-QDM_Tmin),
            SD_Tmin=sd(Tmin_Obs-Tmin_Sim),
            SD_TminBC=sd(Tmin_Obs-QDM_Tmin),
            RMSEP_Tmin=RMSEP(Tmin_Sim,Tmin_Obs),
            RMSEP_TminBC=RMSEP(QDM_Tmin, Tmin_Obs),
            MeanAbsErr_Tmin = mae(Tmin_Obs, Tmin_Sim),
            MeanAbsErrBC_Tmin = mae(Tmin_Obs, QDM_Tmin),
            RPIQ_Tmin=RPIQ(Tmin_Sim,Tmin_Obs),
            RPIQ_TminBC=RPIQ(QDM_Tmin, Tmin_Obs))

#For Tmax
Station_Statistics_QDM_Tmax <- Station_BC_Cal %>% 
  summarise(Mean_Bias_Tmax = mean(Tmax_Obs-Tmax_Sim),
            #Mean_Bias_TmaxBC = mean(Tmax_Obs-QDM_Tmax),
            SD_Tmax=sd(Tmax_Obs-Tmax_Sim),
            SD_TmaxBC=sd(Tmax_Obs-QDM_Tmax),
            RMSEP_Tmax=RMSEP(Tmax_Sim,Tmax_Obs),
            RMSEP_TmaxBC=RMSEP(QDM_Tmax, Tmax_Obs),
            MeanAbsErr_Tmax = mae(Tmax_Obs, Tmax_Sim),
            MeanAbsErrBC_Tmin = mae(Tmax_Obs, QDM_Tmax),
            RPIQ_Tmax=RPIQ(Tmax_Sim,Tmax_Obs),
            RPIQ_TmaxBC=RPIQ(QDM_Tmax, Tmax_Obs))

Station_Average_Statistics <- cbind(Station_Statistics_QDM_Tmin, Station_Statistics_QDM_Tmax)

Station_Statistics_List[[i]] <- Station_Average_Statistics

##write.csv(Station_Average_Statistics, file = paste0("D:/Rdata/QDM/Stations_Obs_Sim/BC_", Station_Names[i], "_Statistics.csv"), row.names = FALSE)

#Merging the two temperature series (Observed with historic bias corrected)

#1 Cleaning recent (Obs)
#Choosing the wanted columns
Recent_QDM <- Station_Obs_Sim[,c("Date", "Tmin_Obs", "Tmax_Obs")] 

#Renaming the column names
colnames(Recent_QDM)[2] <- "Tmin"
colnames(Recent_QDM)[3] <- "Tmax"

#2 Cleaning historic (BC)
#Choosing the wanted columns
Historic_QDM <- Station_BC_His[,c("Date", "QDM_Tmin_His", "QDM_Tmax_His")]

#Renaming the column names
colnames(Historic_QDM)[2] <- "Tmin"
colnames(Historic_QDM)[3] <- "Tmax"

Recent_QDM$Date <- as.character(Recent_QDM$Date)

station_QDM_1980_2020 <- rbind(Historic_QDM, Recent_QDM)

station_QDM_1980_2020 <- station_QDM_1980_2020 %>% 
  arrange(Date)

#Insert Year, Month and Day column 
station_QDM_1980_2020$Date <- as.Date(station_QDM_1980_2020$Date)

# extract year, month and day information into separate columns
station_QDM_1980_2020$Year <- format(station_QDM_1980_2020$Date, '%Y')
station_QDM_1980_2020$Month <- format(station_QDM_1980_2020$Date, '%m')
station_QDM_1980_2020$Day <- format(station_QDM_1980_2020$Date, '%d')

#Rearrange the columns 
station_QDM_1980_2020 <- station_QDM_1980_2020[,c("Date", "Year", "Month", "Day", "Tmin", "Tmax")]

##write.csv(station_QDM_1980_2020, file = paste0("D:/Rdata/QDM/Stations_Obs_Sim/1980_2020_", Station_Names[i], "_BC_QDM.csv"), row.names = FALSE)

setTxtProgressBar(pb,i)
}


#------------------------------------------------------------------------------------------#
# 03: Combining all the average statistics of 51 stations into a single table
#------------------------------------------------------------------------------------------#

#Loading the stations data 
Station_Average_Statistics <-  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs_Sim", prefix = "BC_")

#Since the files names are not there, we used list.file to show the names
filenames2 <- list.files("D:/Rdata/QDM/Stations_Obs_Sim", pattern = "Statistics", all.files = FALSE)
#The names are split by _ however there are still three characters now we assign it to 
#each name using purrr function which works as a loop.
Station_Names <- str_split(filenames2, pattern = "_") %>% 
  purrr::map_chr(2)

names(Station_Average_Statistics) <- Station_Names

#library(dplyr)

# merge all the data frames in the list into one data frame
All_Statistics_QDM <- bind_rows(Station_Average_Statistics)

# create a new column with the station names
All_Statistics_QDM$Station_Name <- Station_Names

# rearrange the columns so that the new column is at the start
All_Statistics_QDM <- All_Statistics_QDM[, c("Station_Name", colnames(All_Statistics_QDM))]

# remove the ".csv" extension from the station names
#All_Statistics$Station_Name <- gsub(".csv", "", All_Statistics$Station_Name)

# Remove the last columns 
All_Statistics_QDM <- All_Statistics_QDM[-c(20)]

kable(All_Statistics_QDM, digits = 3, caption = "Mean statistics using QDM method") %>%
  kable_styling("striped", position = "left", font_size = 10)

write.csv(All_Statistics_QDM, "D:/Rdata/QDM/Stations_Obs_Sim/All_Statistics_QDM_MAE.csv")