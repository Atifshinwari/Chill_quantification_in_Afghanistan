
#------------------------------------------------------------------------------------------#
# Chill Quantification for Afghanistan: Historic scenarios
#------------------------------------------------------------------------------------------#                      ### Chill Quantification for Afghanistan ###
                      
require(ggplot2)
require(lubridate)
require(tidyverse)
require(chillR)
require(kableExtra)
require(Metrics)
require(hydroGOF)
require(downscaleR)
require(zoo)
require(latticeExtra)
require(QMTap)
require(dplyr)
require(stringr)


# Loading temperature data (all stations)
Station_AT_List <-  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs_Sim", prefix = "1980_2020_")

#Assigning the names
List_Names <- list.files("D:/Rdata/QDM/Stations_Obs")
Stations_Names <- str_split(List_Names, pattern = "_") %>% 
  purrr::map_chr(1)
names(Station_AT_List) <- Stations_Names

#------------------------------------------------------------------------------------------#
# For one station: Adraskan historic chill quantification
#------------------------------------------------------------------------------------------#

scenario_1980<-temperature_scenario_from_records(weather=Adraskan_cal,year=1980)

temps_1980<-temperature_generation(weather=Adraskan_cal, years=c(1980,2020),
                                   sim_years=c(2000,2100),
                                   temperature_scenario = scenario_1980)

scenario_2000<-temperature_scenario_from_records(weather=Adraskan_cal,year=2000)

relative_scenario<-temperature_scenario_baseline_adjustment(
  baseline=scenario_2000,
  temperature_scenario = scenario_1980)

temps_1980<-temperature_generation(weather=Adraskan_cal, years=c(1980,2020),
                                   sim_years=c(2000,2100),
                                   temperature_scenario = relative_scenario)


all_past_scenarios<-temperature_scenario_from_records(
  weather=Adraskan_cal,
  year=c(1980,1990,2000,2010))

adjusted_scenarios<-temperature_scenario_baseline_adjustment(
  baseline=scenario_2000,
  temperature_scenario = all_past_scenarios)

all_past_scenario_temps<-temperature_generation(
  weather=Adraskan_cal,
  years=c(1980,2020),
  sim_years=c(2000,2100),
  temperature_scenario = adjusted_scenarios)


chill_hist_scenario_list<-tempResponse_daily_list(all_past_scenario_temps,
                                                  latitude=33.6,
                                                  Start_JDay = 305,
                                                  End_JDay = 59)

scenarios<-names(chill_hist_scenario_list)[1:4]

all_scenarios<-chill_hist_scenario_list[[scenarios[1]]]
all_scenarios[,"scenario"]<-as.numeric(scenarios[1])


for (sc in scenarios[2:4])
  all_scenarios<-rbind(all_scenarios,
                       cbind(chill_hist_scenario_list[[sc]],scenario=as.numeric(sc)))

all_scenarios<-all_scenarios[which(all_scenarios$Perc_complete==100),]

# Compute the actual 'observed' chill for comparison
actual_chill<-tempResponse_daily_list(Adraskan_cal,latitude=33.6,
                                      Start_JDay = 305,
                                      End_JDay = 59)[[1]]
actual_chill<-actual_chill[which(actual_chill$Perc_complete==100),]

Adraskan_all_His_scenarios <- ggplot(data=all_scenarios,aes(scenario,Chill_Portions,
                                                            fill=factor(scenario))) +
  geom_violin() +
  ylab("Chill accumulation (Chill Portions)") +
  xlab("Scenario year") +
  theme_bw(base_size=15) +
  ylim(c(0,90)) +
  geom_point(data=actual_chill,
             aes(End_year,Chill_Portions,fill="blue"),
             col="blue",show.legend = FALSE) +
  scale_fill_discrete(name="Scenario",
                      breaks = unique(all_scenarios$scenario)) 
ggsave("D:/Rdata/Chill_quantification/Adraskan/Adraskan_all_His_scenarios.png", width = 20, height = 10, units = "cm", dpi = 600)


temperature_means<-data.frame(Year=min(Adraskan_cal$Year):max(Adraskan_cal$Year),
                              Tmin=aggregate(Adraskan_cal$Tmin,FUN="mean",
                                             by=list(Adraskan_cal$Year))[,2],
                              Tmax=aggregate(Adraskan_cal$Tmax,FUN="mean",
                                             by=list(Adraskan_cal$Year))[,2])
temperature_means[,"runn_mean_Tmin"]<-runn_mean(temperature_means$Tmin,15)
temperature_means[,"runn_mean_Tmax"]<-runn_mean(temperature_means$Tmax,15)

Tmin_regression<-lm(Tmin~Year, temperature_means)
temperature_means[,"regression_Tmin"]<-Tmin_regression$coefficients[1]+
  Tmin_regression$coefficients[2]*temperature_means$Year

Tmax_regression<-lm(Tmax~Year, temperature_means)
temperature_means[,"regression_Tmax"]<-Tmax_regression$coefficients[1]+
  Tmax_regression$coefficients[2]*temperature_means$Year


Adraskan_Mean_Temp_Tmin <- ggplot(temperature_means,aes(Year, Tmin)) + 
  geom_point() + 
  geom_line(data=temperature_means,aes(Year, runn_mean_Tmin),lwd=2,col="blue") + 
  geom_line(data=temperature_means,aes(Year, regression_Tmin),lwd=2,col="red") +
  theme_bw(base_size=15) +
  ylab("Mean monthly minimum temperature (°C)")
ggsave("D:/Rdata/Chill_quantification/Adraskan/Adraskan_Mean_Temp_Tmin.png", width = 20, height = 10, units = "cm", dpi = 600)


Adraskan_Mean_Temp_Tmax <- ggplot(temperature_means,aes(Year, Tmax)) + 
  geom_point() + 
  geom_line(data=temperature_means,aes(Year, runn_mean_Tmax),lwd=2,col="blue") + 
  geom_line(data=temperature_means,aes(Year, regression_Tmax),lwd=2,col="red") +
  theme_bw(base_size=15) +
  ylab("Mean monthly maximum temperature (°C)")
ggsave("D:/Rdata/Chill_quantification/Adraskan/Adraskan_Mean_Temp_Tmax.png", width = 20, height = 10, units = "cm", dpi = 600)


#------------------------------------------------------------------------------------------#
# Loop Historic temperature scenarios: All stations
#------------------------------------------------------------------------------------------#

pb = txtProgressBar(min = 0, max = length(Station_AT_List), initial = 0, style = 3) 


for(i in 1:length(Station_AT_List)){
  
  Station_cal <- Station_AT_List[[i]]
  Latitudes <- Station_Latitudes[[i]]
  
  scenario_1980<-temperature_scenario_from_records(weather=Station_cal,year=1980)
  
  temps_1980<-temperature_generation(weather=Station_cal, years=c(1980,2020),
                                     sim_years=c(2000,2100),
                                     temperature_scenario = scenario_1980)
  
  scenario_2000<-temperature_scenario_from_records(weather=Station_cal,year=2000)
  
  relative_scenario<-temperature_scenario_baseline_adjustment(
    baseline=scenario_2000,
    temperature_scenario = scenario_1980)
  
  temps_1980<-temperature_generation(weather=Station_cal, years=c(1980,2020),
                                     sim_years=c(2000,2100),
                                     temperature_scenario = relative_scenario)
  write.csv(temps_1980, file = paste0("D:/Rdata/Chill_quantification/Historic_scenarios/Temp_Generation_Historic/temps_1980_", Stations_Names[i], ".csv"), row.names = FALSE)
  
  
  all_past_scenarios<-temperature_scenario_from_records(
    weather=Station_cal,
    year=c(1980,1990,2000,2010))
  
  adjusted_scenarios<-temperature_scenario_baseline_adjustment(
    baseline=scenario_2000,
    temperature_scenario = all_past_scenarios)
  
  all_past_scenario_temps<-temperature_generation(
    weather=Station_cal,
    years=c(1980,2020),
    sim_years=c(2000,2100),
    temperature_scenario = adjusted_scenarios)
  write.csv(all_past_scenario_temps, file = paste0("D:/Rdata/Chill_quantification/Historic_scenarios/All_Historic_Scenarios/all_past_scenarios_", Stations_Names[i], ".csv"), row.names = FALSE)
  
  
  chill_hist_scenario_list<-tempResponse_daily_list(all_past_scenario_temps,
                                                    latitude=Latitudes,
                                                    Start_JDay = 305,
                                                    End_JDay = 59)
  
  scenarios<-names(chill_hist_scenario_list)[1:4]
  
  all_scenarios<-chill_hist_scenario_list[[scenarios[1]]]
  all_scenarios[,"scenario"]<-as.numeric(scenarios[1])
  
  
  for (sc in scenarios[2:4])
    all_scenarios<-rbind(all_scenarios,
                         cbind(chill_hist_scenario_list[[sc]],scenario=as.numeric(sc)))
  
  all_scenarios<-all_scenarios[which(all_scenarios$Perc_complete==100),]
  
  # Compute the actual 'observed' chill for comparison
  actual_chill<-tempResponse_daily_list(Station_cal,latitude=Latitudes,
                                        Start_JDay = 305,
                                        End_JDay = 59)[[1]]
  actual_chill<-actual_chill[which(actual_chill$Perc_complete==100),]
  
  Station_all_His_scenarios <- ggplot(data=all_scenarios,aes(scenario,Chill_Portions,
                                                             fill=factor(scenario))) +
    geom_violin() +
    ylab("Chill accumulation (Chill Portions)") +
    xlab("Scenario year") +
    theme_bw(base_size=15) +
    ylim(c(0,90)) +
    geom_point(data=actual_chill,
               aes(End_year,Chill_Portions,fill="blue"),
               col="blue",show.legend = FALSE) +
    scale_fill_discrete(name="Scenario",
                        breaks = unique(all_scenarios$scenario)) 
  
  ggsave(plot = Station_all_His_scenarios, filename = paste0("D:/Rdata/Chill_quantification/Historic_scenarios/Plots_Historic/All_Hist_Scenarios_", Stations_Names[i],".jpeg"), height = 10, width = 15, units = "cm" )
  
  
  temperature_means<-data.frame(Year=min(Station_cal$Year):max(Station_cal$Year),
                                Tmin=aggregate(Station_cal$Tmin,FUN="mean",
                                               by=list(Station_cal$Year))[,2],
                                Tmax=aggregate(Station_cal$Tmax,FUN="mean",
                                               by=list(Station_cal$Year))[,2])
  temperature_means[,"runn_mean_Tmin"]<-runn_mean(temperature_means$Tmin,15)
  temperature_means[,"runn_mean_Tmax"]<-runn_mean(temperature_means$Tmax,15)
  
  Tmin_regression<-lm(Tmin~Year, temperature_means)
  temperature_means[,"regression_Tmin"]<-Tmin_regression$coefficients[1]+
    Tmin_regression$coefficients[2]*temperature_means$Year
  
  Tmax_regression<-lm(Tmax~Year, temperature_means)
  temperature_means[,"regression_Tmax"]<-Tmax_regression$coefficients[1]+
    Tmax_regression$coefficients[2]*temperature_means$Year
  
  
  Station_Mean_Temp_Tmin <- ggplot(temperature_means,aes(Year, Tmin)) + 
    geom_point() + 
    geom_line(data=temperature_means,aes(Year, runn_mean_Tmin),lwd=2,col="blue") + 
    geom_line(data=temperature_means,aes(Year, regression_Tmin),lwd=2,col="red") +
    theme_bw(base_size=15) +
    ylab("Mean monthly minimum temperature (°C)")
  
  ggsave(plot = Station_Mean_Temp_Tmin, filename = paste0("D:/Rdata/Chill_quantification/Historic_scenarios/Plots_Historic/Tmin_Mean_Temp_", Stations_Names[i],".jpeg"), height = 10, width = 15, units = "cm" )
  
  
  Station_Mean_Temp_Tmax <- ggplot(temperature_means,aes(Year, Tmax)) + 
    geom_point() + 
    geom_line(data=temperature_means,aes(Year, runn_mean_Tmax),lwd=2,col="blue") + 
    geom_line(data=temperature_means,aes(Year, regression_Tmax),lwd=2,col="red") +
    theme_bw(base_size=15) +
    ylab("Mean monthly maximum temperature (°C)")
  
  ggsave(plot = Station_Mean_Temp_Tmax, filename = paste0("D:/Rdata/Chill_quantification/Historic_scenarios/Plots_Historic/Tmax_Mean_Temp_", Stations_Names[i],".jpeg"), height = 10, width = 15, units = "cm" )
  
  setTxtProgressBar(pb,i)
}

