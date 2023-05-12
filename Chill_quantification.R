                      ### Chill Quantification for Afghanistan ###
                      ### Chill Quantification for Afghanistan ###
                      ### Chill Quantification for Afghanistan ###

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


                          ### Adraskan Chill Quantification ###
                          ### Adraskan Chill Quantification ###

#Making hourly temperature 
Adraskan_hourly <- stack_hourly_temps(Station_AT_List$Adraskan, latitude=33.6)
Adraskan_hourly <- Adraskan_hourly$hourtemps

#Making idealized curve
Adraskan_hourly[,"DATE"]<-ISOdate(Adraskan_hourly$Year,Adraskan_hourly$Month, Adraskan_hourly$Day, Adraskan_hourly$Hour)

Adraskan_ideal_curve <- ggplot(Adraskan_hourly[1:96,],aes(x = DATE,y = Temp)) + 
  geom_line(lwd=1.5) + 
  xlab("Date") + 
  ylab("Temperature (°C)") + 
  theme_bw(base_size = 20)

ggsave("D:/Rdata/Chill_quantification/Adraskan/Adraskan_ideal_curve.png", width = 20, height = 10, units = "cm", dpi = 600)

#Chill metrics quantification
Adraskan_metrics<-tempResponse(make_JDay(Adraskan_hourly),Start_JDay = 1, End_JDay = 365, models = list(Chil_Portions = Dynamic_Model, GDH= GDH))

# Saving Adraskan chill metrics
write.csv(Adraskan_metrics, "D:/Rdata/Chill_quantification/Adraskan/Adraskan_metrics.csv", row.names = FALSE)

# Temperature generation
Adraskan_cal <- Station_AT_List$Adraskan
Adraskan_cal <- Adraskan_cal[c("Year", "Month", "Day", "Tmin", "Tmax")]
#write.csv(Adraskan_cal, "D:/Rdata/Chill_quantification/Adraskan/Adraskan_cal.csv", row.names = FALSE)

Adraskan_Temp<-temperature_generation(Adraskan_cal,
                             years=c(1980,2020),
                             sim_years = c(2000,2100))
write.csv(Adraskan_Temp, "D:/Rdata/Chill_quantification/Adraskan/Temp_generation/Adraskan_Temp.csv", row.names = FALSE)
#Adraskan_Temp <-  chillR::load_temperature_scenarios("D:/Rdata/Chill_quantification/Adraskan/Temp_generation/Adraskan_Temp.csv")

# Combining period 1980:2020 with 2000:2100
Adraskan_Temperatures<-cbind(Adraskan_cal[
  which(Adraskan_cal$Year %in% 1980:2020),] ,Data_source="observed")
Adraskan_Temperatures<-rbind(Adraskan_Temperatures,
                    cbind(Adraskan_Temp[[1]][,c("Year","Month","Day","Tmin","Tmax")],
                          Data_source="simulated"))
Adraskan_Temperatures[,"Date"]<-as.Date(ISOdate(2000,
                                                Adraskan_Temperatures$Month,
                                                Adraskan_Temperatures$Day))
write.csv(Adraskan_Temperatures, "D:/Rdata/Chill_quantification/Adraskan/Temp_generation/Adraskan_Temperatures.csv", row.names = FALSE)
#Adraskan_Temperatures <-  chillR::load_temperature_scenarios("D:/Rdata/Chill_quantification/Adraskan/Temp_generation/Adraskan_Temperatures.csv")

# Plotting (Observed temp is from 1980:2020 and simulated from 2000:2100)
# Tmin
Adraskan_Temperatures_Tmin <- ggplot(data=Adraskan_Temperatures, aes(Date,Tmin)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")
ggsave("D:/Rdata/Chill_quantification/Adraskan/Adraskan_Temperatures_Tmin.png", width = 20, height = 10, units = "cm", dpi = 600)

# Tmax
Adraskan_Temperatures_Tmax <- ggplot(data=Adraskan_Temperatures, aes(Date,Tmax)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")
ggsave("D:/Rdata/Chill_quantification/Adraskan/Adraskan_Temperatures_Tmax.png", width = 20, height = 10, units = "cm", dpi = 600)

# Distribution of winter chill for Adraskan based on this analysis
Adraskan_chill_observed<-chilling(
  stack_hourly_temps(
    Adraskan_Temperatures[which(Adraskan_Temperatures$Data_source=="observed"),],
    latitude = 33.6),
  Start_JDay = 305,
  End_JDay = 59)
Adraskan_chill_simulated<-chilling(
  stack_hourly_temps(
    Adraskan_Temperatures[which(Adraskan_Temperatures$Data_source=="simulated"),],
    latitude = 33.6),
  Start_JDay = 305,
  End_JDay = 59)

Adraskan_chill_comparison<-cbind(Adraskan_chill_observed ,Data_source="observed")
Adraskan_chill_comparison<-rbind(Adraskan_chill_comparison,
                        cbind(Adraskan_chill_simulated ,Data_source="simulated"))

Adraskan_chill_comparison_full_seasons<-Adraskan_chill_comparison[
  which(Adraskan_chill_comparison$Perc_complete==100),]

# Now lets plot it (this period is from 1980 to 2100)
Adraskan_chill_distri <- ggplot(Adraskan_chill_comparison_full_seasons, aes(x=Chill_portions)) + 
  geom_histogram(binwidth=1,aes(fill = factor(Data_source))) +
  theme_bw(base_size = 20) +
  labs(fill = "Data source") +
  xlab("Chill accumulation (Chill Portions)") +
  ylab("Frequency")
ggsave("D:/Rdata/Chill_quantification/Adraskan/Adraskan_chill_distri.png", width = 20, height = 10, units = "cm", dpi = 600)

#Now we have a histogram showing the distribution of chill that it would have been
#reasonable to expect in Adraskan between 1980 and 2020

# Plotting it as cumulative distribution function
#Which makes it easy to extract the risk of falling below a particular level of chill accumulation
Adraskan_chill_simulations<-Adraskan_chill_comparison_full_seasons[
  which(Adraskan_chill_comparison_full_seasons$Data_source=="simulated"),]

Adraskan_cumulative_chill <- ggplot(Adraskan_chill_simulations, aes(x=Chill_portions)) +
  stat_ecdf(geom = "step",lwd=1.5,col="blue") +
  ylab("Cumulative probability") +
  xlab("Chill accumulation (in Chill Portions)") +
  theme_bw(base_size = 20)
ggsave("D:/Rdata/Chill_quantification/Adraskan/Adraskan_cumulative_chill.png", width = 20, height = 10, units = "cm", dpi = 600)

# Here's the amount of chill that is exceeded in 90% of all years.(Safe winter chill)
Adraskan_Safe_WC <- quantile(Adraskan_chill_simulations$Chill_portions, 0.1)
write.csv(Adraskan_Safe_WC, "D:/Rdata/Chill_quantification/Adraskan/Adraskan_Safe_WC.csv", row.names = FALSE)

# Here's the 50% confidence interval (25th to 75th percentile)
Adraskan_chill_confInter <- quantile(Adraskan_chill_simulations$Chill_portions, c(0.25,0.75))
write.csv(Adraskan_chill_confInter, "D:/Rdata/Chill_quantification/Adraskan/Adraskan_chill_Confid_Inter.csv", row.names = FALSE)

# Here it is important to note that so far we have not applied the scenarios



                                       ### Loop Chill Quantification for Afghanistan ###
                                       ### Loop Chill Quantification for Afghanistan ###
                                       ### Loop Chill Quantification for Afghanistan ###


# Loading temperature data (all stations)
Station_AT_List <-  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs_Sim", prefix = "1980_2020_")

#Assigning the names
List_Names <- list.files("D:/Rdata/QDM/Stations_Obs")
Stations_Names <- str_split(List_Names, pattern = "_") %>% 
  purrr::map_chr(1)
names(Station_AT_List) <- Stations_Names

Station_Latitudes <- c(33.63, 36.01, 34.91, 35.14, 36.10, 37.00, 34.82, 34.52, 35.72, 34.23, 36.16, 31.13, 35.31, 36.41, 34.54, 35.96, 35.60, 36.73,34.82, 37.11, 32.36, 34.50, 33.59, 35.28, 36.93, 35.55, 35.96, 31.58, 33.35, 34.81, 31.61, 36.18, 34.74, 36.63, 36.80, 36.73, 34.44, 33.55, 34.34, 34.54, 35.67, 36.58, 36.53, 34.68, 34.33, 36.05, 34.40, 36.65, 32.63, 35.25, 34.22)

pb = txtProgressBar(min = 0, max = length(Station_AT_List), initial = 0, style = 3) 


### Adraskan Chill Quantification ###
### Adraskan Chill Quantification ###

for(i in 1:length(Station_AT_List)){
  
  Station_obs <- Station_AT_List[[i]]
  Latitudes <- Station_Latitudes[[i]]

#Making hourly temperature 
Station_hourly <- stack_hourly_temps(Station_obs, latitude= Latitudes)
Station_hourly <- Station_hourly$hourtemps

#Making idealized curve
Station_hourly[,"DATE"]<-ISOdate(Station_hourly$Year,Station_hourly$Month, Station_hourly$Day, Station_hourly$Hour)

Station_ideal_curve <- ggplot(Station_hourly[1:96,],aes(x = DATE,y = Temp)) + 
  geom_line(lwd=1.5) + 
  xlab("Date") + 
  ylab("Temperature (°C)") + 
  theme_bw(base_size = 20)

ggsave(plot = Station_ideal_curve, filename = paste0("D:/Rdata/Chill_quantification/Plots_Chill/Idealized_Curve", Stations_Names[i],"_.jpeg"), height = 10, width = 15, units = "cm" )

#Chill metrics quantification
Station_metrics<-tempResponse(make_JDay(Station_hourly),Start_JDay = 1, End_JDay = 365, models = list(Chil_Portions = Dynamic_Model, GDH= GDH))

# Saving Adraskan chill metrics
write.csv(Station_metrics, file = paste0("D:/Rdata/Chill_quantification/Metrics_Chill/Chill_metrics_", Stations_Names[i], ".csv"), row.names = FALSE)

# Temperature generation
Station_cal <- Station_AT_List[[i]]
Station_cal <- Station_cal[c("Year", "Month", "Day", "Tmin", "Tmax")]
#write.csv(Adraskan_cal, "D:/Rdata/Chill_quantification/Adraskan/Adraskan_cal.csv", row.names = FALSE)

Station_Temp<-temperature_generation(Station_cal,
                                      years=c(1980,2020),
                                      sim_years = c(2000,2100))

write.csv(Station_Temp, file = paste0("D:/Rdata/Chill_quantification/Temp_generation/Station_Temp_", Stations_Names[i], ".csv"), row.names = FALSE)
#Station_Temp <-  chillR::load_temperature_scenarios("D:/Rdata/Chill_quantification/Temp_generation", prefix = "Station_Temp_")

# Combining period 1980:2020 with 2000:2100
Station_Temperatures<-cbind(Station_cal[
  which(Station_cal$Year %in% 1980:2020),] ,Data_source="observed")
Station_Temperatures<-rbind(Station_Temperatures,
                             cbind(Station_Temp[[1]][,c("Year","Month","Day","Tmin","Tmax")],
                                   Data_source="simulated"))
Station_Temperatures[,"Date"]<-as.Date(ISOdate(2000,
                                               Station_Temperatures$Month,
                                               Station_Temperatures$Day))

write.csv(Station_Temperatures, file = paste0("D:/Rdata/Chill_quantification/Temp_generation/Station_Temperatures_", Stations_Names[i], ".csv"), row.names = FALSE)
#Station_Temperatures <-  chillR::load_temperature_scenarios("D:/Rdata/Chill_quantification/Temp_generation", prefix = "Station_Temperatures_")

# Plotting (Observed temp is from 1980:2020 and simulated from 2000:2100)
# Tmin
Station_Temperatures_Tmin <- ggplot(data=Station_Temperatures, aes(Date,Tmin)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")

ggsave(plot = Station_Temperatures_Tmin, filename = paste0("D:/Rdata/Chill_quantification/Plots_Chill/Tmin_Temperature_", Stations_Names[i],".jpeg"), height = 10, width = 15, units = "cm" )

# Tmax
Station_Temperatures_Tmax <- ggplot(data=Station_Temperatures, aes(Date,Tmax)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")
ggsave("D:/Rdata/Chill_quantification/Adraskan/Adraskan_Temperatures_Tmax.png", width = 20, height = 10, units = "cm", dpi = 600)
ggsave(plot = Station_Temperatures_Tmax, filename = paste0("D:/Rdata/Chill_quantification/Plots_Chill/Tmax_Temperature_", Stations_Names[i],".jpeg"), height = 10, width = 15, units = "cm" )

# Distribution of winter chill for Adraskan based on this analysis
Station_chill_observed<-chilling(
  stack_hourly_temps(
    Station_Temperatures[which(Station_Temperatures$Data_source=="observed"),],
    latitude = Latitudes),
  Start_JDay = 305,
  End_JDay = 59)
Station_chill_simulated<-chilling(
  stack_hourly_temps(
    Station_Temperatures[which(Station_Temperatures$Data_source=="simulated"),],
    latitude = Latitudes),
  Start_JDay = 305,
  End_JDay = 59)

Station_chill_comparison<-cbind(Station_chill_observed ,Data_source="observed")
Station_chill_comparison<-rbind(Station_chill_comparison,
                                 cbind(Station_chill_simulated ,Data_source="simulated"))

Station_chill_comparison_full_seasons<-Station_chill_comparison[
  which(Station_chill_comparison$Perc_complete==100),]

# Now lets plot it (this period is from 1980 to 2100)
Station_chill_distri <- ggplot(Station_chill_comparison_full_seasons, aes(x=Chill_portions)) + 
  geom_histogram(binwidth=1,aes(fill = factor(Data_source))) +
  theme_bw(base_size = 20) +
  labs(fill = "Data source") +
  xlab("Chill accumulation (Chill Portions)") +
  ylab("Frequency")

ggsave(plot = Station_chill_distri, filename = paste0("D:/Rdata/Chill_quantification/Plots_Chill/Chill_distribution_", Stations_Names[i],".jpeg"), height = 10, width = 15, units = "cm" )

#Now we have a histogram showing the distribution of chill that it would have been
#reasonable to expect in Adraskan between 1980 and 2020

# Plotting it as cumulative distribution function
#Which makes it easy to extract the risk of falling below a particular level of chill accumulation
Station_chill_simulations<-Station_chill_comparison_full_seasons[
  which(Station_chill_comparison_full_seasons$Data_source=="simulated"),]

Station_cumulative_chill <- ggplot(Station_chill_simulations, aes(x=Chill_portions)) +
  stat_ecdf(geom = "step",lwd=1.5,col="blue") +
  ylab("Cumulative probability") +
  xlab("Chill accumulation (in Chill Portions)") +
  theme_bw(base_size = 20)

ggsave(plot = Station_cumulative_chill, filename = paste0("D:/Rdata/Chill_quantification/Plots_Chill/Cumulative_chill_", Stations_Names[i],".jpeg"), height = 10, width = 15, units = "cm" )

# Here's the amount of chill that is exceeded in 90% of all years.(Safe winter chill)
Station_Safe_WC <- quantile(Station_chill_simulations$Chill_portions, 0.1)
write.csv(Station_Safe_WC, file = paste0("D:/Rdata/Chill_quantification/Metrics_Chill/Safe_WC_", Stations_Names[i], ".csv"), row.names = FALSE)

# Here's the 50% confidence interval (25th to 75th percentile)
Station_chill_confInter <- quantile(Station_chill_simulations$Chill_portions, c(0.25,0.75))

write.csv(Station_chill_confInter, file = paste0("D:/Rdata/Chill_quantification/Metrics_Chill/Confidence_interval_", Stations_Names[i], "chill.csv"), row.names = FALSE)

# Here it is important to note that so far we have not applied the scenarios

setTxtProgressBar(pb,i)
}



                                         # For Adraskan it is below
                                  ### Lopp Historic temperature scenarios###
                                  ### Loop Historic temperature scenarios###
                                  ### Loop Historic temperature scenarios###

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

# Let's compute the actual 'observed' chill for comparison
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



                                   ### Historic temperature scenarios###
                                   ### Historic temperature scenarios###
                                   ### Historic temperature scenarios###



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

# Let's compute the actual 'observed' chill for comparison
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




                                  ### Future temperature scenarios###
                                  ### Future temperature scenarios###
                                  ### Future temperature scenarios###


getClimateWizardData(coordinates=c(longitude=62.26,latitude=33.63),
                     scenario="rcp45", start_year=2020, end_year=2050,
                     metric=c("CD18","R02"), GCMs=c("bcc-csm1-1","BNU-ESM"))


RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)

for(RCP in RCPs)
  for(Time in Times)
  {start_year <- Time-15
  end_year <- Time+15
  clim_scen <-getClimateWizardData(
    c(longitude = 62.262,latitude = 33.637),
    RCP,
    start_year,
    end_year,
    temperature_generation_scenarios = TRUE,
    baseline =c(1975, 2005),
    metric = "monthly_min_max_temps",
    GCMs = "all")
  save_temperature_scenarios(clim_scen,
                             "D:/Rdata/Chill_quantification/Adraskan/ClimateWizard",
                             paste0("Adraskan_futures_",Time,"_",RCP))}


scenario_1990<-temperature_scenario_from_records(Adraskan_cal,1990)
scenario_2000<-temperature_scenario_from_records(Adraskan_cal,2000)
adjustment_scenario<-temperature_scenario_baseline_adjustment(scenario_2000,scenario_1990)

adjustment_scenario

RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)

for(RCP in RCPs)
  for(Time in Times)
  {
    clim_scen<-load_ClimateWizard_scenarios(
      "D:/Rdata/Chill_quantification/Adraskan/ClimateWizard",
      paste0("Adraskan_futures_",Time,"_",RCP))
    clim_scen_adjusted<-
      temperature_scenario_baseline_adjustment(
        baseline_temperature_scenario=adjustment_scenario,
        temperature_scenario=clim_scen)
    Temps<-temperature_generation(
      weather=Adraskan_cal, 
      years=c(1980,2020),
      sim_years=c(2000,2100),
      temperature_scenario = clim_scen_adjusted)
    
    save_temperature_scenarios(
      Temps,
      "D:/Rdata/Chill_quantification/Adraskan/Weather",
      paste0("Adraskan_",Time,"_",RCP))
  }


all_past_scenarios<-temperature_scenario_from_records(
  weather=Adraskan_cal,
  year=c(1980,1990,2000,2010))

adjusted_scenarios<-temperature_scenario_baseline_adjustment(
  baseline=scenario_2000,
  temperature_scenario = all_past_scenarios)

#run below this
all_past_scenario_temps<-temperature_generation(
  weather=Adraskan_cal,
  years=c(1980,2020),
  sim_years=c(2000,2100),
  temperature_scenario = adjusted_scenarios)

save_temperature_scenarios(
  all_past_scenario_temps,
  "D:/Rdata/Chill_quantification/Adraskan/Weather",
  "Adraskan_historic")

models<-list(Chill_CP=Dynamic_Model,Heat_GDH=GDH)

Temps<-load_temperature_scenarios("D:/Rdata/Chill_quantification/Adraskan/Weather","Adraskan_historic")
chill_past_scenarios<-tempResponse_daily_list(
  Temps,
  latitude=33.637,
  Start_JDay = 305,
  End_JDay = 59,
  models=models,
  misstolerance = 10)
chill_observed<-tempResponse_daily_list(
  Adraskan_cal,
  latitude=33.637,
  Start_JDay = 305,
  End_JDay = 59,
  models=models,
  misstolerance = 10)

save_temperature_scenarios(chill_past_scenarios,
                           "D:/Rdata/Chill_quantification/Adraskan/chill",
                           "Adraskan_historic")
save_temperature_scenarios(chill_observed,
                           "D:/Rdata/Chill_quantification/Adraskan/chill",
                           "Adraskan_observed")


chill_past_scenarios<-load_temperature_scenarios(
  "D:/Rdata/Chill_quantification/Adraskan/chill",
  "Adraskan_historic")
chill_observed<-load_temperature_scenarios(
  "D:/Rdata/Chill_quantification/Adraskan/chill",
  "Adraskan_observed")

chills <-make_climate_scenario(
  chill_past_scenarios,
  caption = "Historic",
  historic_data = chill_observed,
  time_series = TRUE)

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)")


for(RCP in RCPs)
  for(Time in Times)
  {
    Temps<-load_temperature_scenarios(
      "D:/Rdata/Chill_quantification/Adraskan/Weather",
      paste0("Adraskan_",Time,"_",RCP))
    chill<-tempResponse_daily_list(
      Temps,
      latitude=33.637,
      Start_JDay = 305,
      End_JDay = 59,
      models=models,
      misstolerance = 10)
    save_temperature_scenarios(
      chill,
      "D:/Rdata/Chill_quantification/Adraskan/chill",
      paste0("Adraskan_",Time,"_",RCP))
  }


for(RCP in RCPs)
  for(Time in Times)
  {
    chill<-load_temperature_scenarios(
      "D:/Rdata/Chill_quantification/Adraskan/chill",
      paste0("Adraskan_",Time,"_",RCP))
    if(RCP=="rcp45") RCPcaption <- "RCP4.5"
    if(RCP=="rcp85") RCPcaption <- "RCP8.5"
    if(Time=="2050") Time_caption <- "2050"
    if(Time=="2085") Time_caption <- "2085"
    chills <-make_climate_scenario(
      chill,
      caption =c(RCPcaption, Time_caption),
      add_to = chills)
  }

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)",
  texcex=1.5)
#ggsave("D:/Rdata/Chill_quantification/Adraskan/chill/Chill_Plot/Adraskan_Chill_Plot.png", width = 20, height = 10, units = "cm", dpi = 600)


plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Heat_GDH",
  metric_label="Heat (Growing Degree Hours)",
  texcex=1.5)


                                    ### Plotting future scenarios###
                                    ### Plotting future scenarios###
                                    ### Plotting future scenarios###



library(chillR)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(ggpmisc)
library(patchwork)

chill_past_scenarios<-load_temperature_scenarios(
  "D:/Rdata/Chill_quantification/Adraskan/chill",
  "Adraskan_historic")
chill_observed<-load_temperature_scenarios(
  "D:/Rdata/Chill_quantification/Adraskan/chill",
  "Adraskan_observed")

chills <-make_climate_scenario(
  chill_past_scenarios,
  caption = "Historic",
  historic_data = chill_observed,
  time_series = TRUE)

RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)

for(RCP in RCPs)
  for(Time in Times)
  { chill<-load_temperature_scenarios(
    "D:/Rdata/Chill_quantification/Adraskan/chill",
    paste0("Adraskan_",Time,"_",RCP))
  if(RCP=="rcp45") RCPcaption <- "RCP4.5"
  if(RCP=="rcp85") RCPcaption <- "RCP8.5"
  if(Time=="2050") Time_caption <- "2050"
  if(Time=="2085") Time_caption <- "2085"
  chills <-make_climate_scenario(
    chill,
    caption =c(RCPcaption, Time_caption),
    add_to = chills)
  }



# We'll first process the past scenarios (element 1 of the chills list).
# Within the data element, we have a list of multiple data.frames for
# the various past scenarios.
# Using a 'for' loop, we cycle through all these data.frames.

for(nam in names(chills[[1]]$data))
{
  # Extract the data frame.
  ch<-chills[[1]]$data[[nam]]
  # Add columns for the new information we have to add and fill them.
  ch[,"GCM"]<-"none"
  ch[,"RCP"]<-"none"
  ch[,"Year"]<-as.numeric(nam)
  
  # Now check if this is the first time we've gone through this loop.
  # If this is the first time, the ch data.frame becomes the output
  # object (past_simulated).
  # If it is not the first time ('else'), we add the current data.frame
  # to the 'past_simulated' object
  if(nam==names(chills[[1]]$data)[1])
    past_simulated<-ch else
      past_simulated<-rbind(past_simulated,ch)
}

# We add another column called 'Scenario' and label all rows as 'Historic' 
past_simulated["Scenario"] <- "Historic"

kable(past_simulated[1:5,])  %>%
  kable_styling("striped", position = "left",font_size = 8)


# We'll want to add the historic observation too, so let's simplify the
# pointer to this information for easier use later

past_observed <- chills[[1]][["historic_data"]]

kable(past_observed[1:5,])  %>%
  kable_styling("striped", position = "left",font_size = 8)



# Extract future data
for(i in 2:length(chills))
  for(nam in names(chills[[i]]$data))
  {ch<-chills[[i]]$data[[nam]]
  ch[,"GCM"]<-nam
  ch[,"RCP"]<-chills[[i]]$caption[1]
  ch[,"Year"]<-chills[[i]]$caption[2]
  if(i==2&nam==names(chills[[i]]$data)[1])
    future_data<-ch else
      future_data<-rbind(future_data,ch)
  }

kable(future_data[1:5,])  %>%
  kable_styling("striped", position = "left",font_size = 8)



# Extract the model names
#Models <- unique(future_data$GCM)

metric<-"Heat_GDH"
axis_label<-"Heat (in GDH)"

plot_scenarios_gg<-function(past_observed,
                            past_simulated,
                            future_data,
                            metric,
                            axis_label)
{
  rng<-range(past_observed[[metric]],
             past_simulated[[metric]],
             future_data[[metric]])  
  past_plot<-ggplot() +
    geom_boxplot(data = past_simulated,
                 aes_string("as.numeric(Year)",metric,group="Year"),
                 fill="skyblue") +
    scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
    labs(x = "Year", y = axis_label) +
    facet_grid(~ Scenario) +
    theme_bw(base_size = 15) +  
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle=45, hjust=1)) +
    geom_point(data = past_observed,
               aes_string("End_year",metric),
               col="blue")
  
  future_plot_list<-list()
  
  for(y in c(2050,2085))
  {
    future_plot_list[[which(y == c(2050,2085))]] <-
      ggplot(data= future_data[which(future_data$Year==y),]) +
      geom_boxplot(aes_string("GCM", metric, fill="GCM")) +
      facet_wrap(vars(RCP)) +
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) +
      scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
      geom_text_npc(aes(npcx = "center", npcy = "top", label = Year),
                    size = 5) +
      theme_bw(base_size = 15) +
      theme(axis.ticks.y = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.margin = margin(0, 0, 0, 0, "cm"),
            legend.background = element_rect(),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold"),
            legend.box.spacing = unit(0, "cm"),
            plot.subtitle = element_text(hjust = 0.5,
                                         vjust = -1,
                                         size = 15 * 1.05,
                                         face = "bold")) 
  }
  
  plot<- (past_plot +
            future_plot_list +
            plot_layout(guides = "collect",
                        widths = c(1,rep(1.8,length(future_plot_list))))
  ) & theme(legend.position = "bottom",
            legend.text = element_text(size=8),
            legend.title = element_text(size=10),
            axis.title.x=element_blank())
  plot
  
}


plot_scenarios_gg(past_observed=past_observed,
                  past_simulated=past_simulated,
                  future_data=future_data,
                  metric="Chill_CP",
                  axis_label="Chill (in Chill Portions)")

ggsave("D:/Rdata/Chill_quantification/Adraskan/chill/Chill_Plot/Adraskan_Future_CP.png", width = 20, height = 10, units = "cm", dpi = 600)



plot_scenarios_gg(past_observed=past_observed,
                  past_simulated=past_simulated,
                  future_data=future_data,
                  metric="Heat_GDH",
                  axis_label="Heat (in Growing Degree Hours)")

ggsave("D:/Rdata/Chill_quantification/Adraskan/chill/Chill_Plot/Adraskan_Future_GDH.png", width = 20, height = 10, units = "cm", dpi = 600)


                           ### Future temperature scenarios using SSP###
                           ### Future temperature scenarios using SSP###
                           ### Future temperature scenarios using SSP###

require(chillR)
require(LarsChill)
require(tidyverse)
require(lubridate)
devtools::install_github("larscaspersen/addition_chillR")

########################
##load weather station data
########################

#in my code I worked with two weather stations, called Durham (du) and Modesto (mo)
#you will probably need to automatize some parts here, maybe use a loop or something


#read weather data
#this is the patched daily weather data from the weather stations
durham <- read.csv('data/weather_data/target_weather/combined_3_cimis_12_1948-2022.csv')
modesto <- read.csv('data/weather_data/target_weather/combined_2_724926_23258.csv')

#bind them together in a list
weather_list <- list(durham, modesto)



# Loading temperature data (all stations)
Station_AT_List <-  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs_Sim", prefix = "1980_2020_")

#Assigning the names
List_Names <- list.files("D:/Rdata/QDM/Stations_Obs")
Stations_Names <- str_split(List_Names, pattern = "_") %>% 
  purrr::map_chr(1)
names(Station_AT_List) <- Stations_Names

Station_Coordinates <- read.csv("D:/Rdata/Chill_quantification/Coordinates.csv")


pb = txtProgressBar(min = 0, max = length(Station_AT_List), initial = 0, style = 3) 


downloaded_SSP <-  LarsChill::get_scenarioMIP_data(coordinates = data.frame('Longitude' = Station_Coordinates$Longitude,
                                                                        'Latitude' = Station_Coordinates$Latitude,
                                                                        'id' = Station_Coordinates$ID),
                                               start_year = 2015,
                                               end_year = 2100,
                                               metric = c('tasmax', 'tasmin'),
                                               experiment = c('ssp126', 'ssp245', 'ssp370', 'ssp585'),
                                               keep_downloaded = TRUE)

save_temperature_scenarios(downloaded, "D:/Rdata/Chill_quantification/Future_scenarios", prefix = "SSP")

#re-organize the extracted weather data
downloaded_list <- downloaded %>% 
  #bring stations in long format
  reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'station') %>% 
  #bring individual weather variables as columns
  reshape2::dcast(Date + model + ssp + station ~ variable, value.var = 'value') %>% 
  #add Year, Month, Day
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date),
         Day = lubridate::day(Date)) %>% 
  rename(Tmin = tasmin,
         Tmax = tasmax) %>% 
  #split into list of weather stations
  split(f = list(.$station, .$ssp, .$model))


#you maybe want to save the data.frames to be on the safe side
#for that we use the chillR function 'save_temperature_scenarios'
#I commented this out, because I wanted to avoid accidentally changing the files
#in a second step I am reading the data again

#save
#save_temperature_scenarios(downloaded_list, path = 'data/weather_data/extracted_climatechange_weather/cimis/', prefix = 'CC_download')

#read files again
downloaded_list <- load_temperature_scenarios('data/weather_data/extracted_climatechange_weather/cimis/', prefix = 'CC_download')



#the problem is now, that the read data is a bit messy
#we need to know which weather station, ssp and gcm we are having (the informations was saved
#in the file names)
#so we need to extract this infórmation from the file names

#these are the two different points in time we are interested in from the gcm output
Times <- c(2050, 2085)

#I want to make sure that the weather_list (the list which contains the original
#local weather observation is named, so that it is easier to match original
#observations to the downloaded projected monthly observations)
names(weather_list) <- weather_station$chillR_code


########
#generate climate informed weather data with weather generator
#######

#this will probably take a while in your case
#if it is really slow you might wanna look into ways to 
#optimze the code (make it faster)
#generally, R-for loops are regarded to be pretty slow, so maybe you might wanna turn the 
#code within the loop into a function and use 'lapply' on the downloaded list with your
#custom function to increase speed. 
#I also heard the data.tables are more efficient than data.frames, you can use them 
#via the 'data.table' package. But you need to watch out, I think the subsetting rules
#are a bit different, there are less exceptional cases than in data.frame, which I may rely
#on because I am not a great code writer. 


#iterate over all downloaded climate change files
for(j in 1:length(downloaded_list)){
  
  #extract information from the items name
  split_names <- str_split(names(downloaded_list)[j], '\\.')
  station_name <- split_names[[1]][1]
  ssp <- split_names[[1]][2]
  gcm_model <- split_names[[1]][3]
  
  #mathc the station name with the data.frame which contains the coordinates and ids
  i <- which(weather_station$chillR_code == station_name)
  
  #sometimes the download failed or the files are empty, in this case ignore the 
  #element and go to the next item on the list
  if(nrow(downloaded_list[[j]]) == 0){
    next()
  }
  
  #if the download was for some models incomplete, then take the next one
  if(any(is.na(downloaded_list[[j]]$Tmax) |
         is.na(downloaded_list[[j]]$Tmin))){
    next()
  }
  
  #iterate over all times
  for(Time in Times){
    
    #the next part of the code is to handle the downscaling problem of the climate change data
    #my work-around is to calculate the change in Tmin and Tmax relative to the beginning of the
    #model output
    #--> I calculate running mean around 2022 of the gcm ouput as the "baseline"
    #and a running mean around the points in time we are interested (2050 and 2085)
    #maybe you wanna changed the window-width of the running mean for the baseline
    #or the central point. everything was a but rushed when I wrote the code 
    #because the deadliune was so pressing
    
    #extract the scenario information from the climate change data
    clim_senc <- temperature_scenario_from_records(weather = downloaded_list[[j]], 
                                                   runn_mean = 15,
                                                   year = 2022)
    
    
    #we also calculate running mean of monthly data for the point of time we are interested
    clim_senc_later <- temperature_scenario_from_records(weather = downloaded_list[[j]], 
                                                         runn_mean = 31,
                                                         year = Time)
    
    #here I tried to minick the output of the scenario data.frame we calculated earlier,
    #just that we have information of change to baseline instead of absolute
    #data is a data.frame that contains the change in monthly mean Tmin and Tmax
    #baseline - climate change
    #reference_year needs to be the centroid of the time period covered by your local
    #weather data, so it is probably different in your case
    
    
    #take the difference of the observations at around 2022 and the climate change scenario
    clim_scen_adjusted <- list(list(data =  clim_senc_later[[1]]$data - clim_senc[[1]]$data,
                                    scenario = ssp,
                                    start_year = Time - 15,
                                    end_year = Time + 15,
                                    scenario_year = Time,
                                    reference_year = 2003.5,
                                    scenario_type = 'relative',
                                    labels = gcm_model))
    
    #this function runs the weather generator
    temps <- temperature_generation(weather = weather_list[[i]],
                                    years = c(1985, 2022), 
                                    sim_years = c(2000, 2100),
                                    temperature_scenario = clim_scen_adjusted,
                                    temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
    
    #create a file name for the generated weather data
    #make sure that this path exists, 
    #probably you need to create a new folder (in which you wanna save all the generated weather data)
    fname <- paste0('data/weather_data/future_sim_weather_cmip6/cimis/',
                    i, '_', station_name, '_', ssp, '_', Time, '_', gcm_model,
                    '.csv')
    
    #save the generated weather data
    write.csv(temps, fname, row.names = FALSE)
    
    
  }
}
#at the end of the loop you should have a huge set of generated weather data
#you can read this data in a new script and process it to calculate the chill portions
#good luck :)

