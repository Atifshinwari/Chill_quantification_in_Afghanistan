
#------------------------------------------------------------------------------------------#
# Chill Quantification for Afghanistan: Future temperature scenarios using SSP
#------------------------------------------------------------------------------------------#                      ### Chill Quantification for Afghanistan ###

require(chillR)
require(ggplot2)
require(LarsChill)
require(tidyverse)
require(lubridate)
require(magrittr)
require(dplyr)
require(kableExtra)
require(Metrics)
require(zoo)
require(latticeExtra)
require(stringr)
library(reshape2)
library(ggpmisc)
library(patchwork)
#devtools::install_github("larscaspersen/addition_chillR")

#------------------------------------------------------------------------------------------#
# Downloading scenarios
#------------------------------------------------------------------------------------------#  

#Loading temperature data (all stations)
Station_AT_List <-
  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs_Sim", prefix = "1980_2020_")

#Assigning the names
List_Names <- list.files("D:/Rdata/QDM/Stations_Obs")
Stations_Names <- str_split(List_Names, pattern = "_") %>%
  purrr::map_chr(1)
names(Station_AT_List) <- Stations_Names

# Chosing the same length of time for all stations (1980 to 2020)

Station_coordinates <-
  read.csv("D:/Rdata/Chill_quantification/Coordinates.csv")

source('helper_functions.R')

# The function does not work for me so I downloaded these scenarios in Posit Cloud
pb = txtProgressBar(
  min = 0,
  max = length(Station_AT_List),
  initial = 0,
  style = 3
)

test <-
  LarsChill::get_scenarioMIP_data(
    coordinates = data.frame(
      'Longitude' = Station_coordinates$Longitude,
      'Latitude' = Station_coordinates$Latitude,
      'id' = Station_coordinates$id
    ),
    start_year = 2015,
    end_year = 2100,
    metric = c('tasmax', 'tasmin'),
    experiment = c('ssp126', 'ssp245', 'ssp370', 'ssp585'),
    keep_downloaded = TRUE
  )

# I am reading the scenarios, I download 10 in each download

downloaded_SSP10 <-
  read.csv("D:/Rdata/Chill_quantification/Future_scenarios/downloaded_SSP10.csv")
downloaded_SSP20 <-
  read.csv("D:/Rdata/Chill_quantification/Future_scenarios/downloaded_SSP20.csv")
downloaded_SSP30 <-
  read.csv("D:/Rdata/Chill_quantification/Future_scenarios/downloaded_SSP30.csv")
downloaded_SSP40 <-
  read.csv("D:/Rdata/Chill_quantification/Future_scenarios/downloaded_SSP40.csv")
downloaded_SSP51 <-
  read.csv("D:/Rdata/Chill_quantification/Future_scenarios/downloaded_SSP51.csv")

# Now combining them into one dataframe

downloaded_SSP <-
  cbind(
    downloaded_SSP10[, 1:15],
    downloaded_SSP20[,-(1:5)],
    downloaded_SSP30[,-(1:5)],
    downloaded_SSP40[,-(1:5)],
    downloaded_SSP51[,-(1:5)]
  )

# Clean the environment
rm(
  downloaded_SSP10,
  downloaded_SSP20,
  downloaded_SSP30,
  downloaded_SSP40,
  downloaded_SSP51
)

# Remove the first unwanted column
downloaded_SSP <- subset(downloaded_SSP, select = -X)

#bring in right format
downloaded_list <- format_downloaded_ssp(downloaded_SSP)

# Since there is issue with files name has dots, its not picking those so
# I reapply the names
names(downloaded_list) <- Stations_Names

# only take downloaded data, for which I have local observations
downloaded_list <- downloaded_list[names(Station_AT_List)]

# extract relative change scenarios and put them in a list
# list has as many elements as you have weather stations
# each element contains another list with the different scenarios (ssp, gcm, time)

# controls which years go into the weather generator for calibration
min_year <- 1980
max_year <- 2020

scenario_list <-
  gen_rel_change_scenario(downloaded_list,
                          Station_AT_List,
                          years_local_weather = c(min_year, max_year))
save_temperature_scenarios(scenario_list, path = "D:/Rdata/Chill_quantification/Future_scenarios/SSP_station_scenarios", prefix = "SSP_")


#------------------------------------------------------------------------------------------#
# # Generating temperature using weather generator#
#------------------------------------------------------------------------------------------#                      ### Chill Quantification for Afghanistan ###

####### First 25 Station #####

# Here I am choosing few stations to divide the downloading process into 3 or 4 phases
scenario_list_S1S25 <- scenario_list[c(1:25)]

# The same I will do for weather data from stations
Station_AT_List_S1S25 <- Station_AT_List[c(1:25)]

# run weather generator using the custom scenarios
for (i in 1:length(scenario_list_S1S25)) {
  for (j in 1:length(scenario_list_S1S25[[i]])) {
    station <- names(scenario_list_S1S25)[i]
    scenario <- names(scenario_list_S1S25[[i]])[j]
    
    generated_temp_S1S25 <-
      temperature_generation(
        weather = Station_AT_List_S1S25[[station]],
        years = c(min_year, max_year),
        sim_years = c(2000, 2100),
        temperature_scenario = list(scenario_list_S1S25[[i]][[j]])
      )
    
    f.name <-
      paste0(
        'D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6/',
        station,
        '_',
        scenario,
        '.csv'
      )
    
    write.csv(generated_temp_S1S25[[1]],
              file = f.name,
              row.names = F)
  }
}


####Generating temperature####
####### Remain 26 Station #####

# Here I am choosing few stations to divide the downloading process into 3 or 4 phases
scenario_list_S26S51 <- scenario_list[c(26:51)]

# The same I will do for weather data from stations
Station_AT_List_S26S51 <- Station_AT_List[c(26:51)]

# run weather generator using the custom scenarios
for (i in 1:length(scenario_list_S26S51)) {
  for (j in 1:length(scenario_list_S26S51[[i]])) {
    station <- names(scenario_list_S26S51)[i]
    scenario <- names(scenario_list_S26S51[[i]])[j]
    
    generated_temp_S26S52 <-
      temperature_generation(
        weather = Station_AT_List_S26S51[[station]],
        years = c(min_year, max_year),
        sim_years = c(2000, 2100),
        temperature_scenario = list(scenario_list_S26S51[[i]][[j]])
      )
    
    f.name <-
      paste0(
        'D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6/',
        station,
        '_',
        scenario,
        '.csv'
      )
    
    write.csv(generated_temp_S26S52[[1]],
              file = f.name,
              row.names = F)
  }
}



###This took quite a while, but now we have all the future temperature data
###we need for our scenario analysis, and all of this is saved to disk.
###So we don’t have to run this code again.

###Let’s add some historic scenarios though. This is similar to what we’ve done before.
###I’ll make scenarios corresponding to 1980, 1990, 2000 and 2010.


#------------------------------------------------------------------------------------------#
# Test for one station Adraskan and small data# 
#------------------------------------------------------------------------------------------#                      ### Chill Quantification for Afghanistan ###

# these are the two different points in time we are interested in from the gcm output

#SSPs<-c("ssp126","ssp245", "ssp370", "ssp585")
SSPs <- c("ssp245", "ssp585")
Times <- c(2050, 2085)
#SSP<-c("ssp126","ssp245", "ssp370", "ssp585")
SSP <- c("ssp245", "ssp585")
Time <- c(2050, 2085)

###########################Here adding past scenarios################################


scenario_2000 <-
  temperature_scenario_from_records(Station_AT_List$Adraskan, 2000)

all_past_scenarios <- temperature_scenario_from_records(weather = Station_AT_List$Adraskan,
                                                        year = c(1980, 1990, 2000, 2010, 2020))

adjusted_scenarios <- temperature_scenario_baseline_adjustment(baseline =
                                                                 scenario_2000,
                                                               temperature_scenario = all_past_scenarios)

all_past_scenario_temps <- temperature_generation(
  weather = Station_AT_List$Adraskan,
  years = c(1980, 2020),
  sim_years = c(2000, 2100),
  temperature_scenario = adjusted_scenarios)

save_temperature_scenarios(
  all_past_scenario_temps,
  "D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6",
  "Adraskan_historic")

# read back
all_past_scenario_temps <- load_temperature_scenarios(
  "D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6",
  "Adraskan_historic")


### Now all we have to do is follow the steps we used to make our historic scenarios.
### The tempResponse_daily_list function makes this reasonably easy.
### Let’s first make a list of models we want to apply - I’m choosing the Dynamic Model
### for chill, the Growing Degree Hours model for heat, and a frost model here:

frost_model <- function(x)
  step_model(x, data.frame(
    lower = c(-1000, 0),
    upper = c(0, 1000),
    weight = c(1, 0)))

models <-
  list(Chill_CP = Dynamic_Model,
       Heat_GDH = GDH,
       Frost_H = frost_model)


### Now let’s first apply these models to the historic data, both the scenarios and
### the observed temperatures:


Temps <-
  load_temperature_scenarios(
    "D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6",
    "Adraskan_historic")
chill_past_scenarios <- tempResponse_daily_list(
  Temps,
  latitude = 33.637,
  Start_JDay = 305,
  End_JDay = 59,
  models = models,
  misstolerance = 10)
chill_observed <- tempResponse_daily_list(
  Station_AT_List$Adraskan,
  latitude = 33.637,
  Start_JDay = 305,
  End_JDay = 59,
  models = models,
  misstolerance = 10)

save_temperature_scenarios(
  chill_past_scenarios,
  "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill",
  "Adraskan_historic")
save_temperature_scenarios(
  chill_observed,
  "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill",
  "Adraskan_observed")


### Load them back if needed for plotting both the scenarios and historic chill,
### however we will be plotting lateron

chill_past_scenarios <- load_temperature_scenarios(
  "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill",
  "Adraskan_historic")
chill_observed <- load_temperature_scenarios(
  "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill",
  "Adraskan_observed")

chills <- make_climate_scenario(
  chill_past_scenarios,
  caption = "Historic",
  historic_data = chill_observed,
  time_series = TRUE)


### Now we run through the same process for all the future climate scenarios.
### For each one, we add the climate scenario to the
### chills object (make_climate_scenario has an argument add_to,
### where we can specify that):

for (SSP in SSPs) {
  for (Time in Times) {
    Temps <- load_temperature_scenarios(
      "D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6",
      paste0("Adraskan_", SSP, "_", Time))
    
    # Assigning names to the data frames
    names(Temps) <-
      c("BCC-CSM2-MR","GFDL-ESM4","INM-CM4-8","INM-CM5-0","MRI-ESM2-0")
    
    chill <- tempResponse_daily_list(
      Temps,
      latitude = 33.637,
      Start_JDay = 305,
      End_JDay = 59,
      models = models,
      misstolerance = 10)
    
    save_temperature_scenarios(
      chill,
      "D:/Rdata/Chill_quantification/Future_scenarios/chill/future_chill",
      paste0("Adraskan_", SSP, "_", Time))
  }
}


### We now load this again, make climate scenario the same way we did for
### the historic data, and add them to our chills list so that we can easily plot them

SSPs <- c("ssp245", "ssp585")
Times <- c(2050, 2085)
GCM <- c("BCC-CSM2-MR","GFDL-ESM4","INM-CM4-8","INM-CM5-0","MRI-ESM2-0")

for (SSP in SSPs)
  for (Time in Times)
  {
    chill <- load_temperature_scenarios(
      "D:/Rdata/Chill_quantification/Future_scenarios/chill/future_chill",
      paste0("Adraskan_", SSP, "_", Time))
    #if(SSP=="ssp126") SSPcaption <- "SSP126"
    if (SSP == "ssp245")
      SSPcaption <- "SSP245"
    #if(SSP=="ssp370") SSPcaption <- "SSP370"
    if (SSP == "ssp585")
      SSPcaption <- "SSP585"
    if (Time == "2050")
      Time_caption <- "2050"
    if (Time == "2085")
      Time_caption <- "2085"
    chills <- make_climate_scenario(chill,
                                    caption = c(SSPcaption, Time_caption),
                                    add_to = chills)
  }



# We'll first process the past scenarios (element 1 of the chills list).
# Within the data element, we have a list of multiple data.frames for
# the various past scenarios.
# Using a 'for' loop, we cycle through all these data.frames.

for (nam in names(chills[[1]]$data))
{
  # Extract the data frame.
  ch <- chills[[1]]$data[[nam]]
  # Add columns for the new information we have to add and fill them.
  ch[, "GCM"] <- "none"
  ch[, "SSP"] <- "none"
  ch[, "Year"] <- as.numeric(nam)
  
  # Now check if this is the first time we've gone through this loop.
  # If this is the first time, the ch data.frame becomes the output
  # object (past_simulated).
  # If it is not the first time ('else'), we add the current data.frame
  # to the 'past_simulated' object
  if (nam == names(chills[[1]]$data)[1])
    past_simulated <- ch
  else
    past_simulated <- rbind(past_simulated, ch)
}

# We add another column called 'Scenario' and label all rows as 'Historic'
past_simulated["Scenario"] <- "Historic"

# We'll want to add the historic observation too, so let's simplify the
# pointer to this information for easier use later

past_observed <- chills[[1]][["historic_data"]]


# Extract future data
for (i in 2:length(chills))
  for (nam in names(chills[[i]]$data))
  {
    ch <- chills[[i]]$data[[nam]]
    ch[, "GCM"] <- nam
    ch[, "SSP"] <- chills[[i]]$caption[1]
    ch[, "Year"] <- chills[[i]]$caption[2]
    if (i == 2 & nam == names(chills[[i]]$data)[1])
      future_data <- ch
    else
      future_data <- rbind(future_data, ch)
  }



### Plotting ####
### Plotting ####
### Plotting ####

# First read the plot_function_scenario_gg function:

# Chill plot
plot_scenarios_gg(
  past_observed = past_observed,
  past_simulated = past_simulated,
  future_data = future_data,
  metric = "Chill_CP",
  axis_label = "Chill (in Chill Portions)")

ggsave(
  "D:/Rdata/Chill_quantification/Adraskan/chill/Chill_Plot/Adraskan_Future_CP.png",
  width = 20,
  height = 10,
  units = "cm",
  dpi = 600)


# GDH plot
plot_scenarios_gg(
  past_observed = past_observed,
  past_simulated = past_simulated,
  future_data = future_data,
  metric = "Heat_GDH",
  axis_label = "Heat (in Growing Degree Hours)")

ggsave(
  "D:/Rdata/Chill_quantification/Adraskan/chill/Chill_Plot/Adraskan_Future_GDH.png",
  width = 20,
  height = 10,
  units = "cm",
  dpi = 600)


# Frost plot
plot_scenarios_gg(
  past_observed = past_observed,
  past_simulated = past_simulated,
  future_data = future_data,
  metric = "Frost_H",
  axis_label = "Heat (in Growing Degree Hours)")

ggsave(
  "D:/Rdata/Chill_quantification/Adraskan/chill/Chill_Plot/Adraskan_Future_Frost.png",
  width = 20,
  height = 10,
  units = "cm",
  dpi = 600)



#------------------------------------------------------------------------------------------#
# Loop for all the stations # 
#------------------------------------------------------------------------------------------# 

#Loading temperature data (all stations)
Station_AT_List <-
  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs_Sim", prefix = "1980_2020_")

#Assigning the names
List_Names <- list.files("D:/Rdata/QDM/Stations_Obs")
Stations_Names <- str_split(List_Names, pattern = "_") %>%
  purrr::map_chr(1)
names(Station_AT_List) <- Stations_Names

Station_Latitudes <- c(33.63, 36.01, 34.91, 35.14, 36.10, 37.00, 34.82, 34.52, 35.72, 34.23, 36.16, 31.13, 35.31, 36.41, 34.54, 35.96, 35.60, 36.73,34.82, 37.11, 32.36, 34.50, 33.59, 35.28, 36.93, 35.55, 35.96, 31.58, 33.35, 34.81, 31.61, 36.18, 34.74, 36.63, 36.80, 36.73, 34.44, 33.55, 34.34, 34.54, 35.67, 36.58, 36.53, 34.68, 34.33, 36.05, 34.40, 36.65, 32.63, 35.25, 34.22)

SSPs<-c("ssp126","ssp245", "ssp370", "ssp585")
Times <- c(2050, 2085)
SSP<-c("ssp126","ssp245", "ssp370", "ssp585")
Time <- c(2050, 2085)

###########################Here adding past scenarios################################


for (i in 1: length(Station_AT_List)) {
  
  Station_List <- Station_AT_List[[i]]
  Latitudes <- Station_Latitudes[[i]]
  
  
scenario_2000 <-
  temperature_scenario_from_records(Station_List, 2000)

all_past_scenarios <- temperature_scenario_from_records(weather = Station_List,
                                                        year = c(1980, 1990, 2000, 2010, 2020))

adjusted_scenarios <- temperature_scenario_baseline_adjustment(baseline =
                                                                 scenario_2000,
                                                               temperature_scenario = all_past_scenarios)

all_past_scenario_temps <- temperature_generation(
  weather = Station_List,
  years = c(1980, 2020),
  sim_years = c(2000, 2100),
  temperature_scenario = adjusted_scenarios)


save_temperature_scenarios(
  all_past_scenario_temps,
  "D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6",
  paste0(Stations_Names[i], "_historic"))


# # Read the temperature scenarios when needed
# file_path <- "D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6"
# file_name <- paste0(Stations_Names[i], "_historic")
# all_past_scenario_temps <- load_temperature_scenarios(file_path, file_name)

### Now all we have to do is follow the steps we used to make our historic scenarios.
### The tempResponse_daily_list function makes this reasonably easy.
### Let’s first make a list of models we want to apply - I’m choosing the Dynamic Model
### for chill, the Growing Degree Hours model for heat, and a frost model here:

frost_model <- function(x)
  step_model(x, data.frame(
    lower = c(-1000, 0),
    upper = c(0, 1000),
    weight = c(1, 0)))

models <-
  list(Chill_CP = Dynamic_Model,
       Heat_GDH = GDH,
       Frost_H = frost_model)


### Now let’s first apply these models to the historic data, both the scenarios and
### the observed temperatures:

Temps <- load_temperature_scenarios(
  "D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6",
  paste0(Stations_Names[i], "_historic"))

chill_past_scenarios <- tempResponse_daily_list(
  Temps,
  latitude = Latitudes,
  Start_JDay = 305,
  End_JDay = 59,
  models = models,
  misstolerance = 10)
chill_observed <- tempResponse_daily_list(
  Station_List,
  latitude = Latitudes,
  Start_JDay = 305,
  End_JDay = 59,
  models = models,
  misstolerance = 10)

save_temperature_scenarios(
  chill_past_scenarios,
  "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill/",
  paste0(Stations_Names[i], "_historic"))


save_temperature_scenarios(
  chill_observed,
  "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill/",
  paste0(Stations_Names[i], "_observed"))

### Load them back if needed for plotting both the scenarios and historic chill,
### however we will be plotting lateron

# file_path2 <- "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill/"
# file_name2 <- paste0(Stations_Names[i], "_historic")
# file_name3 <- paste0(Stations_Names[i], "_observed")
# 
# chill_past_scenarios <- load_temperature_scenarios(file_path2, file_name2)
# chill_observed <- load_temperature_scenarios(file_path2, file_name3)

chills <- make_climate_scenario(
  chill_past_scenarios,
  caption = "Historic",
  historic_data = chill_observed,
  time_series = TRUE)


### Now we run through the same process for all the future climate scenarios.
### For each one, we add the climate scenario to the
### chills object (make_climate_scenario has an argument add_to,
### where we can specify that):

for (SSP in SSPs) {
  for (Time in Times) {
        Temps <- load_temperature_scenarios(
      "D:/Rdata/Chill_quantification/Future_scenarios/Generated_weather_cmip6",
      paste0(Stations_Names[i],"_", SSP, "_", Time))
    
    # Assigning names to the data frames
    names(Temps) <-
      c("BCC-CSM2-MR","GFDL-ESM4","INM-CM4-8","INM-CM5-0","MRI-ESM2-0")

    chill <- tempResponse_daily_list(
      Temps,
      latitude = Latitudes,
      Start_JDay = 305,
      End_JDay = 59,
      models = models,
      misstolerance = 10)
    
    save_temperature_scenarios(
      chill,
      "D:/Rdata/Chill_quantification/Future_scenarios/chill/future_chill",
      paste0(Stations_Names[i], "_", SSP, "_", Time))
  }
}


###We now load this again, make climate scenario the same way we did for
###the historic data, and add them to our chills list so that we can easily plot them

SSPs <- c("ssp126", "ssp245", "ssp370", "ssp585")
Times <- c(2050, 2085)
GCM <- c("BCC-CSM2-MR","GFDL-ESM4","INM-CM4-8","INM-CM5-0","MRI-ESM2-0")

for (SSP in SSPs)
  for (Time in Times)
  {
    chill <- load_temperature_scenarios(
      "D:/Rdata/Chill_quantification/Future_scenarios/chill/future_chill",
      paste0(Stations_Names[i],"_", SSP, "_", Time))
    if (SSP == "ssp126") SSPcaption <- "SSP126"
    if (SSP == "ssp245") SSPcaption <- "SSP245"
    if (SSP == "ssp370") SSPcaption <- "SSP370"
    if (SSP == "ssp585") SSPcaption <- "SSP585"
    if (Time == "2050") Time_caption <- "2050"
    if (Time == "2085") Time_caption <- "2085"
    chills <- make_climate_scenario(chill,
                                    caption = c(SSPcaption, Time_caption),
                                    add_to = chills)
  }



# We'll first process the past scenarios (element 1 of the chills list).
# Within the data element, we have a list of multiple data.frames for
# the various past scenarios.
# Using a 'for' loop, we cycle through all these data.frames.

for (nam in names(chills[[1]]$data))
{
  # Extract the data frame.
  ch <- chills[[1]]$data[[nam]]
  # Add columns for the new information we have to add and fill them.
  ch[, "GCM"] <- "none"
  ch[, "SSP"] <- "none"
  ch[, "Year"] <- as.numeric(nam)
  
  # Now check if this is the first time we've gone through this loop.
  # If this is the first time, the ch data.frame becomes the output
  # object (past_simulated).
  # If it is not the first time ('else'), we add the current data.frame
  # to the 'past_simulated' object
  if (nam == names(chills[[1]]$data)[1])
    past_simulated <- ch
  else
    past_simulated <- rbind(past_simulated, ch)
}

# We add another column called 'Scenario' and label all rows as 'Historic'
past_simulated["Scenario"] <- "Historic"

# kable(past_simulated[1:5,])  %>%
#   kable_styling("striped", position = "left",font_size = 8)
write.csv(past_simulated, 
          file = paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics/", 
                        Stations_Names[i], "_past_simulated.csv"), row.names = FALSE)
## Read back when needed
# file_path3 <- "D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics/"
# file_name4 <- paste0(Stations_Names[i], "_past_simulated.csv")
# past_simulated <- read.csv(paste0(file_path3, file_name4))

# We'll want to add the historic observation too, so let's simplify the
# pointer to this information for easier use later

past_observed <- chills[[1]][["historic_data"]]

# kable(past_observed[1:5,])  %>%
#   kable_styling("striped", position = "left",font_size = 8)
write.csv(past_observed, 
          file = paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics/", 
                        Stations_Names[i], "_past_observed.csv"), row.names = FALSE)

## Read back when needed
# file_path4 <- "D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics/"
# file_name5 <- paste0(Stations_Names[i], "_past_observed.csv")
# past_observed <- read.csv(paste0(file_path4, file_name5))


# Extract future data
for (c in 2:length(chills))
  for (nom in names(chills[[c]]$data))
  {
    ch <- chills[[c]]$data[[nom]]
    ch[, "GCM"] <- nom
    ch[, "SSP"] <- chills[[c]]$caption[1]
    ch[, "Year"] <- chills[[c]]$caption[2]
    if (c == 2 & nom == names(chills[[c]]$data)[1])
      future_data <- ch
    else
      future_data <- rbind(future_data, ch)
  }

# kable(future_data[1:5,])  %>%
#   kable_styling("striped", position = "left",font_size = 8)

write.csv(future_data, 
          file = paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics/", 
                        Stations_Names[i], "_future_data.csv"), row.names = FALSE)


}


#------------------------------------------------------------------------------------------#
# Plotting # 
#------------------------------------------------------------------------------------------# 

#Loading temperature data (all stations)
Station_AT_List <-
  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs_Sim", prefix = "1980_2020_")

#Assigning the names

for (i in 1: length(Stations_Names)) {
  
  Station_name <- Stations_Names[i]

past_simulated <- read.csv(paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics/", 
                                        Station_name, "_past_simulated.csv"))



past_observed <- read.csv(paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics/", 
                                       Station_name, "_past_observed.csv"))



future_data <- read.csv(paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics/", 
                                       Station_name, "_future_data.csv"))


### Plotting ####

# First read the plot_function_scenario_gg function:

# Chill plot
chill_plot <- plot_all_scenarios_gg(
  past_observed = past_observed,
  past_simulated = past_simulated,
  future_data = future_data,
  metric = "Chill_CP",
  axis_label = "Chill (in Chill Portions)")

ggsave(plot = chill_plot, filename = paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/chill_plots/CP_", Stations_Names[i],".png"), height = 13, width = 22, units = "cm", dpi = 600 )


# GDH plot
GDH_plot <- plot_all_scenarios_gg(
  past_observed = past_observed,
  past_simulated = past_simulated,
  future_data = future_data,
  metric = "Heat_GDH",
  axis_label = "Heat (in Growing Degree Hours)")

ggsave(plot = GDH_plot, filename = paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/GDH_plots/GDH_", Stations_Names[i],".png"), height = 13, width = 22, units = "cm", dpi = 600 )


# Frost plot
Frost_plot <- plot_all_scenarios_gg(
  past_observed = past_observed,
  past_simulated = past_simulated,
  future_data = future_data,
  metric = "Frost_H",
  axis_label = "Frost duration (in hours)")

ggsave(plot = Frost_plot, filename = paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/Frost_plots/Frost_", Stations_Names[i],".png"), height = 13, width = 22, units = "cm", dpi = 600 )

}


# Annotate chill plots for their station names

current_names <- c("CP_Qaisar", "CP_DaraiZhwandon", "CP_RabatiBala", "CP_TangiTashqurghan", "CP_Chichakto", "CP_Sayad", "CP_Delmarogh", 
                   "CP_NazdikiKeshandeh", "CP_Dawlatabad", "CP_KheshtPul", "CP_TangiNahrin", "CP_Faizabad", "CP_Baharak", "CP_PuliBangi", 
                   "CP_Khenjan", "CP_Doshi", "CP_NazdikiTaluqan", "CP_Keshem", "CP_PuliAlchin", "CP_Baghlan", "CP_Anjuman", "CP_Eshkashem",
                   "CP_Gardiz", "CP_Maton", "CP_Asmar", "CP_Nawabad", "CP_PuliQarghayi", "CP_PuliBehsod", "CP_Dakah", "CP_Tirin", "CP_NazdikiKandahar",
                   "CP_Lashkargah", "CP_Darwishan", "CP_TagabiGhaza", "CP_Adraskan", "CP_PuliHashemi", "CP_Torghundi", "CP_Farah", "CP_Estalef",
                   "CP_BaghiOmomi", "CP_TangiSayedan", "CP_DashteiSafid", "CP_Shakardara", "CP_PuliGhazni", "CP_Keraman", "CP_Cheghcheran",
                   "CP_Bamyan", "CP_Dawlatyar", "CP_NazdikiNayak", "CP_Waras", "CP_Gardandiwal")


new_annotations <- c("Qaisar (QSR)", "Dara-i-Zhwandon (DZN)", "Rabat-i-Bala (RBA)", "Tang-i-Tashqurghan (TTN)", "Chichakto (CCO)", 
                     "Sayad (SYD)", "Delmarogh (DMH)", "Nazdik-i-Keshandeh (NKH)", "Dawlatabad (DBD)", "Khesht-Pul (KPL)", 
                     "Tang-i-Nahrin (TNN)", "Faizabad (FZD)", "Baharak (BRK)", "Pul-i-Bangi (PBI)", "Khenjan (KJN)", 
                     "Doshi (DSI)", "Nazdik-i-Taluqan (NTN)", "Keshem (KSM)", "Pul-i-Alchin (PAN)", "Baghlan (BLN)", 
                     "Anjuman (AJN)", "Eshkashem (EKM)", "Gardiz (GDZ)", "Maton (MTN)", "Asmar (ASR)", "Nawabad (NBD)", 
                     "Pul-i-Qarghayi (PQI)", "Pul-i-Behsod (PBD)", "Dakah (DKH)", "Tirin (TRN)", "Nazdik-i-Kandahar (NKR)", 
                     "Lashkargah (LGH)", "Darwishan (DSN)", "Tagab-i-Ghaza (TGA)", "Adraskan (ADN)", "Pul-i-Hashemi (PHI)", 
                     "Torghundi (TGI)", "Farah (FRH)", "Estalef (ETF)", "Bagh-i-Omomi (BOI)", "Tang-i-Sayedan (TSN)", 
                     "Dashte-i-Safid (DSD)", "Shakardara (SDA)", "Pul-i-Ghazni (PGI)", "Keraman (KRN)", "Cheghcheran (CCN)", 
                     "Bamyan (BYN)", "Dawlatyar (DYR)", "Nazdik-i-Nayak (NNK)", "Waras (WRS)", "Gardandiwal (GDL)")


# Load required packages
# library(ggplot2)
# library(png)

for (i in seq_along(current_names)) {
  # Read the saved plot
  chill_plot <- readPNG(paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/chill_plots/", current_names[i], ".png"))
  
  # Convert the image to a ggplot object
  chill_plot <- ggplot() +
    annotation_raster(chill_plot, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    xlab("") + ylab("") +  # Remove x and y axis labels
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(t = 10, r = 0, b = -20, l = -20)) # Set plot margins
  
  # Add annotation with the new name
  chill_plot <- chill_plot +
    annotate("text", x = 0.5, y = 1, label = new_annotations[i], hjust = 0.5, vjust = -17.07, size = 3, fontface = "bold") + # old parameter were y = 1, size = 5, vjust 15.8 other same and ggsave size 13,22
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          xlab = NULL, # Remove x-axis label
          ylab = NULL) # Remove y-axis label
  
  # Save the annotated plot
  ggsave(plot = chill_plot, filename = paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/annotated_chill_plots/test_resized_", current_names[i], ".png"), height = 8, width = 14, units = "cm", dpi = 600)

  }



# Run only for two stations Gardandiwal and Nawabad for combining them (I need for my paper)

current_names <- c("CP_Qaisar", "CP_DaraiZhwandon", "CP_RabatiBala", "CP_TangiTashqurghan", "CP_Chichakto", "CP_Sayad", "CP_Delmarogh", 
                   "CP_NazdikiKeshandeh", "CP_Dawlatabad", "CP_KheshtPul", "CP_TangiNahrin", "CP_Faizabad", "CP_Baharak", "CP_PuliBangi", 
                   "CP_Khenjan", "CP_Doshi", "CP_NazdikiTaluqan", "CP_Keshem", "CP_PuliAlchin", "CP_Baghlan", "CP_Anjuman", "CP_Eshkashem",
                   "CP_Gardiz", "CP_Maton", "CP_Asmar", "CP_Nawabad", "CP_PuliQarghayi", "CP_PuliBehsod", "CP_Dakah", "CP_Tirin", "CP_NazdikiKandahar",
                   "CP_Lashkargah", "CP_Darwishan", "CP_TagabiGhaza", "CP_Adraskan", "CP_PuliHashemi", "CP_Torghundi", "CP_Farah", "CP_Estalef",
                   "CP_BaghiOmomi", "CP_TangiSayedan", "CP_DashteiSafid", "CP_Shakardara", "CP_PuliGhazni", "CP_Keraman", "CP_Cheghcheran",
                   "CP_Bamyan", "CP_Dawlatyar", "CP_NazdikiNayak", "CP_Waras", "CP_Gardandiwal")


new_annotations <- c("Qaisar (QSR)", "Dara-i-Zhwandon (DZN)", "Rabat-i-Bala (RBA)", "Tang-i-Tashqurghan (TTN)", "Chichakto (CCO)", 
                     "Sayad (SYD)", "Delmarogh (DMH)", "Nazdik-i-Keshandeh (NKH)", "Dawlatabad (DBD)", "Khesht-Pul (KPL)", 
                     "Tang-i-Nahrin (TNN)", "Faizabad (FZD)", "Baharak (BRK)", "Pul-i-Bangi (PBI)", "Khenjan (KJN)", 
                     "Doshi (DSI)", "Nazdik-i-Taluqan (NTN)", "Keshem (KSM)", "Pul-i-Alchin (PAN)", "Baghlan (BLN)", 
                     "Anjuman (AJN)", "Eshkashem (EKM)", "Gardiz (GDZ)", "Maton (MTN)", "Asmar (ASR)", "Nawabad (NBD) - Warm region", 
                     "Pul-i-Qarghayi (PQI)", "Pul-i-Behsod (PBD)", "Dakah (DKH)", "Tirin (TRN)", "Nazdik-i-Kandahar (NKR)", 
                     "Lashkargah (LGH)", "Darwishan (DSN)", "Tagab-i-Ghaza (TGA)", "Adraskan (ADN)", "Pul-i-Hashemi (PHI)", 
                     "Torghundi (TGI)", "Farah (FRH)", "Estalef (ETF)", "Bagh-i-Omomi (BOI)", "Tang-i-Sayedan (TSN)", 
                     "Dashte-i-Safid (DSD)", "Shakardara (SDA)", "Pul-i-Ghazni (PGI)", "Keraman (KRN)", "Cheghcheran (CCN)", 
                     "Bamyan (BYN)", "Dawlatyar (DYR)", "Nazdik-i-Nayak (NNK)", "Waras (WRS)", "Gardandiwal (GDL) - Cold region")


# Load required packages
# library(ggplot2)
# library(png)
# library(gridExtra)

# Nawabad
station_Naw <- current_names[26]

if (station_Naw %in% current_names) {
  # Read the saved plot
  chill_plotN <- readPNG(paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/chill_plots/", station_Naw, ".png"))
  
  # Convert the image to a ggplot object
  chill_plotN <- ggplot() +
    annotation_raster(chill_plotN, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    xlab("") + ylab("") +  # Remove x and y axis labels
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(t = 10, r = 0, b = -20, l = -20)) # Set plot margins
  
  # Add annotation with the new name
  chill_plotN <- chill_plotN +
    annotate("text", x = 0.5, y = 1, label = "Nawabad (NBD) - Warm region", hjust = 0.5, vjust = -16.8, size = 5, fontface = "bold") + # old parameter were y = 1, size = 5, vjust 15.8 other same and ggsave size 13,22
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          xlab = NULL, # Remove x-axis label
          ylab = NULL) # Remove y-axis label
  
  # Save the annotated plot
  ggsave(plot = chill_plotN, filename = paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/chill_plots/annotated_", station_Naw, ".png"), height = 13, width = 22, units = "cm", dpi = 600)
  
}


# Gardandiwal
station_Gar <- current_names[51]

if (station_Gar %in% current_names) {
  # Read the saved plot
  chill_plotG <- readPNG(paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/chill_plots/", station_Gar, ".png"))
  
  # Convert the image to a ggplot object
  chill_plotG <- ggplot() +
    annotation_raster(chill_plotG, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    xlab("") + ylab("") +  # Remove x and y axis labels
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(t = 10, r = 0, b = -20, l = -20)) # Set plot margins
  
  # Add annotation with the new name
  chill_plotG <- chill_plotG +
    annotate("text", x = 0.5, y = 1, label = "Gardandiwal (GDL) - Cold region", hjust = 0.5, vjust = -16.75, size = 5, fontface = "bold") + # old parameter were y = 1, size = 5, vjust 15.8 other same and ggsave size 13,22
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          xlab = NULL, # Remove x-axis label
          ylab = NULL) # Remove y-axis label
  
  # Save the annotated plot
  ggsave(plot = chill_plotG, filename = paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/chill_plots/annotated_", station_Gar, ".png"), height = 13, width = 22, units = "cm", dpi = 600)
  
}

# Read and annotate plots
chill_plotN <- image_read(paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/chill_plots/",  "annotated_CP_Nawabad.png"))
chill_plotG <-   image_read(paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/chill_plots/",  "Annotated_CP_Gardandiwal.png"))

combined_N_G <- image_append(c(chill_plotN, chill_plotG), stack = TRUE)

image_write(combined_N_G,path=paste0("D:/Rdata/Chill_quantification/Future_scenarios/chill/all_scenarios_plots/chill_plots/N_G.png"))

