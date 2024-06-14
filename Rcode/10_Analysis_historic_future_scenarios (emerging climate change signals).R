
library(chillR)
library(tidyverse)
library(dplyr)

# Historic analysis
# hist_chill_1980 <- read.csv('Future_scenarios/chill/historic_chill/Asmar_historic_1_1980.csv')
# hist_chill_2020 <- read.csv('Future_scenarios/chill/historic_chill/Asmar_historic_5_2020.csv')
# 
# min_cp_1980 <- min(hist_chill_1980$Chill_CP)
# sum(hist_chill_2020$Chill_CP < min_cp_1980)


f <- list.files('Future_scenarios/chill/historic_chill/', full.names = TRUE)
historic_chill_list <- lapply(f, read.csv)

f <- list.files('Future_scenarios/chill/historic_chill/')

station <- strsplit(f, '_') %>% 
  purrr::map(1)%>%
  unlist()

year <- strsplit(f, '_') %>% 
  purrr::map(function(x)  x[length(x)]) %>% 
  str_split('\\.') %>% 
  purrr::map(1) %>% 
  unlist()

chill_summary_df <- data.frame(NULL)

for(stat in unique(station)){
 # stat <- unique(station)[1]
  
  matching_station <-  which(station == stat)
  
  matching_year_1980 <- which(year[matching_station] == '1980')
  matching_year_1990 <- which(year[matching_station] == '1990')
  matching_year_2000 <- which(year[matching_station] == '2000')
  matching_year_2010 <- which(year[matching_station] == '2010')
  matching_year_2020 <- which(year[matching_station] == '2020')
  
  
  chill_1980 <-  historic_chill_list[[matching_station[matching_year_1980]]]
  #chill_1980$Chill_CP <- round(chill_1980$Chill_CP, digits = 0) # round it
  # chill_1990 <-  historic_chill_list[[matching_station[matching_year_1990]]]
  # chill_2000 <-  historic_chill_list[[matching_station[matching_year_2000]]]
  # chill_2010 <-  historic_chill_list[[matching_station[matching_year_2010]]]
  chill_2020 <-  historic_chill_list[[matching_station[matching_year_2020]]]
  #chill_2020$Chill_CP <- round(chill_2020$Chill_CP, digits = 0) # round it
  
  
  min_cp_1980 <- min(chill_1980$Chill_CP)
  min_cp_1980 <- round(min_cp_1980, digits = 0) # round the CP values
  
  # percent_lower_1990 <-  sum(chill_1990$Chill_CP < min_cp_1980)
  # percent_lower_2000 <-  sum(chill_2000$Chill_CP < min_cp_1980)
  # percent_lower_2010 <-  sum(chill_2010$Chill_CP < min_cp_1980)
  percent_lower_2020 <-  sum(chill_2020$Chill_CP < min_cp_1980)
  
  
  max_cp_1980 <- max(chill_1980$Chill_CP)
  max_cp_1980 <- round(max_cp_1980, digits = 0) # round the CP values
  
  # percent_higher_1990 <-  sum(chill_1990$Chill_CP > max_cp_1980)
  # percent_higher_2000 <-  sum(chill_2000$Chill_CP > max_cp_1980)
  # percent_higher_2010 <-  sum(chill_2010$Chill_CP > max_cp_1980)
  percent_higher_2020 <-  sum(chill_2020$Chill_CP > max_cp_1980)

  min_max_CP_1980 <- paste(min_cp_1980, max_cp_1980, sep = "-")
  
  ## i add percent overlap but did not work out##
  #percent_overlap_1980_2020 <- (chill_2020$Chill_CP >= min_chill_1980 & chill_2020$Chill_CP <= max_chill_1980) / nrow(chill_2020) * 100
  
  chill_summary_df <- rbind(chill_summary_df, 
                            data.frame(Station_ID = stat,
                                       baseline_year = 1980,
                                       comparison_year = 2020,
                                       # min_cp_baseline =min_cp_1980,
                                       min_max_CP_1980 = min_max_CP_1980,
                                       # percent_lower_1990 = percent_lower_1990,
                                       # percent_lower_2000 = percent_lower_2000,
                                       # percent_lower_2010 = percent_lower_2010,
                                       percent_lower_2020 = percent_lower_2020,
                                       #max_cp_baseline =max_cp_1980,
                                       # percent_higher_1990 = percent_higher_1990,
                                       # percent_higher_2000 = percent_higher_2000,
                                       # percent_higher_2010 = percent_higher_2010,
                                       percent_higher_2020 = percent_higher_2020))
                                       #percent_overlap = percent_overlap_1980_2020))
                                       
                                       #max_cp_baseline =max_cp_1980,
                                       #percent_higher = percent_higher))

}


# also combine it with station coordinates and elevation
Stations_coordinates <- read.csv("D:/Rdata/Chill_quantification/Coordinates.csv")
Stations_elevation <- read.csv("D:/Rdata/Chill_quantification/Station_elevation.csv")

Coor_elev <- merge(Stations_coordinates, Stations_elevation, by = 'Station_ID')
chill_summary_df <- merge(Coor_elev, chill_summary_df, by = 'Station_ID')

# Combine it with other dataframe from kendall test
All_past_kend <- read.csv("D:/Rdata/Chill_quantification/SWC/historic/kendall_test/All_past_kendall.csv", check.names = FALSE)
chill_summary_historic <- merge(All_past_kend, chill_summary_df, by = c("Station_ID", "Longitude", "Latitude", 
                                                                                "Province", "Elevation..m...m.s.l."))
# round absolute chill change
chill_summary_historic$Abs_SWC_change_1980_2020 <- round(chill_summary_historic$Abs_SWC_change_1980_2020, digits = 0)

# add stations abbreviations as well
station_abr <- read.csv("D:/Rdata/Chill_quantification/SWC/Station_abreviations.csv")
chill_summary_historic <- merge(station_abr, chill_summary_historic, by = "Station_ID")



# Add the quantiles for 1980 and 2020 scenarios

Station_AT_List <-  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs_Sim", prefix = "1980_2020_")
#Assigning the names
List_Names <- list.files("D:/Rdata/QDM/Stations_Obs")
Stations_Names <- str_split(List_Names, pattern = "_") %>% 
  purrr::map_chr(1)
names(Station_AT_List) <- Stations_Names

# create an empty df for putting quantiles
quantiles_df <- data.frame(Station_ID = character(),
                           Q5_1980 = numeric(),
                           Q10_1980 = numeric(),
                           #Q50_1980 = numeric(),
                           Q95_1980 = numeric(),
                           Q5_2020 = numeric(),
                           Q10_2020 = numeric(),
                           #Q50_2020 = numeric(),
                           Q95_2020 = numeric(),
                           stringsAsFactors = FALSE)

quantiles_together <- data.frame(Station_ID = character(),
                           SWC_1980 = character(),
                           SWC_2020 = character(),
                           stringsAsFactors = FALSE)

for (i in 1: length(Station_AT_List)) {
  
  Station_List <- Station_AT_List[[i]]
  
  # Load them computed chill for historic scenarios 
  file_path2 <- "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill/"
  file_name2 <- paste0(Stations_Names[i], "_historic")
  file_name3 <- paste0(Stations_Names[i], "_observed")
  
  chill_past_scenarios <- load_temperature_scenarios(file_path2, file_name2)
  chill_observed <- load_temperature_scenarios(file_path2, file_name3)
  
  # Calculate quantile for SWC (which is median as well), median and 95% (CI)
  QR_1980 <- quantile(chill_past_scenarios[[1]]$Chill_CP, c(0.05, 0.10, 0.50, 0.95))
  # CI_1990 <- quantile(chill_past_scenarios[[2]]$Chill_CP, c(0.05, 0.10, 0.50, 0.95))
  # CI_2000 <- quantile(chill_past_scenarios[[3]]$Chill_CP, c(0.05, 0.10, 0.50, 0.95))
  # CI_2010 <- quantile(chill_past_scenarios[[4]]$Chill_CP, c(0.05, 0.10, 0.50, 0.95))
  QR_2020 <- quantile(chill_past_scenarios[[5]]$Chill_CP, c(0.05, 0.10, 0.50, 0.95))
  
  # put quantile separately in df
  quantiles_df <- rbind(quantiles_df, data.frame(Station_ID = Stations_Names[i],
                             Q5_1980 = round(QR_1980[1], digits = 0),
                             Q10_1980 = round(QR_1980[2], digits = 0),
                             #Q50_1980 = round(QR_1980[3], digits = 0),
                             Q95_1980 = round(QR_1980[4], digits = 0),
                             Q5_2020 = round(QR_2020[1], digits = 0),
                             Q10_2020 = round(QR_2020[2], digits = 0),
                             #Q50_2020 = round(QR_2020[3], digits = 0),
                             Q95_2020 = round(QR_2020[4], digits = 0),
                             stringsAsFactors = FALSE))
  
  # Combine quantiles for 1980 and 2020 into single strings
  # SWC_1980 <- paste0(round(QR_1980[2], digits = 0), " (", round(QR_1980[1], digits = 0), "-", round(QR_1980[4], digits = 0), ")") old format
  # SWC_2020 <- paste0(round(QR_2020[2], digits = 0), " (", round(QR_2020[1], digits = 0), "-", round(QR_2020[4], digits = 0), ")") old format
  
  SWC_1980 <- paste0(round(QR_1980[1], digits = 0), "-", round(QR_1980[4], digits = 0))
  SWC_2020 <- paste0(round(QR_2020[1], digits = 0), "-", round(QR_2020[4], digits = 0))
  
  
  # Add quantiles to dataframe
  quantiles_together <- rbind(quantiles_together, data.frame(Station_ID = Stations_Names[i],
                                                 # SWC_1980 = SWC_1980, old format
                                                 # SWC_2020 = SWC_2020, old format
                                                 CI_1980 = SWC_1980,
                                                 CI_2020 = SWC_2020,
                                                 stringsAsFactors = FALSE))
}


chill_summary_historic <- merge(chill_summary_historic,quantiles_together, by = "Station_ID")


# Categorize all stations and provinces based on the sub regions
chill_summary_df_regions <- chill_summary_historic %>% 
  mutate(Sub_regions = case_when(
    Province %in% c("Jawzjan", "Balkh", "Sari Pul", "Samangan", "Faryab") ~ "North",
    Province %in% c("Badakhshan", "Takhar", "Kunduz", "Baghlan") ~ "Northeast",
    Province %in% c("Nangarhar", "Laghman", "Kunar", "Nuristan", "Paktika", "Gardiz", "Khost") ~ "East",
    Province %in% c("Kandahar", "Zabul", "Hilmand", "Nimroz",  "Uruzgan" ) ~ "South",
    Province %in% c("Hirat", "Farah", "Badghis") ~ "West",
    Province %in% c("Bamyan", "Ghor", "Daykundi", "Ghazni", "Wardak", "Parwan", "Kabul", "Logar", "Kapisa", "Panjshir") ~ "Central",
    TRUE ~ NA_character_  # For any province not in the lists above)
  ))

# Order Sub_regions
chill_summary_df_regions$Sub_regions <- factor(chill_summary_df_regions$Sub_regions, 
                                       levels = c("North", "Northeast", "East", "South", "West", "Central"))

# Sort by regions
chill_summary_df_regions <- chill_summary_df_regions %>% 
  arrange(Sub_regions, Province)

chill_summary_df_regions <- chill_summary_df_regions %>% 
  select(Sub_regions, everything())

my_paper_table <- subset(chill_summary_df_regions, select= c(Sub_regions, Station_ID, Abr, `1980_SWC (CP)`, `2020_SWC (CP)`, 
                                                             CI_1980, CI_2020, min_max_CP_1980, percent_lower_2020, percent_higher_2020))
my_paper_table <- write.csv(my_paper_table, "D:/Rdata/Chill_quantification/SWC/historic/percent_overlap/my_paper_table.csv", row.names = FALSE, fileEncoding = 'UTF-8')  

chill_summary_df_regions <- write.csv(chill_summary_df_regions, "D:/Rdata/Chill_quantification/SWC/historic/percent_overlap/chill_summary_df_regions.csv", row.names = FALSE, fileEncoding = 'UTF-8')


# round the values 
# chill_summary_historic$min_cp_baseline <- round(chill_summary_historic$min_cp_baseline, digits = 0) # round the CP values
# chill_summary_historic$max_cp_baseline <- round(chill_summary_historic$max_cp_baseline, digits = 0) # round the CP values

write.csv(chill_summary_historic, "D:/Rdata/Chill_quantification/SWC/historic/percent_overlap/chill_summary_historic.csv", row.names = FALSE)


# A try to plot the historic chill in a sigma plot as suggested by Katja
# A try to plot the historic chill in a sigma plot as suggested by Katja
# A try to plot the historic chill in a sigma plot as suggested by Katja
# A try to plot the historic chill in a sigma plot as suggested by Katja
# A try to plot the historic chill in a sigma plot as suggested by Katja
# A try to plot the historic chill in a sigma plot as suggested by Katja
# A try to plot the historic chill in a sigma plot as suggested by Katja
# A try to plot the historic chill in a sigma plot as suggested by Katja
# A try to plot the historic chill in a sigma plot as suggested by Katja


h <- list.files('Future_scenarios/chill/historic_chill/', full.names = TRUE)
historic_chill_list <- lapply(h, read.csv)

h <- list.files('Future_scenarios/chill/historic_chill/')

station <- strsplit(h, '_') %>% 
  purrr::map(1)%>%
  unlist()

year <- strsplit(h, '_') %>% 
  purrr::map(function(x)  x[length(x)]) %>% 
  str_split('\\.') %>% 
  purrr::map(1) %>% 
  unlist()


Station_AT_List <-  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs_Sim", prefix = "1980_2020_")

#Assigning the names
List_Names <- list.files("D:/Rdata/QDM/Stations_Obs")
Stations_Names <- str_split(List_Names, pattern = "_") %>% 
  purrr::map_chr(1)
names(Station_AT_List) <- Stations_Names

extracted_data <- list()

for (i in 1: length(Station_AT_List)) {
  
  Station_List <- Station_AT_List[[i]]
  
  # Load them computed chill for historic scenarios 
  file_path2 <- "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill/"
  file_name2 <- paste0(Stations_Names[i], "_historic")

  chill_past_scenarios <- load_temperature_scenarios(file_path2, file_name2)
  
  # Extract Chill_CP column from 1980 data
  chill_1980 <- chill_past_scenarios$"1980"$Chill_CP
  
  # Extract Chill_CP column from 2020 data
  chill_2020 <- chill_past_scenarios$"2020"$Chill_CP
  
  # Combine extracted data into a data frame
  station_data <- data.frame(Chill_1980 = chill_1980, Chill_2020 = chill_2020)
  
  # Assign station name as a column
  station_data$Station_ID <- rep(Stations_Names[i], length(chill_1980))
  
  # Append to the list
  extracted_data[[i]] <- station_data
}

# Combine all extracted data into a single data frame
final_data <- do.call(rbind, extracted_data)

# Print the final data frame
print(final_data)


#### Typical method #### I am commenting this out as I will not use this
########################
# # Clear any existing plots
# dev.off()
# 
# # Loop through each station
# for (i in 1:length(Stations_Names)) {
#   stat <- Stations_Names[i]
#   
# # Subset your data for the Adraskan station
# subset_data <- final_data[final_data$Station_ID == stat, ]
# 
# # Extract the Chill_1980 data for Adraskan station
# chill_data1980 <- subset_data$Chill_1980
# chill_data2020 <- subset_data$Chill_2020
# 
# dens_1980 <- density(chill_data1980, bw= 2)
# dens_2020 <- density(chill_data2020, bw= 2)
# 
# # Set up a multi-panel plot
# par(mfrow = c(1, 2))
# 
# # Plot for 1980
# plot(dens_1980, main = "", 
#      xlab = "Chill accumulation in 1980", ylab = "Probability", 
#      ylim = c(0, 0.08), 
#      xlim = c(min(1), max(100)))
# 
# # Calculate the quantiles
# quantile_5_1980 <- quantile(chill_data1980, probs = 0.05)
# quantile_95_1980 <- quantile(chill_data1980, probs = 0.95)
# quantile_10_1980 <- quantile(chill_data1980, probs = 0.1)
# 
# # Find the index of the x-coordinate closest to the quantile value
# index_quantile_1980 <- which.min(abs(dens_1980$x - quantile_95_1980))
# # Insert the line up to the curve
# lines(c(quantile_95_1980, quantile_95_1980), c(0, dens_1980$y[index_quantile_1980]), col = "brown")
# # add the chill value
# text(quantile_95_1980, dens_1980$y[index_quantile_1980], round(quantile_95_1980, 0), pos = 1, col = "brown")
# 
# index_quantile_1980 <- which.min(abs(dens_1980$x - quantile_5_1980))
# lines(c(quantile_5_1980, quantile_5_1980), c(0, dens_1980$y[index_quantile_1980]), col = "brown")
# text(quantile_5_1980, dens_1980$y[index_quantile_1980], round(quantile_5_1980, 0), pos = 1, col = "brown")
# 
# index_quantile_1980 <- which.min(abs(dens_1980$x - quantile_10_1980))
# lines(c(quantile_10_1980, quantile_10_1980), c(0, dens_1980$y[index_quantile_1980]), col = "seagreen")
# text(quantile_10_1980, dens_1980$y[index_quantile_1980], round(quantile_10_1980, 0), pos = 1, col = "seagreen")
# 
# 
# # Plot for 2020
# plot(dens_2020, main = "", 
#      xlab = "Chill accumulation in 2020", ylab = "", 
#      ylim = NULL,  
#      xlim = c(min(1), max(100)))
# 
# # Calculate the quantiles
# quantile_5_2020 <- quantile(chill_data2020, probs = 0.05)
# quantile_95_2020 <- quantile(chill_data2020, probs = 0.95)
# quantile_10_2020 <- quantile(chill_data2020, probs = 0.1)
# 
# # Find the index of the x-coordinate closest to the quantile value
# index_quantile_2020 <- which.min(abs(dens_2020$x - quantile_95_2020))
# # Insert the line up to the curve
# lines(c(quantile_95_2020, quantile_95_2020), c(0, dens_2020$y[index_quantile_2020]), col = "brown")
# # add the chill value
# text(quantile_95_2020, dens_2020$y[index_quantile_2020], round(quantile_95_2020, 0), pos = 1, col = "brown")
# 
# index_quantile_2020 <- which.min(abs(dens_2020$x - quantile_5_2020))
# lines(c(quantile_5_2020, quantile_5_2020), c(0, dens_2020$y[index_quantile_2020]), col = "brown")
# text(quantile_5_2020, dens_2020$y[index_quantile_2020], round(quantile_5_2020, 0), pos = 1, col = "brown")
# 
# index_quantile_2020 <- which.min(abs(dens_2020$x - quantile_10_2020))
# lines(c(quantile_10_2020, quantile_10_2020), c(0, dens_2020$y[index_quantile_2020]), col = "seagreen")
# text(quantile_10_2020, dens_2020$y[index_quantile_2020], round(quantile_10_2020, 0), pos = 1, col = "seagreen")
# 
# 
# png(paste0("density_plots_", stat, ".png"), width = 1000, height = 500, units = "px", res = 300)
# dev.off()
# 
# }



#### ggplot method #####
########################

# Short form
# ggplot() +
#   # Overlay the normal density area for chill_data1980 (filled dark blue)
#   geom_area(data = data.frame(chill_data1980), aes(x = chill_data1980, y = dnorm(chill_data1980, mean = mean_value_1980, sd = sd_value_1980)), fill = "darkblue", alpha = 0.5) +
#   # Overlay the normal density area for chill_data2020 (filled dark red)
#   geom_area(data = data.frame(chill_data2020), aes(x = chill_data2020, y = dnorm(chill_data2020, mean = mean_value_2020, sd = sd_value_2020)), fill = "darkred", alpha = 0.5) +
#   labs(title = "",
#        x = "Chill accumulation",
#        y = "Density") +
#   theme_minimal()

library(gridExtra)
library(ggplot2)

# Create a named vector with old and new station names
station_mapping <- c("Adraskan" = "Adraskan",
                     "Anjuman" = "Anjuman",
                     "Asmar" = "Asmar",
                     "BaghiOmomi" = "Bagh-i-Omomi",
                     "Baghlan" = "Baghlan",
                     "Baharak" = "Baharak",
                     "Bamyan" = "Bamyan",
                     "Cheghcheran" = "Cheghcheran",
                     "Chichakto" = "Chichakto",
                     "Dakah" = "Dakah",
                     "DaraiZhwandon" = "Dara-i-Zhwandon",
                     "Darwishan" = "Darwishan",
                     "DashteiSafid" = "Dashte-i-Safid",
                     "Dawlatabad" = "Dawlatabad",
                     "Dawlatyar" = "Dawlatyar",
                     "Delmarogh" = "Delmarogh",
                     "Doshi" = "Doshi",
                     "Eshkashem" = "Eshkashem",
                     "Estalef" = "Estalef",
                     "Faizabad" = "Faizabad",
                     "Farah" = "Farah",
                     "Gardandiwal" = "Gardandiwal",
                     "Gardiz" = "Gardiz",
                     "Keraman" = "Keraman",
                     "Keshem" = "Keshem",
                     "Khenjan" = "Khenjan",
                     "KheshtPul" = "Khesht-Pul",
                     "Lashkargah" = "Lashkargah",
                     "Maton" = "Maton",
                     "Nawabad" = "Nawabad",
                     "NazdikiKandahar" = "Nazdik-i-Kandahar",
                     "NazdikiKeshandeh" = "Nazdik-i-Keshandeh",
                     "NazdikiNayak" = "Nazdik-i-Nayak",
                     "NazdikiTaluqan" = "Nazdik-i-Taluqan",
                     "PuliAlchin" = "Pul-i-Alchin",
                     "PuliBangi" = "Pul-i-Bangi",
                     "PuliBehsod" = "Pul-i-Behsod",
                     "PuliGhazni" = "Pul-i-Ghazni",
                     "PuliHashemi" = "Pul-i-Hashemi",
                     "PuliQarghayi" = "Pul-i-Qarghayi",
                     "Qaisar" = "Qaisar",
                     "RabatiBala" = "Rabat-i-Bala",
                     "Sayad" = "Sayad",
                     "Shakardara" = "Shakardara",
                     "TagabiGhaza" = "Tagab-i-Ghaza",
                     "TangiNahrin" = "Tang-i-Nahrin",
                     "TangiSayedan" = "Tang-i-Sayedan",
                     "TangiTashqurghan" = "Tang-i-Tashqurghan",
                     "Tirin" = "Tirin",
                     "Torghundi" = "Torghundi",
                     "Waras" = "Waras"
)

final_df <- final_data

library(plyr)
# Replace old names with new names using mapvalues()
final_df$Station_ID <- mapvalues(final_df$Station_ID, from = names(station_mapping), to = station_mapping)

station_Names <- read.csv("D:/Rdata/Chill_quantification/station_names.csv")
station_names <- as.character(station_Names$Station_ID)

plot_list <- list()


# Loop through each station
for (station in station_names) {
  # Subset data for the current station
  station_data <- subset(final_df, Station_ID == station)
  
  # Extract chill data for 1980 and 2020
  chill_data1980 <- station_data$Chill_1980
  chill_data2020 <- station_data$Chill_2020
  
# Calculate statistics for each year's chill data
mean_value_1980 <- mean(chill_data1980)
sd_value_1980 <- sd(chill_data1980)
mean_value_2020 <- mean(chill_data2020)
sd_value_2020 <- sd(chill_data2020)

# Calculate quantiles for chill_data1980 (within the ggplot call)
#q1980_05 <- quantile(data.frame(chill_data1980)$chill_data1980, probs = 0.05)
q1980_90 <- quantile(data.frame(chill_data1980)$chill_data1980, probs = 0.90)
q1980_10 <- quantile(data.frame(chill_data1980)$chill_data1980, probs = 0.10)
mean_1980 <- mean(data.frame(chill_data1980)$chill_data1980)


# Calculate quantiles for chill_data2020
#q2020_05 <- quantile(data.frame(chill_data2020)$chill_data2020, probs = 0.05)
q2020_90 <- quantile(data.frame(chill_data2020)$chill_data2020, probs = 0.90)
q2020_10 <- quantile(data.frame(chill_data2020)$chill_data2020, probs = 0.10)
mean_2020 <- mean(data.frame(chill_data2020)$chill_data2020)


plot <- ggplot() +
  # Overlay the normal density area for chill_data1980 (filled dark blue)
  geom_area(data = data.frame(chill_data1980), aes(x = chill_data1980, y = dnorm(chill_data1980, mean = mean_value_1980, sd = sd_value_1980)), fill = "darkblue", alpha = 0.5, color = "darkblue") +
  # Overlay the normal density area for chill_data2020 (filled dark red)
  geom_area(data = data.frame(chill_data2020), aes(x = chill_data2020, y = dnorm(chill_data2020, mean = mean_value_2020, sd = sd_value_2020)), fill = "darkred", alpha = 0.5, color = "darkred") +
  
  # Add vertical lines for quantiles on 1980 data
  #geom_segment(aes(x = q1980_05, y = 0, xend = q1980_05, yend = dnorm(q1980_05, mean = mean_value_1980, sd = sd_value_1980)), linetype = "dashed", color = "darkblue", alpha = 0.7) +
  geom_segment(aes(x = q1980_90, y = 0, xend = q1980_90, yend = dnorm(q1980_90, mean = mean_value_1980, sd = sd_value_1980)), linetype = "dashed", color = "darkblue", alpha = 0.7) +
  geom_segment(aes(x = q1980_10, y = 0, xend = q1980_10, yend = dnorm(q1980_10, mean = mean_value_1980, sd = sd_value_1980)), linetype = "dashed", color = "darkblue", alpha = 0.7) +
  geom_segment(aes(x = mean_1980, y = 0, xend = mean_1980, yend = dnorm(mean_1980, mean = mean_value_1980, sd = sd_value_1980)), linetype = "dashed", color = "darkblue", alpha = 0.7) +
  
  # Add vertical lines for quantiles on 2020 data
  #geom_segment(aes(x = q2020_05, y = 0, xend = q2020_05, yend = dnorm(q2020_05, mean = mean_value_2020, sd = sd_value_2020)), linetype = "dashed", color = "darkred", alpha = 0.7) +
  geom_segment(aes(x = q2020_90, y = 0, xend = q2020_90, yend = dnorm(q2020_90, mean = mean_value_2020, sd = sd_value_2020)), linetype = "dashed", color = "darkred", alpha = 0.7) +
  geom_segment(aes(x = q2020_10, y = 0, xend = q2020_10, yend = dnorm(q2020_10, mean = mean_value_2020, sd = sd_value_2020)), linetype = "dashed", color = "darkred", alpha = 0.7) +
  geom_segment(aes(x = mean_2020, y = 0, xend = mean_2020, yend = dnorm(mean_2020, mean = mean_value_2020, sd = sd_value_2020)), linetype = "dashed", color = "darkred", alpha = 0.7) +
  
  # Annotate station names on the left side of the plot in vertical position
 #annotate("text", x = -Inf, y = Inf, label = station, hjust = 2, vjust = 1, angle = 90, size = 5, color = "black") +  
  # Annotate titles at the base
  # annotate("text", x = c(q1980_05, q1980_95, q1980_10, mean_1980, q2020_05, q2020_95, q2020_10, mean_2020),
  #          y = rep(0, 8),
  #          label = c("Q5", "Q95", "SWC", "Mean","Q5", "Q95", "SWC", "Mean"),
  #          vjust = 1.5,
  #          color = c("darkblue", "darkblue", "green","darkblue", "darkred", "darkred", "green","darkred")) +
  
  # Annotate quantile values with zero decimal places
  annotate("text", x = c(q1980_90, q1980_10, mean_1980, q2020_90, q2020_10, mean_2020),
           y = c(dnorm(q1980_90, mean = mean_value_1980, sd = sd_value_1980),
                 dnorm(q1980_10, mean = mean_value_1980, sd = sd_value_1980),
                 dnorm(mean_1980, mean = mean_value_1980, sd = sd_value_1980),
                 
                 dnorm(q2020_90, mean = mean_value_2020, sd = sd_value_2020),
                 dnorm(q2020_10, mean = mean_value_2020, sd = sd_value_2020),
                 dnorm(mean_2020, mean = mean_value_2020, sd = sd_value_2020)) + 0.013, # adjust the offset of values
           label = c(round(q1980_90, 0), round(q1980_10, 0),round(mean_1980, 0),
                     round(q2020_90, 0), round(q2020_10, 0), round(mean_2020, 0)),
           vjust = 0.9,
           size = 5,
           color = c("darkblue", "darkblue", "darkblue", "darkred", "darkred", "darkred")) +
  
  # labs(title = NULL,
  #      x = NULL,
  #      y = NULL) +

  theme_minimal() +
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_blank(),  # Remove y-axis label
        plot.title = element_blank(),    # Remove plot title
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0.5, -0.25, -0.25, -0.25, "cm"),
        #panel.grid = element_line(color = "gray80", linetype = "solid"),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "gray90", fill= NA))


ggsave(filename = paste0("D:/Rdata/Chill_quantification/SWC/historic/plots/", station, ".png"),
       #plot = plot + theme(aspect.ratio = 1/2),
       plot = plot, bg = "white",   width = 6, height = 4, units = "cm", dpi = 600)

# Add the plot to the plot list
plot_list[[length(plot_list) + 1]] <- plot

}

# to fix cheghcheran value error, run the above only for cheghcheran [8]

# Run below part only for one station with a zoom out version e.g. Nawabad
# station <- Nawabad
# Loop through each station
for (station in station_names[30]) {
  # Subset data for the current station
  station_data <- subset(final_df, Station_ID == station)
  
  # Extract chill data for 1980 and 2020
  chill_data1980 <- station_data$Chill_1980
  chill_data2020 <- station_data$Chill_2020
  
  # Calculate statistics for each year's chill data
  mean_value_1980 <- mean(chill_data1980)
  sd_value_1980 <- sd(chill_data1980)
  mean_value_2020 <- mean(chill_data2020)
  sd_value_2020 <- sd(chill_data2020)
  
  # Calculate quantiles for chill_data1980 (within the ggplot call)
  #q1980_05 <- quantile(data.frame(chill_data1980)$chill_data1980, probs = 0.05)
  q1980_90 <- quantile(data.frame(chill_data1980)$chill_data1980, probs = 0.90)
  q1980_10 <- quantile(data.frame(chill_data1980)$chill_data1980, probs = 0.10)
  mean_1980 <- mean(data.frame(chill_data1980)$chill_data1980)
  
  
  # Calculate quantiles for chill_data2020
  #q2020_05 <- quantile(data.frame(chill_data2020)$chill_data2020, probs = 0.05)
  q2020_90 <- quantile(data.frame(chill_data2020)$chill_data2020, probs = 0.90)
  q2020_10 <- quantile(data.frame(chill_data2020)$chill_data2020, probs = 0.10)
  mean_2020 <- mean(data.frame(chill_data2020)$chill_data2020)
  
  
  plot <- ggplot() +
    # Overlay the normal density area for chill_data1980 (filled dark blue)
    geom_area(data = data.frame(chill_data1980), aes(x = chill_data1980, y = dnorm(chill_data1980, mean = mean_value_1980, sd = sd_value_1980), fill = "darkblue"), alpha = 0.5, color = "darkblue") +
    # Overlay the normal density area for chill_data2020 (filled dark red)
    geom_area(data = data.frame(chill_data2020), aes(x = chill_data2020, y = dnorm(chill_data2020, mean = mean_value_2020, sd = sd_value_2020), fill = "darkred"), alpha = 0.5, color = "darkred") +
    
    # Add vertical lines for quantiles on 1980 data
    #geom_segment(aes(x = q1980_05, y = 0, xend = q1980_05, yend = dnorm(q1980_05, mean = mean_value_1980, sd = sd_value_1980), color = "darkblue"), linetype = "dashed", alpha = 0.7) +
    geom_segment(aes(x = q1980_10, y = 0, xend = q1980_10, yend = dnorm(q1980_10, mean = mean_value_1980, sd = sd_value_1980)), linetype = "dashed", color = "darkblue", alpha = 0.7) +
    geom_segment(aes(x = q1980_90, y = 0, xend = q1980_90, yend = dnorm(q1980_90, mean = mean_value_1980, sd = sd_value_1980)), linetype = "dashed", color = "darkblue", alpha = 0.7) +
    geom_segment(aes(x = mean_1980, y = 0, xend = mean_1980, yend = dnorm(mean_1980, mean = mean_value_1980, sd = sd_value_1980)), linetype = "dashed", color = "darkblue", alpha = 0.7) +
    
    # Add vertical lines for quantiles on 2020 data
    #geom_segment(aes(x = q2020_05, y = 0, xend = q2020_05, yend = dnorm(q2020_05, mean = mean_value_2020, sd = sd_value_2020), color = "darkred"), linetype = "dashed", alpha = 0.7) +
    geom_segment(aes(x = q2020_10, y = 0, xend = q2020_10, yend = dnorm(q2020_10, mean = mean_value_2020, sd = sd_value_2020)), linetype = "dashed",  color = "darkred" ,alpha = 0.7) +
    geom_segment(aes(x = q2020_90, y = 0, xend = q2020_90, yend = dnorm(q2020_90, mean = mean_value_2020, sd = sd_value_2020)), linetype = "dashed", color = "darkred", alpha = 0.7) +
    geom_segment(aes(x = mean_2020, y = 0, xend = mean_2020, yend = dnorm(mean_2020, mean = mean_value_2020, sd = sd_value_2020)), linetype = "dashed", color = "darkred", alpha = 0.7) +
    
    
    # Annotate quantile values with zero decimal places
    annotate("text", x = c(q1980_90, q1980_10, mean_1980, q2020_90, q2020_10, mean_2020),
             y = c(dnorm(q1980_90, mean = mean_value_1980, sd = sd_value_1980),
                   dnorm(q1980_10, mean = mean_value_1980, sd = sd_value_1980),
                   dnorm(mean_1980, mean = mean_value_1980, sd = sd_value_1980),
                   
                   dnorm(q2020_90, mean = mean_value_2020, sd = sd_value_2020),
                   dnorm(q2020_10, mean = mean_value_2020, sd = sd_value_2020),
                   dnorm(mean_2020, mean = mean_value_2020, sd = sd_value_2020)) + 0.0062, # adjust the offset of values
             label = c(round(q1980_90, 0), round(q1980_10, 0),round(mean_1980, 0),
                       round(q2020_90, 0), round(q2020_10, 0), round(mean_2020, 0)),
             vjust = 0.9,
             size = 5,
             color = c("darkblue", "darkblue","darkblue", "darkred", "darkred", "darkred")) +
    
    # Add text labels for quantiles (1980)
    geom_text(aes(x = c(q1980_10, mean_1980, q1980_90), 
                  y = 0, 
                  label = c("Q10", "Q50", "Q90")), 
              hjust = 0.6, # Align text to the right
              vjust = 1.1, # Position slightly below the line
              size = 3.8, color = "darkblue") +
    
    # Add text labels for quantiles (2020)
    geom_text(aes(x = c(q2020_10, mean_2020, q2020_90+0.5), 
                  y = 0, 
                  label = c("Q10", "Q50", "Q90")), 
              hjust = 0.6, 
              vjust = 1.1, 
              size = 3.8, color = "darkred") +
    
    labs(title = "NULL",
         x = "Chill accumulation (CP)",
         y = "Density") +
    
    theme_minimal() +
    theme(  # Remove y-axis label
      plot.title = element_blank(),    # Remove plot title
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.ticks.y = element_blank(),
      plot.margin = margin(0.4, 0.1, -0.1, 0.1, "cm"),
      #panel.grid = element_line(color = "gray80", linetype = "solid"),
      panel.grid = element_blank(),
      panel.background = element_rect(colour = "gray90", fill= NA),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 11)) +
    
    # Add a legend for fill colors
    scale_fill_manual(name = "Year",
                      values = c("darkblue" = "darkblue", "darkred" = "darkred"),
                      labels = c("1980", "2020")) 
  
  plot <- plot + ylim(-0.002, 0.082) # move the colored plot slightly above from its base
  # scale_color_manual(name = "Linetype",
  #                      values = c("darkblue" = "darkblue", "darkred" = "darkred", "green" = "green"),
  #                      labels = c("Quantile 1980", "Quantile 2020", "SWC"),
  #                      guide = guide_legend(override.aes = list(linetype = "dashed")))
  
  
  ggsave(filename = paste0("D:/Rdata/Chill_quantification/SWC/historic/plots/", "test", ".png"),
         plot = plot, bg = "white",   width = 12, height = 8, units = "cm", dpi = 600)
  
  
}

require(magick)
#1 Combining plots considering sub-regions
folder <- "D:/Rdata/Chill_quantification/SWC/historic/plots/"

# North
RabatiBala <- image_read(paste0(folder, "Rabat-i-Bala.png"))
TangiTashqurghan <- image_read(paste0(folder, "Tang-i-Tashqurghan.png"))
Chichakto <- image_read(paste0(folder, "Chichakto.png"))
Dawlatabad <- image_read(paste0(folder, "Dawlatabad.png"))
KheshtPul <- image_read(paste0(folder, "Khesht-Pul.png"))
Qaisar <- image_read(paste0(folder, "Qaisar.png"))
DaraiZhwandon <- image_read(paste0(folder, "Dara-i-Zhwandon.png"))
Delmarogh <- image_read(paste0(folder, "Delmarogh.png"))
Sayad <- image_read(paste0(folder, "Sayad.png"))
NazdikiKeshandeh <- image_read(paste0(folder, "Nazdik-i-Keshandeh.png"))

# RabatiBala <- image_annotate(RabatiBala, "Rabat-i-Bala", gravity = "west", 
#                                        location = "+0-430", color = "black", size = 80)


N1 <- image_append(c(RabatiBala, TangiTashqurghan, Chichakto, Dawlatabad))
N2 <- image_append(c(KheshtPul, Qaisar, DaraiZhwandon, Delmarogh))
N3 <- image_append(c(Sayad, NazdikiKeshandeh))

N10 <- image_append(c(N1, N2, N3), stack = TRUE)

image_write(N10,path=paste0(folder,"N10.png"))

# NE
Anjuman <- image_read(paste0(folder, "Anjuman.png"))
Baharak <- image_read(paste0(folder, "Baharak.png"))
Eshkashem <- image_read(paste0(folder, "Eshkashem.png"))
Faizabad <- image_read(paste0(folder, "Faizabad.png"))
Keshem <- image_read(paste0(folder, "Keshem.png"))
Baghlan <- image_read(paste0(folder, "Baghlan.png"))
Doshi <- image_read(paste0(folder, "Doshi.png"))
Khenjan <- image_read(paste0(folder, "Khenjan.png"))
TangiNahrin <- image_read(paste0(folder, "TangiNahrin.png"))
PuliAlchin <- image_read(paste0(folder, "PuliAlchin.png"))
NazdikiTaluqan <- image_read(paste0(folder, "NazdikiTaluqan.png"))
PuliBangi <- image_read(paste0(folder, "PuliBangi.png"))

NE1 <- image_append(c(Anjuman, Baharak, Eshkashem, Faizabad))
NE2 <- image_append(c(Keshem, Baghlan, Doshi, Khenjan))
NE3 <- image_append(c(TangiNahrin, PuliAlchin,NazdikiTaluqan,PuliBangi))

NE12 <- image_append(c(NE1, NE2, NE3), stack = TRUE)

image_write(NE12,path=paste0(folder,"NE12.png"))

# East

Gardiz <- image_read(paste0(folder, "Gardiz.png"))
Maton <- image_read(paste0(folder, "Maton.png"))
Asmar <- image_read(paste0(folder, "Asmar.png"))
Nawabad <- image_read(paste0(folder, "Nawabad.png"))
PuliQarghayi <- image_read(paste0(folder, "PuliQarghayi.png"))
Dakah <- image_read(paste0(folder, "Dakah.png"))
PuliBehsod <- image_read(paste0(folder, "PuliBehsod.png"))

E1 <- image_append(c(Gardiz, Maton, Asmar, Nawabad))
E2 <- image_append(c(PuliQarghayi, Dakah, PuliBehsod))

E7 <- image_append(c(E1, E2), stack = TRUE)

image_write(E7, path = paste0(folder, "E7.png"))

# South
Darwishan <- image_read(paste0(folder, "Darwishan.png"))
Lashkargah <- image_read(paste0(folder, "Lashkargah.png"))
NazdikiKandahar <- image_read(paste0(folder, "NazdikiKandahar.png"))
Tirin <- image_read(paste0(folder, "Tirin.png"))

S4 <- image_append(c(Darwishan, Lashkargah, NazdikiKandahar, Tirin))

image_write(S4, path = paste0(folder, "S4.png"))

# West
Farah <- image_read(paste0(folder, "Farah.png"))
Adraskan <- image_read(paste0(folder, "Adraskan.png"))
PuliHashemi <- image_read(paste0(folder, "PuliHashemi.png"))
TagabiGhaza <- image_read(paste0(folder, "TagabiGhaza.png"))
Torghundi <- image_read(paste0(folder, "Torghundi.png"))

W1 <- image_append(c(Farah, Adraskan, PuliHashemi, TagabiGhaza))
W2 <- image_append(c(Torghundi))

W5 <- image_append(c(W1, W2), stack = TRUE)
image_write(W5, path = paste0(folder, "W5.png"))

# Central
Bamyan <- image_read(paste0(folder, "Bamyan.png"))
DashteiSafid <- image_read(paste0(folder, "DashteiSafid.png"))
NazdikiNayak <- image_read(paste0(folder, "NazdikiNayak.png"))
Waras <- image_read(paste0(folder, "Waras.png"))
PuliGhazni <- image_read(paste0(folder, "PuliGhazni.png"))
Cheghcheran <- image_read(paste0(folder, "Cheghcheran.png"))
Dawlatyar <- image_read(paste0(folder, "Dawlatyar.png"))
Estalef <- image_read(paste0(folder, "Estalef.png"))
Shakardara <- image_read(paste0(folder, "Shakardara.png"))
TangiSayedan <- image_read(paste0(folder, "TangiSayedan.png"))
Keraman <- image_read(paste0(folder, "Keraman.png"))
BaghiOmomi <- image_read(paste0(folder, "BaghiOmomi.png"))
Gardandiwal <- image_read(paste0(folder, "Gardandiwal.png"))

C1 <- image_append(c(Bamyan, DashteiSafid, NazdikiNayak, Waras))
C2 <- image_append(c(PuliGhazni, Cheghcheran, Dawlatyar, Estalef))
C3 <- image_append(c(Shakardara, TangiSayedan, Keraman, BaghiOmomi))
C4 <- image_append(c(Gardandiwal))

C13 <- image_append(c(C1, C2, C3, C4), stack = TRUE)
image_write(C13, path = paste0(folder, "C13.png"))

half1 <- image_append(c(N10,NE12), stack = TRUE)
image_write(half1, path = paste0(folder, "half1.png"))

half2 <- image_append(c(E7,S4,W5,C13), stack = TRUE)
image_write(half2, path = paste0(folder, "half2.png"))

# #2 Combining sub plots without sub-regions
# R1 <- image_append(c(Adraskan, Anjuman, Asmar,BaghiOmomi ))
# R2 <- image_append(c(Baghlan, Baharak,Bamyan,Cheghcheran))
# R3 <- image_append(c(Chichakto, Dakah,DaraiZhwandon,Darwishan))
# R4 <- image_append(c(DashteiSafid, Dawlatabad,Dawlatyar, Delmarogh))
# R5 <- image_append(c(Doshi, Eshkashem,Estalef,Faizabad))
# R6 <- image_append(c(Farah, Gardandiwal, Gardiz, Keraman))
# 
# P24 <- image_append(c(R1, R2, R3, R4, R5, R6), stack = TRUE)
# image_write(P24,path=paste0(folder,"P24.png"))
# 
# R7 <- image_append(c(Keshem, Khenjan, KheshtPul, Lashkargah))
# R8 <- image_append(c(Maton, Nawabad, NazdikiKandahar, NazdikiKeshandeh))
# R9 <- image_append(c(NazdikiNayak, NazdikiTaluqan, PuliAlchin, PuliBangi))
# R10 <- image_append(c(PuliBehsod, PuliGhazni, PuliHashemi, PuliQarghayi))
# R11 <- image_append(c(Qaisar, RabatiBala, Sayad, Shakardara))
# R12 <- image_append(c(TagabiGhaza, TangiNahrin, TangiSayedan, TangiTashqurghan))
# R13 <- image_append(c(Tirin, Torghundi, Waras))
# 
# P26 <- image_append(c(R7, R8, R9, R10, R11, R12, R13), stack = TRUE)
# image_write(P26,path=paste0(folder,"P26.png"))



# # All stations
# Adraskan <- image_read(paste0(folder, "Adraskan.png"))
# Anjuman <- image_read(paste0(folder, "Anjuman.png"))
# Asmar <- image_read(paste0(folder, "Asmar.png"))
# BaghiOmomi <- image_read(paste0(folder, "Bagh-i-Omomi.png"))
# Baghlan <- image_read(paste0(folder, "Baghlan.png"))
# Baharak <- image_read(paste0(folder, "Baharak.png"))
# Bamyan <- image_read(paste0(folder, "Bamyan.png"))
# Cheghcheran <- image_read(paste0(folder, "Cheghcheran.png"))
# Chichakto <- image_read(paste0(folder, "Chichakto.png"))
# Dakah <- image_read(paste0(folder, "Dakah.png"))
# DaraiZhwandon <- image_read(paste0(folder, "Dara-i-Zhwandon.png"))
# Darwishan <- image_read(paste0(folder, "Darwishan.png"))
# DashteiSafid <- image_read(paste0(folder, "Dashte-i-Safid.png"))
# Dawlatabad <- image_read(paste0(folder, "Dawlatabad.png"))
# Dawlatyar <- image_read(paste0(folder, "Dawlatyar.png"))
# Delmarogh <- image_read(paste0(folder, "Delmarogh.png"))
# Doshi <- image_read(paste0(folder, "Doshi.png"))
# Eshkashem <- image_read(paste0(folder, "Eshkashem.png"))
# Estalef <- image_read(paste0(folder, "Estalef.png"))
# Faizabad <- image_read(paste0(folder, "Faizabad.png"))
# Farah <- image_read(paste0(folder, "Farah.png"))
# Gardandiwal <- image_read(paste0(folder, "Gardandiwal.png"))
# Gardiz <- image_read(paste0(folder, "Gardiz.png"))
# Keraman <- image_read(paste0(folder, "Keraman.png"))
# Keshem <- image_read(paste0(folder, "Keshem.png"))
# Khenjan <- image_read(paste0(folder, "Khenjan.png"))
# KheshtPul <- image_read(paste0(folder, "Khesht-Pul.png"))
# Lashkargah <- image_read(paste0(folder, "Lashkargah.png"))
# Maton <- image_read(paste0(folder, "Maton.png"))
# Nawabad <- image_read(paste0(folder, "Nawabad.png"))
# NazdikiKandahar <- image_read(paste0(folder, "Nazdik-i-Kandahar.png"))
# NazdikiKeshandeh <- image_read(paste0(folder, "Nazdik-i-Keshandeh.png"))
# NazdikiNayak <- image_read(paste0(folder, "Nazdik-i-Nayak.png"))
# NazdikiTaluqan <- image_read(paste0(folder, "Nazdik-i-Taluqan.png"))
# PuliAlchin <- image_read(paste0(folder, "Pul-i-Alchin.png"))
# PuliBangi <- image_read(paste0(folder, "Pul-i-Bangi.png"))
# PuliBehsod <- image_read(paste0(folder, "Pul-i-Behsod.png"))
# PuliGhazni <- image_read(paste0(folder, "Pul-i-Ghazni.png"))
# PuliHashemi <- image_read(paste0(folder, "Pul-i-Hashemi.png"))
# PuliQarghayi <- image_read(paste0(folder, "Pul-i-Qarghayi.png"))
# Qaisar <- image_read(paste0(folder, "Qaisar.png"))
# RabatiBala <- image_read(paste0(folder, "Rabat-i-Bala.png"))
# Sayad <- image_read(paste0(folder, "Sayad.png"))
# Shakardara <- image_read(paste0(folder, "Shakardara.png"))
# TagabiGhaza <- image_read(paste0(folder, "Tagab-i-Ghaza.png"))
# TangiNahrin <- image_read(paste0(folder, "Tang-i-Nahrin.png"))
# TangiSayedan <- image_read(paste0(folder, "Tang-i-Sayedan.png"))
# TangiTashqurghan <- image_read(paste0(folder, "Tang-i-Tashqurghan.png"))
# Tirin <- image_read(paste0(folder, "Tirin.png"))
# Torghundi <- image_read(paste0(folder, "Torghundi.png"))
# Waras <- image_read(paste0(folder, "Waras.png"))


# Read all saved plots and add names to it
# List of image names and corresponding annotations
image_names <- c("Adraskan.png", "Anjuman.png", "Asmar.png", "Bagh-i-Omomi.png", "Baghlan.png",
                 "Baharak.png", "Bamyan.png", "Cheghcheran.png", "Chichakto.png", "Dakah.png",
                 "Dara-i-Zhwandon.png", "Darwishan.png", "Dashte-i-Safid.png", "Dawlatabad.png",
                 "Dawlatyar.png", "Delmarogh.png", "Doshi.png", "Eshkashem.png", "Estalef.png",
                 "Faizabad.png", "Farah.png", "Gardandiwal.png", "Gardiz.png", "Keraman.png",
                 "Keshem.png", "Khenjan.png", "Khesht-Pul.png", "Lashkargah.png", "Maton.png",
                 "Nawabad.png", "Nazdik-i-Kandahar.png", "Nazdik-i-Keshandeh.png", "Nazdik-i-Nayak.png",
                 "Nazdik-i-Taluqan.png", "Pul-i-Alchin.png", "Pul-i-Bangi.png", "Pul-i-Behsod.png",
                 "Pul-i-Ghazni.png", "Pul-i-Hashemi.png", "Pul-i-Qarghayi.png", "Qaisar.png",
                 "Rabat-i-Bala.png", "Sayad.png", "Shakardara.png", "Tagab-i-Ghaza.png",
                 "Tang-i-Nahrin.png", "Tang-i-Sayedan.png", "Tang-i-Tashqurghan.png", "Tirin.png",
                 "Torghundi.png", "Waras.png")

annotations <- c("Adraskan (ADN)", "Anjuman (AJN)", "Asmar (ASR)", "Bagh-i-Omomi (BOI)", "Baghlan (BLN)",
                 "Baharak (BRK)", "Bamyan (BYN)", "Cheghcheran (CCN)", "Chichakto (CCO)", "Dakah (DKH)",
                 "Dara-i-Zhwandon (DZN)", "Darwishan (DSN)", "Dashte-i-Safid (DSD)", "Dawlatabad (DBD)",
                 "Dawlatyar (DYR)", "Delmarogh (DMH)", "Doshi (DSI)", "Eshkashem (EKM)", "Estalef (ETF)",
                 "Faizabad (FZD)", "Farah (FRH)", "Gardandiwal (GDL)", "Gardiz (GDZ)", "Keraman (KRN)",
                 "Keshem (KSM)", "Khenjan (KJN)", "Khesht-Pul (KPL)", "Lashkargah (LGH)", "Maton (MTN)",
                 "Nawabad (NBD)", "Nazdik-i-Kandahar (NKR)", "Nazdik-i-Keshandeh (NKH)", "Nazdik-i-Nayak (NNK)",
                 "Nazdik-i-Taluqan (NTN)", "Pul-i-Alchin (PAN)", "Pul-i-Bangi (PBI)", "Pul-i-Behsod (PBD)",
                 "Pul-i-Ghazni (PGI)", "Pul-i-Hashemi (PHI)", "Pul-i-Qarghayi (PQI)", "Qaisar (QSR)",
                 "Rabat-i-Bala (RBA)", "Sayad (SYD)", "Shakardara (SDA)", "Tagab-i-Ghaza (TGA)",
                 "Tang-i-Nahrin (TNN)", "Tang-i-Sayedan (TSN)", "Tang-i-Tashqurghan (TTN)", "Tirin (TRN)",
                 "Torghundi (TGI)", "Waras (WRS)")
                 
# Iterate over each pair of image name and annotation
for (i in seq_along(image_names)) {
  # Read the image
  image <- image_read(paste0(folder, image_names[i]))
  
  output_file <- paste0(folder, "annotated_", image_names[i])
  
  # Annotate the image with the specific name
  #image <- image_annotate(image, annotations[i], gravity = "west", location = "+30-410", color = "black", size = 100)
  image <- image_annotate(image, annotations[i], gravity = "north", location = "+0-8", color = "black", size = 100)
  
  image <- image_border(image = image, color = "gray90")
  image_write(image, output_file)
  
}

# annotate Nawabad individually
image <- image_read(paste0(folder, "test.png"))

output_file <- paste0(folder, "annotated_Nawabad.png")

# Annotate the image with the specific name
image <- image_annotate(image, text = "Nawabad (NBD)",  gravity = "north", location = "+0-18", color = "black", size = 120)

image <- image_border(image = image, color = "gray90")
image_write(image, output_file)


# # for blank figure to fill space
# Blank_plot <- image_read(paste0(folder, "1Blank.png"))
# Blank_plot <- image_annotate(Blank_plot, "", gravity = "north", location = "+0+20", color = "black", size = 100)
# Blank_plot <- image_border(image = Blank_plot, color = "gray90")
# image_write(Blank_plot,path=paste0(folder,"Blank.png"))
# read blank image
Blank <- image_read(paste0(folder, "Blank.png"))


# North
RabatiBala <- image_read(paste0(folder, "Annotated_Rabat-i-Bala.png"))
TangiTashqurghan <- image_read(paste0(folder, "Annotated_Tang-i-Tashqurghan.png"))
Chichakto <- image_read(paste0(folder, "Annotated_Chichakto.png"))
Dawlatabad <- image_read(paste0(folder, "Annotated_Dawlatabad.png"))
KheshtPul <- image_read(paste0(folder, "Annotated_Khesht-Pul.png"))
Qaisar <- image_read(paste0(folder, "Annotated_Qaisar.png"))
DaraiZhwandon <- image_read(paste0(folder, "Annotated_Dara-i-Zhwandon.png"))
Delmarogh <- image_read(paste0(folder, "Annotated_Delmarogh.png"))
Sayad <- image_read(paste0(folder, "Annotated_Sayad.png"))
NazdikiKeshandeh <- image_read(paste0(folder, "Annotated_Nazdik-i-Keshandeh.png"))

# 4 plots per row
# N1 <- image_append(c(RabatiBala, TangiTashqurghan, Chichakto, Dawlatabad))
# N2 <- image_append(c(KheshtPul, Qaisar, DaraiZhwandon, Delmarogh))
# N3 <- image_append(c(Sayad, NazdikiKeshandeh))
# 
# N10 <- image_append(c(N1, N2, N3), stack = TRUE)

# 5 plots per row 
N1 <- image_append(c(RabatiBala, TangiTashqurghan, Chichakto, Dawlatabad, KheshtPul))
N2 <- image_append(c(Qaisar, DaraiZhwandon, Delmarogh,Sayad, NazdikiKeshandeh))
N10 <- image_append(c(N1, N2), stack = TRUE)

# create an upper edge to write name of subregions
upper_edge<-image_blank(3300,110,color="white")

N10<-image_append(c(upper_edge,N10),stack=TRUE)
# test the dimension of of image to put text at center position (use: image_info)
N10<-image_annotate(N10, 'North', size = 135,location = "+3420-45",
                             color = "black",degrees=0,
                             weight = 800)
image_write(N10,path=paste0(folder,"N10.png"))


# Northeast
Anjuman <- image_read(paste0(folder, "Annotated_Anjuman.png"))
Baharak <- image_read(paste0(folder, "Annotated_Baharak.png"))
Eshkashem <- image_read(paste0(folder, "Annotated_Eshkashem.png"))
Faizabad <- image_read(paste0(folder, "Annotated_Faizabad.png"))
Keshem <- image_read(paste0(folder, "Annotated_Keshem.png"))
Baghlan <- image_read(paste0(folder, "Annotated_Baghlan.png"))
Doshi <- image_read(paste0(folder, "Annotated_Doshi.png"))
Khenjan <- image_read(paste0(folder, "Annotated_Khenjan.png"))
TangiNahrin <- image_read(paste0(folder, "Annotated_Tang-i-Nahrin.png"))
PuliAlchin <- image_read(paste0(folder, "Annotated_Pul-i-Alchin.png"))
NazdikiTaluqan <- image_read(paste0(folder, "Annotated_Nazdik-i-Taluqan.png"))
PuliBangi <- image_read(paste0(folder, "Annotated_Pul-i-Bangi.png"))

# NE1 <- image_append(c(Anjuman, Baharak, Eshkashem, Faizabad))
# NE2 <- image_append(c(Keshem, Baghlan, Doshi, Khenjan))
# NE3 <- image_append(c(TangiNahrin, PuliAlchin,NazdikiTaluqan,PuliBangi))
# 
# NE12 <- image_append(c(NE1, NE2, NE3), stack = TRUE)
# 
# image_write(NE12,path=paste0(folder,"NE12.png"))

NE1 <- image_append(c(Anjuman, Baharak, Eshkashem, Faizabad,Keshem))
NE2 <- image_append(c(Baghlan, Doshi, Khenjan,TangiNahrin, PuliAlchin))
NE3 <- image_append(c(NazdikiTaluqan,PuliBangi, Blank, Blank, Blank))
NE12 <- image_append(c(NE1, NE2, NE3), stack = TRUE)

NE12<-image_append(c(upper_edge,NE12),stack=TRUE)
# test the dimension of of image to put text at center position (use: image_info)
NE12<-image_annotate(NE12, 'Northeast', size = 135,location = "+3300-45",
                    color = "black",degrees=0,
                    weight = 800)
image_write(NE12,path=paste0(folder,"NE12.png"))

# East

Nawabad <- image_read(paste0(folder, "Annotated_Nawabad.png"))
Asmar <- image_read(paste0(folder, "Annotated_Asmar.png"))
PuliQarghayi <- image_read(paste0(folder, "Annotated_Pul-i-Qarghayi.png"))
Dakah <- image_read(paste0(folder, "Annotated_Dakah.png"))
PuliBehsod <- image_read(paste0(folder, "Annotated_Pul-i-Behsod.png"))
Gardiz <- image_read(paste0(folder, "Annotated_Gardiz.png"))
Maton <- image_read(paste0(folder, "Annotated_Maton.png"))

# E1 <- image_append(c(Gardiz, Maton, Asmar, Nawabad))
# E2 <- image_append(c(PuliQarghayi, Dakah, PuliBehsod))
# 
# E7 <- image_append(c(E1, E2), stack = TRUE)
# 
# image_write(E7, path = paste0(folder, "E7.png"))

E1 <- image_append(c(Blank,Blank, Asmar,Dakah, PuliQarghayi))
E2 <- image_append(c(Blank, Blank, PuliBehsod, Gardiz, Maton))
E7 <- image_append(c(E1, E2), stack = TRUE)

E7<-image_append(c(upper_edge,E7),stack=TRUE)
# test the dimension of of image to put text at center position (use: image_info)
E7<-image_annotate(E7, 'East', size = 135,location = "+3420-45",
                     color = "black",degrees=0,
                     weight = 800)
image_write(E7, path = paste0(folder, "E7.png"))

# put Nawabad on the empty space in the E7
E7 <- image_read(paste0(folder, "E7.png"))

composite_image <- image_composite(E7, Nawabad, offset = "+10+120", compose = "over")

# Write the resulting image to file
image_write(composite_image, path = paste0(folder, "E7_new.png"))


# South
Darwishan <- image_read(paste0(folder, "Annotated_Darwishan.png"))
Lashkargah <- image_read(paste0(folder, "Annotated_Lashkargah.png"))
NazdikiKandahar <- image_read(paste0(folder, "Annotated_Nazdik-i-Kandahar.png"))
Tirin <- image_read(paste0(folder, "Annotated_Tirin.png"))

S4 <- image_append(c(Darwishan, Lashkargah, NazdikiKandahar, Tirin, Blank))

S4<-image_append(c(upper_edge,S4),stack=TRUE)
# test the dimension of of image to put text at center position (use: image_info)
S4<-image_annotate(S4, 'South', size = 135,location = "+3420-45",
                   color = "black",degrees=0,
                   weight = 800)
image_write(S4, path = paste0(folder, "S4.png"))

# West
Farah <- image_read(paste0(folder, "Annotated_Farah.png"))
Adraskan <- image_read(paste0(folder, "Annotated_Adraskan.png"))
PuliHashemi <- image_read(paste0(folder, "Annotated_Pul-i-Hashemi.png"))
TagabiGhaza <- image_read(paste0(folder, "Annotated_Tagab-i-Ghaza.png"))
Torghundi <- image_read(paste0(folder, "Annotated_Torghundi.png"))

W5 <- image_append(c(Farah, Adraskan, PuliHashemi, TagabiGhaza, Torghundi))

W5<-image_append(c(upper_edge,W5),stack=TRUE)
# test the dimension of of image to put text at center position (use: image_info)
W5<-image_annotate(W5, 'West', size = 135,location = "+3420-45",
                   color = "black",degrees=0,
                   weight = 800)
image_write(W5, path = paste0(folder, "W5.png"))

# Central
Bamyan <- image_read(paste0(folder, "Annotated_Bamyan.png"))
DashteiSafid <- image_read(paste0(folder, "Annotated_Dashte-i-Safid.png"))
NazdikiNayak <- image_read(paste0(folder, "Annotated_Nazdik-i-Nayak.png"))
Waras <- image_read(paste0(folder, "Annotated_Waras.png"))
PuliGhazni <- image_read(paste0(folder, "Annotated_Pul-i-Ghazni.png"))
Cheghcheran <- image_read(paste0(folder, "Annotated_Cheghcheran.png"))
Dawlatyar <- image_read(paste0(folder, "Annotated_Dawlatyar.png"))
Estalef <- image_read(paste0(folder, "Annotated_Estalef.png"))
Shakardara <- image_read(paste0(folder, "Annotated_Shakardara.png"))
TangiSayedan <- image_read(paste0(folder, "Annotated_Tang-i-Sayedan.png"))
Keraman <- image_read(paste0(folder, "Annotated_Keraman.png"))
BaghiOmomi <- image_read(paste0(folder, "Annotated_Bagh-i-Omomi.png"))
Gardandiwal <- image_read(paste0(folder, "Annotated_Gardandiwal.png"))

C1 <- image_append(c(Bamyan, DashteiSafid, NazdikiNayak, Waras,PuliGhazni))
C2 <- image_append(c(Cheghcheran, Dawlatyar, Estalef,Shakardara, TangiSayedan))
C3 <- image_append(c(Keraman, BaghiOmomi,Gardandiwal,Blank,Blank))

C13 <- image_append(c(C1, C2, C3), stack = TRUE)


C13<-image_append(c(upper_edge,C13),stack=TRUE)
# test the dimension of of image to put text at center position (use: image_info)
C13<-image_annotate(C13, 'Central', size = 135,location = "+3400-45",
                   color = "black",degrees=0,
                   weight = 800)
image_write(C13, path = paste0(folder, "C13.png"))

# half1 <- image_append(c(N10,NE12), stack = TRUE)
# image_write(half1, path = paste0(folder, "half1.png"))
# 
# half2 <- image_append(c(E7,S4,W5,C13), stack = TRUE)
# image_write(half2, path = paste0(folder, "half2.png"))




# Future analysis using 2020 as a baseline for comparsion
# Future analysis using 2020 as a baseline for comparsion
# Future analysis using 2020 as a baseline for comparsion


# SSPs <- c("ssp126", "ssp245", "ssp370", "ssp585")
# Times <- c(2050, 2085)
# GCMs <- c("BCC-CSM2-MR","GFDL-ESM4","INM-CM4-8","INM-CM5-0","MRI-ESM2-0")
# 
# f <- list.files("D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics", 
#                 full.names = TRUE, pattern = "*future_data.csv")
# future_chill_list <- lapply(f, read.csv)
# 
# f <- list.files("D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics", pattern = "*future_data.csv")
# 
# station_future <- strsplit(f, '_') %>% 
#   purrr::map(1)%>%
#   unlist()
# 
# names(future_chill_list) <- station_future
# 
# chill_summary_df_f <- data.frame(NULL)
# 
# for (ssp in SSPs){
#   for (time in Times){
#     for (gcm in GCMs){
#       for(stat in names(future_chill_list)){
#         # ssp <- SSPs[1]
#         # time <- Times[1]
#         # gcm <- GCMs[1]
#         # stat <- names(future_chill_list)[1]
#         
#         future_sub <-  future_chill_list[[stat]] %>% 
#           mutate(SSP = tolower(SSP)) %>% 
#           filter(GCM == gcm, SSP == ssp, Year ==  time)
#         
#         
#         
#         station_match <-  which(station == stat)
#         year_match <- which(year[station_match] == "2020")
#         
#         hist_sub <-  historic_chill_list[[station_match[year_match]]]
#         
#         min_cp_2020 <- min(hist_sub$Chill_CP)
#         percent_lower <- sum(future_sub$Chill_CP < min_cp_2020)
#         
#         #cp_lower <- future_sub[future_sub$Chill_CP < min_cp_2020, ]
#         #cp_lower <- future_sub$Chill_CP[future_sub$Chill_CP < min_cp_2020]
#         
#         max_cp_2020 <- max(hist_sub$Chill_CP) 
#         percent_higher <- sum(future_sub$Chill_CP > max_cp_2020)
#         
# 
# 
#         chill_summary_df_f <- rbind(chill_summary_df_f,
#                                     data.frame(station = stat,
#                                                baseline_year = 2020, 
#                                                comparison_year = time,
#                                                SSP = ssp,
#                                                GCM = gcm,
#                                                min_cp_baseline = min_cp_2020,
#                                                #CP_lower = cp_lower,
#                                                percent_lower = percent_lower,
#                                                max_cp_baseline = max_cp_2020,
#                                                percent_higher = percent_higher))
#         
#         
#       }
#     }
#   }
# }



# Future analysis using "Mean historic simulated scenarios" as a baseline for comparsion
# Future analysis using "Mean historic simulated scenarios" as a baseline for comparsion
# Future analysis using "Mean historic simulated scenarios" as a baseline for comparsion


h <- list.files('Future_scenarios/chill/historic_chill/', full.names = TRUE)
historic_chill_list <- lapply(h, read.csv)

h <- list.files('Future_scenarios/chill/historic_chill/')

station <- strsplit(h, '_') %>% 
  purrr::map(1)%>%
  unlist()

year <- strsplit(h, '_') %>% 
  purrr::map(function(x)  x[length(x)]) %>% 
  str_split('\\.') %>% 
  purrr::map(1) %>% 
  unlist()

SSPs <- c("ssp126", "ssp245", "ssp370", "ssp585")
Times <- c(2050, 2085)
GCMs <- c("BCC-CSM2-MR","GFDL-ESM4","INM-CM4-8","INM-CM5-0","MRI-ESM2-0")

f <- list.files("D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics", 
                full.names = TRUE, pattern = "*future_data.csv")
future_chill_list <- lapply(f, read.csv)

f <- list.files("D:/Rdata/Chill_quantification/Future_scenarios/chill/metrics", pattern = "*future_data.csv")

station_future <- strsplit(f, '_') %>% 
  purrr::map(1)%>%
  unlist()

names(future_chill_list) <- station_future

chill_summary_df_f <- data.frame(NULL)

for (ssp in SSPs){
  for (time in Times){
    for (gcm in GCMs){
      for(stat in names(future_chill_list)){
        # ssp <- SSPs[1]
        # time <- Times[1]
        # gcm <- GCMs[1]
        # stat <- names(future_chill_list)[1]
        
        future_sub <-  future_chill_list[[stat]] %>% 
          mutate(SSP = tolower(SSP)) %>% 
          filter(GCM == gcm, SSP == ssp, Year ==  time)
  
        matching_station_1980 <-  which(station == stat)
        matching_station_1990 <-  which(station == stat)
        matching_station_2000 <-  which(station == stat)
        matching_station_2010 <-  which(station == stat)
        matching_station_2020 <-  which(station == stat)
        
        matching_year_1980 <- which(year[matching_station_1980] == '1980')
        matching_year_1990 <- which(year[matching_station_1990] == '1990')
        matching_year_2000 <- which(year[matching_station_2000] == '2000')
        matching_year_2010 <- which(year[matching_station_2010] == '2010')
        matching_year_2020 <- which(year[matching_station_2020] == '2020')
        
        chill_1980 <-  historic_chill_list[[matching_station_1980[matching_year_1980]]]
        chill_1990 <-  historic_chill_list[[matching_station_1990[matching_year_1990]]]
        chill_2000 <-  historic_chill_list[[matching_station_2000[matching_year_2000]]]
        chill_2010 <-  historic_chill_list[[matching_station_2010[matching_year_2010]]]
        chill_2020 <-  historic_chill_list[[matching_station_2020[matching_year_2020]]]
        
        # now take the mean of all historic
        # Bind the data frames into one
        combined_chill_df <- rbind(chill_1980, chill_1990, chill_2000, chill_2010, chill_2020)
        
        # Calculate the mean of the specified columns
        hist_sub_mean <- combined_chill_df %>%
          group_by(Season, End_year, Season_days, Data_days, Perc_complete) %>%
          summarize(
            Mean_Chill_CP = mean(Chill_CP, na.rm = TRUE),
            Mean_Heat_GDH = mean(Heat_GDH, na.rm = TRUE),
            Mean_Frost_H = mean(Frost_H, na.rm = TRUE)
          )
        
        # Now take min and max for baseline
        min_cp_hist <- min(hist_sub_mean$Mean_Chill_CP)
        max_cp_hist <- max(hist_sub_mean$Mean_Chill_CP)
        
        percent_lower <- sum(future_sub$Chill_CP < min_cp_hist)
        percent_higher <- sum(future_sub$Chill_CP > max_cp_hist)
        
        # Extra step to add mean chill value
        mean_chill <- mean(future_sub$Chill_CP)
        
        chill_summary_df_f <- rbind(chill_summary_df_f,
                                    data.frame(station = stat,
                                               #baseline_year = 2020, 
                                               comparison_year = time,
                                               SSP = ssp,
                                               GCM = gcm,
                                               Mean_Chill = mean_chill,
                                               min_hist_cp_baseline = min_cp_hist,
                                               #CP_lower = cp_lower,
                                               percent_lower = percent_lower,
                                               max_hist_cp_baseline = max_cp_hist,
                                               percent_higher = percent_higher))
        
        
      }
    }
  }
}

# Find out which stations showed the highest and lowest % values of overlap for each scenario
# with GCM
station_max_per_lower <- chill_summary_df_f %>%
  group_by(SSP, GCM, comparison_year) %>%
  filter(percent_lower == max(percent_lower)) %>%
  ungroup()
write.csv(station_max_per_lower, "D:/Rdata/Chill_quantification/SWC/future/percent_overlap_f/max_overlap_below_min.csv", row.names = FALSE)

# without GCM
station_below_minbaseline <- chill_summary_df_f %>%
  group_by(SSP, comparison_year) %>%
  filter(percent_lower == max(percent_lower)) %>%
  ungroup()

station_max_per_higher <- chill_summary_df_f %>%
  group_by(SSP, GCM, comparison_year) %>%
  filter(percent_higher == max(percent_higher)) %>%
  ungroup()
write.csv(station_max_per_higher, "D:/Rdata/Chill_quantification/SWC/future/percent_overlap_f/max_overlap_above_max.csv", row.names = FALSE)

# without GCM
station_above_maxbaseline <- chill_summary_df_f %>%
  group_by(SSP, comparison_year) %>%
  filter(percent_lower == max(percent_lower)) %>%
  ungroup()

# To ease analysis lets group by ssps and year by taking mean of gcms
grouped_summary <- chill_summary_df_f %>%
  group_by(station, comparison_year, SSP, min_hist_cp_baseline, max_hist_cp_baseline) %>%
  summarise(
    median_percent_lower = median(percent_lower, na.rm = TRUE),
    median_percent_higher = median(percent_higher, na.rm = TRUE)) %>%
  ungroup()

# highest percent lower values for stations 
highest_percent_lower <- grouped_summary %>%
  group_by(SSP, comparison_year) %>%
  slice(which.max(median_percent_lower)) %>%
  ungroup()
write.csv(highest_percent_lower, "D:/Rdata/Chill_quantification/SWC/future/percent_overlap_f/highest_percent_lower.csv", row.names = FALSE)

# highest percent higher values for stations 
highest_percent_higher <- grouped_summary %>%
  group_by(SSP, comparison_year) %>%
  slice(which.max(median_percent_higher)) %>%
  ungroup()
write.csv(highest_percent_higher, "D:/Rdata/Chill_quantification/SWC/future/percent_overlap_f/highest_percent_higher.csv", row.names = FALSE)

# # We will pivot to have separate columns for each 'SSP' for the 'percent_lower' and 'percent_higher' values
# chill_summary_wide <- chill_summary_df_f %>%
#   pivot_wider(names_from = c(SSP, comparison_year), 
#               values_from = c(percent_lower, percent_higher),
#               names_sep = "_") # This will create new column names by combining the SSP value with the measure names
# 
# write.csv(chill_summary_wide, "D:/Rdata/Chill_quantification/SWC/future/percent_overlap_f/chill_summary_future_ssp.csv", row.names = FALSE)



# Checking the performance of GCMs
# Checking the performance of GCMs 
# Checking the performance of GCMs 

library(dplyr)
summary_df <- chill_summary_df_f %>%
  group_by(station, GCM) %>%
  summarize(
    Mean = mean(percent_lower, na.rm = TRUE),
    Median = median(percent_lower, na.rm = TRUE),
    SD = sd(percent_lower, na.rm = TRUE))

library(ggplot2)
ggplot(chill_summary_df_f, aes(x = GCM, y = Mean_Chill, fill = GCM)) +
  geom_boxplot() +
  facet_wrap(~ station) +
  theme_minimal() +
  labs(title = "Distribution of Mean Chill Across GCMs and Locations", x = "GCM", y = "Mean Chill")

ggplot(chill_summary_df_f, aes(x = station, y = GCM, fill = Mean_Chill)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Mean Chill for GCMs Across Different Locations", x = "Station", y = "GCM")




# Filter the DataFrame for BCC-CSM2-MR SSP126 2050
# Assuming 'filtered_adraskan' is your dataframe that contains the relevant data

filtered_BaghiOmomi <- future_chill_list$BaghiOmomi
# Apply the filter according to the given conditions
filtered_BaghiOmomi_gcm <- filtered_BaghiOmomi[
  filtered_BaghiOmomi$GCM == "BCC-CSM2-MR" & 
    filtered_BaghiOmomi$SSP == "SSP126" & 
    filtered_BaghiOmomi$Year == 2050, 
]

# Count the number of Chill_CP values that are below 50
count_below_min <- sum(filtered_BaghiOmomi_gcm$Chill_CP < 71.46486)

# Output the count
print(count_below_50)


# shape

chill_summ_fut <- chill_summary_df_f %>%
  pivot_wider(
    names_from = SSP,
    values_from = percent_lower
  )

# visualization

library(shiny)
library(ggplot2)
library(readr)

# Assuming your CSV is named 'chill_data.csv'
data <- chill_summary_df_f

ui <- fluidPage(
  titlePanel("Chill Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stationInput", "Choose a Station:", choices = unique(data$station))
    ),
    mainPanel(
      plotOutput("timeSeriesPlot")
    )
  )
)

server <- function(input, output) {
  output$timeSeriesPlot <- renderPlot({
    # Filter data based on selected station
    filtered_data <- data[data$station == input$stationInput, ]
    
    # Generate the time series plot
    ggplot(filtered_data, aes(x = comparison_year, y = min_cp_baseline, color = SSP)) +
      geom_line() +
      labs(title = paste("Chill Data for", input$stationInput),
           x = "Year",
           y = "Minimum Chill Portion",
           color = "Scenario") +
      theme_minimal()
  })
}

shinyApp(ui, server)


# Define UI
ui <- fluidPage(
  titlePanel("Chill Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stationInput", "Choose a Station:", choices = unique(data$station))
    ),
    mainPanel(
      plotOutput("timeSeriesPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$timeSeriesPlot <- renderPlot({
    # Filter data based on selected station
    filtered_data <- data[data$station == input$stationInput, ]
    
    # Calculate the chill values for comparison years
    filtered_data$estimated_chill_value <- filtered_data$min_cp_baseline * (1 - (filtered_data$percent_lower / 100))
    
    # Plotting the baseline chill value across all years for reference
    baseline_value <- min(filtered_data$min_cp_baseline)
    
    # Generate the plot
    ggplot(filtered_data, aes(x = comparison_year, y = estimated_chill_value, group = interaction(SSP, GCM), color = interaction(SSP, GCM))) +
      geom_hline(yintercept = baseline_value, linetype = "dashed", color = "blue", size = 1) +
      geom_line() +
      labs(title = paste("Chill Data for", input$stationInput),
           x = "Year",
           y = "Estimated Chill Value",
           color = "SSP & GCM") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui, server)



library(ggplot2)
library(dplyr)

# Assuming my dataframe is 'data'
adraskan_data <- data %>%
  filter(station == "Adraskan") %>%
  mutate(actual_value = min_cp_baseline * (1 - percent_lower / 100)) %>%
  # Create an interaction term with the correct order
  mutate(ssp_year = factor(paste(comparison_year, SSP),
                           levels = paste(rep(c("2050", "2085"), each = 4), 
                                          c("ssp126", "ssp245", "ssp370", "ssp585"))))


ggplot(adraskan_data, aes(x = ssp_year, y = actual_value, group = GCM, color = GCM)) +
  geom_smooth(se = FALSE) + # Smooth line without the original line
  geom_hline(yintercept = min(adraskan_data$min_cp_baseline), linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(0, 100)) +  # Set y-axis limits
  theme_minimal() +
  labs(title = "Smoothed Trend of Actual Values for Station Adraskan",
       x = "Comparison Year and SSP",
       y = "Actual Value (Percent Lower from Baseline)") +
  annotate("text", x = "ssp126", y = min(adraskan_data$min_cp_baseline), 
           label = "Minimum SWC in 2020", color = "red", vjust = -1)

  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# all plots
  

