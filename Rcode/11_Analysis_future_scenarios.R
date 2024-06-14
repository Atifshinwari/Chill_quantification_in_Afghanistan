
# ######## Loop Future Scenarios #######

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

#Loading temperature data (all stations)
Station_AT_List <-
  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs_Sim", prefix = "1980_2020_")

#Assigning the names
List_Names <- list.files("D:/Rdata/QDM/Stations_Obs")
Stations_Names <- str_split(List_Names, pattern = "_") %>%
  purrr::map_chr(1)
names(Station_AT_List) <- Stations_Names

Station_Latitudes <- c(33.63, 36.01, 34.91, 35.14, 36.10, 37.00, 34.82, 34.52, 35.72, 34.23, 36.16, 31.13, 35.31, 36.41, 34.54, 35.96, 35.60, 36.73,34.82, 37.11, 32.36, 34.50, 33.59, 35.28, 36.93, 35.55, 35.96, 31.58, 33.35, 34.81, 31.61, 36.18, 34.74, 36.63, 36.80, 36.73, 34.44, 33.55, 34.34, 34.54, 35.67, 36.58, 36.53, 34.68, 34.33, 36.05, 34.40, 36.65, 32.63, 35.25, 34.22)

# Load historic chill 2020
file_path2 <- "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill/"
file_name2 <- paste0(Stations_Names[i], "_historic")
#file_name3 <- paste0(Stations_Names[i], "_observed")

chill_past_scenarios <- load_temperature_scenarios(file_path2, file_name2)
chill_past_2020 <- chill_past_scenarios[["2020"]]

# put it in chills
chills <- make_climate_scenario(
  chill_past_2020,
  caption = "historic",
  historic_data =  chill_past_2020,
  time_series = TRUE)

# Load future chill for all scenarios
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

# create new columns in historic df
chill_past_2020$GCM <- NA
chill_past_2020$SSP <- "historic"
chill_past_2020$Year <- "2020"

# Calculate the change or difference between 2020 and each future scenario










# Analysis all the files of chill portions


# Directory paths
dir_future <- "D:/Rdata/Chill_quantification/Future_scenarios/chill/future_chill"
dir_historic <- "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill"

# For future scenarios
files_future <- list.files(dir_future, pattern="*.csv", full.names=TRUE)

# For historic scenarios
files_historic <- list.files(dir_historic, pattern="_historic_5_2020.csv", full.names=TRUE)

# # Combine both lists
# files <- c(files_future, files_historic)

# Read the future files
df_future <- lapply(files_future, function(file) {
  data <- read.csv(file)
  parts <- strsplit(basename(file), "_")[[1]]
  data$GCM <- parts[5]
  data$Scenario <- parts[2] # Extracting the scenario
  data$Time <- as.integer(parts[3])  # Extracting the time
  data$Station_ID <- parts[1] # Extracting the station name
  return(data)
})
df_future <- do.call(rbind, df_future)

# Read the historic files
df_historic <- lapply(files_historic, function(file) {
  data <- read.csv(file)
  data$Scenario <- "historic"
  data$GCM <- "historic"
  data$Time <- 2020  # Setting the time as 2020 for historic data
  data$Station_ID <- strsplit(basename(file), "_")[[1]][1]
  return(data)
})
df_historic <- do.call(rbind, df_historic)

# Convert both chill portions in to SWC
df_future$Chill_CP <- df_future$Chill_CP * 0.90
df_historic$Chill_CP <- df_historic$Chill_CP * 0.90

ssp<- unique(df_future$Scenario)[1]
time<- unique(df_future$Time)[1]
loc<- unique(df_future$Station_ID)[1]

posthoc_df <- data.frame()
for(ssp in unique(df_future$Scenario)){
  for(time in unique(df_future$Time)){
    for(loc in unique(df_future$Station_ID)){

      historic_subset <- df_historic %>% 
        filter(Station_ID == loc)
      
      combined_subset <- df_future %>% 
        filter(Time == time, 
               Scenario == ssp,
               Station_ID == loc) %>% 
        mutate(historic_chill = rep(historic_subset$Chill_CP, length(unique(df_future$GCM))))
      # adding quantile values for heat map later on
      Q5 <- quantile(combined_subset$Chill_CP, 0.05)
      Q50 <- quantile(combined_subset$Chill_CP, 0.50)
      Q95 <- quantile(combined_subset$Chill_CP, 0.95)
      
      
      test_out <- t.test(combined_subset$Chill_CP, combined_subset$historic_chill)
      wilcox_out <- wilcox.test(combined_subset$Chill_CP, combined_subset$historic_chill)
      ks_out <- ks.test(combined_subset$Chill_CP, combined_subset$historic_chill)
      ks_out$p.value

      shapiro_future <- shapiro.test(combined_subset$Chill_CP)
      shapiro_historic <- shapiro.test(combined_subset$historic_chill)
      
      
      if(shapiro_future$p.value < 0.05 | (shapiro_historic$p.value < 0.05 )){
        posthoc_df <- rbind(posthoc_df,
                            data.frame(Scenario = ssp,
                                       Location = loc, 
                                       Time = time,
                                       p_value = wilcox_out$p.value,
                                       Q_5 = Q5,
                                       Q_50= Q50,
                                       Q_95= Q95,
                                       test = "Wilcox"))

        
      } else {
        posthoc_df <- rbind(posthoc_df,
                            data.frame(Scenario = ssp,
                                       Location = loc, 
                                       Time = time,
                                       p_value = test_out$p.value,
                                       Q_5 = Q5,
                                       Q_50= Q50,
                                       Q_95= Q95,
                                       test = "T.test"))
      }

    }

  }
}


#posthoc_df$significant <- posthoc_df$p_value < 0.05
posthoc_df$p_value <- round(posthoc_df$p_value, 3)

library(tidyverse)

# Reshape the data to wide form
posthoc_wide <- posthoc_df %>% 
  unite("Location", Scenario, Time, test, sep = "_") %>%
  spread(key = Location, value = p_value, fill = NA) %>%
  spread(key = Location, value = Q_5, fill = NA) %>%
  spread(key = Location, value = Q_50, fill = NA) %>%
  spread(key = Location, value = Q_95, fill = NA)

write.csv(posthoc_df, "D:/Rdata/Chill_quantification/SWC/future/kendall_test/Willcox_test_future.csv", row.names = FALSE)

summary_posthoc_df <- posthoc_df %>%
  group_by(Scenario, Time, test) %>%
  summarize(
    significant_count = sum(significant),
    total_count = n(),
    Non_significant_stations = toString(Location[p_value > 0.05])
  )

write.csv(summary_posthoc_df, "D:/Rdata/Chill_quantification/SWC/future/kendall_test/summary_Willcox_test.csv", row.names = FALSE)

# heat map of p values
require(ggplot2)
require(dplyr)

posthoc_df$Scenario_Time <- paste(posthoc_df$Scenario, posthoc_df$Time, sep = "_")
ggplot(posthoc_df, aes(x = Location, y = Scenario_Time, fill = p_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "P-values Heatmap", x = "Stations", y = "Scenario_Time", fill = "P-value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# a try to also put median values of SWC
gg_Willcox_test <- ggplot(posthoc_df, aes(x = Location, y = paste(Scenario, Time, sep = "_"), fill = p_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = sprintf("%.2f", Q_50)), size = 3, color = "black") +
  theme_minimal() +
  labs(title = "P-values Heatmap with 50% Quantile of Chill_CP", x = "Stations", y = "Scenario_Time", fill = "P-value", color = "black") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(size = 12, colour = "black"))


# Make sure df_future and posthoc_df are properly set up as per earlier instructions

summary_Willcox_test <- ggplot(posthoc_df, aes(x = Scenario_Time, y = Location, fill = p_value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Q_50)), size = 3) +  # Display Q50 values
  scale_fill_gradient(low = "white", high = "red", name = "P-value") +
  theme_minimal() +
  labs(x = "Scenario_Time", y = "Location") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

print(summary_Willcox_test)


ggsave("D:/Rdata/Chill_quantification/SWC/future/kendall_test/summary_Willcox_test.png", plot = summary_Willcox_test, width = 20, height = 12, dpi = 600, bg = "white")


####################################################################################################

# Create the histogram to see normality of the data
hist(combined_subset$Chill_CP, main = "Histogram of my_data", xlab = "future_data")
hist(combined_subset$historic_chill,main = "Histogram of my_data", xlab = "historic_data")

# Visually both the temperature data seems normally distributed.

####################################################################################################
### Test in case Kolmogorove Smirnove test is needed ###
### Test in case Kolmogorove Smirnove test is needed ###
### Test in case Kolmogorove Smirnove test is needed ###

# library(dplyr)
# 
# # Initialize an empty dataframe to store the results
# posthoc_df <- data.frame(Scenario = character(),
#                          Location = character(),
#                          Time = character(),
#                          p_value = numeric(),
#                          test = character(),
#                          stringsAsFactors = FALSE)
# 
# # Loop through each unique combination of Scenario, Time, and Station_ID
# for(ssp in unique(df_future$Scenario)){
#   for(time in unique(df_future$Time)){
#     for(loc in unique(df_future$Station_ID)){
#       
#       # Filter the historic and future data for the current combination
#       historic_subset <- df_historic %>% 
#         filter(Station_ID == loc)
#       
#       combined_subset <- df_future %>% 
#         filter(Time == time, 
#                Scenario == ssp,
#                Station_ID == loc) %>% 
#         mutate(historic_chill = rep(historic_subset$Chill_CP, length(unique(df_future$GCM))))
#       
#       # Perform the Shapiro-Wilk test for normality on both datasets
#       shapiro_future <- shapiro.test(combined_subset$Chill_CP)
#       shapiro_historic <- shapiro.test(combined_subset$historic_chill)
#       
#       # Decide which test to use based on the normality test results
#       if(shapiro_future$p.value < 0.05 || shapiro_historic$p.value < 0.05) {
#         # If either dataset is not normally distributed, perform the Wilcoxon and K-S tests
#         wilcox_out <- wilcox.test(combined_subset$Chill_CP, combined_subset$historic_chill)
#         ks_out <- ks.test(combined_subset$Chill_CP, combined_subset$historic_chill)
#         
#         # Decide which non-parametric test to report based on your criteria (e.g., lowest p-value)
#         if(wilcox_out$p.value < ks_out$p.value){
#           posthoc_df <- rbind(posthoc_df, data.frame(Scenario = ssp, Location = loc, Time = time, p_value = wilcox_out$p.value, test = "Wilcox"))
#         } else {
#           posthoc_df <- rbind(posthoc_df, data.frame(Scenario = ssp, Location = loc, Time = time, p_value = ks_out$p.value, test = "K-S"))
#         }
#       } else {
#         # If both datasets are normally distributed, perform the t-test
#         test_out <- t.test(combined_subset$Chill_CP, combined_subset$historic_chill)
#         posthoc_df <- rbind(posthoc_df, data.frame(Scenario = ssp, Location = loc, Time = time, p_value = test_out$p.value, test = "T.test"))
#       }
#     }
#   }
# }
# 
# posthoc_df$significant <- posthoc_df$p_value < 0.05

####################################################################################################


####################################################################################################
############ Checking the individual effect of each climate models as compared to other ############
####################################################################################################

#Loading temperature data (all stations)
Station_AT_List <-
  chillR::load_temperature_scenarios("D:/Rdata/QDM/Stations_Obs_Sim", prefix = "1980_2020_")

#Assigning the names
List_Names <- list.files("D:/Rdata/QDM/Stations_Obs")
Stations_Names <- str_split(List_Names, pattern = "_") %>%
  purrr::map_chr(1)
names(Station_AT_List) <- Stations_Names

#Station_Latitudes <- c(33.63, 36.01, 34.91, 35.14, 36.10, 37.00, 34.82, 34.52, 35.72, 34.23, 36.16, 31.13, 35.31, 36.41, 34.54, 35.96, 35.60, 36.73,34.82, 37.11, 32.36, 34.50, 33.59, 35.28, 36.93, 35.55, 35.96, 31.58, 33.35, 34.81, 31.61, 36.18, 34.74, 36.63, 36.80, 36.73, 34.44, 33.55, 34.34, 34.54, 35.67, 36.58, 36.53, 34.68, 34.33, 36.05, 34.40, 36.65, 32.63, 35.25, 34.22)

# Load future chill for all scenarios
SSPs <- c("ssp126", "ssp245", "ssp370", "ssp585")
Times <- c(2050, 2085)
GCM <- c("BCC-CSM2-MR","GFDL-ESM4","INM-CM4-8","INM-CM5-0","MRI-ESM2-0")
Number <- c(1,2,3,4,5)

for (SSP in SSPs)
  for (Time in Times)
  {
    chill <- load_temperature_scenarios(
      "D:/Rdata/Chill_quantification/Future_scenarios/chill/future_chill",
      paste0(Stations_Names[i],"_", SSP, "_", Time))

  }


# Directory paths
dir_future <- "D:/Rdata/Chill_quantification/Future_scenarios/chill/future_chill"
dir_historic <- "D:/Rdata/Chill_quantification/Future_scenarios/chill/historic_chill"

# For future scenarios
files_future <- list.files(dir_future, pattern="*.csv", full.names=TRUE)

# For historic scenarios
files_historic <- list.files(dir_historic, pattern="_historic_5_2020.csv", full.names=TRUE)

# # Combine both lists
# files <- c(files_future, files_historic)

# Read the future files
df_future <- lapply(files_future, function(file) {
  data <- read.csv(file)
  parts <- strsplit(basename(file), "_")[[1]]
  data$GCM <- parts[5]
  #data$Scenario <- parts[2] # Extracting the scenario
  #data$Time <- as.integer(parts[3])  # Extracting the time
  #data$Station_ID <- parts[1] # Extracting the station name
  return(data)
})
df_future <- do.call(rbind, df_future)


result <- df_future %>%
  filter(GCM %in% GCM) %>%
  group_by(GCM) %>%
  mutate(
    Median_Value = median(Chill_CP),
    Median_SWC = quantile(Chill_CP, 0.1),
    Q5 = quantile(Chill_CP, 0.05),
    Q95 = quantile(Chill_CP, 0.95)
  )








# Extracting the 'Chill_CP' column from each data frame in the list
chill_CP <- lapply(chill, function(chill) chill$Chill_CP)

chill_df <- as.data.frame(chill_CP)

# change structure
chill_df <-  chill_df %>% 
  gather(GCM, Value)

# Calculate median values for each GCM
# medians <- chill_df %>%
#   group_by(GCM) %>%
#   summarize(Median_Value = median(Value))

# Assuming 'chill_df' is your data frame
medians <- chill_df %>%
  group_by(GCM) %>%
  summarize(Median_Value = median(Value),
            Median_SWC = quantile(Value, 0.1),
            Q5= quantile(Value, 0.05),
            Q95= quantile(Value, 0.95))


# Merge median values with the original data
chill_df <- merge(chill_df, medians, by = "GCM")

chill_df$Value <- round(chill_df$Value, digits = 0)
chill_df$Median_Value <- round(chill_df$Median_Value, digits = 0)
chill_df$Median_SWC <- round(chill_df$Median_SWC, digits = 0)
chill_df$Q5 <- round(chill_df$Q5, digits = 0)
chill_df$Q95 <- round(chill_df$Q95, digits = 0)
  
GCM_comp_24Jan <-   ggplot(chill_df, aes(x = GCM, y = Value)) +
  geom_boxplot(fill = "grey") +  # Set box colors to grey
  geom_text(aes(label = sprintf("%.0f", Median_SWC), y = Median_SWC, color = "darkred"),
            position = position_dodge(0.75), vjust = -0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.0f", Median_Value), y = Median_Value, color = "green"),
            position = position_dodge(0.75), vjust = -0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.0f", Q5), y = Q5), position = position_dodge(0.75),
            vjust = -0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.0f", Q95), y = Q95), position = position_dodge(0.75),
  vjust = -0.5, show.legend = FALSE) +
  labs(title = NULL,
       x = "GCM", y = "Winter Chill (CP)") +
  #expand_limits(y = 0) +
  theme_bw(base_size = 12.5) +
  theme(panel.background = element_rect(fill = "white"),  # Set background color to slightly white
        axis.title.y = element_text(margin = margin(r = 15), size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5))  # Adjust y-axis title margin
  ggsave(plot = GCM_comp_24Jan, "D:/Rdata/Chill_quantification/Future_scenarios/comp_clim_mod/GCM_comp_24Jan.png", units = "cm", height = 12, width = 18)
  

    

    
    
# Old method of box plot    
medians <- sapply(chill_CP, median)
lower_percentiles <- sapply(chill_CP, function(x) quantile(x, 0.05))
upper_percentiles <- sapply(chill_CP, function(x) quantile(x, 0.95))

# I will save it here
png(filename="D:/Rdata/Chill_quantification/Future_scenarios/comp_clim_mod/old24_climate_model_comp.png", width=1200, height=800)

# Creating the boxplot
boxplot(chill_CP, main="Climate models' comparsion for future chill projection", 
        ylab="Chill Portions (CP)", las=1, cex.main=2.5, cex.axis=1.5, cex.lab=1.5)

# Adding medians
text(1:length(medians), medians, labels=round(medians, 2), col="red", adj=c(0.5, 1.5), cex=1.5)

# Adding 5th percentiles
text(1:length(lower_percentiles), lower_percentiles, labels=round(lower_percentiles, 2), col="blue", adj=c(0.5, -0.5), cex=1.5)

# Adding 95th percentiles
text(1:length(upper_percentiles), upper_percentiles, labels=round(upper_percentiles, 2), col="blue", adj=c(0.5, 2.0), cex=1.5)

dev.off()







# Create a single data frame
df <- do.call(rbind, lapply(names(chill_CP), function(name) {
  data.frame(Model = name, Chill_CP = chill_CP[[name]])
}))

# Ensure the model factor has appropriate levels
df$Model <- factor(df$Model, levels = names(chill_CP))

anova_model <- aov(Chill_CP ~ Model, data = df)
summary(anova_model)

library(multcomp)
tukey_test <- glht(anova_model, linfct = mcp(Model = "Tukey"))
summary(tukey_test)

#Simultaneous Tests for General Linear Hypotheses

# Multiple Comparisons of Means: Tukey Contrasts
# 
# 
# Fit: aov(formula = Chill_CP ~ Model, data = df)
# 
# Linear Hypotheses:
#   Estimate Std. Error t value Pr(>|t|)    
# GFDL-ESM4 - BCC-CSM2-MR == 0    5.0280     0.7593   6.622  < 1e-04 ***
#   INM-CM4-8 - BCC-CSM2-MR == 0   -2.7982     0.7593  -3.685  0.00233 ** 
#   INM-CM5-0 - BCC-CSM2-MR == 0    1.4011     0.7593   1.845  0.34869    
# MRI-ESM2-0 - BCC-CSM2-MR == 0   1.1598     0.7593   1.528  0.54499    
# INM-CM4-8 - GFDL-ESM4 == 0     -7.8262     0.7593 -10.308  < 1e-04 ***
#   INM-CM5-0 - GFDL-ESM4 == 0     -3.6269     0.7593  -4.777  < 1e-04 ***
#   MRI-ESM2-0 - GFDL-ESM4 == 0    -3.8682     0.7593  -5.095  < 1e-04 ***
#   INM-CM5-0 - INM-CM4-8 == 0      4.1993     0.7593   5.531  < 1e-04 ***
#   MRI-ESM2-0 - INM-CM4-8 == 0     3.9580     0.7593   5.213  < 1e-04 ***
#   MRI-ESM2-0 - INM-CM5-0 == 0    -0.2413     0.7593  -0.318  0.99779    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Adjusted p values reported -- single-step method)

################################################################################################






