
#------------------------------------------------------------------------------------------#
# Combined plots for historic scenarios (1980_2020): Box plot
#------------------------------------------------------------------------------------------#

library(gridExtra)
library(ggplot2)
library(chillR)
library(magrittr)
library(stringr)
library(dplyr)
library(cowplot)

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
  
  # Combine 1980 and 2020 data
  chill_1980 <- chill_past_scenarios$"1980"
  chill_1980$Year <- "1980"
  
  chill_2020 <- chill_past_scenarios$"2020"
  chill_2020$Year <- "2020"
  
  station_data <- rbind(chill_1980, chill_2020)

  # Assign station name as a column
  station_data$Station_ID <- rep(Stations_Names[i], nrow(station_data))
  
  # Append to the list
  extracted_data[[i]] <- station_data
}

# Combine all extracted data into a single data frame
final_data <- do.call(rbind, extracted_data)

# Print the final data frame
#print(final_data)

# # change station names for real names
# station_mapping <- c("Adraskan" = "Adraskan (ADN)",
#                      "Anjuman" = "Anjuman (AJN)",
#                      "Asmar" = "Asmar (ASR)",
#                      "BaghiOmomi" = "Bagh-i-Omomi (BOI)",
#                      "Baghlan" = "Baghlan (BLN)",
#                      "Baharak" = "Baharak (BRK)",
#                      "Bamyan" = "Bamyan (BYN)",
#                      "Cheghcheran" = "Cheghcheran (CCN)",
#                      "Chichakto" = "Chichakto (CCO)",
#                      "Dakah" = "Dakah (DKH)",
#                      "DaraiZhwandon" = "Dara-i-Zhwandon (DZN)",
#                      "Darwishan" = "Darwishan (DSN)",
#                      "DashteiSafid" = "Dashte-i-Safid (DSD)",
#                      "Dawlatabad" = "Dawlatabad (DBD)",
#                      "Dawlatyar" = "Dawlatyar (DYR)",
#                      "Delmarogh" = "Delmarogh (DMH)",
#                      "Doshi" = "Doshi (DSI)",
#                      "Eshkashem" = "Eshkashem (EKM)",
#                      "Estalef" = "Estalef (ETF)",
#                      "Faizabad" = "Faizabad (FZD)",
#                      "Farah" = "Farah (FRH)",
#                      "Gardandiwal" = "Gardandiwal (GDL)",
#                      "Gardiz" = "Gardiz (GDZ)",
#                      "Keraman" = "Keraman (KRN)",
#                      "Keshem" = "Keshem (KSM)",
#                      "Khenjan" = "Khenjan (KJN)",
#                      "KheshtPul" = "Khesht-Pul (KPL)",
#                      "Lashkargah" = "Lashkargah (LGH)",
#                      "Maton" = "Maton (MTN)",
#                      "Nawabad" = "Nawabad (NBD)",
#                      "NazdikiKandahar" = "Nazdik-i-Kandahar (NKR)",
#                      "NazdikiKeshandeh" = "Nazdik-i-Keshandeh (NKH)",
#                      "NazdikiNayak" = "Nazdik-i-Nayak (NNK)",
#                      "NazdikiTaluqan" = "Nazdik-i-Taluqan (NTN)",
#                      "PuliAlchin" = "Pul-i-Alchin (PAN)",
#                      "PuliBangi" = "Pul-i-Bangi (PBI)",
#                      "PuliBehsod" = "Pul-i-Behsod (PBD)",
#                      "PuliGhazni" = "Pul-i-Ghazni (PGI)",
#                      "PuliHashemi" = "Pul-i-Hashemi (PHI)",
#                      "PuliQarghayi" = "Pul-i-Qarghayi (PQI)",
#                      "Qaisar" = "Qaisar (QSR)",
#                      "RabatiBala" = "Rabat-i-Bala (RBA)",
#                      "Sayad" = "Sayad (SYD)",
#                      "Shakardara" = "Shakardara (SDA)",
#                      "TagabiGhaza" = "Tagab-i-Ghaza (TGA)",
#                      "TangiNahrin" = "Tang-i-Nahrin (TNN)",
#                      "TangiSayedan" = "Tang-i-Sayedan (TSN)",
#                      "TangiTashqurghan" = "Tang-i-Tashqurghan (TTN)",
#                      "Tirin" = "Tirin (TRN)",
#                      "Torghundi" = "Torghundi (TGI)",
#                      "Waras" = "Waras (WRS)"
# )
# 

final_data <- final_data %>% 
  mutate(Sub_regions = case_when(
    Station_ID %in% c("RabatiBala", "TangiTashqurghan", "Chichakto", "Dawlatabad", "KheshtPul", 
                      "Qaisar", "DaraiZhwandon", "Delmarogh", "Sayad", "NazdikiKeshandeh") ~ "North",
    Station_ID %in% c("Anjuman", "Baharak", "Eshkashem", "Faizabad", "Keshem", "Baghlan", 
                      "Doshi", "Khenjan", "TangiNahrin", "PuliAlchin", "NazdikiTaluqan", "PuliBangi") ~ "Northeast",
    Station_ID %in% c("Nawabad", "Asmar", "Dakah", "PuliQarghayi",  "PuliBehsod", "Gardiz", "Maton") ~ "East",
    Station_ID %in% c("Darwishan", "Lashkargah", "NazdikiKandahar", "Tirin") ~ "South",
    Station_ID %in% c("Farah", "Adraskan", "PuliHashemi", "TagabiGhaza", "Torghundi") ~ "West",
    Station_ID %in% c("Bamyan", "DashteiSafid", "NazdikiNayak", "Waras", "PuliGhazni", "Cheghcheran","Dawlatyar", "Estalef", 
                      "Shakardara", "TangiSayedan", "Keraman", "BaghiOmomi","Gardandiwal") ~ "Central",
    TRUE ~ NA_character_  # For any station not in the lists above
  ))

# Define the desired order of regions
region_order <- c("North", "Northeast", "East", "South", "West", "Central")

# Convert Sub_regions to factor with the desired order
final_data$Sub_regions <- factor(final_data$Sub_regions, levels = region_order)

# rearrange based on sub-regions
final_data <- final_data %>% 
  arrange(Sub_regions)

# # # Define order for each sub-region
# North <- c("KheshtPul", "Dawlatabad", "NazdikiKeshandeh", "Delmarogh", "Sayad", "Chichakto", 
#            "TangiTashqurghan", "RabatiBala", "DaraiZhwandon", "Qaisar")
# Northeast <- c("Eshkashem", "Anjuman", "Baghlan", "PuliAlchin", "Keshem", "NazdikiTaluqan", 
#                "Doshi", "Khenjan", "PuliBangi", "Baharak", "Faizabad", "TangiNahrin")
# East <- c("Dakah", "PuliBehsod", "PuliQarghayi", "Nawabad", "Asmar", "Maton", "Gardiz")
# South <- c("Darwishan", "Lashkargah", "NazdikiKandahar", "Tirin")
# West <- c("Farah", "Torghundi", "PuliHashemi", "Adraskan", "TagabiGhaza")
# Central <- c("Gardandiwal", "Waras", "NazdikiNayak", "Dawlatyar", "Bamyan", "Cheghcheran", 
#              "Keraman", "PuliGhazni", "Shakardara", "DashteiSafid", "TangiSayedan", "BaghiOmomi", "Estalef")

# # Arrange data based on the order for each sub-region
# final_data <- final_data %>% 
#   arrange(match(Station_ID, c(North, Northeast, East, South, West, Central)))


folder_gg <- "D:/Rdata/Chill_quantification/SWC/historic/plots/box_plots/"


# Calculate the 10th quantile or SWC for the years 1980 and 2020
SWC <- final_data %>%
  filter(Year %in% c(1980, 2020)) %>%
  group_by(Station_ID, Year) %>%
  summarize(SWC = quantile(Chill_CP, 0.1))  # Calculate 10th quantile for each station and year

# Merge SWC data with final_data
final_data <- merge(final_data, SWC, by = c("Station_ID", "Year"), all.x = TRUE)

# I define and rearrange station order based on high chill to low chill in subregions
# Define the desired order of stations
station_order <- c("KheshtPul", "Dawlatabad", "NazdikiKeshandeh", "Delmarogh", "Sayad", "Chichakto", "TangiTashqurghan", 
                   "RabatiBala", "DaraiZhwandon", "Qaisar", "Eshkashem", "Anjuman", "Baghlan", "PuliAlchin", 
                   "Keshem", "NazdikiTaluqan", "Doshi", "Khenjan", "PuliBangi", "Baharak", "Faizabad", 
                   "TangiNahrin", "Dakah", "PuliBehsod", "PuliQarghayi", "Nawabad", "Asmar", "Maton", "Gardiz",
                   "Darwishan", "Lashkargah", "NazdikiKandahar", "Tirin", "Farah", "Torghundi", "PuliHashemi", 
                   "Adraskan", "TagabiGhaza", "Gardandiwal", "Waras", "NazdikiNayak", "Dawlatyar", "Bamyan", 
                   "Cheghcheran","Keraman", "PuliGhazni", "Shakardara", "DashteiSafid", "TangiSayedan", "BaghiOmomi", "Estalef")

station_order <- c("Qaisar", "DaraiZhwandon", "RabatiBala", "TangiTashqurghan", "Chichakto", "Sayad", "Delmarogh", 
                   "NazdikiKeshandeh", "Dawlatabad", "KheshtPul", "TangiNahrin", "Faizabad", "Baharak", "PuliBangi", 
                   "Khenjan", "Doshi", "NazdikiTaluqan", "Keshem", "PuliAlchin", "Baghlan", "Anjuman", "Eshkashem",
                   "Gardiz", "Maton", "Asmar", "Nawabad", "PuliQarghayi", "PuliBehsod", "Dakah", "Tirin", "NazdikiKandahar",
                   "Lashkargah", "Darwishan", "TagabiGhaza", "Adraskan", "PuliHashemi", "Torghundi", "Farah", "Estalef",
                   "BaghiOmomi", "TangiSayedan", "DashteiSafid", "Shakardara", "PuliGhazni", "Keraman", "Cheghcheran",
                   "Bamyan", "Dawlatyar", "NazdikiNayak", "Waras", "Gardandiwal")


# Rearrange Station_ID column in final_data according to the desired order
final_data$Station_ID <- factor(final_data$Station_ID, levels = station_order)

# Sort final_data based on the new order of Station_ID
final_data <- final_data[order(final_data$Station_ID), ]

# filter final data for swc values
filtered_data <- final_data[final_data$Year %in% c(1980, 2020), ]
filtered_data <- filtered_data[!duplicated(filtered_data[, c("Station_ID", "Year")]), ]
# Round SWC values in the SWC column of filtered_data to zero decimal places
filtered_data$SWC <- round(filtered_data$SWC, 0)

# read % higher overlap data for annotation
#library(readxl)
percent_higher <- read.csv("D:/Rdata/Chill_quantification/SWC/historic/percent_overlap/percent_higher.csv", stringsAsFactors = FALSE)
percent_higher$percent_higher_2020 <- ifelse(percent_higher$percent_higher_2020 != 0 & !is.na(percent_higher$percent_higher_2020),
                                             paste("+", percent_higher$percent_higher_2020, sep = ""),
                                             percent_higher$percent_higher_2020)
filtered_data <- merge(filtered_data, percent_higher, by = c("Station_ID"), all.x = TRUE)

# Rearrange Station_ID column in filtered_data according to the desired order
filtered_data$Station_ID <- factor(filtered_data$Station_ID, levels = station_order)

# Sort filtered_data based on the new order of Station_ID
filtered_data <- filtered_data[order(filtered_data$Station_ID), ]

# adjust cheghcheran value for 2020
filtered_data[filtered_data$Station_ID == "Cheghcheran" & filtered_data$Year == "2020", "SWC"] <- 61

# Reverse the order of Station_ID
final_data$Station_ID <- factor(final_data$Station_ID, levels = rev(levels(final_data$Station_ID)))


# Use new_annotations as the names for Y axis in place of old names from station_order
station_order <- c("Qaisar", "DaraiZhwandon", "RabatiBala", "TangiTashqurghan", "Chichakto", "Sayad", "Delmarogh", 
                   "NazdikiKeshandeh", "Dawlatabad", "KheshtPul", "TangiNahrin", "Faizabad", "Baharak", "PuliBangi", 
                   "Khenjan", "Doshi", "NazdikiTaluqan", "Keshem", "PuliAlchin", "Baghlan", "Anjuman", "Eshkashem",
                   "Gardiz", "Maton", "Asmar", "Nawabad", "PuliQarghayi", "PuliBehsod", "Dakah", "Tirin", "NazdikiKandahar",
                   "Lashkargah", "Darwishan", "TagabiGhaza", "Adraskan", "PuliHashemi", "Torghundi", "Farah", "Estalef",
                   "BaghiOmomi", "TangiSayedan", "DashteiSafid", "Shakardara", "PuliGhazni", "Keraman", "Cheghcheran",
                   "Bamyan", "Dawlatyar", "NazdikiNayak", "Waras", "Gardandiwal")


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

names(new_annotations) <- station_order

#library(stringr)
# add spacing using pad for percent higher and lower value to get values aligned 

pad_values <- function(x) {
  if (is.na(x)) return(NA)
  if (x == 0) return("   0")
  if (x > 0) {
    if (nchar(sprintf("%d", x)) == 2) {
      return(str_pad(sprintf("+%d", x), width = 4, side = "left"))  # extra space for two-digit + values
    }
    return(str_pad(sprintf("+%d", x), width = 4, side = "left"))
  }
  if (nchar(sprintf("%d", x)) == 3) {
    return(str_pad(sprintf("%d", x), width = 4, side = "left"))  # extra space for two-digit - values
  }
  return(str_pad(sprintf("%d", x), width = 4, side = "left"))
}

# before applying it need to be numeric
filtered_data$percent_higher_2020 <- as.numeric(filtered_data$percent_higher_2020)
filtered_data$percent_lower_2020 <- as.numeric(filtered_data$percent_lower_2020)

# Apply the function to percent_higher_2020 and percent_lower_2020 columns
filtered_data$percent_higher_2020 <- sapply(filtered_data$percent_higher_2020, pad_values)
filtered_data$percent_lower_2020 <- sapply(filtered_data$percent_lower_2020, pad_values)

# Now plotting
plot <- ggplot(data = final_data, aes(x = Chill_CP, y = Station_ID, fill = Year)) +
  geom_boxplot(size = 0.9, width = 0.8, position = position_dodge(width = 0.8)) +  # Increase box plot width # for merging boxes , position = "identity"
  facet_grid(Sub_regions ~ ., scales = "free_y", space = "free_y") +  # Uniform spacing
  
  # add SWC values for the year 1980 only
  geom_text(data = subset(filtered_data, Year == 1980),
            aes(label = paste("(", SWC,",", " ",sep = "")),
            y = subset(filtered_data, Year == 1980)$Station_ID, x = max(final_data$Chill_CP) - 81.6, #-2.8
            size = 5.5, hjust = -0.4, vjust = 0.5, color = "deepskyblue4") +

  # add SWC values for the year 2020 only
  geom_text(data = subset(filtered_data, Year == 2020),
            aes(label = paste(SWC, ")", sep = "")),
            y = subset(filtered_data, Year == 2020)$Station_ID, x = max(final_data$Chill_CP) - 77.5, # + 0
            size = 5.5, hjust = -0.4, vjust = 0.5, color = "red") +

  # # add % overlap deviation values for either 1980 or 2020
  # geom_text(data = subset(filtered_data, Year == 2020 & !is.na(percent_higher_2020)),
  #           aes(label = paste("(",percent_higher_2020, ")", sep = "")),
  #           y = subset(filtered_data, Year == 2020 & !is.na(percent_higher_2020))$Station_ID, x = max(final_data$Chill_CP) - 1.2, # + 0
  #           size = 5.5, hjust = -0.4, vjust = 0.5, color = "red") +
  # 
  # # add % overlap deviation values for either 1980 or 2020
  # geom_text(data = subset(filtered_data, Year == 2020 & !is.na(percent_lower_2020)),
  #           aes(label = paste("(",percent_lower_2020, ")", sep = "")),
  #           y = subset(filtered_data, Year == 2020 & !is.na(percent_lower_2020))$Station_ID, x = max(final_data$Chill_CP) - 0.9, # + 0
  #           size = 5.5, hjust = -0.4, vjust = 0.5, color = "deepskyblue4") +

  # add % overlap deviation values for either 1980 or 2020
  geom_text(data = subset(filtered_data, Year == 2020 & !is.na(percent_higher_2020)),
            aes(label = paste(percent_higher_2020, sep = "")),
            y = subset(filtered_data, Year == 2020 & !is.na(percent_higher_2020))$Station_ID, x = max(final_data$Chill_CP) - 1.2, # + 0
            size = 5.5, hjust = -0.4, vjust = 0.5, color = "red") +
  
  # add % overlap deviation values for either 1980 or 2020
  geom_text(data = subset(filtered_data, Year == 2020 & !is.na(percent_lower_2020)),
            aes(label = paste(percent_lower_2020, sep = "")),
            y = subset(filtered_data, Year == 2020 & !is.na(percent_lower_2020))$Station_ID, x = max(final_data$Chill_CP) - 0.9, # + 0
            size = 5.5, hjust = -0.4, vjust = 0.5, color = "deepskyblue4") +
  
  theme_minimal() +
  #scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_discrete(labels = new_annotations) + # Use annotations as y-axis labels
  
  
  labs(title = "Safe Winter\nChill (SWC)",#                                                                       Chill overlap deviation (%) 2020",
       x = "Chill accumulation (CP)",
       y = "Weather stations") +
  
  scale_fill_manual(values = c("deepskyblue", "#FF6F80"), 
                    breaks = unique(final_data$Year)) +
  
  # Modify the scale_x_continuous function to adjust breaks

  #lims(x = c(10, 91)) +  # Setting x-axis limits from 0 to 100
  scale_x_continuous(breaks = c(20, 40, 60, 80), limits = c(13, 92)) +

  theme(title = element_text(size = 17),
        axis.text = element_text(size = 21),
        #axis.title.y = element_text(size = 22, vjust = 3),  # Adjust margin towards the edge
        axis.title = element_text(size = 25), 
        axis.title.x = element_text(size = 25, vjust = -1), 
        #axis.text.y = element_text(size = 22), # hjust = 0 for y axis title justify
        strip.text.y = element_text(size = 25),
        legend.text = element_text(size = 24),  # Increase legend text size
        legend.title = element_text(size = 25),   # Increase legend title size
        panel.background = element_rect(fill = "white"),  # Set background color
        panel.spacing = unit(0.5, "lines"),  # Adjust panel spacing
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20))  # Adjust plot margins

plot <- plot + guides(fill = guide_legend(override.aes = list(size = 21)))

# Add the text annotation using ggdraw and draw_label
plot <- ggdraw(plot) +
  draw_label("Chill overlap\ndeviation (%) 2020", x = 0.8, y = Inf, vjust = 1.6, color = "black", size = 20.2)

ggsave(plot = plot, path = folder_gg, filename = "final_qqplot.png", dpi = 600, width = 16, height = 17, bg = "white")



