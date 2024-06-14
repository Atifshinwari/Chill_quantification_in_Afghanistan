#------------------------------------------------------------------------------------------#
# Anlaysis of bias correction methods: Only QDM, QMT, QML and real bias
#------------------------------------------------------------------------------------------#

require(ggplot2)

#------------------------------------------------------------------------------------------#
# 1: Standard Deviation
#------------------------------------------------------------------------------------------#

QML <- read.csv("D:/Rdata/QM/Stations_Obs_Sim_QM/All_Statistics_QM.csv")
QMT <- read.csv("D:/Rdata/QMT/Stations_Obs_Sim_QMT/All_Statistics_QMT.csv")
QDM <- read.csv("D:/Rdata/QDM/Stations_Obs_Sim/All_Stats_QDM.csv")
#SDM <- read.csv("D:/Rdata/All_Statistics_Average - Copy.csv")

# a new data frame to store the mean bias values from each data frame
bias_df <- data.frame(BiasTmin = QML$SD_Tmin,
                      BiasTmax = QML$SD_Tmax,
                      QMLmin = QML$SD_TminBC,
                      QMLmax = QML$SD_TmaxBC,
                      QMTmin = QMT$SD_TminBC,
                      QMTmax = QMT$SD_TmaxBC,
                      QDMmin = QDM$SD_TminBC,
                      QDMmax = QDM$SD_TmaxBC)

bias_vector <- unlist(bias_df)

# Plotting the mean bias values using a line plot

# Code for SD_Tmin_BC plot
SD_Tmin_BC <- ggplot(bias_df, aes(x = 1:nrow(bias_df))) +
  geom_line(aes(y = BiasTmin, color = "SD (with bias)"), show.legend = FALSE) +
  geom_line(aes(y = QMLmin, color = "QML"), show.legend = FALSE) +
  geom_line(aes(y = QMTmin, color = "QMT"),show.legend = FALSE) +
  geom_line(aes(y = QDMmin, color = "QDM"), show.legend = FALSE) +
  labs(title = "Standard Deviation after Bias Correction (Tmin)", x = "Stations", y = "Standard_Deviation") +
  scale_color_manual(name = "Bias Correction Methods", values = c("QML" = "darkblue", "QMT" = "lightgreen", "QDM" = "orange", "SD (with bias)" = "red")) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  )

ggsave("D:/Rdata/BiasCorrMethods/SD_Tmin_BC.png", plot = SD_Tmin_BC, width = 18, height = 10, units = "cm")

# Code for SD_Tmax_BC plot
SD_Tmax_BC <- ggplot(bias_df, aes(x = 1:nrow(bias_df))) +
  geom_line(aes(y = BiasTmax, color = "SD (with bias)")) +
  geom_line(aes(y = QMLmax, color = "QML")) +
  geom_line(aes(y = QMTmax, color = "QMT")) +
  geom_line(aes(y = QDMmax, color = "QDM")) +
  labs(title = "Standard Deviation after Bias Correction (Tmax)", x = "Stations", y = NULL) +
  scale_color_manual(name = "Bias Correction \nMethods", values = c("QML" = "darkblue", "QMT" = "lightgreen", "QDM" = "orange", "SD (with bias)" = "red")) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  )

ggsave("D:/Rdata/BiasCorrMethods/SD_Tmax_BC.png", plot = SD_Tmax_BC, width = 18, height = 10, units = "cm")

# Combine the plots
SD_Stations <- SD_Tmin_BC + SD_Tmax_BC
ggsave("D:/Rdata/BiasCorrMethods/SD_stations.png", plot = SD_Stations, width = 30, height = 18, units = "cm")


#------------------------------------------------------------------------------------------#
# 2: RMSEP
#------------------------------------------------------------------------------------------#

QML <- read.csv("D:/Rdata/QM/Stations_Obs_Sim_QM/All_Statistics_QM.csv")
QMT <- read.csv("D:/Rdata/QMT/Stations_Obs_Sim_QMT/All_Statistics_QMT.csv")
QDM <- read.csv("D:/Rdata/QDM/Stations_Obs_Sim/All_Stats_QDM.csv")

# Create a new data frame to store the mean bias values from each data frame
RMSEP_df <- data.frame(BiasTmin = QML$RMSEP_Tmin,
                       BiasTmax = QML$RMSEP_Tmax,
                       QMLmin = QML$RMSEP_TminBC,
                       QMLmax = QML$RMSEP_TmaxBC,
                       QMTmin = QMT$RMSEP_TminBC,
                       QMTmax = QMT$RMSEP_TmaxBC,
                       QDMmin = QDM$RMSEP_TminBC,
                       QDMmax = QDM$RMSEP_TmaxBC)

bias_vector <- unlist(RMSEP_df)

# Plotting the mean bias values using a line plot

# Code for SD_Tmin_BC plot
RMSEP_Tmin_BC <- ggplot(RMSEP_df, aes(x = 1:nrow(RMSEP_df))) +
  geom_line(aes(y = BiasTmin, color = "RMSEP (with bias)"), show.legend = FALSE) +
  geom_line(aes(y = QMLmin, color = "QML"), show.legend = FALSE) +
  geom_line(aes(y = QMTmin, color = "QMT"),show.legend = FALSE) +
  geom_line(aes(y = QDMmin, color = "QDM"), show.legend = FALSE) +
  labs(title = "RMSEP after Bias Correction (Tmin)", x = "Stations", y = "RMSEP") +
  scale_color_manual(name = "Bias Correction Methods", values = c("QML" = "darkblue", "QMT" = "green", "QDM" = "orange", "RMSEP (with bias)" = "red")) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  )

ggsave("D:/Rdata/BiasCorrMethods/RMSEP_Tmin_BC.png", plot = RMSEP_Tmin_BC, width = 18, height = 10, units = "cm")

# Code for SD_Tmax_BC plot
RMSEP_Tmax_BC <- ggplot(RMSEP_df, aes(x = 1:nrow(RMSEP_df))) +
  geom_line(aes(y = BiasTmax, color = "RMSEP (with bias)")) +
  geom_line(aes(y = QMLmax, color = "QML")) +
  geom_line(aes(y = QMTmax, color = "QMT")) +
  geom_line(aes(y = QDMmax, color = "QDM")) +
  labs(title = "RMSEP after Bias Correction (Tmax)", x = "Stations", y = NULL) +
  scale_color_manual(name = "Bias Correction \nMethods", values = c("QML" = "darkblue", "QMT" = "green", "QDM" = "orange", "RMSEP (with bias)" = "red")) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  )

ggsave("D:/Rdata/BiasCorrMethods/RMSEP_Tmax_BC.png", plot = RMSEP_Tmax_BC, width = 18, height = 10, units = "cm")

# Combine the plots
RMSEP_Stations <- RMSEP_Tmin_BC + RMSEP_Tmax_BC
ggsave("D:/Rdata/BiasCorrMethods/RMSEP_stations.png", plot = RMSEP_Stations, width = 30, height = 18, units = "cm")


#------------------------------------------------------------------------------------------#
# 3: RPIQ
#------------------------------------------------------------------------------------------#

RPIQ_df <- data.frame(BiasTmin = QML$RPIQ_Tmin,
                      BiasTmax = QML$RPIQ_Tmax,
                      QMLmin = QML$RPIQ_TminBC,
                      QMLmax = QML$RPIQ_TmaxBC,
                      QMTmin = QMT$RPIQ_TminBC,
                      QMTmax = QMT$RPIQ_TmaxBC,
                      QDMmin = QDM$RPIQ_TminBC,
                      QDMmax = QDM$RPIQ_TmaxBC)

RPIQ_vector <- unlist(RPIQ_df)

# Plotting the mean bias values using a line plot

# Code for SD_Tmin_BC plot
RPIQ_Tmin_BC <- ggplot(RPIQ_df, aes(x = 1:nrow(RPIQ_df))) +
  geom_line(aes(y = BiasTmin, color = "RPIQ (with bias)"), show.legend = FALSE) +
  geom_line(aes(y = QMLmin, color = "QML"), show.legend = FALSE) +
  geom_line(aes(y = QMTmin, color = "QMT"),show.legend = FALSE) +
  geom_line(aes(y = QDMmin, color = "QDM"), show.legend = FALSE) +
  labs(title = "RPIQ after Bias Correction (Tmin)", x = "Stations", y = "RPIQ") +
  scale_color_manual(name = "Bias Correction Methods", values = c("QML" = "darkblue", "QMT" = "green", "QDM" = "orange", "RPIQ (with bias)" = "red")) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  )

ggsave("D:/Rdata/BiasCorrMethods/RPIQ_Tmin_BC.png", plot = RPIQ_Tmin_BC, width = 18, height = 10, units = "cm")

# Code for SD_Tmax_BC plot
RPIQ_Tmax_BC <- ggplot(RPIQ_df, aes(x = 1:nrow(RPIQ_df))) +
  geom_line(aes(y = BiasTmax, color = "RPIQ (with bias)")) +
  geom_line(aes(y = QMLmax, color = "QML")) +
  geom_line(aes(y = QMTmax, color = "QMT")) +
  geom_line(aes(y = QDMmax, color = "QDM")) +
  labs(title = "RPIQ after Bias Correction (Tmax)", x = "Stations", y = NULL) +
  scale_color_manual(name = "Bias Correction \nMethods", values = c("QML" = "darkblue", "QMT" = "green", "QDM" = "orange", "RPIQ (with bias)" = "red")) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  )

ggsave("D:/Rdata/BiasCorrMethods/RPIQ_Tmax_BC.png", plot = RPIQ_Tmax_BC, width = 18, height = 10, units = "cm")

# Combine the plots
RPIQ_Stations <- RPIQ_Tmin_BC + RPIQ_Tmax_BC
ggsave("D:/Rdata/BiasCorrMethods/RPIQ_stations.png", plot = RPIQ_Stations, width = 30, height = 18, units = "cm")



# Plotting statistical indicators using box plot
# RPIQ
QML <- read.csv("D:/Rdata/QM/Stations_Obs_Sim_QM/All_Statistics_QM.csv")
QMT <- read.csv("D:/Rdata/QMT/Stations_Obs_Sim_QMT/All_Statistics_QMT.csv")
QDM <- read.csv("D:/Rdata/QDM/Stations_Obs_Sim/All_Stats_QDM.csv")

# Create a new data frame to store the mean bias values from each data frame
RPIQ_Tmin <- data.frame(BiasTmin = QML$RPIQ_Tmin,
                        QDMmin = QDM$RPIQ_TminBC)

RPIQ_Tmax <- data.frame(BiasTmax = QML$RPIQ_Tmax,
                        QDMmax = QDM$RPIQ_TmaxBC)

# dataframe with the two columns
RPIQ_Tmin <- data.frame(
  value = c(RPIQ_Tmin$QDMmin, RPIQ_Tmin$BiasTmin),
  group = factor(rep(c("Bias corrected", "Bias uncorrected"), each = nrow(RPIQ_Tmin)))
)

RPIQ_Tmax <- data.frame(
  value = c(RPIQ_Tmax$QDMmax, RPIQ_Tmax$BiasTmax),
  group = factor(rep(c("Bias corrected", "Bias uncorrected"), each = nrow(RPIQ_Tmax)))
)

# Re order the level of my group (I want to see uncorrected on left and corrected on right side)
RPIQ_Tmin$group <- factor(RPIQ_Tmin$group, levels = c("Bias uncorrected", "Bias corrected"))
RPIQ_Tmax$group <- factor(RPIQ_Tmax$group, levels = c("Bias uncorrected", "Bias corrected"))

# Best approach 
RPIQ_Tmin$variable <- "Tmin"
RPIQ_Tmax$variable <- "Tmax"

#library(ggplot2)
RPIQ_BC <- RPIQ_Tmin %>% 
  rbind(RPIQ_Tmax) %>% 
  mutate(variable = factor(variable, levels = c("Tmin", "Tmax"))) %>%
  ggplot(aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  labs(title = NULL, x = NULL, y = "Ratio of Performance to InterQuartile distance (RPIQ)")+
  scale_fill_manual(values = c("lightgrey", "skyblue"), name = "Temperature Data Type")+
  expand_limits(y = 0) +
  facet_grid(~variable) +
  theme_bw(base_size = 10)

ggsave(plot = RPIQ_BC, "D:/Rdata/BiasCorrMethods/RPIQ_BC.png", units = "cm", height = 12, width = 18)



