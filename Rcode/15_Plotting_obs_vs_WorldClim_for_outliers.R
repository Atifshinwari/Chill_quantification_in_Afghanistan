
#------------------------------------------------------------------------------------------#
################# Plotting mean temp December observed vs WorldClim data ###################
#------------------------------------------------------------------------------------------#

# This is done to look for potential out liers and comparison of observed and worldclim data

# Some quality check plots
# df needed to draw the ribbon
df <- data.frame(obs_avg_temp_dec = -20:30, avg_temp_dec = -20:30)

####  Tmean ####

ggplot(All_chill, aes(x= obs_avg_temp_dec, y = avg_temp_dec)) + 
  geom_ribbon(data = df, aes(ymin = avg_temp_dec -2, ymax = avg_temp_dec +2), fill="grey", alpha=.5)+
  geom_point() +
  #geom_errorbar(aes(ymin=avg_temp_jul-sd_obs_avg_temp_jul, ymax = avg_temp_jul + sd_obs_avg_temp_jul))+
  geom_abline(slope = 1,intercept = 0)+
  coord_cartesian(xlim = c(-10,20),ylim=c(-10,20))+
  ylab('WorldClim: Average Temperature, December (°C)')+
  xlab('Station: Average Temperature, December (°C)')+
  theme_bw() 
ggsave('D:/Rdata/Chill_quantification/3D Mapping/temp_comparison/outliers_tmean.png', height = 4)

# Tmean outliers #

All_chill$outlier_tmean_dec <- abs(All_chill$avg_temp_dec - All_chill$obs_avg_temp_dec) > 2
sum(All_chill$outlier_tmean_dec)

ggplot(All_chill,aes(x = obs_avg_temp_dec, y = avg_temp_dec)) +
  geom_ribbon(data = df, aes(ymin = avg_temp_dec -2, ymax = avg_temp_dec +2), fill="grey", alpha=.5)+
  geom_point()+
  geom_abline(intercept = 0,slope = 1)+
  geom_label_repel(
    data = subset(All_chill, outlier_tmean_dec),
    aes(label = Station_ID),
    box.padding = unit(0.50, "lines"),
    point.padding = unit(0.2, "lines"),
    min.segment.length = 0,
    nudge_x = 5,
    size = 2,
    max.overlaps = 28) + 
  ylab('WorldClim: Average Temperature, December (°C)')+
  xlab('Station: Average Temperature, December (°C)')+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D Mapping/temp_comparison/outliers_tmean_annotated.png', height = 4)


#### Tmin ####

#for ribbon
df <- data.frame(obs_tmin_dec = -20:20, min_temp_dec = -20:20)

ggplot(All_chill,aes(x= obs_tmin_dec, y = min_temp_dec)) + 
  geom_ribbon(data = df, aes(ymin = min_temp_dec -2, ymax = min_temp_dec +2), fill="grey", alpha=.5)+
  geom_point() +
  #geom_errorbar(aes(ymin=avg_temp_jul-sd_obs_avg_temp_jul, ymax = avg_temp_jul + sd_obs_avg_temp_jul))+
  geom_abline(slope = 1,intercept = 0)+
  coord_cartesian(xlim = c(-15,15),ylim=c(-15,15))+
  ylab('WorldClim: Minimum Temperature, December (°C)')+
  xlab('Station: Minimum Temperature, December (°C)')+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D Mapping/temp_comparison/outliers_tmin.png', height = 4)


# Tmin outliers #

All_chill$outlier_tmin_dec <- abs(All_chill$min_temp_dec - All_chill$obs_tmin_dec) > 2
sum(All_chill$outlier_tmin_dec)

ggplot(All_chill,aes(x= obs_tmin_dec, y = min_temp_dec)) + 
  geom_ribbon(data = df, aes(ymin = min_temp_dec -2, ymax = min_temp_dec +2), fill="grey", alpha=.5)+
  geom_point() +
  #geom_errorbar(aes(ymin=avg_temp_jul-sd_obs_avg_temp_jul, ymax = avg_temp_jul + sd_obs_avg_temp_jul))+
  geom_abline(slope = 1,intercept = 0)+
  geom_label_repel(
    data = subset(All_chill, outlier_tmin_dec),
    aes(label = Station_ID),
    box.padding = unit(0.50, "lines"),
    point.padding = unit(0.2, "lines"),
    min.segment.length = 0,
    nudge_x = 6,
    size = 2,
    max.overlaps = 30)+
  ylab('WorldClim: Minimum Temperature, December (°C)')+
  xlab('Station: Minimum Temperature, December (°C)')+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D Mapping/temp_comparison/outliers_tmin_annotated.png', height = 4)

#### Tmax ####
df <- data.frame(obs_tmax_dec = -10:30, max_temp_dec = -10:30)

ggplot(All_chill,aes(x= obs_tmax_dec, y = max_temp_dec)) + 
  geom_ribbon(data = df, aes(ymin = max_temp_dec -2, ymax = max_temp_dec +2), fill="grey", alpha=.5)+
  geom_point() +
  #geom_errorbar(aes(ymin=avg_temp_jul-sd_obs_avg_temp_jul, ymax = avg_temp_jul + sd_obs_avg_temp_jul))+
  geom_abline(slope = 1,intercept = 0)+
  coord_cartesian(xlim = c(-5,25),ylim=c(-5,25))+
  ylab('WorldClim: Maximum Temperature, December (°C)')+
  xlab('Station: Maximum Temperature, December (°C)')+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D Mapping/temp_comparison/outliers_tmax.png', height = 4)


# Tmax outliers #

All_chill$outlier_tmax_dec <- abs(All_chill$max_temp_dec - All_chill$obs_tmax_dec) > 2
sum(All_chill$outlier_tmax_dec)

ggplot(All_chill,aes(x= obs_tmax_dec, y = max_temp_dec)) + 
  geom_ribbon(data = df, aes(ymin = max_temp_dec -2, ymax = max_temp_dec +2), fill="grey", alpha=.5)+
  geom_point() +
  #geom_errorbar(aes(ymin=avg_temp_jul-sd_obs_avg_temp_jul, ymax = avg_temp_jul + sd_obs_avg_temp_jul))+
  geom_abline(slope = 1,intercept = 0)+
  geom_label_repel(
    data = subset(All_chill, outlier_tmax_dec),
    aes(label = Station_ID),
    box.padding = unit(0.50, "lines"),
    point.padding = unit(0.2, "lines"),
    min.segment.length = 0,
    nudge_x = 6,
    size = 2,
    max.overlaps = 15)+
  ylab('WorldClim: Maximum Temperature, December (°C)')+
  xlab('Station: Maximum Temperature, December (°C)')+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D Mapping/temp_comparison/outliers_tmax_annotated.png', height = 4)

#number of stations outlying either in tmin or tmax
sum((All_chill$outlier_tmin_dec | All_chill$outlier_tmax_dec)) 

#update the All_chill dataframe
write.csv(All_chill, file = 'D:/Rdata/Chill_quantification/SWC/All_chill.csv', row.names = FALSE)


# Create one figure including the outliers for tmin and tmax
stations_tmin <- dplyr::select(All_chill, Station_ID, min_temp_dec, obs_tmin_dec)
colnames(stations_tmin) <- c("Station_ID", "WorldClim", "On-site")
stations_tmin["Var"] <- "Tmin"

# Tmax
stations_tmax <- dplyr::select(All_chill, Station_ID, max_temp_dec, obs_tmax_dec)
colnames(stations_tmax) <- c("Station_ID", "WorldClim", "On-site")
stations_tmax["Var"] <- "Tmax"

# Merge both data frames
stations_both <- bind_rows(stations_tmax, stations_tmin)

# Plot
df <- data.frame(obs_tmax_dec = -20:35, max_temp_dec = -20:35)

ggplot(stations_both, aes(`On-site`, WorldClim)) + 
  geom_point() +
  geom_ribbon(data = df, aes(max_temp_dec, obs_tmax_dec, ymin = max_temp_dec - 2, ymax = max_temp_dec + 2),
              fill = "grey", alpha = .5) +
  geom_abline(slope = 1, intercept = 0) +
  coord_cartesian(xlim = c(-15,22),ylim=c(-15,22))+
  facet_wrap(. ~ factor(Var, labels = c("Maximum temperature", "Minimum temperature")), nrow = 2) +
  geom_label_repel(
    data = stations_both[abs(stations_both$WorldClim - stations_both$`On-site`) > 2, ],
    aes(label = Station_ID),
    box.padding = unit(0.60, "lines"),
    point.padding = unit(0.2, "lines"),
    min.segment.length = 0,
    nudge_x = 5,
    nudge_y = 5,
    max.overlaps = 55,
    size = 2,
    direction = "both",
    force = 30) +
  xlab("On-site data (°C)") +
  ylab("WorldClim data (°C)") +
  theme_bw(base_size = 14)
ggsave('D:/Rdata/Chill_quantification/3D Mapping/temp_comparison/outliers_tmin_tmax_annotated.png', height = 19, width = 17, units = "cm", dpi = 600)

ggsave("figures/final_figures/figure_S1_b.png", height = 19, width = 17, units = "cm", dpi = 600)



#------------------------------------------------------------------------------------------#
############## Some additional work I did for out liers especially Eshkashim ###############
#------------------------------------------------------------------------------------------#



# Checking eshkashim temperature data (which seems as an outlier)

ggplot(All_chill,aes(y=avg_temp_dec,x=obs_avg_temp_dec))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add a reference line
  geom_text(aes(label = Station_ID), vjust = 1.5, hjust = 0.5, size = 2, check_overlap = TRUE) +
  #facet_wrap(~lat_group)+
  coord_cartesian(xlim = c(-10,20),ylim=c(-10,20))+
  ylab('WorldClim Average Temperature, December (°C)')+
  xlab('Observed Average Temperature, December (°C)')+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D Mapping/temp_comparison/myplot_outliers_tmean.png', height = 10,width = 15,units = 'cm')


# Computing the bias between obs and Worldclim temperature

diff_tmin_dec <- All_chill$obs_tmin_dec - All_chill$min_temp_dec
diff_tmax_dec <- All_chill$obs_tmax_dec - All_chill$max_temp_dec
diff_avg_temp_dec <- All_chill$obs_avg_temp_dec - All_chill$avg_temp_dec


# Adding this to All_chill
All_chill <- cbind(All_chill, diff_tmin_dec, diff_tmax_dec,diff_avg_temp_dec)

# Plotting the differences between obs and worldclim tmin, tmax and avg
ggplot(All_chill, aes(x = Station_ID, y = diff_tmin_dec, color = factor(sign(diff_tmin_dec)))) +
  geom_segment(aes(xend = Station_ID, yend = 0), size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "Station_ID", y = "Difference, Tmin Observed and Worldclim  (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('D:/Rdata/Chill_quantification/3D Mapping/temp_comparison/diff_tmin_dec.jpg', height = 12,width = 22,units = 'cm')

ggplot(All_chill, aes(x = Station_ID, y = diff_tmax_dec, color = factor(sign(diff_tmax_dec)))) +
  geom_segment(aes(xend = Station_ID, yend = 0), size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "Station_ID", y = "Difference, Tmax Observed and Worldclim  (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('D:/Rdata/Chill_quantification/3D Mapping/temp_comparison/diff_tmax_dec.jpg', height = 12,width = 22,units = 'cm')

ggplot(All_chill, aes(x = Station_ID, y = diff_avg_temp_dec, color = factor(sign(diff_avg_temp_dec)))) +
  geom_segment(aes(xend = Station_ID, yend = 0), size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "Station_ID", y = "Difference, Average Observed and WorldClim Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('D:/Rdata/Chill_quantification/3D Mapping/temp_comparison/diff_avg_temp_dec.jpg', height = 12,width = 22,units = 'cm')


# Statistics for bias
sd_avg_dec <- sd(All_chill$obs_avg_temp_dec - All_chill$avg_temp_dec)
sd_tmin_dec <- sd(All_chill$obs_tmin_dec - All_chill$min_temp_dec)
sd_tmax_dec <- sd(All_chill$obs_tmax_dec - All_chill$max_temp_dec)

All_chill <- cbind(All_chill, sd_avg_dec, sd_tmin_dec,sd_tmax_dec)

All_chill$sd_avg_dec <- apply(All_chill[, c('obs_avg_temp_dec', 'avg_temp_dec')], 1, sd, na.rm = TRUE)
All_chill$sd_Tmin_dec <- apply(All_chill[, c('obs_tmin_dec', 'min_temp_dec')], 1, sd, na.rm = TRUE)
All_chill$sd_Tmax_dec <- apply(All_chill[, c('obs_tmax_dec', 'max_temp_dec')], 1, sd, na.rm = TRUE)

min(All_chill$sd_avg_dec)
max(All_chill$sd_avg_dec)

min(All_chill$sd_Tmin_dec)
max(All_chill$sd_Tmin_dec)

min(All_chill$sd_Tmax_dec)
max(All_chill$sd_Tmax_dec)

#------------------------------------------------------------------------------------------#