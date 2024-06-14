
#------------------------------------------------------------------------------------------#
########################### Creating 3-phases model ###############################
#------------------------------------------------------------------------------------------#

# adjustment model for chill based on mean temperature ("three-phases model")

library(ggplot2)
library(ggrepel)

All_chill <- read.csv('D:/Rdata/Chill_quantification/SWC/All_chill.csv')

# hand-defined breaks for three phase model
# in avg_temp_dec min value is -7.7 and max is 12.1 degree celsius

upper_break = 7
lower_break = 0

# mark stations which are outlying in worldclim
# I already did this step in "Plotting obs vs worldclim for outlier"

# All_chill$outlier_tmean_dec <- abs(All_chill$avg_temp_dec - All_chill$obs_avg_temp_dec) > 2
# sum(All_chill$outlier_tmean_dec)

####test mean temp December

#subset of original station data according to breaks
low <- subset(All_chill, All_chill$avg_temp_dec <= lower_break)
mid <- subset(All_chill, All_chill$avg_temp_dec > lower_break & All_chill$avg_temp_dec <upper_break)
up <- subset(All_chill, All_chill$avg_temp_dec >= upper_break)

#create model for the subset of chill explained by mean temperature in august
model_low <- lm(data = low, X1980_SWC..CP. ~ avg_temp_dec)
model_mid <- lm(data = mid, X1980_SWC..CP. ~ avg_temp_dec)
model_up <- lm(data = up, X1980_SWC..CP. ~ avg_temp_dec)

#plot three phases model with all stations included, outliers marked by name
ggplot(All_chill,aes(x = avg_temp_dec, y = X1980_SWC..CP.)) +
  ylab('Chill Portion')+
  #coord_cartesian(ylim = c(0,100))+
  ylim(c(0,100))+
  geom_point(data = low, aes(x = avg_temp_dec, y = X1980_SWC..CP.), col = 'blue')+
  geom_smooth(data = low, method = 'lm', col = 'blue')+
  geom_point(data = mid, aes(x = avg_temp_dec, y = X1980_SWC..CP.), col = 'black')+
  geom_smooth(data = mid, method = 'lm', col = 'black')+
  geom_point(data = up, aes(x = avg_temp_dec, y = X1980_SWC..CP.), col = 'red')+
  geom_smooth(data = up, method = 'lm', col = 'red')+
  geom_vline(xintercept=c(lower_break,upper_break))+
  geom_label_repel(
    data = subset(All_chill, outlier_tmean_dec),
    aes(label = Station_ID),
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines"),
    min.segment.length = 0,
    nudge_x = 5
  )+  theme_bw()


# check model without the supposed outliers

#subset without outliers
low_clean <- low[!(low$outlier_tmean_dec),]
mid_clean <- mid[!(mid$outlier_tmean_dec),]
up_clean <- up[!(up$outlier_tmean_dec),]

#create model for the subset of chill explained by mean temperature in august
model_low_clean <- lm(data = low_clean, X1980_SWC..CP.~avg_temp_dec)
model_mid_clean <- lm(data = mid_clean, X1980_SWC..CP.~avg_temp_dec)
model_up_clean <- lm(data = up_clean, X1980_SWC..CP.~avg_temp_dec)

stations_clean <- All_chill[!All_chill$outlier_tmean_dec,]

ggplot(All_chill,aes(x = avg_temp_dec, y = X1980_SWC..CP.)) +
  geom_point(col = 'grey')+
  ylab('Safe Winter Chill (CP)')+
  xlab('Average Temperature, December (°C)')+
  #coord_cartesian(ylim = c(0,100))+
  ylim(c(0,100))+
  geom_point(data = low_clean, aes(x = avg_temp_dec, y = X1980_SWC..CP.), col = 'blue')+
  geom_smooth(data = low_clean, method = 'lm', col = 'blue',se=F)+
  geom_smooth(data = low, method = 'lm', col = 'blue',se=F,linetype = "dashed")+
  geom_point(data = mid_clean, aes(x = avg_temp_dec, y = X1980_SWC..CP.), col = 'black')+
  geom_smooth(data = mid_clean, method = 'lm', col = 'black',se=F)+
  geom_smooth(data = mid, method = 'lm', col = 'black',se=F,linetype = "dashed")+
  geom_point(data = up_clean, aes(x = avg_temp_dec, y = X1980_SWC..CP.), col = 'red')+
  geom_smooth(data = up_clean, method = 'lm', col = 'red',se=F)+
  geom_smooth(data = up, method = 'lm', col = 'red',se=F, linetype = "dashed")+
  geom_vline(xintercept=c(lower_break,upper_break))+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D Mapping/3_phases_model/three_phases_dec.jpg', width = 20, height = 15, units = 'cm')


#####test the same thing with November

upper_break = 11
lower_break = 5

#mark outlier
outlier_tmean_nov <- abs(All_chill$avg_temp_nov - All_chill$obs_avg_temp_nov) > 2
All_chill$outlier_tmean_nov <- outlier_tmean_nov

#subset of original station data according to breaks
low <- subset(All_chill, All_chill$avg_temp_nov <= lower_break)
mid <- subset(All_chill, All_chill$avg_temp_nov > lower_break & All_chill$avg_temp_nov <upper_break)
up <- subset(All_chill, All_chill$avg_temp_nov >= upper_break)

#create model for the subset of chill explained by mean temperature in august
model_low <- lm(data = low, X1980_SWC..CP. ~avg_temp_nov)
model_mid <- lm(data = mid, X1980_SWC..CP.~avg_temp_nov)
model_up <- lm(data = up, X1980_SWC..CP.~avg_temp_nov)



ggplot(All_chill,aes(x = avg_temp_nov, y = X1980_SWC..CP.)) +
  ylab('Chill Portion')+
  #coord_cartesian(ylim = c(0,100))+
  ylim(c(0,100))+
  geom_point(data = low, aes(x = avg_temp_nov, y = X1980_SWC..CP.), col = 'blue')+
  geom_smooth(data = low, method = 'lm', col = 'blue')+
  geom_point(data = mid, aes(x = avg_temp_nov, y = X1980_SWC..CP.), col = 'black')+
  geom_smooth(data = mid, method = 'lm', col = 'black')+
  geom_point(data = up, aes(x = avg_temp_nov, y = X1980_SWC..CP.), col = 'red')+
  geom_smooth(data = up, method = 'lm', col = 'red')+
  geom_vline(xintercept=c(lower_break,upper_break))+
  geom_label_repel(
    data = subset(All_chill, outlier_tmean_nov),
    aes(label = station_name),
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines"),
    min.segment.length = 0,
    nudge_x = 5
  )+  theme_bw()


# check model without the supposed outliers

#subset without outliers
low_clean<- low[!(low$outlier_tmean_nov),]
mid_clean<- mid[!(mid$outlier_tmean_nov),]
up_clean<- up[!(up$outlier_tmean_nov),]

#create model for the subset of chill explained by mean temperature in november
model_low_clean <- lm(data = low_clean, X1980_SWC..CP. ~avg_temp_nov)
model_mid_clean <- lm(data = mid_clean, X1980_SWC..CP. ~avg_temp_nov)
model_up_clean <- lm(data = up_clean, X1980_SWC..CP. ~avg_temp_nov)

stations_clean <- All_chill[!All_chill$outlier_tmean_nov,]

ggplot(All_chill,aes(x = avg_temp_nov, y = X1980_SWC..CP.)) +
  geom_point(col = 'grey')+
  ylab('Safe Winter Chill (CP)')+
  xlab('Average Temperature, November (°C)')+
  #coord_cartesian(ylim = c(0,100))+
  ylim(c(0,100))+
  geom_point(data = low_clean, aes(x = avg_temp_nov, y = X1980_SWC..CP.), col = 'blue')+
  geom_smooth(data = low_clean, method = 'lm', col = 'blue',se=F)+
  geom_smooth(data = low, method = 'lm', col = 'blue',se=F,linetype = "dashed")+
  geom_point(data = mid_clean, aes(x = avg_temp_nov, y = X1980_SWC..CP.), col = 'black')+
  geom_smooth(data = mid_clean, method = 'lm', col = 'black',se=F)+
  geom_smooth(data = mid, method = 'lm', col = 'black',se=F,linetype = "dashed")+
  geom_point(data = up_clean, aes(x = avg_temp_nov, y = X1980_SWC..CP.), col = 'red')+
  geom_smooth(data = up_clean, method = 'lm', col = 'red',se=F)+
  geom_smooth(data = up, method = 'lm', col = 'red',se=F, linetype = "dashed")+
  geom_vline(xintercept=c(lower_break,upper_break))+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D Mapping/3_phases_model/three_phases_nov.jpg', width = 20, height = 15, units = 'cm')

#It can be seen that the outliers are more in the low temperature range while in the mid and high
# temperature the outliers are less.