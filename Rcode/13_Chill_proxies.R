#------------------------------------------------------------------------------------------#
### Apply the chill proxies ### ### Apply the chill proxies ### ### Apply the chill proxies 
#------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------#
# 1. Chill- Elevation effect
#------------------------------------------------------------------------------------------#

library(tidyverse)

# Read my stations, it consist of coordinates, elevation and SWC for past scenarios etc.
All_chill <- read.csv('D:/Rdata/Chill_quantification/SWC/All_chill.csv')

# Creating latitude groups: It is useful for visualization of data or analyze patterns in data
# across different latitude ranges. Since Afghanistan lies between 29N and 38N therefore, the 
# range 25 to 40 at every 5 degrees would be suitable.

All_chill$lat_group <- cut(All_chill$Latitude,breaks = seq(25,40,by= 1.4))
levels(All_chill$lat_group)
All_chill$lat_group <- factor(All_chill$lat_group, levels=rev(levels(All_chill$lat_group)))

# The initial idea was to check it based on latitude group but since the location is small we decided to avoid lat group.

ggplot(All_chill, aes(y = X1980_SWC..CP., x = Elevation..m...m.s.l.)) +
  geom_point() + 
  #geom_smooth(method = 'lm', formula = y ~ poly(x,2), se = FALSE) + 
  geom_text(aes(label = Province), vjust = 1.5, hjust = 0.5, size = 2, check_overlap = TRUE) +
  #facet_wrap(~lat_group) + 
  ylab('Safe Winter Chill 1980 (CP)')+
  xlab('Elevation (m)')+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D mapping/chill_proxies/elevation/chill_elevation_Dec_NoLine.jpg',height = 10,width = 15,units = 'cm')


#------------------------------------------------------------------------------------------#
# 2. Chill- minimum monthly temperature effect
#------------------------------------------------------------------------------------------#

# library(raster)
# library(sp)
# library(rgdal)
# # Load the required packages
# library(raster)
# library(rgdal)
# library(tmap)


# my station file is All_chill, lets change it to CRS
All_chill_crs <- SpatialPointsDataFrame(All_chill[,c("Longitude", "Latitude")],
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                             data = All_chill[,c(6:22)])


# Download temperature data and then crop it for Afghanistan shape file
# I will download for both 10 minutes and 30 seconds  resolution but I use 30 second

# i) Resolution 10 minutes # # Resolution 10 minutes # # Resolution 10 minutes ## Resolution 10 minutes #

# for the current period (1970-2000) from the WorldClim Version 2 website.
urls <- c(
  "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_tavg.zip",
  "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_tmin.zip",
  "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_tmax.zip"
)

# Setting the target directory for downloading and extracting data
target_dir <- "D:/Rdata/Chill_quantification/Chill_mapping/temp_raster"

# Download and unzip the files into the target directory
files <- basename(urls)
for (i in seq_along(urls)) {
  download.file(urls[i], file.path(target_dir, files[i]))
  unzip(file.path(target_dir, files[i]), exdir = target_dir)
}

# Get all the .tif files from the target directory
tif_files <- list.files(target_dir, pattern = "\\.tif$", full.names = TRUE)

# Load the shapefile for Afghanistan
sf_afghanistan <- raster::getData("GADM", country ="AF", level =1) %>% sf::st_as_sf() # Get Afghanistan shape file

# Load the raster files (temperature) and crop them
rasters <- list()
for (i in seq_along(tif_files)) {
  # Load the raster from the extracted file
  raster <- raster::raster(tif_files[i])
  # Crop the raster
  crop_raster <- raster::crop(raster, sf_afghanistan)
  # Save the cropped raster
  writeRaster(crop_raster, filename = file.path(target_dir, paste0("crop_", tools::file_path_sans_ext(basename(tif_files[i])), ".tif")))
  rasters[[i]] <- crop_raster
}


# Resolution 30 seconds (high resolution) # # Resolution 30 seconds (high resolution) # # Resolution 30 seconds (high resolution) #
urls <- c(
  "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tavg.zip",
  "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tmin.zip",
  "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tmax.zip"
)

# Setting the target directory for downloading and extracting data
target_dir <- "D:/Rdata/Chill_quantification/Chill_mapping/temp_raster/30sec"

# Download and unzip the files into the target directory
files <- basename(urls)
for (i in seq_along(urls)) {
  download.file(urls[i], file.path(target_dir, files[i]))
  unzip(file.path(target_dir, files[i]), exdir = target_dir)
}

# For some reason I have manually unzipped the files in the directory

# Get all the .tif files from the target directory
tif_files <- list.files(target_dir, pattern = "\\.tif$", full.names = TRUE)

# Load the shapefile for Afghanistan - I already have it loaded as sf_afghanistan

# Load the raster files (temperature) and crop them
rasters <- list()
for (i in seq_along(tif_files)) {
  # Load the raster from the extracted file
  raster <- raster::raster(tif_files[i])
  # Crop the raster
  crop_raster <- raster::crop(raster, sf_afghanistan)
  # Save the cropped raster
  writeRaster(crop_raster, filename = file.path(target_dir, paste0("crop_", tools::file_path_sans_ext(basename(tif_files[i])), ".tif")))
  rasters[[i]] <- crop_raster
}


# read average monthly temperature data for our desired months
avg_temp_dec <- raster('D:/Rdata/Chill_quantification/Chill_mapping/temp_raster/30sec/crop_wc2.1_30s_tavg_12.tif')
min_temp_dec <- raster('D:/Rdata/Chill_quantification/Chill_mapping/temp_raster/30sec/crop_wc2.1_30s_tmin_12.tif')
max_temp_dec <- raster('D:/Rdata/Chill_quantification/Chill_mapping/temp_raster/30sec/crop_wc2.1_30s_tmax_12.tif')

#adjust bounding box for plotting
# We need to use the latitude and longitude for Afghanistan

b <- bbox(avg_temp_dec)
b[1,] <- c(60.5, 29.4) # Lower left corner: 60.5 (longitude), 29.4 (latitude)
b[2,] <- c(74.9, 38.5) # Upper right corner: 74.9 (longitude), 38.5 (latitude)
b <- bbox(t(b))

require(tmap)
#check for one example map and points
tm_shape(avg_temp_dec, bbox = b) + 
  tm_raster(breaks = c(-30, -20, -10, 0, 10, 20, 30)) + # the breaks values corresponds to December temp so I adjust it accordingly
  tm_shape(All_chill_crs) + tm_dots(size = 0.2)+
  tm_grid()+
  tm_legend(legend.outside=TRUE)

# extract avg temp per month from the files to the All_chill of station data
All_chill$avg_temp_dec <- raster::extract(avg_temp_dec,All_chill_crs)
All_chill$min_temp_dec <- raster::extract(min_temp_dec,All_chill_crs)
All_chill$max_temp_dec <- raster::extract(max_temp_dec,All_chill_crs)


# As now I have extracted mean monthly temps, lets plot it
# 1. Monthly average temperature - December
ggplot(All_chill,aes(y=X1980_SWC..CP.,x=avg_temp_dec))+geom_point()+
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se = FALSE) + 
  geom_text(aes(label = Province), vjust = 1.5, hjust = 0.5, size = 2, check_overlap = TRUE) +
  #facet_wrap(~lat_group)+
  ylab('Safe Winter Chill (CP)')+
  xlab('Average Temperature, December (°C)')+
  theme_bw()

ggsave('D:/Rdata/Chill_quantification/3D mapping/chill_proxies/temperature/chill_temp_avgDec.jpg',height = 10,width = 15,units = 'cm')

# 2. Monthly minimum temperature - December
ggplot(All_chill,aes(y=X1980_SWC..CP.,x=min_temp_dec))+geom_point()+
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se = FALSE) + 
  geom_text(aes(label = Station_ID), vjust = 1.5, hjust = 0.5, size = 2, check_overlap = TRUE) +
  #facet_wrap(~lat_group)+
  ylab('Safe Winter Chill (CP)')+
  xlab('Minimum Temperature, December (°C)')+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D mapping/chill_proxies/temperature/chill_temp_minDec.jpg',height = 10,width = 15,units = 'cm')

# 3. Monthly maximum temperature - December
ggplot(All_chill,aes(y=X1980_SWC..CP.,x=max_temp_dec))+geom_point()+
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se = FALSE) + 
  geom_text(aes(label = Province), vjust = 1.5, hjust = 0.5, size = 2, check_overlap = TRUE) +
  #facet_wrap(~lat_group)+
  ylab('Safe Winter Chill (CP)')+
  xlab('Maximum Temperature, December (°C)')+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D mapping/chill_proxies/temperature/chill_temp_maxDec.jpg',height = 10,width = 15,units = 'cm')

# 3. Combined monthly minimum and maximum temperature - December
test_sub <- subset(stations, stations$Latitude < -10 & stations$Latitude > -45)
test_sub$lat_group <- cut(test_sub$Latitude,breaks = seq(-10,-45,by=-5))
test_sub$lat_group <- factor(test_sub$lat_group, levels=rev(levels(test_sub$lat_group)))

ggplot()+
  geom_point(data = All_chill,aes(y=X1980_SWC..CP.,x=obs_tmin_dec),col = 'black', size = 2)+
  geom_smooth(data = All_chill,mapping = aes(y=X1980_SWC..CP.,x=obs_tmin_dec), se = FALSE,method = 'lm',formula = y ~ poly(x,2), col='blue')+
  geom_point(data = All_chill,aes(y=X1980_SWC..CP.,x=obs_tmax_dec),col = 'black', size = 2)+
  geom_smooth(data = All_chill,mapping = aes(y=X1980_SWC..CP.,x=obs_tmax_dec), se = FALSE,method = 'lm', formula = y ~ poly(x,2), col = 'red')+
  #facet_wrap(~lat_group)+
  ylab('Safe Winter Chill (CP)')+
  xlab('Minimum (blue) and Maximum (red) Temperature (°C) in December')+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D mapping/chill_proxies/temperature/chill_temp_obs_min_maxDec_.jpg',height = 10,width = 15,units = 'cm')

# Plotting temperature vs elevation

ggplot(All_chill,aes(y=obs_avg_temp_dec,x=Elevation..m...m.s.l.))+geom_point()+
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se = FALSE) + 
  geom_text(aes(label = Station_ID), vjust = 1.5, hjust = 0.5, size = 2, check_overlap = TRUE) +
  #facet_wrap(~lat_group)+
  ylab('Average Temperature, December (°C)')+
  xlab('Station Elevation (m)')+
  theme_bw()
ggsave('D:/Rdata/Chill_quantification/3D mapping/chill_proxies/elevation_obs_tempDec.jpg',height = 10,width = 15,units = 'cm')



