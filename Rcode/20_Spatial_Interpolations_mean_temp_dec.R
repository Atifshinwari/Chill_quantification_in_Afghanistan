                                             
#------------------------------------------------------------------------------------------#
# Spatial Interpolation: Using mean temperature in December#
#------------------------------------------------------------------------------------------#

#devtools::install_gitlab("dickoa/rgeoboundaries")
library(rgeoboundaries)
library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(rgdal)
library(tmap)
library(spatstat) 
library(maptools) 
library(raster) 
library(gstat) 
library(sp) 
require(elevatr)
require(tmaptools)
library(dplyr)
library(stringr)
library(automap)

#------------------------------------------------------------------------------------------#
# 1: Spatial Interpolation: maps with actual estimated chill values# 
#------------------------------------------------------------------------------------------#

# Loading the outline for Afghanistan
sf_afghanistan <- raster::getData("GADM", country ="AF", level =1) %>% sf::st_as_sf() # Get Afghanistan shape file
# Converting the sf object to an sp object
sp_afghanistan <- as(sf_afghanistan, "Spatial")

sf_afghanistan$NAME_1
# reading station coordinates with the projected chill (future and historic)
All_chill <- read.csv('D:/Rdata/Chill_quantification/SWC/All_chill.csv')

# reading the abbreviations and add it to All_chill
station_abr <- read.csv('D:/Rdata/Chill_quantification/SWC/Station_abreviations.csv')

All_chill <- merge(All_chill, station_abr, by = "Station_ID")

# Move 'abr' column to the second position
All_chill <- All_chill[c("Station_ID", "Abr", setdiff(names(All_chill), c("Station_ID", "Abr")))]

# require(stringr)
# Replace the X before the scenario years for "scen_"
All_chill <- rename_with(All_chill, function (x) str_replace(x, "X", "scen_"), starts_with("X"))
colnames(All_chill)[8] <- "scen_historic_SWC..CP."
#colnames(All_chill)[5] <- "Elevation"

# Dealing with problems in the column names:
# Specify the scenario column numbers
scenario_column_numbers <- c(8:13, 16:23)
scenarios<-colnames(All_chill)[c(8:13, 16:23)]

# Clean up scenario names in the scenarios vector
scenarios <- gsub("\\.", " ", scenarios)
scenarios <- gsub("_", "-", scenarios)
scenarios <- make.names(scenarios)
colnames(All_chill)[scenario_column_numbers] <- scenarios

# Converting my data into a spatial object
P <- SpatialPointsDataFrame(All_chill[, c("Longitude", "Latitude")],
                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            data = All_chill[,c(1:50)])

# Making P_plot in order to round the chill values for plot visuals
P_df <- All_chill %>%
  select(Station_ID, Abr, Longitude, Latitude, Province, scen.historic.SWC..CP., scen.1980.SWC..CP., scen.1990.SWC..CP., 
         scen.2000.SWC..CP., scen.2010.SWC..CP., scen.2020.SWC..CP., SSP126.2050.SWC..CP., SSP245.2050.SWC..CP., SSP370.2050.SWC..CP.,
         SSP585.2050.SWC..CP., SSP126.2085.SWC..CP., SSP245.2085.SWC..CP., SSP370.2085.SWC..CP., SSP585.2085.SWC..CP.)

# names to be moved to the left side on the map to avoid overlapping with other names
stations_to_remove <- c("Chichakto", "Asmar", "PuliQarghayi", "Estalef", "Doshi", "TangiTashqurghan", "PuliAlchin")
stations_to_right <- c("Baharak", "Keraman")
stations_to_slightly_right <- c("Dawlatyar")
stations_to_top <- c("Baghlan", "PuliBangi")


stations_remove_all <- c(stations_to_remove, stations_to_right, stations_to_slightly_right,stations_to_top)

P_df <- P_df[!P_df$Station_ID %in% stations_remove_all, ]

P_df_round <- P_df %>% 
  mutate_at(vars(scen.historic.SWC..CP., scen.1980.SWC..CP., scen.1990.SWC..CP., 
                 scen.2000.SWC..CP., scen.2010.SWC..CP., scen.2020.SWC..CP., SSP126.2050.SWC..CP., SSP245.2050.SWC..CP., SSP370.2050.SWC..CP.,
                 SSP585.2050.SWC..CP., SSP126.2085.SWC..CP., SSP245.2085.SWC..CP., SSP370.2085.SWC..CP., SSP585.2085.SWC..CP.),
            ~round(., 0))
P_plot_labels<-SpatialPointsDataFrame(P_df_round[,c("Longitude", "Latitude")],
                                      proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                      data=P_df_round[,c(1:18)])


# For the one removed earlier, make a new dataframe
P_df_left <- All_chill %>%
  select(Station_ID, Abr, Longitude, Latitude, Province, scen.historic.SWC..CP., scen.1980.SWC..CP., scen.1990.SWC..CP., 
         scen.2000.SWC..CP., scen.2010.SWC..CP., scen.2020.SWC..CP., SSP126.2050.SWC..CP., SSP245.2050.SWC..CP., SSP370.2050.SWC..CP.,
         SSP585.2050.SWC..CP., SSP126.2085.SWC..CP., SSP245.2085.SWC..CP., SSP370.2085.SWC..CP., SSP585.2085.SWC..CP.)

P_df_left <- P_df_left[P_df_left$Station_ID %in% stations_to_remove, ]

P_df_round_left <- P_df_left %>% 
  mutate_at(vars(scen.historic.SWC..CP., scen.1980.SWC..CP., scen.1990.SWC..CP., 
                 scen.2000.SWC..CP., scen.2010.SWC..CP., scen.2020.SWC..CP., SSP126.2050.SWC..CP., SSP245.2050.SWC..CP., SSP370.2050.SWC..CP.,
                 SSP585.2050.SWC..CP., SSP126.2085.SWC..CP., SSP245.2085.SWC..CP., SSP370.2085.SWC..CP., SSP585.2085.SWC..CP.),
            ~round(., 0))
P_plot_labels_l<-SpatialPointsDataFrame(P_df_round_left[,c("Longitude", "Latitude")],
                                        proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                        data=P_df_round_left[,c(1:19)])


# names to be moved to the left side

P_df_right <- All_chill %>%
  select(Station_ID, Abr, Longitude, Latitude, Province, scen.historic.SWC..CP., scen.1980.SWC..CP., scen.1990.SWC..CP., 
         scen.2000.SWC..CP., scen.2010.SWC..CP., scen.2020.SWC..CP., SSP126.2050.SWC..CP., SSP245.2050.SWC..CP., SSP370.2050.SWC..CP.,
         SSP585.2050.SWC..CP., SSP126.2085.SWC..CP., SSP245.2085.SWC..CP., SSP370.2085.SWC..CP., SSP585.2085.SWC..CP.)

P_df_right <- P_df_right[P_df_right$Station_ID %in% stations_to_right, ]

P_df_round_right <- P_df_right %>% 
  mutate_at(vars(scen.historic.SWC..CP., scen.1980.SWC..CP., scen.1990.SWC..CP., 
                 scen.2000.SWC..CP., scen.2010.SWC..CP., scen.2020.SWC..CP., SSP126.2050.SWC..CP., SSP245.2050.SWC..CP., SSP370.2050.SWC..CP.,
                 SSP585.2050.SWC..CP., SSP126.2085.SWC..CP., SSP245.2085.SWC..CP., SSP370.2085.SWC..CP., SSP585.2085.SWC..CP.),
            ~round(., 0))
P_plot_labels_r<-SpatialPointsDataFrame(P_df_round_right[,c("Longitude", "Latitude")],
                                        proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                        data=P_df_round_right[,c(1:19)])

# names to be moved slightly right
P_df_s_right <- All_chill %>%
  select(Station_ID, Abr, Longitude, Latitude, Province, scen.historic.SWC..CP., scen.1980.SWC..CP., scen.1990.SWC..CP., 
         scen.2000.SWC..CP., scen.2010.SWC..CP., scen.2020.SWC..CP., SSP126.2050.SWC..CP., SSP245.2050.SWC..CP., SSP370.2050.SWC..CP.,
         SSP585.2050.SWC..CP., SSP126.2085.SWC..CP., SSP245.2085.SWC..CP., SSP370.2085.SWC..CP., SSP585.2085.SWC..CP.)

P_df_s_right <- P_df_s_right[P_df_s_right$Station_ID %in% stations_to_slightly_right, ]

P_df_round_s_right <- P_df_s_right %>% 
  mutate_at(vars(scen.historic.SWC..CP., scen.1980.SWC..CP., scen.1990.SWC..CP., 
                 scen.2000.SWC..CP., scen.2010.SWC..CP., scen.2020.SWC..CP., SSP126.2050.SWC..CP., SSP245.2050.SWC..CP., SSP370.2050.SWC..CP.,
                 SSP585.2050.SWC..CP., SSP126.2085.SWC..CP., SSP245.2085.SWC..CP., SSP370.2085.SWC..CP., SSP585.2085.SWC..CP.),
            ~round(., 0))
P_plot_labels_sr<-SpatialPointsDataFrame(P_df_round_s_right[,c("Longitude", "Latitude")],
                                        proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                        data=P_df_round_s_right[,c(1:19)])


# names to be moved top
P_df_top <- All_chill %>%
  select(Station_ID, Abr, Longitude, Latitude, Province, scen.historic.SWC..CP., scen.1980.SWC..CP., scen.1990.SWC..CP., 
         scen.2000.SWC..CP., scen.2010.SWC..CP., scen.2020.SWC..CP., SSP126.2050.SWC..CP., SSP245.2050.SWC..CP., SSP370.2050.SWC..CP.,
         SSP585.2050.SWC..CP., SSP126.2085.SWC..CP., SSP245.2085.SWC..CP., SSP370.2085.SWC..CP., SSP585.2085.SWC..CP.)

P_df_top <- P_df_top[P_df_top$Station_ID %in% stations_to_top, ]

P_df_round_top <- P_df_top %>% 
  mutate_at(vars(scen.historic.SWC..CP., scen.1980.SWC..CP., scen.1990.SWC..CP., 
                 scen.2000.SWC..CP., scen.2010.SWC..CP., scen.2020.SWC..CP., SSP126.2050.SWC..CP., SSP245.2050.SWC..CP., SSP370.2050.SWC..CP.,
                 SSP585.2050.SWC..CP., SSP126.2085.SWC..CP., SSP245.2085.SWC..CP., SSP370.2085.SWC..CP., SSP585.2085.SWC..CP.),
            ~round(., 0))
P_plot_labels_top<-SpatialPointsDataFrame(P_df_round_top[,c("Longitude", "Latitude")],
                                         proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                         data=P_df_round_top[,c(1:19)])

grd <- as.data.frame(spsample(sp_afghanistan, "regular", n=217620)) # I chose it as per 3 kilo meter area per grid for Afghanistan 217620
names(grd) <- c("Longitude", "Latitude")
coordinates(grd) <- c("Longitude", "Latitude")
gridded(grd) <- TRUE # Create SpatialPixel object
fullgrid(grd) <- TRUE # Create SpatialGrid object
proj4string(grd) <- proj4string(P)


# loading tmin and tmax map for December
avg_temp_dec <- raster('D:/Rdata/Chill_quantification/Chill_mapping/temp_raster/30sec/crop_wc2.1_30s_tavg_12.tif')

# setting extent to outline of Afghanistan
bb_afghanistan <- st_bbox(sf_afghanistan)

extent_afghanistan <- extent(bb_afghanistan["xmin"], bb_afghanistan["xmax"], bb_afghanistan["ymin"], bb_afghanistan["ymax"])
# setting extent to outline of Afghanistan
bb <- extent(extent_afghanistan)

# extracting Afghanistan from world wide map
avg_temp_dec <- crop(avg_temp_dec, bb)
# adjustjusting resolution of temperature map to match the grid of our project
avg_temp.res <- resample(avg_temp_dec, raster(grd))


## producing interpolated layer from elevations of all station locations
# # Manual method, but the autofitVariogram results were better, so I would not use this
# var.smpl.temperature <- variogram(f.temperature, P, cloud = FALSE, cutoff=700, width=70)
# dat.fit.temperature <- fit.variogram(var.smpl.temperature, fit.ranges = FALSE,
#                                      fit.sills = FALSE,
#                                      vgm(psill=30, model="Sph", range = 320, nugget=0))
# plot(var.smpl.temperature, dat.fit.temperature)

# Auto fitting the variogram
f.temperature<-as.formula(avg_temp_dec ~ Longitude + Latitude)
dat.fit.temperature <- automap::autofitVariogram(f.temperature, P)
plot(dat.fit.temperature)

# Performing krigging
dat.krg.temperature <- krige( f.temperature, P, grd, dat.fit.temperature$var_model)
r.temperature<-raster(dat.krg.temperature)


scenarios<-colnames(All_chill)[c(8:13, 16:23)]

# saving scenario names to vector
scenarios_fixed <- c(scen.historic.SWC..CP. = "Historic observed", scen.1980.SWC..CP. = "1980",
                     scen.1990.SWC..CP. =  "1990", scen.2000.SWC..CP. = "2000",
                     scen.2010.SWC..CP. = "2010", scen.2020.SWC..CP. = "2020",
                     SSP126.2050.SWC..CP. = "SSP126 \u2013 2050",
                     SSP245.2050.SWC..CP. = "SSP245 \u2013 2050",
                     SSP370.2050.SWC..CP. = "SSP370 \u2013 2050",
                     SSP585.2050.SWC..CP. = "SSP585 \u2013 2050",
                     SSP126.2085.SWC..CP. = "SSP126 \u2013 2085",
                     SSP245.2085.SWC..CP. = "SSP245 \u2013 2085",
                     SSP370.2085.SWC..CP. = "SSP370 \u2013 2085",
                     SSP585.2085.SWC..CP. = "SSP585 \u2013 2085")


height <- 12
width <- 17

# creating empty list which is used to store chill values
chill_list <- list()

# Creating a plot list
plot_list <- list()

for(scen in names(scenarios_fixed)) {
  #if (scen == "scen.2020.SWC..CP.") { run this line when I need 2020 with legend
    
  # Defining the trend model
  f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))
  
  # Autofitting variogram
  dat.fit <- automap::autofitVariogram(f.1, P)
  
  # Performing the krige interpolation (note the use of the variogram model
  # created in the earlier step)
  dat.krg <- krige( f.1, P, grd, dat.fit$var_model)
  r_krig<-raster(dat.krg)
  r.m <- mask(r_krig, sp_afghanistan)
  
  pred.model<-lm(All_chill[,scen]~ All_chill$avg_temp_dec)
  
  avg.temp.typical.chill<-avg_temp.res*pred.model$coefficients[2]+pred.model$coefficients[1] # coefficient [2] is temperature, coefficient [1] is chill value
  
  avg.temp.typical.chill <- resample(avg.temp.typical.chill, r_krig, method = "bilinear")
  
  temp.int.typical.chill<-r.temperature*pred.model$coefficients[2]+pred.model$coefficients[1]
  
  r<-max(r_krig+avg.temp.typical.chill-temp.int.typical.chill,0)
  r.m <- mask(r, sp_afghanistan)
  
  # producing a raster showing the temperature-related chill adjustments
  r_temp_correct<- r-r_krig
  r.m.temp_correct <- mask(r_temp_correct, sp_afghanistan)
  
  # putting chill values in this list
  chill_list <- append(chill_list, r.m) }
  
#   ##########################################################
#   # plotting correction model for checking extremes #
#   ##########################################################
#   #Extract values from the avg_temp_dec raster at the locations defined in P
#   avg_temp_dec_values <- extract(avg_temp_dec, P)
#   
#   # Calculate avg_temp_typical_chill
#   avg_temp_typical_chill_values <- extract(avg_temp.res, P) * pred.model$coefficients[2] + pred.model$coefficients[1]
#   
#   # Create a dataframe for plotting
#   plot_corr_model_df <- data.frame(
#     avg_temp_dec = avg_temp_dec_values,
#     avg_temp_typical_chill = avg_temp_typical_chill_values,
#     Station_ID = P$Station_ID)
#   
#   # Plot using ggplot
#   # corr_model <- ggplot(plot_corr_model_df, aes(x = avg_temp_dec, y = avg_temp_typical_chill)) +
#   #   geom_point() +
#   #   geom_abline(intercept = pred.model$coefficients[1], slope = pred.model$coefficients[2]) +
#   #   xlab("Average Temperature in December") +
#   #   ylab("Typical Chill Value") +
#   #   ggtitle(paste("Relationship Between Temperature and Chill -", scenarios_fixed[scen]))
#   
#   corr_model <- ggplot(plot_corr_model_df, aes(y = avg_temp_typical_chill, x = avg_temp_dec)) +
#     geom_point() +
#     geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = FALSE) + 
#     geom_text(aes(label = Station_ID), vjust = 1.5, hjust = 0.5, size = 2, check_overlap = TRUE) +
#     ylab('Safe Winter Chill (CP)') +
#     xlab('Average Temperature, December (°C)') +
#     theme_bw()
#   # Save the plot
#   ggsave(filename = paste0("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/corr_model/corr_avg_temp_", scen, ".png", sep = ""), plot = corr_model, width = 10, height = 8)
 

# num_breaks = 9
# breaks <- pretty(All_chill$scen.historic.SWC..CP., n = num_breaks)
# # so my number of breaks are 1:13

#------------------------------------------------------------------------------------------#
# 1.1: Plotting interpolation maps with actual estimated chill values
#------------------------------------------------------------------------------------------#

  f_name <- paste('D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/without_leg_', scen, '.png', sep = '')

  # r.m[r.m>100] <- NA
  # Plot the map
  if(scen %in% scenarios) {

    Temperature_map <- tm_shape(r.m) + # Start with the raster layer
      tm_raster(n=10, palette=get_brewer_pal("RdYlBu", n = 12, contrast = c(0, 0.75)),
                stretch = TRUE, midpoint = 50,
                title="Safe Winter Chill \n(Chill Portions)",
                style="cont", breaks=c(0:12*10)) +
      tm_shape(sp_afghanistan)+
      tm_borders(col='grey')+
      tm_shape(P_plot_labels) + tm_dots(shape = 23, col= "red", size=0.10) + # Add a shape layer with point symbols
      tm_text('Abr', size=0.6, ymod=-0.4, xmod = 0, auto.placement = FALSE) + # Add stations abbreviations from each column
      tm_shape(P_plot_labels_l) + tm_dots(shape = 23, col= "red", size=0.10) + # Add a shape layer with point symbols
      tm_text('Abr', size=0.6, ymod=0, xmod = -0.7, auto.placement = FALSE) + # Add the abbreviation to be moved slightly left (overlap)
      tm_shape(P_plot_labels_r) + tm_dots(shape = 23, col= "red", size=0.10) + # Add a shape layer with point symbols
      tm_text('Abr', size=0.6, ymod=0, xmod = 0.7, auto.placement = FALSE) + # Add the abbreviations to be moved slightly left (overlap)
      tm_shape(P_plot_labels_sr) + tm_dots(shape = 23, col= "red", size=0.10) + # Add a shape layer with point symbols
      tm_text('Abr', size=0.6, ymod=-0.4, xmod = 0.25, auto.placement = FALSE) + # Add the abbreviations to be moved slightly left (overlap)
      #tm_legend(legend.outside=F) +
      #tm_scale_bar(position = c(0.41, 0.055),bg.color = 'transparent', text.color = 1.7, color.dark = "grey20", lwd = 2, text.size = 0.8) +  # Add a scale bar on the left side
      #tm_compass(position = c(0.04, 0.80), text.size = 1) +
      tm_graticules(lines = TRUE, labels.size = 0.9, labels.col = "black", labels.inside.frame = FALSE,
                    alpha=0.3, n.y=5, n.x = 4, labels.margin.y=0.1,) +

      tm_layout(main.title = paste0("      ", scenarios_fixed[[scen]]),
                main.title.position = "center",
                main.title.size = 1.3,
                main.title.color = "black",
                legend.show = FALSE,
                legend.title.size = 1,
                legend.text.size = 0.8,
                legend.text.color = 'black',
                legend.position = c(0.80, 0.04),
                legend.width = 1,
                #attr.color = 'white',
                bg.color = "white",
                outer.bg.color = "white",
                frame = FALSE)
    Temperature_map

    plot_list <- append(plot_list, list(Temperature_map))

    tmap_save(Temperature_map, filename = f_name, height = height, width = width, units = 'cm')

    dev.off() # It helps to close the current plotting device after each iteration to clean the memory and save file
  }
  

#------------------------------------------------------------------------------------------#
# 1.2: Combining the produced maps of HISTORIC scenarios into a single plot
#------------------------------------------------------------------------------------------#

require(magick)
# Combining past scenarios
Scen_hist <- image_read("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/without_leg_scen.historic.SWC..CP..png")
Scen_1980 <- image_read("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/without_leg_scen.1980.SWC..CP..png")
Scen_1990 <- image_read("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/without_leg_scen.1990.SWC..CP..png")
Scen_2000 <- image_read("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/without_leg_scen.2000.SWC..CP..png")
Scen_2010 <- image_read("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/without_leg_scen.2010.SWC..CP..png")
Scen_2020 <- image_read("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/without_leg_scen.2020.SWC..CP..png")

# for image on left (coordinate labels visible)
left_image <- function(image_in) {
  imag <- image_crop(image_read(image_in),
                     geometry_area(width = 1750, height = 2500, x_off = 40, y_off = 20))
  return(imag)
}

# for other images (no coordinate labels)
other_image <- function(image_in) {
  imag <- image_crop(image_read(image_in),
                     geometry_area(width = 1750, height = 2500, x_off = 140, y_off = 20))
  return(imag)
}

# historic Safe Winter Chill figure (Fig. 1)
map_folder <- "D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr"

# # names to be copied
# Scen_hist
# Scen_1980
# Scen_1990
# Scen_2000
# Scen_2010
# Scen_2020

top_left<-image_crop(Scen_hist,
                     geometry_area(width = 1736, height = 1243,x_off = 130, y_off = 105))
top_left<-image_annotate(top_left, "Historic Median", size = 70, gravity = "north",
                         color = "black",
                         degrees=0,location="+25+15", weight = 700)

top_right<-image_crop(Scen_1980,
                      geometry_area(width = 1750, height = 1243,x_off = 280, y_off = 105))
top_right<-image_annotate(top_right, "1980", size = 70, gravity = "north",
                          color = "black",
                          degrees=0,location="-70+15", weight = 700)

middle_left<-image_crop(Scen_1990,
                        geometry_area(width = 1736, height = 1228,x_off = 130, y_off = 120))
middle_left<-image_annotate(middle_left, "1990", size = 70, gravity = "north",
                            color = "black",
                            degrees=0,location="+0+15", weight = 700)

middle_right<-image_crop(Scen_2000,
                         geometry_area(width = 1750, height = 1228,x_off = 280, y_off = 120))
middle_right<-image_annotate(middle_right, "2000", size = 70, gravity = "north",
                             color = "black",
                             degrees=0,location="-70+15", weight = 700)

bottom_left<-image_crop(Scen_2010,
                        geometry_area(width = 1736, height = 1350,x_off = 130, y_off = 120))
bottom_left<-image_annotate(bottom_left, "2010", size = 70, gravity = "north",
                            color = "black",
                            degrees=0,location="+0+15", weight = 700)

bottom_right<-image_crop(Scen_2020,
                         geometry_area(width = 1750, height = 1350,x_off = 280, y_off = 120))
bottom_right<-image_annotate(bottom_right, "2020", size = 70, gravity = "north",
                             color = "black",
                             degrees=0,location="-70+15", weight = 700)

T1 <- image_append(c(top_left, top_right))
M1 <- image_append(c(middle_left, middle_right))
B1 <- image_append(c(bottom_left, bottom_right))

T1M1 <- image_append(c(T1, M1), stack = TRUE)
T1M1B1 <- image_append(c(T1M1, B1), stack = TRUE)
# image_write(T1M1B1,path=paste0(map_folder2,"Figure_1_historic.png"))
image_write(T1M1B1, "D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/final_plots/Figure_historic.png")


# ploting only for 4 past scenarios
M1 <- image_append(c(middle_left, middle_right))
B1 <- image_append(c(bottom_left, bottom_right))

M1B1 <- image_append(c(M1, B1), stack = TRUE)

image_write(M1B1, "D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/final_plots/Figure_1-1_historic.png")

# Here I have saved the map for all historic scenarios with chill values

#------------------------------------------------------------------------------------------#
# 1.3: Comparison of 1980 and 2020 scenario: Combining both into a single plot
#------------------------------------------------------------------------------------------#

# Combining past scenarios
Comp_1980 <- image_read("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/without_leg_scen.1980.SWC..CP..png")
Comp_2020 <- image_read("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/leg_scen.2020.SWC..CP..png")

Co_1980<-image_crop(Comp_1980,
                    geometry_area(width = 1736, height = 1350,x_off = 130, y_off = 105))
Co_1980<-image_annotate(Co_1980, "1980", size = 60, gravity = "north",
                        color = "black",
                        degrees=0,location="+25+15", weight = 700)

Co_2020<-image_crop(Comp_2020,
                    geometry_area(width = 1680, height = 1350,x_off = 280, y_off = 105))
Co_2020<-image_annotate(Co_2020, "2020", size = 60, gravity = "north",
                        color = "black",
                        degrees=0,location="-70+15", weight = 700)

Comp_1980_2020 <- image_append(c(Co_1980, Co_2020))
image_write(Comp_1980_2020, "D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/final_plots/Comparison_1980_2020.png")


#------------------------------------------------------------------------------------------#
# 1.4: Combining the produced maps of FUTURE scenarios into a single plot
#------------------------------------------------------------------------------------------#

map_folder <- "D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/"

ssp126_2050 <- image_read(paste0(map_folder, "without_leg_SSP126.2050.SWC..CP..png"))
ssp126_2085 <- image_read(paste0(map_folder, "without_leg_SSP126.2085.SWC..CP..png"))
ssp245_2050 <- image_read(paste0(map_folder, "without_leg_SSP245.2050.SWC..CP..png"))
ssp245_2085 <- image_read(paste0(map_folder, "without_leg_SSP245.2085.SWC..CP..png"))
ssp370_2050 <- image_read(paste0(map_folder, "without_leg_SSP370.2050.SWC..CP..png"))
ssp370_2085 <- image_read(paste0(map_folder, "without_leg_SSP370.2085.SWC..CP..png"))
ssp585_2050 <- image_read(paste0(map_folder, "without_leg_SSP585.2050.SWC..CP..png"))
ssp585_2085 <- image_read(paste0(map_folder, "without_leg_SSP585.2085.SWC..CP..png"))


# # Creating the legend
# scen="For_legend"
# Path_scen <- "D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/"
# 
# legend<-image_read(paste0(Path_scen, scen,".png"))
# 
# legend<-image_crop(legend,
#                    geometry_area(width = 220, height = 900, x_off = 1650, y_off = 470))
# 
# leg1<-image_frame(legend,geometry = "300x320",color="white")
# leg2<-image_annotate(leg1, "Safe Winter", size = 100, gravity = "southwest",
#                      color = "black",
#                      degrees=0,location="+140+1310", weight = 700)
# leg3<-image_annotate(leg2, "Chill", size = 110, gravity = "southwest", color = "black",
#                      degrees=0,location="+300+1200", weight = 700)
# framed_legend<-image_annotate(leg3, "Chill Portions", size = 80, gravity = "southwest",
#                               color = "black",degrees=270,location="+555+570")
# framed_legend<-image_crop(framed_legend,
#                           geometry_area(width = 580, height = 1000,x_off = 130, y_off = 120))
# image_write(framed_legend, path=paste0(map_folder2,"Legend.png"))



top_left<-image_crop(ssp126_2050,
                     geometry_area(width = 1736, height = 1243,x_off = 130, y_off = 105))
top_left<-image_annotate(top_left, "Optimistic (SSP126)", size = 60, gravity = "north",
                         color = "black",
                         degrees=0,location="+25+15", weight = 700)

top_right<-image_crop(ssp126_2085,
                      geometry_area(width = 1750, height = 1243,x_off = 280, y_off = 105))
top_right<-image_annotate(top_right, "Optimistic (SSP126)", size = 60, gravity = "north",
                          color = "black",
                          degrees=0,location="-140+15", weight = 700)

middle_left<-image_crop(ssp245_2050,
                        geometry_area(width = 1736, height = 1228,x_off = 130, y_off = 120))
middle_left<-image_annotate(middle_left, "Intermediate (SSP245)", size = 60, gravity = "north",
                            color = "black",
                            degrees=0,location="+0+15", weight = 700)

middle_right<-image_crop(ssp245_2085,
                         geometry_area(width = 1750, height = 1228,x_off = 280, y_off = 120))
middle_right<-image_annotate(middle_right, "Intermediate (SSP245)", size = 60, gravity = "north",
                             color = "black",
                             degrees=0,location="-155+15", weight = 700)

middle2_left<-image_crop(ssp370_2050,
                         geometry_area(width = 1736, height = 1228,x_off = 130, y_off = 120))
middle2_left<-image_annotate(middle2_left, "Moderately pessimistic (SSP370)", size = 60, gravity = "north",
                             color = "black",
                             degrees=0,location="-170+15", weight = 700)

middle2_right<-image_crop(ssp370_2085,
                          geometry_area(width = 1750, height = 1228,x_off = 280, y_off = 120))
middle2_right<-image_annotate(middle2_right, "Moderately pessimistic (SSP370)", size = 60, gravity = "north",
                              color = "black",
                              degrees=0,location="-320+15", weight = 700)

bottom_left<-image_crop(ssp585_2050,
                        geometry_area(width = 1736, height = 1350,x_off = 130, y_off = 120))
bottom_left<-image_annotate(bottom_left, "Most pessimistic (SSP585)", size = 60, gravity = "north",
                            color = "black",
                            degrees=0,location="-80+15", weight = 700)

bottom_right<-image_crop(ssp585_2085,
                         geometry_area(width = 1750, height = 1350,x_off = 280, y_off = 120))
bottom_right<-image_annotate(bottom_right, "Most pessimistic (SSP585)", size = 60, gravity = "north",
                             color = "black",
                             degrees=0,location="-230+15", weight = 700)

# # Changing years background color for clarity
# top_left <- image_background(image_transparent(top_left,
#                                                "white", fuzz = 0), "grey90")
# middle_left <- image_background(image_transparent(middle_left,
#                                                   "white", fuzz = 0), "grey90")
# middle2_left <- image_background(image_transparent(middle2_left,
#                                                    "white", fuzz = 0), "grey90")
# bottom_left <- image_background(image_transparent(bottom_left,
#                                                   "white", fuzz = 0), "grey90")

# Now join the sub plots
T1 <- image_append(c(top_left, top_right))
M1 <- image_append(c(middle_left, middle_right))
M2 <- image_append(c(middle2_left, middle2_right))
B1 <- image_append(c(bottom_left, bottom_right))

T1M1 <- image_append(c(T1, M1), stack = TRUE)
M2B1 <- image_append(c(M2, B1), stack = TRUE)

T1M1M2B1 <- image_append(c(T1M1, M2B1), stack = TRUE)

# upper edge for year
upper_edge<-image_blank(3300,100,color="white")
all_scen_cap<-image_append(c(upper_edge,T1M1M2B1),stack=TRUE)
all_scen_cap<-image_annotate(all_scen_cap, '2050', size = 80,location = "+1030-20",
                             color = "black",degrees=0,
                             weight = 800)
all_scen_cap<-image_annotate(all_scen_cap, '2085', size = 80,location = "+2670-20",
                             color = "black",degrees=0,
                             weight = 800)
upper_edge_adj <- image_crop(all_scen_cap, geometry_area(width = 3400, height = 120,x_off = 250, y_off = -30))

# add this to the original plot
T1M1M2B1 <- image_append(c(upper_edge_adj, T1M1M2B1), stack = TRUE)

# image_write(T1M1M2B1,path=paste0(map_folder2,"Figure_1_historic.png"))
image_write(T1M1M2B1, "D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/final_plots/Figure_future.png")

# add legend to the future plot

# add the legend (I already cropped as "framed_legend")
legend<-image_read(paste0(map_folder, "Legend.png"))

# make a white strip on the right side
right_edge<-image_blank(3900,0,color="white")
future_scen_cap<-image_append(c(right_edge,T1M1M2B1),stack=TRUE)

# framed_legend<-image_frame(image_scale(framed_legend, "500"),geometry = "20x20",color="white")
# framed_legend_size<-image_composite(T1M1B1, framed_legend, offset = "+3000+1500")
future_scen_cap <- image_composite(future_scen_cap, legend, gravity = "east")

image_write(future_scen_cap,"D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_CP_values/final_plots/Figure_future.png")

# Here I have saved the interpolated chill maps for future scenarios with actual chill values

# Afterwards, we will produce chill change maps



#------------------------------------------------------------------------------------------#
# 2: Future chill change maps for Afghanistan: the one used in my paper
#------------------------------------------------------------------------------------------#


# df for provinces names assigned to sub regions
sub_regions <- read.csv("D:/Rdata/Chill_quantification/sub_regions.csv")

# converting it to spatial object
sub_reg_sp <- SpatialPointsDataFrame(sub_regions[, c("Longitude", "Latitude")],
                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            data = sub_regions[,c(1:7)])
sub_reg_sf <- st_as_sf(sub_reg_sp)

# Extracting longitude and latitude coordinates
# Badghis <- st_coordinates(sf_afghanistan$geometry[[2]])
# daykundi<- st_coordinates(sf_afghanistan$geometry[[6]])
# Jawzjan<- st_coordinates(sf_afghanistan$geometry[[13]])
# Kapisa<- st_coordinates(sf_afghanistan$geometry[[16]])
# Logar<- st_coordinates(sf_afghanistan$geometry[[21]])
# Nimroz<- st_coordinates(sf_afghanistan$geometry[[23]])
# Nuristan<- st_coordinates(sf_afghanistan$geometry[[24]])
# Paktia<- st_coordinates(sf_afghanistan$geometry[[26]])
# Paktika<- st_coordinates(sf_afghanistan$geometry[[25]])
# Zabul<- st_coordinates(sf_afghanistan$geometry[[34]])

# Listing the provinces I want to select in sub-regions
North <- c("Balkh", "Faryab", "Samangan","Sari Pul",   "Jawzjan")
Northeast <- c("Badakhshan", "Baghlan", "Kunduz", "Takhar")
East <- c("Gardiz","Khost","Kunar",  "Laghman" , "Nangarhar",  "Nuristan","Paktya", "Paktika")
South <- c("Hilmand",  "Kandahar", "Nimroz", "Zabul" ,    "Uruzgan")
West <- c("Farah" ,"Hirat", "Badghis" )
Central <- c("Bamyan",     "Bamyan" , "Ghazni",     "Ghor",      
                 "Parwan" , "Wardak",     "Daykundi", "Kabul"  , "Panjshir",  "Logar","Kapisa")

# Filtering the sf_afghanistan object for selecting specific provinces
N <- sf_afghanistan %>%
  filter(NAME_1 %in% North) %>%
  st_union() # dissolve the geometries of polygons (provinces) into a single polygon (sub_region)

NE <- sf_afghanistan %>%
  filter(NAME_1 %in% Northeast) %>%
  st_union()
E <- sf_afghanistan %>%
  filter(NAME_1 %in% East) %>%
  st_union()
S <- sf_afghanistan %>%
  filter(NAME_1 %in% South) %>%
  st_union()
W <- sf_afghanistan %>%
  filter(NAME_1 %in% West) %>%
  st_union()
C <- sf_afghanistan %>%
  filter(NAME_1 %in% Central) %>%
  st_union()

# Converting to an sf object
N_merged <- st_sf(geometry = N)
NE_merged <- st_sf(geometry = NE)
E_merged <- st_sf(geometry = E)
S_merged <- st_sf(geometry = S)
W_merged <- st_sf(geometry = W)
C_merged <- st_sf(geometry = C)


# changing names in list to scenario names
names(chill_list) <- scenarios
names(plot_list) <- scenarios#

# Generate a baseline raster scenario based on the median across historic simulated scenarios
brick_raster <- brick(chill_list[2 : 6])

# Estimating the median across raster layers
median_raster_scen <- calc(brick_raster, median)

# In case I apply 2000 scenarios for comparison
# brick_raster <- brick(chill_list[6 : 6])

# Creating a list to save the plots
change_maps <- list()

# Creating a directory to save the plots
# dir.create("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/change")

# looping for change between historic median and future scenarios
for(scen in scenarios [10]){
  
  # creating file name
  f_name <- paste('D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/change/border/change_size', scen, '.png', sep = '')
  
  difference <- chill_list[[scen]]- median_raster_scen 
  
  # in order to have a + plus sign with legend positive values (run with this when I need legend)
  breaks_values <- c(-70, -50, -30, -10, 0, 10, 30, 50, 70, 90)
  
  # #breaks_values <- seq(-70, 90, by = 20)
  # labels_values <- ifelse(breaks_values > 0, paste0("+", breaks_values),
  #                         ifelse(breaks_values < 0, as.character(breaks_values),"0"))
  # change_map <- tm_shape(difference) +
  #   tm_raster(palette = get_brewer_pal('RdYlBu', n = 12),
  #             stretch = TRUE,
  #             midpoint = 0,
  #             title = 'SWC relative to\n 1980 \u2013 2020',
  #             style = 'cont',
  #             legend.reverse = TRUE,
  #             breaks = breaks_values,
  #             labels = labels_values,
  #             legend.format = list(suffix = " CP", text.align = "center")) +
  
  change_map <- tm_shape(difference) +
    tm_raster(palette = get_brewer_pal('RdYlBu', n = 12),
              stretch = TRUE,
              midpoint = 0,
              title = 'SWC relative to\n 1980 \u2013 2020',
              style = 'cont', legend.reverse = TRUE, breaks = breaks_values, # the high is 88 low is -66
              legend.format = list(suffix = " CP", text.align = "center")) +
    
    tm_shape(sp_afghanistan) +
    tm_borders(col='grey')+
    
    # Adding borders for sub-regions
    tm_shape(N_merged) +
    tm_borders(col = 'darkgrey', lwd = 2) +  
    
    tm_shape(NE_merged) +
    tm_borders(col = 'darkgrey', lwd = 2) +
    
    tm_shape(E_merged) +
    tm_borders(col = 'darkgrey', lwd = 2) +
    
    tm_shape(S_merged) +
    tm_borders(col = 'darkgrey', lwd = 2) +
    
    tm_shape(W_merged) +
    tm_borders(col = 'darkgrey', lwd = 2) +
    
    tm_shape(C_merged) +
    tm_borders(col = 'darkgrey', lwd = 2) +
    
    tm_shape(P_plot_labels) + tm_dots(shape = 23, col= "red", size=0.10) + # Add a shape layer with point symbols
    tm_text('Abr', size=0.81, ymod=-0.42, xmod = 0, auto.placement = FALSE) + # Add stations abbreviations from each column
    tm_shape(P_plot_labels_l) + tm_dots(shape = 23, col= "red", size=0.10) + # Add a shape layer with point symbols
    tm_text('Abr', size=0.81, ymod=0.15, xmod = -0.9, auto.placement = FALSE) + # Add the abbreviation to be moved slightly left (overlap)
    tm_shape(P_plot_labels_r) + tm_dots(shape = 23, col= "red", size=0.10) + # Add a shape layer with point symbols
    tm_text('Abr', size=0.81, ymod=0, xmod = 0.9, auto.placement = FALSE) + # Add the abbreviations to be moved slightly left (overlap)
    tm_shape(P_plot_labels_sr) + tm_dots(shape = 23, col= "red", size=0.10) + # Add a shape layer with point symbols
    tm_text('Abr', size=0.81, ymod=-0.4, xmod = 0.45, auto.placement = FALSE) + # Add the abbreviations to be moved slightly left (overlap)
    tm_shape(P_plot_labels_top) + tm_dots(shape = 23, col= "red", size=0.10) + # Add a shape layer with point symbols
    tm_text('Abr', size=0.81, ymod=0.46, xmod = 0, auto.placement = FALSE) + # Add the abbreviations to be moved slightly left (overlap)
    
    #tm_legend(legend.outside=F) +
    tm_scale_bar(position = c(0.45, 0.045),bg.color = 'transparent', text.color = 1.7, color.dark = "grey20", lwd = 2, text.size = 1.3) +  # Add a scale bar on the left side
    #tm_compass(position = c(0.04, 0.75), text.size = 1.2) +
    tm_graticules(lines = TRUE, labels.size = 1.2, labels.col = "black", labels.inside.frame = FALSE,
                  alpha=0.3, n.y=5, n.x = 4,) +
    tm_add_legend(type = "symbol", labels = "  Weather station", shape = 23, size = 0.5, col = "red") +
    
    tm_layout(main.title = paste0("      ", scenarios_fixed[[scen]]),
              main.title.position = "center",
              main.title.size = 1.3,
              main.title.color = "black",
              legend.show = FALSE,
              legend.title.size = 1,
              legend.text.size = 0.8,
              legend.text.color = 'black',
              legend.position = c(0.80, 0.10),
              legend.width = 1,
              #attr.color = 'white',
              bg.color = "white",
              outer.bg.color = "white",
              frame = FALSE)
  
  change_map
  
  tmap_save(change_map, filename = f_name, height = height, width = width, units = 'cm')  
  
  # Saving the change maps
  change_maps <- append(change_maps, list(change_map))
  dev.off()
}

# We have produced chill change maps for future scenarios as compared to all historic scenarios as a median

# I am running the above plotting code separately for 7 and 10 scen for printing compass and timescale on those plots

# Lets combine the future change maps into single plot


#------------------------------------------------------------------------------------------#
# 2.1: Combining the produced change maps for FUTURE scenarios into a single plot
#------------------------------------------------------------------------------------------#

require(magick)
map_folder <- "D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/change/border/"

ssp126_2050 <- image_read(paste0(map_folder, "change_sizeSSP126.2050.SWC..CP..png"))
ssp126_2085 <- image_read(paste0(map_folder, "change_sizeSSP126.2085.SWC..CP..png"))
ssp245_2050 <- image_read(paste0(map_folder, "change_sizeSSP245.2050.SWC..CP..png"))
ssp245_2085 <- image_read(paste0(map_folder, "change_sizeSSP245.2085.SWC..CP..png"))
ssp370_2050 <- image_read(paste0(map_folder, "change_sizeSSP370.2050.SWC..CP..png"))
ssp370_2085 <- image_read(paste0(map_folder, "change_sizeSSP370.2085.SWC..CP..png"))
ssp585_2050 <- image_read(paste0(map_folder, "change_sizeSSP585.2050.SWC..CP..png"))
ssp585_2085 <- image_read(paste0(map_folder, "change_sizeSSP585.2085.SWC..CP..png"))

# Format
top_left<-image_crop(ssp126_2050,
                     geometry_area(width = 1770, height = 1186,x_off = 120, y_off = 120))
top_left<-image_annotate(top_left, "Optimistic (SSP126)", size = 62, gravity = "west",
                         color = "black",
                         degrees=0,location="+290-550", weight = 700)

top_right<-image_crop(ssp126_2085,
                      geometry_area(width = 1540, height = 1186,x_off = 330, y_off = 120))
top_right<-image_annotate(top_right, "Optimistic (SSP126)", size = 62, gravity = "west",
                          color = "black",
                          degrees=0,location="+63-550", weight = 700)

middle_left<-image_crop(ssp245_2050,
                        geometry_area(width = 1770, height = 1186,x_off = 120, y_off = 120))
middle_left<-image_annotate(middle_left, "Intermediate (SSP245)", size = 62, gravity = "west",
                            color = "black",
                            degrees=0,location="+290-550", weight = 700)

middle_right<-image_crop(ssp245_2085,
                         geometry_area(width = 1540, height = 1186,x_off = 330, y_off = 120))
middle_right<-image_annotate(middle_right, "Intermediate (SSP245)", size = 62, gravity = "west",
                             color = "black",
                             degrees=0,location="+63-550", weight = 700)

middle2_left<-image_crop(ssp370_2050,
                         geometry_area(width = 1770, height = 1186,x_off = 120, y_off = 120))
middle2_left<-image_annotate(middle2_left, "Moderately pessimistic (SSP370)", size = 62, gravity = "west",
                             color = "black",
                             degrees=0,location="+290-550", weight = 700)

middle2_right<-image_crop(ssp370_2085,
                          geometry_area(width = 1540, height = 1186,x_off = 330, y_off = 120))
middle2_right<-image_annotate(middle2_right, "Moderately pessimistic (SSP370)", size = 62, gravity = "west",
                              color = "black",
                              degrees=0,location="+63-550", weight = 700)

bottom_left<-image_crop(ssp585_2050,
                        geometry_area(width = 1770, height = 1286,x_off = 120, y_off = 120))
bottom_left<-image_annotate(bottom_left, "Most pessimistic (SSP585)", size = 62, gravity = "west",
                            color = "black",
                            degrees=0,location="+290-600", weight = 700)

bottom_right<-image_crop(ssp585_2085,
                         geometry_area(width = 1540, height = 1286,x_off = 330, y_off = 120))
bottom_right<-image_annotate(bottom_right, "Most pessimistic (SSP585)", size = 62, gravity = "west",
                             color = "black",
                             degrees=0,location="+100-600", weight = 700)


# Now joining the sub plots
T1 <- image_append(c(top_left, top_right))
M1 <- image_append(c(middle_left, middle_right))
M2 <- image_append(c(middle2_left, middle2_right))
B1 <- image_append(c(bottom_left, bottom_right))

T1M1 <- image_append(c(T1, M1), stack = TRUE)
M2B1 <- image_append(c(M2, B1), stack = TRUE)

T1M1M2B1 <- image_append(c(T1M1, M2B1), stack = TRUE)

# upper edge for year
upper_edge<-image_blank(3300,110,color="white")
all_scen_cap<-image_append(c(upper_edge,T1M1M2B1),stack=TRUE)
all_scen_cap<-image_annotate(all_scen_cap, '2050', size = 82,location = "+1140-2",
                             color = "black",degrees=0,
                             weight = 800)
all_scen_cap<-image_annotate(all_scen_cap, '2085', size = 82,location = "+2690-2",
                             color = "black",degrees=0,
                             weight = 800)
upper_edge_adj <- image_crop(all_scen_cap, geometry_area(width = 3400, height = 120,x_off = 250, y_off = -30))

# I am adding this to the original plot
T1M1M2B1 <- image_append(c(upper_edge_adj, T1M1M2B1), stack = TRUE)

# image_write(T1M1M2B1,path=paste0(map_folder2,"Figure_1_historic.png"))
# image_write(T1M1M2B1, "D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/change/Future_change.png")

# add legend to the future plot

# add the legend (I already cropped as "Leg")
legend<-image_read(paste0(map_folder, "Leg.png"))

# making a white strip on the right side
right_edge<-image_blank(4100,0,color="white")
future_scen_cap<-image_append(c(right_edge,T1M1M2B1),stack=TRUE)

# resize legend
legend<-image_frame(image_scale(legend, "700"),geometry = "20x20",color="white")

# future_scen_cap<-image_composite(future_scen_cap, legend, offset = "+3000+1500")

future_scen_cap <- image_composite(future_scen_cap, legend, gravity = "east")

image_write(future_scen_cap,"D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/change/border/Future_changesize.png")

# Cleaning
# rm(future_scen_cap, ssp126_2050, ssp126_2085 ,ssp245_2050 ,ssp245_2085 ,ssp370_2050,ssp370_2085,ssp585_2050,ssp585_2085, 
#    legend, Legend, T1M1, T1M1B1, T1M1M2B1, upper_edge, upper_edge_adj,all_scen_cap, top_left, top_right, middle_left, 
#    middle_right, middle2_right, middle2_left,bottom_left, bottom_right)


# name the change_maps list
names(change_maps) <- scenarios[7 : 14]


#------------------------------------------------------------------------------------------#
# 2.2: Historic chill change: Calculating chill change b/w 1980 to 2020 and plotting it
#------------------------------------------------------------------------------------------#

scen <- scenarios[2]
f_name <- paste('D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/change/border/change_1980_2020', scen, '.png', sep = '')

change <- chill_list[["scen.2020.SWC..CP."]]- chill_list[[scen]] # Scenario 2020 - 1980

#breaks_values <- seq(-20, 25, length.out = 6)
breaks_values <- c(-20, -15, -10, -5, 0, 5, 10, 15, 20, 25)
labels_values <- ifelse(breaks_values > 0, paste0("+", breaks_values),
                        ifelse(breaks_values < 0, as.character(breaks_values),"0"))

label_values <- 
  change_map <- tm_shape(change) +
  tm_raster(palette = get_brewer_pal('RdYlBu', n = 12),
            stretch = TRUE,
            midpoint = 0,
            title = 'Safe Winter Chill \n(Chill Portions)',
            style = 'cont', legend.reverse = TRUE, breaks = breaks_values, labels = labels_values,
            legend.format = list(suffix = " CP", text.align = "center")) +
  
  tm_shape(sp_afghanistan) +
  tm_borders(col='grey')+

  # Adding borders for sub-regions
  tm_shape(N_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +  
  
  tm_shape(NE_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +
  
  tm_shape(E_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +
  
  tm_shape(S_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +
  
  tm_shape(W_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +
  
  tm_shape(C_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +
  
  tm_shape(P_plot_labels) + tm_dots(shape = 23, col= "red", size=0.10) + # # Add a shape layer with point symbols
  tm_text('Abr', size=0.77, ymod=-0.42, xmod = 0, auto.placement = FALSE) + # # Add stations abbreviations from each column
  tm_shape(P_plot_labels_l) + tm_dots(shape = 23, col= "red", size=0.10) + # # Add a shape layer with point symbols
  tm_text('Abr', size=0.77, ymod=0.15, xmod = -0.9, auto.placement = FALSE) + # # Add the abbreviation to be moved slightly left (overlap)
  tm_shape(P_plot_labels_r) + tm_dots(shape = 23, col= "red", size=0.10) + # # Add a shape layer with point symbols
  tm_text('Abr', size=0.77, ymod=0, xmod = 0.9, auto.placement = FALSE) + # # Add the abbreviations to be moved slightly left (overlap)
  tm_shape(P_plot_labels_sr) + tm_dots(shape = 23, col= "red", size=0.10) + # # Add a shape layer with point symbols
  tm_text('Abr', size=0.77, ymod=-0.4, xmod = 0.45, auto.placement = FALSE) + # # Add the abbreviations to be moved slightly left (overlap)
  tm_shape(P_plot_labels_top) + tm_dots(shape = 23, col= "red", size=0.10) + # # Add a shape layer with point symbols
  tm_text('Abr', size=0.77, ymod=0.46, xmod = 0, auto.placement = FALSE) + # # Add the abbreviations to be moved slightly left (overlap)
  
  tm_legend(legend.outside=F) +
  tm_scale_bar(position = c(0.41, 0.055),bg.color = 'transparent', text.color = 1.7, color.dark = "grey20", lwd = 2, text.size = 0.8) +  # Add a scale bar on the left side
  tm_compass(position = c(0.04, 0.77), text.size = 1) +
  tm_graticules(lines = TRUE, labels.size = 0.9, labels.col = "black", labels.inside.frame = FALSE,
                alpha=0.3, n.y=5, n.x = 4,) +
  tm_add_legend(type = "symbol", labels = "  Weather station", shape = 23, size = 0.5, col = "red") +
  
  tm_layout(main.title = paste0("      Chill change 1980 \u2013 2020"),
            main.title.position = "center",
            main.title.size = 1.3,
            main.title.color = "black",
            legend.show = TRUE,
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.text.color = 'black',
            legend.position = c(0.80, 0.12),
            legend.width = 1.2,
            #attr.color = 'white',
            bg.color = "white",
            outer.bg.color = "white",
            frame = FALSE)

change_map

tmap_save(change_map, filename = f_name, height = height, width = width, units = 'cm') 


# Historic 1980 scenario

f_name <- paste('D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/change/border/historic_1980_', scen, '.png', sep = '')

breaks_values <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)

# selecting 1980 scenario for comparison
historic_1980 <- brick(chill_list[2 : 2])

historic_1980_scen <- tm_shape(historic_1980) +
  tm_raster(palette = get_brewer_pal('RdYlBu', n = 12),
            stretch = TRUE,
            midpoint = 60,
            title = 'Safe Winter Chill \n(Chill Portions)',
            style = 'cont', legend.reverse = TRUE, breaks = breaks_values,
            legend.format = list(suffix = " CP", text.align = "center")) +
  
  tm_shape(sp_afghanistan) +
  tm_borders(col='grey')+
  
  # Adding borders for sub-regions
  tm_shape(N_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +  
  
  tm_shape(NE_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +
  
  tm_shape(E_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +
  
  tm_shape(S_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +
  
  tm_shape(W_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +
  
  tm_shape(C_merged) +
  tm_borders(col = 'darkgrey', lwd = 2) +
  
  tm_shape(P_plot_labels) + tm_dots(shape = 23, col= "red", size=0.10) + # # Add a shape layer with point symbols
  tm_text('Abr', size=0.77, ymod=-0.42, xmod = 0, auto.placement = FALSE) + # # Add stations abbreviations from each column
  tm_shape(P_plot_labels_l) + tm_dots(shape = 23, col= "red", size=0.10) + # # Add a shape layer with point symbols
  tm_text('Abr', size=0.77, ymod=0.15, xmod = -0.9, auto.placement = FALSE) + # # Add the abbreviation to be moved slightly left (overlap)
  tm_shape(P_plot_labels_r) + tm_dots(shape = 23, col= "red", size=0.10) + # # Add a shape layer with point symbols
  tm_text('Abr', size=0.77, ymod=0, xmod = 0.9, auto.placement = FALSE) + # # Add the abbreviations to be moved slightly left (overlap)
  tm_shape(P_plot_labels_sr) + tm_dots(shape = 23, col= "red", size=0.10) + # # Add a shape layer with point symbols
  tm_text('Abr', size=0.77, ymod=-0.4, xmod = 0.45, auto.placement = FALSE) + # # Add the abbreviations to be moved slightly left (overlap)
  tm_shape(P_plot_labels_top) + tm_dots(shape = 23, col= "red", size=0.10) + # # Add a shape layer with point symbols
  tm_text('Abr', size=0.77, ymod=0.46, xmod = 0, auto.placement = FALSE) + # # Add the abbreviations to be moved slightly left (overlap)
  
  tm_legend(legend.outside=F) +
  tm_scale_bar(position = c(0.41, 0.055),bg.color = 'transparent', text.color = 1.7, color.dark = "grey20", lwd = 2, text.size = 0.8) +  # Add a scale bar on the left side
  tm_compass(position = c(0.04, 0.77), text.size = 1) +
  tm_graticules(lines = TRUE, labels.size = 0.9, labels.col = "black", labels.inside.frame = FALSE,
                alpha=0.3, n.y=5, n.x = 4,) +
  tm_add_legend(type = "symbol", labels = "  Weather station", shape = 23, size = 0.5, col = "red") +
  
  tm_layout(main.title = paste0("      Historic Safe Winter Chill (1980)"),
            main.title.position = "center",
            main.title.size = 1.3,
            main.title.color = "black",
            legend.show = TRUE,
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.text.color = 'black',
            legend.position = c(0.80, 0.12),
            legend.width = 1.2,
            #attr.color = 'white',
            bg.color = "white",
            outer.bg.color = "white",
            frame = FALSE)

historic_1980_scen

tmap_save(historic_1980_scen, filename = f_name, height = height, width = width, units = 'cm')  

library(magick)
# Combine both maps
change_1980_2020 <- image_read("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/change/border/change_1980_2020scen.1980.SWC..CP..png")
historic_1980 <- image_read("D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/change/border/historic_1980_scen.1980.SWC..CP..png")

historic_and_change <-image_append(c(historic_1980, change_1980_2020)) # side by side
image_write(historic_and_change, "D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/change/border/historic_and_change.png")

historic_and_change_overlay <- image_append(c(historic_1980, change_1980_2020), stack = TRUE)
image_write(historic_and_change_overlay, "D:/Rdata/Chill_quantification/3D mapping/Temperature_mapping_figure/with_abr/change/border/historic_and_change_overlay.png")



#------------------------------------------------------------------------------------------#
# 3: Frequency of absolute and change in chill
#------------------------------------------------------------------------------------------#

# creating data frame with frequencies of absolute CP

# empty data frame
absolute_chill_df <- data.frame(NULL)

# looping other every entry of the list
for(i in 1 : length(chill_list)){
  #subset data frame and remove NAs
  sub_df <- na.omit(as.data.frame(chill_list[[i]])) 
  #create bins of chill
  out <- table(cut(sub_df$layer, breaks = seq(0, 130, by = 15), include.lowest = T))
  #save binned chill to data frame
  absolute_chill_df <- rbind(absolute_chill_df, out)
}
# adding names to the dataframe 
absolute_chill_df <- cbind(names(chill_list), absolute_chill_df)
names(absolute_chill_df) <- c('scenario_year', names(out))      
# saving the table
write.csv(absolute_chill_df, 'D:/Rdata/Chill_quantification/SWC/absolute_chill_binned.csv',
          row.names = F)   


# creating data frame with frequencies of change in chill

change_chill_list <- list()

for(i in 7 : length(chill_list)){
  change <- chill_list[[i]] - median_raster_scen
  change_chill_list <- append(change_chill_list, change)
}

change <- chill_list[["scen.2020.SWC..CP."]] - chill_list[["scen.1980.SWC..CP."]]
change_chill_list <- append(change_chill_list, change)

names(change_chill_list) <- c(names(chill_list[7:14 : length(chill_list)]), 'scen_2020_vs_scen_1980')

change_chill_df <- data.frame(NULL)

for(i in 1 : length(change_chill_list)){
  sub_df <- na.omit(as.data.frame(change_chill_list[[i]])) 
  out <- table(cut(sub_df$layer, breaks = seq(-70, 90, by = 15)))
  change_chill_df <- rbind(change_chill_df, out)
  
}

change_chill_df <- cbind(names(change_chill_list), change_chill_df)
names(change_chill_df) <- c('year', names(out))      

write.csv(change_chill_df, 'D:/Rdata/Chill_quantification/SWC//change_chill_binned.csv',
          row.names = F)


# Here I am trying to get/capture locations with highest and lowest chill values #

#------------------------------------------------------------------------------------------#
# 4: Capturing locations with highest and lowest estimated/interpolated chill values
#------------------------------------------------------------------------------------------#

max_values <- list()
min_values <- list()

max_coors <- list()
min_coors <- list()

for (i in 1: length(chill_list)) {
  rast <- chill_list[[i]]
  
  # getting grid with max and min chill
  max_grid <- which.max(rast)
  min_grid <- which.min(rast)
  
  # extracting chill values
  max_val <- rast[max_grid]
  min_val <- rast[min_grid]
  
  # appending chill values to list
  max_values <- append(max_values, max_val)
  min_values <- append(min_values, min_val)
  
  # converting grid numbers to coordinates
  max_coor <- xyFromCell(rast, max_grid)
  min_coor <- xyFromCell(rast, min_grid)
  
  # putting in the list
  max_coors <- append(max_coors, list(max_coor))
  min_coors <- append(min_coors, list(min_coor))
  
} 

#Apply names
names(max_values) <- scenarios
names(min_values) <- scenarios

names(min_coors) <- scenarios
names(max_coors) <- scenarios

# make a data frame of these values
overall_high_low_chill <- data.frame(
  scenario = names(chill_list),
  long_high = sapply(max_coors, function(x) x[,1]),
  lat_high = sapply(max_coors, function(x) x[,2]),
  chill_high = unlist(max_values),
  long_low = sapply(min_coors, function(x) x[,1]),
  lat_low = sapply(min_coors, function(x) x[,2]),
  chill_low = unlist(min_values)
)

rownames(overall_high_low_chill) <- NULL
overall_high_low_chill[,2:7] <- round(overall_high_low_chill[,2:7], 2)
overall_high_low_chill$chill_high <- round(overall_high_low_chill$chill_high)
overall_high_low_chill$chill_low <- round(overall_high_low_chill$chill_low)

write.csv(overall_high_low_chill, "D:/Rdata/Chill_quantification/SWC/overall_max_min_chill.csv", row.names = FALSE)

# plotting
# Now, plot each raster and overlay the max and min points
width_pixels <- 800
height_pixels <- 600

scenarios <-  c("scen.historic.SWC..CP.", "scen.1980.SWC..CP.", "scen.1990.SWC..CP.", "scen.2000.SWC..CP.", "scen.2010.SWC..CP.",
                "scen.2020.SWC..CP.", "SSP126.2050.SWC..CP.", "SSP245.2050.SWC..CP.", "SSP370.2050.SWC..CP.",
                "SSP585.2050.SWC..CP.", "SSP126.2085.SWC..CP.", "SSP245.2085.SWC..CP.", "SSP370.2085.SWC..CP.", "SSP585.2085.SWC..CP.")  

for(scen in scenarios [1 : 14]){
  
  #create file name
  f_name <- paste('D:/Rdata/Chill_quantification/SWC/max_min_chill_', scen, '.png', sep = '')
  
  # Start PNG device
  png(filename = f_name, width = width_pixels, height = height_pixels)
  
  
  plot(chill_list[[scen]])
  points(max_coors[[scen]], pch=19, col="blue", cex=1.5)  # max in red
  points(min_coors[[scen]], pch=19, col="red", cex=1.5) # min in blue
  
  # Ending PNG device
  dev.off() 
  
}