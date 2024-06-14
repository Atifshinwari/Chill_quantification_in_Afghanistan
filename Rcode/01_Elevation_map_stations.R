#------------------------------------------------------------------------------------------#
# Afghanistan elevation map including my study stations location#
#------------------------------------------------------------------------------------------#

# Install and load necessary libraries
install.packages(c("elevatr", "sp", "raster", "ggplot2"))
library(elevatr)
library(sp)
library(raster)
library(ggplot2)
library(automap)
library(tmap)
library(RColorBrewer)
library(rgeoboundaries)
library(sf)
library(viridis)
library(rgdal)
library(spatstat)
library(maptools)
library(gstat)
require(tmaptools)
library(dplyr)
library(stringr)

#------------------------------------------------------------------------------------------#
# 01: Before moving into downloading raster and plotting for elevation I am formatting the 
# names of stations for proper positions on the elevation map
#------------------------------------------------------------------------------------------#

# Load the outline for Afghanistan
sf_afghanistan <- raster::getData("GADM", country ="AF", level =1) %>% sf::st_as_sf() # Get Afghanistan shape file

# Convert the sf object to an sp object
sp_afghanistan <- as(sf_afghanistan, "Spatial")

#read station coordinates with the projected chill (future and historic)
All_chill <- read.csv('D:/Rdata/Chill_quantification/SWC/All_chill.csv')

# read the abbreviations and add it to All_chill
station_abr <- read.csv('D:/Rdata/Chill_quantification/SWC/Station_abreviations.csv')

All_chill <- merge(All_chill, station_abr, by = "Station_ID")

# Move 'abr' column to the second position
All_chill <- All_chill[c("Station_ID", "Abr", setdiff(names(All_chill), c("Station_ID", "Abr")))]

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

# Convert my data into a spatial object
P <- SpatialPointsDataFrame(All_chill[, c("Longitude", "Latitude")],
                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            data = All_chill[,c(1:50)])

# Making P_plot in order to round the chill values for plot visuals
P_df <- All_chill %>%
  select(Station_ID, Abr, Longitude, Latitude, Province, scen.historic.SWC..CP., scen.1980.SWC..CP., scen.1990.SWC..CP., 
         scen.2000.SWC..CP., scen.2010.SWC..CP., scen.2020.SWC..CP., SSP126.2050.SWC..CP., SSP245.2050.SWC..CP., SSP370.2050.SWC..CP.,
         SSP585.2050.SWC..CP., SSP126.2085.SWC..CP., SSP245.2085.SWC..CP., SSP370.2085.SWC..CP., SSP585.2085.SWC..CP.)

# names to be moved to the left side
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


# For the one removed earlier, make a new data frame
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


#------------------------------------------------------------------------------------------#
# 2: Shape file of Afghanistan
#------------------------------------------------------------------------------------------#

# Load the outline for Afghanistan
sf_afghanistan <- raster::getData("GADM", country ="AF", level =1) %>% sf::st_as_sf() # Get Afghanistan shape file
# Convert the sf object to an sp object
sp_afghanistan <- as(sf_afghanistan, "Spatial")


# Define the geographic extent of Afghanistan (approximate coordinates)
afghan_extent <- as(extent(60.5, 74.9, 29.4, 38.5), "SpatialPolygons")
proj4string(afghan_extent) <- CRS("+proj=longlat +datum=WGS84")

#------------------------------------------------------------------------------------------#
# 3: Download elevation raster for Afghanistan
#------------------------------------------------------------------------------------------#

# Download elevation data for the defined extent
afghan_elevation <- get_elev_raster(afghan_extent, z = 8, src = "aws")
afghan_elevation <- mask(afghan_elevation, sp_afghanistan) # this is very big vector size of 2.5 Gb

elv_path <- "D:/Rdata/Chill_quantification/Elevation_map/"
# writeRaster(afghan_elevation, filename = elv_path, format = "GTiff", overwrite = TRUE)

# Get the current resolution of the raster
current_res <- res(afghan_elevation)
cat("Current resolution: ", current_res, "\n")

# Calculate the new resolution (1.5 times the current resolution)
new_res <- current_res * 2
cat("New resolution: ", new_res, "\n")

# Create a template raster with the new resolution
template <- raster(extent(afghan_elevation), res = new_res)

# Resample the original raster using the template
afghan_elevation_resized <- resample(afghan_elevation, template, method = "bilinear")

projection(afghan_elevation_resized) <- "+proj=longlat +datum=WGS84"

# Reproject the raster
afghan_elevation_resized <- projectRaster(afghan_elevation_resized, crs = "+proj=longlat +datum=WGS84")


#------------------------------------------------------------------------------------------#
# 4:normal size raster (I am using smaller size raster)
#------------------------------------------------------------------------------------------#

elv_path <- "D:/Rdata/Chill_quantification/Elevation_map/"
afghan_elevation_reduced2 <- raster(paste0(elv_path, "afghan_elevation.tif")) # read the raster

# Adjust resolution for the second one
# Get the current resolution of the raster
current_res2 <- res(afghan_elevation_reduced2)
cat("Current resolution: ", current_res2, "\n")

# Calculate the new resolution (1.5 times the current resolution)
new_res2 <- current_res2 * 2
cat("New resolution: ", new_res2, "\n")

# Create a template raster with the new resolution
template2 <- raster(extent(afghan_elevation_reduced2), res = new_res2)
afghan_elevation_reduced2 <- resample(afghan_elevation_reduced2, template2, method = "bilinear")

projection(afghan_elevation_reduced2) <- "+proj=longlat +datum=WGS84"

# Reproject the raster
afghan_elevation_reduced2 <- projectRaster(afghan_elevation_reduced2, crs = "+proj=longlat +datum=WGS84")


#------------------------------------------------------------------------------------------#
# 5: Plotting
#------------------------------------------------------------------------------------------#

save_path <- "D:/Rdata/Chill_quantification/Elevation_map/Elevation_final_smallsize_TEST.png"
height <- 12
width <- 18

color_comb <- c("#e65b43","#f18b5a","#fbc17c","#fde6a4","#f7f7f7", "#d1e5f0","#92c5de","#4393c3","#2166ac" )

# Create the temperature map
Temperature_map <- tm_shape(afghan_elevation_reduced2) +
  tm_raster(palette= color_comb,
            style = 'cont',
            stretch = TRUE,
            title="Elevation (m a.s.l.)") +
  tm_shape(sp_afghanistan)+
  tm_borders(col='grey', lwd = 1.5, lty = "solid", alpha = 0.7) +
  
  tm_shape(P_plot_labels) + tm_dots(shape = 23, col= "red", size=0.15) + # Add a shape layer with point symbols
  tm_text('Abr', size=0.68, ymod=-0.42, xmod = 0, auto.placement = FALSE) + # Add stations abbreviations from each column
  tm_shape(P_plot_labels_l) + tm_dots(shape = 23, col= "red", size=0.15) + # Add a shape layer with point symbols
  tm_text('Abr', size=0.68, ymod=0.15, xmod = -0.9, auto.placement = FALSE) + # Add the abbreviation to be moved slightly left (overlap)
  tm_shape(P_plot_labels_r) + tm_dots(shape = 23, col= "red", size=0.15) + # Add a shape layer with point symbols
  tm_text('Abr', size=0.68, ymod=0, xmod = 0.9, auto.placement = FALSE) + # Add the abbreviations to be moved slightly left (overlap)
  tm_shape(P_plot_labels_sr) + tm_dots(shape = 23, col= "red", size=0.15) + # Add a shape layer with point symbols
  tm_text('Abr', size=0.78, ymod=-0.4, xmod = 0.45, auto.placement = FALSE) + # Add the abbreviations to be moved slightly left (overlap)
  tm_shape(P_plot_labels_top) + tm_dots(shape = 23, col= "red", size=0.15) + # Add a shape layer with point symbols
  tm_text('Abr', size=0.68, ymod=0.46, xmod = 0, auto.placement = FALSE) + # Add the abbreviations to be moved slightly left (overlap)
  
  tm_legend(legend.outside=F) +
  tm_scale_bar(position = c(0.41, 0.065),bg.color = 'transparent', text.color = 1.7, color.dark = "grey20", lwd = 2, text.size = 0.8) +  # Add a scale bar on the left side
  tm_compass(position = c(0.04, 0.77), text.size = 0.9) +
  tm_graticules(lines = TRUE, labels.size = 0.8, labels.col = "black", labels.inside.frame = FALSE,
                alpha=0.3, n.y=5, n.x = 5, labels.margin.y=0.1,) +
  tm_add_legend(type = "symbol", labels = "  Weather station", shape = 23, size = 0.9, col = "red") +
  
  tm_layout(#main.title = "Locations of the weather stations",
    #main.title.position = "center",
    #main.title.size = 1.3,
    #main.title.color = "black",
    legend.show = TRUE,
    legend.title.size = 1.2,
    legend.text.size = 0.9,
    legend.text.color = 'black',
    legend.position = c(0.80, 0.28),
    legend.width = 1.2,
    legend.height = 1.5,
    #attr.color = 'white',
    bg.color = "white",
    outer.bg.color = "white",
    frame = FALSE)

Temperature_map
tmap_save(Temperature_map, filename = save_path, height = height, width = width, units = 'cm')  

#------------------------------------------------------------------------------------------#



