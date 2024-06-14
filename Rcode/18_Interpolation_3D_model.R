
#------------------------------------------------------------------------------------------#
########################### Interpolation of 3D model ###############################
#------------------------------------------------------------------------------------------#

# The main theme of this code is spatial interpolation using a 3D model to analyze 
# safe winter chill (SWC) patterns across Afghanistan The code performs various steps to 
# achieve this goal, which include:
  
  # Loading Required Libraries: The code starts by loading several libraries such as rgdal, tmap, 
# spatstat, maptools, raster, gstat, sp, fields, metR, colorRamps, sf, tidyverse, and cartography.

# Data Loading and Pre-processing:
  
  # It reads the shapefile representing the outline of Afghanistan.
# Reads station coordinates and projected chill data for future and historic scenarios.
# Renames columns to replace "X" with "scen_" in scenario years.
# Creates a polygon of Afghanistan with dashed patterns.
# Grid Creation:
  
  # Creates an empty grid for interpolation.
# Defines a regular grid with a specified number of cells within the extent of Afghanistan.
# Associates the grid points with the longitude and latitude coordinates.
# Variogram Estimation and Kriging:
  
  # Fits variogram models to the temperature data (minimum and maximum temperatures for December) to understand spatial correlation.
# Automatically fits variogram models using the automap package.
# Performs kriging to interpolate temperature values across the grid using the variogram models.
# Chill Correction:
  
  # Creates a 3D correction plane based on the krigged temperature values.
# Calculates the chill correction values for each grid cell based on the temperature correction plane.
# Applies the chill correction to the krigged chill values.
# Map Generation:
  
  # Generates maps for the adjusted safe winter chill values for each scenario.
# Uses the tmap package to create interactive and static maps with legend and relevant annotations.
# Saves the generated maps as image files in the specified directories.
# Change Analysis:
  
  # Calculates change in safe winter chill between different scenarios, particularly between 1980-2020 
# and various future scenarios.
# Generates maps to visualize changes in safe winter chill between scenarios.
# Output and Visualization:
  
  # The code creates and stores various plots and maps to visualize the safe winter chill patterns, 
# corrections, and changes over different scenarios.

# In summary, the code focuses on spatially interpolating safe winter chill values using a 3D model, 
# applying corrections, and visualizing the results through maps and plots. It demonstrates the 
# analysis of climate data to assess potential impacts on agricultural practices related to chill 
# requirements.


#most important script, spatial interpolation using the "3D model"

library(rgdal)
library(tmap)
library(spatstat) # Used for the dirichlet tessellation function
library(maptools) # Used for conversion from SPDF to ppp
library(raster) # Used to clip out thiessen polygons
library(gstat) # Use gstat's idw routine
library(sp) # Used for the spsample function
require(tmaptools)
library(gridExtra)
library(reshape2)#to melt a data frame
#stuff needed for the set up of the tmin, tmax plane
library(fields)
library(metR)
library(colorRamps)
library(sf)
library(tidyverse)
library(cartography) #needed to include structures like stripes to maps



# Load the outline for Afghanistan
sf_afghanistan <- raster::getData("GADM", country ="AF", level =1) %>% sf::st_as_sf() # Get Afghanistan shape file

# Convert the sf object to an sp object
sp_afghanistan <- as(sf_afghanistan, "Spatial")


#read station coordinates with the projected chill (future and historic)
All_chill <- read.csv('D:/Rdata/Chill_quantification/SWC/All_chill.csv')


# Replace the X before the scenario years for "scen_"
All_chill <- rename_with(All_chill, function (x) str_replace(x, "X", "scen_"), starts_with("X"))


# # Dealing with problems in the column names:
# # Specify the scenario column numbers
scenario_column_numbers <- c(7:12, 15:22)
# scenarions <- make.names(scenarions)
# 
# 
# # Get the corresponding scenario names from the list 
# scenario_names <- scenarios_fixed[scenarions]
# 
# # Apply the scenario names to the corresponding columns in All_chill_crs
# #colnames(All_chill_crs)[scenario_column_numbers] <- scenario_names
# 
# # Apply the scenario names to the corresponding columns in stations_clean
# colnames(stations_clean)[scenario_column_numbers] <- scenarions


#save scenario names to vector
scenarions <- colnames(All_chill)[c(7:12, 15:22)]
# Clean up scenario names in the scenarions vector
scenarions <- gsub("\\.", " ", scenarions)
scenarions <- gsub("_", "-", scenarions)
scenarions <- make.names(scenarions)
colnames(All_chill)[scenario_column_numbers] <- scenarions

All_chill_crs <- SpatialPointsDataFrame(All_chill[, c("Longitude", "Latitude")],
                                proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                data = dplyr::select(All_chill, -Station_ID, -Latitude, -Longitude, -Province))


# Replace point boundary extent of All_chill_crs with that of Afghanistan to make sure the 
# interpolation is done for the whole extend of Afghanistan

All_chill_crs@bbox <- sp_afghanistan@bbox

# Create an empty grid where n is the total number of cells (I will use n =5000, it was 50,000 for SA)
grd <- as.data.frame(spsample(sp_afghanistan, "regular", n=10000)) # I chose it as per area of Afghanistan
names(grd) <- c("Longitude", "Latitude")
coordinates(grd) <- c("Longitude", "Latitude")
gridded(grd) <- TRUE # Create SpatialPixel object
fullgrid(grd) <- TRUE # Create SpatialGrid object
proj4string(grd) <- proj4string(All_chill_crs)

# #save scenario names to vector
scenarios_fixed <- c(historic.SWC..CP. = "Historic observed", scen.1980.SWC..CP. = "Historic simulated (1980)",
                     scen.1990.SWC..CP. =  "Historic simulated (1990)", scen.2000.SWC..CP. = "Historic simulated (2000)",
                     scen.2010.SWC..CP. = "Historic simulated (2010)", scen.2020.SWC..CP. = "Historic simulated (2020)",
                     SSP126.2050.SWC..CP. = "SSP126 \u2013 2050",
                     SSP245.2050.SWC..CP. = "SSP245 \u2013 2050",
                     SSP370.2050.SWC..CP. = "SSP370 \u2013 2050",
                     SSP585.2050.SWC..CP. = "SSP585 \u2013 2050",
                     SSP126.2085.SWC..CP. = "SSP126 \u2013 2085",
                     SSP245.2085.SWC..CP. = "SSP245 \u2013 2085",
                     SSP370.2085.SWC..CP. = "SSP370 \u2013 2085",
                     SSP585.2085.SWC..CP. = "SSP585 \u2013 2085")

#load tmin and tmax map for December
#avg_temp_dec <- raster('D:/Rdata/Chill_quantification/Chill_mapping/temp_raster/30sec/crop_wc2.1_30s_tavg_12.tif')
min_temp_dec <- raster('D:/Rdata/Chill_quantification/Chill_mapping/temp_raster/30sec/crop_wc2.1_30s_tmin_12.tif')
max_temp_dec <- raster('D:/Rdata/Chill_quantification/Chill_mapping/temp_raster/30sec/crop_wc2.1_30s_tmax_12.tif')


#set extent to outline of Afghanistan
bb_afghanistan <- st_bbox(sf_afghanistan)

extent_afghanistan <- extent(bb_afghanistan["xmin"], bb_afghanistan["xmax"], bb_afghanistan["ymin"], bb_afghanistan["ymax"])

#set extent to outline of Afghanistan
bb <- extent(extent_afghanistan)

#extract Afghanistan from world wide map
min_temp_dec <- crop(min_temp_dec, bb)
max_temp_dec <- crop(max_temp_dec, bb)


#adjust resolution of temperature map to match the grid of our project
temp_min.res <- resample(min_temp_dec, raster(grd))
temp_max.res <- resample(max_temp_dec, raster(grd))

# produce interpolated layer from both temperature maps of all station locations
f.temp_min <- as.formula(min_temp_dec ~ Longitude + Latitude)
f.temp_max <- as.formula(max_temp_dec ~ Longitude + Latitude)


# #set up variogram
# var.smpl.temp_min <- variogram(f.temp_min, All_chill_crs, cutoff = 1400)
# var.smpl.temp_max <- variogram(f.temp_max, All_chill_crs)
# 
# dat.fit.temp_min <- fit.variogram(var.smpl.temp_min, fit.ranges = FALSE,
#                                   fit.sills = FALSE,
#                                   vgm(model = "Sph", range = 2600, psil = 12, nugget = 3))
# dat.fit.temp_max <- fit.variogram(var.smpl.temp_max, fit.ranges = FALSE,
#                                   fit.sills = FALSE,
#                                   vgm(model = "Sph", range = 2600, psil = 5, nugget = 1))
# 
# #check the variogram
# plot(var.smpl.temp_min, dat.fit.temp_min)
# plot(var.smpl.temp_max, dat.fit.temp_max)

####semi-variogram

# At this point I will try to use a different approach by using a function that fits automatically the
# variogram based on the data. This may be helpful since there will be no need to set the nugget, psill, and
# range manually
#fix.values = c(nugget, range: sill); NA = not fixed, decided to ignore values of distance > 1500, that is why range = 240
var_smpl_min_temp_dec <- automap::autofitVariogram(f.temp_min, All_chill_crs)
plot(var_smpl_min_temp_dec)

var_smpl_max_temp_dec <- automap::autofitVariogram(f.temp_max, All_chill_crs)
  # Here I removed the, fix.values = c(NA, 240, NA)), I do not understand what was this used for
plot(var_smpl_max_temp_dec)

#do the krigging
dat.krg.temp_min <- krige(f.temp_min, All_chill_crs, grd, var_smpl_min_temp_dec$var_model)
dat.krg.temp_max <- krige(f.temp_max, All_chill_crs, grd, var_smpl_max_temp_dec$var_model)

#transform the kriged surface to a raster
r_krig_min <- raster(dat.krg.temp_min)
#only use the rasters within the boundaries of Afghanistan
r.m_min <- mask(r_krig_min, sp_afghanistan)

# the same for max
r_krig_max <- raster(dat.krg.temp_max)
r.m_max <- mask(r_krig_max, sp_afghanistan)

#calculate a quality map, where I can see the percent difference of krigged temperature to original temperature
temp_diff_min <- r.m_min - temp_min.res
temp_diff_max <- r.m_max - temp_max.res

#remove outliers from the dataset to construct the correction model, outliers were already computed
stations_clean <- subset(All_chill, !(outlier_tmin_dec | outlier_tmax_dec))

#function which gets the chill correction for a tmin and tmax entry, work only for individual values, needs to be used in a loop / apply function
get_chill_correction <-  function(tmin, tmax, lookup = pred){
  if(is.na(tmin) == T){
    return(NA)
  } else if(is.na(tmax) == T){
    return(NA)
  } else{
    tmin_index <- which.min(abs(lookup$x - tmin))
    tmax_index <- which.min(abs(lookup$y - tmax))
    return(lookup$z[tmin_index, tmax_index])
  }
}

#create empty list which is used to store chill values
chill_list <- list()

# Create a plot list
plot_list <- list()

# List for Chile
Afghanistan_list <- list()

#set height and width (cm) of maps when maps are saved
height <- 12
width <- 11

# Create a directory to save the files from the re-analysis
# dir.create("D:/Rdata/Chill_quantification/3D mapping/Figures/re_analysis")
# dir.create("D:/Rdata/Chill_quantification/3D mapping/Figures/re_analysis/correction_model")
# dir.create("D:/Rdata/Chill_quantification/3D mapping/Figures/re_analysis/maps")

# Source a helper function to align the legend in 3D correction model plot
source("helper_function_align_legend_interpolation.R")

for(scen in scenarions){
  
  #krig the tmin tmax data on a plane
  k <- Krig(x = as.matrix(stations_clean[, c("min_temp_dec", "max_temp_dec")]),
            Y = stations_clean[scen])
  pred <- predictSurface(k)
  #error <- predictSurfaceSE(k)
  
  #adjust row and column name of object
  colnames(pred$z) <- pred$y
  rownames(pred$z) <- pred$x
  #colnames(error$z)<-error$y
  #rownames(error$z)<-error$x
  
  #melt df
  melted <- melt(pred$z)
  #melted_error <- melt(error$z)
  
  colnames(melted) <- c("min_temp_dec", "max_temp_dec", "value")
  #colnames(melted_error)<-c("min_temp_jul","max_temp_jul","value")
  
  #plot the grid
  correction_plane <- ggplot(melted, aes(x = min_temp_dec,
                                         y = max_temp_dec,
                                         z = value)) +
    geom_contour_fill(bins = 100) +
    scale_fill_gradientn(colours = alpha(matlab.like(15)),
                         name = paste("Safe Winter Chill\n(in CP)", sep = ''), trans = 'reverse') +
    geom_contour(col = "black")  +
    geom_point(data = All_chill, aes(x = min_temp_dec,
                                    y = max_temp_dec,
                                    z = NULL),
               size = 0.7) +
    geom_text_contour(stroke = 0.2, size = 2) +
    ylab('Monthly maximum temperature in December (°C)') +
    xlab('Monthly minimum temperature in December (°C)') +
    theme_bw(base_size = 12) +
    theme(legend.title.align = 0.5,
          legend.position = c(0.875, 0.3),
          legend.background = element_blank())
  
  correction_plane <- cowplot::ggdraw(align_legend(correction_plane)) 
  
  ggsave(plot = correction_plane,
         filename = paste('D:/Rdata/Chill_quantification/3D mapping/Figures/re_analysis/correction_model/correction_plane_', scen, '.jpg', sep = ''),
         height = 10, width = 15, units = 'cm')
  
  #save number of rows and cols
  no_row <- nrow(r.m_min)
  no_col <- ncol(r.m_min)
  
  #transform kriged tmin and tmax to matrix
  mat_krig_tmin <- matrix(r.m_min, nrow = no_row, ncol = no_col,byrow = T)
  mat_krig_tmax <- matrix(r.m_max, nrow = no_row, ncol = no_col,byrow = T)
  mat_real_tmin <- matrix(temp_min.res, nrow = no_row, ncol = no_col,byrow = T)
  mat_real_tmax <- matrix(temp_max.res, nrow = no_row, ncol = no_col,byrow = T)
  
  #transform matrix to vector and bind tmin and tmax
  t_both <- cbind(as.vector(mat_krig_tmin), as.vector(mat_krig_tmax))
  t_both_real <- cbind(as.vector(mat_real_tmin), as.vector(mat_real_tmax))
  
  #to see how many pixels are outside of the correction range
  #test <- t_both_real[!is.na(t_both_real[,1]),]
  #out_test <- sapply(1:nrow(test), function(i) get_chill_correction(test[i,1], test[i,2]))
  #sum(is.na(out_test)) / length(out_test)
  
  
  #extract the model chill for real and kriged tmin and tmax
  model_krigged_temp <- sapply(1 : nrow(t_both), function(i) get_chill_correction(t_both[i, 1], t_both[i, 2]))
  model_real_temp <- sapply(1:nrow(t_both_real), function(i) get_chill_correction(t_both_real[i, 1], t_both_real[i, 2]))
  
  test_corr_df <- data.frame('min_temp_dec' = as.vector(mat_real_tmin),'max_temp_dec' = as.vector(mat_real_tmax))

  #see where the datapoints are in the correction plane
  hatched_areas <- ggplot(melted,
        aes(x = min_temp_dec, y = max_temp_dec, z = value)) +
    geom_point(data = test_corr_df,
               aes(x = min_temp_dec, y = max_temp_dec, z = NULL),
               size=0.7, alpha = 0.5) +
    geom_contour_fill(alpha = 0.5, color = "grey40") +
    geom_text_contour(stroke = 0.2) +
    scale_fill_gradientn(colours = alpha(matlab.like(15)),
                         name = "Safe Winter Chill\n          (CP)", trans = 'reverse') +
    labs(y = "Maximum temperature in December (°C)",
         x = "Minimum temperature in December (°C)") +
    theme_bw() +
    theme(legend.position = c(0.85, 0.25),
          legend.background = element_blank())

  hatched_areas <- cowplot::ggdraw(align_legend(hatched_areas))

  ggsave(plot = hatched_areas,
         filename = paste('D:/Rdata/Chill_quantification/3D mapping/Figures/final_figures/hatched_areas_', scen, '.jpg', sep = ''),
         height = 10, width = 15, units = 'cm', dpi = 600)
  #ggsave("D:/Rdata/Chill_quantification/3D mapping/Figures/final_figures/figure_S2.png", width = 12, height = 10, units = "cm", dpi = 600)

  #--> many points outside the range of the correction plane
  
  #calculate the adjustment (so the chill, which so far was not captured by krigging)
  #problem: model_real_temp contains many NA
  model_adjust <- model_real_temp - model_krigged_temp
  
  model_adjust <- matrix(model_adjust, nrow = no_row, ncol = no_col)
  #model_orig <- matrix(model_real_temp,nrow = no_row, ncol = no_col)
  #model_krig <- matrix(model_krigged_temp,nrow = no_row, ncol = no_col)
  #exlcude adjustments which would decrease the chill
  
  #how can I transform the matrix back to a grid?
  raster_model_adjust <- raster(model_adjust)
  raster_model_adjust <- setExtent(raster_model_adjust, bb)
  crs(raster_model_adjust) <- crs(r.m_min)
  
  #raster_model_orig <- raster(model_orig)
  #raster_model_orig <- setExtent(raster_model_orig,bb)
  #crs(raster_model_orig) <- crs(r.m_min)
  
  #raster_model_krig <- raster(model_krig)
  #raster_model_krig <- setExtent(raster_model_krig,bb)
  #crs(raster_model_krig) <- crs(r.m_min)
  
  
  #do interpolation of chill
  # Define the krigging model for the chill
  f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))
  
  # # Compute the sample variogram; note that the f.1 trend model is one of the
  # # parameters passed to variogram(). This tells the function to create the
  # # variogram on the de-trended data.
  # var.smpl <- variogram(f.1, All_chill_crs, cloud = FALSE)
  # 
  # # Compute the variogram model by passing the nugget, sill and range values
  # # to fit.variogram() via the vgm() function.
  # dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
  #                          vgm(psill = 275, model = "Sph", nugget = 20, range = 1500))
  # 
  # #check the variogram  
  # #plot(var.smpl,dat.fit)
  # 
  # # Perform the krige interpolation (note the use of the variogram model
  # # created in the earlier step)
  # dat.krg.chil <- krige(f.1, All_chill_crs, grd, dat.fit)
  
  var.smpl <- automap::autofitVariogram(f.1, All_chill_crs)
  
  plot(var.smpl)
  
  #do the krigging
  dat.krg.chil <- krige(f.1, All_chill_crs, grd, var.smpl$var_model)
  
  
  #assign krigged data to the raster
  r_krig <- raster(dat.krg.chil)
  r.m.chill <- mask(r_krig, sp_afghanistan)
  r.m.chill <- max(r.m.chill, 0)
  
  raster_model_adjust <- setExtent(raster_model_adjust, extent(r.m.chill)) 
  #raster_model_krig <- setExtent(raster_model_krig, extent(r.m.chill))
  #raster_model_orig <-  setExtent(raster_model_orig, extent(r.m.chill))
  
  
  #adjust the chill portions, prevent that chill portions become lower than zero
  r <- max(r.m.chill + raster_model_adjust, 0)
  r.m <- mask(r, sp_afghanistan)
  
  chill_list <- append(chill_list, r.m)
  
  

  
  f_name <- paste('D:/Rdata/Chill_quantification/3D mapping/Figures/re_analysis/maps/adjusted_chill_', scen, '.png', sep = '')
  
  chill_map <- tm_shape(sp_afghanistan) +
    tm_fill(col = 'grey10') +
    #tm_shape(SA_test) + I do not have this object since I do not have a dashed area
    #tm_lines(col = 'grey35') +
    tm_shape(r.m) +
    tm_raster(palette = get_brewer_pal('RdYlBu', n = 20),
              midpoint = 30,
              title = 'Safe Winter Chill',
              style = 'cont', legend.reverse = TRUE, breaks = seq(0, 100, by = 20),
              legend.format = list(suffix = " CP", text.align = "center")) +
    tm_shape(All_chill_crs) +
    tm_symbols(size = 0.075, shape = 4, col = 'firebrick', alpha = 0.8) + 
    tm_shape(sp_afghanistan) +
    tm_borders(col = 'grey40') +
    tm_graticules(lines = F, labels.size = 0.6, labels.col = "black") +
    tm_compass(position = c(0.03, 0.80), text.size = 0.6) +
    tm_scale_bar(position = c(0.40, -0.005), bg.color = 'transparent', text.size = 0.6, color.dark = "grey20") +
    #tm_add_legend(type = "line", labels = "Excluded", col = "grey35", lwd = 3) +
    tm_add_legend(type = "symbol", labels = "  Weather station", shape = 4, size = 0.5, col = "firebrick") +
    tm_layout(main.title = paste0("      ", scenarios_fixed[[scen]]),
              main.title.position = "center",
              main.title.size = 1.3,
              main.title.color = "black",
              legend.outside = F,
              legend.title.size = 0.85,
              legend.text.size = 0.65,
              legend.position = c(0.73, 0.08),
              outer.margins = c(0.001, 0.001, 0.001, 0.001),
              bg.color = "black",
              attr.color = "white",
              outer.bg.color = "white")
  
  chill_map
  
  plot_list <- append(plot_list, list(chill_map))
  
  tmap_save(chill_map, filename = f_name, height = height, width = width, units = 'cm')  
  
  
  
  new_seq <- seq(-50, 90, by = 10)
  
  f_name <- paste('D:/Rdata/Chill_quantification/3D mapping/Figures/re_analysis/maps/chill_correction_', scen, '.png', sep = '')
  
  chill_correction <- tm_shape(raster_model_adjust) +
    #tm_lines(col = "grey") +
    #tm_shape(raster_model_adjust) +
    tm_raster(palette = get_brewer_pal("RdBu", contrast = c(0, 0.75)),
              midpoint = 0,
              title = "Safe Winter Chill\n correction                 (CP)",
              breaks = new_seq, style = "cont", legend.reverse = TRUE) +
    tm_shape(All_chill_crs) +
    tm_symbols(size = 0.2, shape = 4, col = 'black') +
    tm_shape(sp_afghanistan) +
    tm_borders(col = 'grey40') +
    tm_graticules(lines = F, labels.size = 0.6) +
    tm_compass(position = c(0.03,0.80), text.size = 0.6) +
    tm_scale_bar(position = c(0.40, -0.005), bg.color = 'transparent', text.size = 0.5) +
    tm_layout(legend.outside = F,
              legend.text.size = 0.45,
              legend.title.size = 0.60,
              legend.position = c(0.82, 0.03),
              #legend.height = 0.3,
              outer.margins = c(0.001, 0.001, 0.001, 0.001))
  
  chill_correction
  
  tmap_save(chill_correction, filename = f_name, height = height, width = width, units = 'cm')  
  
} 


#########################
###compute change chill map
#########################

#change names in list to scenario names
names(chill_list) <- scenarions
names(plot_list) <- scenarions
names(chile_list) <- scenarions

# Generate a baseline raster scenario based on the median across historic simulated scenarios
brick_raster <- brick(chill_list[2 : 11])

# Estimate the median across raster layers
median_raster_scen <- calc(brick_raster, median)

# Create a list to save the plots
change_maps <- list()

# Create a directory to save the plots
dir.create("figures/re_analysis/change")

#loop for change 2017 to future scenarios
for(scen in scenarions[12 : 23]){
  #create file name
  f_name <- paste('figures/re_analysis/change/change_hist_sim_vs_', scen, '.png', sep = '')
  
  change_map <- tm_shape(SA) +
    tm_fill(col = 'grey10') +
    tm_shape(SA_test) +
    tm_lines(col = 'grey35') +
    tm_shape(chill_list[[scen]] - median_raster_scen) +
    tm_raster(palette = get_brewer_pal('RdYlBu', n = 10),
              midpoint = 0,
              title = 'SWC relative to\n 1981 \u2013 2017',
              style = 'cont', legend.reverse = TRUE, breaks = seq(-40, 10, length.out = 6),
              legend.format = list(suffix = " CP", text.align = "center")) +
    tm_shape(Porig) +
    tm_symbols(size = 0.075, shape = 4, col = 'firebrick', alpha = 0.8) + 
    tm_shape(SA) +
    tm_borders(col = 'grey40') +
    tm_graticules(lines = F, labels.size = 0.6, labels.col = "black") +
    tm_compass(position = c(0.66, 0.85), text.size = 0.6) +
    tm_scale_bar(position = c(0.57, 0.925), bg.color = 'transparent', text.size = 0.6, color.dark = "grey20") +
    tm_add_legend(type = "line", labels = "Excluded", col = "grey35", lwd = 3) +
    tm_add_legend(type = "symbol", labels = "  Weather station", shape = 4, size = 0.5, col = "firebrick") +
    tm_layout(main.title = paste0("      ", scenarios_fixed[[scen]]),
              main.title.position = "center",
              main.title.size = 1.3,
              main.title.color = "black",
              legend.outside = F,
              legend.title.size = 0.85,
              legend.text.size = 0.64,
              legend.position = c(0.67, 0.005),
              outer.margins = c(0.001, 0.001, 0.001, 0.001),
              bg.color = "black",
              attr.color = "white",
              outer.bg.color = "white")
  
  change_map
  
  #tmap_save(change_map, filename = f_name, height = height, width = width+1, units = 'cm')  
  
  # Save the maps
  change_maps <- append(change_maps, list(change_map))
  
}

# name the change_maps list
names(change_maps) <- scenarions[12 : 23]



#calculate change 1981 to 2017 (2017 minus 1981)
scen <- scenarions[2]
f_name <- paste('figures/re_analysis/change/change_2017_', scen, '.png', sep = '')

change_map <- tm_shape(SA) +
  tm_fill(col = 'grey10') +
  tm_shape(SA_test) +
  tm_lines(col = 'grey35') +
  tm_shape(chill_list[["scen_2017"]] - chill_list[[scen]]) +
  tm_raster(palette = get_brewer_pal('RdYlBu', n = 20),
            midpoint = 0,
            title = 'Safe Winter Chill',
            style = 'cont', legend.reverse = TRUE, breaks = seq(-15, 10, length.out = 6),
            legend.format = list(suffix = " CP", text.align = "center")) +
  tm_shape(Porig) +
  tm_symbols(size = 0.075, shape = 4, col = 'firebrick', alpha = 0.8) + 
  tm_shape(SA) +
  tm_borders(col = 'grey40') +
  tm_graticules(lines = F, labels.size = 0.6, labels.col = "black") +
  tm_compass(position = c(0.66, 0.85), text.size = 0.6) +
  tm_scale_bar(position = c(0.57, 0.925), bg.color = 'transparent', text.size = 0.6, color.dark = "grey20") +
  tm_add_legend(type = "line", labels = "Excluded", col = "grey35", lwd = 3) +
  tm_add_legend(type = "symbol", labels = "  Weather station", shape = 4, size = 0.5, col = "firebrick") +
  tm_layout(main.title = "      Chill change 1981 \u2013 2017",
            main.title.position = "center",
            main.title.size = 1.3,
            main.title.color = "black",
            legend.outside = F,
            legend.title.size = 0.85,
            legend.text.size = 0.64,
            legend.position = c(0.65, 0.005),
            outer.margins = c(0.001, 0.001, 0.001, 0.001),
            bg.color = "black",
            attr.color = "white",
            outer.bg.color = "white")

change_map

#tmap_save(change_map, filename = f_name, height = height, width = width, units = 'cm')  



############
#create table with absolute and change in chill
############

#create data frame with frequencies of absolute CP
#create data frame with frequencies of change in chill

#empty data frame
absolute_chill_df <- data.frame(NULL)

#loop other every entry of the list
for(i in 1 : length(chill_list)){
  #subset data frame and remove NAs
  sub_df <- na.omit(as.data.frame(chill_list[[i]])) 
  #create bins of chill
  out <- table(cut(sub_df$layer, breaks = seq(0, 120, by = 15), include.lowest = T))
  #save binned chill to data frame
  absolute_chill_df <- rbind(absolute_chill_df, out)
  
}
#add names to the dataframe 
absolute_chill_df <- cbind(names(chill_list), absolute_chill_df)
names(absolute_chill_df) <- c('year', names(out))      
#save the table
write.csv(absolute_chill_df, 'data/absolute_chill_binned.csv',
          row.names = F)   

#create the same for change in chill
change_chill_list <- list()

for(i in 12 : length(chill_list)){
  change <- chill_list[[i]] - median_raster_scen
  change_chill_list <- append(change_chill_list, change)
}

change <- chill_list[["scen_2017"]] - chill_list[["scen_1981"]]
change_chill_list <- append(change_chill_list, change)

names(change_chill_list) <- c(names(chill_list[12 : length(chill_list)]), 'scen_2017_vs_scen_1981')

change_chill_df <- data.frame(NULL)

for(i in 1 : length(change_chill_list)){
  sub_df <- na.omit(as.data.frame(change_chill_list[[i]])) 
  out <- table(cut(sub_df$layer, breaks = seq(-60, 20, by = 10)))
  change_chill_df <- rbind(change_chill_df, out)
  
}

change_chill_df <- cbind(names(change_chill_list), change_chill_df)
names(change_chill_df) <- c('year', names(out))      

write.csv(change_chill_df, 'data/change_chill_binned.csv',
          row.names = F)
