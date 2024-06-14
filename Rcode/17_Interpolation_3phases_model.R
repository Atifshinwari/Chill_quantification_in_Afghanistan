
#------------------------------------------------------------------------------------------#
########################### Interpolation of 3-phases model ###############################
#------------------------------------------------------------------------------------------#
# The main theme of the provided code is spatial analysis and interpolation in the context of 
# temperature and chill factors across Afghanistan. Specifically, the code carries out several 
# key tasks:
  
# Data Loading: Reads geographic data related to Afghanistan's outline and station coordinates 
# containing projected chill (future and historic) information.

# Pre-processing: Sets up various spatial objects, including a grid that will be used for interpolation
# , and re-samples temperature data to match the grid resolution.

# Temperature Analysis: Performs Ordinary Kriging interpolation on the mean temperature for December, 
# creates plots of original, Krigged, and error of temperature, and saves them to files.

# Three-Phase Model Analysis: It applies a hand-defined three-phase model to analyze chill against 
# mean temperature. It creates subsets of the data based on temperature breakpoints and creates linear models for each phase.

# Chill Interpolation and Correction: Uses the defined models to calculate modeled chill and compute 
# adjustment for correction. It seems like a part of the interpolation of chill, including kriging, 
# is also planned but not fully implemented in the provided code.

# Visualization: Throughout the code, various maps and plots are created to visualize the original 
# temperature, the Krigged temperature, error, correction of winter chill, and more.

# Function Definition: Includes a specific function (calc_model_val) that calculates modeled chill 
# values based on input temperature using the defined three-phase models.

# In summary, the code is aimed at spatial analysis, specifically focusing on temperature and chill 
# factors in Afghanistan. It involves spatial interpolation (including Kriging), modeling, 
# and extensive visualization.

# Origininal code starts here: 

#does spatial interpolation with "three-phases model"
#maps were not used in the final report and map appearances were not updated

library(rgdal)
library(tmap)
library(spatstat) 
library(maptools) 
library(raster) 
library(gstat) 
library(sp) 
require(tmaptools)
library(ggplot2)
library(gridExtra)
library(sf)


# Load the outline for Afghanistan
sf_afghanistan <- raster::getData("GADM", country ="AF", level =1) %>% sf::st_as_sf() # Get Afghanistan shape file

# Convert the sf object to an sp object
sp_afghanistan <- as(sf_afghanistan, "Spatial")

#read station coordinates with the projected chill (future and historic)
All_chill <- read.csv('D:/Rdata/Chill_quantification/SWC/All_chill.csv')

# my station file is All_chill, lets change it to CRS
All_chill_crs <- SpatialPointsDataFrame(All_chill[,c("Longitude", "Latitude")],
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                        data = All_chill[,c(1:49)])


# Replace point boundary extent with that of Afghanistan to make sure the interpolation is done for the whole extend of Afghanistan
All_chill_crs@bbox <- sp_afghanistan@bbox

# Create an empty grid where n is the total number of cells (I will use n =2000, it was 50,000 for SA)
grd <- as.data.frame(spsample(sp_afghanistan, "regular", n=5000))
names(grd) <- c("Longitude", "Latitude")
coordinates(grd) <- c("Longitude", "Latitude")
gridded(grd) <- TRUE # Create SpatialPixel object
fullgrid(grd) <- TRUE # Create SpatialGrid object
proj4string(grd) <- proj4string(All_chill_crs)

#save scenario names to vector
scenarions <- colnames(All_chill)[c(7:12, 15:22)]

#chose which data is used for the interpolation (this is where later on the loop is set)
# I will chose scen 2 which is SWC for 1980
scen <- scenarions[1]


###### ordinary krigging with mean temp in december 

#read original mean temp in december file
#same file like in script 10

avg_temp_dec <- raster('D:/Rdata/Chill_quantification/Chill_mapping/temp_raster/30sec/crop_wc2.1_30s_tavg_12.tif')

# Assuming 'sf_afghanistan' is the spatial object containing Afghanistan's shape
#library(sf)
bb_afghanistan <- st_bbox(sf_afghanistan)

extent_afghanistan <- extent(bb_afghanistan["xmin"], bb_afghanistan["xmax"], bb_afghanistan["ymin"], bb_afghanistan["ymax"])

#set extent to outline of Afghanistan
bb <- extent(extent_afghanistan)

#extract Afghanistan from world wide map
avg_temp_dec <- crop(avg_temp_dec, bb)

#adjust resolution of temperature map to match the grid of our project
temp.res<-resample(avg_temp_dec,raster(grd))

# Now lets mask it only for Afghanistan
temp.res <- mask(temp.res, sp_afghanistan)

## produce interpolated layer from avg temp December of all station locations
f.temp<-as.formula(avg_temp_dec ~ Longitude + Latitude)

###here is a big problem: how should we fit the elevation data? 
var.smpl.temp <- variogram(f.temp, All_chill_crs)
# dat.fit.temp <- fit.variogram(var.smpl.temp, fit.ranges = FALSE,
#                               fit.sills = FALSE,
#                               vgm(model="Sph", range = 2600, psil = 5, nugget = 2))

# Fit a model (in this case, a spherical model) to the empirical variogram
# This function will try to find optimal parameters for the range and sill
dat.fit.temp <- fit.variogram(var.smpl.temp, model=vgm("Sph"))

#check the variogram
plot(var.smpl.temp, dat.fit.temp)

#do the krigging
dat.krg.temp <- krige( f.temp, All_chill_crs, grd, dat.fit.temp)
tm_shape(dat.krg.temp)+
tm_raster()

# Now lets save it as raster and mask it only for Afghanistan
r_krig<-raster(dat.krg.temp)
r.m <- mask(r_krig, sp_afghanistan)

#calculate a quality map, where I can see the percent difference of krigged temperature to 
# original temperature
temp_diff <- r.m - temp.res
# temp_qual[temp_qual > 100] <- NA
# temp_qual[temp_qual < -100] <- -NA

#check distribution of quality map
hist(temp_diff) 
summary(temp_diff)

data(World)#for map of Afghanistan

#size of figures which are saved
height <- 13
width <- 12

#plot original temperature
#jpeg(file='original_temp_august.jpg', width = 700, height = 500)

org_temp <- tm_shape(sp_afghanistan) +
  tm_fill()+
  tm_shape(temp.res)+
  tm_raster(n = 7, midpoint = 10, palette = get_brewer_pal("-RdBu", n = 9, contrast = c(0, 0.75)),
            style = "cont", breaks = seq(-10, 20, by = 5),
            title = "Original Mean Temperature\n in December (°C)") +
  tm_shape(All_chill_crs) + tm_symbols(size=0.2,shape = 4,col = 'black')+
  tm_shape(sp_afghanistan)+
  tm_borders(col='black')+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.03,0.75), lwd = 0.2)+
  tm_scale_bar(position = c(0.58,0.03),lwd = 0.01, bg.color = 'white')+
  tm_layout(legend.outside= T,outer.margins = c(0.001,0.001,0,0.001))
org_temp
tmap_save(org_temp, filename = 'D:/Rdata/Chill_quantification/3D Mapping/3_phases_model/3phases_interpolation/original_tavg_dec.jpg',
          height = height, width = width, units = 'cm')  


#plot krigged temperature
krig_temp <- tm_shape(sp_afghanistan) +
  tm_fill()+  
  tm_shape(r.m)+
  tm_raster(midpoint = 10,palette=get_brewer_pal("-RdBu", n = 9, contrast = c(0, 0.75)),
            breaks = seq(-10,20,by=5),
            title="Krigged Mean Temperature\nin December (°C)",
            style = "cont")+
  tm_shape(All_chill_crs) + tm_symbols(size=0.2,shape = 4,col = 'black')+
  tm_shape(sp_afghanistan)+
  tm_borders(col='black')+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.03,0.75))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
krig_temp
tmap_save(krig_temp, filename = 'D:/Rdata/Chill_quantification/3D Mapping/3_phases_model/3phases_interpolation/krig_tavg_dec.jpg',
          height = height, width = width, units = 'cm')  

#tmaptools::palette_explorer()

#plot percent difference of krigged to original temperature
#jpeg(file='error_krigging_temp_december.jpg', width = 700, height = 500)
temp_dif <-tm_shape(sp_afghanistan) +
  tm_fill()+
  tm_shape(temp_diff)+
  tm_raster(palette=get_brewer_pal("-RdBu", n = 9, contrast = c(0, 0.75)),
            midpoint = 0,title="Error\nkrig.-orig.(°C)",
            breaks = c(seq(-10,14,by=4),20), style = "cont")+
  tm_shape(All_chill_crs) + tm_symbols(size=0.2,shape = 4,col = 'black')+
  tm_shape(sp_afghanistan)+
  tm_borders(col='black')+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.03,0.75))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
temp_dif
tmap_save(temp_dif, filename = 'D:/Rdata/Chill_quantification/3D Mapping/3_phases_model/3phases_interpolation/difference_tavg_dec.jpg',height = height,width=width,units = 'cm')  



#plot chill against the mean temperature in Dec
ggplot(All_chill,aes(x = avg_temp_dec, y = historic_SWC..CP.)) +
  ylab('Chill Portion')+
  coord_cartesian(ylim = c(0,100))+
  geom_point()+
  geom_vline(xintercept=c(0,7))+
  theme_bw()

#hand-defined breaks for three phase model
upper_break = 7
lower_break = 0

#subset of original station data according to breaks
low <- subset(All_chill, All_chill$avg_temp_dec <= lower_break)
mid <- subset(All_chill, All_chill$avg_temp_dec > lower_break & All_chill$avg_temp_dec <upper_break)
up <- subset(All_chill, All_chill$avg_temp_dec >= upper_break)

#create model for the subset of chill explained by mean temperature in December (median SWC)
model_low <- lm(data = low, historic_SWC..CP. ~ avg_temp_dec)
model_mid <- lm(data = mid, historic_SWC..CP. ~ avg_temp_dec)
model_up <- lm(data = up, historic_SWC..CP. ~ avg_temp_dec)


# create model for the subset of chill explained by mean temperature in December (SWC 1980)
# model_low <- lm(data = low, X1980_SWC..CP. ~ avg_temp_dec)
# model_mid <- lm(data = mid, X1980_SWC..CP. ~ avg_temp_dec)
# model_up <- lm(data = up, X1980_SWC..CP. ~ avg_temp_dec)


#set up function to automatically calculate the modeled chill by mean temperature in December
#function is defined for individual cell value, so it needs to be used in loop or in apply function
#input: set breaks, the models and the temperature map as a grid
#output: modeled chill as a matrix!

calc_model_val <- function(x, upper_break, lower_break, model_low, model_mid){
  #at first, check if the raster value is nan or na, if so return the same
  if(is.na(x) == T){
    return(NA)
  } else if (is.nan(x) == T){
    return(NaN)
  }
  #now check to which of the three phases the value belongs to and calculate modeled chill
  if(x <= lower_break){
    return(x * model_low$coefficients[2] + model_low$coefficients[1])
  } else if(x >= upper_break){
    return(0)
  } else {
    out <- (x * model_mid$coefficients[2] + model_mid$coefficients[1])
    if(out < 0){
      return(0)
    } else {
      return(out)
    }
  }
}

#test function for single value
calc_model_val(x=NaN,upper_break = upper_break, lower_break = lower_break, 
               model_low = model_low, model_mid = model_mid)

#test function on a matrix
my.matrx <- matrix(c(1:5, 6:10, 11:15, 16:20), nrow = 5, ncol = 4)
my_res <- apply(my.matrx, 1 : 2, calc_model_val, upper_break, lower_break, model_low, model_mid)

#save number of rows and columns, so that the gridded data is presented correctly as a matrix
no_row <- nrow(r.m)
no_col <- ncol(r.m)

#test function on real matrix
#krigged temp
model_krigged_temp <- apply(matrix(r.m, nrow = no_row, ncol = no_col,byrow = T),1:2,calc_model_val, upper_break, lower_break, model_low, model_mid)
#real temp
model_real_temp <- apply(matrix(temp.res,nrow = no_row, ncol = no_col, byrow = T),1:2, calc_model_val, upper_break, lower_break, model_low, model_mid)

#calculate the adjustment (so the chill, which so far was not captured by krigging)
model_adjust <- model_real_temp - model_krigged_temp
#exlcude adjustments which would decrease the chill

#how can I transform the matrix back to a grid?
raster_model_adjust <- raster(model_adjust)
raster_model_adjust <- setExtent(raster_model_adjust,bb)
crs(raster_model_adjust) <- crs(r.m)

tm_shape(raster_model_adjust)+
  tm_raster()+
  tm_shape(sp_afghanistan)+
  tm_borders()

#something is wrong here. the adjustment layer looks completely distorted
chill_adjust <- tm_shape(sp_afghanistan)+
  tm_fill()+
  tm_shape(raster_model_adjust)+
  tm_raster(n=10, palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
            midpoint = 0,title="Correction of\n winter chill (Chill Portions)",
            style="cont")+
  tm_shape(All_chill_crs) + tm_dots(size=0.2)+
  tm_legend(legend.outside=TRUE)
chill_adjust
tmap_save(chill_adjust, filename = 'D:/Rdata/Chill_quantification/3D Mapping/3_phases_model/3phases_interpolation/chill_adjustment.jpg',height = height,width=width,units = 'cm')  



#do interpolation of chill
# Define the krigging model for the chill
f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the
# variogram on the de-trended data.
var.smpl <- variogram(f.1, All_chill_crs, cloud = FALSE)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
# dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
#                          vgm(psill = 275, model="Sph", nugget = 20, range = 1500))

# Here i used this code in place of the above commented out version
dat.fit <- fit.variogram(var.smpl, model=vgm("Sph"))

#check the variogram  
plot(var.smpl,dat.fit)

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg.chil <- krige(f.1, All_chill_crs, grd, dat.fit)

#assign krigged data to the raster
r_krig<-raster(dat.krg.chil)
r.m.chill <- mask(r_krig, sp_afghanistan)
r.m.chill<-max(r.m.chill,0)

raster_model_adjust <- setExtent(raster_model_adjust, extent(r.m.chill)) 

#adjust the chill portions, prevent that chill portions become lower than zero
r<-max(r.m.chill+raster_model_adjust,0)
r.m <- mask(r, sp_afghanistan)


final_chill <- tm_shape(r.m)+
  tm_raster(n=10, palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
            midpoint = 0,title="Safe Winter Chill \n(Chill Portions) \nCorrected for Temp.",
            style="cont")+
  tm_shape(All_chill_crs) + tm_dots(size=0.2)+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.03,0.75))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))

tmap_save(final_chill, filename = paste0('D:/Rdata/Chill_quantification/3D Mapping/3_phases_model/3phases_interpolation/adjusted_chill_three_phases_', scen, '.jpg'),
          height = height,width=width,units = 'cm')  



# Plot the final map
final_chill

#have a look at the region with high density of stations
# b <- bbox(avg_temp_dec)
# b[1,] <- c(-75,-65)
# b[2,] <- c(-40,-25)
# b <- bbox(t(b))
# 
# tm_shape(r.m,bbox = b)+
#   tm_raster(n=10, palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
#             midpoint = 50,title="Safe Winter Chill \n(Chill Portions) \nCorrected for Temp.",
#             style="cont")+
#   tm_shape(Porig) + tm_dots(size=0.2)+
#   tm_grid()+
#   tm_legend(legend.outside=TRUE)



orig_krig_chill <- tm_shape(r.m.chill)+
  tm_raster(palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
            midpoint = 30,title=paste(scen,"\nSafe Winter Chill \n(Chill Portions)\nUncorrected",sep = ''),
            breaks=seq(0,100,by=10), style = "cont")+
  tm_shape(All_chill_crs) + tm_dots(size=0.2,shape = 4,col = 'black')+
  #tm_text("Station_ID", size=0.5, col="black") + 
  tm_shape(sp_afghanistan)+
  tm_borders()+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.03,0.75))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
orig_krig_chill

tmap_save(orig_krig_chill,
          filename = paste0('D:/Rdata/Chill_quantification/3D Mapping/3_phases_model/3phases_interpolation/chill_kriged_uncorrected_', scen, '.jpg'),
          height = height,width=width,units = 'cm')  


