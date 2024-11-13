# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
#setwd("G:/Shared drives/_Org OlffLab/Teaching/APCE/APCE2024/APCE2024GIS")
#to do this, go to verkenner/explorer and copy the directory. Change slashes to forward slashes.
setwd("C:/Users/franc/Documents/Master/APCE2024/QGIS/apce2024gis")

# restore the libraries of the project 
renv::restore()


# load the different libraries
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot

# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,10), col = grey.colors(10))

mycolors<-c("red", "white", "blue")
mycolors

barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale around, so you can change the order of the colors.
barplot(rep(1,10), col = rev(terrain.colors(10)))#topo colors and terrain colors are build in to use. 
library(RColorBrewer) 
RColorBrewer::display.brewer.all()#colour pallettes that you can use.
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))

barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis) #another package with colour pallettes
barplot(rep(1,10), col = rev(viridis::viridis(10)))
barplot(rep(1,10), col = viridis::plasma(10))
barplot(rep(1,10), col = viridis::heat(10))
viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data for the whole ecosystem
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")#. is to use working directory that you set earlier.
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
            layer="protected_areas_2022") # read protected area boundaries)

sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/studyarea.gpkg",
                              layer="my_study_area")


# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)#spat is best option
plot(protected_areas)
plot(rivers)
plot(lakes)
plot(elevation,add=T)

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

# plot the woody biomass map that you want to predict
woody_map<-ggplot()+
  tidyterra::geom_spatraster(data=woodybiom)+
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77,6.55),
                       oob=squish,#means that values outside the limits are set to the colour of the limits.
                       name="TBA/ha")+
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.7,colour="green")+
#add study area, rivers and lakes. STudy area in red,not filled. lake=light blue. rivers=blue.
tidyterra::geom_spatvector(data=studyarea,
                           fill=NA,colour="red",linewidth=1)+
tidyterra::geom_spatvector(data=rivers,colour="blue")+
  tidyterra::geom_spatvector(data=lakes,fill="lightblue")+
  labs(title="Woody biomass")+
  coord_sf(xlimits,ylimits,datum=sf::st_crs(32736))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

woody_map

# plot the rainfall map##########################################################################
rainfall_map<-ggplot()+
  tidyterra::geom_spatraster(data=rainfall)+
  scale_fill_gradientn(colours=rev(pal_zissou1),
                       limits=c(625,1375),
                       oob=squish,#means that values outside the limits are set to the colour of the limits.
                       name="mm/year")+
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.7,colour="green")+
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,colour="red",linewidth=1)+
  tidyterra::geom_spatvector(data=rivers,colour="blue")+
  tidyterra::geom_spatvector(data=lakes,fill="lightblue")+
  labs(title="Annual rainfall")+
  coord_sf(xlimits,ylimits,datum=sf::st_crs(32736))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

rainfall_map

# plot the elevation map##########################################################################
elevation_map<-ggplot()+
  tidyterra::geom_spatraster(data=elevation)+
  scale_fill_gradientn(colours=terrain.colors(10),
                       limits=c(500,2000),
                       oob=squish,#means that values outside the limits are set to the colour of the limits.
                       name="meters")+
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.7,colour="green")+
  #add study area, rivers and lakes. STudy area in red,not filled. lake=light blue. rivers=blue.
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,colour="red",linewidth=1)+
  tidyterra::geom_spatvector(data=rivers,colour="blue")+
  tidyterra::geom_spatvector(data=lakes,fill="lightblue")+
  labs(title="Elevation map")+
  coord_sf(xlimits,ylimits,datum=sf::st_crs(32736))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

elevation_map

# combine the different maps  into one composite map using the patchwork library
# and save it to a high resolution png

allmaps<-woody_map+elevation_map+rainfall_map+
  patchwork::plot_layout(ncol=1)
allmaps
#ggsave("./figures/allmaps.png",allmaps,width=18,height=18,units="cm",dpi=300)




############################
### explore your study area
# set the limits of your study area
xlimits<-sf::st_bbox(studyarea)[c(1,3)]
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)
saExt
# crop the woody biomass to the extent of the studyarea
woodybiom_sa<-terra::crop(woodybiom,saExt)

# plot the woody biomass
woody_map_sa<-ggplot()+
  tidyterra::geom_spatraster(data=woodybiom_sa)+
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77,6.55),
                       oob=squish,#means that values outside the limits are set to the colour of the limits.
                       name="TBA/ha")+
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.7,colour="green")+
  #add study area, rivers and lakes. STudy area in red,not filled. lake=light blue. rivers=blue.
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,colour="red",linewidth=1)+
  tidyterra::geom_spatvector(data=rivers,colour="blue")+
  tidyterra::geom_spatvector(data=lakes,fill="lightblue")+
  labs(title="Woody biomass")+
  coord_sf(xlimits,ylimits, expand=F,datum=sf::st_crs(32736))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

woody_map_sa

# make maps also for the other layers that you found


#distance to river####################################################################################
dist_river_20_sa<-terra::rast("C:/Users/franc/Documents/Master/APCE2024/QGIS/apce2024gis/2022_rivers/DistanceToRiver20.tif")

dist_river_20_sa_map<-ggplot()+
  tidyterra::geom_spatraster(data=dist_river_20_sa/200)+
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77,6.55),
                       oob=squish,#means that values outside the limits are set to the colour of the limits.
                       name="TBA/ha")+
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.7,colour="green")+
  #add study area, rivers and lakes. STudy area in red,not filled. lake=light blue. rivers=blue.
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,colour="red",linewidth=1)+
  tidyterra::geom_spatvector(data=rivers,colour="blue")+
  tidyterra::geom_spatvector(data=lakes,fill="lightblue")+
  labs(title="Distance to River")+
  coord_sf(xlimits,ylimits, expand=F,datum=sf::st_crs(32736))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

dist_river_20_sa_map

#soilfertility
soil_fertility_sa<-terra::rast("C:/Users/franc/Documents/Master/APCE2024/QGIS/apce2024gis/SoilFertilityCEC_5_15cm.tif")


soil_fertility_sa_map<-ggplot()+
  tidyterra::geom_spatraster(data=soil_fertility_sa)+
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77,6.55),
                       oob=squish,#means that values outside the limits are set to the colour of the limits.
                       name="TBA/ha")+
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.7,colour="green")+
  #add study area, rivers and lakes. STudy area in red,not filled. lake=light blue. rivers=blue.
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,colour="red",linewidth=1)+
  tidyterra::geom_spatvector(data=rivers,colour="blue")+
  tidyterra::geom_spatvector(data=lakes,fill="lightblue")+
  labs(title="Woody biomass")+
  coord_sf(xlimits,ylimits, expand=F,datum=sf::st_crs(32736))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl",width_hint=0.2)













# create 500 random points in our study area


# and add them to the previous map

# make distance to river map



### put all maps together



# extract your the values of the different raster layers to the points


# make long format

# plot how woody cover is predicted by different variables


