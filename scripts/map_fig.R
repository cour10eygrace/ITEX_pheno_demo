# Dependencies
library(maps)
library(raster)
#library(maptools)
library(dplyr)
library(RColorBrewer)
library(sf) # Simple feature geometries
library(rnaturalearth) # National boundaries from https://www.naturalearthdata.com/
library(rnaturalearthdata) 
library(cowplot) # Added simplicity for ggplot themes and export
library(ggrepel) # Avoid overlap when placing ggplot labels
#library(ggbeeswarm)#position quasirandom 

#find projections here 
#https://git.earthdata.nasa.gov/projects/GIBSGEN/repos/gibsgen/browse/source/gdal_data/esri_extra.wkt?at=75b510afcc66a0b01d7c671d286edf799af3d911

#setwd("C:/Users/court/Google Drive/UBC Postdoc")

# organize Site coordinates
sites<-read.csv("G:/My Drive/UBC Postdoc/ITEX_locations_ASissue.csv") 
sites<-tidyr::separate(sites, Site, c("Site", "state_province_country"), sep=',')

#R can't figure out how to do g with dot...R just makes it a g
#sites$Site[sites$Site=="Utqia?vik"]="Utqiagvik"
#sites<-filter(sites, !is.na(Site))

sort(unique(sites$Site))

#MAP 1----
# Specify a lambert conformal conical projection, which works nicely
# for the area covered by the sites.
# I use a WKT specification here as this is most easy to understand
# You can change the "Central_Meridian" parameter to shift the central
# meridian of the map: -96 for North America, -40 For Greenland, 
# 0 for Greenwich

lambert_conformic1 <- 'PROJCS["North_America_Lambert_Conformal_Conic",
                            GEOGCS["GCS_North_American_1983",
                                   DATUM["North_American_Datum_1983",
                                         SPHEROID["GRS_1980",6378137,298.257222101]],
                                   PRIMEM["Greenwich",0],
                                   UNIT["Degree",0.017453292519943295]],
                            PROJECTION["Lambert_Conformal_Conic_2SP"],
                            PARAMETER["False_Easting",0],
                            PARAMETER["False_Northing",0],
                            PARAMETER["Central_Meridian",-96], 
                            PARAMETER["Standard_Parallel_1",33],
                            PARAMETER["Standard_Parallel_2",45],
                            PARAMETER["Latitude_Of_Origin",40],
                            UNIT["Meter",1],
                            AUTHORITY["EPSG","102009"]]'

# Generate political boundaries as sf and transform from lat long to lambert
world <- ne_countries(scale = "medium", returnclass = "sf")
world_lambert <- st_transform(world, crs = st_crs(lambert_conformic1))

#make a factor 
sites$Site<-as.factor(sites$Site)
#sites$Manipulation<-as.factor(sites$Manipulation)

#remove Bogong and Changbai for separate plot
#remove duplicates
plot1_sites<-filter(sites, Lat>50&Long<0)%>%
  dplyr::select(Site, Lat, Long, treat)%>%
  group_by(Site, treat)%>%distinct(.)

# Generate sf object for site point sources and transform
sites_sf <- st_as_sf(plot1_sites, coords = c("Long", "Lat"), crs = 4326)
sites_sf <- st_transform(sites_sf , crs = st_crs(lambert_conformic1))

# Next we need to define the area we want to plot
# For this we get the minimum and maximum coordinates and 
# add a good buffer, I started the buffer with a guess and then
# modified till it looked good
map_extent1 <- sites_sf %>% st_coordinates() %>%
  as.data.frame() %>%
  summarise(xmin = min(X) - 1000000,
            xmax = max(X) + 1000000,
            ymin = min(Y) - 1000000,
            ymax = max(Y) + 1000000)

map_extent<-map_extent1


sites_sf<-filter(sites_sf, Site=="Alexandra Fiord*"|Site=="Daring Lake")%>%
  filter(.,treat=="OTC")

# Plot data using ggplot
otc_site_map <- ggplot() + 
  # geom_sf to plot the country boundaries
  geom_sf(data = world_lambert,
          fill = "#d3d3d3FF", 
          colour = "white",
          size = 0.25) + 
  #geom_point(colour="black")+ #trying to get black ouline on points...
  # geom_sf to plot the sites (it automatically knows they're points)
  geom_sf(data = sites_sf, fill = "#ffffff00",  
          size=3, shape = 21, color="#008080",
          stroke = 1.5)+   # geom_label_repel with st_coordinates as stats to plot labels 
  # that are not overlapping
  geom_text_repel(data = sites_sf, aes(label = Site,
                                       geometry = geometry),
                  stat = "sf_coordinates" ,
                  min.segment.length = 0.1,
                  segment.colour = "black", # Just in case you would like to modify these
                  colour = "black") + # Just in case you would like to modify these
  labs(x = "", 
       y = "") + 
  # set extent to be plotted using previously determined coordinates
  coord_sf(xlim = c(map_extent$xmin,
                    map_extent$xmax),
           ylim = c(map_extent$ymin,
                    map_extent$ymax), 
           expand = F) +
  # Map relevant modifiers for the theme
  theme(legend.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "mm"),
        axis.title = element_blank(),
        panel.grid.minor = element_line(colour = "white"), # Just in case you would like to modify these
        panel.grid.major = element_line(colour = "white"), # Just in case you would like to modify these
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        panel.background = element_rect(fill = "white"), # Just in case you would like to modify these
        panel.border = element_rect(colour = "lightgrey", fill=NA, size=0.5)
  ) 

# A quick plot on the native graphics device
otc_site_map
