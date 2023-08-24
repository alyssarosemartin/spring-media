library(raster)
library(sp)
library(rnpn)
library(leaflet)
library(leaflet.extras)
rm(list=ls())

Dir <- "~/Documents/My Files/USA-NPN/Data/Analysis/R_default/npn_analyses/Groundhog"
setwd(Dir)

#helper calls for gridded layers information
layers <- npn_get_layer_details()
print(layers$abstract)
npn_get_layer_details(climate:prism_ppt)

#ACQUIRE DATA
#download all ind pmetrics so far this year- the BLB/initial growth and open flowers (ref - http://www.usanpn.org/npn_portal/phenophases/getPhenophases.xml)
df <- npn_download_individual_phenometrics(
  request_source = 'Alyssa', 
  years = c(2023),
  #species_ids = c(201),
  #phenophase_ids = c(371,373,482,492,201,205,210)
  pheno_class_ids = c(1,7)
)

leaf <- subset(df, pheno_class_id == "1")
flower<- subset(df, pheno_class_id == "7")

#acquire raster forecast anomaly for Feb 2 (32 base)
groundhog <-npn_download_geospatial("gdd:agdd_anomaly","2023-02-13")  
plot(groundhog)
hist(groundhog)


#MAPPING

#prep point data viz, by creating and sizing leaf and flower icons
my_icons <- iconList(
  ileaf <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/leaf-icon-16.png",
                    iconWidth = 20, iconHeight = 20),
  iflower <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/purple-flower-png-17.png",
                      iconWidth = 20, iconHeight = 20)
)

#prep raster viz with  bins and palette
bins <- c(-500, -20, 20, 200, 500) 
pal <- colorBin(c("#6fa8d6", "#f7f0da", "#fa824d", "#f44f07"), domain = groundhog, bins = bins,  na.color = "transparent")

#create map
leaflet(leaf) %>% 
  addRasterImage(groundhog, colors = pal, opacity = 1) %>%
  addMarkers(lng = ~leaf$longitude, lat = ~leaf$latitude, icon = ileaf)  %>%
  addMarkers(lng = ~flower$longitude, lat = ~flower$latitude, icon = iflower)  %>%
  setMapWidgetStyle(list(background= "transparent"))  #this doesn't exactly work - the background is white


  