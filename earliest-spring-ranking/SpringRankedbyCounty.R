library(raster)
library(sf)
library(sp)
library(exactextractr)
library(readr)
library(rnpn)
library(rgeos)
library(rgdal)
library(zoo)
library(dplyr)
library(tibble)
library(tidyverse)

rm(list=ls())

#this code gets the average day of year of leaf out and bloom by county (weighted average of pixels that fall within the county), enabling you
#to get a ranking, the top 10 earliest springs in this county, including the current spring as it unfolds
#this was used in a few outlets in the early spring of 2023, contact Janie.Haseman@hearst.com (example, all behind paywall https://www.houstonchronicle.com/projects/2023/when-is-spring-houston/)

RasterFolder <- "~/Documents/My Files/USA-NPN/Data/Analysis/R_default/npn_analyses/data_sources/rasters/PRISM_doy"
VectorFolder <- "~/Documents/My Files/USA-NPN/Data/Analysis/R_default/npn_analyses/data_sources/polygons/cb_2018_us_county_20m"
OutputFolder <- "~/Documents/My Files/USA-NPN/Data/Analysis/R_default/npn_analyses/SpringByCounty"

#helper call for raster layer details
layers <- npn_get_layer_details()

#GET RASTER DATA TOGETHER
setwd(RasterFolder)

#RUN ONCE (assuming you don't already have the PRISM layers saved on your computer)
#Get the PRISM SI-x data FLI dates, using a loop
#this code uses the same period as the return interval, to match our other communications, so for 2023, it was 1983-2023(I think).
for(year in 1983:2022){
  npn_download_geospatial('si-x:average_leaf_prism', paste0(year,'-01-01'),output_path=paste0(year,"si-x_leaf_prism",".tif"))
}

for(year in 1983:2022){
  npn_download_geospatial('si-x:average_bloom_prism', paste0(year,'-01-01'),output_path=paste0(year,"si-x_bloom_prism",".tif"))
}

#I usually manually get the current year's NCEP layer from the geoserver request builder, and put it in my raster folder
#there is possibly an issue with the NCEP web service call, or you have to use elevation instead of date?

#Read in data and resample the current year's to match the resolution of the prior record for leaf
Leaf2020 <- raster("2020si-x_leaf_prism.tif")
Leaf2023 <- raster("2023si-x_leaf_ncep.tif") #this is the layer I get manually
NAvalue(Leaf2023) <- -9999
plot(Leaf2023)
plot(Leaf2020)

Leaf2023_4K <-resample(Leaf2023, Leaf2020, method='bilinear')

#save out the new resampled raster
writeRaster(Leaf2023_4K, "2023_si-x_leaf_ncep_resampled.tif", format="GTiff",overwrite=TRUE, NAflag=-9999)

#do the same for bloom
Bloom2020 <- raster("2020si-x_bloom_prism.tif")
Bloom2023 <- raster("2023si-x_average_bloom_ncep.tif")
NAvalue(Bloom2023) <- -9999
plot(Bloom2023)

Bloom2023_4K <-resample(Bloom2023, Bloom2020, method='bilinear')

writeRaster(Bloom2023_4K, "2023_si-x_bloom_ncep_resampled.tif", format="GTiff",overwrite=TRUE, NAflag=-9999)

#Now I manually remove the UNresampled NCEP file from the directory

#Read in all the raster data for leaf
leaf_files <- list.files(getwd(),pattern="leaf")
FLI_stack <- stack(leaf_files) 
NAvalue(FLI_stack) <- -9999
crs(FLI_stack) #check coordinate reference system
plot(FLI_stack[[39:41]], NAcol='blue')  #check out a few


#GET VECTOR DATA TOGETHER

#read in US Census County boundaries as a special feature
setwd(VectorFolder)
pl <- st_read("cb_2018_us_county_20m.shp")
pli <- st_as_sf(pl)
crs(pli)
plit <- st_transform(pli, 4269) #code for NAD83
crs(plit)
plot(plit)

#ANALYZE VECTOR AND RASTER TOGETHER

#check that the raster and the vector line up
plot(FLI_stack[[40]], NAcol='blue') 
plot(plit, add=TRUE)

#get the state abbreviation from the FPS Code
str(plit) #check column types 
plit$STATEFP <- as.numeric(plit$STATEFP) #make the FP Code numeric

#get the FPS look up table
setwd(OutputFolder)
fips_codes <- read.csv("us-state-fips.csv")

#merge it with the plit object
plit <- merge(plit, fips_codes, by = 'STATEFP', all = TRUE)

#create a new column for each year with the DOY, weighted by how much of the polygon falls into each cell. 
plit$FLI <- exact_extract(FLI_stack, plit, 'weighted_mean', weights=area(FLI_stack))
tab <-head(plit, n=5) #check this file

#Save out this file that has the geometry and the first leaf dates by year
#create a row name that has both the state abbreviation, the GEO ID and the County Name
plit$STATE_NAME <- paste0(plit$STATE_ABBR, sep = '_', plit$NAME, sep = '_', plit$GEOID)
row.names(plit$FLI) <- plit$STATE_NAME
setwd(OutputFolder)
write.csv(plit$FLI, file = 'County_FLI_values_1983-2023_Aug2023.csv')

#here you call back up that CSV
LEAF <- readr::read_csv("County_FLI_values_1983-2023_Aug2023.csv")

# Transpose the data frame, so each column is a county (excluding the unit names/col 1 bc strings mess up transposing)
mat <- t(as.matrix(LEAF[c(1:42)]))

df1 <- data.frame(Layer = row.names(mat),mat)

#getting a clean year column without all the "weighted_mean.. stuff"
df1$Year <- substr(df1$Layer, 16, 19)
df1 <- df1 %>% select(Year, everything())

write.csv(df1, file = 'first_leaf_dates_by_county_final_Aug2023.csv')

#note that this output gives you year in one column and day of year in the county-by-county columns.
#If you convert DOY to date in Excel don't just change the column type to date, bc it will 
#miss leap years. Better is this formula in Excel =DATE(A4,1,B4) (where col A has the year
#and col B has the day of year). I'm sure this could be done here with lubridate as well.
