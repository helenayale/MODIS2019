library(sp)
library(rgdal)
library(raster)
library(gtools)
library(RStoolbox)



setwd("D:/R/MODIS2019/shp_epsg25832")

shp <- readOGR("lower_frankonia.shp")
plot(shp)


setwd("D:/R/MODIS2019/Landsat_2")
file_landsat <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_landsat
landsat <- stack(file_landsat)

landsat

plot(landsat[[1]])

## cropping

shapefile <- spTransform(shp, CRS(proj4string(landsat[[1]])))
plot(shapefile)

for (i in 1:length(file_landsat)){
  
  crop <- crop(landsat[[i]], extent(shapefile))
  mask <- mask(crop, shapefile)
  if (i == 1){
    crop_landsat <- mask
  }else{
    crop_landsat <- stack(crop_landsat, mask)
  }
}

plot(crop_landsat[[1]])



## save cropped files

setwd("D:/R/MODIS2019/Landsat_crop")

for (i in 1:length(file_landsat)){
  writeRaster(crop_landsat[[i]], file_landsat[i], format = "GTiff", overwrite = TRUE)
}





setwd("D:/R/MODIS2019/MOD13Q1_CKF_2019")
file_CKF <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_CKF
MOD13Q1_CKF <- stack(file_CKF)

MOD13Q1_CKF

plot(MOD13Q1_CKF[[1]])

## cropping

shapefile <- spTransform(shp, CRS(proj4string(MOD13Q1_CKF[[1]])))
plot(shapefile)

for (i in 1:length(file_CKF)){
  
  crop <- crop(MOD13Q1_CKF[[i]],extent(shapefile))
  mask <- mask(crop, shapefile)
  if (i == 1){
    crop_CKF <- mask
  }else{
    crop_CKF <- stack(crop_CKF, mask)
  }
}

plot(crop_CKF[[1]])



## save cropped files

setwd("D:/R/MODIS2019/MOD13Q1_CKF_2019_crop")

for (i in 1:length(file_CKF)){
  writeRaster(crop_CKF[[i]], file_CKF[i], format = "GTiff", overwrite = TRUE)
}


