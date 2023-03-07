library(sp)
library(rgdal)
library(raster)
library(gtools)

setwd("D:/R/MODIS2019/Landsat_crop")
file_list_landsat <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list_landsat
landsat <- stack(file_list_landsat)

landsat


setwd("D:/R/MODIS2019/MOD13Q1_2019_proj")
file_list_MOD13Q1 <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list_MOD13Q1
MOD13Q1 <- stack(file_list_MOD13Q1)

MOD13Q1
plot(MOD13Q1[[1]])

## resampling

for (i in 1:length(file_list_MOD13Q1)){
  
  resampled <- resample(MOD13Q1[[i]], landsat[[8]], method='bilinear')
  
  if (i == 1){
    resampled_MOD13Q1 <- resampled
  }else{
    resampled_MOD13Q1 <- stack(resampled_MOD13Q1 ,resampled)
  }
 
}


plot(resampled_MOD13Q1[[1]])

## save resampled files

setwd("D:/R/MODIS2019/MOD13Q1_2019_resample")

for (i in 1:length(file_list_MOD13Q1)){
  writeRaster(resampled_MOD13Q1[[i]], file_list_MOD13Q1[i], format = "GTiff", overwrite = TRUE)
}



