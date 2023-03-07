library(sp)
library(rgdal)
library(raster)
library(gtools)


setwd("D:/R/MODIS2019/MOD13Q1_CKF_2019_proj")

CKFlist <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
CKFlist
CKF_origin  <- stack(CKFlist)


for (i in 1:length(CKFlist)){
  
  temp <- CKF_origin[[i]] / 10000
  
  if (i == 1){
    CKF <- temp
  }else{
    CKF <- stack(CKF, temp)
  } 
  
}

setwd("D:/R/ODIS2019/MOD13Q1_CKF_2019_div")

for (i in 1:length(CKFlist)){
  writeRaster(CKF[[i]], CKFlist[i], format = "GTiff", overwrite = TRUE)
}



setwd("D:/R/MODIS2019/MOD13Q1_2019_proj")

MOD13Q1list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD13Q1list
MOD13Q1_origin  <- stack(MOD13Q1list)


for (i in 1:length(MOD13Q1list)){
  
  temp <- MOD13Q1_origin[[i]] / 10000
  
  if (i == 1){
    MOD13Q1 <- temp
  }else{
    MOD13Q1 <- stack(MOD13Q1, temp)
  } 
  
}

setwd("D:/R/ODIS2019/MOD13Q1_2019_div")

for (i in 1:length(MOD13Q1list)){
  writeRaster(MOD13Q1[[i]], MOD13Q1list[i], format = "GTiff", overwrite = TRUE)
}



