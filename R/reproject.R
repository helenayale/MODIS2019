library(sp)
library(rgdal)
library(raster)
library(gtools)


setwd("D:/R/MODIS2019/Landsat_crop")
file_list_Landsat <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list_Landsat
Landsat <- stack(file_list_Landsat)

Landsat





setwd("D:/R/MODIS2019/MOD13Q1_2019")
file_list_MOD13Q1 <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list_MOD13Q1
MOD13Q1 <- stack(file_list_MOD13Q1)

MOD13Q1



setwd("D:/R/MODIS2019/MOD13Q1_CKF_2019_crop")
file_list_CKF <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list_CKF
MOD13Q1_CKF <- stack(file_list_CKF)

MOD13Q1_CKF


###########################################
########## reproject MOD13Q1_CKF ##########
###########################################

## reprojection

sr <- proj4string(Landsat)
sr


projected_MOD13Q1_CKF <- projectRaster(MOD13Q1_CKF, Landsat, method = 'bilinear')

projected_MOD13Q1_CKF


temp <- stack(projected_MOD13Q1_CKF[[1]], Landsat[[1]])
plot(temp)




## save reprojected files 

setwd("D:/R/MODIS2019/MOD13Q1_CKF_2019_proj")

for (i in 1:length(file_list_CKF)){
  writeRaster(projected_MOD13Q1_CKF[[i]], file_list_CKF[i], format = "GTiff", overwrite = TRUE)
}




#######################################
########## reproject MOD13Q1 ##########
#######################################

## reprojection

sr <- proj4string(Landsat)
sr

  
projected_MOD13Q1 <- projectRaster(MOD13Q1, Landsat, method = 'bilinear')

projected_MOD13Q1


temp <- stack(projected_MOD13Q1[[1]], Landsat[[1]])
plot(temp)




## save reprojected files 

setwd("D:/R/MODIS2019/MOD13Q1_2019_proj")

for (i in 1:length(file_list_MOD13Q1)){
  writeRaster(projected_MOD13Q1[[i]], file_list_MOD13Q1[i], format = "GTiff", overwrite = TRUE)
}

#######################################
########## reproject MOD09Q1 ##########
#######################################

setwd("D:/R/MODIS2019/MOD09Q1_2019")
file_list_MOD09Q1 <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list_MOD09Q1
MOD09Q1rasters <- stack(file_list_MOD09Q1)

MOD09Q1rasters


## reprojection


projected_MOD09Q1 <- projectRaster(MOD09Q1rasters,Landsat, method = 'bilinear')

projected_MOD09Q1

temp <- stack(projected_MOD09Q1[[1]], Landsat[[1]])
plot(temp)


## save reprojected files

setwd("D:/R/MODIS2019/MOD09Q1_2019_proj")

for (i in 1:length(file_list_MOD09Q1)){
  writeRaster(projected_MOD09Q1[[i]], file_list_MOD09Q1[i], format = "GTiff", overwrite = TRUE)
}



#######################################
########## reproject MOD09GQ ##########
#######################################


setwd("D:/R/MODIS2019/MOD09GQ_2019")
file_list_MOD09GQ <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list_MOD09GQ
MOD09GQrasters <- stack(file_list_MOD09GQ)

MOD09GQrasters


## reproject


projected_MOD09GQ <- projectRaster(MOD09GQrasters, Landsat, method = 'bilinear')

projected_MOD09GQ

temp <- stack(projected_MOD09GQ[[1]], Landsat[[1]])
plot(temp)

## save reprojected files

setwd("D:/R/MODIS2019/MOD09GQ_2019_proj")

for (i in 1:length(file_list_MOD09GQ)){
  writeRaster(projected_MOD09GQ[[i]], file_list_MOD09GQ[i], format = "GTiff", overwrite = TRUE)
}




#######################################
########## reproject MCD43A4 ##########
#######################################


setwd("D:/R/MODIS2019/MCD43A4_2019")
file_list_MCD43A4 <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list_MCD43A4
MCD43A4rasters <- stack(file_list_MCD43A4)

MCD43A4rasters


## reprojection



projected_MCD43A4 <- projectRaster(MCD43A4rasters, Landsat, method = 'bilinear')

projected_MCD43A4

temp <- stack(projected_MCD43A4[[1]], Landsat[[1]])
plot(temp)


## save reprojected fiels

setwd("D:/R/MODIS2019/MCD43A4_2019_proj")

for (i in 1:length(file_list_MCD43A4)){
  writeRaster(projected_MCD43A4[[i]], file_list_MCD43A4[i], format = "GTiff", overwrite = TRUE)
}


