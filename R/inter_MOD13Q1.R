library(sp)
library(rgdal)
library(raster)


## a function to interpolate between two raster ##

interpol2raster <- function(raster_list, i){  ## i is the layer to be interpolate
  inter <- mean(raster_list[[i-1]], raster_list[[i+1]])
  
  maskfile <- mask(inter, raster_list[[i]],  inverse=TRUE, maskvalue=NA)
  
  merge <- merge(raster_list[[i]], maskfile, overlap=TRUE)
  
  return(merge)
}

## a function to linear interpolate between 2 raster ##
inter_linear <- function(raster_list, i,j){  ## i,j are the first/last layers to be interpolate
  
  step <- (raster_list[[j+1]] - raster_list[[i-1]])/(j-i+2)  
  
  for (x in i:j){
    inter <- raster_list[[i-1]] + step * (x-i+1)
    maskfile <- mask(inter, raster_list[[x]],  inverse=TRUE, maskvalue=NA)
    merge <- merge(raster_list[[x]], maskfile, overlap=TRUE)
    if (x == i) {
      result_list <- merge
    } else{
      result_list <- stack(result_list, merge)
    }
    
  }
  
  return(result_list)
}



#######################
####    MOD13Q1    ####
#######################

setwd("D:/R/MODIS2019/MOD13Q1_2019")
file_list <-  list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE)
file_list
allrasters <- stack(file_list)


plot(allrasters[[1]])
plot(allrasters[[2]])
plot(allrasters[[3]])
plot(allrasters[[4]])
plot(allrasters[[5]])
plot(allrasters[[6]])
plot(allrasters[[7]])
plot(allrasters[[8]])
plot(allrasters[[9]])
plot(allrasters[[10]])
plot(allrasters[[11]])
plot(allrasters[[12]])
plot(allrasters[[13]])
plot(allrasters[[14]])
plot(allrasters[[15]])
plot(allrasters[[16]])


