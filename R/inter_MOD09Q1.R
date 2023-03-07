library(sp)
library(rgdal)
library(raster)
library(gtools)


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


## a function to iterpolate & update a single file

update_single <- function(file_list, raster_list, i){  ## i is the layer to be interpolate
  
  merge <- interpol2raster(raster_list,i)
  plot(merge)
  writeRaster(merge, file_list[i], format = "GTiff", overwrite = TRUE)
  
}

## a function to interpolate & update the files

update_file <- function(file_list, raster_list, i,j){  ## i,j are the first/last layers to be interpolate
  
  temp_list <- inter_linear(raster_list, i, j)
  
  for (x in i:j){
    writeRaster(temp_list[[x-i+1]], file_list[x], format = "GTiff", overwrite = TRUE)
  }
  
}




#######################
####    MOD09Q1    ####
#######################

setwd("D:/R/MODIS2019/MOD09Q1_2019")
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

allrasters
plot(allrasters[[1]])

update_single(file_list, allrasters, 2)
update_single(file_list, allrasters, 4)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 4, 5 )

update_single(file_list, allrasters, 9)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 8, 9 )

update_single(file_list, allrasters, 11)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 11, 13 )

update_single(file_list, allrasters, 16)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 16, 17 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 18, 19 )

update_single(file_list, allrasters, 15)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 15, 16 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 16, 18 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 16, 19 )

update_single(file_list, allrasters, 21)
update_single(file_list, allrasters, 24)
update_single(file_list, allrasters, 25)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 24, 25 )
update_file(file_list, allrasters, 27, 28 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 27, 29 )
update_file(file_list, allrasters, 19, 22 )


update_single(file_list, allrasters, 35)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 34, 35 )

update_single(file_list, allrasters, 37)
update_single(file_list, allrasters, 32)
update_single(file_list, allrasters, 33)
update_single(file_list, allrasters, 35)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 33, 36 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 32, 33 )
update_file(file_list, allrasters, 35, 37 )
update_file(file_list, allrasters, 39, 42 )

update_single(file_list, allrasters, 45)


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 44, 45 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 39, 43 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 39, 45 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_single(file_list, allrasters, 44)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 40, 44 )


