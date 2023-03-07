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
####    MCD43A4    ####
#######################

setwd("D:/R/MODIS2019/MCD43A4_2019")
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

allrasters
file_list[78]
plot(allrasters[[78]]) 


update_single(file_list, allrasters, 78)


##plot(allrasters[[106]])
plot(allrasters[[107]])
plot(allrasters[[108]])
plot(allrasters[[109]])
plot(allrasters[[110]])
plot(allrasters[[111]])
plot(allrasters[[112]])
##plot(allrasters[[113]])


update_single(file_list, allrasters, 107)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)


update_file(file_list, allrasters, 107, 112 )
update_file(file_list, allrasters, 66, 72 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 65, 74 )
update_file(file_list, allrasters, 27, 38 )

update_file(file_list, allrasters, 129, 131 )
update_file(file_list, allrasters, 141, 145 )

update_single(file_list, allrasters, 154)
update_single(file_list, allrasters, 161)

update_file(file_list, allrasters, 160, 165 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 165, 166 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 164, 167 )

update_single(file_list, allrasters, 169)
update_single(file_list, allrasters, 170)
update_single(file_list, allrasters, 172)
update_single(file_list, allrasters, 183)
update_single(file_list, allrasters, 196)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

plot(allrasters[[194]]) 

update_file(file_list, allrasters, 194, 197 )


update_single(file_list, allrasters, 202)
update_single(file_list, allrasters, 211)


update_file(file_list, allrasters, 216, 222 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 216, 223 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)


update_file(file_list, allrasters, 216, 225 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)


update_file(file_list, allrasters, 215, 226 )


update_single(file_list, allrasters, 231)
update_single(file_list, allrasters, 238)
update_single(file_list, allrasters, 241)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)


update_file(file_list, allrasters, 240, 241 )

update_single(file_list, allrasters, 245)
update_single(file_list, allrasters, 256)
update_single(file_list, allrasters, 261)


update_file(file_list, allrasters, 273, 277 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 272, 278 )


update_single(file_list, allrasters, 291)
update_single(file_list, allrasters, 293)
update_single(file_list, allrasters, 296)
update_single(file_list, allrasters, 307)
update_single(file_list, allrasters, 309)
update_single(file_list, allrasters, 311)

update_file(file_list, allrasters, 347, 355 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)


update_file(file_list, allrasters, 347, 356 )
update_file(file_list, allrasters, 325, 330 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 313, 330 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 313, 358 )

update_single(file_list, allrasters, 139)


