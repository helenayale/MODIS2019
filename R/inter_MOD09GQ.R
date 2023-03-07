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
####    MOD09GQ    ####
#######################

setwd("D:/R/MODIS2019/MOD09GQ_2019")
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

allrasters   ### missing 1, 40, 59, 365

file_list[19]
plot(allrasters[[19]]) 

update_single(file_list, allrasters, 19)

update_file(file_list, allrasters, 21, 33 )
update_file(file_list, allrasters, 35, 41 )

file_list[43]
update_single(file_list, allrasters, 43)
update_single(file_list, allrasters, 46)

update_file(file_list, allrasters, 48, 52 )

update_single(file_list, allrasters, 55)


file_list[57]

update_file(file_list, allrasters, 57, 61 )
update_file(file_list, allrasters, 63, 74 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 63, 75 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 57, 75 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 57, 76 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 46, 52 )

file_list[64]
update_single(file_list, allrasters, 64)

file_list[66]
update_single(file_list, allrasters, 66)


file_list[78]
update_single(file_list, allrasters, 78)


file_list[80]
update_file(file_list, allrasters, 80, 84 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 80, 85 )

update_single(file_list, allrasters, 87)

file_list[89]
update_file(file_list, allrasters, 89, 92 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 89, 93 )

file_list[95]
update_file(file_list, allrasters, 95, 96 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 95, 97 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 94, 96 )

update_file(file_list, allrasters, 99, 101 )

update_single(file_list, allrasters, 104)
update_single(file_list, allrasters, 110)


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 109, 110 )

update_file(file_list, allrasters, 113, 116 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 112, 116 )

file_list[118]
update_single(file_list, allrasters, 118)
update_single(file_list, allrasters, 119)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 120, 126 )
update_single(file_list, allrasters, 128)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 120, 128 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 120, 129 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 118, 129 )

update_single(file_list, allrasters, 131)

update_file(file_list, allrasters, 133, 134 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 130, 134 )

update_file(file_list, allrasters, 78, 97 )

update_file(file_list, allrasters, 137, 139 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 137, 140 )
update_file(file_list, allrasters, 143, 146 )
update_file(file_list, allrasters, 148, 149 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 136, 146 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 133, 149 )

update_single(file_list, allrasters, 151)
update_single(file_list, allrasters, 154)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 153, 154 )

update_single(file_list, allrasters, 152)
update_single(file_list, allrasters, 156)

update_file(file_list, allrasters, 158, 160 )
update_file(file_list, allrasters, 162, 164 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 162, 165 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 156, 164 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 156, 165 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 154, 166 )

update_file(file_list, allrasters, 168, 170 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 168, 171 )

update_single(file_list, allrasters, 173)
update_single(file_list, allrasters, 179)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 179, 181 )

update_single(file_list, allrasters, 183)
update_single(file_list, allrasters, 185)
update_single(file_list, allrasters, 187)

file_list[189]
update_file(file_list, allrasters, 189, 193 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 179, 193 )

file_list[196]
update_file(file_list, allrasters, 196, 197 )

update_single(file_list, allrasters, 200)


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 196, 200 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 195, 200 )

file_list[205]
update_file(file_list, allrasters, 205, 212 )
update_file(file_list, allrasters, 214, 216 )
update_file(file_list, allrasters, 218, 219 )

update_single(file_list, allrasters, 221)
update_single(file_list, allrasters, 224)
update_single(file_list, allrasters, 226)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 205, 226 )

update_single(file_list, allrasters, 229)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 228, 229 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 205, 230 )

update_single(file_list, allrasters, 233)
update_file(file_list, allrasters, 237, 239 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 233, 239 )
update_file(file_list, allrasters, 241, 242 )

update_single(file_list, allrasters, 231)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 233, 242 )

update_single(file_list, allrasters, 245)
update_file(file_list, allrasters, 247, 249 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 245, 249 )

update_file(file_list, allrasters, 252, 253 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 245, 254 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 244, 253 )
update_file(file_list, allrasters, 256, 258 )

update_single(file_list, allrasters, 260)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 256, 260 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 255, 260 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 252, 260 )

update_file(file_list, allrasters, 263, 268 )

update_file(file_list, allrasters, 270, 281 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 263, 282 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 262, 283 )

update_file(file_list, allrasters, 286, 289 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 285, 289 )

update_single(file_list, allrasters, 291)

update_file(file_list, allrasters, 293, 295 )

update_single(file_list, allrasters, 297)
update_single(file_list, allrasters, 299)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 286, 300 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 285, 300 )

update_file(file_list, allrasters, 302, 307 )
update_file(file_list, allrasters, 309, 312 )
update_file(file_list, allrasters, 314, 323 )
update_file(file_list, allrasters, 325, 330 )

update_single(file_list, allrasters, 332)
update_single(file_list, allrasters, 334)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 332, 334 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 332, 335 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 309, 334 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 302, 334 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 302, 335 )

update_file(file_list, allrasters, 337, 340 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 336, 340 )

update_file(file_list, allrasters, 342, 347 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 336, 347 )

update_file(file_list, allrasters, 349, 351 )
update_file(file_list, allrasters, 353, 356 )

update_single(file_list, allrasters, 359)

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 353, 359 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 349, 359 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 337, 359 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 302, 359 )


file_list[64]
update_single(file_list, allrasters, 64)
update_single(file_list, allrasters, 66)


file_list[78]
update_file(file_list, allrasters, 78, 92 )


update_file(file_list, allrasters, 95, 102 )

update_file(file_list, allrasters, 104, 106 )

file_list[116]
update_single(file_list, allrasters, 116)


update_file(file_list, allrasters, 118, 150 )

update_file(file_list, allrasters, 154, 165 )

update_file(file_list, allrasters, 167, 170 )


## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 153, 171 )
update_file(file_list, allrasters, 173, 174 )
update_file(file_list, allrasters, 183, 202 )

update_file(file_list, allrasters, 204, 230 )

file_list[233]
update_file(file_list, allrasters, 232, 241 )
update_file(file_list, allrasters, 242, 259 )

update_file(file_list, allrasters, 261, 281 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 261, 282 )

update_file(file_list, allrasters, 284, 299 )

update_file(file_list, allrasters, 301, 333 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 301, 334 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)
update_file(file_list, allrasters, 335, 346 )

update_file(file_list, allrasters, 348, 359 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 301, 359 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 301, 358 )


file_list[19]
file_list[41]
update_file(file_list, allrasters, 19, 41 )

## reload files
file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
file_list
allrasters <- stack(file_list)

update_file(file_list, allrasters, 19, 42 )

file_list[48]
update_file(file_list, allrasters, 48, 53 )

