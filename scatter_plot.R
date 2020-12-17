library(sp)
library(rgdal)
library(raster)
library(gtools)
library(dplyr)
library(mapsRinteractive)

library(MASS)
library(ggplot2)
library(viridis)

# load landsat files

setwd("D:/HYe/DataFusion/Landsat_crop_2")

landsat_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
landsat_list
landsat <- stack(landsat_list)


# load masked files for each land cover

# Land Cover 1
setwd("D:/R/MODIS2019/Landsat_LC1")

landsat_lc1_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
landsat_lc1_list
landsat_lc1 <- stack(landsat_lc1_list)

# Land Cover 2
setwd("D:/R/MODIS2019/Landsat_LC2")

landsat_lc2_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
landsat_lc2_list
landsat_lc2 <- stack(landsat_lc2_list)

# Land Cover 3
setwd("D:/R/MODIS2019/Landsat_LC3")

landsat_lc3_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
landsat_lc3_list
landsat_lc3 <- stack(landsat_lc3_list)


# define DOYs

DOY <- c("DOY49","DOY81","DOY113","DOY145","DOY177","DOY193","DOY209","DOY225","DOY241","DOY289")



###############################
############ 43A4  ############
###############################

# load validation files 43A4
setwd("D:/HYe/DataFusion/Validation_43A4")

valid_list_43A4 <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
valid_list_43A4
valid_43A4 <- stack(valid_list_43A4)

# load masked files for each land cover

# Land Cover 1
setwd("D:/R/MODIS2019/MCD43A4_LC1")

MCD43A4_lc1_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MCD43A4_lc1_list
MCD43A4_lc1 <- stack(MCD43A4_lc1_list)

# Land Cover 2
setwd("D:/R/MODIS2019/MCD43A4_LC2")

MCD43A4_lc2_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MCD43A4_lc2_list
MCD43A4_lc2 <- stack(MCD43A4_lc2_list)

# Land Cover 3
setwd("D:/R/MODIS2019/MCD43A4_LC3")

MCD43A4_lc3_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MCD43A4_lc3_list
MCD43A4_lc3 <- stack(MCD43A4_lc3_list)


##################
## scatter plot ##
##################


theme_set(theme_bw(base_size = 16))

# Get density of points in 2 dimensions.
# @param x A numeric vector.
# @param y A numeric vector.
# @param n Create a square n by n grid to compute density.
# @return The density within each square.
get_density <- function(x, y, n) {
  dens <- MASS::kde2d(x, y, n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

set.seed(1)

#for (i in 1:10){
  
  s <- stack(landsat[[1]], valid_43A4[[1]])
  v <- data.frame(na.omit(values(s)))
  
  names(v) <- c('Landsat', 'MOD43A4')
  #rmse <- rmse(v$Landsat, v$MOD43A4)
  v$density <- get_density(v$Landsat, v$MOD43A4, n=10000)
  p <- ggplot(v) + geom_point(aes(Landsat, MOD43A4, color = density)) + scale_color_viridis()

  print(p)
  dev.new()
  
  #print(DOY[i])

  
#}



