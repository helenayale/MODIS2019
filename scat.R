library(sp)
library(rgdal)
library(raster)
library(gtools)

library(MASS)
library(ggplot2)
library(viridis)



# load data
setwd("D:/R/MODIS2019/")

## landsat data, which is the real data (after preprocessing)
landsat <- raster('DOY_049_2019_BAY_NDVI_Landsat_epsg25832.tif')

## Starfm data, which is the predicted data derived from the data fusion algorithm
## here the input data are MOD43A4 & Landsat
valid_43A4 <-  raster('starfm49_from_81.tif')



## the density function

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

## stack the real data & predicted data
## and put them into a dataframe

s <- stack(landsat, valid_43A4)
v <- data.frame(na.omit(values(s)))
names(v) <- c('Landsat', 'MOD43A4')

## get density of the points
## the number of n could be adjusted
## but I can't, because I don't even get the plot

v$density <- get_density(v$Landsat, v$MOD43A4, n=100)


## ggplot the points, which are colored according to density

p <- ggplot(v) + geom_point(aes(Landsat, MOD43A4, color = density)) + scale_color_viridis()
p

