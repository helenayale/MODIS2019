library(sp)
library(rgdal)
library(raster)
library(gtools)
library(dplyr)
library(mapsRinteractive)

library(MASS)
library(ggplot2)
library(viridis)
library(hexbin)


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


## density function ##

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

## plot MCD43A4


for (i in 1:10){
  
  s <- stack(landsat[[i]], valid_43A4[[i]])
  v <- data.frame(na.omit(values(s)))
  names(v) <- c('Landsat', 'MCD43A4')
  
  #random_v <- v[sample(nrow(v),500000),]
  #v$density <- get_density(v$Landsat, v$MCD43A4, n=1000)
  #random_v$density <-  get_density(random_v$Landsat, random_v$MCD43A4, n=5)
  
  # RMSE
  rmse <- rmse(v$Landsat, v$MCD43A4)
  # Linear Regression
  m <- lm(Landsat ~ MCD43A4, data=v)
  
  r_sq <- round(summary(m)$r.squared,3)
  
  title <- toString(DOY[i])
  subtitle <- paste("= ", toString(r_sq), "  ", "RMSE = ", toString(round(rmse,3)),sep = "")
  
  
  p <- ggplot(v, aes(Landsat, MCD43A4)) + 
    geom_hex(bins = 70) +
    scale_fill_continuous(type = "viridis") +
    theme_bw() +
    geom_abline(intercept=0, slope=1, color = "red") + 
    labs(title = title, subtitle = parse(text=paste("R^2~'",subtitle,"'",sep=''))) +
    xlab("StarFM NDVI") + ylab("Landsat NDVI") + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
  p
  print(p)
  
  #dev.new()
  #dev.off()
  #print(DOY[i])
  
}





###############################
############ 09GQ  ############
###############################

# load validation files 09GQ
setwd("D:/HYe/DataFusion/Validation_09GQ")

valid_list_09GQ <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
valid_list_09GQ
valid_09GQ <- stack(valid_list_09GQ)

# load masked files for each land cover

# Land Cover 1
setwd("D:/R/MODIS2019/MOD09GQ_LC1")

MOD09GQ_lc1_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD09GQ_lc1_list
MOD09GQ_lc1 <- stack(MOD09GQ_lc1_list)

# Land Cover 2
setwd("D:/R/MODIS2019/MOD09GQ_LC2")

MOD09GQ_lc2_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD09GQ_lc2_list
MOD09GQ_lc2 <- stack(MOD09GQ_lc2_list)

# Land Cover 3
setwd("D:/R/MODIS2019/MOD09GQ_LC3")

MOD09GQ_lc3_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD09GQ_lc3_list
MOD09GQ_lc3 <- stack(MOD09GQ_lc3_list)


##################
## scatter plot ##
##################


## plot MOD09GQ

for (i in 1:10){
  
  s <- stack(landsat[[i]], valid_09GQ[[i]])
  v <- data.frame(na.omit(values(s)))
  names(v) <- c('Landsat', 'MOD09GQ')
  
  #random_v <- v[sample(nrow(v),500000),]
  #v$density <- get_density(v$Landsat, v$MOD09GQ, n=1000)
  #random_v$density <-  get_density(random_v$Landsat, random_v$MOD09GQ, n=5)
  
  # RMSE
  rmse <- rmse(v$Landsat, v$MOD09GQ)
  # Linear Regression
  m <- lm(Landsat ~ MOD09GQ, data=v)
  
  r_sq <- round(summary(m)$r.squared,3)
  
  title <- toString(DOY[i])
  subtitle <- paste("= ", toString(r_sq), "  ", "RMSE = ", toString(round(rmse,3)),sep = "")
  
  
  p <- ggplot(v, aes(Landsat, MOD09GQ)) + 
    geom_hex(bins = 70) +
    scale_fill_continuous(type = "viridis") +
    theme_bw() +
    geom_abline(intercept=0, slope=1, color = "red") + 
    labs(title = title, subtitle = parse(text=paste("R^2~'",subtitle,"'",sep=''))) +
    xlab("StarFM NDVI") + ylab("Landsat NDVI") + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
  p
  print(p)
  
  #dev.new()
  #dev.off()
  #print(DOY[i])
  
}




###############################
############ 09Q1  ############
###############################

# load validation files 09Q1
setwd("D:/HYe/DataFusion/Validation_09Q1")

valid_list_09Q1 <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
valid_list_09Q1
valid_09Q1 <- stack(valid_list_09Q1)

# load masked files for each land cover

# Land Cover 1
setwd("D:/R/MODIS2019/MOD09Q1_LC1")

MOD09Q1_lc1_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD09Q1_lc1_list
MOD09Q1_lc1 <- stack(MOD09Q1_lc1_list)

# Land Cover 2
setwd("D:/R/MODIS2019/MOD09Q1_LC2")

MOD09Q1_lc2_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD09Q1_lc2_list
MOD09Q1_lc2 <- stack(MOD09Q1_lc2_list)

# Land Cover 3
setwd("D:/R/MODIS2019/MOD09Q1_LC3")

MOD09Q1_lc3_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD09Q1_lc3_list
MOD09Q1_lc3 <- stack(MOD09Q1_lc3_list)


##################
## scatter plot ##
##################


## plot MOD09Q1

for (i in 1:10){
  
  s <- stack(landsat[[i]], valid_09Q1[[i]])
  v <- data.frame(na.omit(values(s)))
  names(v) <- c('Landsat', 'MOD09Q1')
  
  #random_v <- v[sample(nrow(v),500000),]
  #v$density <- get_density(v$Landsat, v$MOD09Q1, n=1000)
  #random_v$density <-  get_density(random_v$Landsat, random_v$MOD09Q1, n=5)
  
  # RMSE
  rmse <- rmse(v$Landsat, v$MOD09Q1)
  # Linear Regression
  m <- lm(Landsat ~ MOD09Q1, data=v)
  
  r_sq <- round(summary(m)$r.squared,3)
  
  title <- toString(DOY[i])
  subtitle <- paste("= ", toString(r_sq), "  ", "RMSE = ", toString(round(rmse,3)),sep = "")
  
  
  p <- ggplot(v, aes(Landsat, MOD09Q1)) + 
    geom_hex(bins = 70) +
    scale_fill_continuous(type = "viridis") +
    theme_bw() +
    geom_abline(intercept=0, slope=1, color = "red") + 
    labs(title = title, subtitle = parse(text=paste("R^2~'",subtitle,"'",sep=''))) +
    xlab("StarFM NDVI") + ylab("Landsat NDVI") + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
  p
  print(p)
  
  #dev.new()
  #dev.off()
  #print(DOY[i])
  
}


###############################
############ 13Q1  ############
###############################

# load validation files 13Q1
setwd("D:/HYe/DataFusion/Validation_13Q1")

valid_list_13Q1 <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
valid_list_13Q1
valid_13Q1 <- stack(valid_list_13Q1)

# load masked files for each land cover

# Land Cover 1
setwd("D:/R/MODIS2019/MOD13Q1_LC1")

MOD13Q1_lc1_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD13Q1_lc1_list
MOD13Q1_lc1 <- stack(MOD13Q1_lc1_list)

# Land Cover 2
setwd("D:/R/MODIS2019/MOD13Q1_LC2")

MOD13Q1_lc2_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD13Q1_lc2_list
MOD13Q1_lc2 <- stack(MOD13Q1_lc2_list)

# Land Cover 3
setwd("D:/R/MODIS2019/MOD13Q1_LC3")

MOD13Q1_lc3_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD13Q1_lc3_list
MOD13Q1_lc3 <- stack(MOD13Q1_lc3_list)


##################
## scatter plot ##
##################


## plot MOD13Q1

for (i in 1:10){
  
  s <- stack(landsat[[i]], valid_13Q1[[i]])
  v <- data.frame(na.omit(values(s)))
  names(v) <- c('Landsat', 'MOD13Q1')
  
  #random_v <- v[sample(nrow(v),500000),]
  #v$density <- get_density(v$Landsat, v$MOD13Q1, n=1000)
  #random_v$density <-  get_density(random_v$Landsat, random_v$MOD13Q1, n=5)
  
  # RMSE
  rmse <- rmse(v$Landsat, v$MOD13Q1)
  # Linear Regression
  m <- lm(Landsat ~ MOD13Q1, data=v)
  
  r_sq <- round(summary(m)$r.squared,3)
  
  title <- toString(DOY[i])
  subtitle <- paste("= ", toString(r_sq), "  ", "RMSE = ", toString(round(rmse,3)),sep = "")
  
  
  p <- ggplot(v, aes(Landsat, MOD13Q1)) + 
    geom_hex(bins = 70) +
    scale_fill_continuous(type = "viridis") +
    theme_bw() +
    geom_abline(intercept=0, slope=1, color = "red") + 
    labs(title = title, subtitle = parse(text=paste("R^2~'",subtitle,"'",sep=''))) +
    xlab("StarFM NDVI") + ylab("Landsat NDVI") + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
  p
  print(p)
  
  #dev.new()
  #dev.off()
  #print(DOY[i])
  
}

###############################
############ CKF  ############
###############################

# load validation files 13Q1_CKF
setwd("D:/HYe/DataFusion/Validation_CKF")

valid_list_13Q1_CKF <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
valid_list_13Q1_CKF
valid_13Q1_CKF <- stack(valid_list_13Q1_CKF)

# load masked files for each land cover

# Land Cover 1
setwd("D:/R/MODIS2019/MOD13Q1_CKF_LC1")

MOD13Q1_CKF_lc1_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD13Q1_CKF_lc1_list
MOD13Q1_CKF_lc1 <- stack(MOD13Q1_CKF_lc1_list)

# Land Cover 2
setwd("D:/R/MODIS2019/MOD13Q1_CKF_LC2")

MOD13Q1_CKF_lc2_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD13Q1_CKF_lc2_list
MOD13Q1_CKF_lc2 <- stack(MOD13Q1_CKF_lc2_list)

# Land Cover 3
setwd("D:/R/MODIS2019/MOD13Q1_CKF_LC3")

MOD13Q1_CKF_lc3_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
MOD13Q1_CKF_lc3_list
MOD13Q1_CKF_lc3 <- stack(MOD13Q1_CKF_lc3_list)


##################
## scatter plot ##
##################


## plot MOD13Q1_CKF

for (i in 1:10){
  
  s <- stack(landsat[[i]], valid_13Q1_CKF[[i]])
  v <- data.frame(na.omit(values(s)))
  names(v) <- c('Landsat', 'MOD13Q1_CKF')
  
  #random_v <- v[sample(nrow(v),500000),]
  #v$density <- get_density(v$Landsat, v$MOD13Q1_CKF, n=1000)
  #random_v$density <-  get_density(random_v$Landsat, random_v$MOD13Q1_CKF, n=5)
  
  # RMSE
  rmse <- rmse(v$Landsat, v$MOD13Q1_CKF)
  # Linear Regression
  m <- lm(Landsat ~ MOD13Q1_CKF, data=v)
  
  r_sq <- round(summary(m)$r.squared,3)
  
  title <- toString(DOY[i])
  subtitle <- paste("= ", toString(r_sq), "  ", "RMSE = ", toString(round(rmse,3)),sep = "")
  
  
  p <- ggplot(v, aes(Landsat, MOD13Q1_CKF)) + 
    geom_hex(bins = 70) +
    scale_fill_continuous(type = "viridis") +
    theme_bw() +
    geom_abline(intercept=0, slope=1, color = "red") + 
    labs(title = title, subtitle = parse(text=paste("R^2~'",subtitle,"'",sep=''))) +
    xlab("StarFM NDVI") + ylab("Landsat NDVI") + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
  p
  print(p)
  
  #dev.new()
  #dev.off()
  #print(DOY[i])
  
}

