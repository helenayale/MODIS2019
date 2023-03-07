library(satellite)
library(RStoolbox)
library(gtools)
library(rgdal)
library(mapview)
library(raster)
library(stringr)
library(sp)

#install.packages("stringr")
#install.packages("gtools")
#install.packages("mapview")

setwd("M:/04-Phil/Fernerkundung3/Exchange/Dhillon-Singh_M/Haiyin/MOD13Q1_CKF_2019_proj")

CKFlist <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
CKFlist
CKF_origin  <- stack(CKFlist)


for (i in 1:length(CKFlist)){
  if (i == 1){
    CKF <- CKF_origin[i] * 0.0001
  }else{
    CKF <- stack(CKF, CKF_origin[i] * 0.0001)
  } 
  
}



setwd("M:/04-Phil/Fernerkundung3/Exchange/Dhillon-Singh_M/Haiyin/MOD13Q1_CKF_2019_div")

for (i in 1:length(CKFlist)){
  writeRaster(CKF[[i]], CKFlist[i], format = "GTiff", overwrite = TRUE)
}






#setwd("M:/04-Phil/Fernerkundung1/Landklif/11_DataFusion/R_code")
MRlist <- "M:/04-Phil/Fernerkundung3/Exchange/Dhillon-Singh_M/Haiyin/MOD13Q1_CKF_2019_div"
HRlist<- "M:/04-Phil/Fernerkundung3/Exchange/Dhillon-Singh_M/Haiyin/Landsat_crop_2"
MRlist_glob_name <- mixedsort(sort(list.files(MRlist,pattern=".tif$")))
HRlist_glob_name <- mixedsort(sort(list.files(HRlist,pattern=".tif$")))



HR_dates  <- gsub(".*_","",sub("(_[^_]+)_.*", "\\1", HRlist_glob_name))


HR_doys <- as.numeric(HR_dates)



MR_dates  <- gsub(".*_","",sub("(_[^_]+)_.*", "\\1", MRlist_glob_name))
MR_doys <- as.numeric(MR_dates)


#plot(b)

MRlist_glob <-  mixedsort(list.files(MRlist,pattern=".tif$", full.names=T))
HRlist_glob <- mixedsort(list.files(HRlist,pattern=".tif$", full.names=T))

match <- intersect(MR_doys,HR_doys)
MRlist_glob_match <- MRlist_glob[which(MR_doys %in% HR_doys)]
HRlist_glob_match <- HRlist_glob[which(HR_doys %in% MR_doys)]

matchList <- rbind(MRlist_glob_match, HRlist_glob_match)
setwd("D://imagefusion//bin")



MRoption <- c()
for (r in 1:length(MRlist_glob)){
  MRoption[r]=str_c("--img=(--file=",MRlist_glob[r]," --date=",MR_doys[r]," --tag=low)")
}
HRoption <- c()
for (t in 1:length(HRlist_glob)){
  HRoption[t]=str_c("--img=(--file=",HRlist_glob[t]," --date=",HR_doys[t]," --tag=high)")
}
W <- 25
Z <- 25

GLoption_starfm <- c()
GLoption_starfm[1] <- "#XXXXX options_starfm XXXX"
GLoption_starfm[2] <- str_c("--number-classes=",Z)
GLoption_starfm[3] <- str_c("--win-size=",W)
GLoption_starfm[4] <- str_c(str_c("--out-prefix=starfm"))
GLoption_starfm[5] <- str_c("--mask-valid-ranges=","[-1,1]")

MASTERoption_starfm <- c(MRoption,HRoption,GLoption_starfm)
writeLines(MASTERoption_starfm,"config_starfm.txt")
exe_output = system("starfm --option-file=config_starfm.txt",wait = T)

