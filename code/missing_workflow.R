### script to check what's missing, and downlaod from erddap
library(tidyverse)
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/dowload_netcdfs_function.R")
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/NC_batch_function.R")
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_netcdf"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_netcdf"

a<-seq(as.Date("2015-08-01"), as.Date("2016-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
b<-seq(as.Date("2016-08-01"), as.Date("2017-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
c<-seq(as.Date("2017-08-01"), as.Date("2018-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
d<-seq(as.Date("2018-08-01"), as.Date("2019-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
x=list(a,b,c,d) %>% unlist()

## modis ####
## download ####
modis_present=list.files("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite")
test=setdiff(x,modis_present) 
dates=test[1:4]
sucess=list()
bb=lapply(dates,FUN = modis,modisDir=modisDir,sucess=sucess) %>% unlist

## unpack ####
output_dir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
netcdf=list.files(modisDir,pattern="*erdMBchla8day_LonPM180*",full.names = T) 
matches <- unique (grep(paste(bb,collapse="|"), 
                          netcdf, value=TRUE))
template_native=raster(netcdf[1])
lapply(matches,FUN = modis_unpack,template_native=template_native,output_dir=output_dir,template=template)

