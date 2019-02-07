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
dates=test
sucess=list()
bb=lapply(dates,FUN = modis,modisDir=modisDir,sucess=sucess) %>% unlist
#bb=lapply(dates,FUN = modis_window,modisDir=modisDir,sucess=sucess) %>% unlist ## testing to see if we can grab missing better with a window

## unpack ####
output_dir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
netcdf=list.files(modisDir,pattern="*erdMBchla8day_LonPM180*",full.names = T) 
matches <- unique (grep(paste(bb,collapse="|"), 
                          netcdf, value=TRUE))
template_native=raster(netcdf[1])
lapply(matches,FUN = modis_unpack,template_native=template_native,output_dir=output_dir,template=template)




## viirs ####
## see what netcdfs are downloaded but not unpacked ####
viirs_present_satellite=list.files("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite")
viirs_present_netcdf=list.files("/Users/heatherwelch/Dropbox/JPSS/viirs_netcdf") %>% gsub("erdVHNchla8day_","",.)%>% gsub(".nc","",.)%>% gsub("erdVHNchla8day_","",.) %>% grep("erdVHNchla1day",.,value=T,invert=T)
viirs_present_netcdf=lapply(viirs_present_netcdf,function(x)
  if(nchar(x)>11){strtrim(x,10)}
  else{strtrim(x,10)}) %>% unlist()
test=setdiff(viirs_present_netcdf,viirs_present_satellite)
test=setdiff(x,viirs_present_netcdf)

## unpack 
output_dir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
netcdf=list.files(viirsDir,pattern="*erdVHNchla8day_*",full.names = T) 
matches <- unique (grep(paste(test,collapse="|"), 
                        netcdf, value=TRUE))
template_native=raster(netcdf[1])
lapply(matches,FUN = viirs_unpack,template_native=template_native,output_dir=output_dir,template=template)

# for(nc in matches){
#   viirs_unpack(template_native = template_native,nc=nc,output_dir = output_dir,template = template)
# }

## download based on missing netcdfs ####
test=setdiff(x,viirs_present_netcdf)
dates=test
sucess=list()
bb=lapply(dates,FUN = viirs,viirsDir=viirsDir,sucess=sucess) %>% unlist

## unpack ####
output_dir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
netcdf=list.files(viirsDir,pattern="*erdVHNchla8day_*",full.names = T) 
matches <- unique (grep(paste(bb,collapse="|"), 
                        netcdf, value=TRUE))
template_native=raster(netcdf[1])
lapply(matches,FUN = viirs_unpack,template_native=template_native,output_dir=output_dir,template=template)

## download based on missing satellite folders ####
test=setdiff(x,viirs_present_satellite)
dates=test
sucess=list()
bb=lapply(dates,FUN = viirs,viirsDir=viirsDir,sucess=sucess) %>% unlist

## unpack ####
output_dir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
netcdf=list.files(viirsDir,pattern="*erdVHNchla8day_*",full.names = T) 
matches <- unique (grep(paste(bb,collapse="|"), 
                        netcdf, value=TRUE))
template_native=raster(netcdf[1])
lapply(matches,FUN = viirs_unpack,template_native=template_native,output_dir=output_dir,template=template)



