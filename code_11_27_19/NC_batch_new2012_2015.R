################ CM2.6.R adapted to batch all netcdfs
## adding in regional merged products for journal resubmission
library(chron)
library(sp)
library(rgdal)
library(raster)
library(ncdf4)
library(tidyverse)
library(glue)

template=raster() ##create template for resampling
res(template)=0.2487562
ncol(template)=201
nrow(template)=201
xmin(template)=-149.875
xmax(template)=-99.875
ymin(template)=10.125
ymax(template)=60.125
projection(template)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# create directories
AVWdir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/";dir.create(AVWdir)
GSMdir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/";dir.create(GSMdir)
OCdir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/";dir.create(OCdir)

#### globcolour_AVW 2015 - 2018; 8 day ####
netcdf_dir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_GSM_netcdf"
output_dir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite";dir.create(output_dir)

netcdf=list.files(netcdf_dir,pattern="*AVW-MODVIR_CHL1_DAY_00*",full.names = T) 
template_native=raster(netcdf[1])

for(nc in netcdf){
  a=raster(nc,var="CHL1_mean")
  r=raster::resample(a,template,field=n,fun=mean)
  r1=log(r+0.001)
  date=substr(nc,64,71) %>% as.Date(format="%Y%m%d")
  save_dir=glue("{output_dir}/{date}");dir.create(save_dir)
  print(save_dir)
  writeRaster(r1,glue("{save_dir}/l.blendChl"),overwrite=T)
}

#### globcolour_AVW 2015 - 2018; 8 day ####
netcdf_dir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_GSM_netcdf"
output_dir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite";dir.create(output_dir)

netcdf=list.files(netcdf_dir,pattern="*GSM-MODVIR_CHL1_DAY_00*",full.names = T) 
template_native=raster(netcdf[1])

for(nc in netcdf){
  a=raster(nc,var="CHL1_mean")
  r=raster::resample(a,template,field=n,fun=mean)
  r1=log(r+0.001)
  date=substr(nc,64,71) %>% as.Date(format="%Y%m%d")
  save_dir=glue("{output_dir}/{date}");dir.create(save_dir)
  print(save_dir)
  writeRaster(r1,glue("{save_dir}/l.blendChl"),overwrite=T)
}

#### oc-cci 2015 - 2018; 8 day ####
## !!!!! ignore all of this stuff, oc-cci has already been processed
## !!!!! nvm, doing it anyways as time-series isn't as long
## note, this is actually 8 day bin
netcdf_dir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_netcdf"
output_dir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite";dir.create(output_dir)

netcdf=list.files(netcdf_dir,pattern="*ESACCI-OC-L3S-CHLOR_A*",full.names = T)
template_native=raster(netcdf[1])

for(nc in netcdf){
  a=raster(nc,var="chlor_a")
  aa=crop(a,template)
  r=raster::resample(aa,template,field=n,fun=mean)
  r1=log(r+0.001)
  date=substr(nc,102,109) %>% as.Date(format="%Y%m%d")
  save_dir=glue("{output_dir}/{date}");dir.create(save_dir)
  print(save_dir)
  writeRaster(r1,glue("{save_dir}/l.blendChl"),overwrite=T)
}

