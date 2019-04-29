### processing global data for quick summary figure ####
setwd("~/Dropbox/JPSS/global/raw_filezilla")
files1=list.files()%>% grep("_4_AVW",.,value=T)
files2=list.files()%>% grep("_4_GSM",.,value=T)
files3=list.files(recursive=T)%>% grep("ESACCI-OC-L3S",.,value=T)
files4=list.files("/Users/heatherwelch/Dropbox/JPSS/global/erddap",full.names =T)%>% grep("erdMH1chlamday",.,value=T)
files5=list.files("/Users/heatherwelch/Dropbox/JPSS/global/erddap",full.names =T)%>% grep("nesdisVHNSQchlaMonthly",.,value=T)
master=c(files1,files2,files3,files4,files5)

for (name in master){
  if(grepl("erdMH1chlamday", name)){
    sensor="erdMH1chlamday"
    date=substr(name,63,72) 
    saveName=paste0(sensor,"_",date)
    print(saveName)
    mean=raster(name,var="chlorophyll") 
    writeRaster(mean,paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/",saveName,".grd"),overwrite=T)
  }
  
  if(grepl("nesdisVHNSQchlaMonthly", name)){
    sensor="nesdisVHNSQchlaMonthly"
    date=substr(name,71,80) 
    saveName=paste0(sensor,"_",date)
    print(saveName)
    mean=raster(name,var="chlor_a")
    writeRaster(mean,paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/",saveName,".grd"),overwrite=T)
  }
  
  if(grepl("AVW", name)){
    sensor="GlobColour_Merged_AVW"
    date=substr(name,5,12) %>% as.Date(format="%Y%m%d")
    saveName=paste0(sensor,"_",date)
    print(saveName)
    mean=raster(name,var="CHL1_mean")
    writeRaster(mean,paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/",saveName,".grd"),overwrite=T)
  }
  
  if(grepl("GSM", name)){
    sensor="GlobColour_Merged_GSM"
    date=substr(name,5,12)%>% as.Date(format="%Y%m%d")
    saveName=paste0(sensor,"_",date)
    print(saveName)
    mean=raster(name,var="CHL1_mean")
    writeRaster(mean,paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/",saveName,".grd"),overwrite=T)
  }
  
  if(grepl("ESACCI-OC-L3S", name)){
    sensor="ESACCI-OC-L3S"
    date=substr(name,62,67) %>% paste0(.,"01")
    saveName=paste0(sensor,"_",date)
    print(saveName)
    mean=raster(name,var="chlor_a")
    writeRaster(mean,paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/",saveName,".grd"),overwrite=T)
  }
}


# temporal averages ####

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/",full.names = T)%>% grep("ESACCI-OC-L3S",.,value=T) %>% grep(".grd",.,value=T)
esa=stack(files) %>% calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/",full.names = T)%>% grep("GlobColour_Merged_GSM",.,value=T)%>% grep(".grd",.,value=T)
glo_gsm=stack(files)%>% calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/",full.names = T)%>% grep("GlobColour_Merged_AVW",.,value=T)%>% grep(".grd",.,value=T)
glo_avw=stack(files)%>% calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/",full.names = T)%>% grep("nesdisVHNSQchlaMonthly",.,value=T)%>% grep(".grd",.,value=T)
viirs=stack(files)%>% calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/",full.names = T)%>% grep("erdMH1chlamday",.,value=T)
modis=stack(files)%>% calc(.,fun=mean,na.rm=T)

# writeRaster(esa,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages/esa.grd")
writeRaster(glo_gsm,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages/glo_gsm.grd")
writeRaster(glo_avw,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages/glo_avw.grd")
writeRaster(viirs,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages/viirs.grd")
writeRaster(modis,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages/modis.grd")


# temporal averages masked ####

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/",full.names = T)%>% grep("ESACCI-OC-L3S",.,value=T) %>% grep(".grd",.,value=T)
esa=stack(files) %>% calc(.,fun=mean,na.rm=T)
writeRaster(esa,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/esa.grd")

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/",full.names = T)%>% grep("GlobColour_Merged_GSM",.,value=T)%>% grep(".grd",.,value=T)
glo_gsm=stack(files)%>% calc(.,fun=mean,na.rm=T)
writeRaster(glo_gsm,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/glo_gsm.grd")

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/",full.names = T)%>% grep("GlobColour_Merged_AVW",.,value=T)%>% grep(".grd",.,value=T)
glo_avw=stack(files)%>% calc(.,fun=mean,na.rm=T)
writeRaster(glo_avw,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/glo_avw.grd")

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/",full.names = T)%>% grep("nesdisVHNSQchlaMonthly",.,value=T)%>% grep(".grd",.,value=T)
viirs=stack(files)%>% calc(.,fun=mean,na.rm=T)
writeRaster(viirs,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/viirs.grd")

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/",full.names = T)%>% grep("erdMH1chlamday",.,value=T)
modis=stack(files)%>% calc(.,fun=mean,na.rm=T)
writeRaster(modis,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/modis.grd")







