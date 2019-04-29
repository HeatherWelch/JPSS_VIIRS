 #### global maskign ####
 source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
 
 savedir="/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/"#;dir.create(savedir)
 rasterdir="/Users/heatherwelch/Dropbox/JPSS/global/global_rasters"
 
### changing the name of OC-CCI ####
 
 esa_files=list.files(rasterdir,pattern = "ESACCI",full.names = T)
 for (i in 1:length(esa_files)){
   name=esa_files[i]
   print(name)
   date=gsub("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/ESACCI-OC-L3S_","",name) %>% gsub(".grd","",.)%>% gsub(".gri","",.)
   date_new=as.Date(date,format = "%Y%m%d")
   if(grepl(".grd",name)){
   name_new=paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/ESACCI-OC-L3S_",date_new,".grd")
   }
   if(grepl(".gri",name)){
     name_new=paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters/ESACCI-OC-L3S_",date_new,".gri")
   }
   
   file.rename(from=name,to=name_new)
   
 }
 
 ### masking ####
 
 dates=seq(as.Date("2012-01-01"),as.Date("2018-12-31"),by="day",format="%Y-%m-%d")
 all_files=list.files(rasterdir,pattern = "grd",full.names = T)
 for(i in 1:length(dates)){
   d=dates[i]
   subset=grep(d,all_files,value = T)
   print(d)
   if(length(subset)==5){
     modis=grep("erdMH1chlamday",subset,value = T) %>% raster()
     viirs=grep("nesdisVHNSQchlaMonthly",subset,value = T) %>% raster()
     glo_gsm=grep("GlobColour_Merged_GSM",subset,value = T) %>% raster()
     glo_avw=grep("GlobColour_Merged_AVW",subset,value = T) %>% raster()
     esa=grep("ESACCI-OC-L3S",subset,value = T) %>% raster()
     
     viirs=raster::resample(viirs,modis)
     glo_gsm=raster::resample(glo_gsm,modis)
     glo_avw=raster::resample(glo_avw,modis)
     esa=raster::resample(esa,modis)
     
     writeRaster(modis, paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_resampled_to_MODIS/erdMH1chlamday_",d,".grd"),overwrite=T)
     writeRaster(viirs, paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_resampled_to_MODIS/nesdisVHNSQchlaMonthly_",d,".grd"),overwrite=T)
     writeRaster(glo_gsm, paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_resampled_to_MODIS/GlobColour_Merged_GSM_",d,".grd"),overwrite=T)
     writeRaster(glo_avw, paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_resampled_to_MODIS/GlobColour_Merged_AVW_",d,".grd"),overwrite=T)
     writeRaster(esa, paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_resampled_to_MODIS/ESACCI-OC-L3S_",d,".grd"),overwrite=T)
     
     master=stack(modis,viirs,glo_avw,glo_gsm,esa) %>% calc(.,fun=sum)
     
     modis=raster::mask(modis,master)
     viirs=raster::mask(viirs,master)
     glo_gsm=raster::mask(glo_gsm,master)
     glo_avw=raster::mask(glo_avw,master)
     esa=raster::mask(esa,master)
     
     writeRaster(modis, paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/erdMH1chlamday_",d,".grd"),overwrite=T)
     writeRaster(viirs, paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/nesdisVHNSQchlaMonthly_",d,".grd"),overwrite=T)
     writeRaster(glo_gsm, paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/GlobColour_Merged_GSM_",d,".grd"),overwrite=T)
     writeRaster(glo_avw, paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/GlobColour_Merged_AVW_",d,".grd"),overwrite=T)
     writeRaster(esa, paste0("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/ESACCI-OC-L3S_",d,".grd"),overwrite=T)
   }
   
   
     
     
   }
   

 