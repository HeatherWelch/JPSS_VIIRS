## masking satellite data to modis NAs

### code from 12.01.19, this is wrong because we had 8 day rolling averages, bins, and 1 days all with different time stamps ####

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirsNASA_8Day/Satellite"

dates=Reduce(intersect, list(list.files(modisDir),list.files(viirsDir)))

modisDirOut="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask_resub_cara";dir.create(modisDirOut)
viirsDirOut="/Users/heatherwelch/Dropbox/JPSS/viirsNASA_8Day/Satellite_mask_resub_cara";dir.create(viirsDirOut)

for(date in dates){
  print(date)
  modisras=raster(glue("{modisDir}/{date}/l.blendChl.grd"))
  viirsras=raster(glue("{viirsDir}/{date}/l.blendChl.grd"))
  
  master=stack(modisras,viirsras) %>% calc(.,fun=sum)
  
  modis=raster::mask(modisras,master)
  viirs=raster::mask(viirsras,master)

  
  modisDirOutOut=glue("{modisDirOut}/{date}");dir.create(modisDirOutOut)
  viirsDirOutOut=glue("{viirsDirOut}/{date}");dir.create(viirsDirOutOut)
  
  print(modisDirOutOut)
  
  writeRaster(modis, glue("{modisDirOutOut}/l.blendChl.grd"),overwrite=T)
  writeRaster(viirs, glue("{viirsDirOutOut}/l.blendChl.grd"),overwrite=T)
  
}
  
