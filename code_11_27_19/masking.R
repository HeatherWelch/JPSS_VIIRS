## masking satellite data to modis NAs

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite"
ocDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite"

modisDirOut="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask_resub";dir.create(modisDirOut)
viirsDirOut="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask_resub";dir.create(viirsDirOut)
avwDirOut="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite_mask_resub";dir.create(avwDirOut)
gsmDirOut="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite_mask_resub";dir.create(gsmDirOut)
ocDirOut="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite_mask_resub";dir.create(ocDirOut)

datesoci=list.files(ocDir) # doing this one because its 8 day binned, while the rest are 8 day rolling averages
datesmodis=list.files(modisDir) # and this one because its only fishing seasons
dates=intersect(datesoci,datesmodis)
# date=dates[1]

for(date in dates[59:length(dates)]){
    print(date)
  ociras=raster(glue("{ocDir}/{date}/l.blendChl.grd"))
  modisras=raster(glue("{modisDir}/{date}/l.blendChl.grd"))
  viirsras=raster(glue("{viirsDir}/{date}/l.blendChl.grd"))
  avwras=raster(glue("{avwDir}/{date}/l.blendChl.grd"))
  gsmras=raster(glue("{gsmDir}/{date}/l.blendChl.grd"))
  
  master=stack(ociras,modisras,viirsras,avwras,gsmras) %>% calc(.,fun=sum)
  
  modis=raster::mask(modisras,master)
  viirs=raster::mask(viirsras,master)
  glo_gsm=raster::mask(gsmras,master)
  glo_avw=raster::mask(avwras,master)
  oci=raster::mask(ociras,master)
  
  modisDirOutOut=glue("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask_resub/{date}");dir.create(modisDirOutOut)
  viirsDirOutOut=glue("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask_resub/{date}");dir.create(viirsDirOutOut)
  avwDirOutOut=glue("/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite_mask_resub/{date}");dir.create(avwDirOutOut)
  gsmDirOutOut=glue("/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite_mask_resub/{date}");dir.create(gsmDirOutOut)
  ocDirOutOut=glue("/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite_mask_resub/{date}");dir.create(ocDirOutOut)
  
  print(modisDirOutOut)
  
  writeRaster(modis, glue("{modisDirOutOut}/l.blendChl.grd"),overwrite=T)
  writeRaster(viirs, glue("{viirsDirOutOut}/l.blendChl.grd"),overwrite=T)
  writeRaster(glo_gsm, glue("{gsmDirOutOut}/l.blendChl.grd"),overwrite=T)
  writeRaster(glo_avw, glue("{avwDirOutOut}/l.blendChl.grd"),overwrite=T)
  writeRaster(oci, glue("{ocDirOutOut}/l.blendChl.grd"),overwrite=T)
  
}
  