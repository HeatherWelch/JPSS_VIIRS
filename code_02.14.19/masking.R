#### masking
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

#####chla #####

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
modisDir_C="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask"#;dir.create(modisDir_C)
viirsDir_C="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"#;dir.create(viirsDir_C)

dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
to_match_date=intersect(dates_m,dates_v)

for(date in to_match_date){
  print(date)
  modis=paste0(modisDir,"/",date,"/l.blendChl.grd") %>% raster()%>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  viirs=paste0(viirsDir,"/",date,"/l.blendChl.grd") %>% raster()%>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  modis_c=mask(modis,viirs)
  viirs_c=mask(viirs,modis)
  mfolder=paste0(modisDir_C,"/",date)
  if(!file.exists(mfolder)){dir.create(mfolder)}
  vfolder=paste0(viirsDir_C,"/",date)
  if(!file.exists(vfolder)){dir.create(vfolder)}
  m_name=paste0(mfolder,"/l.blendChl.grd")
  if(!file.exists(m_name)){writeRaster(modis_c,m_name)}
  v_name=paste0(vfolder,"/l.blendChl.grd")
  if(!file.exists(v_name)){writeRaster(viirs_c,v_name)}
}


##### lbst #####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs"
modisDir_C="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs_mask"#;dir.create(modisDir_C)
viirsDir_C="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs_mask"#;dir.create(viirsDir_C)

dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs/lbst_pa_","",.) %>% gsub("_mean.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs/lbst_pa_","",.) %>% gsub("_mean.grd","",.)
to_match_date=intersect(dates_m,dates_v)

for(date in to_match_date){
  print(date)
  modis=paste0(modisDir,"/lbst_pa_",date,"_mean.grd") %>% raster()%>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  viirs=paste0(viirsDir,"/lbst_pa_",date,"_mean.grd") %>% raster()%>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  maskR=paste0("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask","/",date,"/l.blendChl.grd") %>% raster()
  viirs_c=mask(viirs,maskR)
  modis_c=mask(modis,maskR)

  m_name=paste0(modisDir_C,"/lbst_pa_",date,"_mean.grd")
  if(!file.exists(m_name)){writeRaster(modis_c,m_name)}
  v_name=paste0(viirsDir_C,"/lbst_pa_",date,"_mean.grd")
  if(!file.exists(v_name)){writeRaster(viirs_c,v_name)}
}

##### ecocast #####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"
modisDir_C="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean_mask"#;dir.create(modisDir_C)
viirsDir_C="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean_mask"#;dir.create(viirsDir_C)

dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
to_match_date=intersect(dates_m,dates_v)

for(date in to_match_date){
  print(date)
  modis=paste0(modisDir,"/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_",date,"_mean.grd") %>% raster()%>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  viirs=paste0(viirsDir,"/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_",date,"_mean.grd") %>% raster()%>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  maskR=paste0("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask","/",date,"/l.blendChl.grd") %>% raster()
  viirs_c=mask(viirs,maskR)
  modis_c=mask(modis,maskR)
  
  m_name=paste0(modisDir_C,"/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_",date,"_mean.grd")
  if(!file.exists(m_name)){writeRaster(modis_c,m_name)}
  v_name=paste0(viirsDir_C,"/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_",date,"_mean.grd")
  if(!file.exists(v_name)){writeRaster(viirs_c,v_name)}
}


##### blshobs #####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshObs/predCIs"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshObs/predCIs"
modisDir_C="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshObs/predCIs_mask"#;dir.create(modisDir_C)
viirsDir_C="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshObs/predCIs_mask"#;dir.create(viirsDir_C)

dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshObs/predCIs/blshObs_pa_","",.) %>% gsub("_mean.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshObs/predCIs/blshObs_pa_","",.) %>% gsub("_mean.grd","",.)
to_match_date=intersect(dates_m,dates_v)

for(date in to_match_date){
  print(date)
  modis=paste0(modisDir,"/blshObs_pa_",date,"_mean.grd") %>% raster()%>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  viirs=paste0(viirsDir,"/blshObs_pa_",date,"_mean.grd") %>% raster()%>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  maskR=paste0("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask","/",date,"/l.blendChl.grd") %>% raster()
  viirs_c=mask(viirs,maskR)
  modis_c=mask(modis,maskR)
  
  m_name=paste0(modisDir_C,"/blshObs_pa_",date,"_mean.grd")
  if(!file.exists(m_name)){writeRaster(modis_c,m_name)}
  v_name=paste0(viirsDir_C,"/blshObs_pa_",date,"_mean.grd")
  if(!file.exists(v_name)){writeRaster(viirs_c,v_name)}
}
