#### code to turn daily MODIS and VIIRS into 8 day composites to match the blended products

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
pmlEsaDir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/Satellite"
modisCompDir="/Users/heatherwelch/Dropbox/JPSS/modis_8DayBin/Satellite"#;dir.create(modisCompDir)
viirsCompDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8DayBin/Satellite"#;dir.create(viirsCompDir)

modisDirEco="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
viirsDirEco="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"
nochlaDirEco="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/output/mean"
modisCompDirEco="/Users/heatherwelch/Dropbox/JPSS/modis_8DayBin/EcoCastRuns/output/mean"#;dir.create(modisCompDir)
viirsCompDirEco="/Users/heatherwelch/Dropbox/JPSS/viirs_8DayBin/EcoCastRuns/output/mean"#;dir.create(viirsCompDir)
nochlaCompDirEco="/Users/heatherwelch/Dropbox/JPSS/no_chlaBin/EcoCastRuns/output/mean"

modisDirLBST="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs"
viirsDirLBST="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs"
nochlaDirLBST="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/lbst/predCIs"
modisCompDirLBST="/Users/heatherwelch/Dropbox/JPSS/modis_8DayBin/EcoCastRuns/lbst/predCIs"#;dir.create(modisCompDir)
viirsCompDirLBST="/Users/heatherwelch/Dropbox/JPSS/viirs_8DayBin/EcoCastRuns/lbst/predCIs"#;dir.create(viirsCompDir)
nochlCompDirLBST="/Users/heatherwelch/Dropbox/JPSS/no_chlaBin/EcoCastRuns/lbst/predCIs"#;dir.create(viirsCompDir)

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

dates=list.files(pmlEsaDir) %>% grep("-07-",.,invert=T,value=T)


eight_day=function(datess,outdir,satDir){
  get_date=as.Date(datess)
  print(get_date)
  SEQNCE=unlist(list(as.character(get_date-1),as.character(get_date-2),as.character(get_date-3),as.character(get_date-4),as.character(get_date),as.character(get_date+1),as.character(get_date+2),as.character(get_date+3)))
  stackk=lapply(SEQNCE,function(x)paste0(satDir,"/",x)) %>% unlist() %>% lapply(.,function(x)list.files(x,recursive=T,pattern=".grd",full.names=T)) %>% unlist() 
  if(length(stackk)!=0 &length(stackk)!=1){
    stackk=stackk %>% stack()%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
    name=paste0(outdir,"/",get_date)
    if(!file.exists(name)){dir.create(name)}
    chla_name=paste0(name,"/l.blendChl.grd")
    if(!file.exists(chla_name)){writeRaster(stackk,chla_name)}
  }
}

 lapply(dates,FUN = eight_day,outdir=modisCompDir,satDir=modisDir)
 lapply(dates,FUN = eight_day,outdir=viirsCompDir,satDir=viirsDir)

 
 eight_day_ecocast=function(datess,outdir,satDir){
   get_date=as.Date(datess)
   print(get_date)
   SEQNCE=unlist(list(as.character(get_date-1),as.character(get_date-2),as.character(get_date-3),as.character(get_date-4),as.character(get_date),as.character(get_date+1),as.character(get_date+2),as.character(get_date+3)))
   stackk <- unique (grep(paste(SEQNCE,collapse="|"), 
                          list.files(satDir,pattern = "_mean.grd",full.names = T), value=TRUE))
   if(length(stackk)!=0 &length(stackk)!=1){
     stackk=stackk %>% stack()%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
     name=paste0(outdir,"/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_",get_date,"_mean.grd")
     if(!file.exists(name)){writeRaster(stackk,name)}
   }
 }
 
 
 lapply(dates,FUN = eight_day_ecocast,outdir=modisCompDirEco,satDir=modisDirEco)
 lapply(dates,FUN = eight_day_ecocast,outdir=viirsCompDirEco,satDir=viirsDirEco)
 lapply(dates,FUN = eight_day_ecocast,outdir=nochlaCompDirEco,satDir=nochlaDirEco)


 eight_day_lbst=function(datess,outdir,satDir){
   get_date=as.Date(datess)
   print(get_date)
   SEQNCE=unlist(list(as.character(get_date-1),as.character(get_date-2),as.character(get_date-3),as.character(get_date-4),as.character(get_date),as.character(get_date+1),as.character(get_date+2),as.character(get_date+3)))
   stackk <- unique (grep(paste(SEQNCE,collapse="|"), 
                          list.files(satDir,pattern = "_mean.grd",full.names = T), value=TRUE))
   if(length(stackk)!=0 &length(stackk)!=1){
     stackk=stackk %>% stack()%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
     name=paste0(outdir,"/lbst_pa_",get_date,"_mean.grd")
     if(!file.exists(name)){writeRaster(stackk,name)}
   }
 }
 
 
 lapply(dates,FUN = eight_day_lbst,outdir=modisCompDirLBST,satDir=modisDirLBST)
 lapply(dates,FUN = eight_day_lbst,outdir=viirsCompDirLBST,satDir=viirsDirLBST)
 lapply(dates,FUN = eight_day_lbst,outdir=nochlCompDirLBST,satDir=nochlaDirLBST)
