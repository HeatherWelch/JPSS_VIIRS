#### code to turn daily Glob AVW and GSM into 8 day composites to the erddap files
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")

# for modis and viirs, will just just the 8 day files, which are timestamped to the 5th day as these will all be

## code where i was originally going to 8Day bins ####

avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_1day/Satellite"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_1day/Satellite"
pmlEsaDir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/Satellite"
avwCompDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8DayBin/Satellite";dir.create(avwCompDir)
gsmCompDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8DayBin/Satellite";dir.create(gsmCompDir)

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
    stackk=stackk %>% stack()%>% mean(.,na.rm=T)
    name=paste0(outdir,"/",get_date)
    if(!file.exists(name)){dir.create(name)}
    chla_name=paste0(name,"/l.blendChl.grd")
    #
    writeRaster(stackk,chla_name,overwrite=T)
  }
} #---> bins to match erddap

 lapply(dates,FUN = eight_day,outdir=avwCompDir,satDir=avwDir)
 lapply(dates,FUN = eight_day,outdir=gsmCompDir,satDir=gsmDir)


 ## code to go to 8Day rolling averages ####
 avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_1day/Satellite"
 gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_1day/Satellite"
 ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_1Day/Satellite"
 
 avwCompDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8Day";dir.create(avwCompDir)
 gsmCompDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8Day";dir.create(gsmCompDir)
 ociCompDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8Day";dir.create(ociCompDir)
 
 avwCompDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8Day/Satellite";dir.create(avwCompDir)
 gsmCompDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8Day/Satellite";dir.create(gsmCompDir)
 ociCompDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8Day/Satellite";dir.create(ociCompDir)
 
 path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
 staticdir=paste0(path,"/static_variables/")
 studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")
 
 a=seq(as.Date("2015-08-01"),as.Date("2016-01-01"),by=1) %>% as.character()
 b=seq(as.Date("2016-08-01"),as.Date("2017-01-01"),by=1)%>% as.character()
 c=seq(as.Date("2017-08-01"),as.Date("2018-01-01"),by=1)%>% as.character()
 d=seq(as.Date("2018-08-01"),as.Date("2019-01-01"),by=1)%>% as.character()
 
 dates=list(a,b,c,d) %>% unlist()
 
 eight_day=function(datess,outdir,satDir){
   get_date=as.Date(datess)
   print(get_date)
   SEQNCE=unlist(list(as.character(get_date-1),as.character(get_date-2),as.character(get_date-3),as.character(get_date-4),as.character(get_date),as.character(get_date+1),as.character(get_date+2),as.character(get_date+3)))
   stackk=lapply(SEQNCE,function(x)paste0(satDir,"/",x)) %>% unlist() %>% lapply(.,function(x)list.files(x,recursive=T,pattern=".grd",full.names=T)) %>% unlist()
   if(length(stackk)!=0 &length(stackk)!=1){
     stackk=stackk %>% stack()%>% mean(.,na.rm=T)
     name=paste0(outdir,"/",get_date)
     if(!file.exists(name)){dir.create(name)}
     chla_name=paste0(name,"/l.blendChl.grd")
     #
     writeRaster(stackk,chla_name,overwrite=T)
   }
 } #---> bins to match erddap
 
 lapply(dates,FUN = eight_day,outdir=avwCompDir,satDir=avwDir)
 lapply(dates,FUN = eight_day,outdir=gsmCompDir,satDir=gsmDir)
 lapply(dates,FUN = eight_day,outdir=ociCompDir,satDir=ociDir)
 