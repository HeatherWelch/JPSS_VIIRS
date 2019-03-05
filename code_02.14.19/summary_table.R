### summary table
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)
library(scales)
library(sdmvspecies)

### mean chla influence ----------------------------------------------------> ####
path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
moddir<-paste(path,"/ModRepFiles/",sep="")
lbst=paste0(moddir,"brt_lbst_CIs.rds") %>% read_rds()
blshT=paste0(moddir,"brt_blshTr_CIs.rds")%>% read_rds()
blshO=paste0(moddir,"brt_blshObs_CIs.rds")%>% read_rds()
casl=paste0(moddir,"brt_casl_CIs.rds")%>% read_rds()
swor=paste0(moddir,"brt_swor_CIs.rds")%>% read_rds()

a=summary(casl[[3]])%>% mutate(species=names[i]) %>% mutate(mod_num=i)

models=list(lbst,blshT,blshO,casl,swor)
names=c("lbst","blshT","blshO","casl","swor")

empty=list()

for(i in 1:length(models)){
  print(i)
  print(names[i])
  mod=models[[i]]
  for(ii in 1:10){
    b=mod[[ii]] %>% summary()
    b=b %>% mutate(species=names[i]) %>% mutate(mod_num=ii)
    #empty=list(empty,b) %>% unlist()
    empty[[(i*10)+ii]]=b 
  }
  
}
a=do.call("rbind",empty)
b=a %>% mutate(var=gsub("_mean","",var))%>% mutate(var=gsub("windy_new","ywind",var))%>% mutate(var=gsub("log_eke","l.eke",var))%>% mutate(var=gsub("analysed_","",var))%>% mutate(var=gsub("logChl","l.blendChl",var))
as.factor(b$var) %>% unique()

mean_influence=b %>% group_by(species,var) %>% summarise(mean=mean(rel.inf,na.rm=T)) %>% filter(var=="l.blendChl")


### mean response curve variability ----------------------------------------------------> ####
empty=list()

for(i in 1:length(models)){
  print(i)
  print(names[i])
  mod=models[[i]]
  for(ii in 1:10){
    b=mod[[ii]] %>% summary()
    if("l.blendChl" %in% b$var){
      b=plot.gbm(mod[[ii]],i.var = "l.blendChl",return.grid = T)%>% mutate(species=names[i]) %>% mutate(mod_num=ii) %>% dplyr::rename(logChl=l.blendChl)
    }
    if("logChl" %in% b$var){
      b=plot.gbm(mod[[ii]],i.var = "logChl",return.grid = T)%>% mutate(species=names[i]) %>% mutate(mod_num=ii)
    }
    empty[[(i*10)+ii]]=b
    print((i*10)+ii)
  }
  
}
a=do.call("rbind",empty)

c=a %>% mutate(id=rep(1:100,50)) %>% group_by(species,id) %>% summarise(minx=min(y))
cc=a %>% mutate(id=rep(1:100,50)) %>% group_by(species,id) %>% summarise(maxy=max(y))
cc$minx=c$minx
cc$diff=abs(cc$maxy-cc$minx)
curve_var=cc %>% group_by(species) %>% summarise(mean=mean(diff))

### mean chla difference ----------------------------------------------------> ####
path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

#### lbst ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs_mask"
noCHLADir="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/lbst/predCIs_mask"

#1. time series of spatial average, just mean 
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(modisDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/lbst_pa_","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(viirsDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/lbst_pa_","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df)) %>% mutate(chla=scales::rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
a=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% .[complete.cases(.),]
lbst=a %>% gather(sensor,chla,-c(date,year,month,MODIS,VIIRS)) 
#LBST=left_join(x,LBST)

#### blshobs ####

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshObs/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshObs/predCIs_mask"
noCHLADir="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/blshObs/predCIs_mask"

#1. time series of spatial average, just mean 
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(modisDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/blshObs_pa_","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(viirsDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/blshObs_pa_","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df)) %>% mutate(chla=scales::rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
a=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% .[complete.cases(.),]
blshObs=a %>% gather(sensor,chla,-c(date,year,month,MODIS,VIIRS)) 



#### swor ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/swor/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/swor/predCIs_mask"
noCHLADir="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/swor/predCIs_mask"

#1. time series of spatial average, just mean 
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(modisDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/swor_pa_","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(viirsDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/swor_pa_","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df)) %>% mutate(chla=scales::rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
a=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% .[complete.cases(.),]
swor=a %>% gather(sensor,chla,-c(date,year,month,MODIS,VIIRS)) 
#swor=left_join(x,swor)

#### casl ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/casl/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/casl/predCIs_mask"
noCHLADir="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/casl/predCIs_mask"

#1. time series of spatial average, just mean 
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(modisDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/casl_pa_","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(viirsDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/casl_pa_","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df)) %>% mutate(chla=scales::rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
a=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% .[complete.cases(.),]
casl=a %>% gather(sensor,chla,-c(date,year,month,MODIS,VIIRS)) 
#casl=left_join(x,casl)

#### blshTr ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshTr/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshTr/predCIs_mask"
noCHLADir="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/blshTr/predCIs_mask"

#1. time series of spatial average, just mean 
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(modisDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/blshTr_pa_","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(viirsDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/blshTr_pa_","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df)) %>% mutate(chla=scales::rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
a=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% .[complete.cases(.),]
blshTr=a %>% gather(sensor,chla,-c(date,year,month,MODIS,VIIRS)) 
#blshTr=left_join(x,blshTr)



#### ecocast ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean_mask"

#1. time series of spatial average, just mean 
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean_mask/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean_mask/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df)) %>% mutate(chla=scales::rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
a=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% .[complete.cases(.),]
ECOCAST=a %>% gather(sensor,chla,-c(date,year,month,MODIS,VIIRS)) 

#### chla ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"

#1. time series of spatial average, just mean 
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask/","",.) %>% gsub("/l.blendChl.grd","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask/","",.) %>% gsub("/l.blendChl.grd","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df)) %>% mutate(chla=scales::rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
CHLA=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% .[complete.cases(.),]
#CHLA=left_join(x,CHLA)




#### standardize ####
lbst1=lbst %>%  mutate(product="lbst")
blshObs1=blshObs%>%  mutate(product="blshO")
casl1=casl%>%  mutate(product="casl")
swor1=swor%>%  mutate(product="swor")
blshtr1=blshTr%>%  mutate(product="blshT")
CHLA1=CHLA %>% rename(chla=MODIS_VIIRS) %>% mutate(sensor="MODIS_VIIRS") %>% mutate(product="Chl-a")
ECOCAST1=ECOCAST %>% mutate(product="EcoCast")
master=do.call("rbind",list(lbst1,blshObs1,casl1,swor1,blshtr1,CHLA1,ECOCAST1)) %>% mutate(sensor=as.factor(sensor)) #%>% .[complete.cases(.),]
master$product=as.factor(master$product)
mean_diff=master %>% mutate(chl2=chla)%>% group_by(product) %>% summarise(mean(chl2))


### summary table ----------------------------------------------------> ####
a=mean_diff %>% dplyr::rename(mean=`mean(chl2)`) %>% mutate(stat="mean_diff") %>% mutate(product=as.character(product)) %>% as.data.frame()
b=curve_var %>% dplyr::rename(product=species) %>% mutate(stat="curve_var")%>% as.data.frame()
c=mean_influence %>% dplyr::rename(product=species) %>% select(-var) %>% mutate(stat="mean_influence")%>% as.data.frame()

d=do.call("rbind",list(a,b,c))
e=d %>% spread(stat,mean)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
write.csv(e,paste(outputDir,"summaryTable.csv"))

d=rbind(a,b)
d=rbind(d,c)
