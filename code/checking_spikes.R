#### exploring spikes

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)


modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
pmlEsaDir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/Satellite"

#1. CHLA time series of spatial average, just mean ####
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_pml=list.files(pmlEsaDir,full.names = T,recursive = T,pattern = ".grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_pml=list.files(pmlEsaDir,full.names = T,recursive = T,pattern = ".grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
names(sat_pml)=dates_pml
pml_stats=cellStats(sat_pml,stat="mean")
b=pml_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_pml)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
pml_df=b %>% mutate(sensor="Blended") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df,pml_df))
master$sensor=as.factor(master$sensor)
a=master %>% .[complete.cases(.),] %>% group_by(sensor) %>% summarise(min=min(chla))
b=master %>% filter(chla<(-5.1))
chla=list.files("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite/2016-10-11/",pattern = ".grd",full.names = T) %>% raster() ###omg its just a gdamn mess. remove rome analysis


a=master %>% filter(year==2017) %>% .[complete.cases(.),] %>% group_by(sensor) %>% summarise(max=max(chla))
b=master %>% filter(chla>=(-.9)) %>% filter(year==2017)%>% filter(sensor=="VIIRS")
chla=list.files("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite/2017-08-05/",pattern = ".grd",full.names = T) %>% raster() ### nah it's okay, just missing lots of data which causes weird spike


modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs"
pmlEsaDir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/EcoCastRuns/lbst/predCIs"

#2. LBST time series of spatial average, just mean ####
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs/lbst_pa_","",.) %>% gsub("_mean.grd","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs/lbst_pa_","",.) %>% gsub("_mean.grd","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_pml=list.files(pmlEsaDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_pml=list.files(pmlEsaDir,full.names = T,recursive = T,pattern = "mean.grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/EcoCastRuns/lbst/predCIs/lbst_pa_","",.) %>% gsub("_mean.grd","",.)
names(sat_pml)=dates_pml
pml_stats=cellStats(sat_pml,stat="mean")
b=pml_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_pml)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
pml_df=b %>% mutate(sensor="Blended") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df,pml_df))
master$sensor=as.factor(master$sensor)

a=master %>% filter(year==2016) %>% .[complete.cases(.),] %>% group_by(sensor) %>% summarise(min=min(chla))
b=master %>% filter(chla<=(.19)) %>% filter(year==2016)%>% filter(sensor=="VIIRS")
chla=list.files("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs/",pattern = "mean.grd",full.names = T)%>% grep("2016-09-07",.,value=T) %>% raster() 
chla=list.files("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite/2016-09-07/",pattern = ".grd",full.names = T) %>% raster()
chla=list.files("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite/2016-09-08/",pattern = ".grd",full.names = T) %>% raster()

folder=list.files("/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2016-09-07/",pattern = ".grd",full.names = T) %>% stack() #### MAN YEAH IT'S BECAUSE WIND IS FUCKED
names=list.files("/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2016-09-07/",pattern = ".grd")
names(folder)=names
plot(folder)

folder=list.files("/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2016-09-08/",pattern = ".grd",full.names = T) %>% stack() #### MAN YEAH IT'S BECAUSE WIND IS FUCKED
names=list.files("/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2016-09-08/",pattern = ".grd")
names(folder)=names
plot(folder)
