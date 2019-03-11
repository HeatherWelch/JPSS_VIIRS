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

#a=summary(casl[[3]])%>% mutate(species=names[i]) %>% mutate(mod_num=i)

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

### mean chla difference ----------------------------------------------------> ####
path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

fcnRescale=function(i){
  a <- (i - min(i[], na.rm=TRUE))/(max(i[], na.rm=TRUE)-min(i[], na.rm=TRUE))
}

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean_mask"
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean_mask/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean_mask/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
to_match_date=intersect(dates_m,dates_v)
to_match_date

################### --------------------------------------------------------- > line plot

#### lbst ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs_mask"

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE-viirsE 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
lbst=b 

#### blshobs ####

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshObs/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshObs/predCIs_mask"

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE-viirsE 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
blshobs=b 

#### swor ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/swor/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/swor/predCIs_mask"

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE-viirsE 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
swor=b 

#### casl ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/casl/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/casl/predCIs_mask"

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE-viirsE 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
casl=b 

#### blshTr ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshTr/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshTr/predCIs_mask"

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE-viirsE 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
blshtrk=b 

#### chla ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"

#1. time series of spatial average, just mean 

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE-viirsE 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
chla=b 

#### ecocast ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean_mask"

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE-viirsE 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
ecocast=b 


#### standardize ####
lbst1=lbst %>%  mutate(product="lbst")
blshObs1=blshobs%>%  mutate(product="blshO")
casl1=casl%>%  mutate(product="casl")
swor1=swor%>%  mutate(product="swor")
blshtr1=blshtrk%>%  mutate(product="blshT")
CHLA1=chla %>%  mutate(product="Chl-a")
ECOCAST1=ecocast %>% mutate(product="EcoCast")

master=do.call("rbind",list(lbst1,blshObs1,casl1,swor1,blshtr1,CHLA1,ECOCAST1)) #%>% .[complete.cases(.),]
master$product=as.factor(master$product)
mean_diff=master %>% mutate(chl2=chla)%>% group_by(product) %>% summarise(mean(chl2,na.rm=T))
mean_diff_abs=master %>% mutate(chl2=abs(chla))%>% group_by(product) %>% summarise(mean(chl2,na.rm=T))


### summary table ----------------------------------------------------> ####
a=mean_diff %>% dplyr::rename(mean=`mean(chl2, na.rm = T)`) %>% mutate(stat="mean_diff") %>% mutate(product=as.character(product)) %>% as.data.frame()
b=mean_diff_abs %>% dplyr::rename(mean=`mean(chl2, na.rm = T)`) %>% mutate(stat="mean_diff_abs") %>% mutate(product=as.character(product)) %>% as.data.frame()
c=mean_influence %>% dplyr::rename(product=species) %>% select(-var) %>% mutate(stat="mean_influence")%>% as.data.frame()

d=do.call("rbind",list(a,b,c))
e=d %>% spread(stat,mean)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"
write.csv(e,paste(outputDir,"summaryTable_test.csv"))

d=rbind(a,b)
d=rbind(d,c)
