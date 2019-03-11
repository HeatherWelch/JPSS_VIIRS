#### standardizing difference

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)
library(scales)
library(sdmvspecies)

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

a<-seq(as.Date("2015-08-01"), as.Date("2016-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
b<-seq(as.Date("2016-08-01"), as.Date("2017-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
c<-seq(as.Date("2017-08-01"), as.Date("2018-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
d<-seq(as.Date("2018-08-01"), as.Date("2019-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
x=list(a,b,c,d) %>% unlist() %>% as.data.frame()
colnames(x)="date"
x=x %>% mutate(date=as.Date(date))

################### --------------------------------------------------------- > line plot

#### chla ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask/","",.) %>% gsub("/l.blendChl.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask/","",.) %>% gsub("/l.blendChl.grd","",.)
to_match_date=intersect(dates_m,dates_v)

#1. time series of spatial average, just mean 
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd") %>% grep("2016-10-11",.,invert=T,value=T)
sat_m=unique (grep(paste(to_match_date,collapse="|"),sat_m, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% abs()

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T)
sat_v=unique (grep(paste(to_match_date,collapse="|"),sat_v, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% abs()

sat=sat_m - sat_v
names(sat)=to_match_date
m_stats=cellStats(sat,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07") %>% filter(month!="01"&month!="07")


ggplot(b,aes(x=date,y=chla))+geom_line()+facet_grid(~year, scales="free")

