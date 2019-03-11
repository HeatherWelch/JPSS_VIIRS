### data density all years
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask/","",.) %>% gsub("/l.blendChl.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_msdk/","",.) %>% gsub("/l.blendChl.grd","",.)
to_match_date=intersect(dates_m,dates_v)

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

#### winter 2018 ####
to_matchL=c("-11-","-12-")

modis=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2018",.,,value=T)
modis=unique (grep(paste(to_match_date,collapse="|"),modis, value=TRUE))
modis_2018_late=unique (grep(paste(to_matchL,collapse="|"),modis, value=TRUE))%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,fun=mean,na.rm=T)

viirs=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2018",.,,value=T)
viirs=unique (grep(paste(to_match_date,collapse="|"),viirs, value=TRUE))
viirs_2018_late=unique (grep(paste(to_matchL,collapse="|"),viirs, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,fun=mean,na.rm=T)

modis=as.data.frame(modis_2018_late) %>% mutate(sensor="MODIS")
viirs=as.data.frame(viirs_2018_late) %>% mutate(sensor="VIIRS")
master=rbind(modis,viirs)

a=ggplot(master)+geom_density(aes(x=layer,group=sensor,fill=sensor),alpha=.2)+xlim(-3,3)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="late_2018"
png(paste(outputDir,datatype,"_hist.png",sep=''),width=18,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
a
dev.off()

#### fall 2017 ####
to_matchL=c("-11-","-09-","-10-")

modis=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2017",.,,value=T)
modis=unique (grep(paste(to_match_date,collapse="|"),modis, value=TRUE))
modis_2018_late=unique (grep(paste(to_matchL,collapse="|"),modis, value=TRUE))%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,fun=mean,na.rm=T)

viirs=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2017",.,,value=T)
viirs=unique (grep(paste(to_match_date,collapse="|"),viirs, value=TRUE))
viirs_2018_late=unique (grep(paste(to_matchL,collapse="|"),viirs, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,fun=mean,na.rm=T)

modis=as.data.frame(modis_2018_late) %>% mutate(sensor="MODIS")
viirs=as.data.frame(viirs_2018_late) %>% mutate(sensor="VIIRS")
master=rbind(modis,viirs)

a=ggplot(master)+geom_density(aes(x=layer,group=sensor,fill=sensor),alpha=.2)+xlim(-3.5,3.5)
a

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="fall_2017"
png(paste(outputDir,datatype,"_hist.png",sep=''),width=18,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
a
dev.off()

