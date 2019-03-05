##### testing ability to superimpose response curves and chla in one plot
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
winter_2018=rbind(modis,viirs)

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
fall_2017=rbind(modis,viirs)

### partial plots ####
path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
moddir<-paste(path,"/ModRepFiles/",sep="")
lbst=paste0(moddir,"brt_lbst_CIs.rds") %>% read_rds()
blshT=paste0(moddir,"brt_blshTr_CIs.rds")%>% read_rds()
blshO=paste0(moddir,"brt_blshObs_CIs.rds")%>% read_rds()
casl=paste0(moddir,"brt_casl_CIs.rds")%>% read_rds()
swor=paste0(moddir,"brt_swor_CIs.rds")%>% read_rds()

models=list(lbst,blshT,blshO,casl,swor)
names=c("lbst","blshT","blshO","casl","swor")

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
b=a %>% mutate(id=rep(1:100,50)) %>% group_by(species,id) %>% summarise(meanx=mean(logChl))
bb=a %>% mutate(id=rep(1:100,50)) %>% group_by(species,id) %>% summarise(meany=mean(y))
bb$meanx=b$meanx


c=ggplot()+geom_line(data=a,aes(x=logChl,y=y,group=mod_num))
d=c+geom_line(data=bb,aes(x=meanx,y=meany),color="red")+facet_grid(~species,scales="free")
d

####

c=a %>% mutate(id=rep(1:100,50)) %>% group_by(species,id) %>% summarise(minx=min(y))
cc=a %>% mutate(id=rep(1:100,50)) %>% group_by(species,id) %>% summarise(maxy=max(y))
cc$minx=c$minx
cc$x=bb$meanx

#### swor ####

swor=bb %>% filter(species=="swor")
swor_ribbon=cc %>% filter(species=="swor")
curve=left_join(swor,swor_ribbon) %>% gather(thing,value,-c(species,id,x,meanx)) %>% mutate(value=scales::rescale(value,c(0,1))) %>% spread(thing,value)

d=ggplot()+geom_density(data=fall_2017,aes(x=layer,group=sensor,fill=sensor),alpha=.2)+xlim(-4,3.5)+
  scale_fill_manual("Sensor",values=c("MODIS"="blue","VIIRS"="yellow"))
e=d+geom_ribbon(data=curve,aes(x=x,ymin=minx,ymax=maxy),fill="darkgrey",alpha=.5)+
  ylab("f(Chla)")+xlab("Chla")+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))
f=e+geom_line(data=curve,aes(x=meanx,y=meany),color="Red")
f=f+geom_vline(xintercept=-2,linetype="dashed")+geom_vline(xintercept=0,linetype="dashed")
f=f+ylab("Scaled f(chla) and density")+xlab("Chla (mg^3")+theme(legend.position=c(.1,.9),legend.justification = c(.9,.9))
f

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="fall_2017_curve"
png(paste(outputDir,datatype,"_hist.png",sep=''),width=18,height=12,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
f
dev.off()

####casl ####

casl=bb %>% filter(species=="casl")
casl_ribbon=cc %>% filter(species=="casl")
curve=left_join(casl,casl_ribbon) %>% gather(thing,value,-c(species,id,x,meanx)) %>% mutate(value=scales::rescale(value,c(0,1))) %>% spread(thing,value)

d=ggplot()+geom_density(data=winter_2018,aes(x=layer,group=sensor,fill=sensor),alpha=.2)+xlim(-3,3)+
  scale_fill_manual("Sensor",values=c("MODIS"="blue","VIIRS"="yellow"))
e=d+geom_ribbon(data=curve,aes(x=x,ymin=minx,ymax=maxy),fill="darkgrey",alpha=.5)+
  ylab("f(Chla)")+xlab("Chla")+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))
f=e+geom_line(data=curve,aes(x=meanx,y=meany),color="Red")
f=f+geom_vline(xintercept=2,linetype="dashed")+geom_vline(xintercept=0,linetype="dashed")
f=f+ylab("Scaled f(chla) and density")+xlab("Chla (mg^3")+theme(legend.position=c(.1,.9),legend.justification = c(.9,.9))
f

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="late_2018_curve"
png(paste(outputDir,datatype,"_hist.png",sep=''),width=18,height=12,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
f
dev.off()
