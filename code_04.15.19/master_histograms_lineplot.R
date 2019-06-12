### master histograms - all products

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)
library(scales)
library(sdmvspecies)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_04.14.19/"

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
lbst1=lbst %>%  mutate(product="Leatherback")
blshObs1=blshobs%>%  mutate(product="Blueshark - observer")
casl1=casl%>%  mutate(product="Sea lion")
swor1=swor%>%  mutate(product="Swordfish")
blshtr1=blshtrk%>%  mutate(product="Blueshark - tracking")
CHLA1=chla %>%  mutate(product="Chlorophyll")
ECOCAST1=ecocast %>% mutate(product="EcoCast's fishing suitability")

species=do.call("rbind",list(lbst1,blshObs1,casl1,swor1,blshtr1))  %>% mutate(product=as.factor(product)) %>% .[complete.cases(.),]
chla_eco=do.call("rbind",list(CHLA1,ECOCAST1))  %>% mutate(product=as.factor(product)) %>% .[complete.cases(.),]

### line plots ####
ann_text <- data.frame(date = as.Date("2015-08-15"),chla = .1,lab = "Text",
                       year = factor(2015,levels = c("2015","2016","2017","2018")))

ann_rect_2017 <- data.frame(date = seq(as.Date("2017-08-01"),as.Date("2017-10-01"),by=1),chla = .1,lab = "Text",
                       year = factor(2017,levels = c("2015","2016","2017","2018")))
ann_rect_2018 <- data.frame(date = seq(as.Date("2018-08-01"),as.Date("2019-01-01"),by=1),chla = .1,lab = "Text",
                       year = factor(2018,levels = c("2015","2016","2017","2018")))

ann_text_2017 <- data.frame(date =as.Date("2017-09-13"),chla = .085,lab = "Text",
                            year = factor(2017,levels = c("2015","2016","2017","2018")))
ann_text_2018 <- data.frame(date = as.Date("2018-12-18"),chla = -.055,lab = "Text",
                            year = factor(2018,levels = c("2015","2016","2017","2018")))



lineplot_SP=ggplot(species,aes(x=date))+geom_line(aes(y=chla,group=product,color=product),size=.3)+geom_point(aes(y=chla,group=product,color=product),size=.4)+
  scale_x_date(date_breaks="month",date_labels = "%b",date_minor_breaks = "months")+geom_hline(yintercept=0)+
  scale_color_manual("Product",values=c("Leatherback"="#518ab1","Sea lion"="black","Swordfish"="coral1","Blueshark - observer"="#8d4138","Blueshark - tracking"="grey"))+
  facet_grid(~year, scales="free")+labs(x="Month")+labs(y="MODIS minus VIIRS")+theme(legend.position=c(.24,.29),legend.justification = c(.9,.9))+
  theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 10),strip.text.x = element_text(size = 10), strip.background = element_blank(),axis.text.x = element_text(angle=45))+
  theme(legend.key.size = unit(.5,'lines'))+ylim(c(-.07,.1))+
  geom_text(data=ann_text,aes(x=date,y=chla),label="B",size=4)+
  geom_rect(data = ann_rect_2017,xmax=as.Date("2017-10-12"),xmin=as.Date("2017-09-01"),ymin=.032,ymax=.093,fill=NA,color="black")+
  geom_rect(data = ann_rect_2018,xmax=as.Date("2018-11-01"),xmin=as.Date("2019-01-01"),ymin=(-.05),ymax=(-.025),fill=NA,color="black")+
  geom_text(data=ann_text_2017,aes(x=date,y=chla),label="Box 1",size=4)+
  geom_text(data=ann_text_2018,aes(x=date,y=chla),label="Box 2",size=4)

lineplot_SP

lineplot_chla_eco=ggplot(chla_eco,aes(x=date))+geom_line(aes(y=chla,group=product,color=product),size=.3)+geom_point(aes(y=chla,group=product,color=product),size=.4)+
  scale_x_date(date_breaks="month",date_labels = "%b",date_minor_breaks = "months")+geom_hline(yintercept=0)+
  scale_color_manual("Product",values=c("Chlorophyll"="#1a2977","EcoCast's fishing suitability"="#bfb939"))+
  facet_grid(~year, scales="free")+labs(x="Month")+labs(y="MODIS minus VIIRS")+theme(legend.position=c(.29,.18),legend.justification = c(.9,.9))+
  theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 10),strip.text.x = element_text(size = 10), strip.background = element_blank(),axis.text.x = element_text(angle=45))+
  theme(legend.key.size = unit(.5,'lines'))+ylim(c(-.07,.1))+
  geom_text(data=ann_text,aes(x=date,y=chla),label="A",size=4)

lineplot_chla_eco

datatype="lineplot"

png(paste(outputDir,datatype,".png",sep=''),width=36,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
plot_grid(lineplot_chla_eco,lineplot_SP,nrow = 1,ncol = 2)
dev.off()


### histograms ####

histtt_SP=ggplot(species,aes(x=chla,group=product))+geom_density(aes(fill=product),alpha=.4)+
  geom_vline(xintercept=0,linetype="dashed")+
  scale_fill_manual("Product",values=c("Leatherback"="#518ab1","Sea lion"="black","Swordfish"="coral1","Blueshark - observer"="#8d4138","Blueshark - tracking"="grey"))+
  theme(legend.key.size = unit(.5,'lines'))+xlim(c(-.07,.1))+labs(x="MODIS minus VIIRS")+labs(y="Density")+
  theme(legend.position=c(.5,.85))+annotate(geom = "text",x=-.07,y=130,label="B",size=4)+
  theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())

# histtt_SPa=ggplot(species,aes(x=chla,group=product))+geom_density(aes(color=product),alpha=.4,size=.7)+
#   geom_vline(xintercept=0,linetype="dashed")+
#   scale_color_manual("Product",values=c("Leatherback"="#518ab1","Sea lion"="black","Swordfish"="coral1","Blueshark - observer"="#8d4138","Blueshark - tracking"="grey"))+
#   theme(legend.key.size = unit(.5,'lines'))+xlim(c(-.07,.1))+labs(x="MODIS minus VIIRS")+labs(y="Density")+
#   theme(legend.position=c(.5,.85))+annotate(geom = "text",x=-.07,y=130,label="B",size=4)+
#   theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())

histtt_SP

histtt_chla_eco=ggplot(chla_eco,aes(x=chla,group=product))+geom_density(aes(fill=product),alpha=.4)+
  geom_vline(xintercept=0,linetype="dashed")+
  scale_fill_manual("Product",values=c("Chlorophyll"="#1a2977","EcoCast's fishing suitability"="#bfb939"))+
  theme(legend.key.size = unit(.5,'lines'))+xlim(c(-.07,.1))+labs(x="MODIS minus VIIRS")+labs(y="Density")+
  theme(legend.position=c(.55,.9))+annotate(geom = "text",x=-.07,y=40,label="A",size=4)+
  theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())

histtt_chla_eco

datatype="histogram_line"

png(paste(outputDir,datatype,".png",sep=''),width=20,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
plot_grid(histtt_chla_eco,histtt_SP,nrow = 1,ncol = 2)
#plot_grid(histtt_SPa,histtt_SP,nrow = 1,ncol = 2)
dev.off()

master=rbind(chla_eco,species)
mean_diff=master %>% mutate(chl2=chla)%>% group_by(product) %>% summarise(mean(chl2,na.rm=T))
mean_diff_abs=master %>% mutate(chl2=abs(chla))%>% group_by(product) %>% summarise(mean(chl2,na.rm=T))
