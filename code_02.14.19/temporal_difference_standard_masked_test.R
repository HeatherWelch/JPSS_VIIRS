#### standardizing difference

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)
library(scales)
library(sdmvspecies)

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

fcnRescale=function(i){
  a <- (i - min(i[], na.rm=TRUE))/(max(i[], na.rm=TRUE)-min(i[], na.rm=TRUE))
}

dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
to_match_date=intersect(dates_m,dates_v)

################### --------------------------------------------------------- > line plot

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
CHLA1=chla %>%  mutate(product="Chl-a")
LBST1=lbst %>%  mutate(product="Leatherback")
ECOCAST1=ecocast %>% mutate(product="EcoCast")

master=do.call("rbind",list(CHLA1,LBST1,ECOCAST1))  #%>% .[complete.cases(.),]
#master=master %>% filter(product=="Chl-a")

lineplot=ggplot(master,aes(x=date))+geom_line(aes(y=chla,group=product,color=product),size=.3)+geom_point(aes(y=chla,group=product,color=product),size=.4)+
  scale_x_date(date_breaks="month",date_labels = "%b",date_minor_breaks = "months")+geom_hline(yintercept=0)+
  scale_color_manual("Product",values=c("Leatherback"="darkgoldenrod","EcoCast"="coral1","Chl-a"="gray"))+
  facet_grid(~year, scales="free")+labs(x="Date")+labs(y="Chlorophyl difference ")+theme(legend.position=c(.4,.5),legend.justification = c(.9,.9))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))

lineplot

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="standard_difference_mask_test"

png(paste(outputDir,datatype,"_line.png",sep=''),width=18,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
lineplot
dev.off()
master$product=as.factor(master$product)

histtt=ggplot(master,aes(x=chla,group=product))+geom_density(aes(fill=product),alpha=.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  #scale_color_manual("Sensor",values=c("Leatherback"="darkgoldenrod","EcoCast"="coral1","Chl-a"="gray"))+
  #facet_grid(sensor~year, scales="free")+labs(x="Date")+labs(y="Chlorophyl difference ")+theme(legend.position=c(.4,.5),legend.justification = c(.9,.9))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))

histtt

png(paste(outputDir,datatype,"_hist_all_years_test.png",sep=''),width=18,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
histtt
dev.off()

a=master %>% mutate(chl2=chla)%>% group_by(product) %>% summarise(mean(chl2,na.rm=T))
a

histtt=ggplot(master,aes(x=chla,group=product))+geom_freqpoly(aes(y=..density..,color=product),fill=NA)+
  geom_vline(xintercept=0)+
  scale_color_manual("Sensor",values=c("Leatherback"="darkgoldenrod","EcoCast"="coral1","Chl-a"="gray"))+
  facet_grid(sensor~year, scales="free")+labs(x="Date")+labs(y="Chlorophyl difference ")+theme(legend.position=c(.4,.5),legend.justification = c(.9,.9))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))

histtt

