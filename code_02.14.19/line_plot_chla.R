### difference, chla, just viirs and modis

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)


modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

a<-seq(as.Date("2015-08-01"), as.Date("2019-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character() %>% as.data.frame() 
colnames(a)="full_TS"
a=a %>% mutate(full_TS=as.Date(full_TS))

#1. time series of spatial average, just mean ####
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
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

master=do.call("rbind",list(m_df,v_df))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
a=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% .[complete.cases(.),]
#a=a %>% gather(sensor,chla,-c(date,year,month,MODIS,VIIRS)) 

lineplot=ggplot(a,aes(x=date,y=MODIS_VIIRS))+geom_line(size=.3,color="darkgoldenrod")+geom_point(size=.6,color="darkgoldenrod")+
  scale_x_date(date_breaks="month",date_labels = "%b",date_minor_breaks = "months")+geom_hline(yintercept=0)+
  #scale_color_manual("Sensor",values=c("MODIS_VIIRS"="darkgoldenrod","MODIS_Blended"="coral1","VIIRS_Blended"="gray"))+
  facet_wrap(~year, scales="free_x", nrow=1)+labs(x="Date")+labs(y="Chlorophyl difference (MODIS minus VIIRS)")+theme(legend.position=c(.1,.9),legend.justification = c(.9,.9))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))

lineplot

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"#;dir.create(outputDir)
datatype="chla_difference"

png(paste(outputDir,datatype,"_line.png",sep=''),width=18,height=6,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
lineplot
dev.off()


#3. average difference between products ####
modis_2015=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2015",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
modis_2016=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
modis_2017=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2017",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
modis_2018=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2018",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)

viirs_2015=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2015",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirs_2016=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirs_2017=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2017",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirs_2018=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2018",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)

a2015=modis_2015-viirs_2015
a2016=modis_2016-viirs_2016
a2017=modis_2017-viirs_2017
a2018=modis_2018-viirs_2018

master=stack(a2015,a2016,a2017,a2018)
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="chla_difference"
PlotPNGs_difference<-function(stack,datatype,outputDir){
  
  col=colorRamps:::blue2red(255)
  
  H=maxValue(stack) %>% max()
  L=minValue(stack) %>% min()
  # zlimits=c(L,H)
  zlimits=c(-1.5,1.5)
  
  ####### produce png ####
  png(paste(outputDir,datatype,"_difference.png",sep=''),width=24,height=6,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,4))
  par(oma=c(0,0,0,1))
  
  plusSD=rasterToPoints(stack[[1]],fun=function(x)x>cellStats(stack[[1]],mean)+cellStats(stack[[1]],sd))
  minusSD=rasterToPoints(stack[[1]],fun=function(x)x<cellStats(stack[[1]],mean)-cellStats(stack[[1]],sd))
  
  image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[1]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"MODIS - VIIRS 2015",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[2]],fun=function(x)x>cellStats(stack[[2]],mean)+cellStats(stack[[2]],sd))
  minusSD=rasterToPoints(stack[[2]],fun=function(x)x<cellStats(stack[[2]],mean)-cellStats(stack[[2]],sd))
  
  image.plot(stack[[2]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[2]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"MODIS - VIIRS 2016",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[3]],fun=function(x)x>cellStats(stack[[3]],mean)+cellStats(stack[[3]],sd))
  minusSD=rasterToPoints(stack[[3]],fun=function(x)x<cellStats(stack[[3]],mean)-cellStats(stack[[3]],sd))
  
  image.plot(stack[[3]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[3]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"MODIS - VIIRS 2017",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[4]],fun=function(x)x>cellStats(stack[[4]],mean)+cellStats(stack[[4]],sd))
  minusSD=rasterToPoints(stack[[4]],fun=function(x)x<cellStats(stack[[4]],mean)-cellStats(stack[[4]],sd))
  
  image.plot(stack[[4]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[4]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"MODIS - VIIRS 2018",adj=c(0,0),cex=.8)
  
  
  box()
  dev.off()
  
}

PlotPNGs_difference(stack = master,datatype = datatype,outputDir = outputDir)


