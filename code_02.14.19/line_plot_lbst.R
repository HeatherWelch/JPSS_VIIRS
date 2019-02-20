### difference, chla, just viirs and modis

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)


modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs"
noCHLADir="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/lbst/predCIs"

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

a<-seq(as.Date("2015-08-01"), as.Date("2019-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character() %>% as.data.frame() 
colnames(a)="full_TS"
a=a %>% mutate(full_TS=as.Date(full_TS))

#1. time series of spatial average, just mean ####
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

sat_nochla=list.files(noCHLADir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_nochla=list.files(noCHLADir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub(noCHLADir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/lbst_pa_","",.)
names(sat_nochla)=dates_nochla
nochla_stats=cellStats(sat_nochla,stat="mean")
b=nochla_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_nochla)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
nochla_df=b %>% mutate(sensor="No_Chl_a") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df,nochla_df))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
a=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% mutate(MODIS_noCHLA=MODIS-No_Chl_a) %>% mutate(VIRIS_noCHLA=VIIRS-No_Chl_a)#%>% .[complete.cases(.),]
a=a %>% gather(sensor,chla,-c(date,year,month,MODIS,VIIRS,No_Chl_a)) 

lineplot=ggplot(a,aes(x=date,y=chla))+geom_line(aes(group=sensor,color=sensor),size=.3)+geom_point(aes(group=sensor,color=sensor),size=.4)+
  scale_x_date(date_breaks="month",date_labels = "%b",date_minor_breaks = "months")+geom_hline(yintercept=0)+
  scale_color_manual("Sensor",values=c("MODIS_VIIRS"="darkgoldenrod","MODIS_noCHLA"="coral1","VIRIS_noCHLA"="gray"))+
  facet_wrap(~year, scales="free_x", nrow=1)+labs(x="Date")+labs(y="Chlorophyl difference ")+theme(legend.position=c(.4,.5),legend.justification = c(.9,.9))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))

lineplot

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"#;dir.create(outputDir)
datatype="lbst_difference"

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

noCHLA_2015=list.files(noCHLADir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2015",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
noCHLA_2016=list.files(noCHLADir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
noCHLA_2017=list.files(noCHLADir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2017",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
noCHLA_2018=list.files(noCHLADir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2018",.,,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)

mv2015=modis_2015-viirs_2015
mv2016=modis_2016-viirs_2016
mv2017=modis_2017-viirs_2017
mv2018=modis_2018-viirs_2018

mnC2015=modis_2015-noCHLA_2015
mnC2016=modis_2016-noCHLA_2016
mnC2017=modis_2017-noCHLA_2017
mnC2018=modis_2018-noCHLA_2018

vnC2015=viirs_2015-noCHLA_2015
vnC2016=viirs_2016-noCHLA_2016
vnC2017=viirs_2017-noCHLA_2017
vnC2018=viirs_2018-noCHLA_2018

master=stack(mv2015,mv2016,mv2017,mv2018,mnC2015,mnC2016,mnC2017,mnC2018,vnC2015,vnC2016,vnC2017,vnC2018)
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="lbst_difference"
PlotPNGs_difference<-function(stack,datatype,outputDir){
  
  col=colorRamps:::blue2red(255)
  
  H=maxValue(stack) %>% max()
  L=minValue(stack) %>% min()
  # zlimits=c(L,H)
  zlimits=c(-.32,.32)
  
  ####### produce png ####
  png(paste(outputDir,datatype,"_difference.png",sep=''),width=28,height=24,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(3,4))
  par(oma=c(0,0,0,1))
  
  #### MODIS - VIIRS ####
  
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
  
  #### MODIS - No Chla ####
  
  plusSD=rasterToPoints(stack[[5]],fun=function(x)x>cellStats(stack[[5]],mean)+cellStats(stack[[5]],sd))
  minusSD=rasterToPoints(stack[[5]],fun=function(x)x<cellStats(stack[[5]],mean)-cellStats(stack[[5]],sd))
  
  image.plot(stack[[5]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[5]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"MODIS - No Chla 2015",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[6]],fun=function(x)x>cellStats(stack[[6]],mean)+cellStats(stack[[6]],sd))
  minusSD=rasterToPoints(stack[[6]],fun=function(x)x<cellStats(stack[[6]],mean)-cellStats(stack[[6]],sd))
  
  image.plot(stack[[6]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[6]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"MODIS - No Chla 2016",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[7]],fun=function(x)x>cellStats(stack[[7]],mean)+cellStats(stack[[7]],sd))
  minusSD=rasterToPoints(stack[[7]],fun=function(x)x<cellStats(stack[[7]],mean)-cellStats(stack[[7]],sd))
  
  image.plot(stack[[7]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[7]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"MODIS - No Chla 2017",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[8]],fun=function(x)x>cellStats(stack[[8]],mean)+cellStats(stack[[8]],sd))
  minusSD=rasterToPoints(stack[[8]],fun=function(x)x<cellStats(stack[[8]],mean)-cellStats(stack[[8]],sd))
  
  image.plot(stack[[8]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[8]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"MODIS - No Chla 2018",adj=c(0,0),cex=.8)
  
  #### VIIRS - No Chla ####
  
  plusSD=rasterToPoints(stack[[9]],fun=function(x)x>cellStats(stack[[9]],mean)+cellStats(stack[[9]],sd))
  minusSD=rasterToPoints(stack[[9]],fun=function(x)x<cellStats(stack[[9]],mean)-cellStats(stack[[9]],sd))
  
  image.plot(stack[[9]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[9]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"VIIRS - No Chla 2015",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[10]],fun=function(x)x>cellStats(stack[[10]],mean)+cellStats(stack[[10]],sd))
  minusSD=rasterToPoints(stack[[10]],fun=function(x)x<cellStats(stack[[10]],mean)-cellStats(stack[[10]],sd))
  
  image.plot(stack[[10]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[10]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"VIIRS - No Chla 2016",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[11]],fun=function(x)x>cellStats(stack[[11]],mean)+cellStats(stack[[11]],sd))
  minusSD=rasterToPoints(stack[[11]],fun=function(x)x<cellStats(stack[[11]],mean)-cellStats(stack[[11]],sd))
  
  image.plot(stack[[11]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[11]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"VIIRS - No Chla 2017",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[12]],fun=function(x)x>cellStats(stack[[12]],mean)+cellStats(stack[[12]],sd))
  minusSD=rasterToPoints(stack[[12]],fun=function(x)x<cellStats(stack[[12]],mean)-cellStats(stack[[12]],sd))
  
  image.plot(stack[[12]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[12]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"VIIRS - No Chla 2018",adj=c(0,0),cex=.8)
  
  
  box()
  dev.off()
  
}

PlotPNGs_difference(stack = master,datatype = datatype,outputDir = outputDir)


