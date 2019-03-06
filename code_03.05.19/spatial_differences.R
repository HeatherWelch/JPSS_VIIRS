### spatial difference - 3 columns by two rows
#1. scatter
#2. chla averaged across time-series. Maybe draw transition line
#3. chla difference
#4. blshtrk difference
#5. swordifish difference
#6. ecocast difference

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"

library(scales)
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(plotly)

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


#1. scatter ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask/","",.) %>% gsub("/l.blendChl.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask/","",.) %>% gsub("/l.blendChl.grd","",.)
to_match=intersect(dates_m,dates_v)

sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask/","",.) %>% gsub("/l.blendChl.grd","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))
m_df=m_df[m_df$date %in% as.Date(to_match),]

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask/","",.) %>% gsub("/l.blendChl.grd","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))
v_df=v_df[v_df$date %in% as.Date(to_match),]

master=do.call("rbind",list(m_df,v_df)) 
master$sensor=as.factor(master$sensor)
chla_scatter=master %>% spread(sensor,chla)

### colored/shaped by month and year. not doing this for the moment
# a=ggplot(chla_scatter,aes(x=MODIS,y=VIIRS))+geom_point(aes(color=year,shape=month))+geom_abline(slope=1,intercept = 0)+xlim(0,1)+ylim(0,1)+stat_smooth(method="lm",se = F,linetype="dashed",color="black")+
#   scale_color_manual("year",values=c("2015"="darkgoldenrod","2016"="coral1","2017"="gray","2018"="cadetblue3"))+
#   ylab("VIIRS Chl-a (mg^3)")+xlab("MODIS Chl-a (mg^3)")+
#   theme(legend.key.size = unit(.5,'lines'),legend.position=c(.1,.8))
# a

a=ggplot(chla_scatter,aes(x=MODIS,y=VIIRS))+geom_point(shape=1)+geom_abline(slope=1,intercept = 0,color="blue")+xlim(-2,-.44)+ylim(-2,-.44)+stat_smooth(method="lm",se = F,linetype="dashed",color="blue")+
  ylab("VIIRS Chl-a (mg^3)")+xlab("MODIS Chl-a (mg^3)")+
  theme(legend.key.size = unit(.5,'lines'),legend.position=c(.1,.8))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())
scatterplot=a
scatterplot

datatype="scatterplot"

png(paste(outputDir,datatype,".png",sep=''),width=10,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
scatterplot
dev.off()

#2. chla averaged across time-series. Maybe draw transition line ####

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"

#1. time series of spatial average, just mean 

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))


PlotPNGs<-function(stack,datatype,outputDir){
  
  EcoCols<-colorRampPalette(c("red","orange","white","cyan","blue"))
  ChlorCols<-colorRampPalette(brewer.pal(9,'YlGn'))
  SpCols<-colorRampPalette(brewer.pal(9,'GnBu'))
  
  ####### produce png ####
  png(paste(outputDir,datatype,".png",sep=''),width=24,height=24,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,1))

    col=ChlorCols(255)
  
  stackM=stack %>% calc(.,fun=mean,na.rm=T)
  H=maxValue(stackM) %>% max(na.rm=T)
  L=minValue(stackM) %>% min(na.rm=T)
  zlimits=c(L,H)
  
  image.plot(stackM,col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.args = list(text="mg/m3",cex=1, side=3, line=0,adj=-.1))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stackM, add=TRUE, col="black",levels=c(-1.14),labcex = 1,lwd=2)
  text(-122,45,datatype,adj=c(0,0),cex=1.5)
  
  
  box()
  dev.off()
  
}

PlotPNGs(stack=modisE,datatype = "MODIS Chl-a",outputDir = outputDir)
PlotPNGs(stack=viirsE,datatype = "VIIRS Chl-a",outputDir = outputDir)


#3. spatial differences ####
### chla ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

chla=modisE-viirsE 

### blshTr ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshTr/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshTr/predCIs_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

blshtrk=modisE-viirsE 

### swor ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/swor/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/swor/predCIs_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

swor=modisE-viirsE 

### ecocast ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean_mask"

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

ecocast=modisE-viirsE 

#### plot all ####

plotting=stack(calc(chla,mean,na.rm=T),calc(swor,mean,na.rm=T),calc(blshtrk,mean,na.rm=T),calc(ecocast,mean,na.rm=T))
H=max(plotting[],na.rm=T)
L=min(plotting[],na.rm=T)

PlotPNGs_difference<-function(stack,product,outputDir,H,L,countour_ras){
  
  col=colorRamps:::blue2red(255)
  
  stackM=stack %>% calc(.,mean,na.rm=T)
  
  Labs=abs(L)
  both=c(H,Labs)
  MaxVal=max(both) 
  # zlimits=c(L,H)
  zlimits=c((MaxVal*-1),H)
  
  ####### produce png ####
  png(paste(outputDir,product,"_M-V_difference2.png",sep=''),width=24,height=24,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,1))
  par(oma=c(0,0,0,1))
  
  #### MODIS - VIIRS ####
  
  stackSD=stack %>% calc(.,mean,na.rm=T) %>% cellStats(.,sd)
  stackMP=stackM%>% cellStats(.,mean)
  #postive=stackM[stackM>(stackM+stackSD)]
  
  plusSD=rasterToPoints(stackM,fun=function(x)x>(stackMP+stackSD))
  minusSD=rasterToPoints(stackM,fun=function(x)x<(stackMP-stackSD))
  
  image.plot(stackM,col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.cex=.5)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=5,pch = ".")
  points(minusSD,cex=5,pch = ".")
  contour(countour_ras, add=TRUE, col="black",levels=c(-1.14),labcex = 1,lwd=2)
  text(-122,45,product,adj=c(0,0),cex=1.5)
  
  box()
  dev.off()
  
}


viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) 
countour_ras=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))%>% calc(.,fun=mean,na.rm=T)


PlotPNGs_difference(stack = chla,product = "Chl-a",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)
PlotPNGs_difference(stack = swor,product = "Swordfish",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)
PlotPNGs_difference(stack = blshtrk,product = "Blueshark - Tracking",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)
PlotPNGs_difference(stack = ecocast,product = "EcoCast",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)
