### ideas for how to quantify differece

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)


modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"
pmlEsaDir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/EcoCastRuns/output/mean"

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

a<-seq(as.Date("2015-08-01"), as.Date("2019-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character() %>% as.data.frame() 
colnames(a)="full_TS"
a=a %>% mutate(full_TS=as.Date(full_TS))

#1. time series of spatial average, just mean ####
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_pml=list.files(pmlEsaDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_pml=list.files(pmlEsaDir,full.names = T,recursive = T,pattern = "mean.grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_pml)=dates_pml
pml_stats=cellStats(sat_pml,stat="mean")
b=pml_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_pml)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
pml_df=b %>% mutate(sensor="Blended") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df,pml_df))
master$sensor=as.factor(master$sensor)

lineplot=ggplot(master,aes(x=date,y=chla))+geom_line(aes(group=sensor,color=sensor),size=.3)+geom_point(aes(color=sensor),size=.6)+
  scale_x_date(date_breaks="month",date_labels = "%b",date_minor_breaks = "months")+
  scale_color_manual("Sensor",values=c("Blended"="darkgoldenrod","MODIS"="coral1","VIIRS"="gray"))+
  facet_wrap(~year, scales="free_x", nrow=1)+labs(x="Date")+labs(y="EcoCast fishing suitability")+theme(legend.position=c(.1,.95),legend.justification = c(.9,.9))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))


lineplot

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots/"
datatype="ecocast"

png(paste(outputDir,datatype,"_line.png",sep=''),width=18,height=6,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
lineplot
dev.off()



#1. time series of spatial average,  mean +/- SD ####
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
m_stats_SD=cellStats(sat_m,stat="sd")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b$upper=b$chla+m_stats_SD
b$lower=b$chla-m_stats_SD
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
v_stats_SD=cellStats(sat_v,stat="sd")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b$upper=b$chla+v_stats_SD
b$lower=b$chla-v_stats_SD
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_pml=list.files(pmlEsaDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_pml=list.files(pmlEsaDir,full.names = T,recursive = T,pattern = "mean.grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_pml)=dates_pml
pml_stats=cellStats(sat_pml,stat="mean")
pml_stats_SD=cellStats(sat_pml,stat="sd")
b=pml_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_pml)) 
colnames(b)=c("chla","date")
b$upper=b$chla+pml_stats_SD
b$lower=b$chla-pml_stats_SD
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
pml_df=b %>% mutate(sensor="Blended") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df,pml_df))
master$sensor=as.factor(master$sensor)

lineplot=ggplot(master,aes(x=date,y=chla))+geom_line(aes(group=sensor,color=sensor),size=.3)+geom_point(aes(color=sensor),size=.4)+
  geom_ribbon(aes(x=date,ymin=lower, ymax=upper,fill=sensor),alpha=0.3)+
  scale_x_date(date_breaks="month",date_labels = "%b",date_minor_breaks = "months")+
  scale_color_manual("Sensor",values=c("Blended"="cadetblue3","MODIS"="coral1","VIIRS"="gray"))+
  scale_fill_manual("",values=c("Blended"="cadetblue3","MODIS"="coral1","VIIRS"="gray"),guide=F)+
  facet_wrap(~year, scales="free_x", nrow=1)+labs(x="Date")+labs(y="EcoCast fishing suitability")+theme(legend.position=c(.25,.3),legend.justification = c(.9,.9))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))


lineplot

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots/"
datatype="ecocastaRibbon"

png(paste(outputDir,datatype,"_line.png",sep=''),width=18,height=6,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
lineplot
dev.off()


#2.temporal average ####
modis=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirs=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
blend=list.files(pmlEsaDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)

master=stack(modis,viirs,blend)
names(master)=c("modis","viirs","blend")
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots/"#;dir.create(outputDir)
datatype="ecocast"
PlotPNGs<-function(stack,datatype,outputDir){
  
  H=maxValue(stack) %>% max()
  L=minValue(stack) %>% min()
  zlimits=c(L,H)
  
  EcoCols<-colorRampPalette(c("red","orange","white","cyan","blue"))
  ChlorCols<-colorRampPalette(brewer.pal(9,'YlGn'))
  SpCols<-colorRampPalette(brewer.pal(9,'GnBu'))
  
  ####### produce png ####
  png(paste(outputDir,datatype,".png",sep=''),width=18,height=6,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,3))
  par(oma=c(0,0,0,1))
  
  if(datatype=="chla") {
    col=ChlorCols(255)
  }
  
  if(datatype=="lbst") {
    col=SpCols(255)
  }
  
  if(datatype=="ecocast") {
    col=EcoCols(255)
  }
  
  image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[1]], add=TRUE, col="black",levels=c(.2,.4))
  text(-122,45,"MODIS",adj=c(0,0),cex=1.5)
  
  image.plot(stack[[2]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[2]], add=TRUE, col="black",levels=c(.2,.4))
  text(-122,45,"VIIRS",adj=c(0,0),cex=1.5)
  
  image.plot(stack[[3]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[3]], add=TRUE, col="black",levels=c(.2,.4))
  text(-122,45,"Blended",adj=c(0,0),cex=1.5)
  
  
  box()
  dev.off()
  
}

PlotPNGs(stack=master,datatype = datatype,outputDir = outputDir)
  

#3. average difference between products ####
modis=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirs=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
blend=list.files(pmlEsaDir,full.names = T,recursive = T,pattern = "mean.grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)

m_v=modis-viirs
m_v_A=cellStats(m_v,mean)
m_v_SD=cellStats(m_v,sd)

m_b=modis-blend
m_b_A=cellStats(m_b,mean)
m_b_SD=cellStats(m_b,sd)

v_b=viirs-blend
v_b_A=cellStats(v_b,mean)
v_b_SD=cellStats(v_b,sd)

master=stack(m_v,m_b,v_b)
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots/"#;dir.create(outputDir)
datatype="ecocast"
PlotPNGs_difference<-function(stack,datatype,outputDir){
  
  # DiffCols<-colorRampPalette(brewer.pal(11,'RdYlGn'))
  # DiffCols<-colorRampPalette(pal(300))
  #DiffCols<-colorRampPalette(brewer.pal(colorspace::diverge_hcl))
  col=colorRamps:::blue2red(255)
  
  H=maxValue(stack) %>% max()
  L=minValue(stack) %>% min()
  # zlimits=c(L,H)
  zlimits=c(-.24,.24)
  print("one")
  
  ####### produce png ####
  png(paste(outputDir,datatype,"_difference.png",sep=''),width=18,height=6,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,3))
  par(oma=c(0,0,0,1))
  
  plusSD=rasterToPoints(stack[[1]],fun=function(x)x>cellStats(stack[[1]],mean)+cellStats(stack[[1]],sd))
  minusSD=rasterToPoints(stack[[1]],fun=function(x)x<cellStats(stack[[1]],mean)-cellStats(stack[[1]],sd))
  
  image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[1]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-122,45,"Modis - VIIRS",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[2]],fun=function(x)x>cellStats(stack[[2]],mean)+cellStats(stack[[2]],sd))
  minusSD=rasterToPoints(stack[[2]],fun=function(x)x<cellStats(stack[[2]],mean)-cellStats(stack[[2]],sd))
  
  print("two")
  
  image.plot(stack[[2]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[2]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-122,45,"Modis - Blended",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[3]],fun=function(x)x>cellStats(stack[[3]],mean)+cellStats(stack[[3]],sd))
  minusSD=rasterToPoints(stack[[3]],fun=function(x)x<cellStats(stack[[3]],mean)-cellStats(stack[[3]],sd))
  
  image.plot(stack[[3]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[3]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-122,45,"VIIRS - Blended",adj=c(0,0),cex=.8)
  
  print("three")
  
  box()
  dev.off()
  
}

PlotPNGs_difference(stack = master,datatype = datatype,outputDir = outputDir)



