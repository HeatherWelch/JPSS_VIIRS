### data density
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
to_match_date=intersect(dates_m,dates_v)

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

to_matchE=c("-08-","-09-","-10-")
to_matchL=c("-11-","-12-")

modis_2018=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2018",.,,value=T)
modis_2018=unique (grep(paste(to_match_date,collapse="|"),modis_2018, value=TRUE)) 

modis_2018_early=unique (grep(paste(to_matchE,collapse="|"),modis_2018, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,fun=mean,na.rm=F)

modis_2018_late=unique (grep(paste(to_matchL,collapse="|"),modis_2018, value=TRUE))%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,fun=mean,na.rm=F)

viirs_2018=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2018",.,,value=T)
viirs_2018=unique (grep(paste(to_match_date,collapse="|"),viirs_2018, value=TRUE)) 

viirs_2018_early=unique (grep(paste(to_matchE,collapse="|"),viirs_2018, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,fun=mean,na.rm=F)

viirs_2018_late=unique (grep(paste(to_matchL,collapse="|"),viirs_2018, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,fun=mean,na.rm=F)

master=stack(modis_2018_early,viirs_2018_early,modis_2018_late,viirs_2018_late)
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="data_densitiy_2_2018"
PlotPNGs_difference<-function(stack,datatype,outputDir){
  
  ChlorCols<-colorRampPalette(brewer.pal(9,'YlGn'))
  col=ChlorCols(255)
  
  H=maxValue(stack) %>% max()
  L=minValue(stack) %>% min()
  zlimits=c(L,H)
  
  ####### produce png ####
  png(paste(outputDir,datatype,"_difference.png",sep=''),width=24,height=6,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,4))
  par(oma=c(0,0,0,1))
  
  #### MODIS - VIIRS ####

  
  image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[1]], add=TRUE, col="white",levels=c(60,65,80),lwd=.5,labcex=.3)
  text(-123,45,"modis_2018_early",adj=c(0,0),cex=.6)
  
  image.plot(stack[[2]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[2]], add=TRUE, col="white",levels=c(60,65,80),lwd=.5,labcex=.3)
  text(-123,45,"viirs_2018_early",adj=c(0,0),cex=.6)
  
  image.plot(stack[[3]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[3]], add=TRUE, col="white",levels=c(40,50,55),lwd=.5,labcex=.3)
  text(-123,45,"modis_2018_late",adj=c(0,0),cex=.6)
  
  image.plot(stack[[4]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[4]], add=TRUE, col="white",levels=c(40,50,55),lwd=.5,labcex=.3)
  text(-123,45,"viirs_2018_late",adj=c(0,0),cex=.6)
  
  box()
  dev.off()
  
}

PlotPNGs_difference(stack = master,datatype = datatype,outputDir = outputDir)

