### data density all years
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)

## across full 612 day time series ####

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite"
ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite"

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

a=seq(as.Date("2015-08-01"),as.Date("2015-12-31"),by=1) %>% as.character()
b=seq(as.Date("2016-08-01"),as.Date("2016-12-31"),by=1)%>% as.character()
c=seq(as.Date("2017-08-01"),as.Date("2017-12-31"),by=1)%>% as.character()
d=seq(as.Date("2018-08-01"),as.Date("2018-12-31"),by=1)%>% as.character()

dates=list(a,b,c,d) %>% unlist()

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd") %>% grep(paste(dates,collapse="|"),., value=TRUE) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% grep(paste(dates,collapse="|"),., value=TRUE)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
gsmE=list.files(gsmDir,full.names = T,recursive = T,pattern = ".grd") %>% grep(paste(dates,collapse="|"),., value=TRUE)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
avwE=list.files(avwDir,full.names = T,recursive = T,pattern = ".grd") %>% grep(paste(dates,collapse="|"),., value=TRUE)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
ociE=list.files(ociDir,full.names = T,recursive = T,pattern = ".grd") %>% grep(paste(dates,collapse="|"),., value=TRUE)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

modisE[!is.na(modisE)]=1
viirsE[!is.na(viirsE)]=1
gsmE[!is.na(gsmE)]=1
avwE[!is.na(avwE)]=1
ociE[!is.na(ociE)]=1

modisE=modisE %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
viirsE=viirsE %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
gsmE=gsmE %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
avwE=avwE %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
ociE=ociE %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 

master=stack(modisE,viirsE,gsmE,avwE,ociE)
master2=master/length(dates)*100

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_12.01.19/"
datatype="data_density"
PlotPNGs_difference<-function(stack,datatype,outputDir){
  
  ChlorCols<-colorRampPalette(brewer.pal(11,'Spectral'))
  col=ChlorCols(255)
  
  H=maxValue(stack) %>% max()
  L=minValue(stack) %>% min()
  zlimits=c(L,H)
  
  ####### produce png ####
  png(paste(outputDir,datatype,".png",sep=''),width=48,height=24,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,2))
  par(oma=c(0,0,0,1))
  
  #### MODIS - VIIRS ####

  
  image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[1]], add=TRUE, col="black",levels=c(300,330,360),labcex = 1,lwd=2)
  text(-123,45,"MODIS Chl-a",adj=c(0,0),cex=.6)
  
  image.plot(stack[[2]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[2]], add=TRUE, col="black",levels=c(300,330,360),labcex = 1,lwd=2)
  text(-123,45,"VIIRS Chl-a",adj=c(0,0),cex=.6)
  
  box()
  dev.off()
  
}
PlotPNGs_difference_percent<-function(stack,datatype,outputDir){
  
  ChlorCols<-colorRampPalette(brewer.pal(11,'Spectral'))
  col=ChlorCols(255)
  
  H=maxValue(stack) %>% max()
  L=minValue(stack) %>% min()
  zlimits=c(L,H)
  
  ####### produce png ####
  png(paste(outputDir,datatype,".png",sep=''),width=72,height=48,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(2,3))
  par(oma=c(0,0,0,1))
  
  #### MODIS - VIIRS ####
  
  image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.args = list(text="%",cex=1, side=3, line=1,adj=0))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[1]], add=TRUE, col="black",levels=c(95,90),labcex = 1,lwd=2)
  text(-122,45,"MODIS Chl-a",adj=c(0,0),cex=1.5)
  
  image.plot(stack[[2]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.args = list(text="%",cex=1, side=3, line=1,adj=0))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[2]], add=TRUE, col="black",levels=c(95,90),labcex = 1,lwd=2)
  text(-122,45,"VIIRS Chl-a",adj=c(0,0),cex=1.5)
  
  image.plot(stack[[3]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.args = list(text="%",cex=1, side=3, line=1,adj=0))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[3]], add=TRUE, col="black",levels=c(95,90),labcex = 1,lwd=2)
  text(-122,45,"Globcolour GSM Chl-a",adj=c(0,0),cex=1.5)
  
  image.plot(stack[[4]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.args = list(text="%",cex=1, side=3, line=1,adj=0))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[4]], add=TRUE, col="black",levels=c(95,90),labcex = 1,lwd=2)
  text(-122,45,"Globcolour AVW Chl-a",adj=c(0,0),cex=1.5)
  
  image.plot(stack[[5]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.args = list(text="%",cex=1, side=3, line=1,adj=0))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[5]], add=TRUE, col="black",levels=c(95,90),labcex = 1,lwd=2)
  text(-122,45,"OC-CCI Chl-a",adj=c(0,0),cex=1.5)
  
  box()
  dev.off()
  
}

# datatype="data_density"
# PlotPNGs_difference(stack = master,datatype = datatype,outputDir = outputDir)

datatype="data_density_percent"
PlotPNGs_difference_percent(stack = master2,datatype = datatype,outputDir = outputDir)

## across time series common to all products ####

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite"
ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite"

dates=Reduce(intersect, list(list.files(modisDir),list.files(viirsDir),list.files(gsmDir),list.files(avwDir),list.files(ociDir)))

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd") %>% grep(paste(dates,collapse="|"),., value=TRUE) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% grep(paste(dates,collapse="|"),., value=TRUE)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
gsmE=list.files(gsmDir,full.names = T,recursive = T,pattern = ".grd") %>% grep(paste(dates,collapse="|"),., value=TRUE)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
avwE=list.files(avwDir,full.names = T,recursive = T,pattern = ".grd") %>% grep(paste(dates,collapse="|"),., value=TRUE)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
ociE=list.files(ociDir,full.names = T,recursive = T,pattern = ".grd") %>% grep(paste(dates,collapse="|"),., value=TRUE)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

modisE[!is.na(modisE)]=1
viirsE[!is.na(viirsE)]=1
gsmE[!is.na(gsmE)]=1
avwE[!is.na(avwE)]=1
ociE[!is.na(ociE)]=1

modisE=modisE %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
viirsE=viirsE %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
gsmE=gsmE %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
avwE=avwE %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
ociE=ociE %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 

master=stack(modisE,viirsE,gsmE,avwE,ociE)
master2=master/length(dates)*100

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_12.01.19/"

PlotPNGs_difference<-function(stack,datatype,outputDir){
  
  ChlorCols<-colorRampPalette(brewer.pal(11,'Spectral'))
  col=ChlorCols(255)
  
  H=maxValue(stack) %>% max()
  L=minValue(stack) %>% min()
  zlimits=c(L,H)
  
  ####### produce png ####
  png(paste(outputDir,datatype,".png",sep=''),width=48,height=24,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,2))
  par(oma=c(0,0,0,1))
  
  #### MODIS - VIIRS ####
  
  
  image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[1]], add=TRUE, col="black",levels=c(300,330,360),labcex = 1,lwd=2)
  text(-123,45,"MODIS Chl-a",adj=c(0,0),cex=.6)
  
  image.plot(stack[[2]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[2]], add=TRUE, col="black",levels=c(300,330,360),labcex = 1,lwd=2)
  text(-123,45,"VIIRS Chl-a",adj=c(0,0),cex=.6)
  
  box()
  dev.off()
  
}
PlotPNGs_difference_percent<-function(stack,datatype,outputDir){
  
  ChlorCols<-colorRampPalette(brewer.pal(11,'Spectral'))
  col=ChlorCols(255)
  
  H=maxValue(stack) %>% max()
  L=minValue(stack) %>% min()
  zlimits=c(L,H)
  
  ####### produce png ####
  png(paste(outputDir,datatype,".png",sep=''),width=52,height=38,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(2,3))
  par(oma=c(0,0,0,1))
  
  #### MODIS - VIIRS ####
  
  image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.args = list(text="%",cex=1, side=3, line=1,adj=0))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[1]], add=TRUE, col="black",levels=c(95,90),labcex = 1,lwd=2)
  text(-122,45,"MODIS Chl-a",adj=c(0,0),cex=1.5)
  
  image.plot(stack[[2]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.args = list(text="%",cex=1, side=3, line=1,adj=0))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[2]], add=TRUE, col="black",levels=c(95,90),labcex = 1,lwd=2)
  text(-122,45,"VIIRS Chl-a",adj=c(0,0),cex=1.5)
  
  image.plot(stack[[3]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.args = list(text="%",cex=1, side=3, line=1,adj=0))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[3]], add=TRUE, col="black",levels=c(95,90),labcex = 1,lwd=2)
  text(-122,45,"Globcolour GSM Chl-a",adj=c(0,0),cex=1.5)
  
  image.plot(stack[[4]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.args = list(text="%",cex=1, side=3, line=1,adj=0))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[4]], add=TRUE, col="black",levels=c(95,90),labcex = 1,lwd=2)
  text(-122,45,"Globcolour AVW Chl-a",adj=c(0,0),cex=1.5)
  
  image.plot(stack[[5]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,legend.args = list(text="%",cex=1, side=3, line=1,adj=0))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[5]], add=TRUE, col="black",levels=c(95,90),labcex = 1,lwd=2)
  text(-122,45,"OC-CCI Chl-a",adj=c(0,0),cex=1.5)
  
  box()
  dev.off()
  
}

# datatype="data_density"
# PlotPNGs_difference(stack = master,datatype = datatype,outputDir = outputDir)

datatype="data_density_partial_timeseries"
PlotPNGs_difference_percent(stack = master2,datatype = datatype,outputDir = outputDir)

## % difference
m=cellStats(modisE,sum)
v=cellStats(viirsE,sum)
av=cellStats(avwE,sum)
gs=cellStats(gsmE,sum)
oc=cellStats(ociE,sum)

round(100-((v/m)*100),2)
round(100-((av/m)*100),2)
round(100-((gs/m)*100),2)
round(100-((oc/m)*100),2)

