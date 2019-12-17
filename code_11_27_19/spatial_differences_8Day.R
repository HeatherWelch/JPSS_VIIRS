### spatial difference - 2 columns by two rows
#3. chla difference
#6. ecocast difference

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_12.01.19/"

# library(scales)
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
# library(plotly)
library(tidyverse)
library(raster)
library(rgdal)

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

fcnRescale=function(i){
  a <- (i - min(i[], na.rm=TRUE))/(max(i[], na.rm=TRUE)-min(i[], na.rm=TRUE))
}

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask_resub_V3"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask_resub_V3"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite_mask_resub_V3"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite_mask_resub_V3"
ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite_mask_resub_V3"

to_match_date=Reduce(intersect, list(list.files(modisDir),list.files(viirsDir),list.files(gsmDir),list.files(avwDir),list.files(ociDir)))




#3. spatial differences ####
### chla ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask_resub_V3"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask_resub_V3"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite_mask_resub_V3"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite_mask_resub_V3"
ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite_mask_resub_V3"

#1. read in and stack
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
gsmE=list.files(gsmDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
avwE=list.files(avwDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
ociE=list.files(ociDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

# viirs
rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]
viirs=modisE1-viirsE1 

# gsm
rescaledStack=fcnRescale(stack(modisE,gsmE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]
gsm=modisE1-viirsE1 

# avw
rescaledStack=fcnRescale(stack(modisE,avwE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]
avw=modisE1-viirsE1  

# oci
rescaledStack=fcnRescale(stack(modisE,ociE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]
oci=modisE1-viirsE1

### ecocast ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns_resub_V3/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns_resub_V3/output/mean"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/EcoCastRuns_resub_V3/output/mean"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/EcoCastRuns_resub_V3/output/mean"
ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/EcoCastRuns_resub_V3/output/mean"

#1. read in and stack
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
gsmE=list.files(gsmDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
avwE=list.files(avwDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
ociE=list.files(ociDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

# viirs
rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]
viirsEco=modisE1-viirsE1 

# gsm
rescaledStack=fcnRescale(stack(modisE,gsmE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]
gsmEco=modisE1-viirsE1 

# avw
rescaledStack=fcnRescale(stack(modisE,avwE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]
avwEco=modisE1-viirsE1  

# oci
rescaledStack=fcnRescale(stack(modisE,ociE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]
ociEco=modisE1-viirsE1

#### plot all ####

# plotting=stack(calc(viirs,mean,na.rm=T),calc(gsm,mean,na.rm=T),calc(avw,mean,na.rm=T),calc(oci,mean,na.rm=T))
plotting=stack(mean(viirs,na.rm=T),mean(gsm,na.rm=T),mean(avw,na.rm=T),mean(oci,na.rm=T),mean(viirsEco,na.rm=T),mean(gsmEco,na.rm=T),mean(avwEco,na.rm=T),mean(ociEco,na.rm=T))
H=max(plotting[],na.rm=T)
L=min(plotting[],na.rm=T)

PlotPNGs_difference<-function(stack,product,outputDir,H,L,countour_ras){
  
  col=colorRamps:::blue2red(255)
  
  stackM=stack %>% mean(.,na.rm=T)
  
  Labs=abs(L)
  both=c(H,Labs)
  MaxVal=max(both) 
  # zlimits=c(L,H)
  zlimits=c((MaxVal*-1),H)
  
  ####### produce png ####
  png(paste(outputDir,product,".png",sep=''),width=15,height=15,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,1))
  par(oma=c(0,0,0,1))
  
  #### MODIS - VIIRS ####
  
  stackSD=stack %>% mean(.,na.rm=T) %>% cellStats(.,sd)
  stackMP=stackM %>% raster::cellStats(.,"mean",na.rm=T)
  #postive=stackM[stackM>(stackM+stackSD)]
  
  plusSD=rasterToPoints(stackM,fun=function(x)x>(stackMP+stackSD))
  minusSD=rasterToPoints(stackM,fun=function(x)x<(stackMP-stackSD))
  
  image.plot(stackM,col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits,cex.axis=1.8,axis.args=list(cex.axis=1.8))
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=5,pch = ".")
  points(minusSD,cex=5,pch = ".")
  # contour(countour_ras, add=TRUE, col="black",levels=c(-.51),labcex = 1,lwd=2)
  text(-123.5,45,product,adj=c(0,0),cex=1)
  
  box()
  dev.off()
  
}


viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) 
countour_ras=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))%>% mean(.,na.rm=T)
countour_ras=countour_ras*.4343

PlotPNGs_difference(stack = oci,product = "OC-CCI Chl-a",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)
PlotPNGs_difference(stack = avw,product = "Globcolour AVW Chl-a",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)
PlotPNGs_difference(stack = gsm,product = "Globcolour GSM Chl-a",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)
PlotPNGs_difference(stack = viirs,product = "VIIRS Chl-a",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)

PlotPNGs_difference(stack = ociEco,product = "OC-CCI EcoCast",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)
PlotPNGs_difference(stack = avwEco,product = "Globcolour AVW EcoCast",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)
PlotPNGs_difference(stack = gsmEco,product = "Globcolour GSM EcoCast",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)
PlotPNGs_difference(stack = viirsEco,product = "VIIRS EcoCast",outputDir = outputDir,H=H,L=L,countour_ras = countour_ras)
