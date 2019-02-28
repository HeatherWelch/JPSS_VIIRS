### spatial differences

### chla
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
modisC=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirsC=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
a=stack(modisC,viirsC) %>% as.data.frame() %>% gather() %>% mutate(value=scales::rescale(value,c(0,1)))
modis=a %>% filter(key=="layer.1")
viirs=a %>% filter(key=="layer.2")
modisC[]=modis$value
viirsC[]=viirs$value

### lbst
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs"
modisL=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirsL=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
a=stack(modisL,viirsL) %>% as.data.frame() %>% gather() %>% mutate(value=scales::rescale(value,c(0,1)))
modis=a %>% filter(key=="layer.1")
viirs=a %>% filter(key=="layer.2")
modisL[]=modis$value
viirsL[]=viirs$value

### ecocast
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)

a=stack(modisE,viirsE) %>% as.data.frame() %>% gather() %>% mutate(value=scales::rescale(value,c(0,1)))
modis=a %>% filter(key=="layer.1")
viirs=a %>% filter(key=="layer.2")
modisE[]=modis$value
viirsE[]=viirs$value

chla=modisC-viirsC
lbst=modisL-viirsL
ecocast=modisE-viirsE

master=stack(chla, lbst, ecocast)
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="spatial"
PlotPNGs_difference<-function(stack,datatype,outputDir){
  
  col=colorRamps:::blue2red(255)
  
  H=maxValue(stack) %>% max()
  L=minValue(stack) %>% min()
  # zlimits=c(L,H)
  zlimits=c(-.15,.15)
  
  ####### produce png ####
  png(paste(outputDir,datatype,"_difference.png",sep=''),width=18,height=6,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,3))
  par(oma=c(0,0,0,1))
  
  #### MODIS - VIIRS ####
  
  plusSD=rasterToPoints(stack[[1]],fun=function(x)x>cellStats(stack[[1]],mean)+cellStats(stack[[1]],sd))
  minusSD=rasterToPoints(stack[[1]],fun=function(x)x<cellStats(stack[[1]],mean)-cellStats(stack[[1]],sd))
  
  image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[1]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"MODIS - VIIRS CHLA",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[2]],fun=function(x)x>cellStats(stack[[2]],mean)+cellStats(stack[[2]],sd))
  minusSD=rasterToPoints(stack[[2]],fun=function(x)x<cellStats(stack[[2]],mean)-cellStats(stack[[2]],sd))
  
  image.plot(stack[[2]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[2]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"MODIS - VIIRS LBST",adj=c(0,0),cex=.8)
  
  plusSD=rasterToPoints(stack[[3]],fun=function(x)x>cellStats(stack[[3]],mean)+cellStats(stack[[3]],sd))
  minusSD=rasterToPoints(stack[[3]],fun=function(x)x<cellStats(stack[[3]],mean)-cellStats(stack[[3]],sd))
  
  image.plot(stack[[3]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=.7,pch = ".")
  points(minusSD,cex=.7,pch = ".")
  #contour(stack[[3]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-123,45,"MODIS - VIIRS ECO",adj=c(0,0),cex=.8)
  
  box()
  dev.off()
  
}

PlotPNGs_difference(stack = master,datatype = datatype,outputDir = outputDir)
