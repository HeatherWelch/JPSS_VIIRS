### spatial differences

### chla
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"
modisC=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirsC=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
a=stack(modisC,viirsC) %>% as.data.frame() %>% gather() %>% mutate(value=scales::rescale(value,c(0,1)))
modis=a %>% filter(key=="layer.1")
viirs=a %>% filter(key=="layer.2")
modisC[]=modis$value
viirsC[]=viirs$value

### lbst
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs_mask"
modisL=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirsL=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
a=stack(modisL,viirsL) %>% as.data.frame() %>% gather() %>% mutate(value=scales::rescale(value,c(0,1)))
modis=a %>% filter(key=="layer.1")
viirs=a %>% filter(key=="layer.2")
modisL[]=modis$value
viirsL[]=viirs$value

### swor
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/swor/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/swor/predCIs_mask"
modisS=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirsS=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
a=stack(modisS,viirsS) %>% as.data.frame() %>% gather() %>% mutate(value=scales::rescale(value,c(0,1)))
modis=a %>% filter(key=="layer.1")
viirs=a %>% filter(key=="layer.2")
modisS[]=modis$value
viirsS[]=viirs$value

### blshTr
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshTr/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshTr/predCIs_mask"
modisblshTr=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirsblshTr=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
a=stack(modisblshTr,viirsblshTr) %>% as.data.frame() %>% gather() %>% mutate(value=scales::rescale(value,c(0,1)))
modis=a %>% filter(key=="layer.1")
viirs=a %>% filter(key=="layer.2")
modisblshTr[]=modis$value
viirsblshTr[]=viirs$value

### blshObs
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshObs/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshObs/predCIs_mask"
modisblshObs=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirsblshObs=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
a=stack(modisblshObs,viirsblshObs) %>% as.data.frame() %>% gather() %>% mutate(value=scales::rescale(value,c(0,1)))
modis=a %>% filter(key=="layer.1")
viirs=a %>% filter(key=="layer.2")
modisblshObs[]=modis$value
viirsblshObs[]=viirs$value

### casl
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/casl/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/casl/predCIs_mask"
modiscasl=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
viirscasl=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
a=stack(modiscasl,viirscasl) %>% as.data.frame() %>% gather() %>% mutate(value=scales::rescale(value,c(0,1)))
modis=a %>% filter(key=="layer.1")
viirs=a %>% filter(key=="layer.2")
modiscasl[]=modis$value
viirscasl[]=viirs$value

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

a=stack(modisE,viirsE) %>% as.data.frame() %>% gather() %>% mutate(value=scales::rescale(value,c(0,1)))
modis=a %>% filter(key=="layer.1")
viirs=a %>% filter(key=="layer.2")
modisE[]=modis$value
viirsE[]=viirs$value

chla=modisC-viirsC
lbst=modisL-viirsL
swor=modisS-viirsS
casl=modiscasl-viirscasl
blshobs=modisblshObs-viirsblshObs
blshtrk=modisblshTr-viirsblshTr
ecocast=modisE-viirsE

#master=stack(chla, blshobs, blshtrk, casl, lbst, swor, ecocast)
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"

PlotPNGs_difference<-function(stack,product,outputDir){
  
  col=colorRamps:::blue2red(255)
  
  H=maxValue(stackM) %>% max()
  L=minValue(stackM) %>% min()
  Labs=abs(L)
  both=c(H,Labs)
  MaxVal=max(both) 
  # zlimits=c(L,H)
  zlimits=c((MaxVal*-1),H)
  
  ####### produce png ####
  png(paste(outputDir,product,"_difference_original.png",sep=''),width=24,height=24,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,1))
  par(oma=c(0,0,0,1))
  
  #### MODIS - VIIRS ####
  
  plusSD=rasterToPoints(stack[[1]],fun=function(x)x>cellStats(stack,mean)+cellStats(stack,sd))
  minusSD=rasterToPoints(stack[[1]],fun=function(x)x<cellStats(stack,mean)-cellStats(stack,sd))
  
  image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  points(plusSD,cex=5,pch = ".")
  points(minusSD,cex=5,pch = ".")
  #contour(stack[[1]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-120,45,product,adj=c(0,0),cex=2)
  
  box()
  dev.off()
  
}

PlotPNGs_difference(stack = chla,product = "chla",outputDir = outputDir)
PlotPNGs_difference(stack = lbst,product = "lbst",outputDir = outputDir)
PlotPNGs_difference(stack = swor,product = "swor",outputDir = outputDir)
PlotPNGs_difference(stack = casl,product = "casl",outputDir = outputDir)
PlotPNGs_difference(stack = blshobs,product = "blshobs",outputDir = outputDir)
PlotPNGs_difference(stack = blshtrk,product = "blshtrk",outputDir = outputDir)
PlotPNGs_difference(stack = ecocast,product = "ecocast",outputDir = outputDir)
