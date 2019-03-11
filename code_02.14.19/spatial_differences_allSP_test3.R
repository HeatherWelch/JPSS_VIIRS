### spatial differences
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
to_match_date=intersect(dates_m,dates_v)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"

PlotPNGs_difference<-function(stack,product,outputDir,H,L){
  
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
  #contour(stack[[1]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
  text(-120,45,product,adj=c(0,0),cex=2)
  
  box()
  dev.off()
  
}

fcnRescale=function(i){
  a <- (i - min(i[], na.rm=TRUE))/(max(i[], na.rm=TRUE)-min(i[], na.rm=TRUE))
}


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

### lbst ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

lbst=modisE-viirsE 

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

### blshObs ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshObs/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshObs/predCIs_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

blshobs=modisE-viirsE 

### casl####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/casl/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/casl/predCIs_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE=rescaledStack[[grep(".2$",names(rescaledStack))]]

casl=modisE-viirsE 

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

plotting=stack(calc(chla,mean,na.rm=T),calc(lbst,mean,na.rm=T),calc(swor,mean,na.rm=T),calc(casl,mean,na.rm=T),calc(blshobs,mean,na.rm=T),calc(blshtrk,mean,na.rm=T),calc(ecocast,mean,na.rm=T))
H=max(plotting[],na.rm=T)
L=min(plotting[],na.rm=T)

PlotPNGs_difference(stack = chla,product = "chla",outputDir = outputDir,H=H,L=L)
PlotPNGs_difference(stack = lbst,product = "lbst",outputDir = outputDir,H=H,L=L)
PlotPNGs_difference(stack = swor,product = "swor",outputDir = outputDir,H=H,L=L)
PlotPNGs_difference(stack = casl,product = "casl",outputDir = outputDir,H=H,L=L)
PlotPNGs_difference(stack = blshobs,product = "blshobs",outputDir = outputDir,H=H,L=L)
PlotPNGs_difference(stack = blshtrk,product = "blshtrk",outputDir = outputDir,H=H,L=L)
PlotPNGs_difference(stack = ecocast,product = "ecocast",outputDir = outputDir,H=H,L=L)


