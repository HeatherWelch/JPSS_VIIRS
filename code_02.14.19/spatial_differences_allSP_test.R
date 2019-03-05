### spatial differences

dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
to_match_date=intersect(dates_m,dates_v)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"

PlotPNGs_difference<-function(stack,product,outputDir){
  
  col=colorRamps:::blue2red(255)
  
  stackM=stack %>% calc(.,mean,na.rm=T)
  
  H=maxValue(stackM) %>% max()
  L=minValue(stackM) %>% min()
  Labs=abs(L)
  both=c(H,Labs)
  MaxVal=max(both) 
  # zlimits=c(L,H)
  zlimits=c((MaxVal*-1),H)
  
  ####### produce png ####
  png(paste(outputDir,product,"_M-V_difference.png",sep=''),width=24,height=24,units='cm',res=400)
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


### chla
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

chla=modisE-viirsE 
# chla=ecocast%>% calc(.,mean,na.rm=T)

PlotPNGs_difference(stack=chla,product = "chla",outputDir = outputDir)

### lbst
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

lbst=modisE-viirsE 
# lbst=ecocast%>% calc(.,mean,na.rm=T)

PlotPNGs_difference(stack=lbst,product = "lbst",outputDir = outputDir)

### swor
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/swor/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/swor/predCIs_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

swor=modisE-viirsE 
# swor=ecocast%>% calc(.,mean,na.rm=T)

PlotPNGs_difference(stack=swor,product = "swor",outputDir = outputDir)

### blshTr
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshTr/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshTr/predCIs_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

blshtrk=modisE-viirsE 
# blshtrk=ecocast%>% calc(.,mean,na.rm=T)

PlotPNGs_difference(stack=blshtrk,product = "blshtrk",outputDir = outputDir)

### blshObs
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/blshObs/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/blshObs/predCIs_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

blshobs=modisE-viirsE 
# blshobs=ecocast%>% calc(.,mean,na.rm=T)

PlotPNGs_difference(stack=blshobs,product = "blshobs",outputDir = outputDir)

### casl
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/casl/predCIs_mask"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/casl/predCIs_mask"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

casl=modisE-viirsE 
# casl=ecocast%>% calc(.,mean,na.rm=T)

PlotPNGs_difference(stack=casl,product = "casl",outputDir = outputDir)


### ecocast
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"

modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)
modisE=unique (grep(paste(to_match_date,collapse="|"),modisE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) 
viirsE=unique (grep(paste(to_match_date,collapse="|"),viirsE, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

ecocast=modisE-viirsE 

PlotPNGs_difference(stack=ecocast,product = "ecocast",outputDir = outputDir)

###############################################################




# ecocast=ecocast%>% calc(.,mean,na.rm=T)
# 
# master=stack(chla, blshobs, blshtrk, casl, lbst, swor, ecocast)
# a=master %>% as.data.frame() %>% gather() %>%  mutate(value=scales::rescale(value,to=c(-1,1)))
# chla1=a %>% filter(key=="layer.1")
# blshobs1=a %>% filter(key=="layer.2")
# blshtrk1=a %>% filter(key=="layer.3")
# casl1=a %>% filter(key=="layer.4")
# lbst1=a %>% filter(key=="layer.5")
# swor1=a %>% filter(key=="layer.6")
# ecocast1=a %>% filter(key=="layer.7")
# 
# chla[]=chla1$value
# blshobs[]=blshobs1$value
# blshtrk[]=blshtrk1$value
# casl[]=casl1$value
# lbst[]=lbst1$value
# swor[]=swor1$value
# ecocast[]=ecocast1$value
# 
# master=stack(chla, blshobs, blshtrk, casl, lbst, swor, ecocast)

# PlotPNGs_difference<-function(stack,datatype,outputDir){
#   
#   col=colorRamps:::blue2red(255)
#   
#   H=maxValue(stack) %>% max()
#   L=minValue(stack) %>% min()
#   # zlimits=c(L,H)
#   zlimits=c(-.52,.52)
#   
#   ####### produce png ####
#   png(paste(outputDir,datatype,"_difference.png",sep=''),width=25,height=12,units='cm',res=400)
#   par(mar=c(3,3,.5,.5),las=1,font=2)
#   par(mfrow=c(2,4))
#   par(oma=c(0,0,0,1))
#   
#   #### MODIS - VIIRS ####
#   
#   plusSD=rasterToPoints(stack[[1]],fun=function(x)x>cellStats(stack[[1]],mean)+cellStats(stack[[1]],sd))
#   minusSD=rasterToPoints(stack[[1]],fun=function(x)x<cellStats(stack[[1]],mean)-cellStats(stack[[1]],sd))
#   
#   image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
#   maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#   points(plusSD,cex=.7,pch = ".")
#   points(minusSD,cex=.7,pch = ".")
#   #contour(stack[[1]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
#   text(-123,45,"MODIS - VIIRS CHLA",adj=c(0,0),cex=.8)
#   
#   plusSD=rasterToPoints(stack[[2]],fun=function(x)x>cellStats(stack[[2]],mean)+cellStats(stack[[2]],sd))
#   minusSD=rasterToPoints(stack[[2]],fun=function(x)x<cellStats(stack[[2]],mean)-cellStats(stack[[2]],sd))
#   
#   image.plot(stack[[2]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
#   maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#   points(plusSD,cex=.7,pch = ".")
#   points(minusSD,cex=.7,pch = ".")
#   #contour(stack[[2]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
#   text(-123,45,"MODIS - VIIRS blshobs",adj=c(0,0),cex=.8)
#   
#   plusSD=rasterToPoints(stack[[3]],fun=function(x)x>cellStats(stack[[3]],mean)+cellStats(stack[[3]],sd))
#   minusSD=rasterToPoints(stack[[3]],fun=function(x)x<cellStats(stack[[3]],mean)-cellStats(stack[[3]],sd))
#   
#   image.plot(stack[[3]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
#   maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#   points(plusSD,cex=.7,pch = ".")
#   points(minusSD,cex=.7,pch = ".")
#   #contour(stack[[3]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
#   text(-123,45,"MODIS - VIIRS blshtrk",adj=c(0,0),cex=.8)
#   
#   plusSD=rasterToPoints(stack[[4]],fun=function(x)x>cellStats(stack[[4]],mean)+cellStats(stack[[4]],sd))
#   minusSD=rasterToPoints(stack[[4]],fun=function(x)x<cellStats(stack[[4]],mean)-cellStats(stack[[4]],sd))
#   
#   image.plot(stack[[4]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
#   maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#   points(plusSD,cex=.7,pch = ".")
#   points(minusSD,cex=.7,pch = ".")
#   #contour(stack[[4]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
#   text(-123,45,"MODIS - VIIRS casl",adj=c(0,0),cex=.8)
#   
#   plusSD=rasterToPoints(stack[[5]],fun=function(x)x>cellStats(stack[[5]],mean)+cellStats(stack[[5]],sd))
#   minusSD=rasterToPoints(stack[[5]],fun=function(x)x<cellStats(stack[[5]],mean)-cellStats(stack[[5]],sd))
#   
#   image.plot(stack[[5]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
#   maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#   points(plusSD,cex=.7,pch = ".")
#   points(minusSD,cex=.7,pch = ".")
#   #contour(stack[[5]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
#   text(-123,45,"MODIS - VIIRS lbst",adj=c(0,0),cex=.8)
#   
#   plusSD=rasterToPoints(stack[[6]],fun=function(x)x>cellStats(stack[[6]],mean)+cellStats(stack[[6]],sd))
#   minusSD=rasterToPoints(stack[[6]],fun=function(x)x<cellStats(stack[[6]],mean)-cellStats(stack[[6]],sd))
#   
#   image.plot(stack[[6]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
#   maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#   points(plusSD,cex=.7,pch = ".")
#   points(minusSD,cex=.7,pch = ".")
#   #contour(stack[[6]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
#   text(-123,45,"MODIS - VIIRS swor",adj=c(0,0),cex=.8)
#   
#   plusSD=rasterToPoints(stack[[7]],fun=function(x)x>cellStats(stack[[7]],mean)+cellStats(stack[[7]],sd))
#   minusSD=rasterToPoints(stack[[7]],fun=function(x)x<cellStats(stack[[7]],mean)-cellStats(stack[[7]],sd))
#   
#   image.plot(stack[[7]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
#   maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#   points(plusSD,cex=.7,pch = ".")
#   points(minusSD,cex=.7,pch = ".")
#   #contour(stack[[7]], add=TRUE, col="black",levels=c(-.2,.2),lwd=.5,labcex=.3)
#   text(-123,45,"MODIS - VIIRS ECO",adj=c(0,0),cex=.8)
#   
#   box()
#   dev.off()
#   
# }

# PlotPNGs_difference(stack = master,datatype = datatype,outputDir = outputDir)
