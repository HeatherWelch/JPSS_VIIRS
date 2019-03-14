modis=raster("/Users/heatherwelch/Dropbox/JPSS/global/global_averages/modis.grd") 
viirs=raster("/Users/heatherwelch/Dropbox/JPSS/global/global_averages/viirs.grd")
glo_avw=raster("/Users/heatherwelch/Dropbox/JPSS/global/global_averages/glo_avw.grd")
glo_gsm=raster("/Users/heatherwelch/Dropbox/JPSS/global/global_averages/glo_gsm.grd")
esa=raster("/Users/heatherwelch/Dropbox/JPSS/global/global_averages/esa.grd")

modis=log(modis+0.001)
viirs=log(viirs+0.001)
glo_avw=log(glo_avw+0.001)
glo_gsm=log(glo_gsm+0.001)
esa=log(esa+0.001)

modis[modis>2]<-2
viirs[viirs>2]<-2
glo_avw[glo_avw>2]<-2
glo_gsm[glo_gsm>2]<-2
esa[esa>2]<-2

viirs=raster::resample(viirs,modis)
glo_avw=raster::resample(glo_avw,modis)
glo_gsm=raster::resample(glo_gsm,modis)
esa=raster::resample(esa,modis)

modis[modis<(-3.5)]<-(-3.5)
viirs[viirs<(-3.5)]<-(-3.5)
glo_avw[glo_avw<(-3.5)]<-(-3.5)
glo_gsm[glo_gsm<(-3.5)]<-(-3.5)
esa[esa<(-3.5)]<-(-3.5)

master=stack(modis,viirs,glo_avw,glo_gsm,esa) %>% calc(.,fun=mean)
#writeRaster(master,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages/mean_all_products.grd")
master_sd=stack(modis,viirs,glo_avw,glo_gsm,esa) %>% calc(.,fun=sd)
#writeRaster(master_sd,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages/sd_all_products.grd")
plus=master+(master_sd*1.5)
minus=master-(master_sd*1.5)

plot_global=function(raster,datatype,plus,minus,outputDir){
  pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow", "red"))
  ncolors <- 255
  breaks <- seq(-3.5,2,,ncolors+1)
  
  png(paste(outputDir,datatype,"_global.png",sep=''),width=18,height=12,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,1))
  par(oma=c(0,0,0,2))
  
  mplus=raster-plus
  mplus[mplus<0]<-NA
  mplus=rasterToPoints(mplus)
  
  mminus=raster-minus
  mminus[mminus>0]<-NA
  mminus=rasterToPoints(mminus)
  
  image.plot(raster, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-180, 180),ylim=c(-90.00001, 90),legend.cex=.5)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  text(0,-80,datatype,adj=c(0,0),cex=1.5,col="black")
  points(mplus,cex=.02,pch = ".",col="grey")
  points(mminus,cex=.02,pch = ".",col="black")
  
  box()
  dev.off()
  
}

plot_global(raster=modis,datatype = "MODIS2",plus=plus,minus=minus,outputDir = outputDir)
plot_global(raster=viirs,datatype = "VIIRS2",plus=plus,minus=minus,outputDir = outputDir)
plot_global(raster=glo_avw,datatype = "GlobColour AVW2",plus=plus,minus=minus,outputDir = outputDir)
plot_global(raster=glo_gsm,datatype = "GlobColour GSM2",plus=plus,minus=minus,outputDir = outputDir)
plot_global(raster=esa,datatype = "OC-CCI2",plus=plus,minus=minus,outputDir = outputDir)


##modis trial
# mplus=modis-plus
# mplus[mplus>0]<-NA
# mplus=rasterToPoints(mplus)


# mplus=modis
# mplus[mplus<plus]=NA
# mplus=rasterToPoints(mplus)

mplus=modis-plus
mplus[mplus<0]<-NA
mplus=rasterToPoints(mplus)

# 
# mminus=modis
# mminus[mminus>minus]=NA
# mminus=rasterToPoints(mminus)

mminus=modis-minus
mminus[mminus>0]<-NA
mminus=rasterToPoints(mminus)


pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow", "red"))
ncolors <- 255
breaks <- seq(-3.5,2,,ncolors+1)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"
datatype="global_map_log"


png(paste(outputDir,datatype,".png",sep=''),width=90,height=12,units='cm',res=400)
par(mar=c(3,3,.5,.5),las=1,font=2)
par(mfrow=c(1,5))
par(oma=c(0,0,0,2))

image.plot(modis, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-180, 180),ylim=c(-90.00001, 90),legend.cex=.5)
#maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#contour(modis, add=TRUE, col="white",levels=c(-2,0),labcex = 1,lwd=2)
text(0,-80,"MODIS",adj=c(0,0),cex=1.5,col="black")
points(mplus,cex=.1,pch = ".")
points(mminus,cex=.1,pch = ".",col="black")

image.plot(viirs, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-180, 180),ylim=c(-90.00001, 90),legend.cex=.5)
#maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#contour(viirs, add=TRUE, col="white",levels=c(-2,-0),labcex = 1,lwd=2)
text(0,-80,"VIIRS",adj=c(0,0),cex=1.5,col="black")

image.plot(glo_avw, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-180, 180),ylim=c(-90.00001, 90),legend.cex=.5)
#maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#contour(glo_avw, add=TRUE, col="white",levels=c(-2,0),labcex = 1,lwd=2)
text(0,-80,"GlobColour AVW",adj=c(0,0),cex=1.5,col="black")

image.plot(glo_gsm, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-180, 180),ylim=c(-90.00001, 90),legend.cex=.5)
#maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#contour(glo_gsm, add=TRUE, col="white",levels=c(-2,0),labcex = 1,lwd=2)
text(0,-80,"GlobColour GSM",adj=c(0,0),cex=1.5,col="black")

image.plot(esa, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-180, 180),ylim=c(-90.00001, 90),legend.cex=.5)
#maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
#contour(esa, add=TRUE, col="white",levels=c(-2,0),labcex = 1,lwd=2)
text(0,-80,"OC-CCI",adj=c(0,0),cex=1.5,col="black")

box()
dev.off()

