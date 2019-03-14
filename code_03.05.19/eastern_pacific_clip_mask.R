#### croping by Eastern Pacific Ocean
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
e=extent(-150, -120, 20, 50)
savedir="/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_rasters_mask/"#;dir.create(savedir)
rasterdir="/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask"

for(file in list.files(rasterdir,full.names = T,pattern = ".grd")){
  print(file)
  name=gsub("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/","",file)
  grid=raster(file)
  clip=crop(grid,e)
  writeRaster(clip,paste0(savedir,name),overwrite=T)
}


# temporal averages masked ####

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_rasters_mask",full.names = T)%>% grep("ESACCI-OC-L3S",.,value=T) %>% grep(".grd",.,value=T)
esa=stack(files) %>% raster::calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_rasters_mask",full.names = T)%>% grep("GlobColour_Merged_GSM",.,value=T)%>% grep(".grd",.,value=T)
glo_gsm=stack(files)%>% calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_rasters_mask",full.names = T)%>% grep("GlobColour_Merged_AVW",.,value=T)%>% grep(".grd",.,value=T)
glo_avw=stack(files)%>% calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_rasters_mask",full.names = T)%>% grep("nesdisVHNSQchlaMonthly",.,value=T)%>% grep(".grd",.,value=T)
viirs=stack(files)%>% calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_rasters_mask",full.names = T)%>% grep("erdMH1chlamday",.,value=T)
modis=stack(files)%>% calc(.,fun=mean,na.rm=T)

writeRaster(esa,"/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages_mask/esa.grd")
writeRaster(glo_gsm,"/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages_mask/glo_gsm.grd")
writeRaster(glo_avw,"/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages_mask/glo_avw.grd")
writeRaster(viirs,"/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages_mask/viirs.grd")
writeRaster(modis,"/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages_mask/modis.grd")

### plotting ####
modis=raster("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages_mask/modis.grd") 
viirs=raster("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages_mask/viirs.grd")
glo_avw=raster("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages_mask/glo_avw.grd")
glo_gsm=raster("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages_mask/glo_gsm.grd")
esa=raster("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages_mask/esa.grd")

modis=log(modis+0.001)
viirs=log(viirs+0.001)
glo_avw=log(glo_avw+0.001)
glo_gsm=log(glo_gsm+0.001)
esa=log(esa+0.001)

pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow", "red"))
ncolors <- 255
breaks <- seq(-4,3,,ncolors+1)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"
datatype="eastern_pacific_map_log_mask"

png(paste(outputDir,datatype,".png",sep=''),width=60,height=12,units='cm',res=400)
par(mar=c(3,3,.5,.5),las=1,font=2)
par(mfrow=c(1,5))

image.plot(modis, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-150, -120),ylim=c(20, 50),legend.cex=.5)
maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
contour(modis, add=TRUE, col="white",levels=c(.1,.2),labcex = 1,lwd=2)
text(-145,25,"MODIS",adj=c(0,0),cex=1.5,col="white")

image.plot(viirs, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-150, -120),ylim=c(20, 50),legend.cex=.5)
maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
contour(viirs, add=TRUE, col="white",levels=c(.1,.2),labcex = 1,lwd=2)
text(-145,25,"VIIRS",adj=c(0,0),cex=1.5,col="white")

image.plot(glo_avw, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-150, -120),ylim=c(20, 50),legend.cex=.5)
maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
contour(glo_avw, add=TRUE, col="white",levels=c(.1,.2),labcex = 1,lwd=2)
text(-145,25,"GlobColour AVW",adj=c(0,0),cex=1.5,col="white")

image.plot(glo_gsm, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-150, -120),ylim=c(20, 50),legend.cex=.5)
maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
contour(glo_gsm, add=TRUE, col="white",levels=c(.1,.2),labcex = 1,lwd=2)
text(-145,25,"GlobColour GSM",adj=c(0,0),cex=1.5,col="white")

image.plot(esa, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-150, -120),ylim=c(20, 50),legend.cex=.5)
maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
contour(esa, add=TRUE, col="white",levels=c(.1,.2),labcex = 1,lwd=2)
text(-145,25,"OC-CCI",adj=c(0,0),cex=1.5,col="white")

box()
dev.off()

### plotting log####
modis=raster("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages/modis.grd") 
viirs=raster("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages/viirs.grd")
glo_avw=raster("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages/glo_avw.grd")
glo_gsm=raster("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages/glo_gsm.grd")
esa=raster("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_averages/esa.grd")

modis=log(modis+0.001)
viirs=log(viirs+0.001)
glo_avw=log(glo_avw+0.001)
glo_gsm=log(glo_gsm+0.001)
esa=log(esa+0.001)

modis[modis>0]<-0
viirs[viirs>0]<-0
glo_avw[glo_avw>0]<-0
glo_gsm[glo_gsm>0]<-0
esa[esa>0]<-0

pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow", "red"))
ncolors <- 255
breaks <- seq(-3,0,,ncolors+1)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"
datatype="eastern_pacific_map_log"

png(paste(outputDir,datatype,".png",sep=''),width=60,height=12,units='cm',res=400)
par(mar=c(3,3,.5,.5),las=1,font=2)
par(mfrow=c(1,5))
par(oma=c(0,0,0,2))

image.plot(modis, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-150, -120),ylim=c(20, 50),legend.cex=.5)
maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
contour(modis, add=TRUE, col="white",levels=c(-2,-2.5),labcex = 1,lwd=2)
text(-145,25,"MODIS",adj=c(0,0),cex=1.5,col="white")

image.plot(viirs, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-150, -120),ylim=c(20, 50),legend.cex=.5)
maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
contour(viirs, add=TRUE, col="white",levels=c(-2,-2.5),labcex = 1,lwd=2)
text(-145,25,"VIIRS",adj=c(0,0),cex=1.5,col="white")

image.plot(glo_avw, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-150, -120),ylim=c(20, 50),legend.cex=.5)
maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
contour(glo_avw, add=TRUE, col="white",levels=c(-2,-2.5),labcex = 1,lwd=2)
text(-145,25,"GlobColour AVW",adj=c(0,0),cex=1.5,col="white")

image.plot(glo_gsm, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-150, -120),ylim=c(20, 50),legend.cex=.5)
maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
contour(glo_gsm, add=TRUE, col="white",levels=c(-2,-2.5),labcex = 1,lwd=2)
text(-145,25,"GlobColour GSM",adj=c(0,0),cex=1.5,col="white")

image.plot(esa, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-150, -120),ylim=c(20, 50),legend.cex=.5)
maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
contour(esa, add=TRUE, col="white",levels=c(-2,-2.5),labcex = 1,lwd=2)
text(-145,25,"OC-CCI",adj=c(0,0),cex=1.5,col="white")

box()
dev.off()
