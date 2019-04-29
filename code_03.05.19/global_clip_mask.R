#### making global plots

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")

### plotting log ####
modis=raster("/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/modis.grd") 
viirs=raster("/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/viirs.grd")
glo_avw=raster("/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/glo_avw.grd")
glo_gsm=raster("/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/glo_gsm.grd")
esa=raster("/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/esa.grd")

modis=log(modis+0.001)
viirs=log(viirs+0.001)
glo_avw=log(glo_avw+0.001)
glo_gsm=log(glo_gsm+0.001)
esa=log(esa+0.001)

master=stack(modis,viirs,glo_avw,glo_gsm,esa) %>% calc(.,fun=mean)
writeRaster(master,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/mean_all_products.grd")
master_sd=stack(modis,viirs,glo_avw,glo_gsm,esa) %>% calc(.,fun=sd)
writeRaster(master_sd,"/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/sd_all_products.grd")
plus=master+(master_sd*1.5)
minus=master-(master_sd*1.5)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"
plot_global=function(raster,datatype,plus,minus,outputDir){
  pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow", "red"))
  ncolors <- 255
  breaks <- seq(-4,3,,ncolors+1)
  
  png(paste(outputDir,datatype,"_globa_masked.png",sep=''),width=18,height=12,units='cm',res=400)
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
  maps::map('worldHires',add=TRUE,col="white",fill=TRUE)
  text(0,-83,datatype,adj=c(0,0),cex=1.5,col="black")
  points(mplus,cex=.02,pch = ".",col="grey")
  points(mminus,cex=.02,pch = ".",col="black")
  
  box()
  dev.off()
  
}

plot_global(raster=modis,datatype = "MODIS",plus=plus,minus=minus,outputDir = outputDir)
plot_global(raster=viirs,datatype = "VIIRS",plus=plus,minus=minus,outputDir = outputDir)
plot_global(raster=glo_avw,datatype = "GlobColour AVW",plus=plus,minus=minus,outputDir = outputDir)
plot_global(raster=glo_gsm,datatype = "GlobColour GSM",plus=plus,minus=minus,outputDir = outputDir)
plot_global(raster=esa,datatype = "OC-CCI",plus=plus,minus=minus,outputDir = outputDir)
