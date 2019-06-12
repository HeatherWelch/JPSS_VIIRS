#### croping by Eastern Pacific Ocean
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
e=extent(-80, 0, 10, 50)
savedir="/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_rasters_mask/"#;dir.create(savedir)
rasterdir="/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask"

for(file in list.files(rasterdir,full.names = T,pattern = ".grd")){
  print(file)
  name=gsub("/Users/heatherwelch/Dropbox/JPSS/global/global_rasters_mask/","",file)
  grid=raster(file)
  clip=crop(grid,e)
  writeRaster(clip,paste0(savedir,name),overwrite=T)
}


# temporal averages masked ####

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_rasters_mask",full.names = T)%>% grep("ESACCI-OC-L3S",.,value=T) %>% grep(".grd",.,value=T)
esa=stack(files) %>% raster::calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_rasters_mask",full.names = T)%>% grep("GlobColour_Merged_GSM",.,value=T)%>% grep(".grd",.,value=T)
glo_gsm=stack(files)%>% calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_rasters_mask",full.names = T)%>% grep("GlobColour_Merged_AVW",.,value=T)%>% grep(".grd",.,value=T)
glo_avw=stack(files)%>% calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_rasters_mask",full.names = T)%>% grep("nesdisVHNSQchlaMonthly",.,value=T)%>% grep(".grd",.,value=T)
viirs=stack(files)%>% calc(.,fun=mean,na.rm=T)

files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_rasters_mask",full.names = T)%>% grep("erdMH1chlamday",.,value=T)
modis=stack(files)%>% calc(.,fun=mean,na.rm=T)

writeRaster(esa,"/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/esa.grd")
writeRaster(glo_gsm,"/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/glo_gsm.grd")
writeRaster(glo_avw,"/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/glo_avw.grd")
writeRaster(viirs,"/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/viirs.grd")
writeRaster(modis,"/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/modis.grd")

# ### plotting ####
# modis=raster("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/modis.grd") 
# viirs=raster("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/viirs.grd")
# glo_avw=raster("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/glo_avw.grd")
# glo_gsm=raster("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/glo_gsm.grd")
# esa=raster("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/esa.grd")
# 
# modis=log(modis+0.001)
# viirs=log(viirs+0.001)
# glo_avw=log(glo_avw+0.001)
# glo_gsm=log(glo_gsm+0.001)
# esa=log(esa+0.001)
# 
# modis[modis>0]<-0
# viirs[viirs>0]<-0
# glo_avw[glo_avw>0]<-0
# glo_gsm[glo_gsm>0]<-0
# esa[esa>0]<-0
# 
# outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"
# plot_EP=function(raster,datatype,outputDir){
#   pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow", "red"))
#   ncolors <- 255
#   breaks <- seq(-3,0,,ncolors+1)
#   
#   png(paste(outputDir,datatype,"_north_atlantic_masked.png",sep=''),width=18,height=12,units='cm',res=400)
#   par(mar=c(3,3,.5,.5),las=1,font=2)
#   par(mfrow=c(1,1))
#   par(oma=c(0,0,0,1))
#   
#   image.plot(raster, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-150, -120),ylim=c(20, 50),legend.cex=.5)
#   maps::map('worldHires',add=TRUE,col="white",fill=TRUE)
#   contour(raster, add=TRUE, col="white",levels=c(-2,-2.5),labcex = 1,lwd=2)
#   text(-145,22,datatype,adj=c(0,0),cex=1.5,col="white")
#   
#   box()
#   dev.off()
#   
# }
# 
# plot_EP(raster=modis,datatype = "MODIS",outputDir = outputDir)
# plot_EP(raster=viirs,datatype = "VIIRS",outputDir = outputDir)
# plot_EP(raster=glo_avw,datatype = "GlobColour AVW",outputDir = outputDir)
# plot_EP(raster=glo_gsm,datatype = "GlobColour GSM",outputDir = outputDir)
# plot_EP(raster=esa,datatype = "OC-CCI",outputDir = outputDir)
# 


### plotting with standard deviations ####
modis=raster("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/modis.grd") 
viirs=raster("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/viirs.grd")
glo_avw=raster("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/glo_avw.grd")
glo_gsm=raster("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/glo_gsm.grd")
esa=raster("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/esa.grd")

modis=log10(modis+0.001)
viirs=log10(viirs+0.001)
glo_avw=log10(glo_avw+0.001)
glo_gsm=log10(glo_gsm+0.001)
esa=log10(esa+0.001)

master=stack(modis,viirs,glo_avw,glo_gsm,esa) %>% calc(.,fun=mean)
#writeRaster(master,"/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/mean_all_products.grd")
master_sd=stack(modis,viirs,glo_avw,glo_gsm,esa) %>% calc(.,fun=sd)
#writeRaster(master_sd,"/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_averages_mask/sd_all_products.grd")
plus=master+(master_sd*1.5)
minus=master-(master_sd*1.5)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_04.14.19/"
plot_EP_SD=function(raster,datatype,plus,minus,outputDir){
  pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow", "red"))
  ncolors <- 255
  breaks <- seq(-1.4,1.3,,ncolors+1)
  
  png(paste(outputDir,datatype,"_north_atlantic_masked_SD.png",sep=''),width=18,height=12,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,1))
  par(oma=c(0,0,0,2))
  
  mplus=raster-plus
  mplus[mplus<0]<-NA
  mplus=rasterToPoints(mplus)
  
  mminus=raster-minus
  mminus[mminus>0]<-NA
  mminus=rasterToPoints(mminus)
  
  image.plot(raster, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-80, 0),ylim=c(10, 50),legend.cex=.5)
  maps::map('worldHires',add=TRUE,col="white",fill=TRUE)
  points(mplus,cex=.02,pch = ".",col="grey")
  points(mminus,cex=.02,pch = ".",col="black")
  contour(raster, add=TRUE, col="white",levels=c(-.7),labcex = 1,lwd=2)
  text(-4,48,datatype,adj=c(0,0),cex=1.5,col="black",pos=2)
  
  box()
  dev.off()
  
}

plot_EP_SD(raster=modis,datatype = "MODIS",outputDir = outputDir,plus=plus,minus=minus)
plot_EP_SD(raster=viirs,datatype = "VIIRS",outputDir = outputDir,plus=plus,minus=minus)
plot_EP_SD(raster=glo_avw,datatype = "GlobColour AVW",outputDir = outputDir,plus=plus,minus=minus)
plot_EP_SD(raster=glo_gsm,datatype = "GlobColour GSM",outputDir = outputDir,plus=plus,minus=minus)
plot_EP_SD(raster=esa,datatype = "OC-CCI",outputDir = outputDir,plus=plus,minus=minus)
