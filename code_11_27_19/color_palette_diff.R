### blank map with rectangles
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")

master=raster("/Users/heatherwelch/Dropbox/JPSS/global/global_averages_mask/mean_all_products_log10.grd")
# raster=raster("/Users/heatherwelch/Dropbox/JPSS/global/SE_Asia_averages_mask/esa.grd")
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_04.14.19/"

png(paste(outputDir,"blank_map.png",sep=''),width=20,height=12,units='cm',res=400)
par(mar=c(3,3,.5,.5),las=1,font=2)
par(mfrow=c(1,1))
par(oma=c(0,0,0,0))

# pal <- colorRampPalette(c("white", "white"))
# ncolors <- 255
# breaks <- seq(-4,3,,ncolors+1)

pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow","red"))
ncolors <- 255
breaks <- seq(-1.967,1.7,,ncolors+1)

# image.plot(raster, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-180, 180),ylim=c(-90.00001, 90),legend.cex=.5)
image.plot(master, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=c(-180, 180),ylim=c(-90.00001, 90),legend.cex=.5)
maps::map('worldHires',col="white",fill=TRUE,add=T)
rect(95,-10,150, 20,border = "red",lwd=2)
rect(-80,10, 0, 50,border="red",lwd=2)
rect(-150,20,  -120, 50,border="red",lwd=2)


box()
dev.off()
