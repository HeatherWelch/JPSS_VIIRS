### data density all years
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
to_match_date=intersect(dates_m,dates_v)

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

modis=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)
modis=unique (grep(paste(to_match_date,collapse="|"),modis, value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 

modis[!is.na(modis)]=1
modis=modis %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 

viirs=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)
viirs=unique (grep(paste(to_match_date,collapse="|"),viirs, value=TRUE))%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 

viirs[!is.na(viirs)]=1
viirs=viirs %>% calc(.,fun=sum,na.rm=T)%>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 

master=stack(modis,viirs)
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="data_density_allyears"
PlotPNGs_difference<-function(stack,datatype,outputDir){
  
  ChlorCols<-colorRampPalette(brewer.pal(9,'YlGn'))
  col=ChlorCols(255)
  
  H=maxValue(stack) %>% max()
  L=minValue(stack) %>% min()
zlimits=c(L,H)
  
  ####### produce png ####
  png(paste(outputDir,datatype,"_difference.png",sep=''),width=14,height=6,units='cm',res=400)
  par(mar=c(3,3,.5,.5),las=1,font=2)
  par(mfrow=c(1,2))
  par(oma=c(0,0,0,1))
  
  #### MODIS - VIIRS ####

  
  image.plot(stack[[1]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[1]], add=TRUE, col="white",levels=c(300,330,360),lwd=.5,labcex=.3)
  text(-123,45,"modis",adj=c(0,0),cex=.6)
  
  image.plot(stack[[2]],col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  contour(stack[[2]], add=TRUE, col="white",levels=c(00,330,360),lwd=.5,labcex=.3)
  text(-123,45,"viirs",adj=c(0,0),cex=.6)
  
  box()
  dev.off()
  
}

PlotPNGs_difference(stack = master,datatype = datatype,outputDir = outputDir)

## % difference
m=cellStats(modis,sum)
v=cellStats(viirs,sum)
diff=m-v
100-((v/m)*100)
