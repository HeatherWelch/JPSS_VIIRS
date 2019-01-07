### getting stuff together for the quarterly report
# written by HW 01.04.18

# Download a day of VIIRS, run EcoCast, quick cell summary to same day EcoCast MODIS
# Download a day of NASA VIIRS, quick cell summary to same day NOAA VIIRS
# start batch download of NOAA VIIRS 2012 2015

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
source("/Volumes/EcoCast_SeaGate/EcoCast_HW/EcoCastGit_private/Code/Operationalizing_code_V3/4_predict_CIs.R",chdir = TRUE)
source("/Volumes/EcoCast_SeaGate/EcoCast_HW/EcoCastGit_private/Code/Operationalizing_code_V3/5_plot_EcoCast.R",chdir = TRUE)

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
envdir="/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/2012-08-10_viirs/" 

outdir <- outdir <- paste(path,"/EcoCastRuns/",sep="")
staticdir=paste0(path,"/static_variables/")
ecocastdir="/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/2012-08-10_ecocast/";dir.create(ecocastdir)
moddir<-paste(path,"/ModRepFiles/",sep="")

get_date="2012-08-01"
outdir2012="/Volumes/EcoCast_SeaGate/ERD_DOM/ncdf"
tmpdir="/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/data"
finaldir=envdir

namesrisk<-c("Blue shark bycatch","Blue sharks","Sea lions","Leatherbacks","Swordfish")
ecocastrisk<-c(-0.2,-0.2,-0.05,-0.9,0.9) #upweight swordfish a bit
bycatchrisk<-c(-0.1,-0.1,-0.1,-0.7,0) #all non-target species

waitfor <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  print(proc.time() - p1) # The cpu usage should be negligible
}

template=raster() ##create template for resampling
res(template)=0.2487562
ncol(template)=201
nrow(template)=201
xmin(template)=-149.875
xmax(template)=-99.875
ymin(template)=10.125
ymax(template)=60.125
projection(template)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Download a day of VIIRS, run EcoCast, quick cell summary to same day EcoCast MODIS ####

#NOAA VIIRS - https://upwell.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaDaily.html
dates<-seq(as.Date("2015-08-01"), as.Date("2015-08-01"), by = "day",format="%Y/%mm/%dd")

i<-1
  startdate<-dates[i]
  filenm<-paste(outdir2012,"/nesdisVHNSQchlaDaily_",startdate,".nc",sep="")
  url<-paste("http://upwell.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaDaily.nc?chlor_a[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(0.0):1:(0.0)][(60):1:(10.0)][(-150):1:(-100)]",sep="")
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
  close(f)
  
  
  #NASA VIIRS - https://upwell.pfeg.noaa.gov/erddap/griddap/erdVH2018chla1day.html
  dates<-seq(as.Date("2015-08-01"), as.Date("2015-08-01"), by = "day",format="%Y/%mm/%dd")

    i<-1
  startdate<-dates[i]
  filenm<-paste(outdir2012,"/erdVH2018chla1day",startdate,".nc",sep="")
  url<-paste("http://upwell.pfeg.noaa.gov/erddap/griddap/erdVH2018chla1day.nc?chla[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(60):1:(10.0)][(-150):1:(-100)]",sep="")
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
  close(f)
  
  setwd("~/Dropbox/JPSS/JPSS_VIIRS/data")
  a=list.files()
  b=raster(a[3])
  
 chla=raster(paste(tmpdir,"/nesdisVHNSQchlaDaily_2012-08-01.nc",sep=""))
  r=log(raster::resample(chla, template, method="bilinear")+0.001)
  writeRaster(r,paste("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/2012-08-10_viirs","/l.blendChl",sep=""))
  
  move_file=function(final_name){
    if(file.exists(paste(envdir,most_recent,"/",final_name,".grd",sep=""))==TRUE){ # if url isn't successfully found, start checking for older layers, but check mtime of layer to make sure it's within 3 days window
      print(paste(final_name," doesn't exist for ",get_date,", using most recent file instead (",most_recent,").",sep=""))
      file_path=paste(envdir,most_recent,"/",final_name,".grd",sep="")
    }else if (file.exists(paste(envdir,as.Date(most_recent)-1,"/",final_name,".grd",sep=""))==TRUE){
      print(paste(final_name," doesn't exist for ",get_date,", using most recent file instead (",as.Date(most_recent)-1,").",sep=""))
      file_path=paste(envdir,as.Date(most_recent)-1,"/",final_name,".grd",sep="")
    }else if (file.exists(paste(envdir,as.Date(most_recent)-2,"/",final_name,".grd",sep=""))==TRUE){
      print(paste(final_name," doesn't exist for ",get_date,", using most recent file instead (",as.Date(most_recent)-2,").",sep=""))
      file_path=paste(envdir,as.Date(most_recent)-2,"/",final_name,".grd",sep="")
    }else{
      print(paste(final_name," not available within the past three days, EcoCast run for ",get_date," will not include ",final_name,sep=""))
      file_path=NULL
    }
    return(file_path)
  }
  
  ############ 4. Define global objects
  studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")
  FileList_get_date=list.files(paste(envdir,sep=""),pattern="*.grd$") # all the files from get_date
  FileList_full=c("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd") # all of the dynamic variables, static ones will always be there
  FileList_missing=setdiff(FileList_full,FileList_get_date) # list of dynamic variables missing from get_date
  FileList_final=list.files(paste(envdir,sep=""),pattern="*.grd$",full.names = TRUE) # start of final list to pass to preCIs script
  
  return_list=list("FileList_final"=FileList_final,"FileList_missing"=FileList_missing) 
  Run_ecocast(get_date=get_date,moddir=moddir,outdir = outdir,ecocastdir = ecocastdir,namesrisk=namesrisk,ecocastrisk=ecocastrisk,bycatchrisk=bycatchrisk,final_path_list=return_list,logodir=logodir,studyarea=studyarea,staticdir=staticdir)
  
  setwd("~/Dropbox/JPSS/JPSS_VIIRS/2012-08-10_ecocast/mean")
  a=list.files(pattern = ".grd")
  virrs=raster(a[2])
  modis=raster(a[1])%>%mask(.,studyarea)%>% crop(.,extent(studyarea))
  
  EcoCols<-colorRampPalette(c("red","orange","white","cyan","blue"))
  ByCols<-colorRampPalette(c("red","orange","white"))
  SeCols<-colorRampPalette(c("coral3","cadetblue3","white","cadetblue3","coral3"))
  
  PlotEcoROMS<-function(r,get_date,outdir,leg=TRUE,scalbar=FALSE,risk=weightings,spp=namesrisk,version="_V1",contourval=NA,addLCA=FALSE,addtext=TRUE){
    
    ####### produce png - unscaled
    png(paste(outdir,"EcoROMS_original_unscaled_",paste(risk,collapse="_"),'_',get_date,version,'.png',sep=''),width=960,height=1100,units='px',pointsize=20)
    par(mar=c(3,3,.5,.5),las=1,font=2)
    
    if (version=="_se") {
      zlimits<-c(-0.2,0.2)
      col=SeCols(255)}
    
    if(risk[5]!=0 && version=="_mean") {
      zlimits=c(-1,1)
      col=EcoCols(255)
      #r1=rasterRescale(r)
    }
    
    if(risk[5]==0 && version=="_mean") {
      zlimits=c(-1,1)
      col=ByCols(255)
      #r1<-alt_rasterRescale2(r)
    }
    
    if (leg) {
      image.plot(r,col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
    } else {
      image(r,col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits) ## PRESABS
    }
    if(scalbar) scalebar(110,type="bar", divs=2,below="kilometers")
    if(!is.na(contourval)) {
      SP <- rasterToPolygons(clump(clipLand(r)<(contourval)), dissolve=TRUE)
      plot(SP, add=TRUE)
    }
    if(addLCA) {
      pl <- rbind(c(-121,36.3064), c(-123.583,34.45), c(-129,34.45), c(-129,45), c(-121,45))
      pl <- SpatialPolygons(list(Polygons(list(Polygon(pl)), 1)))
      projection(pl) <- projstring
      plot(pl, border="dark grey", add=TRUE, lty=3, lwd=4)
    }
    
    
    
    maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
    if (addtext) {
      text(-122,46,get_date,adj=c(0,0),cex=2) 
      text(-122,45,"Species weightings",adj=c(0,0),cex=1)
      #text(-122,45,paste(namesrisk[1],' weighting = ',risk[1],sep=''),adj=c(0,0),cex=.75)
      text(-122,44.5,paste(namesrisk[2],' weighting = ',risk[2],sep=''),adj=c(0,0),cex=.75)
      text(-122,44,paste(namesrisk[3],' weighting = ',risk[3],sep=''),adj=c(0,0),cex=.75)
      text(-122,43.5,paste(namesrisk[4],' weighting = ',risk[4],sep=''),adj=c(0,0),cex=.75)
      text(-122,43,paste(namesrisk[5],' weighting = ',risk[5],sep=''),adj=c(0,0),cex=.75)
      text(-122,42,"EcoROMS original (unscaled)",adj=c(0,0),cex=1)
      
    }
    
    box()
    dev.off()
    
    
    ####### produce raster
    #writeRaster(r,filename=paste(wd,'/EcoROMS_original_unscaled_',paste(risk,collapse="_"),"_",get_date,version,'.grd',sep=''),overwrite=TRUE) 
    
  }
  
 PlotEcoROMS(r=virrs,get_date = get_date,outdir = ecocastdir,risk=ecocastrisk,version="_mean")
 PlotEcoROMS(r=modis,get_date = get_date,outdir = ecocastdir,risk=ecocastrisk,version="_mean")
 
 difference=modis-virrs
 PlotEcoROMS(r=difference,get_date = get_date,outdir = ecocastdir,risk=ecocastrisk,version="_se")
  