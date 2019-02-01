#### script to download NOAA VIIRS and MODIS as a function

waitfor <- function(x){
    p1 <- proc.time()
    Sys.sleep(x)
    print(proc.time() - p1) # The cpu usage should be negligible
}

modis=function(dates,modisDir,sucess){
# sucess=list()
waitsecs<-2
startdate<-dates
print(startdate)
filenm<-paste(modisDir,"/erdMBchla8day_LonPM180_",startdate,".nc",sep="")
url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMBchla8day_LonPM180.nc?chlorophyll[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(0.0):1:(0.0)][(10):1:(60.0)][(-150):1:(-100)]",sep="")
print(startdate)
f = CFILE(filenm,mode="wb")
curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
close(f)
waitfor(waitsecs)
if (file.info(filenm)$size > 2000){
  sucess=list(sucess,startdate)
}

return(sucess)
}

viirs=function(dates,viirsDir,sucess){
  waitsecs<-2
  startdate<-dates
  print(startdate)
  filenm<-paste(viirsDir,"/erdVHNchla8day_",startdate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla8day.nc?chla[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(0.0):1:(0.0)][(60):1:(10.0)][(-150):1:(-110.5)]",sep="")
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
  close(f)
  waitfor(waitsecs)
  if (file.info(filenm)$size > 2000){
    sucess=list(sucess,startdate)
  }
  
  return(sucess)
}


