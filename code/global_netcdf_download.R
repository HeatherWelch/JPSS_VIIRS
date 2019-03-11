#### downloading global MODIS and VIIRS for summary figure
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")

saveDir="/Users/heatherwelch/Dropbox/JPSS/global/raw_filezilla/"

waitfor <- function(x){
  p1 <- proc.time()
  Sys.sleep(x)
  print(proc.time() - p1) # The cpu usage should be negligible
}

### viirs ###
dates<-seq(as.Date("2012-01-01"), as.Date("2018-12-31"), by = "month",format="%Y/%mm/%dd")

i<-1
waitsecs<-2
while (i < length(dates)){
  startdate<-dates[i]
  #enddate=dates[i+1]
  print(startdate)
  filenm<-paste(saveDir,"nesdisVHNSQchlaMonthly_",startdate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaMonthly.nc?chlor_a[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(0.0):1:(0.0)][(89.75625):1:(-89.75626)][(-179.9812):1:(179.9813)]",sep="")
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
  close(f)
  i<-i+1
  if (is.na(file.info(filenm)$size)) {
    i<-i-1
    waitfor(waitsecs)
    waitsecs<-waitsecs+2
  }
  else if (file.info(filenm)$size < 2000){
    i<-i-1
    waitfor(waitsecs)
    waitsecs<-waitsecs+2
  }
  else waitsecs<-2
  if (waitsecs > 90) waitsecs <- 30
}


#### modis ####
"https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1chlamday.htmlTable?chlorophyll[(2019-01-16T00:00:00Z):1:(2019-01-16T00:00:00Z)][(89.97916):1:(-89.97918)][(-179.9792):1:(179.9792)]"

i<-1
waitsecs<-2
while (i < length(dates)){
  startdate<-dates[i]
  #enddate=dates[i+1]
  print(startdate)
  filenm<-paste(saveDir,"erdMH1chlamday_",startdate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1chlamday.nc?chlorophyll[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(89.75625):1:(-89.75626)][(-179.9812):1:(179.9813)]",sep="")
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
  close(f)
  i<-i+1
  if (is.na(file.info(filenm)$size)) {
    i<-i-1
    waitfor(waitsecs)
    waitsecs<-waitsecs+2
  }
  else if (file.info(filenm)$size < 2000){
    i<-i-1
    waitfor(waitsecs)
    waitsecs<-waitsecs+2
  }
  else waitsecs<-2
  if (waitsecs > 90) waitsecs <- 30
}

