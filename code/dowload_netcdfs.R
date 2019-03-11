#### script to download NOAA VIIRS and MODIS
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_netcdf"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_netcdf"
pmlEsaDir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_netcdf"#;dir.create(pmlEsaDir)

waitfor <- function(x){
    p1 <- proc.time()
    Sys.sleep(x)
    print(proc.time() - p1) # The cpu usage should be negligible
}

#### modis 2012 and 2015; 1 day ####
dates<-seq(as.Date("2012-08-01"), as.Date("2012-12-01"), by = "day",format="%Y/%mm/%dd")

startdate<-dates[i]
enddate="2012-12-01"
filenm<-paste(modisDir,"/erdMH1chla1day_",startdate,"_",enddate,".nc",sep="")
url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1chla1day.nc?chlorophyll[(",startdate,"T12:00:00Z):1:(",enddate,"T12:00:00Z)][(60):1:(10.0)][(-150):1:(-100)]",sep="")
print(startdate)
f = CFILE(filenm,mode="wb")
curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
close(f)

##

startdate<-"2015-08-01"
enddate="2015-12-01"
filenm<-paste(modisDir,"/erdMH1chla1day_",startdate,"_",enddate,".nc",sep="")
url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1chla1day.nc?chlorophyll[(",startdate,"T12:00:00Z):1:(",enddate,"T12:00:00Z)][(60):1:(10.0)][(-150):1:(-100)]",sep="")
print(startdate)
f = CFILE(filenm,mode="wb")
curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
close(f)

#### modis 2012 and 2015; 8 day ####
dates<-seq(as.Date("2012-08-05"), as.Date("2012-12-01"), by = "day",format="%Y/%mm/%dd")
dates<-seq(as.Date("2015-07-31"), as.Date("2015-08-02"), by = "day",format="%Y/%mm/%dd")

i<-1
waitsecs<-2
while (i < length(dates)){
startdate<-dates[i]
enddate=dates[i+1]
print(startdate)
filenm<-paste(modisDir,"/erdMBchla8day_LonPM180_",startdate,"_",enddate,".nc",sep="")
url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMBchla8day_LonPM180.nc?chlorophyll[(",startdate,"T12:00:00Z):1:(",enddate,"T12:00:00Z)][(0.0):1:(0.0)][(10):1:(60.0)][(-150):1:(-100)]",sep="")
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

##
dates<-seq(as.Date("2015-08-01"), as.Date("2015-12-01"), by = "day",format="%Y/%mm/%dd")

i<-1
waitsecs<-2
while (i < length(dates)){
  startdate<-dates[i]
  enddate=dates[i+1]
  print(startdate)
  filenm<-paste(modisDir,"/erdMBchla8day_LonPM180_",startdate,"_",enddate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMBchla8day_LonPM180.nc?chlorophyll[(",startdate,"T12:00:00Z):1:(",enddate,"T12:00:00Z)][(0.0):1:(0.0)][(10):1:(60.0)][(-150):1:(-100)]",sep="")
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

#### viirs 2015; 8 day ####
#dates<-seq(as.Date("2015-08-01"), as.Date("2015-12-01"), by = "day",format="%Y/%mm/%dd")
dates<-seq(as.Date("2015-12-01"), as.Date("2016-01-01"), by = "day",format="%Y/%mm/%dd")

i<-1
waitsecs<-2
while (i < length(dates)){
  startdate<-dates[i]
  #enddate=dates[i+1]
  print(startdate)
  filenm<-paste(viirsDir,"/erdVHNchla8day_",startdate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla8day.nc?chla[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(0.0):1:(0.0)][(60):1:(10.0)][(-150):1:(-110.5)]",sep="")
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

##
#### viirs 2015; 1 day ####
dates<-seq(as.Date("2015-08-01"), as.Date("2015-12-01"), by = "day",format="%Y/%mm/%dd")

i<-1
waitsecs<-2
while (i < length(dates)){
  startdate<-dates[i]
  enddate=dates[i+1]
  print(startdate)
  filenm<-paste(viirsDir,"/erdVHNchla1day_",startdate,"_",enddate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla1day.nc?chla[(",startdate,"T12:00:00Z):1:(",enddate,"T12:00:00Z)][(0.0):1:(0.0)][(60):1:(10.0)][(-150):1:(-110.5)]",sep="")
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

##
#### viirs 2016; 8 day ####
dates<-seq(as.Date("2016-08-01"), as.Date("2017-01-01"), by = "day",format="%Y/%mm/%dd")

i<-1
waitsecs<-2
while (i < length(dates)){
  startdate<-dates[i]
  #enddate=dates[i+1]
  print(startdate)
  filenm<-paste(viirsDir,"/erdVHNchla8day_",startdate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla8day.nc?chla[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(0.0):1:(0.0)][(60):1:(10.0)][(-150):1:(-110.5)]",sep="")
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

##

#### viirs 2017; 8 day ####
dates<-seq(as.Date("2017-08-01"), as.Date("2018-01-01"), by = "day",format="%Y/%mm/%dd")

i<-1
waitsecs<-2
while (i < length(dates)){
  startdate<-dates[i]
  #enddate=dates[i+1]
  print(startdate)
  filenm<-paste(viirsDir,"/erdVHNchla8day_",startdate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla8day.nc?chla[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(0.0):1:(0.0)][(60):1:(10.0)][(-150):1:(-110.5)]",sep="")
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

##
#### viirs 2018; 8 day ####
dates<-seq(as.Date("2018-08-01"), as.Date("2019-01-01"), by = "day",format="%Y/%mm/%dd")
<<<<<<< HEAD
=======
dates<-seq(as.Date("2018-12-30"), as.Date("2019-01-01"), by = "day",format="%Y/%mm/%dd")
>>>>>>> 29c872b3ea81f92271726ccd8aa1767310a87d12

i<-1
waitsecs<-2
while (i < length(dates)){
  startdate<-dates[i]
  #enddate=dates[i+1]
  print(startdate)
  filenm<-paste(viirsDir,"/erdVHNchla8day_",startdate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla8day.nc?chla[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(0.0):1:(0.0)][(60):1:(10.0)][(-150):1:(-110.5)]",sep="")
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

<<<<<<< HEAD
=======
##
#### omlEsa 2015-2018; 8 day ####
a<-seq(as.Date("2015-07-25"), as.Date("2016-01-07"), by = "week",format="%Y/%mm/%dd") %>% as.character()
b<-seq(as.Date("2016-07-25"), as.Date("2017-01-07"), by = "week",format="%Y/%mm/%dd") %>% as.character()
c<-seq(as.Date("2017-07-25"), as.Date("2018-01-07"), by = "week",format="%Y/%mm/%dd") %>% as.character()
d<-seq(as.Date("2018-07-25"), as.Date("2019-01-07"), by = "week",format="%Y/%mm/%dd") %>% as.character()
dates=list(a,b,c,d) %>% unlist()

i<-1
waitsecs<-2
while (i < length(dates)){
  startdate<-dates[i]
  #enddate=dates[i+1]
  print(startdate)
  filenm<-paste(pmlEsaDir,"/pmlEsaCCI31OceanColor8Day_",startdate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/pmlEsaCCI31OceanColor8Day.nc?chlor_a[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(60):1:(10.0)][(-150):1:(-110.5)]",sep="")
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

>>>>>>> 29c872b3ea81f92271726ccd8aa1767310a87d12
##