#### script to download NOAA VIIRS and MODIS
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirsNASA_netcdf";dir.create(viirsDir)

waitfor <- function(x){
    p1 <- proc.time()
    Sys.sleep(x)
    print(proc.time() - p1) # The cpu usage should be negligible
}

#### viirsDir 2015 - 2018; 8 day A ####
a=seq(as.Date("2015-08-01"),as.Date("2016-01-01"),by=1) %>% as.character()
b=seq(as.Date("2016-08-01"),as.Date("2017-01-01"),by=1)%>% as.character()
c=seq(as.Date("2017-08-01"),as.Date("2018-01-01"),by=1)%>% as.character()
d=seq(as.Date("2018-08-01"),as.Date("2019-01-01"),by=1)%>% as.character()

dates=a

for(i in 1:length(dates)){
startdate<-dates[i]
print(startdate)
# enddate=dates[i+1]
filenm<-paste(viirsDir,"/erdVH2018chla8day","_",startdate,".nc",sep="")
url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVH2018chla8day.nc?chla[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(60):1:(10.0)][(-150):1:(-100)]",sep="")
print(startdate)
f = CFILE(filenm,mode="wb")
curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
close(f)
waitfor(3)
}

#### viirsDir 2015 - 2018; 8 day B ####
a=seq(as.Date("2015-08-01"),as.Date("2016-01-01"),by=1) %>% as.character()
b=seq(as.Date("2016-08-01"),as.Date("2017-01-01"),by=1)%>% as.character()
c=seq(as.Date("2017-08-01"),as.Date("2018-01-01"),by=1)%>% as.character()
d=seq(as.Date("2018-08-01"),as.Date("2019-01-01"),by=1)%>% as.character()

dates=b

for(i in 1:length(dates)){
  startdate<-dates[i]
  print(startdate)
  # enddate=dates[i+1]
  filenm<-paste(viirsDir,"/erdVH2018chla8day","_",startdate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVH2018chla8day.nc?chla[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(60):1:(10.0)][(-150):1:(-100)]",sep="")
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
  close(f)
  waitfor(3)
}

#### viirsDir 2015 - 2018; 8 day C ####
a=seq(as.Date("2015-08-01"),as.Date("2016-01-01"),by=1) %>% as.character()
b=seq(as.Date("2016-08-01"),as.Date("2017-01-01"),by=1)%>% as.character()
c=seq(as.Date("2017-08-01"),as.Date("2018-01-01"),by=1)%>% as.character()
d=seq(as.Date("2018-08-01"),as.Date("2019-01-01"),by=1)%>% as.character()

dates=c

for(i in 1:length(dates)){
  startdate<-dates[i]
  print(startdate)
  # enddate=dates[i+1]
  filenm<-paste(viirsDir,"/erdVH2018chla8day","_",startdate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVH2018chla8day.nc?chla[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(60):1:(10.0)][(-150):1:(-100)]",sep="")
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
  close(f)
  waitfor(3)
}

#### viirsDir 2015 - 2018; 8 day D ####
a=seq(as.Date("2015-08-01"),as.Date("2016-01-01"),by=1) %>% as.character()
b=seq(as.Date("2016-08-01"),as.Date("2017-01-01"),by=1)%>% as.character()
c=seq(as.Date("2017-08-01"),as.Date("2018-01-01"),by=1)%>% as.character()
d=seq(as.Date("2018-08-01"),as.Date("2019-01-01"),by=1)%>% as.character()

dates=d

for(i in 1:length(dates)){
  startdate<-dates[i]
  print(startdate)
  # enddate=dates[i+1]
  filenm<-paste(viirsDir,"/erdVH2018chla8day","_",startdate,".nc",sep="")
  url<-paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVH2018chla8day.nc?chla[(",startdate,"T12:00:00Z):1:(",startdate,"T12:00:00Z)][(60):1:(10.0)][(-150):1:(-100)]",sep="")
  print(startdate)
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref,noprogress=FALSE) 
  close(f)
  waitfor(3)
}

