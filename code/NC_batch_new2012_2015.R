################ CM2.6.R adapted to batch all netcdfs
library(chron)
library(sp)
library(rgdal)
library(raster)
library(ncdf4)
library(tidyverse)

template=raster() ##create template for resampling
res(template)=0.2487562
ncol(template)=201
nrow(template)=201
xmin(template)=-149.875
xmax(template)=-99.875
ymin(template)=10.125
ymax(template)=60.125
projection(template)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#### modis 2012 and 2015; 1 day ####
netcdf_dir="/Users/heatherwelch/Dropbox/JPSS/modis_netcdf"
output_dir="/Users/heatherwelch/Dropbox/JPSS/modis_1Day/Satellite"#;dir.create(output_dir)

netcdf=list.files(netcdf_dir,pattern="*erdMH1chla1day*",full.names = T)
template_native=raster(netcdf[1])

for(nc in netcdf){
  print(nc)
  #ncc=paste(nc,".nc",sep="")
  ncc=nc
  ncin <- nc_open(ncc)
  print(ncin)
  dname="chlorophyll" # define variable of interest ########### change for each variable
  print("defining variables")
  lon <- ncvar_get(ncin, "longitude") # define longitude
  nlon <- dim(lon)
  lat <- ncvar_get(ncin, "latitude", verbose = F) # define latitude
  nlat <- dim(lat)
  t <- ncvar_get(ncin, "time") # define time field
  tunits <- ncatt_get(ncin, "time", "units") # get time units
  nt <- dim(t)
  tmp.array <- ncvar_get(ncin, dname)
  dlname <- ncatt_get(ncin, dname, "long_name") #grab global attributes
  dunits <- ncatt_get(ncin, dname, "units") #grab global attributes
  fillvalue <- ncatt_get(ncin, dname, "_FillValue") #grab global attributes
  print("changing date format")
  tustr <- strsplit(tunits$value, " ") #changing date format
  tdstr <- strsplit(unlist(tustr)[3], "-") #changing date format
  tmonth = as.integer(unlist(tdstr)[2]) #changing date format
  tday=as.integer(gsub("T00:00:00Z","",unlist(tdstr)[3]))
  tyear = as.integer(unlist(tdstr)[1]) #changing date format
  tmp.array[tmp.array==fillvalue$value]=NA #setting fill value
  tmp.vec.long <- as.vector(tmp.array)
  length(tmp.vec.long)
  tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
  print("Formatting column names")
  #date2=as.character(chron(t, origin = c(tmonth, tday, tyear))) ####getting names together
  date1=as.character(as.POSIXlt(t,origin='1970-01-01',tz= "UTC"))
  date0=lapply(date1,function(x)(gsub(" 12:00:00","",x))) %>% unlist() 
  print("Creating spatial dataframe")
  lonlat <- expand.grid(lon, lat)
  names(lonlat) <- c("lon","lat")
  tmp.df02 <- data.frame(tmp.mat)
  names(tmp.df02) <- date0
  tmp.df02 <- cbind(lonlat, tmp.df02)
  coordinates(tmp.df02)=~lon+lat
  print("converting to raster")
  for(n in names(tmp.df02)){
    print(n)
       path1=paste(output_dir,"/",n,sep="");dir.create(path1)
       print(path1)
       r=rasterize(tmp.df02,template,field=n,fun=mean)
       r1=log(r+0.001)
       writeRaster(r1,paste(path1,"/l.blendChl",sep=""),overwrite=TRUE)
    }
  }
  

#### modis 2015 - 2018; 8 day ####
netcdf_dir="/Users/heatherwelch/Dropbox/JPSS/modis_netcdf"
output_dir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"#;dir.create(output_dir)

netcdf=list.files(netcdf_dir,pattern="*erdMBchla8day_LonPM180*",full.names = T) %>% .[247:623]
template_native=raster(netcdf[1])

for(nc in netcdf){
  print(nc)
  #ncc=paste(nc,".nc",sep="")
  ncc=nc
  ncin <- nc_open(ncc)
  print(ncin)
  dname="chlorophyll" # define variable of interest ########### change for each variable
  print("defining variables")
  lon <- ncvar_get(ncin, "longitude") # define longitude
  nlon <- dim(lon)
  lat <- ncvar_get(ncin, "latitude", verbose = F) # define latitude
  nlat <- dim(lat)
  t <- ncvar_get(ncin, "time") # define time field
  tunits <- ncatt_get(ncin, "time", "units") # get time units
  nt <- dim(t)
  tmp.array <- ncvar_get(ncin, dname)
  dlname <- ncatt_get(ncin, dname, "long_name") #grab global attributes
  dunits <- ncatt_get(ncin, dname, "units") #grab global attributes
  fillvalue <- ncatt_get(ncin, dname, "_FillValue") #grab global attributes
  print("changing date format")
  tustr <- strsplit(tunits$value, " ") #changing date format
  tdstr <- strsplit(unlist(tustr)[3], "-") #changing date format
  tmonth = as.integer(unlist(tdstr)[2]) #changing date format
  tday=as.integer(gsub("T00:00:00Z","",unlist(tdstr)[3]))
  tyear = as.integer(unlist(tdstr)[1]) #changing date format
  tmp.array[tmp.array==fillvalue$value]=NA #setting fill value
  tmp.vec.long <- as.vector(tmp.array)
  length(tmp.vec.long)
  tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
  print("Formatting column names")
  #date2=as.character(chron(t, origin = c(tmonth, tday, tyear))) ####getting names together
  date1=as.character(as.POSIXlt(t,origin='1970-01-01',tz= "UTC"))
  date0=lapply(date1,function(x)(gsub(" 12:00:00","",x))) %>% unlist() 
  print("Creating spatial dataframe")
  lonlat <- expand.grid(lon, lat)
  names(lonlat) <- c("lon","lat")
  tmp.df02 <- data.frame(tmp.mat)
  names(tmp.df02) <- date0
  tmp.df02 <- cbind(lonlat, tmp.df02)
  coordinates(tmp.df02)=~lon+lat
  print("converting to raster")
  for(n in names(tmp.df02)){
    print(n)
    path1=paste(output_dir,"/",n,sep="")
    if(!file.exists(path1)){
    dir.create(path1)
      if(!file.exists(paste(path1,"/l.blendChl",sep=""))){
    r=rasterize(tmp.df02,template,field=n,fun=mean)
    r1=log(r+0.001)
    writeRaster(r1,paste(path1,"/l.blendChl",sep=""),overwrite=TRUE)
      }
    }
    }
}


#### viirs 2015; 1 day ####
  netcdf_dir="/Users/heatherwelch/Dropbox/JPSS/viirs_netcdf"
  output_dir="/Users/ecocast/Dropbox/JPSS/viirs_1Day";dir.create(output_dir)
  output_dir="/Users/ecocast/Dropbox/JPSS/viirs_1Day/Satellite";dir.create(output_dir)
  
  netcdf=list.files(netcdf_dir,pattern="*erdVHNchla1day*",full.names = T)
  template_native=raster(netcdf[1])
  
  for(nc in netcdf){
    print(nc)
    #ncc=paste(nc,".nc",sep="")
    ncc=nc
    ncin <- nc_open(ncc)
    print(ncin)
    dname="chla" # define variable of interest ########### change for each variable
    print("defining variables")
    lon <- ncvar_get(ncin, "longitude") # define longitude
    nlon <- dim(lon)
    lat <- ncvar_get(ncin, "latitude", verbose = F) # define latitude
    nlat <- dim(lat)
    t <- ncvar_get(ncin, "time") # define time field
    tunits <- ncatt_get(ncin, "time", "units") # get time units
    nt <- dim(t)
    tmp.array <- ncvar_get(ncin, dname)
    dlname <- ncatt_get(ncin, dname, "long_name") #grab global attributes
    dunits <- ncatt_get(ncin, dname, "units") #grab global attributes
    fillvalue <- ncatt_get(ncin, dname, "_FillValue") #grab global attributes
    print("changing date format")
    tustr <- strsplit(tunits$value, " ") #changing date format
    tdstr <- strsplit(unlist(tustr)[3], "-") #changing date format
    tmonth = as.integer(unlist(tdstr)[2]) #changing date format
    tday=as.integer(gsub("T00:00:00Z","",unlist(tdstr)[3]))
    tyear = as.integer(unlist(tdstr)[1]) #changing date format
    tmp.array[tmp.array==fillvalue$value]=NA #setting fill value
    tmp.vec.long <- as.vector(tmp.array)
    length(tmp.vec.long)
    tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
    print("Formatting column names")
    #date2=as.character(chron(t, origin = c(tmonth, tday, tyear))) ####getting names together
    date1=as.character(as.POSIXlt(t,origin='1970-01-01',tz= "UTC"))
    date0=lapply(date1,function(x)(gsub(" 12:00:00","",x))) %>% unlist() 
    print("Creating spatial dataframe")
    lonlat <- expand.grid(lon, lat)
    names(lonlat) <- c("lon","lat")
    tmp.df02 <- data.frame(tmp.mat)
    names(tmp.df02) <- date0
    tmp.df02 <- cbind(lonlat, tmp.df02)
    coordinates(tmp.df02)=~lon+lat
    print("converting to raster")
    for(n in names(tmp.df02)){
      print(n)
      path1=paste(output_dir,"/",n,sep="");dir.create(path1)
      print(path1)
      r=rasterize(tmp.df02,template,field=n,fun=mean)
      r1=log(r+0.001)
      writeRaster(r1,paste(path1,"/l.blendChl",sep=""),overwrite=TRUE)
    }
  }
  
  
#### viirs 2015 - 2018; 8 day ####
  netcdf_dir="/Users/heatherwelch/Dropbox/JPSS/viirs_netcdf"
  output_dir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day"#;dir.create(output_dir)
  output_dir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"#;dir.create(output_dir)
  
  netcdf=list.files(netcdf_dir,pattern="*erdVHNchla8day*",full.names = T) %>% .[125:611]
  template_native=raster(netcdf[1])
  
  for(nc in netcdf){
    print(nc)
    #ncc=paste(nc,".nc",sep="")
    ncc=nc
    ncin <- nc_open(ncc)
    print(ncin)
    dname="chla" # define variable of interest ########### change for each variable
    print("defining variables")
    lon <- ncvar_get(ncin, "longitude") # define longitude
    nlon <- dim(lon)
    lat <- ncvar_get(ncin, "latitude", verbose = F) # define latitude
    nlat <- dim(lat)
    t <- ncvar_get(ncin, "time") # define time field
    tunits <- ncatt_get(ncin, "time", "units") # get time units
    nt <- dim(t)
    tmp.array <- ncvar_get(ncin, dname)
    dlname <- ncatt_get(ncin, dname, "long_name") #grab global attributes
    dunits <- ncatt_get(ncin, dname, "units") #grab global attributes
    fillvalue <- ncatt_get(ncin, dname, "_FillValue") #grab global attributes
    print("changing date format")
    tustr <- strsplit(tunits$value, " ") #changing date format
    tdstr <- strsplit(unlist(tustr)[3], "-") #changing date format
    tmonth = as.integer(unlist(tdstr)[2]) #changing date format
    tday=as.integer(gsub("T00:00:00Z","",unlist(tdstr)[3]))
    tyear = as.integer(unlist(tdstr)[1]) #changing date format
    tmp.array[tmp.array==fillvalue$value]=NA #setting fill value
    tmp.vec.long <- as.vector(tmp.array)
    length(tmp.vec.long)
    tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
    print("Formatting column names")
    #date2=as.character(chron(t, origin = c(tmonth, tday, tyear))) ####getting names together
    date1=as.character(as.POSIXlt(t,origin='1970-01-01',tz= "UTC"))
    date0=lapply(date1,function(x)(gsub(" 12:00:00","",x))) %>% unlist() 
    print("Creating spatial dataframe")
    lonlat <- expand.grid(lon, lat)
    names(lonlat) <- c("lon","lat")
    tmp.df02 <- data.frame(tmp.mat)
    names(tmp.df02) <- date0
    tmp.df02 <- cbind(lonlat, tmp.df02)
    coordinates(tmp.df02)=~lon+lat
    print("converting to raster")
    for(n in names(tmp.df02)){
      print(n)
      path1=paste(output_dir,"/",n,sep="")
      if(!file.exists(path1)){
        dir.create(path1)
        if(!file.exists(paste(path1,"/l.blendChl",sep=""))){
          r=rasterize(tmp.df02,template,field=n,fun=mean)
          r1=log(r+0.001)
          writeRaster(r1,paste(path1,"/l.blendChl",sep=""),overwrite=TRUE)
        }
      }
    }
  }
  
  
#### pmlEsa 2015 - 2018; 8 day ####
  netcdf_dir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_netcdf"
  output_dir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day"#;dir.create(output_dir)
  output_dir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/Satellite"#;dir.create(output_dir)
  
  netcdf=list.files(netcdf_dir,pattern="*pmlEsaCCI31OceanColor8Day*",full.names = T) 
  template_native=raster(netcdf[1])
  
  for(nc in netcdf){
    print(nc)
    #ncc=paste(nc,".nc",sep="")
    ncc=nc
    ncin <- nc_open(ncc)
    print(ncin)
    dname="chlor_a" # define variable of interest ########### change for each variable
    print("defining variables")
    lon <- ncvar_get(ncin, "longitude") # define longitude
    nlon <- dim(lon)
    lat <- ncvar_get(ncin, "latitude", verbose = F) # define latitude
    nlat <- dim(lat)
    t <- ncvar_get(ncin, "time") # define time field
    tunits <- ncatt_get(ncin, "time", "units") # get time units
    nt <- dim(t)
    tmp.array <- ncvar_get(ncin, dname)
    dlname <- ncatt_get(ncin, dname, "long_name") #grab global attributes
    dunits <- ncatt_get(ncin, dname, "units") #grab global attributes
    fillvalue <- ncatt_get(ncin, dname, "_FillValue") #grab global attributes
    print("changing date format")
    tustr <- strsplit(tunits$value, " ") #changing date format
    tdstr <- strsplit(unlist(tustr)[3], "-") #changing date format
    tmonth = as.integer(unlist(tdstr)[2]) #changing date format
    tday=as.integer(gsub("T00:00:00Z","",unlist(tdstr)[3]))
    tyear = as.integer(unlist(tdstr)[1]) #changing date format
    tmp.array[tmp.array==fillvalue$value]=NA #setting fill value
    tmp.vec.long <- as.vector(tmp.array)
    length(tmp.vec.long)
    tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
    print("Formatting column names")
    #date2=as.character(chron(t, origin = c(tmonth, tday, tyear))) ####getting names together
    date1=as.character(as.POSIXlt(t,origin='1970-01-01',tz= "UTC"))
    date0=lapply(date1,function(x)(gsub(" 12:00:00","",x))) %>% unlist() 
    print("Creating spatial dataframe")
    lonlat <- expand.grid(lon, lat)
    names(lonlat) <- c("lon","lat")
    tmp.df02 <- data.frame(tmp.mat)
    names(tmp.df02) <- date0
    tmp.df02 <- cbind(lonlat, tmp.df02)
    coordinates(tmp.df02)=~lon+lat
    print("converting to raster")
    for(n in names(tmp.df02)){
      print(n)
      path1=paste(output_dir,"/",n,sep="")
      if(!file.exists(path1)){
        dir.create(path1)
        if(!file.exists(paste(path1,"/l.blendChl",sep=""))){
          r=rasterize(tmp.df02,template,field=n,fun=mean)
          r1=log(r+0.001)
          writeRaster(r1,paste(path1,"/l.blendChl",sep=""),overwrite=TRUE)
        }
      }
    }
  }
  
  