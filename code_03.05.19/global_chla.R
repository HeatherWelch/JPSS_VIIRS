### downloading global data for quick summary figure

## nvm, used filezilla

waitfor <- function(x){
  p1 <- proc.time()
  Sys.sleep(x)
  print(proc.time() - p1) # The cpu usage should be negligible
}


# filelist=read.csv("/Users/heatherwelch/Dropbox/JPSS/global/global_order.csv") %>% .[,1] %>% grep("GSM",.,value=T,invert=T) #%>% gsub("/mount/dmz28/www/hermes/ftp/548388170/","",.)%>% gsub("/mount/dmz29/prod_data/globcolour/data/GLOB/viirsn/month/2014/04/01/","",.)
# userpwd="ftp_hermes:hermes%"
# 
# for(i in 1:length(filelist)){
#   url=url=paste0("ftp://ftp.hermes.acri.fr/548388170/",filelist[i])
#   print(url)
#   waitfor(3)
#   name=filelist[i] %>% gsub(".nc","",.)
#   if(grepl("AV-VIR", name)){tmpdir="/Users/heatherwelch/Dropbox/JPSS/global/viirs/"}
#   if(grepl("AV-MOD", name)){tmpdir="/Users/heatherwelch/Dropbox/JPSS/global/modis/"}
#   if(grepl("AVW-MODVIR", name)){tmpdir="/Users/heatherwelch/Dropbox/JPSS/global/merged/"}
#   if(grepl("AVW-MERMODVIR", name)){tmpdir="/Users/heatherwelch/Dropbox/JPSS/global/merged/"}
#   if(grepl("AVW-MERMOD", name)){tmpdir="/Users/heatherwelch/Dropbox/JPSS/global/merged/"}
#   print(tmpdir)
#   data=getBinaryURL(url,userpwd = userpwd,ftp.use.epsv = FALSE,ssl.verifypeer = FALSE,noprogress=FALSE) 
#   con <- file(paste(tmpdir,"/",name,".nc",sep=""), open = "wb") # write data to a file
#   writeBin(data,con)
#   waitfor(3)
#   close(con)
# }

setwd("~/Dropbox/JPSS/global/raw_filezilla")

# df <- data.frame(year=character(504),
#                  month=character(504), 
#                  date=as.Date(character(504)), 
#                  sensor=character(504), 
#                  mean=numeric(504), 
#                  stringsAsFactors=FALSE)

#### globcolour-cmemes AVW, viirs, modis, merged ####

df=data.frame(number=1:252) %>% mutate(year=NA)%>% mutate(month=NA)%>% mutate(date=as.Date("2012-03-01"))%>% mutate(sensor=NA)%>% mutate(mean=NA)

files=list.files()%>% grep("GSM",.,value=T,invert=T)%>% grep("ESACCI-OC-L3S",.,value=T,invert=T) %>% grep("GLOB_4_",.,value=T,invert=T) %>% .[8:length(.)]

for(i in 1:length(files)){
  name=files[i]
  print(name)
  
  if(grepl("AV-VIR", name)){sensor="VIIRS AV"}
  if(grepl("AV-MOD", name)){sensor="MODIS AV"}
  if(grepl("AVW-MODVIR", name)){sensor="GlobColour Merged AVW"}
  if(grepl("AVW-MERMODVIR", name)){sensor="GlobColour Merged AVW"}
  if(grepl("AVW-MERMOD", name)){sensor="GlobColour Merged AVW"}
  
  date=substr(name,5,12) %>% as.Date(format="%Y%m%d")
  year=substr(name,5,8)
  month=substr(name,9,10)
  
  mean=raster(name,var="CHL1_mean") %>% cellStats(.,stat="mean",na.rm=T)
  df$year[i]=year
  df$month[i]=month
  df$date[i]=date
  df$sensor[i]=sensor
  df$mean[i]=mean
  
}

df$sensor=as.factor(df$sensor)
dfGlobAVW=df

#### globcolour-cmemes GSM, merged ####

df=data.frame(number=1:83) %>% mutate(year=NA)%>% mutate(month=NA)%>% mutate(date=as.Date("2012-03-01"))%>% mutate(sensor=NA)%>% mutate(mean=NA)

files=list.files()%>% grep("AV",.,value=T,invert=T)%>% grep("MODVIR",.,value=T)%>% grep("ESACCI-OC-L3S",.,value=T,invert=T) %>% grep("GLOB_4_",.,value=T,invert=T) 

for(i in 1:length(files)){
  name=files[i]
  print(name)
  
  sensor="GlobColour Merged GSM"
  
  date=substr(name,5,12) %>% as.Date(format="%Y%m%d")
  year=substr(name,5,8)
  month=substr(name,9,10)
  
  mean=raster(name,var="CHL1_mean") %>% cellStats(.,stat="mean",na.rm=T)
  df$year[i]=year
  df$month[i]=month
  df$date[i]=date
  df$sensor[i]=sensor
  df$mean[i]=mean
  
}

df$sensor=as.factor(df$sensor)
dfGlobGSM=df

### adding ESA stuff ####

df=data.frame(number=1:78) %>% mutate(year=NA)%>% mutate(month=NA)%>% mutate(date=as.Date("2012-03-01"))%>% mutate(sensor=NA)%>% mutate(mean=NA)

files=list.files(recursive=T)%>% grep("ESACCI-OC-L3S",.,value=T)

for(i in 1:length(files)){
  name=files[i]
  print(name)
  
  sensor="ESA Merged"
  
  date=substr(name,62,67) %>% paste0(.,"01") %>% as.Date(format="%Y%m%d")
  year=substr(name,62,65)
  month=substr(name,66,67)
  
  mean=raster(name,var="chlor_a") %>% cellStats(.,stat="mean",na.rm=T)
  df$year[i]=year
  df$month[i]=month
  df$date[i]=date
  df$sensor[i]=sensor
  df$mean[i]=mean
  
}

df$sensor=as.factor(df$sensor)
dfESA=df

#### plotting ####

df=rbind(dfESA,dfGlobGSM,dfGlobAVW)


a=ggplot(df,aes(x=date,y=mean)) +geom_line(aes(group=sensor,color=sensor),size=.5)+
  scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months")+
  theme(legend.position=c(.9,.9),legend.justification = c(.4,.4))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))+
  scale_color_manual("Product",values=c("VIIRS AV"="#d3ad06","MODIS AV"="#0066cc","ESA Merged"="red","GlobColour Merged GSM"="black","GlobColour Merged AVW"="pink"))+ylab("Chl-a (mg/m3)")+xlab("Year")

a

datatype="global_chla"
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"

png(paste(outputDir,datatype,".png",sep=''),width=12,height=6,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
a
dev.off()


#### globcolour-cmemes GSM, merged, modis, viirs ####

df=data.frame(number=1:252) %>% mutate(year=NA)%>% mutate(month=NA)%>% mutate(date=as.Date("2012-03-01"))%>% mutate(sensor=NA)%>% mutate(mean=NA)

files=list.files()%>% grep("AV",.,value=T,invert=T)%>% grep("ESACCI-OC-L3S",.,value=T,invert=T) %>% grep("GLOB_4_",.,value=T,invert=T) %>% .[8:length(.)]

for(i in 1:length(files)){
  name=files[i]
  print(name)
  
  if(grepl("GSM-VIR", name)){sensor="VIIRS GSM"}
  if(grepl("GSM-MOD", name)){sensor="MODIS GSM"}
  if(grepl("GSM-MODVIR", name)){sensor="GlobColour Merged GSM"}
  if(grepl("GSM-MERMODVIR", name)){sensor="GlobColour Merged GSM"}
  if(grepl("GSM-MERMOD", name)){sensor="GlobColour Merged GSM"}
  
  date=substr(name,5,12) %>% as.Date(format="%Y%m%d")
  year=substr(name,5,8)
  month=substr(name,9,10)
  
  mean=raster(name,var="CHL1_mean") %>% cellStats(.,stat="mean",na.rm=T)
  df$year[i]=year
  df$month[i]=month
  df$date[i]=date
  df$sensor[i]=sensor
  df$mean[i]=mean
  
}

df$sensor=as.factor(df$sensor)
dfGlobGSM_all=df

#### plotting ####

df=rbind(dfESA,dfGlobGSM_all,dfGlobAVW)
df$algorithm=NA

df <- within(df, algorithm[sensor == 'MODIS GSM'] <- 'GSM')
df <- within(df, algorithm[sensor == 'VIIRS GSM'] <- 'GSM')
df <- within(df, algorithm[sensor == 'GlobColour Merged GSM'] <- 'GSM')
df <- within(df, algorithm[sensor == 'MODIS AV'] <- 'AV/W')
df <- within(df, algorithm[sensor == 'VIIRS AV'] <- 'AV/W')
df <- within(df, algorithm[sensor == 'GlobColour Merged AVW'] <- 'AV/W')
df <- within(df, algorithm[sensor == 'ESA Merged'] <- 'ESA')


a=ggplot(df,aes(x=date,y=mean)) +geom_line(aes(group=sensor,color=sensor,linetype=algorithm),size=.5)+geom_point(aes(group=sensor,color=sensor))+
  scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months")+
  theme(legend.position=c(.9,.9),legend.justification = c(.4,.55))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))+
  scale_linetype_manual("Algorithm",values = c("GSM"="dashed","AV/W"="solid","ESA"="solid"))+
  scale_color_manual("Product",values=c("VIIRS AV"="red","MODIS AV"="yellow","ESA Merged"="black","GlobColour Merged GSM"="darkgreen","GlobColour Merged AVW"="orange","VIIRS GSM"="green","MODIS GSM"="blue"))+ylab("Chl-a (mg/m3)")+xlab("Year")

a

#### 4 km globcolour, merged ####

df=data.frame(number=1:84) %>% mutate(year=NA)%>% mutate(month=NA)%>% mutate(date=as.Date("2012-03-01"))%>% mutate(sensor=NA)%>% mutate(mean=NA)

files=list.files()%>% grep("AV",.,value=T,invert=T)%>% grep("ESACCI-OC-L3S",.,value=T,invert=T) %>% grep("GLOB_100_",.,value=T,invert=T) %>% .[8:length(.)]

for(i in 1:length(files)){
  name=files[i]
  print(name)
  
  if(grepl("GSM-VIR", name)){sensor="VIIRS GSM"}
  if(grepl("GSM-MOD", name)){sensor="MODIS GSM"}
  if(grepl("GSM-MODVIR", name)){sensor="GlobColour Merged GSM"}
  if(grepl("GSM-MERMODVIR", name)){sensor="GlobColour Merged GSM"}
  if(grepl("GSM-MERMOD", name)){sensor="GlobColour Merged GSM"}
  
  date=substr(name,5,12) %>% as.Date(format="%Y%m%d")
  year=substr(name,5,8)
  month=substr(name,9,10)
  
  mean=raster(name,var="CHL1_mean") %>% cellStats(.,stat="mean",na.rm=T)
  df$year[i]=year
  df$month[i]=month
  df$date[i]=date
  df$sensor[i]=sensor
  df$mean[i]=mean
  
}

df$sensor=as.factor(df$sensor)
dfGlobGSM_4=df
df=rbind(dfESA,dfGlobGSM_4)

ggplot(df,aes(x=date,y=mean))+geom_line(aes(group=sensor,color=sensor))
