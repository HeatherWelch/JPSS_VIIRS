### downloading global data for quick summary figure

## nvm, used filezilla

waitfor <- function(x){
  p1 <- proc.time()
  Sys.sleep(x)
  print(proc.time() - p1) # The cpu usage should be negligible
}


setwd("~/Dropbox/JPSS/global/raw_filezilla")

#### GLOBCOLOUR ####
setwd("~/Dropbox/JPSS/global/raw_filezilla")

files=list.files()%>% grep("_4_",.,value=T)#%>% grep("ESACCI-OC-L3S",.,value=T,invert=T) %>% grep("GLOB_4_",.,value=T,invert=T) %>% .[8:length(.)]
df=data.frame(number=1:length(files)) %>% mutate(year=NA)%>% mutate(month=NA)%>% mutate(date=as.Date("2012-03-01"))%>% mutate(sensor=NA)%>% mutate(mean=NA)

for(i in 1:length(files)){
  name=files[i]
  print(name)
  
  if(grepl("AVW", name)){sensor="GlobColour Merged AVW"}
  if(grepl("GSM", name)){sensor="GlobColour Merged GSM"}
  
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
dfGlob=df
write.csv(dfGlob,"/Users/heatherwelch/Dropbox/JPSS/global/csvs/Glob_4km_merged_GSM_AVW.csv")

### adding ESA stuff ####
files=list.files(recursive=T)%>% grep("ESACCI-OC-L3S",.,value=T)
df=data.frame(number=1:length(files)) %>% mutate(year=NA)%>% mutate(month=NA)%>% mutate(date=as.Date("2012-03-01"))%>% mutate(sensor=NA)%>% mutate(mean=NA)

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
write.csv(dfESA,"/Users/heatherwelch/Dropbox/JPSS/global/csvs/ESA_4km_merged.csv")

#### erddap ####
files=list.files("/Users/heatherwelch/Dropbox/JPSS/global/erddap",full.names =T)
df=data.frame(number=1:length(files)) %>% mutate(year=NA)%>% mutate(month=NA)%>% mutate(date=as.Date("2012-03-01"))%>% mutate(sensor=NA)%>% mutate(mean=NA)

for(i in 1:length(files)){
  name=files[i]
  print(name)
  
  if(grepl("erdMH1chlamday", name)){
    sensor="MODIS"
    date=substr(name,63,72) %>% as.Date() 
    year=substr(name,63,66)
    month=substr(name,68,69)
    mean=raster(name,var="chlorophyll") %>% cellStats(.,stat="mean",na.rm=T)
    }
  if(grepl("nesdisVHNSQchlaMonthly", name)){
    sensor="VIIRS"
    date=substr(name,71,80) %>% as.Date() 
    year=substr(name,71,74)
    month=substr(name,76,77)
    mean=raster(name,var="chlor_a") %>% cellStats(.,stat="mean",na.rm=T)
    }
  
  df$year[i]=year
  df$month[i]=month
  df$date[i]=date
  df$sensor[i]=sensor
  df$mean[i]=mean
  
}

df$sensor=as.factor(df$sensor)
dferddap=df
write.csv(dferddap,"/Users/heatherwelch/Dropbox/JPSS/global/csvs/ERDDAP_MODIS_VIIRS_4km.csv")

#### plotting ####

df=rbind(dfGlob,dfESA,dferddap)

df$algorithm=NA

df <- within(df, algorithm[sensor == 'MODIS GSM'] <- 'GSM')
df <- within(df, algorithm[sensor == 'VIIRS GSM'] <- 'GSM')
df <- within(df, algorithm[sensor == 'GlobColour Merged GSM'] <- 'GSM')
df <- within(df, algorithm[sensor == 'MODIS AV'] <- 'AV/W')
df <- within(df, algorithm[sensor == 'VIIRS AV'] <- 'AV/W')
df <- within(df, algorithm[sensor == 'GlobColour Merged AVW'] <- 'AV/W')
df <- within(df, algorithm[sensor == 'ESA Merged'] <- 'ESA')

a=ggplot(df,aes(x=date,y=mean)) +geom_line(aes(group=sensor,color=sensor,linetype=sensor),size=.5)+geom_point(aes(color=sensor))+
  scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months")+
  theme(legend.position=c(.9,.9),legend.justification = c(.4,.4))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))+
  scale_color_manual("Product",values=c("VIIRS"="#d3ad06","MODIS"="#0066cc","ESA Merged"="red","GlobColour Merged GSM"="black","GlobColour Merged AVW"="darkgreen"))+ylab("Chl-a (mg/m3)")+xlab("Year")+
  scale_linetype_manual("Product",values = c("VIIRS"="dashed","MODIS"="dashed","ESA Merged"="solid","GlobColour Merged GSM"="solid","GlobColour Merged AVW"="solid"))

a

datatype="global_chla_4km"
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"

png(paste(outputDir,datatype,".png",sep=''),width=12,height=6,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
a
dev.off()

