### creating area averaged line plot

rasterdir="/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_rasters_mask/"
csvdir="/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_csvs_mask/"#;dir.create(csvdir)
rasterlist=list.files(rasterdir,pattern = ".grd")
setwd(rasterdir)

df=data.frame(number=1:length(rasterlist)) %>% mutate(date=as.Date("2012-03-01"))%>% mutate(sensor=NA)%>% mutate(mean=NA)

for(i in 1:length(rasterlist)){
  name=rasterlist[i]
  ras=rasterlist[i] %>% raster()
  print(name)
  
  if(grepl("erdMH1chlamday", name)){
    sensor="MODIS"
    date=substr(name,16,25) %>% as.Date()
  }
  
  if(grepl("nesdisVHNSQchlaMonthly", name)){
    sensor="VIIRS"
    date=substr(name,24,33) %>% as.Date()
  }
  
  if(grepl("AVW", name)){
    sensor="GlobColour Merged AVW"
    date=substr(name,23,32) %>% as.Date()
  }
  
  if(grepl("GSM", name)){
    sensor="GlobColour Merged GSM"
    date=substr(name,23,32)%>% as.Date()
  }
  
  if(grepl("ESACCI-OC-L3S", name)){
    sensor="OC-CCI"
    date=substr(name,15,24) %>% as.Date()
  }
  
  mean=log(ras+0.001) %>% cellStats(.,stat="mean",na.rm=T)
  df$date[i]=date
  df$sensor[i]=sensor
  df$mean[i]=mean
  
}

df$sensor=as.factor(df$sensor)
dfAll=df
write.csv(dfAll,"/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_csvs_mask/All_products_4km_northA_log_mask.csv")

#### plotting ####

a=ggplot(dfAll,aes(x=date,y=mean)) +geom_line(aes(group=sensor,color=sensor,linetype=sensor),size=.5)+geom_point(aes(color=sensor))+
  scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months")+
  theme(legend.position=c(.9,.9),legend.justification = c(.4,.4))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))+
  scale_color_manual("Product",values=c("VIIRS"="#d3ad06","MODIS"="#0066cc","OC-CCI"="red","GlobColour Merged GSM"="black","GlobColour Merged AVW"="darkgreen"))+ylab("log Chl-a (mg/m3)")+xlab("Year")+
  scale_linetype_manual("Product",values = c("VIIRS"="dashed","MODIS"="dashed","OC-CCI"="solid","GlobColour Merged GSM"="solid","GlobColour Merged AVW"="solid"))

a

datatype="northA_chla_4km_log_mask"
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"

png(paste(outputDir,datatype,".png",sep=''),width=24,height=12,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
a
dev.off()

