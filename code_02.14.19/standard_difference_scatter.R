### standard difference through time
library(scales)
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

### chla ####

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
to_match=intersect(dates_m,dates_v)

sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))
m_df=m_df[m_df$date %in% as.Date(to_match),]

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))
v_df=v_df[v_df$date %in% as.Date(to_match),]

master=do.call("rbind",list(m_df,v_df)) %>% mutate(chla=scales::rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)
chla=master %>% spread(sensor,chla)

#ggplot(chla,aes(x=MODIS,y=VIIRS))+geom_point(aes(color=year,shape=month))+ggtitle("Chla")+geom_abline(slope=1,intercept = 0)+xlim(0,1)

### lbst ####

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs"
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs/lbst_pa_","",.) %>% gsub("_mean.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs/lbst_pa_","",.) %>% gsub("_mean.grd","",.)
to_match=intersect(dates_m,dates_v)

sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(modisDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/lbst_pa_","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))
m_df=m_df[m_df$date %in% as.Date(to_match),]

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(viirsDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/lbst_pa_","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))
v_df=v_df[v_df$date %in% as.Date(to_match),]

master=do.call("rbind",list(m_df,v_df)) %>% mutate(chla=scales::rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)
lbst=master %>% spread(sensor,chla)

#ggplot(lbst,aes(x=MODIS,y=VIIRS))+geom_point(aes(color=year,shape=month))+ggtitle("LBST")+geom_abline(slope=1,intercept = 0)+xlim(0,1)

### ecocast ####

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
to_match=intersect(dates_m,dates_v)

sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))
m_df=m_df[m_df$date %in% as.Date(to_match),]

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))
v_df=v_df[v_df$date %in% as.Date(to_match),]

master=do.call("rbind",list(m_df,v_df)) %>% mutate(chla=scales::rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)

ecocast=master %>% spread(sensor,chla)

#ggplot(ecocast,aes(x=MODIS,y=VIIRS))+geom_point(aes(color=year,shape=month))+ggtitle("EcoCast")+geom_abline(slope=1,intercept = 0)+xlim(0,1)

### plots ####
a=ggplot(chla,aes(x=MODIS,y=VIIRS))+geom_point(aes(color=year,shape=month))+ggtitle("Chla")+geom_abline(slope=1,intercept = 0)+xlim(0,1)+#+stat_smooth(method="lm",se = F)
  scale_color_manual("year",values=c("2015"="darkgoldenrod","2016"="coral1","2017"="gray","2018"="cadetblue3"))
b=ggplot(lbst,aes(x=MODIS,y=VIIRS))+geom_point(aes(color=year,shape=month))+ggtitle("LBST")+geom_abline(slope=1,intercept = 0)+xlim(0,1)+#+stat_smooth(method="lm",se = F)
  scale_color_manual("year",values=c("2015"="darkgoldenrod","2016"="coral1","2017"="gray","2018"="cadetblue3"))
c=ggplot(ecocast,aes(x=MODIS,y=VIIRS))+geom_point(aes(color=year,shape=month))+ggtitle("EcoCast")+geom_abline(slope=1,intercept = 0)+xlim(0,1)+#+stat_smooth(method="lm",se = F)
  scale_color_manual("year",values=c("2015"="darkgoldenrod","2016"="coral1","2017"="gray","2018"="cadetblue3"))

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="standard_difference_scatter"

png(paste(outputDir,datatype,".png",sep=''),width=39,height=13,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
plot_grid(a,b,c,nrow = 1,ncol = 3)
dev.off()
