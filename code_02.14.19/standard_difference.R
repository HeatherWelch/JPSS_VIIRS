#### standardizing difference

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)
library(scales)
library(sdmvspecies)

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

################### --------------------------------------------------------- > line plot

#### chla ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"

#1. time series of spatial average, just mean ####
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd") %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite/","",.) %>% gsub("/l.blendChl.grd","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df)) %>% mutate(chla=rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
CHLA=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% .[complete.cases(.),]



#### lbst ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs"
noCHLADir="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/lbst/predCIs"

#1. time series of spatial average, just mean ####
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(modisDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/lbst_pa_","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T)  %>% gsub(viirsDir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/lbst_pa_","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_nochla=list.files(noCHLADir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_nochla=list.files(noCHLADir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub(noCHLADir,"",.) %>% gsub("_mean.grd","",.)%>% gsub("/lbst_pa_","",.)
names(sat_nochla)=dates_nochla
nochla_stats=cellStats(sat_nochla,stat="mean")
b=nochla_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_nochla)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
nochla_df=b %>% mutate(sensor="No_Chl_a") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df,nochla_df)) %>% mutate(chla=rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
a=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% mutate(MODIS_noCHLA=MODIS-No_Chl_a) %>% mutate(VIRIS_noCHLA=VIIRS-No_Chl_a)#%>% .[complete.cases(.),]
LBST=a %>% gather(sensor,chla,-c(date,year,month,MODIS,VIIRS,No_Chl_a)) 



#### ecocast ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"
noCHLADir="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/output/mean"

#1. time series of spatial average, just mean ####
sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_m=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_m)=dates_m
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_m)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
m_df=b %>% mutate(sensor="MODIS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_v=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)
names(sat_v)=dates_v
v_stats=cellStats(sat_v,stat="mean")
b=v_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_v)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
v_df=b %>% mutate(sensor="VIIRS") #%>% left_join(a,.,by=c("full_TS"="date"))

sat_nochla=list.files(noCHLADir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
dates_nochla=list.files(noCHLADir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% gsub("/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/output/mean/EcoCast_-0.1_-0.1_-0.05_-0.9_0.9_","",.) %>% gsub("_mean.grd","",.)%>% gsub("_mean.grd","",.)
names(sat_nochla)=dates_nochla
nochla_stats=cellStats(sat_nochla,stat="mean")
b=nochla_stats %>% as.data.frame() %>% mutate(date=as.Date(dates_nochla)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
nochla_df=b %>% mutate(sensor="No_Chl_a") #%>% left_join(a,.,by=c("full_TS"="date"))

master=do.call("rbind",list(m_df,v_df,nochla_df)) %>% mutate(chla=rescale(chla,to=c(0,1)))
master$sensor=as.factor(master$sensor)

### plot to show difference, i don't really like it so byeee
a=master %>% spread(sensor,chla) %>% mutate(MODIS_VIIRS=MODIS-VIIRS) %>% mutate(MODIS_noCHLA=MODIS-No_Chl_a) %>% mutate(VIRIS_noCHLA=VIIRS-No_Chl_a)#%>% .[complete.cases(.),]
ECOCAST=a %>% gather(sensor,chla,-c(date,year,month,MODIS,VIIRS,No_Chl_a)) 


#### standardize ####
CHLA1=CHLA %>% rename(chla=MODIS_VIIRS) %>% mutate(sensor="MODIS_VIIRS") %>% mutate(product="Chl-a")
LBST1=LBST %>% select(-c(No_Chl_a )) %>% mutate(product="Leatherback")
ECOCAST1=ECOCAST %>% select(-c(No_Chl_a ))%>% mutate(product="EcoCast")

master=do.call("rbind",list(CHLA1,LBST1,ECOCAST1))
#master$chla_standard=rescale(master$chla,to=c(-1,1))

lineplot=ggplot(master,aes(x=date,y=chla))+geom_line(aes(group=product,color=product),size=.3)+geom_point(aes(group=product,color=product),size=.4)+
  scale_x_date(date_breaks="month",date_labels = "%b",date_minor_breaks = "months")+geom_hline(yintercept=0)+
  scale_color_manual("Sensor",values=c("Leatherback"="darkgoldenrod","EcoCast"="coral1","Chl-a"="gray"))+
  facet_grid(sensor~year, scales="free")+labs(x="Date")+labs(y="Chlorophyl difference ")+theme(legend.position=c(.4,.5),legend.justification = c(.9,.9))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))

lineplot

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="standard_difference"

png(paste(outputDir,datatype,"_line.png",sep=''),width=18,height=30,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
lineplot
dev.off()


################### --------------------------------------------------------- > spatial plot, havent' quite figured this one out

# f2 <- function(x, mn, mx) {
#   x <- t(x)
#   # i <- which(x > 0)
#   # i[is.na(i)] <- FALSE
#   mxx <- x / mx
#   x <- x / mn
#   x[i] <- mxx[i]
#   t(x)
# }
# 
# ## chla
# modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
# viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
# modisC=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# viirsC=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# 
# s=stack(modisC,viirsC)
# minv <- abs(cellStats(s,'min'))
# maxv <- cellStats(s,'max')
# ss2 <- calc(s, fun=function(x) f2(x, minv, maxv))
# 
# a=sdmvspecies::rescaleStack(s)
# 
# ## lbst
# modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs"
# viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs"
# modisL=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# viirsL=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-09-07",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# 
# ## ecocast
# modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
# viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"
# modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-09-07",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
