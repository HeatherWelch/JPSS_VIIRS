#### histograms, standardized value

### chla
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
modisC=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
viirsC=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
a=as.data.frame(modisC)
b=gather(a)%>% .[complete.cases(.),] %>% mutate(sensor="MODIS")
c=as.data.frame(viirsC)
d=gather(c) %>% .[complete.cases(.),]%>% mutate(sensor="VIIRS")

bb=rbind(b,d) %>% mutate(value=scales::rescale(value,c(0,1)))
mc=bb %>% group_by(sensor) %>% summarise(mean=mean(value)) %>% .[1,2] %>% as.numeric()
vc=bb %>% group_by(sensor) %>% summarise(mean=mean(value)) %>% .[2,2] %>% as.numeric()

a=ggplot(data=bb,aes(x=value,group=sensor))+#geom_histogram(aes(y=..density..,color=sensor),fill=NA)+
  geom_density(aes(fill=sensor),alpha=.2) + geom_vline(aes(xintercept=mc), linetype="dashed", size=.7,color="darkgoldenrod")+ 
  geom_vline(aes(xintercept=vc), linetype="dashed", size=.7,color="cadetblue3")+
  scale_color_manual("Sensor",values=c("MODIS"="darkgoldenrod","VIIRS"="cadetblue3"))+scale_fill_manual("Sensor",values=c("MODIS"="darkgoldenrod","VIIRS"="cadetblue3"))
b=a+labs(y="Density")+labs(x="Scaled Chlorophyll a")+theme(legend.position=c(.2,.9),legend.justification = c(.9,.9))+
  #scale_y_continuous(expand = c(0,0))+scale_x_continuous(expand = c(0,0)) +geom_text(x=.75,y=7,label=paste0("MODIS mean= ",m))+geom_text(x=.75,y=6.5,label=paste0("VIIRS mean= ",v))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))
b=b+scale_y_continuous(expand = c(0,0))+scale_x_continuous(expand = c(0,0))
#c=b+geom_text(x=.75,y=7,label=paste0("MODIS mean= ",round(m,2)))+ geom_text(x=.75,y=6.5,label=paste0("VIIRS mean= ",round(v,)))

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"#;dir.create(outputDir)
datatype="chla"

png(paste(outputDir,datatype,"_hist.png",sep=''),width=8,height=8,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
b
dev.off()


### lbst
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs"
noCHLADir="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/lbst/predCIs"
modisL=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
viirsL=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-09-07",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
noCHLAE=list.files(noCHLADir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-09-07",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
a=as.data.frame(modisL)
b=gather(a)%>% .[complete.cases(.),] %>% mutate(sensor="MODIS")
c=as.data.frame(viirsL)
d=gather(c) %>% .[complete.cases(.),]%>% mutate(sensor="VIIRS")
e=as.data.frame(noCHLAE)
f=gather(e) %>% .[complete.cases(.),]%>% mutate(sensor="No_Chla")

bb=rbind(b,d,f)%>% mutate(value=scales::rescale(value,c(0,1)))
ml=bb %>% group_by(sensor) %>% summarise(mean=mean(value)) %>% .[1,2] %>% as.numeric()
vl=bb %>% group_by(sensor) %>% summarise(mean=mean(value)) %>% .[2,2] %>% as.numeric()
nl=bb %>% group_by(sensor) %>% summarise(mean=mean(value)) %>% .[3,2] %>% as.numeric()

a=ggplot(data=bb,aes(x=value,group=sensor))+#geom_histogram(aes(y=..density..,color=sensor),fill=NA)+
  geom_density(aes(fill=sensor),alpha=.2) + geom_vline(aes(xintercept=ml), linetype="dashed", size=.7,color="darkgoldenrod")+ 
  geom_vline(aes(xintercept=vl), linetype="dashed", size=.7,color="cadetblue3")+
  scale_color_manual("Sensor",values=c("MODIS"="darkgoldenrod","VIIRS"="cadetblue3","No_Chla"="coral1"))+scale_fill_manual("Sensor",values=c("MODIS"="darkgoldenrod","VIIRS"="cadetblue3","No_Chla"="coral1"))
b=a+labs(y="Density")+labs(x="Scaled Leatherback")+theme(legend.position=c(.7,.9),legend.justification = c(.7,.9))+
  #scale_y_continuous(expand = c(0,0))+scale_x_continuous(expand = c(0,0)) +geom_text(x=.75,y=7,label=paste0("MODIS mean= ",m))+geom_text(x=.75,y=6.5,label=paste0("VIIRS mean= ",v))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))
b=b+scale_y_continuous(expand = c(0,0))+scale_x_continuous(expand = c(0,0))
b=b+geom_vline(aes(xintercept=nl), linetype="dashed", size=.7,color="coral1")
b


outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"#;dir.create(outputDir)
datatype="lbst_noCHLA"

png(paste(outputDir,datatype,"_hist.png",sep=''),width=8,height=8,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
b
dev.off()


### ecocast
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"
noCHLADir="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/output/mean"
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-09-07",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
noCHLAE=list.files(noCHLADir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-09-07",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) 
a=as.data.frame(modisE)
b=gather(a)%>% .[complete.cases(.),] %>% mutate(sensor="MODIS")
c=as.data.frame(viirsE)
d=gather(c) %>% .[complete.cases(.),]%>% mutate(sensor="VIIRS")
e=as.data.frame(noCHLAE)
f=gather(e) %>% .[complete.cases(.),]%>% mutate(sensor="No_Chla")

bb=rbind(b,d,f)%>% mutate(value=scales::rescale(value,c(0,1)))
me=bb %>% group_by(sensor) %>% summarise(mean=mean(value)) %>% .[1,2] %>% as.numeric()
ve=bb %>% group_by(sensor) %>% summarise(mean=mean(value)) %>% .[2,2] %>% as.numeric()
ne=bb %>% group_by(sensor) %>% summarise(mean=mean(value)) %>% .[3,2] %>% as.numeric()

a=ggplot(data=bb,aes(x=value,group=sensor))+#geom_histogram(aes(y=..density..,color=sensor),fill=NA)+
  geom_density(aes(fill=sensor),alpha=.2) + geom_vline(aes(xintercept=me), linetype="dashed", size=.7,color="darkgoldenrod")+ 
  geom_vline(aes(xintercept=ve), linetype="dashed", size=.7,color="cadetblue3")+
  scale_color_manual("Sensor",values=c("MODIS"="darkgoldenrod","VIIRS"="cadetblue3","No_Chla"="coral1"))+scale_fill_manual("Sensor",values=c("MODIS"="darkgoldenrod","VIIRS"="cadetblue3","No_Chla"="coral1"))
b=a+labs(y="Density")+labs(x="Scaled EcoCast")+theme(legend.position=c(.2,.9),legend.justification = c(.9,.9))+
  #scale_y_continuous(expand = c(0,0))+scale_x_continuous(expand = c(0,0)) +geom_text(x=.75,y=7,label=paste0("MODIS mean= ",m))+geom_text(x=.75,y=6.5,label=paste0("VIIRS mean= ",v))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))
b=b+scale_y_continuous(expand = c(0,0))+scale_x_continuous(expand = c(0,0))
b=b+geom_vline(aes(xintercept=ne), linetype="dashed", size=.7,color="coral1")
b

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="ecocast_noCHLA"

png(paste(outputDir,datatype,"_hist.png",sep=''),width=8,height=8,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
b
dev.off()

### summary dataframe

df <- data.frame(matrix(ncol = 4, nrow = 3))
colnames(df)=c("Product","VIIRS","MODIS","NoCHLA")
df$Product=c("CHLA","LBST","ECOCAST")
df$VIIRS=c(vc,vl,ve)
df$MODIS=c(mc,ml,me)
df$NoCHLA=c(NA,nl,ne)
df$difference=round((df$MODIS-df$VIIRS),3)

write.csv(df,paste0(outputDir,"hist_difference_nochla.csv"))

#####

# modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
# viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
# 
# #1. time series of spatial average, just mean 
# sat_m=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# sat_v=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# hist(sat_m,col=rgb(1,0,0,0.5),xlab="Probability of presence")
# hist(sat_v,col=rgb(0,0,1,0.5),add=T)
# 
# 
# modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs"
# viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs"
# noCHLADir="/Users/heatherwelch/Dropbox/JPSS/no_chla/EcoCastRuns/lbst/predCIs"
# 
# 
# 
# 
# ## chla
# 
# modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
# viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
# modisC=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# viirsC=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% grep("2016-09-07",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# 
# hist(modisC,col=rgb(1,0,0,0.5),xlab="Probability of presence")
# hist(viirsC,col=rgb(0,0,1,0.5),add=T)
# 
# ## lbst
# modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/lbst/predCIs"
# viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/lbst/predCIs"
# modisL=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# viirsL=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-09-07",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# 
# hist(modisL,col=rgb(1,0,0,0.5),xlab="Probability of presence")
# hist(viirsL,col=rgb(0,0,1,0.5),add=T)
# 
# ## ecocast
# modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
# viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"
# modisE=list.files(modisDir,full.names = T,recursive = T,pattern = "mean.grd")%>% grep("2016-10-11",.,invert=T,value=T)%>% grep("2016-09-07",.,invert=T,value=T) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = "mean.grd") %>% grep("2016-09-07",.,invert=T,value=T)%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea)) %>% calc(.,mean,na.rm=T)
# 
# hist(modisE,col=rgb(1,0,0,0.5),xlab="Probability of presence")
# hist(viirsE,col=rgb(0,0,1,0.5),add=T)

