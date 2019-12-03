### master histograms - all products

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)
library(scales)
library(sdmvspecies)


outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_12.01.19/"
path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

fcnRescale=function(i){
  a <- (i - min(i[], na.rm=TRUE))/(max(i[], na.rm=TRUE)-min(i[], na.rm=TRUE))
}

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask_resub"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask_resub"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite_mask_resub"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite_mask_resub"
ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite_mask_resub"

to_match_date=Reduce(intersect, list(list.files(modisDir),list.files(viirsDir),list.files(gsmDir),list.files(avwDir),list.files(ociDir)))


#### chla ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask_resub"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask_resub"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite_mask_resub"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite_mask_resub"
ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite_mask_resub"

#1. read in and stack
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
gsmE=list.files(gsmDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
avwE=list.files(avwDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
ociE=list.files(ociDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

# viirs
rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE1-viirsE1 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
viirs_diff=b 

# gsm
rescaledStack=fcnRescale(stack(modisE,gsmE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE1-viirsE1 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
gsm_diff=b 

# avw
rescaledStack=fcnRescale(stack(modisE,avwE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE1-viirsE1 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
avw_diff=b 

# oci
rescaledStack=fcnRescale(stack(modisE,ociE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE1-viirsE1 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
oci_diff=b 

#### ecocast ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns_resub/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns_resub/output/mean"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/EcoCastRuns_resub/output/mean"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/EcoCastRuns_resub/output/mean"
ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/EcoCastRuns_resub/output/mean"

#1. read in and stack
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
gsmE=list.files(gsmDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
avwE=list.files(avwDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
ociE=list.files(ociDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

# viirs
rescaledStack=fcnRescale(stack(modisE,viirsE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE1-viirsE1 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
viirs_diff3=b 

# gsm
rescaledStack=fcnRescale(stack(modisE,gsmE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE1-viirsE1 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
gsm_diff3=b 

# avw
rescaledStack=fcnRescale(stack(modisE,avwE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE1-viirsE1 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
avw_diff3=b 

# oci
rescaledStack=fcnRescale(stack(modisE,ociE))
modisE1=rescaledStack[[grep(".1$",names(rescaledStack))]]
viirsE1=rescaledStack[[grep(".2$",names(rescaledStack))]]

sat_m=modisE1-viirsE1 
names(sat_m)=to_match_date
m_stats=cellStats(sat_m,stat="mean")
b=m_stats %>% as.data.frame() %>% mutate(date=as.Date(to_match_date)) 
colnames(b)=c("chla","date")
b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
oci_diff3=b 

#### standardize ####
viirs_diff1=viirs_diff%>%  mutate(product="VIIRS")
gsm_diff1=gsm_diff%>%  mutate(product="GSM")
avw_diff1=avw_diff %>%  mutate(product="AVW")
oci_diff1=oci_diff %>% mutate(product="OC-CCI")

viirs_diff2=viirs_diff3%>%  mutate(product="VIIRS")
gsm_diff2=gsm_diff3%>%  mutate(product="GSM")
avw_diff2=avw_diff3 %>%  mutate(product="AVW")
oci_diff2=oci_diff3 %>% mutate(product="OC-CCI")

chla=do.call("rbind",list(viirs_diff1,gsm_diff1,avw_diff1,oci_diff1))
eco=do.call("rbind",list(viirs_diff2,gsm_diff2,avw_diff2,oci_diff2))

### histograms ####

histtt_chla=ggplot(chla,aes(x=chla,group=product))+geom_density(aes(fill=product),alpha=.4)+
  geom_vline(xintercept=0,linetype="dashed")+
  scale_fill_manual("Product",values=c("VIIRS"="#518ab1","GSM"="black","AVW"="coral1","OC-CCI"="#8d4138","Blueshark - tracking"="grey"))+
  theme(legend.key.size = unit(.5,'lines'))+xlim(c(-.07,.13))+ylim(c(0,65))+labs(x="MODIS minus other products - Chlorophyll")+labs(y="Density")+
  theme(legend.position=c(.5,.85))+annotate(geom = "text",x=-.07,y=130,label="B",size=4)+
  theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())

histtt_chla

histtt_eco=ggplot(eco,aes(x=chla,group=product))+geom_density(aes(fill=product),alpha=.4)+
  geom_vline(xintercept=0,linetype="dashed")+
  scale_fill_manual("Product",values=c("VIIRS"="#518ab1","GSM"="black","AVW"="coral1","OC-CCI"="#8d4138"))+
  theme(legend.key.size = unit(.5,'lines'))+xlim(c(-.07,.13))+ylim(c(0,65))+labs(x="MODIS minus other products - EcoCast")+labs(y="Density")+
  theme(legend.position=c(.55,.9))+annotate(geom = "text",x=-.07,y=40,label="A",size=4)+
  theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())

histtt_eco

datatype="histogram_line"

png(paste(outputDir,datatype,".png",sep=''),width=20,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
plot_grid(histtt_chla,histtt_eco,nrow = 1,ncol = 2)
#plot_grid(histtt_SPa,histtt_SP,nrow = 1,ncol = 2)
dev.off()

master=rbind(chla_eco,species)
mean_diff=master %>% mutate(chl2=chla)%>% group_by(product) %>% summarise(mean(chl2,na.rm=T))
mean_diff_abs=master %>% mutate(chl2=abs(chla))%>% group_by(product) %>% summarise(mean(chl2,na.rm=T))
