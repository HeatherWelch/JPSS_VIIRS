### summary table
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)
library(scales)
library(sdmvspecies)

### mean chla difference ----------------------------------------------------> ####
path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

fcnRescale=function(i){
  a <- (i - min(i[], na.rm=TRUE))/(max(i[], na.rm=TRUE)-min(i[], na.rm=TRUE))
}

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask_resub_V3"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask_resub_V3"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite_mask_resub_V3"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite_mask_resub_V3"
ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite_mask_resub_V3"

to_match_date=Reduce(intersect, list(list.files(modisDir),list.files(viirsDir),list.files(gsmDir),list.files(avwDir),list.files(ociDir)))

#### chla ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask_resub_V3"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask_resub_V3"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite_mask_resub_V3"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite_mask_resub_V3"
ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite_mask_resub_V3"

#1. read in and stack
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
gsmE=list.files(gsmDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
avwE=list.files(avwDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
ociE=list.files(ociDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

### p-values, diff from modis ###
m_stats=modisE %>% as.data.frame() %>% gather(layer,chl)%>% mutate(sensor="MODIS")
v_stats=viirsE %>% as.data.frame() %>% gather(layer,chl)%>% mutate(sensor="VIIRS")
g_stats=gsmE %>% as.data.frame() %>% gather(layer,chl)%>% mutate(sensor="GSM")
a_stats=avwE %>% as.data.frame() %>% gather(layer,chl)%>% mutate(sensor="AVW")
o_stats=ociE %>% as.data.frame() %>% gather(layer,chl)%>% mutate(sensor="OC-CCI")

# a=do.call("rbind",list(m_stats,v_stats,g_stats,a_stats,o_stats))%>% mutate(sensor=as.factor(sensor)) %>% select(-layer) %>% .[complete.cases(.),]
a=rbind(m_stats,v_stats) %>% mutate(sensor=as.factor(sensor)) %>% select(-layer) %>% .[complete.cases(.),]
viirs_aov=aov(chl~sensor,data=a)

a=rbind(m_stats,g_stats) %>% mutate(sensor=as.factor(sensor)) %>% select(-layer) %>% .[complete.cases(.),]
gsm_aov=aov(chl~sensor,data=a)

a=rbind(m_stats,a_stats) %>% mutate(sensor=as.factor(sensor)) %>% select(-layer) %>% .[complete.cases(.),]
avw_aov=aov(chl~sensor,data=a)

a=rbind(m_stats,o_stats) %>% mutate(sensor=as.factor(sensor)) %>% select(-layer) %>% .[complete.cases(.),]
oci_aov=aov(chl~sensor,data=a)

### clean up anova stats ###
viirs_summary=summary(viirs_aov)[[1]]%>% as.data.frame(.) %>% .[1,4:5] %>% t() %>% as.data.frame() %>% mutate(stat=rownames(.)) %>% rename("mean"="sensor     ") %>% mutate("product"="VIIRS") 
gsm_summary=summary(gsm_aov)[[1]]%>% as.data.frame(.) %>% .[1,4:5] %>% t() %>% as.data.frame() %>% mutate(stat=rownames(.)) %>% rename("mean"="sensor     ") %>% mutate("product"="GSM") 
avw_summary=summary(avw_aov)[[1]]%>% as.data.frame(.) %>% .[1,4:5] %>% t() %>% as.data.frame() %>% mutate(stat=rownames(.)) %>% rename("mean"="sensor     ") %>% mutate("product"="AVW") 
oci_summary=summary(oci_aov)[[1]]%>% as.data.frame(.) %>% .[1,4:5] %>% t() %>% as.data.frame() %>% mutate(stat=rownames(.)) %>% rename("mean"="sensor     ") %>% mutate("product"="OC-CCI") 

sig_chl=do.call("rbind",list(viirs_summary,gsm_summary,avw_summary,oci_summary))

### get difference as a time-series
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

### cleaning up difference ###
viirs_diff1=viirs_diff%>%  mutate(product="VIIRS")
gsm_diff1=gsm_diff%>%  mutate(product="GSM")
avw_diff1=avw_diff %>%  mutate(product="AVW")
oci_diff1=oci_diff %>% mutate(product="OC-CCI")

master_chla=do.call("rbind",list(viirs_diff1,gsm_diff1,avw_diff1,oci_diff1)) #%>% .[complete.cases(.),]
master_chla$product=as.factor(master_chla$product)
mean_diff_chla=master_chla %>% mutate(chl2=chla)%>% group_by(product) %>% summarise(mean(chl2,na.rm=T))
mean_diff_abs_chla=master_chla %>% mutate(chl2=abs(chla))%>% group_by(product) %>% summarise(mean(chl2,na.rm=T))

#### ecocast ####
modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns_resub_V3/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns_resub_V3/output/mean"
gsmDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/EcoCastRuns_resub_V3/output/mean"
avwDir="/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/EcoCastRuns_resub_V3/output/mean"
ociDir="/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/EcoCastRuns_resub_V3/output/mean"

#1. read in and stack
modisE=list.files(modisDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
viirsE=list.files(viirsDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
gsmE=list.files(gsmDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
avwE=list.files(avwDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
ociE=list.files(ociDir,full.names = T,recursive = T,pattern = ".grd")%>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))

### p-values, diff from modis ###
m_stats=modisE %>% as.data.frame() %>% gather(layer,chl)%>% mutate(sensor="MODIS")
v_stats=viirsE %>% as.data.frame() %>% gather(layer,chl)%>% mutate(sensor="VIIRS")
g_stats=gsmE %>% as.data.frame() %>% gather(layer,chl)%>% mutate(sensor="GSM")
a_stats=avwE %>% as.data.frame() %>% gather(layer,chl)%>% mutate(sensor="AVW")
o_stats=ociE %>% as.data.frame() %>% gather(layer,chl)%>% mutate(sensor="OC-CCI")

# a=do.call("rbind",list(m_stats,v_stats,g_stats,a_stats,o_stats))%>% mutate(sensor=as.factor(sensor)) %>% select(-layer) %>% .[complete.cases(.),]
a=rbind(m_stats,v_stats) %>% mutate(sensor=as.factor(sensor)) %>% select(-layer) %>% .[complete.cases(.),]
viirs_aov=aov(chl~sensor,data=a)

a=rbind(m_stats,g_stats) %>% mutate(sensor=as.factor(sensor)) %>% select(-layer) %>% .[complete.cases(.),]
gsm_aov=aov(chl~sensor,data=a)

a=rbind(m_stats,a_stats) %>% mutate(sensor=as.factor(sensor)) %>% select(-layer) %>% .[complete.cases(.),]
avw_aov=aov(chl~sensor,data=a)

a=rbind(m_stats,o_stats) %>% mutate(sensor=as.factor(sensor)) %>% select(-layer) %>% .[complete.cases(.),]
oci_aov=aov(chl~sensor,data=a)

### clean up anova stats ###
viirs_summary=summary(viirs_aov)[[1]]%>% as.data.frame(.) %>% .[1,4:5] %>% t() %>% as.data.frame() %>% mutate(stat=rownames(.)) %>% rename("mean"="sensor     ") %>% mutate("product"="VIIRS") 
gsm_summary=summary(gsm_aov)[[1]]%>% as.data.frame(.) %>% .[1,4:5] %>% t() %>% as.data.frame() %>% mutate(stat=rownames(.)) %>% rename("mean"="sensor     ") %>% mutate("product"="GSM") 
avw_summary=summary(avw_aov)[[1]]%>% as.data.frame(.) %>% .[1,4:5] %>% t() %>% as.data.frame() %>% mutate(stat=rownames(.)) %>% rename("mean"="sensor     ") %>% mutate("product"="AVW") 
oci_summary=summary(oci_aov)[[1]]%>% as.data.frame(.) %>% .[1,4:5] %>% t() %>% as.data.frame() %>% mutate(stat=rownames(.)) %>% rename("mean"="sensor     ") %>% mutate("product"="OC-CCI") 

sig_eco=do.call("rbind",list(viirs_summary,gsm_summary,avw_summary,oci_summary))

### get difference as a time-series
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

### cleaning up difference ###
viirs_diff1=viirs_diff%>%  mutate(product="VIIRS")
gsm_diff1=gsm_diff%>%  mutate(product="GSM")
avw_diff1=avw_diff %>%  mutate(product="AVW")
oci_diff1=oci_diff %>% mutate(product="OC-CCI")

master_eco=do.call("rbind",list(viirs_diff1,gsm_diff1,avw_diff1,oci_diff1)) #%>% .[complete.cases(.),]
master_eco$product=as.factor(master_eco$product)
mean_diff_eco=master_eco %>% mutate(chl2=chla)%>% group_by(product) %>% summarise(mean(chl2,na.rm=T))
mean_diff_abs_eco=master_eco %>% mutate(chl2=abs(chla))%>% group_by(product) %>% summarise(mean(chl2,na.rm=T))


#### standardize ####
mean_diff_chla1=mean_diff_chla %>%  mutate(master_product="Chlorophyll") %>% mutate(stat="mean_diff") %>% mutate(value=`mean(chl2, na.rm = T)`) %>% select(-`mean(chl2, na.rm = T)`)
mean_diff_eco1=mean_diff_eco%>%  mutate(master_product="EcoCast")%>% mutate(stat="mean_diff") %>% mutate(value=`mean(chl2, na.rm = T)`) %>% select(-`mean(chl2, na.rm = T)`)
mean_diff_abs_chla1=mean_diff_abs_chla%>%  mutate(master_product="Chlorophyll")%>% mutate(stat="mean_abs_diff") %>% mutate(value=`mean(chl2, na.rm = T)`) %>% select(-`mean(chl2, na.rm = T)`)
mean_diff_abs_eco1=mean_diff_abs_eco%>%  mutate(master_product="EcoCast")%>% mutate(stat="mean_abs_diff") %>% mutate(value=`mean(chl2, na.rm = T)`) %>% select(-`mean(chl2, na.rm = T)`)
sig_chl1=sig_chl%>%  mutate(master_product="Chlorophyll") %>% mutate(value=round(mean,2)) %>% select(-mean)
sig_eco1=sig_eco%>%  mutate(master_product="EcoCast") %>% mutate(value=round(mean,2)) %>% select(-mean)


master=do.call("rbind",list(mean_diff_chla1,mean_diff_eco1,mean_diff_abs_chla1,mean_diff_abs_eco1,sig_chl1,sig_eco1)) #%>% .[complete.cases(.),]
master1=master %>% mutate(value=round(value,4)) %>% spread(stat,value) %>% arrange(master_product)

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_12.01.19/";dir.create(outputDir)
write.csv(master1,paste(outputDir,"summaryTable_test8D.csv"))

