#### master line figure
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")

### global ####
global=read.csv("/Users/heatherwelch/Dropbox/JPSS/global/csvs/All_products_4km_log_masked.csv") %>% mutate(date=as.Date(date)) %>% mutate(mean=mean*.4343)
a=ggplot(global,aes(x=date,y=mean)) +geom_line(aes(group=sensor,color=sensor,linetype=sensor),size=.5)+geom_point(aes(color=sensor))+
  scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months")+
  theme(legend.position=c(.1,.2),legend.justification = c(.4,.4))+annotate(geom = "text",x=as.Date("2012-01-01"),y=-1.5,label="A. Global",size=4)+
  theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 8),strip.text.x = element_text(size = 8), strip.background = element_blank())+
  theme(legend.key.size = unit(.8,'lines'))+ylim(-1.1,-0.64)+
  scale_color_manual("Product",values=c("VIIRS"="#d3ad06","MODIS"="#0066cc","OC-CCI"="red","GlobColour Merged GSM"="black","GlobColour Merged AVW"="darkgreen"))+ylab("log chlorophyll (mg m-3)")+xlab("Year")+
  scale_linetype_manual("Product",values = c("VIIRS"="dashed","MODIS"="dashed","OC-CCI"="solid","GlobColour Merged GSM"="solid","GlobColour Merged AVW"="solid"))

a

### eastern pacific ####
EP=read.csv("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_csvs_mask/All_products_4km_easternP_log_mask.csv") %>% mutate(date=as.Date(date))%>% mutate(mean=mean*.4343)
b=ggplot(EP,aes(x=date,y=mean)) +geom_line(aes(group=sensor,color=sensor,linetype=sensor),size=.5)+geom_point(aes(color=sensor))+
  scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months")+
  theme(legend.position=c(.9,.9),legend.justification = c(.4,.4))+annotate(geom = "text",x=as.Date("2012-04-01"),y=-1.5,label="B. Eastern Pacific",size=4)+
  theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 8),strip.text.x = element_text(size = 8), strip.background = element_blank())+
  theme(legend.key.size = unit(.8,'lines'))+ylim(-1.1,-0.64)+
  scale_color_manual("Product",values=c("VIIRS"="#d3ad06","MODIS"="#0066cc","OC-CCI"="red","GlobColour Merged GSM"="black","GlobColour Merged AVW"="darkgreen"),guide=F)+ylab("log chlorophyll (mg m-3)")+xlab("Year")+
  scale_linetype_manual("Product",values = c("VIIRS"="dashed","MODIS"="dashed","OC-CCI"="solid","GlobColour Merged GSM"="solid","GlobColour Merged AVW"="solid"),guide=F)

b

### north atlanctic ####
norA=read.csv("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_csvs_mask/All_products_4km_northA_log_mask.csv") %>% mutate(date=as.Date(date))%>% mutate(mean=mean*.4343)
c=ggplot(norA,aes(x=date,y=mean)) +geom_line(aes(group=sensor,color=sensor,linetype=sensor),size=.5)+geom_point(aes(color=sensor))+
  scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months")+
  theme(legend.position=c(.9,.9),legend.justification = c(.4,.4))+annotate(geom = "text",x=as.Date("2012-04-15"),y=-1.5,label="C. Northern Atlantic",size=4)+
  theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 8),strip.text.x = element_text(size = 8), strip.background = element_blank())+
  theme(legend.key.size = unit(.8,'lines'))+ylim(-1.1,-0.64)+
  scale_color_manual("Product",values=c("VIIRS"="#d3ad06","MODIS"="#0066cc","OC-CCI"="red","GlobColour Merged GSM"="black","GlobColour Merged AVW"="darkgreen"),guide=F)+ylab("log chlorophyll (mg m-3)")+xlab("Year")+
  scale_linetype_manual("Product",values = c("VIIRS"="dashed","MODIS"="dashed","OC-CCI"="solid","GlobColour Merged GSM"="solid","GlobColour Merged AVW"="solid"),guide=F)

c

### SE Asia ####
SEA=read.csv("/Users/heatherwelch/Dropbox/JPSS/global/SE_Asia_csvs_mask/All_products_4km_SE_A_log_mask.csv") %>% mutate(date=as.Date(date))%>% mutate(mean=mean*.4343)
d=ggplot(SEA,aes(x=date,y=mean)) +geom_line(aes(group=sensor,color=sensor,linetype=sensor),size=.5)+geom_point(aes(color=sensor))+
  scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months")+
  theme(legend.position=c(.9,.9),legend.justification = c(.4,.4))+annotate(geom = "text",x=as.Date("2012-04-01"),y=-1.5,label="D. Southeast Asia",size=4)+
  theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 8),strip.text.x = element_text(size = 8), strip.background = element_blank())+
  theme(legend.key.size = unit(.8,'lines'))+ylim(-1.1,-0.64)+
  scale_color_manual("Product",values=c("VIIRS"="#d3ad06","MODIS"="#0066cc","OC-CCI"="red","GlobColour Merged GSM"="black","GlobColour Merged AVW"="darkgreen"),guide=F)+ylab("log chlorophyll (mg m-3)")+xlab("Year")+
  scale_linetype_manual("Product",values = c("VIIRS"="dashed","MODIS"="dashed","OC-CCI"="solid","GlobColour Merged GSM"="solid","GlobColour Merged AVW"="solid"),guide=F)

d


datatype="global_regional_lines"
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_04.14.19/"

png(paste(outputDir,datatype,".png",sep=''),width=48,height=24,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
plot_grid(a,b,c,d,nrow = 2,ncol = 2)
dev.off()
