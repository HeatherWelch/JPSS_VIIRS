##### plots of all covariates for years. to see why 2017 and 2018 are so different

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
library(padr)
library(zoo)

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
staticdir=paste0(path,"/static_variables/")
studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")

satDir="/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite"

a<-seq(as.Date("2015-08-01"), as.Date("2016-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
b<-seq(as.Date("2016-08-01"), as.Date("2017-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
c<-seq(as.Date("2017-08-01"), as.Date("2018-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
d<-seq(as.Date("2018-08-01"), as.Date("2019-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
dates=list(a,b,c,d) %>% unlist()

get_timeseries=function(satDir,variable,studyarea,dates){
  print("creating stack")
  print(as.character(variable))
  dates_list=unique (grep(paste(dates,collapse="|"),
                          list.files(satDir,pattern = paste0(variable,".grd"),recursive = T), value=TRUE)) %>% gsub(paste0("/",variable,".grd"),"",.)
  variable_list <- unique (grep(paste(dates,collapse="|"),
                      list.files(satDir,pattern = paste0(variable,".grd"),full.names = T,recursive = T), value=TRUE)) %>% stack() %>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  names(variable_list)=dates_list
  stats=cellStats(variable_list,stat="mean")
  print("performing stats")
  b=stats %>% as.data.frame() %>% mutate(date=as.Date(dates_list))
  colnames(b)=c("value","date")
  b=b %>% mutate(year=as.factor(strtrim(as.character(date),4))) %>% filter(year!=2012&year!=2019)%>% mutate(month=as.factor(substr(as.character(date),6,7)))%>% filter(month!="01"&month!="07")
  df=b %>% mutate(Variable=variable)
return(df)
}


covariates=c("analysed_sst","l.eke_mean","sla","ywind")

sst=get_timeseries(satDir = satDir,variable=covariates[1],studyarea = studyarea,dates=dates)
eke=get_timeseries(satDir = satDir,variable=covariates[2],studyarea = studyarea,dates=dates)
sla=get_timeseries(satDir = satDir,variable=covariates[3],studyarea = studyarea,dates=dates)
wind=get_timeseries(satDir = satDir,variable=covariates[4],studyarea = studyarea,dates=dates)

master=do.call("rbind",list(sst,eke,sla,wind))
master$Variable=as.factor(master$Variable)

lineplot=ggplot(master,aes(x=date,y=value))+geom_line(aes(group=Variable,color=Variable),size=.3)+geom_point(aes(color=Variable),size=.6)+
  scale_x_date(date_breaks="month",date_labels = "%b",date_minor_breaks = "months")+
  scale_color_manual("Variable",values=c("analysed_sst"="darkgoldenrod","l.eke_mean"="coral1","sla"="gray","ywind"="cadetblue3"))+
  facet_grid(Variable~year, scales="free")+labs(x="Date")+labs(y="Chlorophyla (mg^-3)")+theme(legend.position=c(.1,.9),legend.justification = c(.9,.9))+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))


lineplot

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots/"
datatype="allVars"

png(paste(outputDir,datatype,"_line.png",sep=''),width=18,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
lineplot
dev.off()

