#### checking chl-a importance of species

###lbst
path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
moddir<-paste(path,"/ModRepFiles/",sep="")
lbst=paste0(moddir,"brt_lbst_CIs.rds") %>% read_rds()
blshT=paste0(moddir,"brt_blshTr_CIs.rds")%>% read_rds()
blshO=paste0(moddir,"brt_blshObs_CIs.rds")%>% read_rds()
casl=paste0(moddir,"brt_casl_CIs.rds")%>% read_rds()
swor=paste0(moddir,"brt_swor_CIs.rds")%>% read_rds()

a=summary(casl[[3]])%>% mutate(species=names[i]) %>% mutate(mod_num=i)

models=list(lbst,blshT,blshO,casl,swor)
names=c("lbst","blshT","blshO","casl","swor")

empty=list()

for(i in 1:length(models)){
  print(i)
  print(names[i])
  mod=models[[i]]
  for(ii in 1:10){
    b=mod[[ii]] %>% summary()
    b=b %>% mutate(species=names[i]) %>% mutate(mod_num=ii)
   #empty=list(empty,b) %>% unlist()
    empty[[i*ii]]=b
  }
  
}
a=do.call("rbind",empty)
b=a %>% mutate(var=gsub("_mean","",var))%>% mutate(var=gsub("windy_new","ywind",var))%>% mutate(var=gsub("log_eke","l.eke",var))%>% mutate(var=gsub("analysed_","",var))%>% mutate(var=gsub("logChl","l.blendChl",var))
as.factor(b$var) %>% unique()

c=b %>% group_by(species,var) %>% summarise(mean=mean(rel.inf,na.rm=T))
d=b %>% group_by(species,var) %>% summarise(sd=sd(rel.inf))
c$sd=d$sd

d=ggplot(c,aes(x=var,y=mean))+geom_bar(aes(fill=var),stat = "identity",color="black")+facet_grid(~species,scales="free_x")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_errorbar(aes(x=var,ymin=mean-sd,ymax=mean+sd))+scale_fill_manual("Covariate",values=c("l.blendChl"="blue","l.eke"="grey","lunillum"="grey","sla"="grey","sla_sd"="grey","sst"="grey","sst_sd"="grey","ywind"="grey","z"="grey","zsd"="grey"))+
  ylab("Mean variable influence across 10 models")+xlab("Covariates")+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))
d

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_02.14.19/"
datatype="variable influence"

png(paste(outputDir,datatype,".png",sep=''),width=24,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
d
dev.off()
