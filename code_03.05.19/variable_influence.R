#### checking chl-a importance of species
library(pdp)

###variable influence ####  
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
    empty[[(i*10)+ii]]=b 
  }
  
}
a=do.call("rbind",empty)
b=a %>% mutate(var=gsub("_mean","",var))%>% mutate(var=gsub("windy_new","ywind",var))%>% mutate(var=gsub("log_eke","l.eke",var))%>% mutate(var=gsub("analysed_","",var))%>% mutate(var=gsub("logChl","l.blendChl",var))
as.factor(b$var) %>% unique()

c=b %>% group_by(species,var) %>% summarise(mean=mean(rel.inf,na.rm=T))
d=b %>% group_by(species,var) %>% summarise(sd=sd(rel.inf))
c$sd=d$sd

c <- within(c, species[species == 'blshO'] <- 'Blueshark - Observer')
c <- within(c, species[species == 'blshT'] <- 'Blueshark - Tracking')
c <- within(c, species[species == 'casl'] <- 'Sealion')
c <- within(c, species[species == 'lbst'] <- 'Leatherback')
c <- within(c, species[species == 'swor'] <- 'Swordfish')

d=ggplot(c,aes(x=var,y=mean))+geom_bar(aes(fill=var),stat = "identity",color="black")+facet_grid(~species,scales="free_x")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_errorbar(aes(x=var,ymin=mean-sd,ymax=mean+sd))+scale_fill_manual("Covariate",values=c("l.blendChl"="blue","l.eke"="grey","lunillum"="grey","sla"="grey","sla_sd"="grey","sst"="grey","sst_sd"="grey","ywind"="grey","z"="grey","zsd"="grey"))+
  ylab("Mean variable influence across 10 models (+/- 1sd")+xlab("Covariates")+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))
d

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"
datatype="variable_influence"

png(paste(outputDir,datatype,".png",sep=''),width=24,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
d
dev.off()


### partial plots ####
path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
moddir<-paste(path,"/ModRepFiles/",sep="")
lbst=paste0(moddir,"brt_lbst_CIs.rds") %>% read_rds()
blshT=paste0(moddir,"brt_blshTr_CIs.rds")%>% read_rds()
blshO=paste0(moddir,"brt_blshObs_CIs.rds")%>% read_rds()
casl=paste0(moddir,"brt_casl_CIs.rds")%>% read_rds()
swor=paste0(moddir,"brt_swor_CIs.rds")%>% read_rds()

b=plot.gbm(lbst[[4]],i.var = "logChl",return.grid = F)
plot.gbm(lbst[[4]],i.var = c(4),lwd = 2, col = "blue", main = "")

a=lbst[[4]]

partial(lbst[[4]],pred.var="logChl",plot=T)

models=list(lbst,blshT,blshO,casl,swor)
names=c("lbst","blshT","blshO","casl","swor")

empty=list()

for(i in 1:length(models)){
  print(i)
  print(names[i])
  mod=models[[i]]
  for(ii in 1:10){
    b=mod[[ii]] %>% summary()
    if("l.blendChl" %in% b$var){
      b=plot.gbm(mod[[ii]],i.var = "l.blendChl",return.grid = T)%>% mutate(species=names[i]) %>% mutate(mod_num=ii) %>% dplyr::rename(logChl=l.blendChl)
    }
    if("logChl" %in% b$var){
      b=plot.gbm(mod[[ii]],i.var = "logChl",return.grid = T)%>% mutate(species=names[i]) %>% mutate(mod_num=ii)
    }
    empty[[(i*10)+ii]]=b
    print((i*10)+ii)
  }
  
}
a=do.call("rbind",empty)
b=a %>% mutate(id=rep(1:100,50)) %>% group_by(species,id) %>% summarise(meanx=mean(logChl))
bb=a %>% mutate(id=rep(1:100,50)) %>% group_by(species,id) %>% summarise(meany=mean(y))
bb$meanx=b$meanx


c=ggplot()+geom_line(data=a,aes(x=logChl,y=y,group=mod_num))
d=c+geom_line(data=bb,aes(x=meanx,y=meany),color="red")+facet_grid(~species,scales="free")
d

####

c=a %>% mutate(id=rep(1:100,50)) %>% group_by(species,id) %>% summarise(minx=min(y))
cc=a %>% mutate(id=rep(1:100,50)) %>% group_by(species,id) %>% summarise(maxy=max(y))
cc$minx=c$minx
cc$x=bb$meanx

#d=a%>% mutate(id=rep(1:100,50)) %>% filter(species=="blshO") %>% filter(id==1)

bb <- within(bb, species[species == 'blshO'] <- 'Blueshark - Observer')
bb <- within(bb, species[species == 'blshT'] <- 'Blueshark - Tracking')
bb <- within(bb, species[species == 'casl'] <- 'Sealion')
bb <- within(bb, species[species == 'lbst'] <- 'Leatherback')
bb <- within(bb, species[species == 'swor'] <- 'Swordfish')

cc <- within(cc, species[species == 'blshO'] <- 'Blueshark - Observer')
cc <- within(cc, species[species == 'blshT'] <- 'Blueshark - Tracking')
cc <- within(cc, species[species == 'casl'] <- 'Sealion')
cc <- within(cc, species[species == 'lbst'] <- 'Leatherback')
cc <- within(cc, species[species == 'swor'] <- 'Swordfish')

d=ggplot()+geom_line(data=bb,aes(x=meanx,y=meany),color="red")+facet_grid(~species,scales="free")
e=d+geom_ribbon(data=cc,aes(x=x,ymin=minx,ymax=maxy),fill="grey",alpha=.5)+facet_grid(~species,scales="free")+
  ylab("Chl-a influence")+xlab("Chl-a (mg/m3)")+
  theme(axis.text = element_text(size=6),axis.title = element_text(size=6),legend.text=element_text(size=6),legend.title = element_text(size=6),strip.text.y = element_text(size = 6),strip.text.x = element_text(size = 6), strip.background = element_blank())+
  theme(legend.key.size = unit(.5,'lines'))
e

outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_03.05.19/"
datatype="partial_plot"

png(paste(outputDir,datatype,".png",sep=''),width=24,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
e
dev.off()
