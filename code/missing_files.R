## script to find missing rasters 02.07.19

a<-seq(as.Date("2015-08-01"), as.Date("2016-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
b<-seq(as.Date("2016-08-01"), as.Date("2017-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
c<-seq(as.Date("2017-08-01"), as.Date("2018-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
d<-seq(as.Date("2018-08-01"), as.Date("2019-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
x=list(a,b,c,d) %>% unlist()

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
pmlEsaDir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/Satellite"

modis_present=list.files(modisDir)
m_missing=setdiff(x,modis_present) 

viirs_present=list.files(viirsDir)
v_missing=setdiff(x,viirs_present) 

max.len = max(length(v_missing), length(m_missing))
v_missing = c(v_missing, rep(NA, max.len - length(v_missing)))
m_missing = c(m_missing, rep(NA, max.len - length(m_missing)))

df=data.frame("modis_missing"=m_missing,viirs_missing=v_missing)
df$pmlEsa_missing="Fishing season 2018"
df
write.csv(df,"/Users/heatherwelch/Dropbox/JPSS/missing_files/missing02.07.19.csv")


#### deleting SLA in 2015 bc it looks bad
satDir="/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite"
to_match<-seq(as.Date("2015-08-01"), as.Date("2016-01-01"), by = "day",format="%Y/%mm/%dd") %>% as.character()
#a=list.files(satDir,full.names = T,recursive = T,pattern="sla") 
file_list=unique (grep(paste(to_match,collapse="|"),
                       list.files(satDir,full.names = T,recursive = T,pattern="sla") , value=TRUE)) 
file.remove(file_list)
file_list=unique (grep(paste(to_match,collapse="|"),
                       list.files(satDir,full.names = T,recursive = T,pattern="l.eke") , value=TRUE)) 
file.remove(file_list)



modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/EcoCastRuns/output/mean"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/EcoCastRuns/output/mean"
pmlEsaDir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/EcoCastRuns/output/mean"

file_list=unique (grep(paste(to_match,collapse="|"),
                       list.files(modisDir,full.names = T,recursive = T,pattern="2015") , value=TRUE)) 
file.remove(file_list)
file_list=unique (grep(paste(to_match,collapse="|"),
                       list.files(viirsDir,full.names = T,recursive = T,pattern="2015") , value=TRUE)) 
file.remove(file_list)
file_list=unique (grep(paste(to_match,collapse="|"),
                       list.files(pmlEsaDir,full.names = T,recursive = T,pattern="2015") , value=TRUE)) 
file.remove(file_list)

modisCompDirEco="/Users/heatherwelch/Dropbox/JPSS/modis_8DayBin/EcoCastRuns/output/mean"
viirsCompDirEco="/Users/heatherwelch/Dropbox/JPSS/viirs_8DayBin/EcoCastRuns/output/mean"
nochlaCompDirEco="/Users/heatherwelch/Dropbox/JPSS/no_chlaBin/EcoCastRuns/output/mean"


file_list=unique (grep(paste(to_match,collapse="|"),
                       list.files(modisCompDirEco,full.names = T,recursive = T,pattern="2015") , value=TRUE)) 
file.remove(file_list)
file_list=unique (grep(paste(to_match,collapse="|"),
                       list.files(viirsCompDirEco,full.names = T,recursive = T,pattern="2015") , value=TRUE)) 
file.remove(file_list)
file_list=unique (grep(paste(to_match,collapse="|"),
                       list.files(nochlaCompDirEco,full.names = T,recursive = T,pattern="2015") , value=TRUE)) 
file.remove(file_list)