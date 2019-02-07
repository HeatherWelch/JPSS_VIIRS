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
