### getting temporal averages of global_regional_lines.R
outputDir="/Users/heatherwelch/Dropbox/JPSS/plots_04.14.19/"

source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")
global=read.csv("/Users/heatherwelch/Dropbox/JPSS/global/csvs/All_products_4km_log_masked.csv") %>% mutate(date=as.Date(date)) %>% mutate(region="Global")
EP=read.csv("/Users/heatherwelch/Dropbox/JPSS/global/eastern_pacific_csvs_mask/All_products_4km_easternP_log_mask.csv") %>% mutate(date=as.Date(date)) %>% mutate(region="Eastern Pacific")
norA=read.csv("/Users/heatherwelch/Dropbox/JPSS/global/north_atlantic_csvs_mask/All_products_4km_northA_log_mask.csv") %>% mutate(date=as.Date(date)) %>% mutate(region="North Atlantic")
SEA=read.csv("/Users/heatherwelch/Dropbox/JPSS/global/SE_Asia_csvs_mask/All_products_4km_SE_A_log_mask.csv") %>% mutate(date=as.Date(date)) %>% mutate(region="Southeast Asia")


master=do.call("rbind",list(global,EP,norA,SEA)) %>% mutate(mean=mean*.4343) %>% group_by(region,sensor) %>% summarise(mean=mean(mean)) %>% as.data.frame()
master2=do.call("rbind",list(global,EP,norA,SEA)) %>% group_by(region,sensor) %>% summarise(sd=sd(mean))%>% as.data.frame()
master3=master %>% mutate(sd=master2$sd) %>% mutate(cv=sd/mean)

write.csv(master3,paste0(outputDir,"summary_global_regional_lines.csv"))
