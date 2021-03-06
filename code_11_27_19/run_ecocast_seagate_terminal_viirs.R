### running ecocast on the downloaded data
source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/load_functions.R")

# run_ecocast=function(data_product,date,datapath){
#   get_date=as.Date(date)
#   print(get_date)
#   
#   ## paths for JPSS ####
#   envdir2=paste(datapath,"/",data_product,"/Satellite_mask_resub/",sep="") 
#   outdir=paste(datapath,"/",data_product,"/EcoCastRuns_resub/",sep="") 
#   if(!dir.exists(outdir)){dir.create(outdir)}
#   ecocastdir=paste(outdir,"output/",sep="")
#   if(!dir.exists(ecocastdir)){dir.create(ecocastdir)}
#   outdir_M=paste(ecocastdir,"mean/",sep="") 
#   if(!dir.exists(outdir_M)){dir.create(outdir_M)}
#   outdir_M1=paste(ecocastdir,"mean/latest/",sep="") 
#   if(!dir.exists(outdir_M1)){dir.create(outdir_M1)}
#   outdir_S=paste(ecocastdir,"se/",sep="") 
#   if(!dir.exists(outdir_S)){dir.create(outdir_S)}
#   outdir_S=paste(ecocastdir,"se/latest/",sep="") 
#   if(!dir.exists(outdir_S)){dir.create(outdir_S)}
#   
#   filelist=list.files(outdir_M,pattern=as.character(get_date),full.names = TRUE)
#   if(length(filelist)==0){
#   
#   blshObs=paste(outdir,"blshObs/",sep="")
#   if(!dir.exists(blshObs)){dir.create(blshObs)}
#   blshTr=paste(outdir,"blshTr/",sep="")
#   if(!dir.exists(blshTr)){dir.create(blshTr)}
#   casl=paste(outdir,"casl/",sep="")
#   if(!dir.exists(casl)){dir.create(casl)}
#   swor=paste(outdir,"swor/",sep="")
#   if(!dir.exists(swor)){dir.create(swor)}
#   lbst=paste(outdir,"lbst/",sep="")
#   if(!dir.exists(lbst)){dir.create(lbst)}
#   finaldir_data_product=paste(envdir2,get_date,sep="")
#   
#   if(file.exists(finaldir_data_product)){
#   
#   blshObs=paste(outdir,"blshObs/predCIs/",sep="")
#   if(!dir.exists(blshObs)){dir.create(blshObs)}
#   blshTr=paste(outdir,"blshTr/predCIs/",sep="")
#   if(!dir.exists(blshTr)){dir.create(blshTr)}
#   casl=paste(outdir,"casl/predCIs/",sep="")
#   if(!dir.exists(casl)){dir.create(casl)}
#   swor=paste(outdir,"swor/predCIs/",sep="")
#   if(!dir.exists(swor)){dir.create(swor)}
#   lbst=paste(outdir,"lbst/predCIs/",sep="")
#   if(!dir.exists(lbst)){dir.create(lbst)}
#   
#   ## paths for general EcoCast ####
#   path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
#   logodir<-"/Volumes/EcoCast_SeaGate/EcoCast_HW/EcoCastGit_private/logo/"
#   staticdir=paste0(path,"/static_variables/")
#   moddir<-paste(path,"/ModRepFiles/",sep="")
#   envdir=paste(path,"/SpatialPredictions_EnvData/Satellite/",sep="") 
#   finaldir=paste(envdir,get_date,sep="")
#   source("/Volumes/EcoCast_SeaGate/EcoCast_HW/EcoCastGit_private/Code/Operationalizing_code_V3/2_load_libraries.R",chdir = TRUE)
#   source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/5_plot_EcoCast.R")
#   source("/Volumes/EcoCast_SeaGate/EcoCast_HW/EcoCastGit_private/Code/Operationalizing_code_V3/4_predict_CIs.R")
#   
#   # C. Define species weightings ####
#   namesrisk<-c("Blue shark bycatch","Blue sharks","Sea lions","Leatherbacks","Swordfish")
#   ecocastrisk<-c(-0.1,-0.1,-0.05,-0.9,0.9) #upweight swordfish a bit
#   bycatchrisk<-c(-0.1,-0.1,-0.1,-0.7,0) #all non-target species
#   
#   ###### 5. Define global objects ####
#   #these are grabbed from sla_mean.grd in /EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2012-08-04
#   template=raster() ##create template for resampling
#   res(template)=0.2487562
#   ncol(template)=201
#   nrow(template)=201
#   xmin(template)=-149.875
#   xmax(template)=-99.875
#   ymin(template)=10.125
#   ymax(template)=60.125
#   projection(template)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#   studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")
#   
#   ############ 2. Define time and dynamic directories ####
#   #get_date=date
#   print(get_date)
#   most_recent=as.character(get_date-1) ##change for each user
#   get_date_composite=get_date-4
#   year=get_date %>% as.character() %>% substr(.,start=1,stop=4)
#   month=get_date %>% as.character() %>% substr(.,start=6,stop=7)
#   
#   ### F. Checks for the most recent layer for a given variable within a three-day window and returns the full path ####
#   move_file=function(final_name){
#     if(file.exists(paste(envdir,most_recent,"/",final_name,".grd",sep=""))==TRUE){ # if url isn't successfully found, start checking for older layers, but check mtime of layer to make sure it's within 3 days window
#       print(paste(final_name," doesn't exist for ",get_date,", using most recent file instead (",most_recent,").",sep=""))
#       file_path=paste(envdir,most_recent,"/",final_name,".grd",sep="")
#     }else if (file.exists(paste(envdir,as.Date(most_recent)-1,"/",final_name,".grd",sep=""))==TRUE){
#       print(paste(final_name," doesn't exist for ",get_date,", using most recent file instead (",as.Date(most_recent)-1,").",sep=""))
#       file_path=paste(envdir,as.Date(most_recent)-1,"/",final_name,".grd",sep="")
#     }else if (file.exists(paste(envdir,as.Date(most_recent)-2,"/",final_name,".grd",sep=""))==TRUE){
#       print(paste(final_name," doesn't exist for ",get_date,", using most recent file instead (",as.Date(most_recent)-2,").",sep=""))
#       file_path=paste(envdir,as.Date(most_recent)-2,"/",final_name,".grd",sep="")
#     }else{
#       print(paste(final_name," not available within the past three days, EcoCast run for ",get_date," will not include ",final_name,sep=""))
#       file_path=NULL
#     }
#     return(file_path)
#   }
#   
#   ############ 11.Get a list of the paths of the env variables for get_date, or the most recent path if missing ####
#   FileList_get_date=list.files(paste(envdir,get_date,sep=""),pattern="*.grd$") # all the files from get_date
#   FileList_full=c("analysed_sst.grd","analysed_sst_sd.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd") # all of the dynamic variables, static ones will always be there
#   FileList_missing=setdiff(FileList_full,FileList_get_date) # list of dynamic variables missing from get_date
#   FileList_final=list.files(paste(envdir,get_date,sep=""),pattern="*.grd$",full.names = TRUE) %>% grep("l.blendChl",.,inv=T, value=T)# start of final list to pass to preCIs script
#   chla=list.files(paste0(envdir2,get_date),pattern = "*grd",full.names = T)
#   FileList_final=list(FileList_final,chla) %>% unlist()
#   
#   for(missing in FileList_missing){ # for each missing dynamic variable
#     print(paste(missing," is missing from ",get_date,sep=""))
#     final_name=gsub(".grd","",missing) # get rid of .grd to match move_file function (need the grid to differentiate between the means and sds)
#     pathA=move_file(final_name=final_name) # get the pathway of the most recent version of each dynamic variable and..
#     FileList_final=unlist(list(FileList_final,pathA)) # add it to our final path list
#   }
#   
#   a=lapply(FileList_final,function(x)unlist(strsplit(x,"/")))
#   available=unlist(lapply(a,function(x)x[length(x)])) # files available for get_date or get_date - 3
#   FileList_missing=setdiff(FileList_full,available) # files unavailable for get_date or get_date - 3
#   
#   return_list=list("FileList_final"=FileList_final,"FileList_missing"=FileList_missing) # return list comprised to two lists: files we have for get date, and files we don't have for get_date
#   
#   predCIs_master(get_date=get_date,envdir = envdir,moddir=moddir,outdir = outdir,path = path,final_path_list=return_list)
#   Run_ecocast(get_date=get_date,moddir=moddir,outdir = outdir,ecocastdir = ecocastdir,namesrisk=namesrisk,ecocastrisk=ecocastrisk,bycatchrisk=bycatchrisk,final_path_list=return_list,logodir=logodir,studyarea=studyarea,staticdir=staticdir)
#   }
#   }
# }

# ####--------------------------------> older code with different time stamps ####
# 
# #### modis 2015 - 2018; 8 day (running w new sla) ####
# dates=list.files("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask_resub")
# data_product="modis_8Day"
# datapath="/Users/heatherwelch/Dropbox/JPSS"
# lapply(dates,FUN=run_ecocast,data_product=data_product,datapath=datapath)
# 
# #### viirs 2015 - 2018; 8 day (running w new sla) ####
# dates=list.files("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask_resub")
# data_product="viirs_8Day"
# datapath="/Users/heatherwelch/Dropbox/JPSS"
# lapply(dates,FUN=run_ecocast,data_product=data_product,datapath=datapath)
# 
# #### globcolour_GSM 2015 - 2018; 8 day (running w new sla) ####
# dates=list.files("/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8day/Satellite_mask_resub")
# data_product="globcolour_GSM_8day"
# datapath="/Users/heatherwelch/Dropbox/JPSS"
# lapply(dates,FUN=run_ecocast,data_product=data_product,datapath=datapath)
# 
# #### globcolour_AVW 2015 - 2018; 8 day (running w new sla) ####
# dates=list.files("/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8day/Satellite_mask_resub")
# data_product="globcolour_AVW_8day"
# datapath="/Users/heatherwelch/Dropbox/JPSS"
# lapply(dates,FUN=run_ecocast,data_product=data_product,datapath=datapath)
# 
# #### oc_cci 2015 - 2018; 8 day (running w new sla) ####
# dates=list.files("/Users/heatherwelch/Dropbox/JPSS/oc_cci_8day/Satellite_mask_resub")
# data_product="oc_cci_8day"
# datapath="/Users/heatherwelch/Dropbox/JPSS"
# lapply(dates,FUN=run_ecocast,data_product=data_product,datapath=datapath)


####--------------------------------> new code with constant time stamp of 8 day rolling average named @ 5th day, pathways set to work off seagate ####
library(glue)
library(sf)
library(parallel)
run_ecocast=function(data_product,date,datapath){
  get_date=as.Date(date)
  print(get_date)
  
  ## paths for JPSS ####
  envdir2=paste(datapath,"/",data_product,"/Satellite_mask_resub_V3/",sep="") 
  outdir=paste(datapath,"/",data_product,"/EcoCastRuns_resub_V3/",sep="") 
  if(!dir.exists(outdir)){dir.create(outdir)}
  ecocastdir=paste(outdir,"output/",sep="")
  if(!dir.exists(ecocastdir)){dir.create(ecocastdir)}
  outdir_M=paste(ecocastdir,"mean/",sep="") 
  if(!dir.exists(outdir_M)){dir.create(outdir_M)}
  outdir_M1=paste(ecocastdir,"mean/latest/",sep="") 
  if(!dir.exists(outdir_M1)){dir.create(outdir_M1)}
  outdir_S=paste(ecocastdir,"se/",sep="") 
  if(!dir.exists(outdir_S)){dir.create(outdir_S)}
  outdir_S=paste(ecocastdir,"se/latest/",sep="") 
  if(!dir.exists(outdir_S)){dir.create(outdir_S)}
  
  filelist=list.files(outdir_M,pattern=as.character(get_date),full.names = TRUE)
  if(length(filelist)==0){
    
    blshObs=paste(outdir,"blshObs/",sep="")
    if(!dir.exists(blshObs)){dir.create(blshObs)}
    blshTr=paste(outdir,"blshTr/",sep="")
    if(!dir.exists(blshTr)){dir.create(blshTr)}
    casl=paste(outdir,"casl/",sep="")
    if(!dir.exists(casl)){dir.create(casl)}
    swor=paste(outdir,"swor/",sep="")
    if(!dir.exists(swor)){dir.create(swor)}
    lbst=paste(outdir,"lbst/",sep="")
    if(!dir.exists(lbst)){dir.create(lbst)}
    finaldir_data_product=paste(envdir2,get_date,sep="")
    
    if(file.exists(finaldir_data_product)){
      
      blshObs=paste(outdir,"blshObs/predCIs/",sep="")
      if(!dir.exists(blshObs)){dir.create(blshObs)}
      blshTr=paste(outdir,"blshTr/predCIs/",sep="")
      if(!dir.exists(blshTr)){dir.create(blshTr)}
      casl=paste(outdir,"casl/predCIs/",sep="")
      if(!dir.exists(casl)){dir.create(casl)}
      swor=paste(outdir,"swor/predCIs/",sep="")
      if(!dir.exists(swor)){dir.create(swor)}
      lbst=paste(outdir,"lbst/predCIs/",sep="")
      if(!dir.exists(lbst)){dir.create(lbst)}
      
      ## paths for general EcoCast ####
      path = "/Volumes/SeaGate/EcoCast_CodeArchive"
      logodir<-"/Volumes/SeaGate/EcoCast_CodeArchive/logo/"
      staticdir=paste0(path,"/static_variables/")
      moddir<-paste(path,"/ModRepFiles/",sep="")
      envdir=paste(path,"/SpatialPredictions_EnvData/Satellite/",sep="") 
      finaldir=paste(envdir,get_date,sep="")
      source("/Volumes/SeaGate/EcoCast_CodeArchive/Operationalizing_code_V3/2_load_libraries.R",chdir = TRUE)
      source("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/code/5_plot_EcoCast_seagate.R")
      source("/Volumes/SeaGate/EcoCast_CodeArchive/Operationalizing_code_V3/4_predict_CIs.R")
      
      # C. Define species weightings ####
      namesrisk<-c("Blue shark bycatch","Blue sharks","Sea lions","Leatherbacks","Swordfish")
      ecocastrisk<-c(-0.1,-0.1,-0.05,-0.9,0.9) #upweight swordfish a bit
      bycatchrisk<-c(-0.1,-0.1,-0.1,-0.7,0) #all non-target species
      
      ###### 5. Define global objects ####
      #these are grabbed from sla_mean.grd in /EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2012-08-04
      template=raster() ##create template for resampling
      res(template)=0.2487562
      ncol(template)=201
      nrow(template)=201
      xmin(template)=-149.875
      xmax(template)=-99.875
      ymin(template)=10.125
      ymax(template)=60.125
      projection(template)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      # studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")
      studyarea=st_read(glue("{staticdir}sa_square_coast3.shp"))
      studyarea <- sf:::st_zm(studyarea$geom)
      studyarea=as(studyarea, "Spatial")
      
      
      ############ 2. Define time and dynamic directories ####
      #get_date=date
      print(get_date)
      most_recent=as.character(get_date-1) ##change for each user
      get_date_composite=get_date-4
      year=get_date %>% as.character() %>% substr(.,start=1,stop=4)
      month=get_date %>% as.character() %>% substr(.,start=6,stop=7)
      
      ### F. Checks for the most recent layer for a given variable within a three-day window and returns the full path ####
      move_file=function(final_name){
        if(file.exists(paste(envdir,most_recent,"/",final_name,".grd",sep=""))==TRUE){ # if url isn't successfully found, start checking for older layers, but check mtime of layer to make sure it's within 3 days window
          print(paste(final_name," doesn't exist for ",get_date,", using most recent file instead (",most_recent,").",sep=""))
          file_path=paste(envdir,most_recent,"/",final_name,".grd",sep="")
        }else if (file.exists(paste(envdir,as.Date(most_recent)-1,"/",final_name,".grd",sep=""))==TRUE){
          print(paste(final_name," doesn't exist for ",get_date,", using most recent file instead (",as.Date(most_recent)-1,").",sep=""))
          file_path=paste(envdir,as.Date(most_recent)-1,"/",final_name,".grd",sep="")
        }else if (file.exists(paste(envdir,as.Date(most_recent)-2,"/",final_name,".grd",sep=""))==TRUE){
          print(paste(final_name," doesn't exist for ",get_date,", using most recent file instead (",as.Date(most_recent)-2,").",sep=""))
          file_path=paste(envdir,as.Date(most_recent)-2,"/",final_name,".grd",sep="")
        }else{
          print(paste(final_name," not available within the past three days, EcoCast run for ",get_date," will not include ",final_name,sep=""))
          file_path=NULL
        }
        return(file_path)
      }
      
      ############ 11.Get a list of the paths of the env variables for get_date, or the most recent path if missing ####
      FileList_get_date=list.files(paste(envdir,get_date,sep=""),pattern="*.grd$") # all the files from get_date
      FileList_full=c("analysed_sst.grd","analysed_sst_sd.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd") # all of the dynamic variables, static ones will always be there
      FileList_missing=setdiff(FileList_full,FileList_get_date) # list of dynamic variables missing from get_date
      FileList_final=list.files(paste(envdir,get_date,sep=""),pattern="*.grd$",full.names = TRUE) %>% grep("l.blendChl",.,inv=T, value=T)# start of final list to pass to preCIs script
      chla=list.files(paste0(envdir2,get_date),pattern = "*grd",full.names = T)
      FileList_final=list(FileList_final,chla) %>% unlist()
      
      for(missing in FileList_missing){ # for each missing dynamic variable
        print(paste(missing," is missing from ",get_date,sep=""))
        final_name=gsub(".grd","",missing) # get rid of .grd to match move_file function (need the grid to differentiate between the means and sds)
        pathA=move_file(final_name=final_name) # get the pathway of the most recent version of each dynamic variable and..
        FileList_final=unlist(list(FileList_final,pathA)) # add it to our final path list
      }
      
      a=lapply(FileList_final,function(x)unlist(strsplit(x,"/")))
      available=unlist(lapply(a,function(x)x[length(x)])) # files available for get_date or get_date - 3
      FileList_missing=setdiff(FileList_full,available) # files unavailable for get_date or get_date - 3
      
      return_list=list("FileList_final"=FileList_final,"FileList_missing"=FileList_missing) # return list comprised to two lists: files we have for get date, and files we don't have for get_date
      
      predCIs_master(get_date=get_date,envdir = envdir,moddir=moddir,outdir = outdir,path = path,final_path_list=return_list)
      Run_ecocast(get_date=get_date,moddir=moddir,outdir = outdir,ecocastdir = ecocastdir,namesrisk=namesrisk,ecocastrisk=ecocastrisk,bycatchrisk=bycatchrisk,final_path_list=return_list,logodir=logodir,studyarea=studyarea,staticdir=staticdir)
    }
  }
}
# #### modis 2015 - 2018; 8 day (running w new sla) ####
# dates=list.files("/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite_mask_resub_V3")
# data_product="modis_8Day"
# datapath="/Users/heatherwelch/Dropbox/JPSS"
# lapply(dates,FUN=run_ecocast,data_product=data_product,datapath=datapath)
# 
#### viirs 2015 - 2018; 8 day (running w new sla) ####
dates=list.files("/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite_mask_resub_V3")
data_product="viirs_8Day"
datapath="/Users/heatherwelch/Dropbox/JPSS"
mclapply(dates,FUN=run_ecocast,data_product=data_product,datapath=datapath,mc.cores=4)

# #### globcolour_GSM 2015 - 2018; 8 day (running w new sla) ####
# dates=list.files("/Users/heatherwelch/Dropbox/JPSS/globcolour_GSM_8Day/Satellite_mask_resub_V3")
# data_product="globcolour_GSM_8Day"
# datapath="/Users/heatherwelch/Dropbox/JPSS"
# mclapply(dates,FUN=run_ecocast,data_product=data_product,datapath=datapath,mc.cores=4)

# #### globcolour_AVW 2015 - 2018; 8 day (running w new sla) ####
# dates=list.files("/Users/heatherwelch/Dropbox/JPSS/globcolour_AVW_8Day/Satellite_mask_resub_V3")
# data_product="globcolour_AVW_8Day"
# datapath="/Users/heatherwelch/Dropbox/JPSS"
# lapply(dates,FUN=run_ecocast,data_product=data_product,datapath=datapath)
# 
# #### oc_cci 2015 - 2018; 8 day (running w new sla) ####
# dates=list.files("/Users/heatherwelch/Dropbox/JPSS/oc_cci_8Day/Satellite_mask_resub_V3")
# data_product="oc_cci_8Day"
# datapath="/Users/heatherwelch/Dropbox/JPSS"
# mclapply(dates,FUN=run_ecocast,data_product=data_product,datapath=datapath)
# 
# 
# 
# 
# 
