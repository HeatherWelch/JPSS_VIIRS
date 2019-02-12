#### code to turn daily MODIS and VIIRS into 8 day composites to match the blended products

modisDir="/Users/heatherwelch/Dropbox/JPSS/modis_8Day/Satellite"
viirsDir="/Users/heatherwelch/Dropbox/JPSS/viirs_8Day/Satellite"
pmlEsaDir="/Users/heatherwelch/Dropbox/JPSS/pmlEsa_8Day/Satellite"
#modisCompDir="/Users/heatherwelch/Dropbox/JPSS/modis_8DayBin"#;dir.create(modisCompDir)
modisCompDir="/Users/heatherwelch/Dropbox/JPSS/modis_8DayBin/Satellite"#;dir.create(modisCompDir)

dates=list.files(pmlEsaDir)

eight_day=function(get_date){
  get_date=as.Date(get_date)
  SEQNCE=unlist(list(as.character(get_date-1),as.character(get_date-2),as.character(get_date-3),as.character(get_date-4),as.character(get_date),as.character(get_date+1),as.character(get_date+2),as.character(get_date+3)))
  
}





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