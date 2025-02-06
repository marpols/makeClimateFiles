#Replace missing data from ECCC with NasaPower Values (and add Solar Radiation
#data from NasaPower)
#Created by Mariaelisa Polsinelli, 2022

read_climateFile <- function(stn){
  #read climateFile for weather station
  weather_dat <- read.csv(file.path(dir,sprintf("%s/%s_ECCCclimatefile.csv", stn, stn)))
  return(weather_dat)
}

read_NP_file <- function(stn) {
  #read corresponding NasaPower file for weather station
  NP_data <- read.csv(file.path(dir,sprintf("%s/%s_nasapower.csv", stn, stn)))
  return(NP_data)
}

get_missing <- function(climfile,NPfile,climcols,NPcols){
#find and replace missing values
 i <- 1
 while (i <= length(climcols)){
    c <- climcols[i]
    d <- NPcols[i]
    missing <- which(is.na(climfile[c]))
    for (j in missing){
      climfile[j,c] <- NPfile[j,d]
    }
    i <- i + 1
 }
 return(climfile)
}

fill_gaps <- function(){
  for(obj in station_list){
    #run for each station in station list
    cfile <- read_climateFile(obj$name)
    NPfile <- read_NP_file(obj$name)
    
    cfile$Solar.Radiation <- NPfile$ALLSKY_SFC_SW_DWN #add solar radiation data
    
    write.csv(get_missing(cfile, NPfile, c(5,6,7,8,9),c(9,10,11,12,13)), 
              file = file.path(dir,sprintf("%s/%s_complete.csv", 
                                           obj$name, obj$name)), 
              row.names = F)
    message("Successfully replaced missing data for : ", obj$name)
  }
}


