#-----------------------------
#Pull nasapower data from multiple coordinates/weather stations and save as .csv
#Mariaelisa Polsinelli for OMAFRA and AAFC
#May 2022
#-----------------------------


download_nasapower <- function(coordinates, pars_list, date_list,temporal_type ){
  #calls get_power for one coordinate set
  output_file <- get_power(community = "ag",lonlat = coordinates, pars = pars_list, dates = date_list, temporal_api = temporal_type)
  return(output_file)
}

get_NP <- function(){
  i <- 1
  while (i <= length(station_list)){
    stn_name <- station_list[[i]]$name
    filename <- file.path(dir, sprintf("%s/%s_nasapower.csv",
                                       stn_name, stn_name))
    if(filename %in% list.files(file.path(dir,stn_name), full.names = T)){
      message(sprintf("NASA POWER file for %s already exists", stn_name))
    } else {
    power_file <-  download_nasapower(c(as.numeric(station_list[[i]]$long),
                                        as.numeric(station_list[[i]]$lat)), 
                                      climate_data, 
                                      c(station_list[[i]]$start,
                                        station_list[[i]]$end), 
                                      time_period)
    
    write.csv(power_file, file = filename )
    message("Successfully downloaded climate file from NASA POWER: ",
            stn_name, " as ", 
            paste(stn_name, "nasapower",".csv",sep="_"))
    }
    i <- i + 1
  }
}



