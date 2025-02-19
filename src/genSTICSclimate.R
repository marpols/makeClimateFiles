#write a climate file
#takes a dataframe or list  to be written and the path/name.year as a string
#write_STICS_file func. from climate app created by Kristen Murchison for AAFC

library("stringi")
library("humidity") #https://cran.r-project.org/web/packages/humidity/humidity.pdf
library("lubridate")

write_STICS_file <- function(df, ptnmyr){
  #create STICS climate file format
  #df - dataframe containing climate data - columns: 
  #ptnmyr - file name (e.g. Station_name.year)
  for(i in 2:13){
    tryCatch(
      df[i] <- as.numeric(df[[i]]),
      error = function(e){df[[i]] <- as.numeric(df[[i]])}
    )
  }
  cc <<- 0
  #pf <- list('%s', '%5.0f', '%3.0f', '%3.0f', '%4.0f', '%8.1f', '%7.1f', '%7.1f', 
   #          '%7.1f', '%7.1f', '%7.1f', '%7.1f', '%7.1f')
  pf <- list('%s', '%1.0f', '%1.0f', '%1.0f', '%1.0f', '%1.1f', '%1.1f', '%1.1f',
             '%1.1f', '%1.1f', '%1.1f', '%1.1f', '%1.1f')
  CF <- sapply(pf, function(x){
    cc <<- cc + 1
    sprintf(x, df[[cc]])})
  
  write.table(CF, ptnmyr,
              quote = FALSE, col.names = FALSE, row.names = FALSE)
}

make_STICS <- function(){
  for (obj in station_list){
    files <- read.csv(file.path(dir,sprintf("%s/%s_complete.csv", 
                                            obj$name, obj$name)))
    
    files$Station <- obj$name
    files$DOY <- yday(ISOdate(files[,2],files[,3],files[,4])) #calculate DOY
    files$Average.Temp <- (files[,5]+files[,6])/2 #average daily temperature
    files$Vapour.Pressure <- NA
    files$PET <- -999.9
    files$CO2 <- 330.0
    
    n <- 1
    while (n <= nrow(files)){
      #calculate water vapour presuure from relative humidity and average temp.
      #calculate saturated water vapour pressure
      Es <- SVP(files$Average.Temp[n], isK = FALSE, formula = c("Clausius-Clapeyron"))
      #calculate vp in millibars (Pascals/100)
      files$Vapour.Pressure[n] <- WVP2(files$Relative.Humidity[n], Es)/100
      n <- n + 1
    }
    
    climyrs <- files[,c(11,2,3,4,12,6,5,10,15,7,9,14,16)] |> group_by(Year) |> group_split()
    for (c in climyrs){
      fname <- file.path(dir,sprintf("%s/%s.%d", 
                                     obj$name, obj$name, c$Year[1]))
      write_STICS_file(c, fname)
      message("Successfully created STICS climate file for : ", obj$name, " ", c$Year[1])
    }
  }
}