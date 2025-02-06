#converts ECCC hourly data into daily average values and combines with daily ECCC data#
#Mariaelisa Polsinelli for AAFC#

library("data.table")
library("dplyr")

createClimFile <- function(stn,years,fnm){
  #Formats .csv file
  #creates complete climate file for each weather station
  #stn - station name (char)
  #years - range of years (list of doubles)
  #fnm - file name (char)
  
  wd <- file.path(dir,stn)
  setwd(wd)
  
  first <- T
  
  for (y in years){
    cur_dlyyr <- read.csv(file = file.path(wd,sprintf("%d/%s_%d_ECCC.csv",y,stn,y)),
                          header=TRUE, sep=",") %>% select(5,6,7,8,10,12,24)
    if (first){
      first <- F
      write.csv(cur_dlyyr,file = fnm, row.names=F)
    }else{
      write.table(cur_dlyyr,file= fnm, append = T, col.names = F, row.names = F, sep=",")
    }
  }
}

get_avgs <- function(hrly,col){
    #calculate daily average for given column number
    #hrly - table of current month of hourly data (list)
    #col - column number in which averages will be calculated (int)
  
    if (col == 20){
    #average windspeed in km/h
    avgs <- hrly |> group_by(Day) |> summarize(Wind.Speed = mean(Wind.Spd..km.h., na.rm = TRUE))
    #convert to m/s
    avgs$Wind.Speed <- avgs$Wind.Speed/3.6
    }else{
    #% relative humidty
    avgs <- hrly |> group_by(Day) |> summarize(Relative.Humidity = mean(Rel.Hum...., na.rm = TRUE))  
    } 
    avgs$Month <- hrly$Month[1]
    avgs$Year <- hrly$Year[1]
    return(avgs[,c(4,3,1,2)])
}

calcAVGS <- function(stn,yrs,colnum){
  #parse through hourly data files and organise calculated averages
  #stn - weather station name (char)
  #yrs - year range (list of doubles)
  #colnum - column number in which averages will be calculated (double)
  
  wd <- file.path(dir,stn)

  data <- c()
  
  i <- 1
  for(y in yrs){
    #year directory
    fstem <- file.path(wd,sprintf("%d/hourly_%d_",y,y))
    while(i <= 12){
      #parse through each month of hourly data and append averages to data
      cur_hrly <- read.csv(paste(fstem,sprintf("%02d", i),".csv",sep=""))
      data <- rbind(data,get_avgs(cur_hrly,colnum))
      i <- i + 1
    }
    i <- 1
  }
  return(data[,4]) #return last column in dataframe only
}

make_climFile <- function(){
  for (obj in station_list){
    #create climate file for each station
    fname <- paste(obj$name,"_ECCCclimatefile.csv",sep="")
    
    createClimFile(obj$name,obj$firstyear:obj$lastyear,fname)
    climFile <- read.csv(fname)
    
    for (v in values){
      #generate daily averages for each selected hourly parameter
      climFile <- cbind(climFile, 
                        calcAVGS(obj$name,
                                 obj$firstyear:obj$lastyear,
                                 as.integer(v[2])))
    }
    write.csv(climFile,file = fname, col.names = T, row.names = F)
    message("Successfully created climate file for: ", 
            obj$name, " as ", paste(fstem,sprintf("%02d", i),".csv",sep=""))
  }
}



  
    
    
    