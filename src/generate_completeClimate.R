#converts ECCC hourly data into daily average values and combines with daily ECCC data#
#Mariaelisa Polsinelli for AAFC#

createClimFile <- function(station, fnm){
  #Formats .csv file
  #creates complete climate file for each weather station
  #station - station object
  
  first <- T
  
  stn <- station$name
  years <- seq(station$firstyear, station$lastyear)
  path <- file.path(dir,sprintf("%s/%s", stn, fnm))
    
  for (y in years){
    cur_dlyyr <- read.csv(file = file.path(dir,sprintf("%s/%d/%s_%d_ECCC.csv",
                                                       stn,y,stn,y)),
                          header=TRUE, sep=",") |> select(5,6,7,8,10,12,24)
    if (first){
      first <- F
      write.csv(cur_dlyyr,file = path, row.names=F)
    }else{
      write.table(cur_dlyyr,file= path, append = T,
                  col.names = F, row.names = F, sep=",")
    }
  }
}

get_avgs <- function(hrly,col){
    #calculate daily average for given column number
    #hrly - table of current month of hourly data (list)
    #col - column number in which averages will be calculated (int)
  
    if (col == 20){
    #average wind speed in km/h
    avgs <- hrly |> group_by(Day) |> 
      summarize(Wind.Speed = mean(Wind.Spd..km.h., na.rm = TRUE))
    #convert to m/s
    avgs$Wind.Speed <- avgs$Wind.Speed/3.6
    }else{
    #% relative humidity
    avgs <- hrly |> group_by(Day) |> 
      summarize(Relative.Humidity = mean(Rel.Hum...., na.rm = TRUE))  
    } 
    avgs$Month <- hrly$Month[1]
    avgs$Year <- hrly$Year[1]
    return(avgs[,c(4,3,1,2)])
}

calcAVGS <- function(station,colnum){
  #parse through hourly data files and organise calculated averages
  #station - station object
  #colnum - column number in which averages will be calculated (double)
  
  stn <- station$name
  years <- seq(station$firstyear, station$lastyear)

  data <- c()
  
  i <- 1
  for(y in years){
    #year directory
    fstem <- file.path(dir,sprintf("%s/%d/hourly_%d_",stn,y,y))
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
    
    createClimFile(obj,fname)
    climFile <- read.csv(file.path(dir,obj$name,fname))
    
    for (v in values){
      #generate daily averages for each selected hourly parameter
      climFile <- cbind(climFile, 
                        calcAVGS(obj,
                                 as.integer(v[2])))
    }
    write.csv(climFile,file = file.path(dir, obj$name, fname), 
              col.names = T, row.names = F)
    message("Successfully created climate file for: ", 
            obj$name, " as ", fname)
  }
}



  
    
    
    