#download ECCC historical climate daily and hourly data files

# Load necessary packages
if (!require("R6")){
  install.packages("R6")
}
if (!require("logger")){
  install.packages("logger")
}

library(R6)
library(lubridate)
library(httr)
library(logger)


# Define the Station class
Station <- R6Class("Station",
                   public = list(
                     name = NULL,
                     id = NULL,
                     start = NULL,
                     end = NULL,
                     firstyear = NULL,
                     lastyear = NULL,
                     long = NULL,
                     lat = NULL,
                     
                     initialize = function(name, id, start, end) {
                       self$name <- name
                       self$id <- id
                       self$start <- start
                       self$end <- end
                       self$firstyear <- year(ymd(start))
                       self$lastyear <- year(ymd(end))
                     }
                   ),
                   lock_objects = FALSE
)

# Get the current working directory
root <- dir

download_climate <- function(){
  # Loop through each station
  for (obj in station_list) {
    
    station_name <- obj$name
    path <- file.path(root, obj$name)
    
    # Create the station folder if it doesn't exist
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      message("Successfully created directory: ", path)
    } else {
      message("Directory already exists: ", path)
    }
    
    # Move into station directory
    setwd(path)
    
    # Define months and days for leap and non-leap years
    months <- sprintf("%02d", 1:12)  # Format months as "01", "02", ..., "12"
    
    for (yr in obj$firstyear:obj$lastyear) {
      
      # Determine the number of days in February based on leap year status
      if (leap_year(yr)) {
        days <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      } else {
        days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      }
      
      # Create a folder for the year
      year_path <- file.path(path, as.character(yr))
      if (!dir.exists(year_path)) {
        dir.create(year_path, recursive = TRUE)
        message("Successfully created directory: ", year_path)
      } else {
        message("Directory already exists: ", year_path)
      }
      
      # Move into year folder
      setwd(year_path)
      
      # Download the daily climate data file
      daily_url <- sprintf(
        "http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=%s&Year=%d&Month=12&Day=1&timeframe=2&submit=Download+Data",
        obj$id, yr
      )
      
      daily_file <- sprintf("%s_%d_ECCC.csv", obj$name, yr)
      message("Downloading ", daily_file)
      download.file(daily_url, destfile = daily_file, quiet = TRUE, mode = "wb")
      
      for (i in seq_along(months)) {
        hourly_url <- sprintf(
          "http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=%s&Year=%d&Month=%s&Day=%d&timeframe=1&submit=Download+Data",
          obj$id, yr, months[i], days[i]
        )
        
        hourly_file <- sprintf("hourly_%d_%s.csv", yr, months[i])
        
        message("Downloading ", hourly_file)
        download.file(hourly_url, destfile = hourly_file, quiet = TRUE, mode = "wb")
      }
      # Move back to station folder
      setwd(path)
    }
    #get lat and long from ECCC file
    cfile <- read.csv(file.path(path,
                     list.files(path, recursive = T)[1])
             )
    obj$long <- cfile$Longitude..x.[1]
    obj$lat <- cfile$Latitude..y.[1]
    # Return to root directory
    setwd(root)
  }
  print(root)
}


    