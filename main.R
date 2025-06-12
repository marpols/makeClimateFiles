#Download historical climate data from ECCC, 
#replace missing values with NASA Power simulated climate
#create STICS climate files

#Sources------------------------------------------------------------------------
srcs <- c("src/packages.R","src/download_ECCC.R","src/generate_completeClimate.R",
          "src/download_nasapowerfiles.R","src/fill_MD.R",
          "src/genSTICSclimate.R")
invisible(sapply(srcs, source))

#---------------------------------Settings--------------------------------------
#output directory
dir <- rstudioapi::selectDirectory()
#or from path
dir <- "outputs"

#Stations--------------------------------
#list of weather station names to extract data from
#select date range of data to download (start and end date as "yyyy-mm-dd")
#use as.character(Sys.Date()) for current date
#add new station as stations$new("county name", "id", "start date", "end date")

# Create a list of stations
station_list <- list(
  Station$new("Harrington", "30308", "2004-01-01", "2024-12-31"),
  Station$new("Summerside", "10800", "2004-01-01", "2024-12-31"),
  Station$new("New Glasgow", "6537", "2004-01-01", "2024-12-31"),
  Station$new("East Point (AUT)", "7177", "2004-01-01", "2024-12-31"),
  Station$new("North Cape", "10814", "2004-01-01", "2024-12-31")
)

names <- sapply(station_list, function(x) x$name)
station_list <- setNames(station_list,names)

#ECCC Hourly Data------------------------
#Columns of data from weather data table to calculate averages for
#14 - RH, 20 - wind speed
values <- list(c('RH',14),
               c("Wind Speed",20))

#NASA POWER------------------------------
#parameters - selected weather data types for NasaPower
#all data types and their abbreviations can be found here https://power.larc.nasa.gov/#resources
#"T2M_MAX" - max temperature (C)
#"T2M_MIN" - min temperature (C)
#"PRECTOTCORR" - precipitation (mm)
# RH2M - MERRA-2 Relative Humidity at 2 Meters (%) 
# WS10M - MERRA-2 Wind Speed at 10 Meters (m/s)
#"ALLSKY_SFC_SW_DWN" - all sky surface shortwave downward irradiance (MJ/m^2/day)

climate_data <- c("T2M_MAX",
                  "T2M_MIN",
                  "PRECTOTCORR",
                  "RH2M", 
                  "WS10M",
                  "ALLSKY_SFC_SW_DWN")

#choose "hourly", "daily" or "monthly" 
#(daily is needed to combine with ECCC file and generate STICS climate files)
time_period <- "daily"

#Generate STICS files
STICS_files <- T


#Function Calls-----------------------------------------------------------------

#download ECCC climate data
download_climate()

#Generate Climate File from ECCC data
make_climFile()

#download files from NASA Power
get_NP()

#Fill in Missing Data from NASA POWER
fill_gaps()

#Generate STICS files
if (STICS_files){
make_STICS()
}
