#Download historical climate data from ECCC, 
#replace missing values with NASA Power simulated climate
#create STICS climate files

#Sources------------------------------------------------------------------------
source("src/download_ECCC.R")
source("src/generate_completeClimate.R")
source("src/download_nasapowerfiles.R")
source("src/fill_MD.R")
source("src/genSTICSclimate.R")


#---------------------------------Settings--------------------------------------
#working directory for climate files
dir <- "C:/Users/PolsinelliM/OneDrive - AGR-AGR/Documents/MntGem STICS"

#Stations--------------------------------
#list of weather station names to extract data from
#select date range of data to download (start and end date as "yyyy-mm-dd")
#use as.character(Sys.Date()) for current date
#add new station as stations$new("county name", "id", "start date", "end date")

# Create a list of stations
station_list <- list(
  # Station$new("Harrington", "30308", "2004-01-01", "2024-12-31"),
  # Station$new("Summerside", "10800", "1994-01-01", "2024-12-31")
  Station$new("Ste-Anne-de-Bellvue", "10873", "2022-01-01", "2024-12-31")
)

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
time_period <- "daily"

#Generate STICS files
STICS_files <- T


#Function Calls-----------------------------------------------------------------

setwd(dir)

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
