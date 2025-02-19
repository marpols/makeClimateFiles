stn_name <- "Harrington"
stn_dir <- file.path(dir,stn_name)
base <- 5

clim_data <- read.csv(file.path(stn_dir,
                                paste0(stn_name,"_complete.csv")))

eccc_data <- read.csv(file.path(stn_dir,
                                paste0(stn_name,"_ECCCclimatefile.csv")))

nasa_data <- read.csv(file.path(stn_dir,
                                paste0(stn_name,"_nasapower_.csv")))


#rows/dates with missing values (temp or precip)
ecccm <- eccc_data[rowSums(is.na(eccc_data[,1:7])) > 0,] 

#values from nasapower used to replace missing values in ecccm
nasar <- nasa_data[rowSums(is.na(eccc_data[,1:7])) > 0,] 

#rows/dates with missing values (all)
ecccm_all <- eccc_data[rowSums(is.na(eccc_data)) > 0,]

#values from nasapower used to replace missing values in ecccm_all
nasar <- nasa_data[rowSums(is.na(eccc_data)) > 0,] 


#monthly averages/totals before gap filling with nasapower
mavg_eccc <- eccc_data |> 
  group_by(Year,Month) |> 
  summarize(Min.Temp = mean(Min.Temp...C., na.rm = TRUE,),
                                    Max.Temp = mean(Max.Temp...C., na.rm = TRUE,),
                                    Precipitation = sum(Total.Precip..mm., na.rm = TRUE))

#monthly averages/totals after gap filling with nasapower
mavg <- clim_data |>   group_by(Year,Month) |> 
  summarize(Min.Temp = mean(Min.Temp...C., na.rm = TRUE,),
            Max.Temp = mean(Max.Temp...C., na.rm = TRUE,),
            Precipitation = sum(Total.Precip..mm., na.rm = TRUE))

#(after gap filling)
#yearly total precip
ptot <- clim_data |>  group_by(Year) |> 
  summarize(Precipitation = sum(Total.Precip..mm., na.rm = TRUE))

#growing season total precip
ptot_gs <- clim_data[(clim_data$Month %in% c(5:10)),] |> 
  group_by(Year) |> 
  summarize(Precipitation = sum(Total.Precip..mm., na.rm = TRUE))

#GDD
clim_data$GDD <- pmax(0,(clim_data$Max.Temp...C.+clim_data$Min.Temp...C.)/2 - base)

#cummulative Yearly GDD from over january to october
GDDcum <- clim_data[(clim_data$Month %in% c(1:10)),] |> 
  group_by(Year) |> 
  summarize(GDD.cummulative = sum(GDD))



final_year <- clim_data$Year[nrow(clim_data)]                          
dur <- 20
hyrs <- c((final_year - dur):(final_year - 1))

#historical monthly averages
histavg <- mavg[mavg$Year %in% hyrs,] |>
  group_by(Month) |> 
  summarize(Min.Temp = mean(Min.Temp, na.rm = TRUE,),
            Max.Temp = mean(Max.Temp, na.rm = TRUE,),
            Precipitation = mean(Precipitation, na.rm = TRUE))

#historical yearly average precipitation
hprecip <- mean(ptot$Precipitation[ptot$Year %in% hyrs])

#historical yearly average growing season precipitation
hprecip_gs <- mean(ptot_gs$Precipitation[ptot_gs$Year %in% hyrs])

#historical average jan-oct GDD cummulative
hgdd <- mean(GDDcum$GDD.cummulative[GDDcum$Year %in% hyrs])

summary_table <- 
  data.frame(Month = histavg$Month,
             "2024 Min. Temp" = mavg$Min.Temp[mavg$Year == final_year],
             "20 Year Avg. Min" = histavg$Min.Temp,
             "2024 Max. Temp" = mavg$Max.Temp[mavg$Year == final_year],
             "20 Year Avg. Max" = histavg$Max.Temp,
             "2024 Precipitation" = mavg$Precipitation[mavg$Year == final_year],
             "20 Year Avg. Precip." = histavg$Precipitation,
             check.names = F)

yearly_summary <- 
  data.frame("2024" = c(ptot$Precipitation[ptot$Year == final_year],
                        ptot_gs$Precipitation[ptot_gs$Year == final_year],
                        GDDcum$GDD.cummulative[GDDcum$Year == final_year]),
             "20 Year Avg." = c(hprecip, hprecip_gs, hgdd),
             check.names = F)
row.names(yearly_summary) <- c("precipitation (mm)", 
                               "growing season precip. (mm)", 
                               "cummulative GDD (base 5)")


write.table(summary_table, file = file.path(stn_dir,"monthly_averages.csv"))
write.table(yearly_summary, file = file.path(stn_dir,"yearly_totals.csv"))

#plot daily values against 20 year average

#daily 20 year temperature averages
dhavg <- clim_data[clim_data$Year != final_year,] |>   group_by(Month,Day) |> 
  summarize(Min.Temp = mean(Min.Temp...C., na.rm = TRUE,),
            Max.Temp = mean(Max.Temp...C., na.rm = TRUE,))
            
ptmin <- ggplot()
             

