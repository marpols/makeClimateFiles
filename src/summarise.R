#stn_name - (chr) name of weather station
#gs - (int vector sequence) range of months in growing season
#avg_dur - (int) number of years over which the historical average is calculated
#base - (int) base for GDD calculation. Default = 5C
summarise <- function(stn_name, gs, avg_dur, base = 5) {
  stn_dir <- file.path(dir, stn_name)
  
  clim_data <- read.csv(file.path(stn_dir, paste0(stn_name, "_complete.csv")))
  
  eccc_data <- read.csv(file.path(stn_dir, paste0(stn_name, "_ECCCclimatefile.csv")))
  
  nasa_data <- read.csv(file.path(stn_dir, paste0(stn_name, "_nasapower.csv")))
  
  
  #rows/dates with missing values (temp or precip)
  ecccm <- eccc_data[rowSums(is.na(eccc_data[, 1:7])) > 0, ]
  
  #values from nasapower used to replace missing values in ecccm
  nasar <- nasa_data[rowSums(is.na(eccc_data[, 1:7])) > 0, ]
  
  #rows/dates with missing values (all)
  ecccm_all <- eccc_data[rowSums(is.na(eccc_data)) > 0, ]
  
  #values from nasapower used to replace missing values in ecccm_all
  nasar <- nasa_data[rowSums(is.na(eccc_data)) > 0, ]
  
  
  #monthly averages/totals before gap filling with nasapower
  mavg_eccc <- eccc_data |>
    group_by(Year, Month) |>
    summarize(
      Min.Temp = mean(Min.Temp...C., na.rm = TRUE, ),
      Max.Temp = mean(Max.Temp...C., na.rm = TRUE, ),
      Precipitation = sum(Total.Precip..mm., na.rm = TRUE)
    )
  
  #monthly averages/totals after gap filling with NASA POWER
  mavg <- clim_data |>   group_by(Year, Month) |>
    summarize(
      Min.Temp = mean(Min.Temp...C., na.rm = TRUE, ),
      Max.Temp = mean(Max.Temp...C., na.rm = TRUE, ),
      Precipitation = sum(Total.Precip..mm., na.rm = TRUE)
    )
  
  #Write table flagging replaced data
  missing_summary <- list()
  cols <- c("Year", "Month", "Avg.Min.Temp - before", "Avg.Min.Temp - after",
            "Min.Temp. - difference",
           "Avg.Max.Temp - before", "Avg.Min.Temp - after",
           "Max.Temp. - difference",
           "Cum.Precip - before", "Cum.Precip - after",
           "Cum.Precip - difference")
  
  missing_summary[["Monthly Averages"]] <- setNames(cbind(mavg_eccc[,1:3],
                                                mavg[,3],
                                                mavg[,3] - mavg_eccc[,3],
                                                yes = mavg_eccc[,4], mavg[,4],
                                                mavg[,4] - mavg_eccc[,4],
                                                mavg_eccc[,5], mavg[,5],
                                                mavg[,5] - mavg_eccc[,5]),
                                                cols)
  eccc_replaced <- ecccm_all
  nasar <- nasar[, c(8,4,5,6, 9, 10, 11, 12, 10)]
  eccc_replaced[is.na(eccc_replaced)] <- nasar[is.na(ecccm_all)]
  
  missing_summary[["Filled Values"]] <- eccc_replaced
  
  #write to excel file
  write2excel_missing(stn_name,missing_summary,ecccm_all)
  
  #(after gap filling)
  #yearly total precip
  ptot <- clim_data |>  group_by(Year) |>
    summarize(Precipitation = sum(Total.Precip..mm., na.rm = TRUE))
  
  #growing season total precip
  ptot_gs <- clim_data[(clim_data$Month %in% gs), ] |>
    group_by(Year) |>
    summarize(Precipitation = sum(Total.Precip..mm., na.rm = TRUE))
  
  #GDD
  clim_data$GDD <- pmax(0,
                        (clim_data$Max.Temp...C. + clim_data$Min.Temp...C.) / 2 - base)
  
  #monthly cummulative GDD
  GDDmthly <- GDDcum <- clim_data |>
    group_by(Year, Month) |>
    summarize(GDD.cummulative = sum(GDD))
  
  #cummulative Yearly GDD from over gs
  GDDcum <- clim_data[(clim_data$Month %in% gs), ] |>
    group_by(Year) |>
    summarize(GDD.cummulative = sum(GDD))
  
  
  final_year <- clim_data$Year[nrow(clim_data)]
  dur <- avg_dur
  hyrs <- c((final_year - dur + 1):(final_year))
  
  #historical monthly averages
  histavg <- mavg[mavg$Year %in% hyrs, ] |>
    group_by(Month) |>
    summarize(
      Min.Temp = mean(Min.Temp, na.rm = TRUE, ),
      Max.Temp = mean(Max.Temp, na.rm = TRUE, ),
      Precipitation = mean(Precipitation, na.rm = TRUE)
    )
  
  #historical yearly average precipitation
  hprecip <- mean(ptot$Precipitation[ptot$Year %in% hyrs])
  
  #historical yearly average growing season precipitation
  hprecip_gs <- mean(ptot_gs$Precipitation[ptot_gs$Year %in% hyrs])
  
  #historical average GS GDD cummulative
  hgdd_cum <- mean(GDDcum$GDD.cummulative[GDDcum$Year %in% hyrs])
  
  #historical average daily GDD
  hgdd <- clim_data[clim_data$Year %in% hyrs, ] |>
    group_by(Month, Day) |>
    summarize(Avg_GDD = mean(GDD, na.rm = TRUE, ))
  hgdd$doy <- get_doy(hgdd$Month, hgdd$Day)
  
  
  #Make summary tables of values against historical average
  summaries <- list()
  
  values <- c("Min.Temp", "Max.Temp", "Precipitation")
  for(v in values){
    summaries[[{{v}}]] <- make_summary_table(dfs = list(mavg, histavg), 
                                             var = v, 
                                             years = hyrs)
  }
  
  summaries[["Yearly"]] <- make_summary_table(dfs = list(ptot, ptot_gs, GDDcum),
                                              years = hyrs,
                                              monthly = F,
                                              hist_vals = c(hprecip, 
                                                            hprecip_gs, 
                                                            hgdd_cum)
                                              )
  
  
  write.table(summary_table, file = file.path(stn_dir, "monthly_averages.csv"))
  write.table(yearly_summary, file = file.path(stn_dir, "yearly_totals.csv"))
  
}

get_doy <- function(month, day, year = 2020) {
  date <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
  as.integer(strftime(date, format = "%j"))
}

make_summary_table <- function(dfs, 
                               var = NULL, 
                               years, 
                               monthly = T,
                               hist_vals = NULL){
  
  if (monthly){
    df_avgs <- dfs[[1]]
    df_hist <- dfs[[2]]
    summary_table <- list(Month = df_hist$Month)
    for (y in years) {
      summary_table[[{{as.character(y)}}]] <-
        df_avgs[df_avgs$Year == y, names(df_avgs) == var]
    }
    summary_table[[{{sprintf("%d Year Avg.", length(years))}}]] <-
      df_hist[,names(df_hist) == var]
    
    rnms <- NULL
    
  } else {
    df_wy <- dfs[[1]]
    df_gs <- dfs[[2]]
    df_gdd <- dfs[[3]]
    summary_table <- list()
    for (y in years){
      summary_table[[{{as.character(y)}}]] <- c(
        df_wy$Precipitation[df_wy$Year == y],
        df_gs$Precipitation[df_gs$Year == y],
        df_gdd$GDD.cummulative[df_gdd$Year == y]
      )
    }
    summary_table[[{{sprintf("%d Year Avg.", length(years))}}]] <-
      hist_vals
    rnms <- c("precipitation (mm)",
              "GS precip. (mm)",
              sprintf("cummulative GS GDD (base %d)", base))
  }
  
  new_df <- setNames(do.call(cbind, 
                             lapply(summary_table, as.data.frame)),
                     names(summary_table))
  rownames(new_df) <- rnms
  
  new_df
}

write2excel_missing <- function(stn_name, table_list, eccc_org) {
  #write to excel file
  style_pink <- createStyle(fontColour = "#000000", fgFill = "pink")
  style_orange <- createStyle(fontColour = "#000000", fgFill = "orange")
  style_red <- createStyle(fontColour = "#000000", fgFill = "red")
  
  wb <- createWorkbook()
  
  sheets <- names(table_list)
  for (s in sheets) {
    addWorksheet(wb, s)
    
    if (s == "Monthly Averages") {
      s_df <- table_list$`Monthly Averages`
      writeData(wb, sheet = s, x = s_df)
      
      #Max and Min Temp
      for (c in c(5, 8)) {
        df <- s_df[,c]
        for (i in 1:nrow(df)) {
          if (abs(df[i,]) > 0 & abs(df[i,]) < 1)  {
            addStyle(
              wb,
              "Monthly Averages",
              style = style_pink,
              rows = i + 1,
              cols = c,
              gridExpand = TRUE
            )
          } else if (abs(df[i,]) > 1){
            addStyle(
              wb,
              "Monthly Averages",
              style = style_red,
              rows = i + 1,
              cols = c,
              gridExpand = TRUE
            )
          }
        }
      }
      #Precip
      df <- s_df[, 11]
      sd <- sd(s_df[,9][[1]])
      for (i in 1:nrow(df)) {
        if (abs(df[i,]) > sd * 3)  {
          addStyle(
            wb,
            "Monthly Averages",
            style = style_red,
            rows = i + 1,
            cols = 11,
            gridExpand = TRUE
          )
        } else if (abs(df[i,]) >= sd & abs(df[i,]) < sd * 2) {
          addStyle(
            wb,
            "Monthly Averages",
            style = style_orange,
            rows = i + 1,
            cols = 11,
            gridExpand = TRUE
          )
        } else if (abs(df[i,]) >= 10) {
          addStyle(
            wb,
            "Monthly Averages",
            style = style_pink,
            rows = i + 1,
            cols = 11,
            gridExpand = TRUE
          )
        }
      }
    } else if (s == "Filled Values"){ #flag values that have been replaced
      s_df <- table_list$`Filled Values`
      writeData(wb, sheet = s, x = s_df)
      
      na_locations <- which(is.na(eccc_org), arr.ind = TRUE)
      na_list <- split(na_locations, seq(nrow(na_locations)))
      
      sapply(na_list, function(x){
        addStyle(
          wb,
          s,
          style = style_pink,
          rows = x[1] + 1,
          cols = x[2],
          gridExpand = TRUE
        )
      })
    }
  }
  saveWorkbook(wb, 
               file.path(dir, stn_name, 
                         sprintf("%s_gap_filling_summary.xlsx", stn_name)), 
               overwrite = TRUE)
}

#TODO
#write2excel for summaries (historical average comparisons)
#code to plot somethings 
  # (e.g. cummulative GDD vs cummulative GDD historical average, same for precip.)
