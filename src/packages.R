#library humidity from https://cran.r-project.org/web/packages/humidity/humidity.pdf

packages <- c("R6", "logger", "lubridate","httr", "nasapower","data.table",
              "dplyr","stringi","humidity", "openxlsx")

sapply(packages, function(pkg){
  if(!(pkg %in% installed.packages())){
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
})
