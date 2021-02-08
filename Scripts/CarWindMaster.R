WindCollection = function(anem_data,finalFilename,finalFolder) {
  anem_data <- read.csv(anem_data, header = FALSE)
  library("zoo")
  library("signal")
  library(tidyr)
  library(dplyr)
  library(readr)
  library(forecast)
  library(zoo)
  
  # Open data files
  {
    lat_anem <- 40.545957
    long_anem <- -105.0359567
    
    
    rollavg <- 20 #rollmean setting
    
    
    
  }
  
  #Anem Data Collection
  {
    ####
    ######
    ###SUPER UGLY DATA COLLECTION########
    ######
    
    #Components are where the wind is going, ie +NS value means wind is travelling north. Direction is where wind is coming from.
    
    NS <- (as.numeric(anem_data$V19))
    EW <- (as.numeric(anem_data$V20))
    Z <- (as.numeric(anem_data$V21)) #+z is up
    mag <- round((EW^2 + NS^2)^(1/2),2)
    wind_dir <- round((atan2((EW),(NS)))*180/pi + 180,2)
    
    
    

    
    #Timestamps for the range
    Timestamp <- anem_data$V1 #Timestamp for the array
    options(digits=11)
    UnixTime <- (as.numeric(as.POSIXct(Timestamp, format="%Y-%m-%d %H:%M:%OS")) - 3600) #correcting for anem clock not falling back for daylight savings
    Timestamp <- format(as.POSIXct(round(UnixTime,1), origin="1970-01-01"),"%Y-%m-%d %H:%M:%OS3")
    
    anem_collected <-  data.frame(Timestamp,UnixTime,EW,NS,Z,mag,wind_dir)
    anem_collected <- anem_collected[100:nrow(anem_collected),1:ncol(anem_collected)] #Shaving off beginning of data
    
    ######
    ###END SUPER UGLY DATA COLLECTION########
    ######
  }
  
  #cardata <- data.frame(read.csv(cardata))
  #cardata <- data.frame(read.csv(cardata))
  cardata <- read.csv(paste(finalFolder,'\\',finalFilename,sep=''))
  cardata <- select(cardata, 1:2,4:9,20:23,26:30)
  cardata <- cardata %>%
    arrange((nearest10hz))
  library("geosphere")
  time1car <- min(cardata["nearest10hz"]) #Begin time
  time2car <- max(cardata["nearest10hz"]) #Find end time
  
  
  #Index where the stationary array time is closest to time 1 of car, with a "buffer" of data below
  arrayindex1 <- which.min(abs(time1car-anem_collected$UnixTime)) - 20
  
  #Index where the stationary array time is closest to time 2 of car, with a "buffer" of data above 
  arrayindex2 <- which.min(abs(time2car-anem_collected$UnixTime)) + 20
  
  indexrange <- arrayindex1:arrayindex2
  
  cardata["Windmaster EW"] <- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"EW"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
  cardata["Windmaster NS"] <- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"NS"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
  cardata["Windmaster Mag."] <- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"mag"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
  cardata["Windmaster Angle"] <- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"wind_dir"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
  cardata["RealTime"] <- format(as.POSIXct(round(cardata[1:nrow(cardata),"nearest10hz"],1), origin="1970-01-01"),"%Y-%m-%d %H:%M:%OS3")
  cardata <- data.frame(cardata[c(1:2, 22, 3:21)])
  return(cardata)
  
}