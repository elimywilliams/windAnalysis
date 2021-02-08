WindCollection = function(anem_data,finalFilename,finalFolder,lat_anem,long_anem) {
  anem_data <- read.csv(anem_data, header = FALSE)
  anem_data <- read_csv("data/Stationary_210202_122942.csv", 
                        skip = 1)
  anem_data <- read_csv("data/array0204.dat", 
                        skip = 1)
  
  library(zoo)
  library(signal)
  library(tidyr)
  library(dplyr)
  library(readr)
  library(forecast)
  library(zoo)
  
  # Open data files
  {
    #lat_anem <- 40.545957
    #long_anem <- -105.0359567
    
    
    rollavg <- 20 #rollmean setting
    
    
    
  }
  
  #Anem Data Collection
  {
    ####
    ######
    ###SUPER UGLY DATA COLLECTION########
    ######
    
    #Components are where the wind is going, ie +NS value means wind is travelling north. Direction is where wind is coming from.
    
    EW1 <- -1*(as.numeric(anem_data$V4))
    EW1 <- -1*(as.numeric(anem_data$`Uy(1)`))
    #NS1 <- -1*(as.numeric(anem_data$)
    
    NS1 <- (as.numeric(anem_data$V5))
    Z1 <- (-1*as.numeric(anem_data$V3)) #+z is up
    mag1 <- round((EW1^2 + NS1^2)^(1/2),2)
    wind_dir1 <- round((atan2((EW1),(NS1)))*180/pi + 180,2)
    
    
    
    EW2 <- -1*(as.numeric(anem_data$V8))
    NS2 <- (as.numeric(anem_data$V9))
    Z2 <- (-1*as.numeric(anem_data$V7))
    mag2 <- round((EW2^2 + NS2^2)^(1/2),2)
    wind_dir2 <- round((atan2((EW2),(NS2)))*180/pi + 180,2)
    
    
    EW3 <- -1*(as.numeric(anem_data$V12))
    NS3 <- (as.numeric(anem_data$V13))
    Z3 <- (-1*as.numeric(anem_data$V11))
    mag3 <- round((EW3^2 + NS3^2)^(1/2),2)
    wind_dir3 <- round((atan2((EW3),(NS3)))*180/pi + 180,2)
    
    
    EW4 <- -1*(as.numeric(anem_data$V16))
    NS4 <- (as.numeric(anem_data$V17))
    Z4 <- (-1*as.numeric(anem_data$V15))
    mag4 <- round((EW4^2 + NS4^2)^(1/2),2)
    wind_dir4 <- round((atan2((EW4),(NS4)))*180/pi + 180,2)
    
    
    EW5 <- -1*(as.numeric(anem_data$V20))
    NS5 <- (as.numeric(anem_data$V21))
    Z5 <- (-1*as.numeric(anem_data$V19))
    mag5 <- round((EW5^2 + NS5^2)^(1/2),2)
    wind_dir5 <- round((atan2((EW5),(NS5)))*180/pi + 180,2)
  }
  {
    
    #Timestamps for the range
    #Timestamp <- anem_data$V1 #Timestamp for the array
    #options(digits=11)
    
    UnixTime_calc <- (as.numeric(as.POSIXct(anem_data$TIMESTAMP, format="%Y-%m-%d %H:%M:%OS")) - 3600) #correcting for anem clock not falling back for daylight savings
    Timestamp_calc <- format(as.POSIXct(round(UnixTime_calc,1), origin="1970-01-01"),"%Y-%m-%d %H:%M:%OS3")
    
    #anem_collected <-  data.frame(Timestamp,UnixTime,EW1,NS1,Z1,mag1,wind_dir1,EW2,NS2,Z2,mag2,wind_dir2,EW3,NS3,Z3,mag3,wind_dir3,EW4,NS4,Z4,mag4,wind_dir4,EW5,NS5,Z5,mag5,wind_dir5)
    
    anem_collected <- anem_data %>% 
      mutate(Timestamp = Timestamp_calc,
             UnixTime = UnixTime_calc) %>% 
            rename(Z1 = `Ux(1)`,
                   Z2 = `Ux(2)`,
                   Z3 = `Ux(3)`,
                   Z4 = `Ux(4)`,
                   Z5 = `Ux(5)`,
                   EW1 = `Uy(1)`,
                   EW2 = `Uy(2)`,
                   EW3 = `Uy(3)`,
                   EW4 = `Uy(4)`,
                   EW5 = `Uy(5)`,
                   NS1 = `Uz(1)`,
                   NS2 = `Uz(2)`,
                   NS3 = `Uz(3)`,
                   NS4 = `Uz(4)`,
                   NS5 = `Uz(5)`) %>% 
      dplyr::select(-contains('Ts'),-TIMESTAMP,-RECORD)
                   
    anem_collected <- anem_collected[100:nrow(anem_collected),1:ncol(anem_collected)] #Shaving off beginning of data
    
    ######
    ###END SUPER UGLY DATA COLLECTION########
    ######
  }
  
  #cardata <- data.frame(read.csv(cardata))
  cardata <- read.csv(finalFolder)
  #cardata <- select(cardata, 1:2,4:9,20:23,26:30)
  cardata <- cardata %>%
    arrange((nearest10hz))
  library("geosphere")
  time1car <- min(cardata["nearest10hz"]) #Begin time
  time2car <- max(cardata["nearest10hz"]) #Find end time
  
  
  buffer <- 0
  
  #Index where the stationary array time is closest to time 1 of car, with a "buffer" of data below
  arrayindex1 <- which.min(abs(time1car-anem_collected$UnixTime)) - buffer
  
  #Index where the stationary array time is closest to time 2 of car, with a "buffer" of data above 
  arrayindex2 <- which.min(abs(time2car-anem_collected$UnixTime)) + buffer
  
  indexrange <- arrayindex1:arrayindex2
  
  small_collected <- (anem_collected[indexrange,])
  
  together <- plyr::join(small_collected %>% rename(nearest10hz = UnixTime),cardata,type='full')
  
#   {
#   cardata['Array_EW1'] <- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"EW1"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_NS1'] <- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"NS1"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_Z1'] <- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"Z1"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_EW2']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"EW2"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_NS2']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"NS2"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_Z2']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"Z2"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_EW3']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"EW3"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_NS3']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"NS3"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_Z3']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"Z3"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_EW4']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"EW4"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_NS4']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"NS4"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_Z4']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"Z4"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_EW5']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"EW5"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_NS5']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"NS5"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata['Array_Z5']<- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"Z5"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
# }
#   
#   
#   cardata["Array Station 4 EW"] <- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"EW4"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata["Array Station 4 NS"] <- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"NS4"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata["Array Station 4 Mag."] <- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"mag4"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata["Array Station 4 Angle"] <- interp1(anem_collected[indexrange,"UnixTime"],anem_collected[indexrange,"wind_dir4"],cardata[1:nrow(cardata),"nearest10hz"],extrap=TRUE)
#   cardata["RealTime"] <- format(as.POSIXct(round(cardata[1:nrow(cardata),"nearest10hz"],1), origin="1970-01-01"),"%Y-%m-%d %H:%M:%OS3")
#   cardata <- data.frame(cardata[c(1:2, 22, 3:21)])
#   return(cardata)
#   return(together)
  
}