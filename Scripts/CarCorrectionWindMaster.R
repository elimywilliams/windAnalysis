CarCorrection = function(indata,fileFolder, VesselHeadingName) {
  indata <- indata
  Car_time <- data.frame(indata['nearest10hz'])
  outdata <- data.frame(Car_time)
  library("geosphere")
  library(dplyr)
  library(readr)
  library(tidyr)
  library(forecast)
  library(zoo)
  library("signal")
  
  
  lat_anem <-  40.5949267
  long_anem <- -105.13984
  
  outdata["Latitude"] <- indata['Latitude']
  outdata["Longitude"] <- indata['Longitude']
  
  #Car Data Collection 
  {
    ####Finding Car Speed and Heading
    pos1_range <- 1:(nrow(indata)-1)
    pos2_range <- 2:nrow(indata)
    range_length <- length(pos1_range)

    car_matrix1 <- matrix(ncol = 2, nrow = range_length) #First points
    car_matrix2 <- matrix(ncol = 2, nrow = range_length) #second points
    colnames(car_matrix1) <- c("Long1","Lat1")
    colnames(car_matrix2) <- c("Long2","Lat2")
    
    
    car_matrix1[1:range_length, "Long1"] <- outdata[pos1_range,"Longitude"]
    car_matrix1[1:range_length, "Lat1"] <- outdata[pos1_range,"Latitude"]
    
    car_matrix2[1:range_length, "Long2"] <- outdata[pos2_range,"Longitude"]
    car_matrix2[1:range_length, "Lat2"] <- outdata[pos2_range,"Latitude"]
    
    
    #Getting VesselHeading
#    VesselHeading <- data.frame(read_csv(paste(fileFolder,'\\',VesselHeadingName,sep=''))) %>% 
#     mutate(locDevice = ifelse(deviceID == 16,'HEMISPHERE','AIRMAR')) %>% 
#      dplyr::filter(locDevice == 'AIRMAR') %>% 
#      group_by(locDevice) %>% 
#      ungroup() 
#    
#    outdata["VesselHeading"] <-interp1(unlist(VesselHeading["timestamp"]),unlist(VesselHeading["heading"]),indata[,"timestamp"],extrap=TRUE)   # Getting Car Speed and heading #

    rollavg <- 1
    outdata[pos2_range,"relative_carSpeedEW"] <- rollmean(indata[pos2_range,"Velocity"]*sin((indata[pos2_range,"Bearing"]) ),rollavg, na.pad=TRUE)#Components of direction. +N/E -S/W
    outdata[pos2_range,"relative_carSpeedNS"] <- rollmean(indata[pos2_range,"Velocity"]*cos((indata[pos2_range,"Bearing"]) ),rollavg, na.pad=TRUE) # ^^^^^
    
    FinalAggregated2[pos2_range,"AnemDistance"] <- distHaversine(car_matrix2,c(long_anem,lat_anem))
  }
  
  
  #Car Wind Speed Correction
  {
    outdata[pos2_range,"globalCarwindSpeedEW"] <- -1*rollmean(indata[pos2_range,"Windmaster.Mag."]*sin((indata[pos2_range,"Windmaster.Angle"]*pi/180 +  indata[pos2_range,"Bearing"])),rollavg,  na.pad=TRUE)
    outdata[pos2_range,"globalCarwindSpeedNS"] <- -1*rollmean(indata[pos2_range,"Windmaster.Mag."]*cos((indata[pos2_range,"Windmaster.Angle"]*pi/180 + indata[pos2_range,"Bearing"])),rollavg, na.pad=TRUE)
    
    outdata[pos2_range,"TruewindSpeedEW"] <-  outdata[pos2_range,"globalCarwindSpeedEW"] + outdata[pos2_range,"relative_carSpeedEW"]
    outdata[pos2_range,"TruewindSpeedNS"] <-  outdata[pos2_range,"globalCarwindSpeedNS"] + outdata[pos2_range,"relative_carSpeedNS"]
    
    outdata[pos2_range,"TruewindSpeed_Mag"] <- (outdata[pos2_range,"TruewindSpeedEW"]^2 + outdata[pos2_range,"TruewindSpeedNS"]^2)^0.5
    outdata[pos2_range,"TrueWindDirection"] <- (atan2(outdata[pos2_range,"TruewindSpeedEW"],outdata[pos2_range,"TruewindSpeedNS"])*180/pi + 180)
  }
  
  #Organizing output 
  {
  FinalAggregated2[pos2_range,"AnemDistance"] <- distHaversine(car_matrix2,c(long_anem,lat_anem))
  FinalAggregated2["globalCarwindSpeedEW"] <- outdata["globalCarwindSpeedEW"]
  FinalAggregated2["globalCarwindSpeedNS"] <- outdata["globalCarwindSpeedNS"]
  FinalAggregated2["TruewindSpeedEW"] <- outdata["TruewindSpeedEW"]
  FinalAggregated2["TruewindSpeedNS"] <- outdata["TruewindSpeedNS"]
  FinalAggregated2["TruewindSpeed_Mag"] <- outdata["TruewindSpeed_Mag"]
  FinalAggregated2["TrueWindDirection"] <- outdata["TrueWindDirection"]
  
  
  
  FinalAggregated2['carSide'] <- ifelse( ( FinalAggregated2$'Windmaster.Angle' > 0 & FinalAggregated2$'Windmaster.Angle'  <= 90 ),'FR',
                                           ifelse( ( FinalAggregated2$'Windmaster.Angle'  > 90 & FinalAggregated2$'Windmaster.Angle'  <= 180 ),'BR',
                                                   ifelse( ( FinalAggregated2$'Windmaster.Angle'  > 180 & FinalAggregated2$'Windmaster.Angle'  <= 270 ),'BL','FL')))
  
  
  
  
  
  
  
  
  #FinalAggregated2["CarNSRatio"] <- (as.numeric(FinalAggregated2[1:nrow(FinalAggregated2),"TruewindSpeedNS"]/FinalAggregated2[1:nrow(FinalAggregated2),"Array.Station.4.NS"]))
  #FinalAggregated2["CarEWRatio"] <- (as.numeric(FinalAggregated2[1:nrow(FinalAggregated2),"TruewindSpeedEW"]/FinalAggregated2[1:nrow(FinalAggregated2),"Array.Station.4.EW"]))
  FinalAggregated2["CarMagRatio"] <- (as.numeric(FinalAggregated2[1:nrow(FinalAggregated2),"TruewindSpeed_Mag"]/FinalAggregated2[1:nrow(FinalAggregated2),"Windmaster.Mag."]))
  
  
  #FinalAggregated2["RelErrorNS"] <- (FinalAggregated2["TruewindSpeedNS"] - FinalAggregated2["Array.Station.4.NS"])/((FinalAggregated2["Array.Station.4.NS"]))
  #FinalAggregated2["RelErrorEW"] <- (FinalAggregated2["TruewindSpeedEW"] - FinalAggregated2["Array.Station.4.EW"])/((FinalAggregated2["Array.Station.4.EW"]))
  #FinalAggregated2["RelErrorMag"]<- (FinalAggregated2["TruewindSpeed_Mag"] - FinalAggregated2["Array.Station.4.Mag."])/((FinalAggregated2["Array.Station.4.Mag."]))
  #FinalAggregated2["VesselHeading"] <- indata["VesselHeading"]*180/pi
  #FinalAggregated2["Bearing"] <- indata["Bearing"]*180/pi
  #FinalAggregated2["windAngle"] <- indata["windAngle"]*180/pi
  

  #FinalAggregated2 <- data.frame(FinalAggregated2[c(1:7, 25, 8:34)])
  }
  return(FinalAggregated2)
}