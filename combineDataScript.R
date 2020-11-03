###### Libraries
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forecast)
library(zoo)
options(digits = 15)

ch4_lag <- 4

#### equations 

gcd.hf2 <- function(loc1deg, loc2deg) 
{
  long1r <- loc1deg[1]*pi/180
  lat1r <- loc1deg[2]*pi/180
  long2r <- loc2deg[1]*pi/180
  lat2r <- loc2deg[2]*pi/180
  R <- 6364.335164547501335619
  delta.long <- long2r-long1r
  delta.lat <- lat2r-lat1r
  a <- sin(delta.lat/2)^2 + cos(lat1r)*cos(lat2r)*sin(delta.long/2)^2
  c <- 2*asin(min(1,sqrt(a)))
  d <- R*c
  return(d*1000) # Distance in m
}

calc_bearing <- function(loc1deg,loc2deg){
  long1r <- loc1deg[1]*pi/180
  lat1r <- loc1deg[2]*pi/180
  long2r <- loc2deg[1]*pi/180
  lat2r <- loc2deg[2]*pi/180
  
  X <-  cos(lat2r) * sin(long2r - long1r)
  Y <-  cos(lat1r) * sin(lat2r) - (sin(lat1r) * cos(lat2r) * cos(long2r - long1r))
  
  theta <-  atan2(X, Y)
  theta <-  theta %% (2 * pi)
  return (theta)
}


#### info
# timestamp is when the data was written onto the archiver
# devicetimestamp is based on when data was read in?
# goal is to join based on timestamp

## CREATE VALUES TO THE NEAREST MS FIRST
roundVal <- 3


## BRING IN WIND DATA FILE
#WindData <- read_csv("~/Documents/archive_201001_120120_driving/WindData.csv") %>% 
WindData <- read_csv(paste(fileFolder,'/',WindFileName,sep='')) %>% 
  mutate(obsNum = 1:n()) %>% 
  filter(obsNum > 300)%>%  ## remove 1st 30s ish of data
  select(-obsNum) %>% 
  mutate(nearestMS = format(round(round(timestamp,roundVal),roundVal),nsmall = roundVal)) %>% 
  group_by(nearestMS) %>% 
  mutate(totMS = n()) %>% 
  ungroup %>% 
  mutate(step = timestamp - lag(timestamp,1)) %>% 
  mutate(msstep = as.numeric(nearestMS) - as.numeric(lag(nearestMS,1),na.rm=T)) %>% 
  group_by(nearestMS) %>% 
  sample_n(1) %>% 
  ungroup()


## BRING IN LOCATION DATA FILE
#locData <- read_csv("~/Documents/archive_201001_120120_driving/Location.csv") %>% 
locData <- read_csv(paste(fileFolder,'/',LocationFileName,sep='')) %>% 
  mutate(locDevice = ifelse(deviceID == 16,'HEMISPHERE','AIRMAR')) %>% 
  filter(locDevice == 'HEMISPHERE') %>% 
  group_by(locDevice) %>% 
  mutate(obsNum = 1:n()) %>% 
  ungroup() %>% 
  filter(obsNum > 300)%>%  ## remove 1st 30s ish of data
  select(-obsNum) %>% 
  mutate(nearestMS = format(round(round(timestamp,roundVal),roundVal),nsmall = roundVal)) %>% 
  group_by(nearestMS,locDevice) %>% 
  mutate(totMS = n()) %>% 
  mutate(latdif = latitude - lag(latitude,1)) %>% 
  mutate(londif = longitude - lag(longitude,1)) %>% 
  ungroup() %>% 
  group_by(locDevice) %>% 
  mutate(step = timestamp - lag(timestamp,1)) %>% 
  mutate(msstep = as.numeric(nearestMS) - as.numeric(lag(nearestMS,1),na.rm=T)) %>% 
  ungroup() %>% 
  rename(tsloc = timestamp) %>% 
  rename(dtsloc = deviceTimestamp,
         nameloc = name,
         channelloc = channel,
         deviceIDloc = deviceID,
         steploc = step,
         mssteploc = msstep) %>% 
  select(-totMS)

time_shift <- 1
locDatSpeed <- locData %>% 
  mutate(num = 1:n()) %>% 
  rename(Latitude = latitude,Longitude = longitude) %>% 
  mutate(prev_lat = lag(Latitude,time_shift),
         next_lat = lead(Latitude,time_shift),
         prev_long = lag(Longitude,time_shift),
         next_long = lead(Longitude,time_shift),
         prev_time = lag(tsloc,time_shift),
         next_time = lead(tsloc,time_shift)) %>% 
  group_by(num) %>% 
  mutate(dist = gcd.hf2(c(prev_long,prev_lat),c(next_long,next_lat))) %>% 
  mutate(bearing = calc_bearing(c(prev_long,prev_lat),c(next_long,next_lat))) %>% 
  mutate(timediff = next_time - prev_time,
         VELOCITY = dist/timediff) %>% 
  mutate(theta_compass =  bearing,
         car_velocity = VELOCITY) %>% 
  mutate(theta =  (pi/2 - theta_compass)%%(2*pi)) %>% 
  ungroup()

locDatSpeed2 <- locDatSpeed %>% 
  mutate(num = 1:n()) %>% 
  #rename(Latitude = latitude,Longitude = longitude) %>% 
  mutate(prev_lat = lag(Latitude,time_shift),
         next_lat = lead(Latitude,time_shift),
         prev_long = lag(Longitude,time_shift),
         next_long = lead(Longitude,time_shift),
         prev_time_ms = as.numeric(lag(nearestMS,time_shift)),
         next_time_ms = as.numeric(lead(nearestMS,time_shift))) %>% 
  # filter(!is.na(prev_lat),!is.na(next_lat)) %>% 
  group_by(num) %>% 
  mutate(dist_ms = gcd.hf2(c(prev_long,prev_lat),c(next_long,next_lat))) %>% 
  mutate(bearing_ms = calc_bearing(c(prev_long,prev_lat),c(next_long,next_lat))) %>% 
  mutate(timediff_ms = next_time_ms - prev_time_ms,
         VELOCITY_ms = dist_ms/timediff_ms) %>% 
  mutate(theta_compass_ms =  bearing_ms,
         car_velocity_ms = VELOCITY_ms) %>% 
  mutate(theta_ms =  (pi/2 - theta_compass_ms)%%(2*pi)) %>% 
  ungroup() %>% 
  mutate(nearestMS = as.numeric(nearestMS),which2 = 'loc')

## BRING IN AERIS DATA FILE
#AerisDat <- read_csv("~/Documents/archive_201001_120120_driving/Aeris.csv") %>% 
AerisDat <- read_csv(paste(fileFolder,'/',AerisFileName,sep='')) %>% 
  select(-name,-channel,-deviceID) %>% 
  rename('adevTS' = devTimestamp,'C1C2' = `C1/C2`) %>% 
  mutate(obsNum = 1:n()) %>% 
  filter(obsNum > 5)%>%  ## remove 1st 30s ish of data
  select(-obsNum) %>% 
  mutate(nearestMS = format(round(round(timestamp,roundVal),roundVal),nsmall = roundVal)) %>% 
  group_by(nearestMS) %>% 
  mutate(totMS = n()) %>% 
  ungroup() %>% 
  mutate(step = timestamp - lag(timestamp,1)) %>% 
  mutate(msstep = as.numeric(nearestMS) - as.numeric(lag(nearestMS,1),na.rm=T)) %>% 
  mutate(shiftedCH4 = lag(CH4,ch4_lag),
         shiftedC2H6 = lag(C2H6,ch4_lag)) %>% 
  select(-inletNumber)



## first
firstLocTime <- as.numeric(locData$nearestMS[1])
firstwindTime <- as.numeric(WindData$nearestMS[1])
firstaerisTime <- as.numeric(AerisDat$nearestMS[1])

lastLocTime <- max(as.numeric(locData$nearestMS),na.rm=T)
lastwindTime <- max(as.numeric(WindData$nearestMS),na.rm=T)
lastaerisTime <- max(as.numeric(AerisDat$nearestMS),na.rm=T)


### CORRECTING FOR LOCATION DATA (ADDING IN MS)
betweentimes <- seq(round(firstLocTime,roundVal),ceiling(lastLocTime),by=1*10^(-roundVal))
joincols <- data.frame(nearestMS = as.character(betweentimes),which1='fulltime')
#nearcols <- data.frame(nearestMS =as.character(nearestTimes),some = 'loc')
bothties <- plyr::join(joincols,locDatSpeed %>% mutate(nearestMS = as.character(nearestMS)),type='full')

## INTERPOLATING THE LOCATION DATA
loc_int <- bothties %>% 
  select(-car_velocity,-nameloc,-channelloc,-which1) %>% 
  mutate(true_loc = ifelse(locDevice == "HEMISPHERE",T,F)) %>% 
  mutate(obsnum = 1:n())

loc_int$Latitude_interp <- as.numeric(na.interp(loc_int$Latitude))
loc_int$Longitude_interp <- as.numeric(na.interp(loc_int$Longitude))

firstVel <-  min(loc_int[!is.na(loc_int$VELOCITY),]$obsnum)

loc_int2 <- loc_int %>% 
  #mutate(obs= 1:n()) %>% 
  filter(obsnum >= firstVel) %>% 
  mutate(fillVel= zoo::na.locf(VELOCITY)) %>% 
  mutate(filldtsloc= zoo::na.locf(dtsloc)) %>% 
  mutate(filltsloc= zoo::na.locf(tsloc)) %>% 
  mutate(fillbearing= zoo::na.locf(bearing)) %>% 
  select(nearestMS,filltsloc,filldtsloc,Latitude_interp,Longitude_interp,fillVel,fillbearing,true_loc)


### CORRECTING FOR WIND DATA (ADDING IN MS)
WindData2 <- WindData %>% 
  mutate(nearestMS = as.character(as.numeric((nearestMS))))

betweentimes_wd <- seq(round(firstwindTime,roundVal),ceiling(lastwindTime),by=1*10^(-roundVal))
joincols_wd <- data.frame(nearestMS = betweentimes_wd,windtime_full=T) %>% 
  mutate(nearestMS = as.character(nearestMS))
#nearcols_wd <- data.frame(nearestMS =as.character(nearestTimes),some = 'loc')

bothties_wd <- plyr::join((joincols_wd %>% 
                             mutate(nearestMS = as.character(nearestMS))),WindData2,type='full')

### CORRECTING FOR AERIS DATA (ADDING IN MS)
aerisDat2 <- AerisDat %>% 
  mutate(nearestMS = as.character(as.numeric((nearestMS))))

betweentimes_aer <- seq(round(firstaerisTime,roundVal),ceiling(lastaerisTime),by=1*10^(-roundVal))
joincols_aer <- data.frame(nearestMS = betweentimes_aer,aerisTime=T) %>% 
  mutate(nearestMS = as.character(nearestMS))
bothties_aer <- plyr::join((joincols_aer %>% 
                              mutate(nearestMS = as.character(nearestMS))),aerisDat2,type='full') %>% 
  mutate(obsnum = 1:n())

firstval <-  min(bothties_aer[!is.na(bothties_aer$shiftedCH4),]$obsnum)

aer_int <- bothties_aer %>% 
  select(-step,-msstep,-batteryCharge) %>% 
  filter(obsnum >= firstval) %>% 
  group_by(nearestMS) %>% 
  mutate(true_aeris = as.logical(ifelse(!is.na(P),1,0))) %>% 
  ungroup() %>% 
  mutate(fillP= zoo::na.locf(P)) %>% 
  mutate(fillT= zoo::na.locf(T)) %>% 
  mutate(fillCH4= zoo::na.locf(CH4)) %>% 
  mutate(fillH2O= zoo::na.locf(H2O)) %>% 
  mutate(fillC2H6= zoo::na.locf(C2H6)) %>% 
  mutate(fillC2H6= zoo::na.locf(C2H6)) %>% 
  mutate(fillR= zoo::na.locf(R)) %>% 
  mutate(fillC1C2= zoo::na.locf(C1C2)) %>% 
  mutate(fillC2H6= zoo::na.locf(C2H6)) %>% 
  mutate(fillpower= zoo::na.locf(powerInput)) %>% 
  mutate(fillcurr= zoo::na.locf(current)) %>% 
  mutate(fillSOC= zoo::na.locf(SOC)) %>% 
  mutate(fillaets= zoo::na.locf(aerisTimestamp)) %>% 
  mutate(fillshiftCH4= zoo::na.locf(shiftedCH4)) %>% 
  mutate(fillshiftC2H6= zoo::na.locf(shiftedC2H6)) 

aer_int2 <- aer_int %>% 
  select(nearestMS,starts_with('fill'),true_aeris)

#   
# loc_int$Latitude_interp <- as.numeric(na.interp(loc_int$Latitude))
# loc_int$Longitude_interp <- as.numeric(na.interp(loc_int$Longitude))
# 
# loc_int2 <- loc_int %>% 
#   mutate(obs= 1:n()) %>% 
#   filter(obs >= 100) %>% 
#   mutate(fillVel= zoo::na.locf(VELOCITY)) %>% 
#   mutate(filldtsloc= zoo::na.locf(dtsloc)) %>% 
#   mutate(filltsloc= zoo::na.locf(tsloc)) %>% 
#   
#   mutate(fillbearing= zoo::na.locf(bearing)) %>% 
#   select(nearestMS,filltsloc,filldtsloc,Latitude_interp,Longitude_interp,fillVel,fillbearing,true_loc)
# 


t1 <- loc_int2 %>%   
  mutate(nearestMS = as.character(as.numeric(nearestMS))) 

t2 <- bothties_wd %>%   
  mutate(nearestMS = as.character(as.numeric(nearestMS)))

t3 <- aer_int2 %>% 
  mutate(nearestMS = as.character(as.numeric(nearestMS))) 

windLocation <- plyr::join(t1,t2,type='full')
allDatCombined <- plyr::join(windLocation,t3,type='full')


## smallW_wind
allDat_w_wind <- allDatCombined %>% 
  filter(!is.na(windReference)) %>% 
  select(-filltsloc,-filldtsloc,-timestamp,-deviceTimestamp,-name,-channel,-deviceID,
         -sequenceID,-step,-msstep,-fillaets,-windtime_full,-totMS) %>% 
  rename('Latitude' = Latitude_interp,
         'Longitude' = Longitude_interp,
         'Velocity' = fillVel,
         'Bearing' = fillbearing,
         'TrueLocationReading' =true_loc,
         'P' = fillP,
         'T' = fillT,
         'CH4' = fillCH4,
         'H2O' = fillH2O,
         'C2H6' = fillC2H6,
         'R' = fillR,
         'C1C2' = fillC1C2,
         'Power' = fillpower,
         'Current' = fillcurr,
         'SOC' = fillSOC,
         'CH4_Shift' = fillshiftCH4,
         'C2H6_Shift' = fillshiftC2H6,
         'TrueAerisReading' = true_aeris,
         'timestamp' = nearestMS)

write.csv(allDat_w_wind,paste(finalFolder,'/',finalFilename,sep=''))
rm(list=ls())

