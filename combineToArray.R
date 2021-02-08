library(zoo)
library(signal)
library(tidyr)
library(dplyr)
library(readr)
library(forecast)
library(geosphere)

#### gather and join to wind data
fileFolder <- 'data/ControlledRelease/FinalAggregated_210202_151036.csv'
Append <- '_210202_151036.csv'
#StationaryAnem <- "data/Stationary_210202_122942.csv"
StationaryAnem <- 'data/array0204.dat'
lat_anem <- 40.595306532795795817
long_anem <- -105.14417418850229069

#AerisFileName <- paste('Aeris',Append,sep='_')
#LocationFileName <- paste("Location",Append,sep='_')
VesselHeadingName <- paste("VesselHeading",Append,sep='_')
WindFileName <- paste('Stationary',Append,sep='_')
finalFolder <- fileFolder
finalFilename <- paste('finalAggregated',Append,sep='_')
combinedScriptLoc <- 'combineDataScript.R'

#anem_data <- read.csv(anem_data, header = FALSE)
#anem_data <- read_csv("data/Stationary_210202_122942.csv", 
#                      skip = 1)
anem_data <- read_csv(StationaryAnem,skip = 1)

UnixTime_calc <- (as.numeric(as.POSIXct(anem_data$TIMESTAMP, format="%Y-%m-%d %H:%M:%OS")) - 3600) #correcting for anem clock not falling back for daylight savings
Timestamp_calc <- format(as.POSIXct(round(UnixTime_calc,1), origin="1970-01-01"),"%Y-%m-%d %H:%M:%OS3")

anem_collected <- anem_data %>% 
  mutate(Timestamp = Timestamp_calc,
         UnixTime = UnixTime_calc) %>% 
  rename(Z1 = `Ux(1)`,Z2 = `Ux(2)`,Z3 = `Ux(3)`,Z4 = `Ux(4)`, Z5 = `Ux(5)`,
         EW1 = `Uy(1)`,EW2 = `Uy(2)`,EW3 = `Uy(3)`,EW4 = `Uy(4)`,EW5 = `Uy(5)`,
         NS1 = `Uz(1)`,NS2 = `Uz(2)`,NS3 = `Uz(3)`,NS4 = `Uz(4)`,NS5 = `Uz(5)`) %>% 
  dplyr::select(-contains('Ts'),-TIMESTAMP,-RECORD)

anem_collected <- anem_collected[100:nrow(anem_collected),1:ncol(anem_collected)] #Shaving off beginning of data
cardata <- read.csv(finalFolder)
#cardata <- select(cardata, 1:2,4:9,20:23,26:30)
cardata <- cardata %>%
  arrange((nearest10hz))
time1car <- min(cardata["nearest10hz"]) #Begin time
time2car <- max(cardata["nearest10hz"]) #Find end time
buffer <- 0

#Index where the stationary array time is closest to time 1 of car, with a "buffer" of data below
arrayindex1 <- which.min(abs(time1car-anem_collected$UnixTime)) - buffer
#Index where the stationary array time is closest to time 2 of car, with a "buffer" of data above 
arrayindex2 <- which.min(abs(time2car-anem_collected$UnixTime)) + buffer
indexrange <- arrayindex1:arrayindex2
small_collected <- (anem_collected[indexrange,])
combinedSets <- plyr::join(small_collected %>% rename(nearest10hz = UnixTime),cardata,type='full') %>% 
  mutate(LeakLat = lat_anem,
         LeakLon = long_anem)


