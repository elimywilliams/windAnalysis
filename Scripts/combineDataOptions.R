library(readr)
library(signal)
#These Values Need to be changed for each data set 
{
  fileFolder <- 'data/FinalAggregated_210204_122536.csv'
  Append <- '_210204_122536.csv'
  #StationaryAnem <- "data/Stationary_210202_122942.csv"
  StationaryAnem <- 'data/array0204.dat'
  lat_anem <- 40.594926667
  long_anem <- -105.13984
}

#These Values remain constant 
{
  
  #AerisFileName <- paste('Aeris',Append,sep='_')
  #LocationFileName <- paste("Location",Append,sep='_')
  VesselHeadingName <- paste("VesselHeading",Append,sep='_')
  WindFileName <- paste('Stationary',Append,sep='_')
  finalFolder <- fileFolder
  finalFilename <- paste('finalAggregated',Append,sep='_')
  combinedScriptLoc <- 'combineDataScript.R'
  
  source("Scripts/WindCollection.R")
  
  source("Scripts/CarCorrection.R")
  
  source("Scripts/CarCorrectionAlt.R")
  
  #suppressMessages(source(combinedScriptLoc))
  
}



FinalAggregated2 <- WindCollection(StationaryAnem, finalFilename, finalFolder, lat_anem, long_anem)
write_csv(FinalAggregated2,'data/appended_210202_121448')
FinalAggregated2 <- CarCorrection_airmar(FinalAggregated2,fileFolder, VesselHeadingName, lat_anem, long_anem)
FinalAggregated2 <- CarCorrection_airmar_Alt(FinalAggregated2,fileFolder, VesselHeadingName, lat_anem, long_anem)

anem_data <- StationaryAnem
finalFilename <- finalFilename
finalFolder <- finalFolder
lat_anem <- lat_anem
long_anem <- long_anem

test <- filter(FinalAggregated2, AnemDistance < 43, RelErrorMag > -15, RelErrorMag < 15, AnemDeriv < 0, Velocity > 0.3, CarMagRatio < 10, TurnRate < 0.3)


selectdata <- FinalAggregated2[1:1500,1:ncol(FinalAggregated2)]


CarVel <- ggplot() +
  geom_point(data = selecdata, aes(x = nearest10hz, y = Velocity, color = "blue")) + # must include argument label "data"
  labs(title = "Car Velocity", x = "Time (s)", y = "Velocity (m/s)") +
  theme(legend.position="top",   plot.title = element_text(hjust = 0.5),)


MagPlot <- ggplot() +
  geom_point(data = FinalAggregated2, aes(x = nearest10hz, y = Array.Station.4.Mag., color = "blue")) + # must include argument label "data"
  geom_point(data = FinalAggregated2, aes(x = nearest10hz, y = TruewindSpeed_Mag_am, color = "purple")) + 
  geom_point(data = FinalAggregated2, aes(x = nearest10hz, y = Velocity, color = "red")) + 
  labs(title = "Car Wind Mag\n", x = "Time (s)", y = "Velocity (m/s)") +
  scale_color_manual(labels=c("Windmaster Array","Car Airmar","Car Velocity"), values = c("blue", "purple","red")) + 
  ylim(-2,30) +
  theme(legend.position="top",   plot.title = element_text(hjust = 0.5),)
MagPlot


Dirlot <- ggplot() +
  geom_point(data = selecdata, aes(x = nearest10hz, y = TrueWindDirection_am, color = "blue")) + # must include argument label "data"
  geom_point(data = selecdata, aes(x = nearest10hz, y = Array.Station.4.Angle, color = "red")) + 
  labs(title = "Wind Direction\n", x = "Time (s)", y = "Direction (degrees)") +
  scale_color_manual(labels=c("Car","Stationary"), values = c("blue", "red")) + 
  ylim(0,360) +
  theme(legend.position="top",   plot.title = element_text(hjust = 0.5),)


CarVStatVel <- ggplot() +
  geom_point(data = selecdata, aes(x = Array.Station.4.Mag., y = TruewindSpeed_Mag)) + 
  labs(title = "Stationary vs Car Wind Speed", x = "Stationary Mag (m/s)", y = "Car Mag (m/s)") +
  ylim(0,9) +
  theme(plot.title = element_text(hjust = 0.5),)


CarArrayRatio <- ggplot(data = test, aes(x = Velocity, y = CarMagRatio, color = Accel)) +
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  labs(title = "Car-Array Mag Ratio", x = "Car Velocity (m/s)", y = "Ratio") +
  ylim(0,10) +
  xlim(0,10) +
  theme(plot.title = element_text(hjust = 0.5),)
CarArrayRatio


Path <- ggplot() +
        geom_point(data = FinalAggregated2, aes(x = Latitude, y = Longitude, color = Accel)) + xlim(40.5949,40.5956)
Path

