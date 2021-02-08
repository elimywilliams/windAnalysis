calcVals <- function(ws,theta){
  theta_rad <- theta *pi/180
  #FrontBack <- round(-ws*cos(theta_rad),2)
  
 #SideSide <- round(ws*sin(theta_rad),2)
  SideSide <- round(-ws*cos(theta_rad),2)
  FrontBack <- round(-ws*sin(theta_rad),2)
  
  return(paste('FB: ',FrontBack, ' SS: ',SideSide,sep=''))
  
  
}

calcVals(5,0)
calcVals(5,45)
calcVals(5,90)
calcVals(5,180)
calcVals(5,270)
calcVals(5,203)

library(openair)
comp1a <- comp1 %>% 
  group_by(rnum) %>% 
  mutate(wd = airmar_wd*180/pi) %>% 
  mutate(ws = airmar_ws) %>% 
  ungroup() %>% 
  dplyr::filter(wd < 0)

windRose(comp1a)

comp1a %>% 
  #dplyr::filter(rnum >20000) %>% 
  #dplyr::filter(rnum < 30000) %>% 
  dplyr::filter(rnum %%50 ==0) %>% 
  ggplot() +
  geom_vector(aes(angle=wd,mag=ws,
                  x=rnum,y=1)) + 
  theme(legend.position  ='none',
        axis.text.y = element_blank())

plot(comp1a$frontBackAirmar/(comp1a$ws))
plot(comp1a$sideSideAirmar/(comp1a$ws))

plot(comp1$airmar_ws,ylim=c(0,20),pch=20)
points(comp1$windmaster_ws_4,col='red',pch=20)
plot(comp1$airmar_wd)

library(zoo)

comp1 %>% 
  mutate(airroll=rollapply(airmar_ws_fit,400,median,align='center',fill=NA)) %>%
  mutate(windroll=rollapply(windmaster_ws_4,400,median,align='center',fill=NA)) %>% 
  ggplot(aes(y=airroll-windroll,x=airroll))+geom_point()
