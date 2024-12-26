#library(tidyverse)
#library(TSA)
d = read.csv(file = "F:/lessons/time series/project/Iran.Khodro.csv")
d = separate(d,2,c("year","mouth","day"),
           sep =c(4,6),remove = TRUE)
d= d[,c(2,3,4,8)]
#View(d)



###### for example we have the 100 day 


response = d$X.CLOSE.[(nrow(d)-200):nrow(d)]

plot(response,typ="o")

#plot of auto correlation function: acf
acf(response,lag=50,ylab="acf")
#plot of  partial auto correlation function: pacf
pacf(response,lag=50,ylab="pacf")



(M1=arima(response,order=c(0,0,1)))
(M2=arima(response,order=c(0,0,2)))
(M3=arima(response,order=c(1,0,0)))
(M4=arima(response,order=c(2,0,0)))
(M5=arima(response,order=c(1,0,1)))
(M6=arima(response,order=c(2,0,2)))
(M7=arima(response,order=c(0,1,1)))
(M8=arima(response,order=c(0,1,2)))
(M9=arima(response,order=c(1,1,0)))
(M10=arima(response,order=c(2,1,0)))
(M11=arima(response,order=c(1,1,1)))
(M12=arima(response,order=c(2,1,2)))
(M13=arima(response,order=c(0,2,1)))
(M14=arima(response,order=c(0,2,2)))
(M15=arima(response,order=c(1,2,0)))
(M16=arima(response,order=c(2,2,0)))
(M17=arima(response,order=c(1,2,1)))
(M18=arima(response,order=c(2,2,2)))
#compute aic

c(M1$aic,M2$aic,M3$aic,M4$aic,M5$aic,M6$aic,
  M7$aic,M8$aic,M9$aic,M10$aic,M11$aic,M12$aic,
  M13$aic,M14$aic,M15$aic,M16$aic,M17$aic,M18$aic)
