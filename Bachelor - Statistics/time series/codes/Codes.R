# TSA package for fit time series models  in R

library(TSA)

#simulation of random walk process x_t=x_{t-1}+z_t,  let x_0=0

n=100

sim=rnorm(n)

x=cumsum(sim)

sim.random.walk=ts(x,freq=1,start=1)

plot(sim.random.walk,type="o",ylab="Another Random Walk")


data(larain)

larain

length(larain)

plot(larain,ylab="Inches",xlab="Year",type="o")


model=lm(larain~time(larain))

abline(model)

summary(model)

fitted(model)

acf(larain,lag=100)

#prinf acf

acf(larain,plot=F,lag=50)$acf


pacf(larain,lag=50)


#MA######################################################


#simulation of MA(1): x_t=z_t-0.9z_{t-1}*************

set.seed(12345)

y=arima.sim(model=list(ma=c(0.9)),n=100)

y

plot(y,typ="o")


#plot of auto correlation function: acf

acf(y,lag=50,ylab="acf")

#plot of  partial auto correlation function: pacf

pacf(y,lag=50,ylab="pacf")


M1=arima(y,order=c(0,0,1))

M2=arima(y,order=c(0,0,2))

M3=arima(y,order=c(1,0,0))

M4=arima(y,order=c(2,0,0))

#compute aic

c(M1$aic,M2$aic,M3$aic,M4$aic)



#simulation of MA(2): x_t=z_t-0.9z_{t-1}+0.4z_{t-2}************

set.seed(12345)

y=arima.sim(model=list(ma=c(0.9,-0.4)),n=100)

y

plot(y,typ="o")


#plot of auto correlation function: acf

acf(y,lag=50,ylab="acf")

#plot of  partial auto correlation function: pacf

pacf(y,lag=50,ylab="pacf")


M1=arima(y,order=c(0,0,1))

M2=arima(y,order=c(0,0,2))

M3=arima(y,order=c(1,0,0))

M4=arima(y,order=c(2,0,0))

#compute aic

c(M1$aic,M2$aic,M3$aic,M4$aic)

#AR#######################################################


#simulation of AR(1): x_t=-0.9x_{t-1}+z_{t}*********************

y=arima.sim(model=list(ar=c(-0.9)),n=100)

y

plot(y,typ="o")

#plot of auto correlation function: acf

acf(y,lag=50,ylab="acf")

#plot of  partial auto correlation function: pacf

pacf(y,lag=50,ylab="pacf")


M1=arima(y,order=c(0,0,1))

M2=arima(y,order=c(0,0,2))

M3=arima(y,order=c(1,0,0))

M4=arima(y,order=c(2,0,0))

#compute aic

c(M1$aic,M2$aic,M3$aic,M4$aic)



#simulation of AR(2): x_t=0.4x_{t-1}+0.1x_{t-2}+z_{t}************


y=arima.sim(model=list(ar=c(0.4,0.1)),n=100)

y

plot(y,typ="o")

#plot of auto correlation function: acf

acf(y,lag=50,ylab="acf")

#plot of  partial auto correlation function: pacf

pacf(y,lag=50,ylab="pacf")


M1=arima(y,order=c(0,0,1))

M2=arima(y,order=c(0,0,2))

M3=arima(y,order=c(1,0,0))

M4=arima(y,order=c(2,0,0))

#compute aic

c(M1$aic,M2$aic,M3$aic,M4$aic)


####ARMA######################################################

#simulation of ARMA(1,1): x_t=0.9x_{t-1}+z_{t}-0.5z_{t-1}

y=arima.sim(model=list(ar=c(0.9),ma=c(0.5)),n=100)

plot(y,typ="o")

#plot of auto correlation function: acf

acf(y,lag=50,ylab="acf")

#plot of  partial auto correlation function: pacf

pacf(y,lag=50,ylab="pacf")


M1=arima(y,order=c(0,0,1))

M2=arima(y,order=c(0,0,2))

M3=arima(y,order=c(1,0,0))

M4=arima(y,order=c(2,0,0))


M5=arima(y,order=c(1,0,1))

M6=arima(y,order=c(2,0,2))

#compute aic

c(M1$aic,M2$aic,M3$aic,M4$aic,M5$aic,M6$aic)



#simulation of ARMA(1,2): x_t=0.9x_{t-1}+z_{t}-0.5z_{t-1}+0.6z_{t-2}***********

y=arima.sim(model=list(ar=c(0.9),ma=c(0.5,-0.6)),n=100)

plot(y,typ="o")

#plot of auto correlation function: acf

acf(y,lag=50,ylab="acf")

#plot of  partial auto correlation function: pacf

pacf(y,lag=50,ylab="pacf")


M1=arima(y,order=c(1,0,1))

M2=arima(y,order=c(2,0,2))

M3=arima(y,order=c(1,0,2))


#compute aic

c(M1$aic,M2$aic,M3$aic)


#simulation of ARMA(2,2): x_t=0.4x_{t-1}+0.2x_{t-2}+z_{t}-0.5z_{t-1}+0.6z_{t-2}*****

y=arima.sim(model=list(ar=c(0.4,0.2),ma=c(0.5,-0.6)),n=100)

plot(y,typ="o")

#plot of auto correlation function: acf

acf(y,lag=50,ylab="acf")

#plot of  partial auto correlation function: pacf

pacf(y,lag=50,ylab="pacf")


M1=arima(y,order=c(1,0,1))

M2=arima(y,order=c(2,0,2))

M3=arima(y,order=c(1,0,2))


#compute aic

c(M1$aic,M2$aic,M3$aic)


############################################################################

