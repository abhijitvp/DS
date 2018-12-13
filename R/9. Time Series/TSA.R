setwd("C:/Govinda/PU2376/project/Desktop/Desktop/Data Science/Edu Pristine/Yogesh Sir Batch/timeseriesandclustering")
rain=read.csv("rain.csv")

# converting to a time series data format
raints=ts(rain,start=c(1817))

plot(raints)

########################################################################
#                 Single Exponential Smoothening                       #
########################################################################

rainforecast=HoltWinters(raints,beta=F,gamma=F)

rainforecast
plot(rainforecast)
names(rainforecast)  
rainforecast$fitted

r2=HoltWinters(raints,alpha=0.02,beta=F,gamma=F)

#Making future forecasts
library(forecast)
r2forecast=forecast.HoltWinters(r2,h=10)
plot(r2forecast)

# To figure out the randomness of the model
hist(r2forecast$residuals)
acf(r2forecast$residuals,lag.max=20)
qqnorm(r2forecast$residuals)

########################################################################
#                 Double Exponential Smoothening                       #
########################################################################

skirts=read.csv("skirts.csv")
skirts=ts(skirts,start=c(1866))
plot(skirts)
class(skirts)

#as you can see this time series has trend and a simple "level" forecast will not be enough.
skirtforecast=HoltWinters(skirts,gamma=F)

plot(skirtforecast)
skirtforecast

#Making future forecast
skirtfuture=forecast.HoltWinters(skirtforecast,h=19)
plot(skirtfuture)
skirts
#To figure out the randomness of the model
qqnorm(skirtfuture$residual)

########################################################################
#                  Triple Exponential Smoothening                      #
########################################################################

souvenir=read.csv("souvenir.csv")
souvenirts <- ts(souvenir, frequency=12, start=c(1987,1))
plot(souvenirts)

#Removing the multiplicative trend
souvenirts=log(souvenirts)
plot(souvenirts)

souvenirforecast=HoltWinters(souvenirts)
souvenirforecast

plot(souvenirforecast)
souvenirfuture=forecast.HoltWinters(souvenirforecast,h=48)

plot(souvenirfuture)

#To figure out the randomness of the model
acf(souvenirfuture$residuals,lag.max=30)
qqnorm(souvenirfuture$residuals)
plot(souvenirfuture$residuals)

########################################################################
#                             ARIMA                                    #
########################################################################

#ARIMA models have 3 parameters  and is generally written as ARIMA(p,d,q)
library(forecast)

souvenir=read.csv("souvenir.csv")
souvenirts <- ts(souvenir, frequency=12, start=c(1987,1))
plot(souvenirts)


plot(diff(souvenirts,1))
plot(diff(souvenirts,12))
plot(diff(diff(souvenirts,1),12))


auto.arima(souvenirts)

#ARIMA(2,0,0)(0,1,1)[12]

#p=2 ,d=0, q=0 | P=0 , D=1 , Q=1 | m=12

arimafit=arima(souvenirts,order=c(1,1,1),seasonal=c(0,1,1))

arimafuture=forecast.Arima(arimafit,h=48)
plot(arimafuture)

acf(arimafuture$residuals,lag.max=20)
hist(arimafuture$residuals)
qqnorm(arimafuture$residuals)
