install.packages( rcmdr dependencies=TTR)
install.packages( rcmdr dependencies=forecast)
install.packages( rcmdr dependencies=tseries)

library("TTR")
library("forecast")
library("tseries")
library("Rcmdr")

##############################################################

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings
kingstimeseries <- ts(kings)
kingstimeseries

plot.ts(kingstimeseries)
# DickeyFuller Test
adf.test(kingstimeseries)
# KPSS Test
kpss.test(kingstimeseries)

acf(kingstimeseries)
pacf(kingstimeseries)
tsdisplay(kingstimeseries)

kingstimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot.ts(kingstimeseriesdiff1)
acf(kingstimeseriesdiff1)
pacf(kingstimeseriesdiff1)

model.1<- arima(kingstimeseries, order=c(1,1,0))
summary(model.1)

model.2<- arima(kingstimeseries, order=c(0,1,1))
summary(model.2)

auto.arima(kingstimeseries)
kingstimeseriesarima<-auto.arima(kingstimeseries)
plot(kingstimeseriesarima)

kingstimeseriesforecasts <- forecast(kingstimeseriesarima, h=5)
kingstimeseriesforecasts
plot(kingstimeseriesforecasts)

#####################################################

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))

plot.ts(rainseries)
acf(rainseries)
pacf(rainseries)

model.1<- arima(rainseries, order=c(0,0,1))
summary(model.1)

model.2<- arima(rainseries, order=c(1,0,0))
summary(model.2)

auto.arima(rainseries)
rainseriesarima<-auto.arima(rainseries)

rainseriesforecasts <- forecast.arima(rainseriesarima, h=10)
rainseriesforecasts 
plot(rainseriesforecasts)

####################################################

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
plot.ts(birthstimeseries)

birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

auto.arima(birthstimeseries)
birthseriesarima<-auto.arima(birthstimeseries)

birthseriesforecast<-forecast(birthseriesarima, h=20)
birthseriesforecast
plot(birthseriesforecast)

####################################################

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
plot.ts(souvenirtimeseries)

souvenirtimeseriescomponents <- decompose(souvenirtimeseries)
plot(souvenirtimeseriescomponents)

auto.arima(souvenirtimeseries)
souvenirmodel<-auto.arima(souvenirtimeseries)
souvenirforecast<-forecast(souvenirmodel, h=20)
plot(souvenirforecast)


####################################################

logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

logsouvenirtimeseriescomponents <- decompose(logsouvenirtimeseries)
plot(logsouvenirtimeseriescomponents)


