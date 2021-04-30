#####################################
###### FINC 4355 Assignment 2 #######
#####################################
###### Time Series Forecasting ######
#####################################

#1. Unemployment Rate Time Series Forecast

#Loading relevant packages into memory
library(forecast)
library(tseries)
library(TTR)
library(quadprog)

#Loading Arkansas Economic Data into memory and view data

library(readxl)
ArkansasGDP <- read_excel("C:/Users/maxim/Downloads/ArkansasGDP.xls")
View(ArkansasGDP)

#Pull Unemployment data from data set

unemprt <- ArkansasGDP$unemprt

#Plot the data to check if stationary

ts.plot(unemprt)

#Perform KPSS test to determine if stationary

kpss.test(unemprt)

#Transform data to ensure it is stationary

unemprt <- diff(unemprt, lag = 1)
plot.ts(unemprt)

#Plot acf to identify an AR model

acf(unemprt, lag.max = 48)

#Plot pacf to identify MA model

pacf(unemprt, lag.max = 48)

#Find the best ARIMA model (unemprtarima2)

unemprtarima1 <- arima(unemprt, order = c(1,0,0))
summary(unemprtarima1)

unemprtarima2 <- arima(unemprt, order = c(1,0,1))
summary(unemprtarima2)
plot(unemprtarima2)

unemprtarima3 <- arima(unemprt, order = c(1,1,1))
summary(unemprtarima3)

#Let R find the best model

unemprtautoarima <- auto.arima(unemprt)
summary(unemprtautoarima)
forecastunemprt <- forecast(unemprtautoarima, h = 20)
forecastunemprt
plot(forecastunemprt)

#2. Average Income Time Series Forecast

#Pull Average Wage data from data set

avgwage <- ArkansasGDP$avgwage

#Plot the data to check if stationary

ts.plot(avgwage)

#Perform KPSS test to determine if stationary

kpss.test(avgwage)

#Transform data to ensure it is stationary

avgwage <- diff(avgwage, lag = 1)
plot.ts(avgwage)

#Plot acf to identify an AR model

acf(avgwage, lag.max = 48)

#Plot pacf to identify MA model

pacf(avgwage, lag.max = 48)

#Find the best ARIMA model (avgwagearima2)

avgwagearima1 <- arima(avgwage, order = c(1,0,0))
summary(avgwagearima1)

avgwagearima2 <- arima(avgwage, order = c(1,0,1))
summary(avgwagearima2)

avgwagearima3 <- arima(avgwage, order = c(1,1,1))
summary(avgwagearima3)

#Let R find the best model

avgwageautoarima <- auto.arima(avgwage)
summary(avgwageautoarima)
forecastavgwage <- forecast(avgwageautoarima, h = 20)
forecastavgwage
plot(forecastavgwage)

#3. Google Trends Fever Time Series Forecast

#Pull Fever data from data set

fever <- FeverData$Fever

#Plot the data to check if stationary

ts.plot(fever)

#Perform KPSS test to determine if stationary

kpss.test(fever)

#Plot acf to identify an AR model

acf(fever, lag.max = 48)

#Plot pacf to identify MA model

pacf(fever, lag.max = 48)

#Find the best ARIMA model (feverarima3)

feverarima1 <- arima(fever, order = c(1,0,0))
summary(feverarima1)

feverarima2 <- arima(fever, order = c(1,0,1))
summary(feverarima2)

feverarima3 <- arima(fever, order = c(1,1,1))
summary(feverarima3)

#Let R find the best model

feverautoarima <- auto.arima(fever)
summary(feverautoarima)
forecastfever <- forecast(feverautoarima, h = 20)
forecastfever
plot(forecastfever)

