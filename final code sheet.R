
# setting the working directory, reading the .csv file and defining the time series object 

setwd("C:/Users/Amit Kulkarni/Documents/R Programming/Time Series/project 6")
demand=read.csv("demand.csv")
demandA = ts(demand[,3], start=c(2002, 1), frequency = 12)
plot(demandA)
demandB = ts(demand[,4], start=c(2002, 1), frequency = 12)
plot(demandB)
monthplot(demandA)
monthplot(demandB)

# decomposition and STL model of forecasting

demandAstl = stl(demandA, s.window = 'p') # Assuming seasonality in data
demandAstl
plot(demandAstl)
demandAstl7 = stl(demandA, s.window = 7) # Assuming no seasonality in data
demandAstl7
plot(demandAstl7)
demandBstl7 = stl(demandB, s.window = 7) # Assuming no seasonality in data
plot(demandBstl7)
demandBstl = stl(demandB, s.window = 'p') # Assuming seasonality in data
plot(demandBstl)


Desesrev= (demandAstl7$time.series[,2]+demandAstl7$time.series[,3])
ts.plot(Desesrev, demandA, col=c("red", "blue"), main="Comparison of demand and deseasonalized demand")

DesesrevB= (demandBstl7$time.series[,2]+demandBstl7$time.series[,3])
ts.plot(DesesrevB, demandB, col=c("red", "blue"), main="Comparison of demand and deseasonalized demand")

# dividing the data in train and hold out period using the function "window"

demandAtrain= window(demandA, start=c(2002,1), end=c(2015, 10), frequency = 12)
demandAtest = window(demandA, start=c(2015,11), frequency=12)
demandBtrain= window(demandB, start=c(2002,1), end=c(2015, 10), frequency = 12)
demandBtest = window(demandB, start=c(2015,11), frequency=12)

#decomposition of the train data

DmndTrnA= stl(demandAtrain, s.window = 7)
DmndTrnB= stl(demandBtrain, s.window = 7)

library(reshape2)
#install.packages("tseries")
library(tseries)


#install.packages("fpp2")
library(fpp2)
# Demand Forecasting and calculation of MAPE under exponential smoothing
forecast.demandA <- forecast(DmndTrnA, method="rwdrift", h=21)
plot(forecast.demandA)
forecast.demandA
forecast.demandB <- forecast(DmndTrnB, method="rwdrift", h=21)
plot(forecast.demandB)
forecast.demandB
VecA<- cbind(demandAtest,forecast.demandA$mean)
ts.plot(VecA, col=c("blue", "red"), main="Demand A vs Forecast")
MAPE <- mean(abs(VecA[,1]-VecA[,2])/VecA[,1])
MAPE
VecB<- cbind(demandBtest,forecast.demandB$mean)
ts.plot(VecB, col=c("blue", "red"), main="Demand A vs Forecast")
MAPE <- mean(abs(VecB[,1]-VecB[,2])/VecB[,1])
MAPE
# Forecast for simple exponential smoothing
fcdemandA = ses(demandAtest, h =5)
plot(fcdemandA)
fcdmndA = ses(demandA, h =3)
plot(fcdmndA)
ts.plot(demandA, fcdmndA$fitted, col=c("brown", "gold"))
round(accuracy(fcdmndA),2)
fcdmndA$model
fcdmndA$mean
fcdmndA2 = ses(demandA, alpha = 0.2)
fcdmndA2
fcdmndA5 = ses(demandA, alpha = 0.5)
fcdmndA5
fcdmndA8 = ses(demandA, alpha = 0.8)
fcdmndA8
ts.plot(demandA,fcdmndA2$fitted, fcdmndA5$fitted, fcdmndA8$fitted, col=c("black","red","blue"))
#Seasonality with Holt Winter Model
fcdmndA_Holt = holt(demandA, h=21)
plot(fcdmndA_Holt)
fcdmndA_Holt
ts.plot(demandA, fcdmndA_Holt$fitted, col=c("black","red"))

fcdmndB_Holt = holt(demandB, h=21)
plot(fcdmndB_Holt)
fcdmndB_Holt
ts.plot(demandB, fcdmndB_Holt$fitted, col=c("gold","red"))

VecA_Holt = cbind(demandA, fcdmndA_Holt$fitted)
MAPE_Holt = mean(abs(VecA_Holt[,1] - VecA_Holt[,2])/VecA_Holt[,1])
MAPE_Holt
fcdmndA_Holt$model
fcdmndA_Holt$mean
VecB_Holt = cbind(demandB, fcdmndB_Holt$fitted)
MAPE_Holt_B = mean(abs(VecB_Holt[,1] - VecB_Holt[,2])/VecB_Holt[,1])
MAPE_Holt_B
fcdmndB_Holt$model
fcdmndB_Holt$mean

#Holt Winters with function hw
demandAHW = hw(demandA, h=21)
ts.plot(demandAtest, demandAHW$mean, col=c("red", "blue"))

VecA_HW = cbind(demandA, demandAHW$fitted)
MAPE_HW = mean(abs(VecA_HW[,1] - VecA_HW[,2])/VecA_HW[,1])
MAPE_HW

demandAHW$model
demandAHW$mean

# for demand B

demandBHW = hw(demandB, h=21)
ts.plot(demandBtest, demandBHW$mean, col=c("red", "blue"))
VecB_HW = cbind(demandB, demandBHW$fitted)
MAPE_HW_B = mean(abs(VecB_HW[,1] - VecB_HW[,2])/VecB_HW[,1])
MAPE_HW_B

demandBHW$model
demandBHW$mean

#forecast for demand A
demandAtest_stl= stl(demandAtrain, s.window=5)
fc_demandAtest_stl= forecast(demandAtest_stl, method="rwdrift", h=21)
plot(fc_demandAtest_stl)
vecA_STL = cbind(demandAtest, fc_demandAtest_stl$mean)
MAPE_A_STL = mean(abs(vecA_STL[,1]-vecA_STL[,2])/vecA_STL[,1])
MAPE_A_STL

#forecast for demand B
demandBtest_stl= stl(demandBtrain, s.window=5)
fc_demandBtest_stl= forecast(demandBtest_stl, method="rwdrift", h=21)
plot(fc_demandBtest_stl)
vecB_STL = cbind(demandBtest, fc_demandBtest_stl$mean)
MAPE_B_STL = mean(abs(vecB_STL[,1]-vecB_STL[,2])/vecB_STL[,1])
MAPE_B_STL

# Doing the stationary test using the ADF test
adf.test(demandA)
adf.test(demandB)


demandA.arima.fit = arima(demandAtest, c(1,1,0))
demandA.arima.fit
demandAfit = fitted(demandA.arima.fit)
ts.plot(demandAtrain, demandAfit, col=c("red", "green"))
demandB.arima.fit = arima(demandBtest, c(1,1,0))
demandB.arima.fit
demandBfit = fitted(demandB.arima.fit)
ts.plot(demandBtrain, demandBfit, col=c("blue", "brown"))

Box.test(demandA.arima.fit$residuals, lag=30, type="Ljung-Box")
acf(demandA.arima.fit$residuals)



#AutoRegression ACF 
plot(acf(demandAtrain, lag=50))
plot(acf(demandBtrain, lag=50))


demandA.arima.fit = arima(demandAtrain, c(1,1,0))
demandA.arima.fit
hist(demandA.arima.fit$residuals, col="red")
demandB.arima.fit = arima(demandBtrain, c(1,1,0))
demandB.arima.fit
hist(demandB.arima.fit$residuals, col="navy")
Box.test(demandB.arima.fit$residuals, lag=30, type="Ljung-Box")
# setwd ("C:/Users/Amit Kulkarni/Documents/R Programming/packages")

demandA.arima.fit_FC = forecast(demandA.arima.fit, h=15)
plot(demandA.arima.fit_FC)
demandA.arima.fit_FC

demandB.arima.fit_FC = forecast(demandB.arima.fit, h=15)
plot(demandB.arima.fit_FC)
demandB.arima.fit_FC
