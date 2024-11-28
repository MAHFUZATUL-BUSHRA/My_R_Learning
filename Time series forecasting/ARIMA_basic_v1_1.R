##Libraries required
library(tseries) #for adf.test
library(forecast)

##Import data
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
tdata <- ts(births, frequency = 12, start = c(1946, 1))
tdata

##plot
plot(tdata, col = "blue", lwd = 2)

##Explore the components

#Mean
plot(
  tdata,
  col = "blue",
  lwd = 2,
  main = "number of births per month in New York city",
  xlab = "months",
  ylab = "numder of births"
)

abline(reg = lm(tdata ~ time(tdata)),
       col = "red",
       lwd = 2)

#Trend
plot(aggregate(tdata, FUN = mean), col = "blue", lwd = 2)

#Decompose
decomp_ts <- decompose(tdata)

plot(decomp_ts)

##Steps to make the data stationary

##Differencing to remove the trend

tdata2 <- diff(tdata, differences = 1)

plot(tdata2)

abline(reg = lm(diff(log(tdata)) ~ time(diff(log(
  tdata
)))),
col = "red",
lwd = 1.5)

#ACF plot
acf(tdata2, lag.max = 50)

#Seasonal differencing
tdata3 <- diff(tdata2, lag = 12)

acf(tdata3, lag.max = 50)
pacf(tdata3, lag.max = 50)

##Stationarity test
adf.test(tdata3)

##ARIMA Model and exploration of its coefficients
#AR -> Auto-Regressive Model
#MA -> Moving Average Model
#I -> Integrated

#AR -> p = 3
#I -> d = 1
#MA -> q = 3

#Value of q
acf(tdata3, lag.max = 50)

#Value of p
pacf(tdata3, lag.max = 50)

#Value of d
#d = number of times of differencing

##Build model
model <- auto.arima(tdata, trace = T)

summary(model)

#Ljung-Box test
Box.test(model$residual, type = "Ljung")

##Predict
plot(forecast(model, h = 120))

##Model performance
tdata_train <-
  ts(
    births,
    frequency = 12,
    start = c(1946, 1),
    end = c(1958, 12)
  )

model <- auto.arima(tdata_train, trace = T)

#Prediction
pred <- predict(model, n.ahead = 12)

##Check result
round(tail(tdata, 12), 0)
round(pred$pred, 0)
