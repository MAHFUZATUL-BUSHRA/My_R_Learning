#Libs Reqd
library(forecast)
library(ggplot2)
library(dplyr)

#Load data
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
tdata <- ts(births, frequency = 12, start = c(1946, 1))

#Plot

#Use of pipe operator
#data
#function - x()
#function - y()
#function - z()
#data %>% x() %>% y() %>% z()

tdata %>% plot()

#Decompose
tdata %>% decompose() %>% plot()

#Plot ARIMA models
ts <- tdata %>% auto.arima(trace = T)

accuracy(ts)

pred <- forecast(ts, h = 12*10)

autoplot(tdata, series = "Main Data") +
  autolayer(pred, series = "ARIMA forecast") +
  ylab("numder of births") + xlab("months") + ggtitle("number of births per month in New York city")

#Checking residual
checkresiduals(ts)

train <- window(tdata, start = c(1946,1), end = c(1958,12))

test <- window(tdata, start = c(1959,1))

train_df <- train %>% auto.arima()

pred_1960 <- forecast(train_df, h = 12)

accuracy(object = pred_1960, x = test)
