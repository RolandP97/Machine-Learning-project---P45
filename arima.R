setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))
library(dplyr)
library(forecast)

data <- read_csv("train.csv")
data$time <- as.POSIXlt(data$time) 
data <- data %>% filter(!is.na(consumption))

val <- data[ceiling(nrow(data)*0.8):nrow(data),]
train <- data[1:ceiling(nrow(data)*0.8)-1,]

TSGraphs <- function(series, lags = 30){
  layout(1:3)
  plot(series)
  acf(series, lags)
  pacf(series,lags)
  layout(1)
}

ts <- ts(train$consumption, start=c(2021,21),frequency=24*365)
ts
TSGraphs(ts, 600)

test$time <- as.POSIXlt(test$time) 
ts_test <- ts(train$consumption, start=c(2021,21),frequency=24*365)
predict(m1.1, 168)

m1.2 <- arima(ts, order = c(3,0,0))
tsdiag(m1.2,360)
AIC(m1.2)

m1.3 <- arima(ts, order = c(3,1,0))
tsdiag(m1.3,360)
AIC(m1.3)

m1.4 <- arima(ts, order = c(3,0,1))
tsdiag(m1.4,360)
AIC(m1.4)

m1.5 <- arima(ts, order = c(4,3,1))
tsdiag(m1.5,360)

AIC(m1.5)

TSGraphs(diff(diff(ts,24)),1000)

plot(ts)
model <- auto.arima(ts)
tsdiag(model, 360)
model

pred <- predict(model, 1719) # no predictive power
val$consumption

plot(pred$pred-val$consumption)

