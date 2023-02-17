# clear existing environment 
rm(list=ls(all=TRUE))

# importing library
library("tseries")
library("forecast")
library("tidyr") # -- to find the missing date
library("dplyr") # data manipulation 
library("lubridate") # play with date-format

# import data
train<-read.csv("train.csv", na.strings = "")
test<-read.csv("test.csv",na.strings = "")
holidays<-read.csv("holidays.csv", na.strings = "")

factor=train

summary(factor)
boxplot(factor$transactions)
head(factor)

# outlier check 
q=quantile(factor$transactions, probs = c(0.25,0.75), )
IQR(factor$transactions)
upperlimit=q[2]+1.5*IQR(factor$transactions)
lowerlimit=q[1]-1.5*IQR(factor$transactions)
factor %>% 
  filter(transactions>upperlimit | transactions<lowerlimit)

# since these outlier follows a particular trend, hence this should not be 
# considered as outleir. This is the event in the series 



# check for continuity of series 
factor$date=as.Date(factor$date, format = '%d-%m-%Y')
holidays$Date=as.Date(holidays$Date, format = '%d-%m-%Y')
names(holidays)[1]='date'


# Use complete function to find missing date
complete_data <- complete(factor, date = seq(min(factor$date), 
                                             max(factor$date), 
                                             by = "day"))
# just like seq(1,10, by=2) 

complete_data%>%
  filter(is.na(transactions))
colSums(is.na(complete_data))
# missing value also follows a particular pattern 
# merge the complete data with the holidays data set 

data= merge(complete_data, holidays, by='date', all.x=TRUE)
colSums(is.na(data))
data%>%
  filter(is.na(transactions))

# only one missing value in data, rest all are holidays 
# that is 2016-01-03 

#handling missing value
data%>% filter(month(date)==1 & year(date)==2016)

# to visualise all the breaks in the data on 12-25
a=data%>%
  filter((month(date)==12 & day(date)>20) | 
           (month(date)==1 & day(date)<1) )

plot(a$transactions, type = 'o')

# converting transaction into time series object
data_ts<- ts(data$transactions, start=c(2013,1), frequency = 365)


# interpolating the na values in the time series object 
data_ts <- na.interp(data_ts)
plot(data_ts)

# decomposing time series to view componenets of the series 
decompose(data_ts)
plot(decompose(data_ts))

# check if the time series is stationary
adf.test(data_ts)


# 1. simple exponential smoothing model - no trend and no seasonality
ses_model=ses(data_ts)

ses_predict=forecast(ses_model, length(test$date))

plot(data_ts, main = "Time Series Plot with SES Forecast")
lines(ses_model$fitted, col='yellow')
lines(ses_predict$mean, col='red')

accuracy(ses_predict$fitted, factor$transactions)
#            ME  RMSE      MAE       MPE     MAPE
# Test set 361.4328 11694 8440.096 -15.23363 24.19513

# 2. holt model (captures only trend)
holt_model=holt(data_ts)

holt_pred=forecast(holt_model, 227)       

plot(data_ts, main = "Time Series Plot with Holt Forecast")
lines(holt_model$fitted,col='yellow')
lines(holt_pred$mean, col='red')

accuracy(holt_pred$fitted, factor$transactions)
#               ME     RMSE      MAE       MPE     MAPE
# Test set -120.7219 11966.84 8746.874 -16.08675 24.88127

# 3. hold winter model (captures trend as well as seasonality)
hw_model=HoltWinters(data_ts)
autoplot(hw_model$fitted)

hw_pred=forecast(hw_model, 227)

plot(data_ts, main = "Time Series Plot with Holt Winter Forecast")
lines(hw_pred$mean, col='red')

accuracy(hw_pred$fitted, factor$transactions)
#             ME     RMSE     MAE       MPE    MAPE
#Test set 283.0083 14271.54 9933.54 -5.919217 16.5553

# 4. auto arima model 

# building model 
arima_model=auto.arima(data_ts)

arima_pred=forecast(arima_model, length(test$date))

plot(data_ts, main = "Time Series Plot with ARIMAX Forecast")
lines(arima_pred$mean, col = "red")


accuracy(arima_pred$fitted, factor$transactions)
#            ME     RMSE      MAE       MPE     MAPE
# Test set 308.6671 13054.54 8798.926 -9.593629 19.10027

#----------------------------------------------

plot(decompose(ts_train))

model<- auto.arima(ts_train)
prediction<- forecast(model, length(test$date))

model<-ses(ts_train)
prediction<-forecast(model,227)


model<-holt(ts_train)
prediction<-forecast(model,227)

model<-HoltWinters(ts_train)
prediction<-forecast(model,227)

summary(model)
plot(ts_train)
lines(model$fitted, col='yellow')
lines(prediction$mean, col='red')
length(prediction$mean)