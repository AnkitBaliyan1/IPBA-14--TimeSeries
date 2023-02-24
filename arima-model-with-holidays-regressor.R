# clear existing environment 
#rm(list=ls(all=TRUE))


# importing library
library("tseries")
library("forecast")
library("dplyr")
library("tidyr") #-- use complete function
library("ggplot2") # to convert the ts object back to dataframe



# import data
train<-read.csv("train.csv", na.strings = "")
test<-read.csv("test.csv",na.strings = "")
holidays<-read.csv("holidays.csv", na.strings = "")


# reading date columns as.Date
train$date=as.Date(train$date, format = '%d-%m-%Y')
holidays$Date=as.Date(holidays$Date, format='%d-%m-%Y')
test$date=as.Date(test$date, format='%d-%m-%Y')

# preparing holidays data
complete_holiday <- data.frame(complete(holidays, Date = seq(min(holidays$Date), 
                                                        max(holidays$Date), 
                                                        by = "day")))
complete_holiday$Holidays<- ifelse(complete_holiday$Holidays!=1, 0,
                          complete_holiday$Holidays)
colnames(complete_holiday)=c('date','holidays')

# understand dataset 
head(train)
plot(train[,2], type='l')
colSums(is.na(train))
summary(train)
boxplot(train$transactions)

# Use complete function to find missing date
complete_train <- data.frame(complete(train, date = seq(min(train$date), 
                                                        max(train$date), 
                                                        by = "day")))
complete_train$date=as.Date(complete_train$date, format = '%d-%m-%Y')
class(complete_train)


complete_train=merge(complete_train, complete_holiday, by='date')
complete_test=merge(test, complete_holiday, by='date')

# finding missing values
colSums(is.na(complete_train))
complete_train %>% 
  filter(is.na(transactions))

which(is.na(ts_train))
miss<-as.numeric(strsplit(as.character(which(is.na(ts_train))), split="\\."))
class(miss)

for (i in miss) {
  complete_train$transactions[i]<-
    (complete_train$transactions[i-1] + complete_train$transactions[i+1]) / 2
}

summary(ts_train)
any(is.na(ts_train))
# missing value row number: 
# 2013-12-25	359 
# 2014-12-25	724 
# 2015-12-25	1089 
# 2016-01-01	1096 
# 2016-01-03	1098 
# 2016-12-25	1455 

any(is.na(ts_train))

ts_train<-ts(complete_train$transactions, start=c(2013,1), frequency = 365)
plot(ts_train, type='l')
ts_holiday<-ts(complete_train$holidays, start=c(2013,1), frequency = 365)
plot(ts_holiday, type='l')
ts_test_holiday<-ts(complete_test$holidays, start=c(2017,1), frequency = 365)
paste('Number of NA in dataset:', sum(is.na(ts_train)))


# no NA value in the series now 

# building model for future forcast 
acf(ts_train)
pacf(ts_train)

adf.test(ts_train)

# using ts_train to train the holtwinters model 
hw_final<-HoltWinters(ts_train)
hw_final_pred<-forecast(hw_final, length(test$date))

hw_final_h<-HoltWinters(ts_train, optim.control = list(ts_holiday))
hw_final_pred_h<-forecast(hw_final_h, length(test$date))

accuracy(hw_final_pred$mean, hw_final_pred_h$mean)

Box.test(hw_final_pred_h$residuals, type='Ljung-Box')


# auto arima regressor

ar<-auto.arima(ts_train)
ar_pred<-forecast(ar, length(test$date))

ar_h<-auto.arima(ts_train, xreg = as.numeric(ts_holiday))
ar_pred_h<-forecast(ar_h, length(test$date), xreg = as.numeric(ts_test_holiday))

accuracy(ar_pred$mean, ar_pred_h$mean)

# submitting final predictions
sample_submission<-read.csv("sample_submission.csv")
submission_file<-sample_submission
submission_file$transactions<-NA
submission_file$hw<-hw_final_pred_h$mean

Box.test(ar_pred_h$residuals, type='Ljung-Box')
acf(ar_pred_h$residuals, lag=20)
plot(ar_pred_h$residuals)
hist(ar_pred_h$residuals, freq = FALSE)

#write.csv(
 # submission_file,'arima_model with holidays regressor.csv', row.names = FALSE)
