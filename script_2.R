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

# understand dataset 
head(train)
plot(train[,2], type='l')
colSums(is.na(train))
summary(train)
boxplot(train$transactions)

# Use complete function to find missing date
complete_train <- complete(train, date = seq(min(train$date), 
                                             max(train$date), 
                                             by = "day"))
# finding missing values
colSums(is.na(complete_train))
complete_train %>% 
  filter(is.na(transactions))

ts_train<-ts(complete_train$transactions, start=c(2013,1), frequency = 365)
plot(ts_train, type='l')
paste('Number of NA in dataset:', sum(is.na(ts_train)))


fill_miss<- function(loc){
  till_loc<-ts_train[1:loc-1]
  till_loc_ts<-ts(till_loc, start=c(2013,1), frequency=365) # to get the plot 
  # with time reference
  
  # building model and forcasting
  mod<- auto.arima(till_loc_ts)
  val_fill<-forecast(mod,1)
  
  # plot
  plot(till_loc_ts, type='l')
  lines(mod$fitted, col='yellow')
  lines(val_fill$mean, col='red')
  return(val_fill$mean)
}

which(is.na(ts_train))
miss<-as.numeric(strsplit(as.character(which(is.na(ts_train))), split="\\."))
class(miss)

# missing value row number: 
# 2013-12-25	359 
# 2014-12-25	724 
# 2015-12-25	1089 
# 2016-01-01	1096 
# 2016-01-03	1098 
# 2016-12-25	1455 

# 2013-12-25	359
ts_train[359]<-fill_miss(359)
# 2014-12-25	724
ts_train[724]<-fill_miss(724)
# 2015-12-25	1089
ts_train[1089]<-fill_miss(1089)
# 2016-01-01	1096
ts_train[1096]<-fill_miss(1096)
# 2016-01-03	1098
ts_train[1098]<-fill_miss(1098)
# 2016-12-25	1455
ts_train[1455]<-fill_miss(1455)


any(is.na(ts_train))

# no NA value in the series now 

# building model for future forcast 
acf(ts_train)
pacf(ts_train)

# using ts_train to train the arima model 
hw_final<-HoltWinters(ts_train)

hw_final_pred<-forecast(hw_final, length(test$date))
plot(hw_final_pred)

# submitting final predictions
sample_submission<-read.csv("sample_submission.csv")
submission_file<-sample_submission
submission_file$transactions<-NA
submission_file$transactions<-hw_final_pred$mean

#write.csv(submission_file, 'forecast_file.csv', row.names = FALSE)
