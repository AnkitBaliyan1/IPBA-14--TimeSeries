# holiday used 
# clear existing environment 
#rm(list=ls(all=TRUE))


# importing library
library("tseries")
library("forecast")
library("dplyr")
library("lubridate")
library("tidyr") #-- use complete function
library("ggplot2") # to convert the ts object back to dataframe



# import data
train<-read.csv("train.csv", na.strings = "")
test<-read.csv("test.csv",na.strings = "")
holidays<-read.csv("holidays.csv", na.strings = "")


# reading date columns as.Date
train$date=as.Date(train$date, format = '%d-%m-%Y')
test$date=as.Date(test$date, format = '%d-%m-%Y')
holidays$Date<-as.Date(holidays$Date, format = '%d-%m-%Y')

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

complete_holidays <- data.frame(complete(holidays, Date = seq(min(holidays$Date), 
                                                        max(holidays$Date), 
                                                        by = "day")))

# finding missing values
colSums(is.na(complete_train))
complete_train %>% 
  filter(is.na(transactions))

colSums(is.na(complete_holidays))
complete_holidays[is.na(complete_holidays$Holidays),]

complete_holidays$holidays<-ifelse(is.na(complete_holidays$holidays),0,1)



# merging complete_train and complete_holidays into one dataset
names(complete_holidays)<-c('date','holidays')
complete_train<-merge(complete_train, complete_holidays, by='date')

# treating na =0 and abrupt values =1 in holidays 
complete_train$holidays<-ifelse(is.na(complete_train$holidays), 
                                0,
                                ifelse(complete_train$holidays==0, 
                                       complete_train$holidays,
                                       1))

# missing value row number: 
# 2013-12-25	359 
# 2014-12-25	724 
# 2015-12-25	1089 
# 2016-01-01	1096 
# 2016-01-03	1098 
# 2016-12-25	1455 

complete_train %>% 
  filter(date=='2016-01-01') %>%
  select(transactions) 
complete_train %>%
  filter(month(date)==01 & day(date)==03) %>%
  summarise(m = mean(transactions, na.rm=TRUE))


fill_1<-complete_train %>%
  filter(month(date)==01 & day(date)==01) %>%
  summarise(m = mean(transactions, na.rm=TRUE))

fill_3<-complete_train %>%
  filter(month(date)==01 & day(date)==03) %>%
  summarise(m = mean(transactions, na.rm=TRUE))

# fill day mean for 1/1 and 1/3 in 2016 missing
complete_train$transactions<-
  ifelse(complete_train$date=='2016-01-01', 
         as.numeric(fill_1)
         ,ifelse(complete_train$date=='2016-01-03', 
                 as.numeric(fill_3)
                 ,complete_train$transactions))

# filling 0 @12-25
complete_train$transactions<-
  ifelse(is.na(complete_train$transactions),0,complete_train$transactions)

# deleting 29 feb from data 
complete_train <- complete_train %>% filter(date != as.Date('2016-02-29'))


ts_train<-ts(complete_train$transactions, start=c(2013,1), frequency = 365)
plot(ts_train, type='l')
paste('Number of NA in dataset:', sum(is.na(ts_train)))

which(is.na(ts_train))

summary(ts_train)
any(is.na(ts_train))

# no NA value in the series now 

# building model for future forcast 
plot(decompose(ts_train))
acf(ts_train)
pacf(ts_train)

adf.test(ts_train)

# building model 

mod1<-auto.arima(ts_train, xreg = complete_train$holidays)

plot(ts_train)
lines(mod1$fitted, col='red')

test<- merge(test, complete_holidays, by='date')
trans_pred<-forecast(mod1, h=length(test$id), xreg= test$holidays)

lines(trans_pred$mean,col='blue')
plot(trans_pred)

# submitting final predictions
sample_submission<-read.csv("sample_submission.csv")
submission_file<-sample_submission
submission_file$transactions<-NA
submission_file$transactions<-trans_pred$mean

Box.test(trans_pred$residuals, type='Ljung-Box')
checkresiduals(trans_pred$residuals)

# write.csv(submission_file, 'missing with holidays.csv', row.names = FALSE)

mod_arimax<-arima(ts_train, xreg = complete_train$holidays)

plot(ts_train)
pred_arimax<-forecast(mod_arimax, h = length(test$id))
