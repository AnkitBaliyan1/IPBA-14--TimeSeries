# forecast using prophet model 

# clear existing environment 
#rm(list=ls(all=TRUE))


# importing library
library("tseries")
library("forecast")
library("dplyr")
library("tidyr") #-- use complete function
library("prophet")

# import data
train<-read.csv("train.csv", na.strings = "")
test<-read.csv("test.csv",na.strings = "")
holidays<-read.csv("holidays.csv", na.strings = "")

# reading date columns as.Date
train$date=as.Date(train$date, format = '%d-%m-%Y')
holidays$Date=as.Date(holidays$Date, format = '%d-%m-%Y')

# Use complete function to find missing date
complete_train <- data.frame(complete(train, 
                                      date = seq(min(train$date),
                                                 max(train$date),
                                                 by = "day")))

complete_train %>% filter(is.na(transactions))

miss=which(is.na(complete_train$transactions))

for (i in miss) {
  complete_train$transactions[i]<-
    (complete_train$transactions[i-1] + 
       complete_train$transactions[i+1]) / 2
}

colSums(is.na(complete_train))

# prearing data frame to use prophet forecaster 
names(complete_train)<-c('ds','y')

pro_mod<-prophet(complete_train)
future<-make_future_dataframe(pro_mod,length(test$date))


# forecasting
yhat<-predict(pro_mod, future)
head(yhat[c('ds','yhat')])


# ploting
dyplot.prophet(pro_mod, yhat)
prophet_plot_components(pro_mod, yhat)



# submitting final predictions
yhat %>%
  filter(as.Date(ds)>="2017-01-01") %>%
  select(c('ds','yhat')) ->a
colnames(a)<-c('date','transactions')

sample_submission<-read.csv("sample_submission.csv")
submission_file<-sample_submission
submission_file$transactions<-NA
submission_file$transactions<-a$transactions

#write.csv(submission_file, 'basic_prophet.csv', row.names = FALSE)
# mape being 40.77539
