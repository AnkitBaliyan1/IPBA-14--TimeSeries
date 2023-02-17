# clear existing environment 
rm(list=ls(all=TRUE))


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
complete_train <- data.frame(complete(train, date = seq(min(train$date), 
                                                        max(train$date), 
                                                        by = "day")))
complete_train$date=as.Date(complete_train$date, format = '%d-%m-%Y')
class(complete_train)

# finding missing values
colSums(is.na(complete_train))
complete_train %>% 
  filter(is.na(transactions))

ts_train<-ts(complete_train$transactions, start=c(2013,1), frequency = 365)
plot(ts_train, type='l')
paste('Number of NA in dataset:', sum(is.na(ts_train)))

miss<-as.numeric(strsplit(as.character(which(is.na(ts_train))), split="\\."))

data.frame(complete_train %>% 
             group_by(year(date)) %>%
             summarise(mean(transactions, na.rm=TRUE)))-> mean_table
names(mean_table)<-c('year','mean')

na_fill<- function(index){
  for (y in seq(1:length(mean_table$year))){
    if (mean_table[y,1] == year(complete_train$date[index])){
      return(mean_table[y,2])
    }
  }
}

for (i in miss) {
  ts_train[i]<-na_fill(i)
}


any(is.na(ts_train))

# no NA value in the series now 

# building model for future forcast 
acf(ts_train)
pacf(ts_train)

adf.test(ts_train)

# using ts_train to train the arima model 
hw_final<-HoltWinters(ts_train)

hw_final_pred<-forecast(hw_final, length(test$date))
plot(hw_final_pred)

# submitting final predictions
sample_submission<-read.csv("sample_submission.csv")
submission_file<-sample_submission
submission_file$transactions<-NA
submission_file$transactions<-hw_final_pred$mean

write.csv(submission_file, 'hw & yearly_mean_at_na.csv', row.names = FALSE)
