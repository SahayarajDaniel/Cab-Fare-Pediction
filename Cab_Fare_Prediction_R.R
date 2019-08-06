rm(list = ls())

install.packages(c("ggplot2", "corrgram", "rpart", "geosphere"))

library(ggplot2)
library(DMwR)
library(dplyr)
library(corrgram)#for correlation calculaltion
library(rpart)#decision tree alg
library(rpart.plot)
library(randomForest)#for random forest algorithm
library(geosphere)

#set working directory
setwd("E:/EDW/Projects/Cab Fare Prediction")

#Loading Train Data and Test data to R environment
cab_data_train = read.csv("train_cab_data.csv", header = T)
cab_data_test = read.csv("test_cab_data.csv", header = T)
str(cab_data_train)  #'data.frame':	16067 obs. of  07 variables

#Converting to required data types
cab_data_train$fare_amount <- as.numeric(as.character(cab_data_train$fare_amount))
cab_data_train$pickup_datetime = strptime(x=as.character(cab_data_train$pickup_datetime), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#Train and Test data Exploration
train_sum <- summary(cab_data_train)
test_sum <- summary(cab_data_test)
test_summary <- as.data.frame(sapply(cab_data_test[ ,c("pickup_longitude", "pickup_latitude", "dropoff_longitude", "dropoff_latitude", "passenger_count")], summary))

#Outlier Limit Values determined from Test data
min_pickup_long = test_summary$pickup_longitude[1]
min_pickup_lat = test_summary$pickup_latitude[1]
min_drop_long = test_summary$dropoff_longitude[1]
min_drop_lat = test_summary$dropoff_latitude[1]
max_pickup_long = test_summary$pickup_longitude[6]
max_pickup_lat = test_summary$pickup_latitude[6]
max_drop_long = test_summary$dropoff_longitude[6]
max_drop_lat = test_summary$dropoff_latitude[6]
min_passenger_cnt = test_summary$passenger_count[1]
max_passenger_cnt = test_summary$passenger_count[6]

#Finding Latitude and Longitude Limits
min_latitude = min(min_pickup_lat, min_drop_lat)
max_latitude = max(max_pickup_lat, max_drop_lat)
min_longitude = min(min_pickup_long, min_drop_long)
max_longitude = max(max_pickup_long, max_drop_long)


#extrapolating hours, months, years values from date time field
cab_data_train$pickup_year <- cab_data_train$pickup_datetime$year+1900
cab_data_train$pickup_month <- cab_data_train$pickup_datetime$mon
cab_data_train$pickup_weekday <- cab_data_train$pickup_datetime$wday
cab_data_train$pickup_hour <- cab_data_train$pickup_datetime$hour
str(cab_data_train)  #'data.frame':	16067 obs. of  11 variable

cab_data_train$pickup_datetime <- as.POSIXct(cab_data_train$pickup_datetime)


#Missing Value analysis:
missing_val = data.frame(apply(cab_data_train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing Value Count"
missing_val$Missing_percentage = (missing_val$`Missing Value Count`/nrow(cab_data_train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1,3)]

# 
# #Actual value
# # > cab_data_train$passenger_count[14] # [1] 2
# # > cab_data_train$passenger_count[36] # [1] 6
# # > cab_data_train$passenger_count[52] # [1] 4
# cab_data_train$passenger_count[14] = NA
# cab_data_train$passenger_count[36] = NA
# cab_data_train$passenger_count[52] = NA
# # #Mean:
# # > cab_data_train$passenger_count[14]#[1] 2.629447    #Without Outliers - 1.649872
# # > cab_data_train$passenger_count[36]#[1] 2.629447
# # > cab_data_train$passenger_count[52]#[1] 2.629447
# #Median
# # > cab_data_train$passenger_count[14]# [1] 1          #Without Outliers - 1
# # > cab_data_train$passenger_count[36]# [1] 1
# # > cab_data_train$passenger_count[52]# [1] 1
# #kNN Imputation
# # > cab_data_train$passenger_count[14]# [1] 1
# # > cab_data_train$passenger_count[36]# [1] 1.325
# # > cab_data_train$passenger_count[52]# [1] 1.296
# 
# #Mean Method
# #cab_data_train$passenger_count[is.na(cab_data_train$passenger_count)] = mean(cab_data_train$passenger_count, na.rm = T)
# 
# #Median Method
# #cab_data_train$passenger_count[is.na(cab_data_train$passenger_count)] = median(cab_data_train$passenger_count, na.rm = T)
# 
# # kNN Imputation
# #temp_df <- cab_data_train[, -2 ]
# cab_data_train = knnImputation(cab_data_train[, -2], k = 3)
# #sum(is.na(cab_data_train))
# row.names(cab_data_train) <- NULL

#removing outliers in longitude and latitude values based on limit values from test data
cab_data_train <- cab_data_train[!((cab_data_train$pickup_longitude == 0) | (cab_data_train$pickup_latitude == 0) | (cab_data_train$dropoff_longitude==0) | (cab_data_train$dropoff_latitude == 0)), ]
nrow(cab_data_train) #15741 
cab_data_train <- cab_data_train[!((cab_data_train$pickup_longitude <= min_longitude) | (cab_data_train$pickup_longitude >= max_longitude) | (cab_data_train$dropoff_longitude >= max_longitude) | (cab_data_train$dropoff_longitude <= min_longitude)), ]
nrow(cab_data_train) #15729
cab_data_train <- cab_data_train[ !((cab_data_train$pickup_latitude <= min_latitude) | (cab_data_train$pickup_latitude >= max_latitude) | (cab_data_train$dropoff_latitude >= max_latitude) | (cab_data_train$dropoff_latitude <= min_latitude)) , ]
nrow(cab_data_train) #15714

#Calculating Distance from Latitudes and Longitudes
cab_data_train$Distance_Haversine = distHaversine(cbind(cab_data_train$pickup_longitude, cab_data_train$pickup_latitude),cbind(cab_data_train$dropoff_longitude, cab_data_train$dropoff_latitude))
cab_data_train$Distance_Meeus = distMeeus(cbind(cab_data_train$pickup_longitude, cab_data_train$pickup_latitude),cbind(cab_data_train$dropoff_longitude, cab_data_train$dropoff_latitude))
cab_data_train$Distance_Cosine = distCosine(cbind(cab_data_train$pickup_longitude, cab_data_train$pickup_latitude),cbind(cab_data_train$dropoff_longitude, cab_data_train$dropoff_latitude))
cab_data_train$Distance_Vincenty = distVincentyEllipsoid(cbind(cab_data_train$pickup_longitude, cab_data_train$pickup_latitude),cbind(cab_data_train$dropoff_longitude, cab_data_train$dropoff_latitude))

#Outlier analysis
numeric_index = sapply(cab_data_train,is.numeric) #selecting only numeric
numeric_data = cab_data_train[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("boxplot",i), ggplot(aes_string(y = (cnames[i])), data = subset(cab_data_train))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot:",cnames[i])))
}

# Plotting box plots together for dependant variables
gridExtra::grid.arrange(boxplot1, boxplot2, ncol=2)
gridExtra::grid.arrange(boxplot3, boxplot4,ncol=2)
gridExtra::grid.arrange(boxplot5, boxplot6, ncol=2)
gridExtra::grid.arrange(boxplot7, boxplot8,ncol=2)
gridExtra::grid.arrange(boxplot9, boxplot10,ncol=2)
gridExtra::grid.arrange(boxplot11, boxplot12,ncol=2)
gridExtra::grid.arrange(boxplot13, boxplot14,ncol=2)

#removing outliers in passenger count as the max passenger count can only be 6 (based on test data)
n1 <- sapply(cab_data_train$pickup_datetime, is.na)   #identifying the NA value in datetime column and removing it
cab_data_train <- cab_data_train[-which(n1), ]
nrow(cab_data_train)  ##15713
n2 <- sapply(cab_data_train$fare_amount, is.na)  #identifying the NA value in fare_amount column and removing it
cab_data_train <- cab_data_train[-which(n2), ]
cab_data_train <- cab_data_train[-which(cab_data_train$fare_amount<0), ] #removing negative values of fare amount
nrow(cab_data_train)  ##15687
#row.names(cab_data_train) <- NULL
#removing passengers greater than 6 and less than 1
cab_data_train <- cab_data_train[which((cab_data_train$passenger_count <= max_passenger_cnt) & (cab_data_train$passenger_count >= min_passenger_cnt)), ]
nrow(cab_data_train) #15559 after removing outliers in passenger count
#removing outliers in fare amount
fare_outlier = cab_data_train$fare_amount[cab_data_train$fare_amount %in% boxplot.stats(cab_data_train$fare_amount)$out]
cab_data_train = cab_data_train[which(!cab_data_train$fare_amount %in% fare_outlier),]
nrow(cab_data_train) #14210 after removing outliers
#removing outliers in Distance
dist_outlier = cab_data_train$Distance_Haversine[cab_data_train$Distance_Haversine %in% boxplot.stats(cab_data_train$Distance_Haversine)$out]
cab_data_train = cab_data_train[which(!cab_data_train$Distance_Haversine %in% dist_outlier),]
nrow(cab_data_train) #13572 after removing outliers

#BoxPlots after removing outliers
numeric_index = sapply(cab_data_train,is.numeric) #selecting only numeric
numeric_data = cab_data_train[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("boxplot",i+14), ggplot(aes_string(y = (cnames[i])), data = subset(cab_data_train))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot-Outliers Removed: ",cnames[i])))
}

# Plotting box plots together for dependant variables
gridExtra::grid.arrange(boxplot15, boxplot16, ncol=2)
gridExtra::grid.arrange(boxplot17, boxplot18,ncol=2)
gridExtra::grid.arrange(boxplot19, boxplot20, ncol=2)
gridExtra::grid.arrange(boxplot21, boxplot22,ncol=2)
gridExtra::grid.arrange(boxplot23, boxplot24,ncol=2)
gridExtra::grid.arrange(boxplot25, boxplot26,ncol=2)
gridExtra::grid.arrange(boxplot27, boxplot28,ncol=2)

#Missing Value analysis after removing Outliers:
missing_val = data.frame(apply(cab_data_train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing Value Count"
missing_val$Missing_percentage = (missing_val$`Missing Value Count`/nrow(cab_data_train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1,3)]
#No missing Values found

#Visualization:
#distribution of  variables
ggplot(data= cab_data_train, aes(x=fare_amount)) + ggtitle(paste("Distribution of Fare_Amount"))+ geom_freqpoly(bins =5)
ggplot(data= cab_data_train, aes(x=pickup_hour)) + ggtitle(paste("Distribution of Hour"))+ geom_freqpoly(bins = 5)
ggplot(data= cab_data_train, aes(x=pickup_weekday)) + ggtitle(paste("Distribution of Weekday"))+ geom_freqpoly(bins = 5)
ggplot(data= cab_data_train, aes(x=pickup_month)) + ggtitle(paste("Distribution of Pickup_Month"))+ geom_freqpoly(bins = 5)
ggplot(data= cab_data_train, aes(x=Distance_Haversine)) +ggtitle(paste("Distribution of Distance"))+ geom_freqpoly(bins = 5)
ggplot(data= cab_data_train, aes(x=passenger_count)) +ggtitle(paste("Distribution of Passenger Count"))+ geom_freqpoly(bins = 5)


cleaned_data <- write.csv(cab_data_train, file = "cleaned_cab_train_data.csv")

##Feature Selection - Correlation Plot 
corrgram(cab_data_train[,numeric_index], order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

cab_data_train <- cab_data_train[, c("fare_amount","pickup_longitude", "pickup_latitude", "passenger_count", "pickup_hour", "pickup_weekday", "Distance_Haversine")]    

#Matching the data frame structure of test and train data set
#Converting to required data types
cab_data_test$fare_amount <- 0
cab_data_test$pickup_datetime = strptime(x=as.character(cab_data_test$pickup_datetime), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#extrapolating hours, months, years values from date time field for test data
cab_data_test$pickup_year <- cab_data_test$pickup_datetime$year+1900
cab_data_test$pickup_month <- cab_data_test$pickup_datetime$mon
cab_data_test$pickup_weekday <- cab_data_test$pickup_datetime$wday
cab_data_test$pickup_hour <- cab_data_test$pickup_datetime$hour
cab_data_test$Distance_Haversine = distHaversine(cbind(cab_data_test$pickup_longitude, cab_data_test$pickup_latitude),cbind(cab_data_test$dropoff_longitude, cab_data_test$dropoff_latitude))

str(cab_data_test)  ##'data.frame':	9914 obs. of  11 variables
cleaned_test_data <- write.csv(cab_data_test, file = "cleaned_cab_test_data.csv")

cab_data_test <- cab_data_test[, c("fare_amount","pickup_longitude", "pickup_latitude", "passenger_count", "pickup_hour", "pickup_weekday", "Distance_Haversine")]    

#Divide the data into train and test
train_index = sample(1:nrow(cab_data_train), 0.70 * nrow(cab_data_train))
train = cab_data_train[train_index,]
test = cab_data_train[-train_index,]

#1. Decision tree algorith for regression
# cab_data_test <- cab_data_test[ , -1]
# cab_data_train <- cab_data_train[, -2]

##Regression algorithm - anova
regression_result = rpart(fare_amount ~ ., data = train, method = "anova", control = rpart.control(cp = 0.00085, minsplit = 4, minbucket = 5, maxdepth = 10))
#prediction of test values
predict_DT = predict(regression_result, test[,-1])
rpart.plot(regression_result, type = 1)
printcp(regression_result)
plotcp(regression_result)
# 
# #MAPE
# #calculate MAPE
# mape = function(y, yhat){
#   mean(abs((y - yhat)))*100
# }
#RMSE
#calculate RMSE
Rmse = function(x,xhat){
  sqrt(mean(x-xhat)^2)
}
#accuracy_DT = 100 - (mape(cab_data_test[,6], predict_DT))
rmse_DT = Rmse(test[,1], predict_DT)
rmse_DT
#alternate method to find error metrics for regression.
#regr.eval(test[,1], predict_DT, stats = c('mae', 'rmse', 'mse'))

##2. Random Forest algorithm
RF_model = randomForest(fare_amount ~ ., train, importance = TRUE, ntree = 50, mtry = 2)
#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-1])
#accuracy_RF = 100 - (mape(cab_data_test[,1], RF_Predictions))
rmse_RF = Rmse(test[,1], RF_Predictions)
rmse_RF
#regr.eval(test[,1], RF_Predictions, stats =c('mae', 'rmse', 'mse'))
#bestmtry <- tuneRF(cab_data_train, cab_data_train$fare_amount, stepFactor = 1.5, improve = 0.01, trace = T, plot = T)



##3.Linear regression

#check multicollearity
library(usdm) #calculate Variation Inflation factor
#install.packages("rms")
require(rms)
vif(train[,-1]) #to find there is any multicollinearity in the data
vifcor(train[,-1], th = 0.9)

#run regression model
lm_model = lm(fare_amount ~., data = train)
# model1 <- ols(fare_amount~., data=cab_data_train)
# model2 = fastbw(model1, rule="p", sls=0.4)

#Summary of the model
summary(lm_model)

#Predict

predictions_LR = predict(lm_model, test[,-1])
#accuracy_LR_wo = 100 - (mape(cab_data_test[,1], predictions_LR_wo))
rmse_LR = Rmse(test[,1], predictions_LR)
rmse_LR
#regr.eval(test[,1], predictions_LR, stats = c('rmse'))

print("RMSE of Decision Tree Method")
rmse_DT
print("RMSE of Random Forest Method")
rmse_RF
print("RMSE of Linear Regression Method")
rmse_LR

#########################################################################################################
scatter.smooth(x=cab_data_train$Distance_Haversine, y= cab_data_train$fare_amount, span = 1/9, main = "Distance vs Fare")
scatter.smooth(x=cab_data_train$passenger_count, y= cab_data_train$fare_amount, span = 1/9, main = "Distance vs Fare")
plot(density(cab_data_train$fare_amount), main = "Fare Amount Density func", ylab = "Frequency", col = "red")
polygon(density(cab_data_train$fare_amount), col = "red")
