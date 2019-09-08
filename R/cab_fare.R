setwd("C:/manideep/edwisor/project - 3/")

# loading datasets
cab_fare = read.csv("C:/manideep/edwisor/project - 3/train_cab.csv", header = T, na.strings = c(" ", "", "NA"))
cab_fare_test = read.csv("C:/manideep/edwisor/project - 3/test.csv")

#test_pickup_datetime = test["pickup_datetime"]

# basic datatypes of variables in the data.
str(cab_fare)
str(cab_fare_test)
summary(cab_fare)
summary(cab_fare_test)


#----------------------------------------------Exploratory Data Analysis--------------------------------------------------
# Changing the data types of variables
cab_fare$fare_amount = as.numeric(as.character(cab_fare$fare_amount))
class(cab_fare$fare_amount)


# Identifying the passenger count which are beyond the seat limit.
nrow(cab_fare[which((cab_fare$passenger_count > 6) | (cab_fare$passenger_count < 1)),])


# Remove the observations with abnormal passenger_count.
cab_fare = cab_fare[-which(cab_fare$passenger_count < 1 ),]
cab_fare = cab_fare[-which(cab_fare$passenger_count == 1.3 ),]
cab_fare = cab_fare[-which(cab_fare$passenger_count > 6),]

cab_fare$passenger_count = as.factor(as.character(cab_fare$passenger_count))
cab_fare_test$passenger_count = as.factor(as.character(cab_fare_test$passenger_count))

# Identifying the latitudes and longitudes limit.
print(paste('pickup_longitude above 180 :',nrow(cab_fare[which(cab_fare$pickup_longitude > 180 ),])))
print(paste('pickup_longitude above -180 :',nrow(cab_fare[which(cab_fare$pickup_longitude < -180 ),])))
print(paste('pickup_latitude above 90 :',nrow(cab_fare[which(cab_fare$pickup_latitude > 90 ),])))
print(paste('pickup_latitude above -90 :',nrow(cab_fare[which(cab_fare$pickup_latitude < -90 ),])))
print(paste('dropoff_longitude above 180 :',nrow(cab_fare[which(cab_fare$dropoff_longitude > 180 ),])))
print(paste('dropoff_longitude above -180 :',nrow(cab_fare[which(cab_fare$dropoff_longitude < -180 ),])))
print(paste('dropoff_latitude above -90 :',nrow(cab_fare[which(cab_fare$dropoff_latitude < -90 ),])))
print(paste('dropoff_latitude above 90 :',nrow(cab_fare[which(cab_fare$dropoff_latitude > 90 ),])))


# Removing the observations for which latitudes and longitudes are abnormal.
cab_fare = cab_fare[-which(cab_fare$pickup_latitude > 90),]


#-----------------------------------------Missing Value Analysis--------------------------------------------------

missing_val = data.frame(apply(cab_fare,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(cab_fare)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val

# 1.For Passenger_count:
# Actual value = 1
# Mode = 1
# KNN = 1
cab_fare$passenger_count[99]
cab_fare$passenger_count[99] = NA
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Mode Method
getmode(cab_fare$passenger_count)

# 2.For fare_amount:
# Actual value = 18.1,
# Mean = 15.117,
# Median = 8.5,
# KNN = 18.28

cab_fare$fare_amount[99]
cab_fare$fare_amount[99]= NA

library(DMwR)

# Mean Method
mean(cab_fare$fare_amount, na.rm = T)

#Median Method
median(cab_fare$fare_amount, na.rm = T)

# kNN Imputation
cab_fare = knnImputation(cab_fare, k = 199)
cab_fare$fare_amount[99]
cab_fare$passenger_count[99]


#--------------------------------------------------Outlier Analysis----------------------------------------------
library(rlang)
library(ggplot2)

#box plot for target variable
b_plot = ggplot(cab_fare,aes(x = passenger_count,y = fare_amount))
b_plot + geom_boxplot(outlier.colour="grey", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)

#Remove outliers
numeric_index = sapply(cab_fare,is.numeric) #selecting only numeric
numeric_data = cab_fare[,numeric_index]
cnames = colnames(numeric_data)

for(i in cnames){
  print(i)
  val = cab_fare[,i][cab_fare[,i] %in% boxplot.stats(cab_fare[,i])$out]
  print(length(val))
  cab_fare = cab_fare[which(!cab_fare[,i] %in% val),]
}

#-------------------------------------------------Feature engineering-------------------------------------

cab_fare$pickup_date = as.Date(as.character(cab_fare$pickup_datetime))
cab_fare$pickup_mnth = as.numeric(format(cab_fare$pickup_date,"%m"))
cab_fare$pickup_yr = as.numeric(format(cab_fare$pickup_date,"%Y"))
pickup_time = strptime(cab_fare$pickup_datetime,"%Y-%m-%d %H:%M:%S")
cab_fare$pickup_hour = as.numeric(format(pickup_time,"%H"))

#Add same features to test set
cab_fare_test$pickup_date = as.Date(as.character(cab_fare_test$pickup_datetime))
cab_fare_test$pickup_mnth = as.numeric(format(cab_fare_test$pickup_date,"%m"))
cab_fare_test$pickup_yr = as.numeric(format(cab_fare_test$pickup_date,"%Y"))
pickup_time = strptime(cab_fare_test$pickup_datetime,"%Y-%m-%d %H:%M:%S")
cab_fare_test$pickup_hour = as.numeric(format(pickup_time,"%H"))


# Calculate distance in kilometers between two points
distance_travelled <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

missing_val1 = data.frame(apply(cab_fare,2,function(x){sum(is.na(x))}))

#Imputation
#pickup_mnth, pickup_year,pickup_hr
#Applying mode.
cab_fare$pickup_mnth[is.na(cab_fare$pickup_mnth)] = getmode(cab_fare$pickup_mnth)
cab_fare$pickup_yr[is.na(cab_fare$pickup_yr)] = getmode(cab_fare$pickup_yr)
cab_fare$pickup_hour[is.na(cab_fare$pickup_hour)] = getmode(cab_fare$pickup_hour)

cab_fare$tot_dis = distance_travelled(cab_fare$pickup_longitude, cab_fare$pickup_latitude, cab_fare$dropoff_longitude, cab_fare$dropoff_latitude)
cab_fare_test$tot_dis = distance_travelled(cab_fare_test$pickup_longitude,cab_fare_test$pickup_latitude,cab_fare_test$dropoff_longitude,cab_fare_test$dropoff_latitude)

cab_fare = subset(cab_fare, select = -c(pickup_datetime,pickup_date,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
cab_fare_test = subset(cab_fare_test ,select = -c(pickup_datetime,pickup_date,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))



#--------------------------------------------------Feature selection----------------------------------------------

numeric_index1 = sapply(cab_fare,is.numeric) #selecting only numeric
numeric_data1 = cab_fare[,numeric_index1]
cnames1 = colnames(numeric_data1)

#Correlation analysis for numeric variables
library(corrplot)

cor_matrix = cor(cab_fare[,numeric_index1])
corrplot(cor_matrix, type = "lower", tl.col = "red")

#ANOVA test for categorical variable with target numeric variable

anova = aov(fare_amount ~ passenger_count, data = cab_fare)
summary(anova)





#-------------------------------------------------Normalization plot-----------------------------------------
#Normality check
qqnorm(cab_fare$fare_amount)
histogram(cab_fare$fare_amount)


# multicollearity test
library(usdm)
vifcor(numeric_data1, th = 0.9)

#----------------------------------------------Model Development----------------------------------------------
set.seed(1000)

#splitting into train and test
library(caret)
sp = createDataPartition(cab_fare[,"fare_amount"], p = .80, list = FALSE)
train = cab_fare[ sp,]
test  = cab_fare[-sp,]

#-------------------------------------------------Linear regression------------------------------------------
lm_model = lm(fare_amount ~.,data=train)

summary(lm_model)
str(train)
plot(lm_model$fitted.values,main = "Residual plot",
     xlab = "Predicted values of fare_amount",
     ylab = "standardized residuals")


lm_predictions = predict(lm_model,train)

#metrics
regr.eval(test[,1],lm_predictions)


#---------------------------------------------Decision Tree-------------------------------------------------
library(rpart)

dec_model = rpart(fare_amount ~ ., data = train, method = "anova")

summary(dec_model)

#Predict for new test cases
dec_predictions = predict(dec_model, test[,2:6])

#metrics
regr.eval(test[,1],dec_predictions)

#-----------------------------------------Random forest-----------------------------------------------------
library(randomForest)

rf_model = randomForest(fare_amount ~.,data=train)

summary(rf_model)

rf_predictions = predict(rf_model,test[,2:6])

regr.eval(test[,1],rf_predictions)

#-----------------------------------------------Tuning using XGBOOST-------------------------------------------------

#converting into matrix form
train_matrix = as.matrix(sapply(train[-1],as.numeric))
test_matrix = as.matrix(sapply(test[-1],as.numeric))

library(xgboost)

xgboost_model = xgboost(data = train_matrix,label = train$fare_amount,nrounds = 20,verbose =TRUE)
summary(xgboost_model)

xgb_predictions = predict(xgboost_model,test_matrix)

regr.eval(test[,1],xgb_predictions)
  

#--------------------------------------Finalizing and Saving the model-----------------------------------------------
# performance on original given data.
train_matrix2 = as.matrix(sapply(cab_fare[-1],as.numeric))
test_matrix2 = as.matrix(sapply(cab_fare_test,as.numeric))

xgboost_model2 = xgboost(data = train_matrix2,label = cab_fare$fare_amount,nrounds = 20,verbose = TRUE)

# Save the trained model
saveRDS(xgboost_model2, "./Xgboost_model_R.rds")

# load the saved model
saved_model <- readRDS("./Xgboost_model_R.rds")
print(saved_model)

# predict on test dataset
xgb = predict(saved_model,test_matrix2)

xgb_pred = as.data.frame(xgb)
colnames(xgb_pred) = c("predictions")

# Write the output 
write.csv(xgb_pred,"xgb_predictions_R.csv",row.names = FALSE)
