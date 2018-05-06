
##----------------------------------------------------------------------------------##
##--- SIMPLE LINEAR REGRESSION MODEL building on BigMac dataset - ASSIGNMENT -------##
##----------------------------------------------------------------------------------##


##--- Step 1: Clear environment variables ------------------------------------------##
rm(list=ls(all=TRUE))

##__________________________________________________________________________________##


##--- Step 2: Set working Directory ------------------------------------------------##
setwd("E:\\INSOFE_CPEE\\Wk5\\02102017_Day01\\20170902_Batch32_CSE7302c_Lab01_SimpleLinReg")

##__________________________________________________________________________________##


##--- Step 3: Read the data from the csv file --------------------------------------##
data = read.csv("BigMac-NetHourlyWage.csv",header = TRUE)

##__________________________________________________________________________________##


##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop any irrelevant attributes attribute:
data = data[,-1]

## Summary of the data and look for any missing values:
summary(data)


## Correlation and Covariance between the attributes:
cov(data)
names(data)
plot(data$Big.Mac.Price....,data$Net.Hourly.Wage...., xlab = "Big.Mac.Price....", ylab = "Net.Hourly.Wage....", pch = 18, col = "blue")

#Describe how the covarainace and correlation coefficients are the BigMac dataset

#Covariance between Big.Mac.Price and Net.Hourly.Wage is 5.092068. This indicates a positive linear relationship, With increasing Big Mac Price, Net hourly wage also increases

#Do the attributes have a good enough correlation coefficient to support linear regression model building?
cor(data)
cor(data$Big.Mac.Price....,data$Net.Hourly.Wage....) #0.71705
#Yes the attributes have good corelation coefficient to build linear regression model.
# Corelation coefficient indicates stregth and direction of the relationship. This can be observed in the scatter plot. Between the two variable, co-relation coefficient r = 0.71705, this indicates the stregth is quite good and direction is positive.

##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (80:20) ratio
rows = seq(1, nrow(data),1) # data is assembled as sequence of data
dim(rows) #NULL, as it is one dimentional vector
length(rows) #27 , same as data

set.seed(123)
trainRows = sample(rows,(70*nrow(data))/100) 
data_train = data[trainRows,] 
data_test = data[-trainRows,]

data_train
data_test

##__________________________________________________________________________________##


##--- Step 6: Linear regression model building--------------------------------------##
names(data_train)
data_model = lm(formula = Net.Hourly.Wage....~ Big.Mac.Price....,data_train )


## Summary of model:
summary(data_model)

#Extract the intercept coefficient from the linear regression model
coefficients(data_model)

#Extract the residual values
data_model$residuals

##__________________________________________________________________________________##


##--- Step 7: Check for validity of linear regression assumptions ------------------##
#plot the 4 graphs to check
par(mfrow =c(2,2))
plot(data_model)

#Plot 1 - Residuals Vs Fitted Values - The plot is the test for linear relationship. The dotted line at y=0 indicates the fit line. Residual poinst above this line are positive and those below are negative.The red-line indicates a linear relationshoip between error items (residuals) and Fitted values (predicted values)
#Plot 2 - Normal Q-Q - The plot is the test for normal distribution of residuals. As the data points follow the dotted line closely, the model passes the test of normality.
#Plot 3 - This plot verifies homoscedacity. There is no pattern in Fiited values Vs sqrt(Standardised residuals), hence the model is not heteroscadastic.
#Plot 4 This plot helps to identify the points of high influence, as there are no points beyond curve marked 1 , there are no influencial points at Cooks distance >1.


##__________________________________________________________________________________##


##--- Step 8: Predict on testdata --------------------------------------------------##

test_prediction = predict(object = data_model,newdata = data_test)


##__________________________________________________________________________________##


##--- Step 9: Error Metrics --------------------------------------------------------##
library(DMwR)


#Error verification on train data
regr.eval(data_train$Net.Hourly.Wage....,data_model$fitted.values)

#mae        mse       rmse       mape 
#3.3862698 18.4228156  4.2921807  0.5818848

#Error verification on test data
regr.eval(data_test$Net.Hourly.Wage....,test_prediction)
#mae        mse       rmse       mape 
#3.1742993 12.8148391  3.5797820  0.9209434

#As most of errors (excluding mape), are reducing in test compaired to tarin, the model is good

##__________________________________________________________________________________##


##--- Step 10: Confidence and Prediction Intervals----------------------------------##
#Find the confidence and prediction intervals and plot them for the WHOLE dataset
conf_Pred = data.frame(predict(data_model, data_test, interval="confidence",level=0.95))
conf_Pred
pred_Pred = data.frame(predict(data_model, data_test, interval="prediction",level=0.95))
pred_Pred

names(data_test)
plot(data_test$Big.Mac.Price...., data_test$Net.Hourly.Wage...., xlab = "Big Mac Price", ylab = "Net Hourly Wage")

#overlays points
points(data_test$Big.Mac.Price....,conf_Pred$fit,type="l", col="green", lwd=2)
points(data_test$Big.Mac.Price....,conf_Pred$lwr,pch="-", col="red", lwd=3)
points(data_test$Big.Mac.Price....,conf_Pred$upr,pch="-", col="red", lwd=3)
points(data_test$Big.Mac.Price....,pred_Pred$lwr,pch="-", col="blue", lwd=3)
points(data_test$Big.Mac.Price....,pred_Pred$upr,pch="-", col="blue", lwd=3)





##__________________________________________________________________________________##
#-----------------------end---------------------------------------------------------##