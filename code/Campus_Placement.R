#Add some functions to use for analysis from server
source("http://bigblue.depaul.edu/jlee141/econdata/R/func_lib.R") 

#Import Data
Placement_data <- read.csv("/Users/rosenguyen/Documents/Classes/ECO520/Placement_Data_Full_Class.csv") 
#library
library(dplyr)

#Descriptive Analytics
str(Placement_data)
head(Placement_data)
tail(Placement_data)
summary(Placement_data)

#find missing values
Placement <- subset(Placement_data, is.na(salary) | salary < 900000)

#data without unnecessary variables
Placement <- subset(Placement, select=-c(sl_no))

#Categorical Variables
table(Placement$gender)
table(Placement$ssc_b)
table(Placement$hsc_b)
table(Placement$hsc_s)
table(Placement$degree_t)
table(Placement$workex)
table(Placement$specialisation)
table(Placement$status)

#Convert Categorical Variables into Dummy Variables
Placement$gender <- ifelse(Placement$gender=="F",1,0)
Placement$ssc_b <- ifelse(Placement$ssc_b =="Central",1,0)
Placement$hsc_b <- ifelse(Placement$hsc_b =="Central",1,0)
Placement$workex <- ifelse(Placement$workex =="Yes",1,0)
Placement$specialisation <- ifelse(Placement$specialisation =="Mkt&Fin",1,0)
Placement$status <- ifelse(Placement$status =="Placed",1,0)
Placement$hsc_s <- ifelse(Placement$hsc_s == "Arts", 1, ifelse(Placement$hsc_s == "Commerce", 2, 0))
Placement$degree_t <- ifelse(Placement$degree_t == "Comm&Mgmt", 1, ifelse(Placement$degree_t == "Sci&Tech",2,0))

str(Placement)

#factor variables
Placement$gender <- as.factor(Placement$gender)
Placement$ssc_b <- as.factor(Placement$ssc_b)
Placement$hsc_b <- as.factor(Placement$hsc_b)
Placement$hsc_s <- as.factor(Placement$hsc_s)
Placement$degree_t <- as.factor(Placement$degree_t)
Placement$workex <- as.factor(Placement$workex)
Placement$specialisation <- as.factor(Placement$specialisation)
str(Placement)

#Dummy Variables
table(Placement$gender)
table(Placement$ssc_b)
table(Placement$hsc_b)
table(Placement$hsc_s)
table(Placement$degree_t)
table(Placement$workex)
table(Placement$specialisation)
table(Placement$status)

##  Regression Models using Train Data set 
## Model 1: salary= b0 + b1*degree_t + e
mod1 <- lm(salary ~ degree_t, data=Placement)
summary(mod1)

## Model 2: using stepwise to remove all the insignificant variables 
mod2 <- step(lm(salary ~., data=Placement, direction = "both"))
summary(mod2)

## Residuals of 3 models 
pe1 <- residuals(mod1, newdata=Placement)
pe2 <- residuals(mod2, newdata=Placement)

mse1 <- mean(pe1^2)
mse2 <- mean(pe2^2)
print(c(mse1,mse2))

rmse1 <- mse1^0.5
rmse2 <- mse2^0.5
print(c(rmse1,rmse2))


#Remove salary dependent variable 
Placement_status<- subset(Placement, select=-c(salary))

#Split data set into train and test data
set.seed(2111845)
train_idx <- sample(nrow(Placement_status), nrow(Placement_status)*0.8)
train <- Placement_status[train_idx,]
test <- Placement_status[-train_idx,]
testy <- test$status

library(ROCR)
#Linear Probability Model
## Model 1: lpm0
lpm0 <- lm(status~ssc_p+hsc_p+degree_p+etest_p+mba_p, data=train)
summary(lpm0)
###lpm0 - Adjusted R-squared:  0.5017 
yhat0 <- predict(lpm0, newdata = test)
auc_plot(yhat0, testy, "LPM")
###AUC=0.93214

##Model 2: lpm1
lpm1<- step(lm(status~., data=train), direction = "both")
summary(lpm1)
##lpm1 <- Adjusted R-squared:  0.5651
yhat1 <- predict(lpm1, newdata=test)
auc_plot(yhat1, testy, "LPM")
##AUC=0.95582
conf_table(yhat1, testy, "LPM")

##Final decision
dec_lpm <- ifelse(yhat1>=0.3, 1,0)
table(testy, dec_lpm)

#Logistic Model

##Model 1: logit0
logit0 <- glm(status~ssc_p+hsc_p+degree_p+etest_p+mba_p, data=train, family= binomial(link = logit))
loghat0 <- predict(logit0, newdata = test, type="response")
###AUC = 0.93204

##Final decision
dec_logit <- ifelse(yhat1>=0.3, 1,0)
table(testy, dec_lpm)

##Model 2: logit1
logit1 <- step(glm(status~., data = train, family = binomial(link=logit)), direction = "both")
loghat1 <- predict(logit1, newdata = test, type = "response")
auc_plot(loghat1, testy, "LOGIT")
###AUC = 0.95961

conf_table(loghat1, testy, "LOGIT")

#Final Decision
dec_logit <- ifelse(loghat1>=0.2,1,0)
table(testy, dec_logit)

#Random Forest Model
train$status <- as.factor(train$status)
test$status <- as.factor(test$status)
if(!(require(randomForest))) install.packages("randomForest")
rf1 <- randomForest(formula=status~., data=train, mtry=5, ntree=500)
summary(rf1)
rfhat1 <- predict(rf1, newdata=test, type="prob")
rfhat1 <- rfhat1[,2]
auc_plot(rfhat1, testy, "RANDOMFOREST")
###AUC=1
conf_table(rfhat1, testy, "RANDOMFOREST")
###Final Decision
dec_rf <- ifelse(rfhat1 >= 0.1,1,0)
table(testy, dec_rf)

#Neural Network Analysis
##install.packages(neuralnet)
library(neuralnet)
Placement_status[] <- lapply(Placement_status, as.numeric)
str(Placement_status)
zplacement_status<- min_max_nor(Placement_status)

##split dataset
#Split Train and Test
zplacement_status_idx <- sample(nrow(zplacement_status), round(0.7*nrow(zplacement_status)))
ztrain <- zplacement_status[zplacement_status_idx,]
ztest <- zplacement_status[-zplacement_status_idx,]
ztesty <- ztest$status

##Model 1: nnet1
ztrain$status <- as.factor(ztrain$status)
nnet1 <- neuralnet(status~., data=ztrain, hidden=c(3), stepmax=1e+06)
plot(nnet1)
pred1 <- predict(nnet1, newdata= ztest)
pred1 <- pred1[,2]
auc_plot(pred1, ztesty, "Neural Network")
###AUC= 0.9603

##Final Decision
conf_table(pred1, ztesty, "Neural Network")
dec_nn <- ifelse(pred1 >=0.1,1,0)
table(ztesty, dec_nn)

#Combine and Compare Models

##Combine Graphs
par(mfrow=c(2,2))
plot(testy, yhat1)
plot(testy, loghat1)
plot(testy, rfhat1)
plot(ztesty, pred1)

##Find the model with the lowest error
fit_eval(yhat1, testy, "Linear Progression")
fit_eval(loghat1, testy, "Logit")
fit_eval(rfhat1, testy, "Random Forest")
fit_eval(pred1, ztesty, "Neural Network")



