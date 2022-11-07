#Libraries
library(ggplot2)
library(C50)
library(gmodels)
library(caret)
library(ROCR)
library(rpart)
library(nnet)
library(xgboost)
library(ROSE)

#Read in  and check working directory
setwd("C:/Users/alexa/OneDrive/Data Analytics/project")
getwd()

#Read in data-set csv file and check values
health <- read.csv("health_indicators.csv")
original_health <- read.csv("health_indicators.csv")
str(health)

#-------------Cleaning-----------------

#Change Column Names
colnames(health) <- c("HeartDisease",
                     "HighBloodPressure",
                     "HighCholestrol",
                     "CholestrolCheck",
                     "BMI",
                     "Smoker",
                     "Stroke",
                     "Diabetes",
                     "PhysicalActivity",
                     "Fruits",
                     "Vegetables",
                     "HighAlcoholConsmp",
                     "Healthcare",
                     "MedicalCost",
                     "GeneralHealth",
                     "MentalHealth",
                     "PhysicalHealth",
                     "WalkingDifficulty",
                     "Sex",
                     "Age",
                     "EducationLevel",
                     "IncomeLevel")


#Change Fields from integer to logical

health$HeartDisease <- as.factor(health$HeartDisease)
health$HighBloodPressure <- as.factor(health$HighBloodPressure)
health$HighCholestrol <- as.factor(health$HighCholestrol)
health$CholestrolCheck <- as.factor(health$CholestrolCheck)
health$Smoker <- as.factor(health$Smoker)
health$Stroke <- as.factor(health$Stroke)
health$PhysicalActivity <- as.factor(health$PhysicalActivity)
health$HighAlcoholConsmp <- as.factor(health$HighAlcoholConsmp)
health$Healthcare <- as.factor(health$Healthcare)
health$MedicalCost <- as.factor(health$MedicalCost)
health$WalkingDifficulty <- as.factor(health$WalkingDifficulty)

#Remove Columns 
health$Fruits <- NULL
health$Vegetables <- NULL

#Change Scale from 1-13 to Age Range

health$Age [health$Age=="1"] <- "18-24"
health$Age [health$Age=="2"] <- "25-29"
health$Age [health$Age=="3"] <- "30-34"
health$Age [health$Age=="4"] <- "35-39"
health$Age [health$Age=="5"] <- "40-44"
health$Age [health$Age=="6"] <- "45-49"
health$Age [health$Age=="7"] <- "50-54"
health$Age [health$Age=="8"] <- "55-59"
health$Age [health$Age=="9"] <- "60-64"
health$Age [health$Age=="10"] <- "65-69"
health$Age [health$Age=="11"] <- "70-74"
health$Age [health$Age=="12"] <- "75-79"
health$Age [health$Age=="13"] <- "80+"

health$Age=as.factor(health$Age)


#Change Diabetes from 0-2 to Diabetic Types

health$Diabetes [health$Diabetes=="0"] <- "Not Diabetic"
health$Diabetes [health$Diabetes=="1"] <- "Pre-Diabetic"
health$Diabetes [health$Diabetes=="2"] <- "Diabetic"

health$Diabetes=as.factor(health$Diabetes)


#Change General Health from 1-5 to Description

health$GeneralHealth [health$GeneralHealth=="1"] <- "Excellent"
health$GeneralHealth [health$GeneralHealth=="2"] <- "Good"
health$GeneralHealth [health$GeneralHealth=="3"] <- "OK"
health$GeneralHealth [health$GeneralHealth=="4"] <- "Poor"
health$GeneralHealth [health$GeneralHealth=="5"] <- "Very Poor"

health$GeneralHealth=as.factor(health$GeneralHealth)


#Change Sex from 0-1

health$Sex [health$Sex=="0"] <- "Female"
health$Sex [health$Sex=="1"] <- "Male"

health$Sex=as.factor(health$Sex)


#Check for missing values and display results
colSums(is.na(health))

#---------Splitting data set and applying ROSE sub sampling package-----------

#Partitioning the data set into Train and Test

set.seed(1234)
health.down<-ROSE(HeartDisease~.,data=health)$data
table(health.down$HeartDisease)

set.seed(1) 
sample <- sample.int(n = nrow(health.down), size = floor(.70*nrow(health.down)), replace = F)
health_train <- health.down[sample, ]
health_test  <- health.down[-sample, ]

#-------------Plotting-----------------

#Histogram of Age to Sex
ggplot(health) + aes(x=as.numeric(Age), fill=Sex, group=Sex) +
  geom_histogram(binwidth=1, color='black')

#Histogram of Count of Ages
ggplot(data = health.down, aes(x=Age,fill=HeartDisease))+geom_bar()

#Week 3 practical Box Plot
p <- ggplot(health.down, aes(HeartDisease, BMI))
p + geom_boxplot()

#-------------Training the data set-----------------

#ML algorithm C50

health_model <- C5.0(health_train[-1], health_train$HeartDisease, trials = 3)
health_model
summary(health_model)

health_pred <- predict(health_model, health_test, type = 'class')

CrossTable(x =health_test$HeartDisease, y=health_pred, prop.c = F,prop.r = F)

confusionMatrix(health_pred, health_test$HeartDisease)

#ML algorithm rpart

tree <- rpart(HeartDisease ~ ., data = health_train, minbucket = 1, minsplit=1, method = 'class', cp = 2e-4)

tree_pred <- predict(tree, health_test, type = 'class')

confusionMatrix(tree_pred, health_test$HeartDisease)

CrossTable(x =health_test$HeartDisease, y=tree_pred, prop.c = F,prop.r = F)

#ML algorithm nnet
nn1 <- nnet(HeartDisease ~ ., data = health_train, size=10, decay=1.0e-5, maxit=50)
nn1_pred <- predict(nn1, health_test)
pred1 <- rep('0', length(nn1._pred))
pred1[nn1.pred>=.4] <- '1'

confusionMatrix(as.factor(pred1),as.factor(health_test$HeartDisease))

CrossTable(x =health_test$HeartDisease, y=nn1_pred, prop.c = F,prop.r = F)

#ML algorithm XGBoost

X_train = data.matrix(health_train[,-1])             
y_train = health_train[,1]                                

X_test = data.matrix(health_test[,-1])                    
y_test = health_test[,1]                                   


xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)

model <- xgboost(data = xgboost_train,                      
                 max.depth=6,                            
                 nrounds=70)                              


boost_test <- predict(model, xgboost_test)

boost_test[(boost_test>6)] = 6
pred_y = as.factor((levels(y_test))[round(boost_test)])
confusionMatrix(y_test, pred_y)


#------------------ROCR graph---------------

#C5.0

health_pred  <- as.data.frame(health_pred)
c50_roc <- data.frame(health_pred, health_test$HeartDisease)
colnames(c50_roc) <- c("predict", "label", "HeartDisease")

pred_roc <- prediction(c50_roc$label, c50_roc$HeartDisease)

#rpart

tree_pred  <- as.data.frame(tree_pred)
rpart_roc <- data.frame(tree_pred, health_test$HeartDisease)
colnames(rpart_roc) <- c("predict", "label", "HeartDisease")

pred2_roc <- prediction(rpart_roc$label, rpart_roc$HeartDisease)


#nnet
nnet_pred  <- as.data.frame(nn1_pred)
nnet_roc <- data.frame(nn1_pred, health_test$HeartDisease)
colnames(nnet_roc) <- c("label", "HeartDisease")

pred3_roc <- prediction(nnet_roc$label, nnet_roc$HeartDisease)

#xgboost
boost_pred  <- as.data.frame(boost_test)
xgboost_roc <- data.frame(boost_test, health_test$HeartDisease)
colnames(xgboost_roc) <- c("label", "HeartDisease")

pred4_roc <- prediction(xgboost_roc$label, xgboost_roc$HeartDisease)

#Plotting Curve
perf <- performance(pred_roc, measure = "tpr", x.measure = "fpr")
perf2 <- performance(pred2_roc, measure = "tpr", x.measure = "fpr")
perf3 <- performance(pred3_roc, measure = "tpr", x.measure = "fpr")
perf4 <- performance(pred3_roc, measure = "tpr", x.measure = "fpr")

plot(perf,main = "ROC curve", col = "blue", lwd = 2)
plot(perf2, add = TRUE, col = "red", lwd = 2)
plot(perf3, add = TRUE, col = "green", lwd = 2)
plot(perf4, add = TRUE, col = "green", lwd = 2)

# add a reference line to the graph
abline(a = 0, b = 1, lwd = 2, lty = 2)

#AUC calculation
perf.auc <- performance(pred_roc, measure = "auc")
str(perf.auc)
as.numeric(perf.auc@y.values)

perf2.auc <- performance(pred2_roc, measure = "auc")
str(perf2.auc)
as.numeric(perf2.auc@y.values)

perf3.auc <- performance(pred3_roc, measure = "auc")
str(perf3.auc)
as.numeric(perf3.auc@y.values)

perf4.auc <- performance(pred4_roc, measure = "auc")
str(perf4.auc)
as.numeric(perf4.auc@y.values)


