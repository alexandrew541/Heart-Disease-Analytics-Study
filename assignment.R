
#-----------------Heart Disease Analysis-----------------------

#Install Libraries
install.packages("caret")
install.packages("ggplot2")
install.packages("C50")
install.packages("gmodels")
install.packages("ROCR")
install.packages("rpart")
install.packages("nnet")
install.packages("xgboost")
install.packages("ROSE")

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
library(scales)

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
                     "HighCholesterol",
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


#Change Field types to factors

health$HeartDisease <- as.factor(health$HeartDisease)
health$HighBloodPressure <- as.factor(health$HighBloodPressure)
health$HighCholesterol <- as.factor(health$HighCholesterol)
health$CholestrolCheck <- as.factor(health$CholestrolCheck)
health$Smoker <- as.factor(health$Smoker)
health$Stroke <- as.factor(health$Stroke)
health$PhysicalActivity <- as.factor(health$PhysicalActivity)
health$HighAlcoholConsmp <- as.factor(health$HighAlcoholConsmp)
health$Healthcare <- as.factor(health$Healthcare)
health$MedicalCost <- as.factor(health$MedicalCost)
health$WalkingDifficulty <- as.factor(health$WalkingDifficulty)
health$EducationLevel <- as.factor(health$EducationLevel)
health$BMI <- as.factor(health$BMI)


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


#Income Level Change
health$IncomeLevel [health$IncomeLevel=="1"] <- "<$10000"
health$IncomeLevel [health$IncomeLevel=="2"] <- ">=$10000"
health$IncomeLevel [health$IncomeLevel=="3"] <- ">=$25000"
health$IncomeLevel [health$IncomeLevel=="4"] <- ">=$35000"
health$IncomeLevel [health$IncomeLevel=="5"] <- ">=$45000"
health$IncomeLevel [health$IncomeLevel=="6"] <- ">=$55000"
health$IncomeLevel [health$IncomeLevel=="7"] <- ">=$65000"
health$IncomeLevel [health$IncomeLevel=="8"] <- ">=$75000"

health$IncomeLevel=as.factor(health$IncomeLevel)

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

#-------------Data set arrangement-----------

#Applying ROSE sub sampling 
set.seed(1234)
health<-ROSE(HeartDisease~.,data=health)$data

#Comparison between sub sampled data and the original
table(health$HeartDisease)
table(original_health$HeartDiseaseorAttack)

#Partitioning the data set into Train and Test
set.seed(1) 
sample <- sample.int(n = nrow(health), size = floor(.70*nrow(health)), replace = F)
health_train <- health[sample, ]
health_test  <- health[-sample, ]

str(health_train)
#-------------Method Graph Plots-----------------

#Heart disease label
health_label <- c("False", "True")

#Plot unchanged Heart disease
ggplot(data = original_health, aes(x=as.factor(HeartDiseaseorAttack),
                                   fill= as.factor(HeartDiseaseorAttack)))+ 
  geom_bar()+ scale_y_continuous(breaks = seq(0, 240000, 20000), lim = c(0, 240000))+
  scale_x_discrete(labels= health_label, "Heart Disease") + 
  scale_fill_discrete(name = "Heart Disease", labels = c("False", "True"))+ 
  ggtitle("Unsampled Heart Disease")+
  theme(plot.title = element_text(hjust = 0.5))

#Plot Changed Heart Disease
ggplot(data = health, aes(x=HeartDisease,fill=HeartDisease))+geom_bar() +
  scale_y_continuous(breaks = seq(0, 130000, 20000), lim = c(0, 130000))+
  scale_x_discrete(labels= health_label, "Heart Disease") + 
  scale_fill_discrete(name = "Heart Disease", labels = c("False", "True")) + 
  ggtitle("Sampled Heart Disease")+
  theme(plot.title = element_text(hjust = 0.5))


#----------------Discussion Graphs------------------


#Histogram of Count of Ages to heart disease
ggplot(data = health, aes(x=Age,fill=HeartDisease))+geom_bar()+
  scale_y_continuous(breaks = seq(0, 40000, 5000), lim = c(0, 40000))+
  scale_fill_discrete(name = "Heart Disease", labels = c("False", "True")) + 
  ggtitle("Age to Heart Disease")+
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=16))+ 
  geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            position = position_stack(vjust = 0.5), size = 4, stat = "count")


#Histogram of diabetes to heart disease
ggplot(data = health, aes(x=Diabetes,fill=HeartDisease))+geom_bar()+
  scale_y_continuous(breaks = seq(0, 200000, 20000), lim = c(0, 200000))+
  scale_fill_discrete(name = "Heart Disease", labels = c("False", "True")) + 
  ggtitle("Diabetic status to Heart Disease")+
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16))+ 
  geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            position = position_stack(vjust = 0.5), size = 4, stat = "count")


#Histogram of general health to heart disease
ggplot(data = health, aes(x=GeneralHealth,fill=HeartDisease))+geom_bar()+
  scale_x_discrete(name = "General Health")+
  scale_y_continuous(breaks = seq(0, 80000, 5000), lim = c(0, 80000))+
  scale_fill_discrete(name = "Heart Disease", labels = c("False", "True")) + 
  ggtitle("General Health to Heart Disease")+
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=16))+ 
  geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            position = position_stack(vjust = 0.5), size = 4, stat = "count")

#Relationship between blood pressure and cholesterol
ggplot(data = health) +
  geom_bar(aes(x=HighBloodPressure, fill=HighCholesterol),
           position = "dodge") +
  facet_wrap(~HeartDisease, labeller = labeller(HeartDisease = c("0" = "Does not have Heart Disease", "1" = "Has Heart Disease")))+ 
  ggtitle("High Cholestrol, Blood Pressure to Heart Disease")+
  scale_x_discrete(name ="High Blood Pressure", labels=c("0" = "False","1" = "True"))+
  scale_y_continuous(breaks = seq(0, 80000, 10000), lim = c(0, 80000))+
  scale_fill_discrete(name = "High Cholestrol", labels = c("False", "True")) + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=16))

#Relationship between smoking, drinking and heart disease
ggplot(data = health) +
  geom_bar(aes(x=HighAlcoholConsmp, fill=Smoker),
           position = "dodge") +
  facet_wrap(~HeartDisease, labeller = labeller(HeartDisease = c("0" = "Does not have Heart Disease", "1" = "Has Heart Disease")))+ 
  ggtitle("Smoking, Alcohol Consumption to Heart Disease")+
  scale_x_discrete(name ="High Alcohol Consumption", labels=c("0" = "False","1" = "True"))+
  scale_y_continuous(breaks = seq(0, 80000, 10000), lim = c(0, 80000))+
  scale_fill_discrete(name = "Smoker", labels = c("False", "True")) + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=16))


#BMI Histogram
ggplot(data = health, aes(x=BMI,fill=HeartDisease))+geom_bar()+
  scale_y_continuous(breaks = seq(0, 30000, 5000), lim = c(0, 30000))+
  scale_x_discrete(name = "BMI", breaks = seq(12, 96, 6))+
  scale_fill_discrete(name = "Heart Disease", labels = c("False", "True")) + 
  ggtitle("BMI to Heart Disease")+
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=16))


#Histogram of Sex to Heart disease
ggplot(data = health, aes(x=Sex,fill=HeartDisease))+geom_bar()+
  scale_y_continuous(breaks = seq(0, 140000, 20000), lim = c(0, 140000))+
  scale_fill_discrete(name = "Heart Disease", labels = c("False", "True")) + 
  ggtitle("Sex to Heart Disease")+
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=16))+ 
  geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            position = position_stack(vjust = 0.5), size = 4, stat = "count")

#-------------Training the data set-----------------

#ML algorithm C50

health_model <- C5.0(health_train[-1], health_train$HeartDisease, trials = 3)

health_pred <- predict(health_model, health_test, type = 'class')

CrossTable(x =health_test$HeartDisease, y=health_pred, prop.c = F,prop.r = F)

confusionMatrix(health_pred, health_test$HeartDisease)

#ML algorithm rpart

part <- rpart(HeartDisease ~ ., data = health_train, minbucket = 1, minsplit=1, 
              method = 'class', cp = 2e-4)

part_pred <- predict(part, health_test, type = 'class')

CrossTable(x =health_test$HeartDisease, y=part_pred, prop.c = F,prop.r = F)

confusionMatrix(part_pred, health_test$HeartDisease)

#Neural Networks
nn <- nnet(HeartDisease ~ ., data = health_train, size=10, maxit=200, MaxNWts = 5000)

nn_pred <- predict(nn, health_test, type = 'raw')

pred1 <- rep('0', length(nn_pred))

pred1[nn_pred>=.4] <- '1'

CrossTable(x =health_test$HeartDisease, y=pred1, prop.c = F,prop.r = F)

confusionMatrix(as.factor(pred1),as.factor(health_test$HeartDisease))

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


boost_test <- predict(model, xgboost_test, type = 'class')

boost_test[(boost_test>6)] = 6
pred_y = as.factor((levels(y_test))[round(boost_test)])

CrossTable(x =y_test, y=pred_y, prop.c = F,prop.r = F)

confusionMatrix(y_test, pred_y)


#------------------ROCR graph---------------

#C5.0
health_pred <- predict(health_model, health_test, type = 'prob')

health_pred  <- as.data.frame(health_pred)
c50_roc <- data.frame(health_pred, health_test$HeartDisease)
colnames(c50_roc) <- c("predict", "label", "HeartDisease")

pred_roc <- prediction(c50_roc$label, c50_roc$HeartDisease)

#rpart
part_pred <- predict(part, health_test, type = 'prob')

part_pred  <- as.data.frame(part_pred)
rpart_roc <- data.frame(part_pred, health_test$HeartDisease)
colnames(rpart_roc) <- c("predict", "label", "HeartDisease")

pred2_roc <- prediction(rpart_roc$label, rpart_roc$HeartDisease)

#xgboost
boost_test <- predict(model, xgboost_test, type = 'prob')

boost_pred  <- as.data.frame(boost_test)
xgboost_roc <- data.frame(boost_test, health_test$HeartDisease)
colnames(xgboost_roc) <- c("label", "HeartDisease")

pred3_roc <- prediction(xgboost_roc$label, xgboost_roc$HeartDisease)

#nnet
nnet_pred  <- as.data.frame(nn_pred)
nnet_roc <- data.frame(nn_pred, health_test$HeartDisease)
colnames(nnet_roc) <- c("label", "HeartDisease")

pred4_roc <- prediction(nnet_roc$label, nnet_roc$HeartDisease)

#Plotting Curve
perf <- performance(pred_roc, measure = "tpr", x.measure = "fpr")
perf2 <- performance(pred2_roc, measure = "tpr", x.measure = "fpr")
perf3 <- performance(pred3_roc, measure = "tpr", x.measure = "fpr")
perf4 <- performance(pred4_roc, measure = "tpr", x.measure = "fpr")

plot(perf,main = "ROC curve", col = "blue", lwd = 2 ,
     xlab="False Positive Rate", ylab="True Positive Rate")
plot(perf2, add = TRUE, col = "red", lwd = 2)
plot(perf3, add = TRUE, col = "green", lwd = 2)
plot(perf4, add = TRUE, col = "black", lwd = 2)

#Graph parameters
abline(a = 0, b = 1, lwd = 1.5, lty = 6)
grid (10,10, lty = 6, col = "gray")
legend("bottomright", inset=.02, title="Classifiers",
       c("C5.0","XGBoost","Neural Network","RPART"), 
       fill = c("blue", "green", "black", "red"), cex=0.8)

#AUC calculations

#C5.0
perf.auc <- performance(pred_roc, measure = "auc")
as.numeric(perf.auc@y.values)

#RPART
perf2.auc <- performance(pred2_roc, measure = "auc")
as.numeric(perf2.auc@y.values)

#XGBoost
perf3.auc <- performance(pred3_roc, measure = "auc")
as.numeric(perf3.auc@y.values)

#NNET
perf4.auc <- performance(pred4_roc, measure = "auc")
as.numeric(perf4.auc@y.values)

#Combine results into a table
auc_comb <- rbind(perf.auc@y.values,
                  perf2.auc@y.values,
                  perf3.auc@y.values,
                  perf4.auc@y.values)
rownames(auc_comb) <- (c('C5.0', 'RPART', 'XGBoost', 'Neural Networks'))
colnames(auc_comb) <- 'Area Under Curve'
auc_comb
