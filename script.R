install.packages("caret")
install.packages("ggplot2")
install.packages("C50")
install.packages("gmodels")
install.packages("ROCR")
install.packages("rpart")
install.packages("nnet")
install.packages("xgboost")
install.packages("ROSE")

table(health$HeartDisease)

#Histogram of Heart disease by Age
ggplot(health) + aes(x=as.numeric(HeartDisease), group=Age, fill=Age) +
geom_histogram(binwidth=1, color='blue')

health$Fruits <- as.factor(health$Fruits)
health$Vegetables <- as.factor(health$Vegetables)


#Down Sample
True = which(health$HeartDisease=="1")
False = which(health$HeartDisease=="0")
False.downsample <- sample(False,length(True))
health.down <- health[c(False.downsample,True),]


#Bar plot for Age on Heart Disease
count <- table(health[health$Age == '18-24',]$HeartDisease)["TRUE"]
count <- c(count, table(health[health$Age == '25-29',]$HeartDisease)["TRUE"])
count <- c(count, table(health[health$Age == '30-34',]$HeartDisease)["TRUE"])
count <- c(count, table(health[health$Age == '35-39',]$HeartDisease)["TRUE"])
count <- c(count, table(health[health$Age == '40-44',]$HeartDisease)["TRUE"])
count <- c(count, table(health[health$Age == '45-49',]$HeartDisease)["TRUE"])
count <- c(count, table(health[health$Age == '50-54',]$HeartDisease)["TRUE"])
count <- c(count, table(health[health$Age == '55-59',]$HeartDisease)["TRUE"])
count <- c(count, table(health[health$Age == '60-64',]$HeartDisease)["TRUE"])
count <- c(count, table(health[health$Age == '65-69',]$HeartDisease)["TRUE"])
count <- c(count, table(health[health$Age == '70-74',]$HeartDisease)["TRUE"])
count <- c(count, table(health[health$Age == '75-79',]$HeartDisease)["TRUE"])
count <- c(count, table(health[health$Age == '80+',]$HeartDisease)["TRUE"])
count <- as.numeric(count)

scale_x_discrete(name = "BMI")+
  scale_fill_discrete(name = "Heart Disease", labels = c("False", "True")) + 
  ggtitle("General Health to Heart Disease")+
  theme(plot.title = element_text(hjust = 0.5))
