Data1<-read.csv(file.choose())
str(Data1)
xtabs(~ Outcome+DiabetesPedigreeFunction ,data=Data1)
#logistic regression model

logistic1<-glm(Outcome ~ ., data= Data1,family = "binomial")
logistic1
summary(logistic1)

exp(coef(logistic1))
# Confusion matrix table 
prob <- predict(logistic1,type=c("response"),Data1)
prob
confusion<-table(prob>0.5,Data1$Outcome)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
library("prediction")
# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,Data1$Outcome)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
