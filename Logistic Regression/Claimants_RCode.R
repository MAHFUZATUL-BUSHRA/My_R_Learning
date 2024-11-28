claimants <- read.csv(file.choose()) # Choose the claimants Data set
View(claimants)
attach(claimants)

# Linear Regression
fit<-lm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+LOSS)
summary(fit)
# Linear regression technique can not be employed

# Logistic Regression 
logit<-glm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+LOSS,family=binomial,data = claimants)
summary(logit)

logit1<-glm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+CLMAGE+LOSS,family=binomial,data = claimants)
summary(logit)


# Odds Ratio
exp(coef(logit))

# Confusion matrix table 
prob <- predict(logit1,type=c("response"),claimants)
prob
confusion<-table(prob>0.5,claimants$ATTORNEY)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

library("prediction")

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,claimants$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
