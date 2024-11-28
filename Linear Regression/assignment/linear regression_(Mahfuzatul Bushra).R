D2<-read.csv(file.choose())
Data1<-read.csv(file.choose())
summary(Data1)
attach(Data1)
#clean dataset
as.data.frame(Data1)
na.omit(Data1)->Data1

#Checking the dataset 
complete.cases(Data1)
#visualize & relationship between variables
plot(Data1)
plot(price~carlength+carwidth+carheight+horsepower,data=Data1,col="red",pch=25,cex=1.7)
#.............
str(Data1)
coef(Data1)
library(ggplot2)
ggplot(data=Data1,aes(x=price)) +geom_histogram()
#correlation
library(MASS)
library(corrplot)
library(quantmod)
D2<-Data1[,4:8]
pairs(D2)
cor(D2)

#Check Collinearity
ivar<-Data1[,4:7]
ivarcor<-cor(ivar)
ivaricor<-ginv(ivarcor)
colnames(ivarcor)<-colnames((ivar))
rownames(ivarcor)<-colnames((ivar))
corrplot(ivarcor,method="number",is.corr=F)


#Splitting The data into training and testing dataset
library(caTools)
sample.split(Data1$price,SplitRatio = 0.75)->split_tag
subset(Data1,split_tag==T) -> train
subset(Data1,split_tag==F) -> test
nrow(train)
ncol(test)

#multiple linear regression model
lm(price~carlength+carwidth+carheight+horsepower,data = train)->LM2
LM2
summary(LM2)
# Prediction based on only carlength
model1<-lm(price~carlength)
summary(model1) 

# Prediction based on only carwidth
model2<-lm(price~carwidth)
summary(model2) 


# Prediction based on only carheight
model3<-lm(price~carheight)
summary(model3) 

# Prediction based on only horsepower
model4<-lm(price~horsepower)
summary(model4) 

#Horsepower is more significant than other variables

avPlots(LM2,id.n=2,id.cex=0.7)
qqPlot(LM2,id.n = 5)
#Calculating Vif
library(car)
vif(LM2)
#Test and train dataset
predict(LM2,data= test)->my_result2
my_result2
cbind(Actual= test$price, Predicted= my_result2) -> final_data2
final_data2
as.data.frame(final_data2)->final_data2
final_data2
(final_data2$Actual-final_data2$Predicted)->error2
error2
cbind(final_data2,error2)->final_data2
final_data2

sqrt(mean((final_data$error2)^2))->error2
error2
