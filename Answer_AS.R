##Read the data 
getwd()
setwd("E:/Sita/BACP/R Data")
mydata=read.csv("Factor-Hair-Revised.csv",header=T)
mydata
attach(mydata)
names(mydata)
##Summary of the data
summary(mydata)
## To check the correlation
COR=cor(mydata)
#dimesnsions
dim(mydata)
#structure of the data
str(mydata)
##To check if any blank data is present
sum(is.na(mydata))
##Remove the first row since its ID values 
mydata=mydata[2:13]
# library DataExplorer for EDA and plots
library("DataExplorer")
library("corrplot")
corrplot(COR)
plot_histogram(mydata)
plot_density(mydata)
boxplot(mydata)
##Multicollinearity
library("car")
vif(lm(Satisfaction~.,data=mydata))
##Plot on multicolinearity
plot(mydata)
#Simple linear Regression
Model_ProdQ = lm(Satisfaction~ProdQual)
summary(Model_ProdQ)
Model_Ecom= lm(Satisfaction~Ecom)
summary(Model_Ecom)
Model_TechSup= lm(Satisfaction~TechSup)
summary(Model_TechSup)
Model_CR = lm(Satisfaction~CompRes)
summary(Model_CR)
Model_Adv = lm(Satisfaction~Advertising)
summary(Model_Adv)
Model_PL = lm(Satisfaction~ProdLine)
summary(Model_PL)
Model_SalesF = lm(Satisfaction~SalesFImage)
summary(Model_SalesF)
Model_ComP = lm(Satisfaction~ComPricing)
summary(Model_ComP)
Model_WC = lm(Satisfaction~WartyClaim)
summary(Model_WC)
Model_OB = lm(Satisfaction~OrdBilling)
summary(Model_OB)
Model_DS = lm(Satisfaction~DelSpeed)
summary(Model_DS)
#PCA/Factor Analysis
library(psych)
cortest.bartlett(cor(mydata[1:11]),100)
ev=eigen(cor(mydata[1:11]))
ev
EigenValue = ev$values
EigenValue
Factor=c(1,2,3,4,5,6,7,8,9,10,11)
Scree = data.frame(Factor,EigenValue)
plot(Scree,main="Scree plot",col="Blue",ylim=c(0,4))
lines(Scree,col="Red")
Unrotate = principal(mydata[1:11], nfactors=4, rotate="none")
print(Unrotate,digits=3)
UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings))
Rotate=principal(mydata[1:11],nfactors=4,rotate="varimax")
print(Rotate,digits=3)
RotatedProfile=plot(Rotate,row.names(Rotate$loadings),cex=1.0)
## data frame with four factors and dependent variable
as.data.frame(Rotate$scores)
mydata2=cbind(mydata[,12],Rotate$scores)
colnames(mydata2)=c("Satisfaction","PrchExp","BndRecog","AftSaSrvc","ProdFtr")
mydata2=as.data.frame(mydata2)
attach(mydata2)
#Multiple linear regression
Model1=lm(Satisfaction~PrchExp+BndRecog+AftSaSrvc+ProdFtr,mydata2)
print(summary(Model1),digits=3)
#Predict using new model
mydata3=predict(Model1)
mydata3 = as.data.frame(mydata3)
colnames(mydata3)=c("Pred_Satis")
mydata3=cbind(mydata2,mydata3)
mydata3$Pred_Satis=round(mydata3$Pred_Satis,2)
plot(mydata3$Satisfaction,col="Blue",xlab="IDs",ylab="Satisfaction")
lines(mydata3$Satisfaction,col="Blue")
plot(mydata3$Pred_Satis,col="Red")
lines(mydata3$Pred_Satis,col="Red")
