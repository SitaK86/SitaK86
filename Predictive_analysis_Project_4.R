# Loading the Packages ----------------------------------------------------

pacman::p_load(ggplot2,
               summarytools,
               esquisse,
               pscl,
               funModeling,
               ROCR,
               lntest,
               caret,
               Epi,
               e1071,
               corrplot,
               car,
               class,
               rsample,
               caTools,
               scales,
               data.table,
               plyr,
               formattable
)


# Setting Working Directory and Importing Data ----------------------------

setwd("E:/Sita/BACP/R Data")
mydata=read.csv("Cellphone.csv",header=T)
attach(mydata)

#Exploratory Data Analysis -----------------------------------------------
  
summarytools::view(dfSummary(mydata))

dim(mydata)

View(mydata)

str(mydata)

summary(mydata)

colSums(is.na(mydata))


### Univariate analysis

### Continuous variable

## Checking the outliers

ggplot(data = mydata, mapping = aes(y = AccountWeeks)) +
  geom_boxplot()

ggplot(data = mydata, mapping = aes(y = DataUsage)) +
  geom_boxplot()

ggplot(data = mydata, mapping = aes(y = DayMins)) +
  geom_boxplot()

ggplot(data = mydata, mapping = aes(y = DayCalls)) +
  geom_boxplot()

ggplot(data = mydata, mapping = aes(y = MonthlyCharge)) +
  geom_boxplot()

ggplot(data = mydata, mapping = aes(y = OverageFee)) +
  geom_boxplot()

### For Categorical Variable

ggplot(data = mydata) + geom_bar(mapping = aes(x = Churn))

xtabs(~Churn, data=mydata)

ggplot(data = mydata) + geom_bar(mapping = aes(x = ContractRenewal))

xtabs(~ContractRenewal, data=mydata)

ggplot(data = mydata) + geom_bar(mapping = aes(x = DataPlan))

xtabs(~DataPlan, data=mydata)

ggplot(data = mydata) + geom_bar(mapping = aes(x = CustServCalls))

xtabs(~CustServCalls, data=mydata)

### Bivariate analysis

### Continuous variable

ggplot(data = mydata, mapping = aes(x = Churn, y = AccountWeeks)) +
  geom_boxplot()

ggplot(data = mydata, mapping = aes(x = Churn, y = DataUsage)) +
  geom_boxplot()

ggplot(data = mydata, mapping = aes(x = Churn, y = DayMins)) +
  geom_boxplot()

ggplot(data = mydata, mapping = aes(x = Churn, y = DayCalls)) +
  geom_boxplot()

ggplot(data = mydata, mapping = aes(x = Churn, y = MonthlyCharge)) +
  geom_boxplot()

ggplot(data = mydata, mapping = aes(x = Churn, y = OverageFee)) +
  geom_boxplot()

### Bivariate analysis 
### find the significance of these variable with respect to target variable, using chi-square test)

ggplot(data = mydata) +  geom_count(mapping = aes(x = Churn, y = ContractRenewal))
mytable1 <- xtabs(~Churn+ContractRenewal, data=mydata)
ftable(mytable1)
summary(mytable1)

ggplot(data = mydata) +  geom_count(mapping = aes(x = Churn, y = DataPlan))
mytable2 <- xtabs(~Churn+DataPlan, data=mydata)
ftable(mytable2)
summary(mytable2)

ggplot(data = mydata) +  geom_count(mapping = aes(x = Churn, y = CustServCalls))
mytable3 <- xtabs(~Churn+CustServCalls, data=mydata)
ftable(mytable3)
summary(mytable3)

funModeling::plot_num(mydata)
funModeling::cross_plot(data = mydata, target = "Churn")
funModeling::correlation_table(data = mydata, "Churn")

#Multicollinearity--------------------

corr.matrix <- cor(mydata[,2:11])
corrplot(corr.matrix, main="\n\nCorrelation Plot`", method="number")
vif(lm(Churn~.,data=mydata))

#Data Plan & Data Usage, Day Mins & Monthly charges and highly correlated-----

mydata$MonthlyCharge= NULL
mydata$DataUsage=NULL
#mydata$DayMins=NULL
corr.matrix <- cor(mydata[,2:9])
corrplot(corr.matrix, main="\n\nCorrelation Plot`", method="number")
mydata$Churn=as.integer(mydata$Churn)
vif(lm(Churn~.,data=mydata))

mydata$Churn=as.factor(mydata$Churn)
mydata$ContractRenewal=as.factor(mydata$ContractRenewal)
mydata$DataPlan=as.factor(mydata$DataPlan)

# Creating Train and Test Data Set ----------------------------------------

set.seed(123)

split = sample.split(mydata$Churn, SplitRatio = 0.7)
traindata = subset(mydata, split == TRUE)
testdata = subset(mydata, split == FALSE)

table(mydata$Churn)
table(traindata$Churn)
table(testdata$Churn)

percent(prop.table(table(mydata$Churn)))
percent(prop.table(table(traindata$Churn)))
percent(prop.table(table(testdata$Churn)))

#Logistic Regression-------------

LR <- glm(Churn ~ ., data = traindata, family = 'binomial')

#Step 1: Data Validation for Logistic Regression
lmtest::lrtest(LR)

#Step 2: McFadden R Square
pscl::pR2(LR)

#Step 3: Test for individual Betas
print(summary(LR), digits = 6)

#Step 4: Explanatory Power of Odds and Intrepretation 
exp(coef(LR))
percent((exp(coef(LR)) / (1 + exp(coef(LR)))))

#Step 5: Prediction
(Prediction_Train <- predict(LR, traindata,type = "response"))

ROC_train = Epi::ROC(form = Churn ~.,data = traindata)
(cutoff <- floor(Prediction_Train > 0.165))
table(Actual = traindata$Churn, Predicted = cutoff)

cutoff=as.factor(cutoff)
class(cutoff)

Churn1=as.factor(traindata$Churn)

#Step 6 Model Validation

pred <- ROCR::prediction(Prediction_Train, traindata$Churn)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc <- performance(pred, "auc")
auc <- as.numeric(auc@y.values)
percent(auc)

#KS score
(train.ks=percent(max(attr(performance(pred, "tpr", "fpr"), "y.values")[[1]] - (attr(performance(pred, "tpr", "fpr"), "x.values")[[1]]))))

#GINI
(train.gini = (2 * (percent(auc))) - 1)

#Confusion Matrix
caret::confusionMatrix(cutoff, Churn1,positive="1")

#Prediction OF Testing Data
(Prediction_Test <- predict(LR, type = "response", newdata = testdata))

(cutoff <- floor(Prediction_Test > 0.165))
table(Actual = testdata$Churn, Predicted = cutoff)
cutoff=as.factor(cutoff)
class(cutoff)

Churn2=as.factor(testdata$Churn)

#Test Model Validation

pred <- ROCR::prediction(Prediction_Test, testdata$Churn)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
auc <- performance(pred, "auc")
auc <- as.numeric(auc@y.values)
percent(auc)

caret::confusionMatrix(cutoff, Churn2,positive ="1")

#KS score
(test.ks=percent(max(attr(performance(pred, "tpr", "fpr"), "y.values")[[1]] - (attr(performance(pred, "tpr", "fpr"), "x.values")[[1]]))))

#GINI
(test.gini = (2 * (percent(auc))) - 1)

#for knn scaling is must-----------------

normalize<-function(x){
  +return((x-min(x))/(max(x)-min(x)))}

data_norm <- as.data.frame(lapply(mydata[,c(2:9)], normalize))
summary(data_norm)

set.seed(1234)
pd<-sample(2,nrow(data_norm),replace=TRUE, prob=c(0.7,0.3))
train<-data_norm[pd==1,]
val<-data_norm[pd==2,]

#Data Frame for KNN
#Normalization is must

train_y <- mydata[pd==1,1]
train_y
head(train_y)
test_y <- mydata[pd==2,1]

#KNN 

pr <- knn(train = train,test = val,cl=train_y,k=7)

##create confusion matrix
tab <- table(pr,test_y)
tab
caret::confusionMatrix(pr,test_y,positive="1")
accuracy <- function(x){formattable::percent(sum(diag(x)/(sum(rowSums(x)))))}
accuracy(tab)

#NB
train_y<-as.factor(train_y)
test_y<-as.factor(test_y)
head(train)
NB<-naiveBayes(x=train, y=train_y)

y_pred.NB<-predict(NB,newdata=val)
y_pred.NB


#Confusion matrix
tab.NB=table(test_y,y_pred.NB)
tab.NB

accuracy.NB<-formattable::percent(sum(diag(tab.NB))/sum(tab.NB))
accuracy.NB
CM_NB=caret::confusionMatrix(y_pred.NB,test_y,positive="1")
CM_NB

#Variable Importance
varImp(LR)