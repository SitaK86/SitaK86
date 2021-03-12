cart.data<- read.table(file.choose(), sep = ",", header = T)

dim(cart.data)

colSums(is.na(cart.data))

summary(cart.data)

str(cart.data)

##Splitting the data

library(rpart)
library(rpart.plot)
library(caret)

sampledata <- createDataPartition(cart.data$Claimed,p=0.7,list=F)

cart.dev <- cart.data[sampledata,]
cart.holdout <- cart.data[-sampledata,]
c(nrow(cart.dev), nrow(cart.holdout))

###CART Model

## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 5)

m1 <- rpart(formula = Claimed ~ ., data = cart.dev, method = "class", control = r.ctrl)

rpart.plot(m1)

printcp(m1)
plotcp(m1)

## Pruning Code
ptree<- prune(m1, cp= 0.0077280,"CP")
printcp(ptree)
rpart.plot(ptree)


## Scoring syntax
cart.dev$predict.class <- predict(ptree, cart.dev, type="class")
predict(ptree, cart.dev)

cart.dev$predict.score <- predict(ptree, cart.dev)[,2]

## Scoring Holdout sample
cart.holdout$predict.class <- predict(ptree, cart.holdout, type="class")
cart.holdout$predict.score <- predict(ptree, cart.holdout)[,2]


#checking the performance of the model
library(ROCR)
pred <- ROCR::prediction(cart.dev$predict.score, cart.dev$Claimed)
perf <- ROCR::performance(pred, "tpr", "fpr")
plot(perf)

auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc


confusionMatrix(cart.dev$predict.class,cart.dev$Claimed,mode="everything")


##Checking the performance of the predicted model


pred <- ROCR::prediction(cart.holdout$predict.score, cart.holdout$Claimed)
perf <- ROCR::performance(pred, "tpr", "fpr")
plot(perf)
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

confusionMatrix(cart.holdout$predict.class,cart.holdout$Claimed,mode = "everything")

##Random Forest Model
##install.packages("randomForest")
library(randomForest)

## Calling syntax to build the Random Forest
RF <- randomForest(Claimed ~ ., data = cart.dev, 
                   ntree=501, mtry = 5, nodesize = 10,
                   importance=TRUE)


print(RF)

plot(RF)

RF$err.rate


## List the importance of the variables.
importance(RF)

## Tuning Random Forest
set.seed(123)
tRF <- tuneRF(x = cart.dev[,-4], 
              y=as.factor(cart.dev$Claimed),
              mtryStart = 5, 
              ntreeTry=491, 
              stepFactor = 1.15, 
              improve = 0.01, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 50, 
              importance=FALSE
)

## Scoring syntax
cart.dev$predict.class <- predict(tRF, cart.dev, type="class")
cart.dev$predict.score <- predict(tRF, cart.dev, type="prob")[,2]
head(cart.dev)

## Scoring syntax
cart.holdout$predict.class <- predict(tRF, cart.holdout, type="class")
cart.holdout$predict.score <- predict(tRF, cart.holdout, type="prob")[,2]

library(ROCR)
pred <- ROCR::prediction(cart.dev$predict.score, cart.dev$Claimed)
perf <- ROCR::performance(pred, "tpr", "fpr")
plot(perf)
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

library(caret)
confusionMatrix(table(cart.dev$predict.class,cart.dev$Claimed),
                mode = "everything")


#For the holdout sample
library(ROCR)
pred <- ROCR::prediction(cart.holdout$predict.score, cart.holdout$Claimed)
perf <- ROCR::performance(pred, "tpr", "fpr")
plot(perf)
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

library(caret)
confusionMatrix(table(cart.holdout$predict.class,cart.holdout$Claimed),
                mode = "everything")



##ANN

#Scale train and test data

nn.devscaled <- scale(cart.dev[,c(1,5,7,8)])

library(caret)

devscaling <- preProcess(cart.dev[,-4])
nn.holdoutscaled <- predict(devscaling, cart.holdout[,-4])

nn.devscaled <- cbind(cart.dev[,c(2,3,4,6,9,10)],nn.devscaled)
nn.holdoutscaled <- cbind(cart.holdout[,c(2,3,4,6,9,10)],nn.holdoutscaled)

str(nn.devscaled)
## Installing the Neural Net package; 

## If already installed do not run the below step
install.packages("neuralnet")


library(neuralnet)

m1=model.matrix(~Claimed+Agency_Code+Type+Channel+Product.Name+Destination+Age+Commision+Duration+Sales,nn.devscaled)
m2=model.matrix(~Claimed+Agency_Code+Type+Channel+Product.Name+Destination+Age+Commision+Duration+Sales,nn.holdoutscaled)

d1=as.data.frame(m1)
d2=as.data.frame(m2)

colnames(d1)<- gsub(" ",'_',colnames(d1))
colnames(d2)<- gsub(" ",'_',colnames(d2))

str(d1)
str(d2)

d1=d1[,-1]
d2=d2[,-1]

nn1 <- neuralnet(formula = ClaimedYes ~ .,
                 data = d1, 
                 hidden = 4,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 4000
)

plot (nn1)


## Assigning the Probabilities to Dev Sample
d1$Prob = nn1$net.result[[1]] 


## The distribution of the estimated probabilities
quantile(d1$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hist(d1$Prob)


## Assgining 0 / 1 class based on certain threshold
d1$Class <- ifelse(d1$Prob>0.5,1,0)



## Scoring test dataset using the Neural Net Model Object
## To score we will use the compute function

compute.output = compute(nn1, d2)
d2$Predict.score <- compute.output$net.result
View(d2)

## Assigning 0 / 1 class based on certain threshold
d2$Class = ifelse(d2$Predict.score>0.5,1,0)


library(ROCR)
str(d1)

pred <- ROCR::prediction(d1$Prob, d1$ClaimedYes)
perf <- ROCR::performance(pred, "tpr", "fpr")
plot(perf)
auc <- ROCR::performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

library(caret)
confusionMatrix(table(d1$Class,d1$ClaimedYes),positive = "1",mode = "everything")

#for the holdout sample
library(ROCR)
pred <- ROCR::prediction(d2$Predict.score, d2$ClaimedYes)
perf <- ROCR::performance(pred, "tpr", "fpr")
plot(perf)
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

library(caret)
confusionMatrix(table(d2$Class,d2$ClaimedYes),positive = "1",mode = "everything")

#Comparison of models and final conclusion
model_compare <- data.frame(Model = c('Random Forest',
                                      'CART',
                                      'ANN'),
                            Accuracy = c(0.8144,
                                         0.7782,
                                          0.8015))
ggplot(aes(x=Model, y=Accuracy), data=model_compare) +
  geom_bar(stat='identity', fill = 'blue') +
  ggtitle('Comparative Accuracy of Models on Cross-Validation Data') +
  xlab('Models') +
  ylab('Overall Accuracy')

varImpPlot(tRF)
varImp(tRF)
