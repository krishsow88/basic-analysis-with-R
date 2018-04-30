

#logistic Regression

#view data
View(binaryN)
str(binaryN)

#change class
attach(binaryN)
binaryN$admit<- as.factor(binaryN$admit)
binaryN$rank<- as.factor(binaryN$rank)

#two way table of factor variable
xtabs(~admit+rank, data = binaryN)#creating a table. table can also be used in place of xtabs

#divide data to train and test
set.seed(1234)
ind<- sample(2, nrow(binaryN), replace = T, prob = c(0.8,0.2))
trainB<- binaryN[ind==1,]
testB<- binaryN[ind==2,]

#logistic regression model
modelB <-glm(admit ~ .,family = "binomial", data = trainB)
summary(modelB)
modelB <-glm(admit ~ gpa+rank,family = "binomial", data = trainB)
summary(modelB)

#prediction
P1<-predict(modelB,trainB, type='response')
head(P1)
head(trainB)

#probability calculation
predict1<- ifelse(P1>0.5, 1,0)
tab1<- table(predicted =predict1, actual = trainB$admit)
tab1#confusion matrix
1-sum(diag(tab1))/sum(tab1)

#misclassification error - test data 
P2<- predict(modelB, testB, type = 'response')
pred2<- ifelse(P2>0.5,1,0)
tab2<- table(Predicted = pred2, Actual = testB$admit)
tab2
1-sum(diag(tab2))/sum(tab2)

#Goodness of- fit test
with(modelB, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))

#///////////////////--------//////////////////////---------////////////////////
#whether student is admited or not
#model performance evaluation
#model
library(nnet)
modelLB<- multinom(admit~., data=binaryN )
#missclassification rate
pB<- predict(modelLB, binaryN) 
tabB<- table(pB, binaryN$admit)
tabB
1-sum(diag(tabB))/sum(tabB)

table(binaryN$admit)
#for better prediction- using ROCR-----------------
install.packages("ROCR")
library(ROCR)
Predict_admit<- predict(modelLB, binaryN, type = 'prob')
head(Predict_admit)
head(binaryN)#here we assuemed cutoff point to p=0.5. but how can we conclude like that?

#find cutoff probability point
hist(Predict_admit)
Predict_admit<- prediction(Predict_admit, binaryN$admit)
evaluation<- performance(Predict_admit, "acc")
plot(evaluation)
abline(h=0.75,v= 0.46)

#identify best value
evaluation
max<- which.max(slot(evaluation, "y.values")[[1]])
max
acc<- slot(evaluation, "y.values")[[1]][max]
acc
cut<-slot(evaluation, "x.values")[[1]][max]
print(c(Accuracy= acc, cutoff = cut))
 
#prediction using ROC curve---------------
pred_ROC<- prediction(pred_ROC, binaryN$admit)
 