###############################################
# Lab: Decision Trees with German Credit Data #
#      Using random forests()                 #
###############################################


### Install packages

#install.packages("randomForest")
library(randomForest)

### Grow trees

set.seed(1234)
fit = randomForest(variety~., data=iris, ntree=100, mtry=2, importance=T)

plot(fit, type="l")
importance(fit)


### Prediction

pred = predict(fit, newdata=iris, type="class")
ctable = table(iris$variety, pred, dnn=c("Actual", "Predicted")); ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy

###########################################
# Computing the test error by paritioning


### Data partition

set.seed(123)
V = 2
n =  nrow(iris)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
iris.train = iris[ii,]
iris.test  = iris[-ii,]


### Grow trees

set.seed(1234)
fit = randomForest(variety~., data=iris.train, ntree=100, mtry=2, importance=T)

plot(fit, type="l")
importance(fit)


### Prediction

pred = predict(fit, newdata=iris.test, type="class")
ctable = table(iris.test$variety, pred, dnn=c("Actual", "Predicted")); ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy


##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0

set.seed(12345)
id = sample(1:V, nrow(iris), replace = T)

for(i in 1:V) {
  
  print(i)
  
  iris.train = iris[id != i,] 
  iris.test = iris[id == i,] 
  
  fit = randomForest(variety~., data=iris.train, ntree=100, mtry=2, importance=T)

  pred = predict(fit, newdata=iris.train, type="class")
  ctable = table(iris.train$variety, pred, dnn=c("Actual", "Predicted"))
  miss.err.train.temp = 1-sum(diag(ctable))/sum(ctable)
  miss.err.train = miss.err.train + miss.err.train.temp

  pred = predict(fit, newdata=iris.test, type="class")
  ctable = table(iris.test$variety, pred, dnn=c("Actual", "Predicted"))
  miss.err.test.temp = 1-sum(diag(ctable))/sum(ctable)
  miss.err.test = miss.err.test + miss.err.test.temp
  
}

cv.err.train = miss.err.train/ V; cv.err.train # CV training error
cv.err.test = miss.err.test/ V;cv.err.test # CV test error



### END
