###############################################
# Lab: Decision Trees with German Credit Data #
#      Using bagging()                        #
###############################################


### Install packages

#install.packages("rpart")
#install.packages("adabag")
library(rpart)
library(adabag)


### Grow trees

set.seed(1234)
my.control = rpart.control(xval=0, cp=0, minsplit=5, maxdepth=10)
fit = bagging(variety~., data=iris, mfinal=50, control=my.control)

print(fit$importance)
importanceplot(fit)


### Prediction

pred = predict.bagging(fit, newdata=iris)

ctable = table(iris$variety, pred$class, dnn=c("Actual", "Predicted")); ctable #classification table


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
my.control = rpart.control(xval=0, cp=0, minsplit=5, maxdepth=10)
fit = bagging(variety~., data=iris.train, mfinal=50, control=my.control)

print(fit$importance)
importanceplot(fit)

### Prediction

pred = predict.bagging(fit, newdata=iris.test)
ctable = table(iris.test$variety, pred$class, dnn=c("Actual", "Predicted")); ctable #classification table


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
  
  my.control = rpart.control(xval=0, cp=0, minsplit=5, maxdepth=10)
  fit = bagging(variety~., data=iris.train, mfinal=50, control=my.control)

  pred = predict.bagging(fit, newdata=iris.train)
  ctable = table(iris.train$variety, pred$class, dnn=c("Actual", "Predicted"))
  miss.err.train.temp = 1-sum(diag(ctable))/sum(ctable)
  miss.err.train = miss.err.train + miss.err.train.temp
  
  pred = predict.bagging(fit, newdata=iris.test)
  ctable = table(iris.test$variety, pred$class, dnn=c("Actual", "Predicted"))
  miss.err.test.temp = 1-sum(diag(ctable))/sum(ctable)
  miss.err.test = miss.err.test + miss.err.test.temp
  
}

cv.err.train = miss.err.train/ V; cv.err.train # CV training error
cv.err.test = miss.err.test/ V;cv.err.test # CV test error



### END
