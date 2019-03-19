
library(nnet)

### Data import

iris <- read.csv("iris.csv",header=T) 
iris1 <- iris
iris1$variety <- as.factor(iris1$variety)


### Data exploration
str(iris1)


### Split Data
a<-sample(1:150, 100)
iris.train <- iris1[a,]
iris.test <- iris1[-a,]

### Model fit(Full)
fit = multinom(variety ~., data = iris.train)
summary(fit)


### Model selection
fit2 = step(fit, direction = "both")
summary(fit2)


### Prediction
#train
iris.train$reduce = predict(fit2,iris.train,type = "class")
train <- table(iris.train$variety,iris.train$reduce)
train
#test
iris.test$reduce = predict(fit2,iris.test,type = "class")
head(iris.test)

test<-table(iris.test$variety,iris.test$reduce)
test

### Errors
#train
miss.err = 1-sum(diag(train))/sum(train) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  
#test
miss.err = 1-sum(diag(test))/sum(test) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  


##########################
# Computing the CV error

V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(iris1), replace = T)

for(i in 1:V) {
  
  print(i)

  iris.train = iris1[id != i,] 
  iris.test = iris1[id == i,] 
  
  fit = multinom(variety ~., data = iris.train)
  fit2 = step(fit, direction = "both") #Stepwise variable selection
  iris.train$reduce = predict(fit2,iris.train,type = "class")
  train <- table(iris.train$variety,iris.train$reduce)
  iris.test$reduce = predict(fit2,iris.test,type = "class")
  test<-table(iris.test$variety,iris.test$reduce)
  
  pred.train = predict(fit2, newdata=iris.train, type="class")
  miss.err.train.temp = 1-sum(diag(train))/sum(train)
  miss.err.train = miss.err.train + miss.err.train.temp
  
  pred.test = predict(fit2, newdata=iris.test, type="class")
  miss.err.test.temp = 1-sum(diag(test))/sum(test)
  miss.err.test = miss.err.train + miss.err.test.temp
  
}

cv.err.train = miss.err.train/ V # CV training error
cv.err.train

cv.err.test = miss.err.test/ V # CV test error
cv.err.test

