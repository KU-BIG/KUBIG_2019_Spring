### Data import

iris = read.csv("iris.csv",header=T) 
iris$variety = as.factor(iris$variety)

### Data exploration

str(iris)
dim(iris)
barplot(table(iris$variety), col = "blue", xlab = "Variety", ylab = "Frequency")


### Install packages

#install.packages("tree")
library(tree)


### Grow a tree

fit = tree(variety ~., data=iris, split="deviance")
#NOTE: deviance=(entropy * 2n), using base e for log 
#split="gini" differs from split="deviance", but deviance is printed for both.

fit
summary(fit)
plot(fit);  text(fit)


### Prune the tree

cv.fit = cv.tree(fit, FUN=prune.misclass)
names(cv.fit)
cv.fit

par(mfrow=c(2,2))

plot(cv.fit$size,cv.fit$dev, type="b")

plot(cv.fit$k,cv.fit$dev, type="b")
fit.pruned = prune.misclass(fit, best=5) 
plot(fit.pruned);text(fit.pruned)


fit.pruned = prune.misclass(fit, best=cv.fit$size[which.min(cv.fit$dev)]) 
plot(fit.pruned);text(fit.pruned)



### Prediction
pred = predict(fit.pruned, newdata=iris, type="class") #prediction
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


### Grow a tree

fit = tree(variety ~., iris.train)
fit
summary(fit)
plot(fit);  text(fit)


### Prune the tree

set.seed(3)
cv.fit = cv.tree(fit, FUN=prune.misclass)
names(cv.fit)
cv.fit

par(mfrow=c(2,2))
plot(cv.fit$size,cv.fit$dev, type="b")
plot(cv.fit$k,cv.fit$dev, type="b")

fit.pruned = prune.misclass(fit, best=cv.fit$size[which.min(cv.fit$dev)])
plot(fit.pruned);text(fit.pruned)



### Prediction

pred = predict(fit.pruned, newdata=iris.test, type="class") #prediction
ctable = table(iris.test$variety, pred, dnn=c("Actual", "Predicted"));
ctable #classification table


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
  
  iris.train = iris1[id != i,] 
  iris.test = iris1[id == i,] 
  
  fit = tree(variety ~., iris.train) #Growing
  
  cv.fit = cv.tree(fit, FUN=prune.misclass)
  tree.size.min.dev = cv.fit$size[which.min(cv.fit$dev)]
  fit.pruned = prune.misclass(fit, best=tree.size.min.dev) # Pruning
  
  pred = predict(fit.pruned, newdata=iris.train, type="class") #prediction
  ctable = table(iris.train$variety, pred, dnn=c("Actual", "Predicted"));
  miss.err.train.temp = 1-sum(diag(ctable))/sum(ctable)
  miss.err.train = miss.err.train + miss.err.train.temp
  
  pred = predict(fit.pruned, newdata=iris.test, type="class") #prediction
  ctable = table(iris.test$variety, pred, dnn=c("Actual", "Predicted"));
  miss.err.test.temp = 1-sum(diag(ctable))/sum(ctable)
  miss.err.test = miss.err.test + miss.err.test.temp 
  
}

cv.err.train = miss.err.train/ V; cv.err.train # CV training error
cv.err.test = miss.err.test/ V;cv.err.test # CV test error



### END
