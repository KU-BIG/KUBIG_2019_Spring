
install.packages("keras")
library(keras)
install_keras()
install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

#training data & test data 확인
train=read.csv("C:/Users/chs76/Desktop/kubig/mnist_train_100.csv",header=F)
test=read.csv("C:/Users/chs76/Desktop/kubig/mnist_test_10.csv",header=F)
train_x=train[,-1]
train_y=train[,1]
test_x=test[,-1]
test_y=test[,1]
train_x=as.matrix(train_x)
test_x=as.matrix(test_x)
train_x=train_x / 255
test_x=test_x / 255
cat(dim(train_x)[[1]], '개: 훈련표본\n')
cat(dim(test_x)[[1]], '개: 검증표본\n')
plot(table(train_y),type="h")
plot(table(test_y),type="h")


#epoch=10
train=read.csv("C:/Users/chs76/Desktop/kubig/mnist_train_100.csv",header=F)
test=read.csv("C:/Users/chs76/Desktop/kubig/mnist_test_10.csv",header=F)
train_x=train[,-1]
train_y=train[,1]
test_x=test[,-1]
test_y=test[,1]
train_x=as.matrix(train_x)
test_x=as.matrix(test_x)
train_x=train_x / 255
test_x=test_x / 255
train_y=to_categorical(train_y,10)
test_y=to_categorical(test_y,10)

model = keras_model_sequential()
model %>% 
  layer_dense(units=256, activation="relu", input_shape=c(784))  %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units=10,activation="softmax") 

summary(model)

#training data 에서 validation data를 20프로 추출(임의로 지정)
model %>%
  compile(loss ="categorical_crossentropy",
          optimizer = "adam",
          metrics= c("accuracy"))
history <- model %>% fit(train_x, train_y, epochs = 10, batch_size = 128,
                         callbacks = callback_tensorboard(log_dir = "logs/run_b"),
                         validation_split = 0.2) 
history$metrics

score <- model %>% 
  evaluate(test_x, test_y, batch_size=128)
cat("정확도(Accurracy: ", scales::percent(score[[2]]), "\n")


#epoch=50
train=read.csv("C:/Users/hongsuk/Desktop/홍석/mnist_train_100.csv",header=F)
test=read.csv("C:/Users/hongsuk/Desktop/홍석/mnist_test_10.csv",header=F)
train_x=train[,-1]
train_y=train[,1]
test_x=test[,-1]
test_y=test[,1]
train_x=as.matrix(train_x)
test_x=as.matrix(test_x)
train_x=train_x / 255
test_x=test_x / 255
train_y=to_categorical(train_y,10)
test_y=to_categorical(test_y,10)

model = keras_model_sequential()
model %>% 
  layer_dense(units=256, activation="relu", input_shape=c(784))  %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units=10,activation="softmax") 

summary(model)

#training data 에서 validation data를 20프로 추출(임의로 지정)
model %>%
  compile(loss ="categorical_crossentropy",
          optimizer = "adam",
          metrics= c("accuracy"))
history <- model %>% fit(train_x, train_y, epochs = 50, batch_size = 128,
                         callbacks = callback_tensorboard(log_dir = "logs/run_b"),
                         validation_split = 0.2) 
history$metrics

score <- model %>% 
  evaluate(test_x, test_y, batch_size=128)
cat("정확도(Accurracy: ", scales::percent(score[[2]]), "\n")

#epoch를 10과 50으로 나눠서 한 이유는 얼마나 정확도가 올라가는지 보여주기 위함.