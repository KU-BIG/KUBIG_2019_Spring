#install.packages("cluster")
#install.packages("mclust")
#install.packages("NbClust")
#install.packages("mvtnorm")

library(mclust)
library(cluster)
library(NbClust)
library(mvtnorm)

#data 읽기
iris.data=read.csv("C:/Users/chs76/Desktop/kubig/iris.csv",header=T)
#주어진 데이터에서 숫자로 표시된 부분 scailing. clustering은 단위가 중요하다.
iris1=scale(iris.data[,-5]) 


#군집갯수 설정: 방법 1
NbClust(iris1, min.nc = 2, max.nc = 15, method = "kmeans")
#나온 결과중, Conclusion에서 최적의 군집추천

#군집갯수 설정: 방법 2
wssplot <- function(data, n = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:n) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:n, wss, type="b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")}

wssplot(iris1)
#군집3부터 큰 차이없이 감소 -> 군집 3으로 설정

#heirarchical clustering
par(mfrow = c(1,1))
plot(hclust(dist(iris1,"euclidean"),method="complete"))


#kmeans clustering
iris.kmeans = kmeans(iris1, centers = 3, iter.max = 10000)
iris.kmeans$centers
iris.data$cluster = as.factor(iris.kmeans$cluster)

qplot(petal.width, petal.length, colour = cluster, data = iris.data)
table(iris.data$variety, iris.data$cluster)


#pca 이용한 clustering
pc = princomp(iris1)
plot(pc)
summary(pc) 
#cumulative proportion을 보면 comp1,2 가 95퍼센트의 설명력을 갖는것을 알 수 있음

pc2=prcomp(iris1)
comp=data.frame(pc2$x[,1:4])
k = kmeans(comp, 3, nstart=25, iter.max=10000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp[,1:2], col=k$clust, pch=16)

table(iris.data$variety,k$cluster) #큰 차이가 없다..



