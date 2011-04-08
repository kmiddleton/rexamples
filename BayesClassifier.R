library(e1071)
?naiveBayes

# and compared to your example:
cl <- kmeans(iris[,1:4], 3)
table(cl$cluster, iris[,5])

m <- naiveBayes(iris[,1:4], iris[,5])
table(predict(m, iris[,1:4]), iris[,5])