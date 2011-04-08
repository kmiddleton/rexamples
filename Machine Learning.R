#
# This file gives a quick demonstration of a few ML techniques that
# are available in R. The file is designed as a walk through, so instead
# of simply running the entire file at once, copy and paste the logical
# blocks and check the output as we go along.
#
# Joshua Reich (josh@i2pi.com) 	
# April 2, 2009
#


# First we need to load up some packages to support ML.
# If your system doesn't have the packages, check out
# the install.packages() command. 

library(rpart)
library(MASS)
library(class)
library(e1071)


rmulnorm <- function (n, mu, sigma)
{
	# A simple function for producing n random samples
	# from a multivariate normal distribution with mean mu
	# and covariance matrix sigma

	M <- t(chol(sigma))
	d <- nrow(sigma)
	Z <- matrix(rnorm(d*n),d,n)
	t(M %*% Z + mu)
}

cm <- function (actual, predicted)
{
	# Produce a confusion matrix

	t<-table(predicted,actual)
	# there is a potential bug in here if columns are tied for ordering
	t[apply(t,2,function(c) order(-c)[1]),] 
}

# Total number of observations
N <- 1000 * 3

# Number of training observations
Ntrain <- N * 0.7


# The data that we will be using for the demonstration consists
# of a mixture of 3 multivariate normal distributions. The goal
# is to come up with a classification system that can tell us,
# given a pair of coordinates, from which distribution the data 
# arises.

A <- rmulnorm (N/3, c(1,1), matrix(c(4,-6,-6,18), 2,2))
B <- rmulnorm (N/3, c(8,1), matrix(c(1,0,0,1), 2,2))
C <- rmulnorm (N/3, c(3,8), matrix(c(4,0.5,0.5,2), 2,2))

data <- data.frame(rbind (A,B,C))
colnames(data) <- c('x', 'y')
data$class <- c(rep('A', N/3), rep('B', N/3), rep('C', N/3))

# Lets have a look
plot_it <- function () {
	plot (data[,1:2], type='n')
	points(A, pch='A', col='red')
	points(B, pch='B', col='blue')
	points(C, pch='C', col='orange')
}
plot_it()

# Randomly arrange the data and divide it into a training
# and test set.
data <- data[sample(1:N),]
train <- data[1:Ntrain,]
test <- data[(Ntrain+1):N,]

# OK. Lets get to it

# K-Means

# kmeans(), built into the R base package, is an unsupervised
# learning technique. The goal is to cluster the observed data
# into groups. This is achieved by assuming a Euclidean distance
# metric, and finding points which lie at local centroids. All
# points are then assigned to their closest centroid and are
# thus clustered. The algorithmic approach to finding these
# centroids is to pick k points at random then assign all other 
# points to the centroids. The algorithm then chooses new
# centroids based on the mean point of the resulting clusters.
# Then with these new centroids, the remaining N-k points are
# reclustered. This repeats until some stopping condition is
# reached.
# This algorithm is quite simple to implement and quite often
# the Euclidean distance metric is inappropriate and I find 
# myself re-writing the algorithm using a different measure.
# Here we know, a priori, that there are 3 clusters, so we set 
# k = 3.

k <- kmeans(train[,1:2], 3)
plot(train[,1:2], type='n')
text(train[,1:2], as.character(k$cluster))

cm (train$class, k$cluster)

# K Nearest Neighbor

# In KNN we classify an unknown point by looking at k nearest
# neighbors of known classification. In the case of k=1, we
# find the closest point in our training set to a new point
# from our test set. We then assume that the new point has
# the same class as its closest neighbor in the test set.
# For k>1, we typically apply a voting mechanism to pick the
# modal class from the neighborhood of points in the training
# set. 

test$predicted_class <- knn(train[,1:2], test[,1:2], train$class, k=1)

(m<-cm(test$class, test$predicted_class))

# Here we demonstrate the bias-variance tradeoff as we increase k.

err <- matrix(nrow=N/100, ncol=2)
for (i in 1:nrow(err))
{
	k <- i * 4
	test$predicted_class <- knn(train[,1:2], test[,1:2], train$class, k=k)
	m<-cm(test$class, test$predicted_class)
	err[i,] <- c(k, 1 - sum(diag(m)) / sum(m))
}

plot (err)

# Kernel Methods

# Rather than using library functions, we will build our own here
# to demonstrate the mechanism, sacrificing efficiencies for 
# edification. This code is slow.

# Kernel methods follow on from KNN. Whereas KNN is by its nature
# highly local, and thus potentially non-smooth, kernel techniques
# apply a windowing function to the dataset to smooth the classifier.
# Here we use a Gaussian/radial function to weight the influence
# of the each point in the training set. We then take the average
# of the weighted distances to determine the most likely classification
# for new points in the test set.

kernel <- function (a, b)
{
	# Lets make a simple 'Gaussian' like kernel that
	# measures the distance between two points, a & b.
	exp(-sum((a-b)^2))
}

test$predicted_class <- NA
for (i in 1:nrow(test))
{
	print(i)
	# The weighted distances from each point in the training set to 
	# row i in the test set
	d<-apply(train[,1:2], 1, function(r) kernel(r, test[i,1:2]))

	# The class votes, based on the mean distance
	v<-aggregate(d, list(class=train$class), mean)

	# Predicted class = the class with the lowest distance
	test$predicted_class[i] <- v$class[order(-v$x)[1]]
}	

cm(test$class, test$predicted_class)


# Recursive Partitioning / Regression Trees

# rpart() implements an algorithm that attempts to recursively split
# the data such that each split best partitions the space according 
# to the classification. In a simple one-dimensional case with binary
# classification, the first split will occur at the point on the line
# where there is the biggest difference between the proportion of 
# cases on either side of that point. The algorithm continues to 
# split the space until a stopping condition is reached. Once the
# tree of splits is produced it can be pruned using regularization
# parameters that seek to ameliorate overfitting.

(r <- rpart(class ~ x + y, data = train))
plot(r)
text(r)

# Here we look at the confusion matrix and overall error rate from applying
# the tree rules to the training data.
predicted <- as.numeric(apply(predict(r), 1, function(r) order(-r)[1]))
(m <- cm (train$class, predicted))
1 - sum(diag(m)) / sum(m)

# And by comparison, against the test data.
predicted <- as.numeric(apply(predict(r, test[,1:2]), 1, function(r) order(-r)[1]))
(m <- cm (test$class, predicted))
1 - sum(diag(m)) / sum(m)

# PCA - Demonstrating that orthogonal bases are better for trees

# Recursive partitioning splits the space along orthogonal hyperplanes 
# that are parallel to the original feature coordinate axes. However,
# in our case, the clusters are not neatly split by such planes and
# better results can be found by transforming to another space. We
# use principle component analysis (PCA) to transform our space.
# PCA transforms the space by looking at the vectors along which the
# bulk of the variance in the data occur. The vector that embodies
# the greatest variance becomes the first principle component axis
# in the transformed space. The second axis then is formed along the
# vector that is orthogonal to the first but with the second most 
# variance in the data. And so on.
# It should be clear how this transform improves the performance of 
# recursive partitioning, but the cost is that the tree splits 
# no longer directly map to the feature space, which makes 
# interpretation much more difficult.

p<-princomp(train[,1:2])
train_pca <- data.frame(p$scores)
train_pca$class <- train$class

# Compare the alignment of the clusters to the axis in the feature
# space versus the transformed space.
par(mfrow=c(1,2))
train_lda <- as.matrix(train[,1:2]) %*% l$scaling
plot_it()
plot(train_pca[,1:2], type='n')
text(train_pca[,1:2], train_pca$class)
par(mfrow=c(1,1))

r2 <- rpart(class ~ Comp.1 + Comp.2, data = train_pca)

predicted2 <- as.numeric(apply(predict(r2), 1, function(r) order(-r)[1]))
(m <- cm (train$class, predicted2))
1 - sum(diag(m)) / sum(m)

# LDA

# In linear discriminant analysis we no longer look for recursive 
# partitions, but rather for lines that go between the clusters.
# In some ways, this is similar to KNN. LDA makes the assumption
# that the clusters are drawn from multivariate normal distributions
# with different means, but identical covariances. LDA approaches
# the problem by applying a transform that applies the inverse of 
# the estimated covariance matrix to distributed the points 
# spherically. In this transformed space classification is simply
# a matter of finding the closest cluster mean. 
# The assumption of identical covariances doesn't hold for our
# dataset, but still provides an improvement. Quadratic DA drops
# this assumption at the cost of greater complexity. 

l <- lda(class ~ x + y, data = train)

(m <- cm(train$class, predict(l)$class))
1 - sum(diag(m)) / sum(m)

par(mfrow=c(1,2))
train_lda <- as.matrix(train[,1:2]) %*% l$scaling
plot_it()
plot(train_lda, type='n')
text(train_lda, train$class)
par(mfrow=c(1,1))

# SVM

# Support vector machines take the next step from LDA/QDA. However
# instead of making linear voronoi boundaries between the cluster
# means, we concern ourselves primarily with the points on the 
# boundaries between the clusters. These boundary points define
# the 'support vector'. Between two completely separable clusters
# there are two support vectors and a margin of empty space
# between them. The SVM optimization technique seeks to maximize
# the margin by choosing a hyperplane between the support vectors
# of the opposing clusters. For non-separable clusters, a slack
# constraint is added to allow for a small number of points to
# lie inside the margin space. The Cost parameter defines how
# to choose the optimal classifier given the presence of points
# inside the margin. Using the kernel trick (see Mercer's theorem)
# we can get around the requirement for linear separation 
# by representing the mapping from the linear feature space to
# some other non-linear space that maximizes separation. Normally
# a kernel would be used to define this mapping, but with the
# kernel trick, we can represent this kernel as a dot product.
# In the end, we don't even have to define the transform between
# spaces, only the dot product distance metric. This leaves
# this algorithm much the same, but with the addition of
# parameters that define this metric. The default kernel used
# is a radial kernel, similar to the kernel defined in my 
# kernel method example. The addition is a term, gamma, to
# add a regularization term to weight the importance of distance.

s <- svm( I(factor(class)) ~ x + y, data = train, cost = 100, gama = 1)

(m <- cm(train$class, predict(s)))
1 - sum(diag(m)) / sum(m)

(m <- cm(test$class, predict(s, test[,1:2])))
1 - sum(diag(m)) / sum(m)

