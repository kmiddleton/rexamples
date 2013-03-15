# see http://little-book-of-r-for-multivariate-analysis.readthedocs.org/en/latest/src/multivariateanalysis.html
# see http://www.statmethods.net/advstats/discriminant.html

library(MASS)
library(car)

makeProfilePlot <- function(mylist,names) {
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose 'numvariables' random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}

printMeanAndSdByGroup <- function(variables,groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variables[groupvariable==leveli,]
    groupsize <- nrow(levelidata)
    print(paste("Group",leveli,"Group size:",groupsize))
    print(paste("Group",leveli,"Means:"))
    print(mean(levelidata))
    print(paste("Group",leveli,"Standard Deviations:"))
    print(sd(levelidata))
  }
}

calcAllocationRuleAccuracy <- function(ldavalue, groupvariable, cutoffpoints)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the number of true positives and false negatives for each group
  numlevels <- length(levels)
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- ldavalue[groupvariable==leveli]
    # see how many of the samples from this group are classified in each group
    for (j in 1:numlevels)
    {
      levelj <- levels[j]
      if (j == 1)
      {
        cutoff1 <- cutoffpoints[1]
        cutoff2 <- "NA"
        results <- summary(levelidata <= cutoff1)
      }
      else if (j == numlevels)
      {
        cutoff1 <- cutoffpoints[(numlevels-1)]
        cutoff2 <- "NA"
        results <- summary(levelidata > cutoff1)
      }
      else
      {
        cutoff1 <- cutoffpoints[(j-1)]
        cutoff2 <- cutoffpoints[(j)]
        results <- summary(levelidata > cutoff1 & levelidata <= cutoff2)
      }
      trues <- results["TRUE"]
      trues <- trues[[1]]
      print(paste("Number of samples of group",leveli,"classified as group",levelj," : ",
                  trues,"(cutoffs:",cutoff1,",",cutoff2,")"))
    }
  }
}

calcAllocationRuleAccuracy <- function(ldavalue, groupvariable, cutoffpoints)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the number of true positives and false negatives for each group
  numlevels <- length(levels)
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- ldavalue[groupvariable==leveli]
    # see how many of the samples from this group are classified in each group
    for (j in 1:numlevels)
    {
      levelj <- levels[j]
      if (j == 1)
      {
        cutoff1 <- cutoffpoints[1]
        cutoff2 <- "NA"
        results <- summary(levelidata <= cutoff1)
      }
      else if (j == numlevels)
      {
        cutoff1 <- cutoffpoints[(numlevels-1)]
        cutoff2 <- "NA"
        results <- summary(levelidata > cutoff1)
      }
      else
      {
        cutoff1 <- cutoffpoints[(j-1)]
        cutoff2 <- cutoffpoints[(j)]
        results <- summary(levelidata > cutoff1 & levelidata <= cutoff2)
      }
      trues <- results["TRUE"]
      trues <- trues[[1]]
      print(paste("Number of samples of group",leveli,"classified as group",levelj," : ",
                  trues,"(cutoffs:",cutoff1,",",cutoff2,")"))
    }
  }
}

wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",")
wine$V1 <- factor(wine$V1)

scatterplotMatrix(wine[2:6])

names <- c("V2","V3","V4","V5","V6")
mylist <- list(wine$V2,wine$V3,wine$V4,wine$V5,wine$V6)
makeProfilePlot(mylist,names)

printMeanAndSdByGroup(wine[2:14],wine[1])

wine.Z <- as.data.frame(scale(wine[2:14]))
wine.Z <- cbind(V1 = wine$V1, wine.Z)

wine.lda <- lda(V1 ~ ., data = wine.Z)
wine.lda

ct <- table(wine$V1, wine.lda$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

library(klaR)
partimat(V1 ~ V2 + V3 + V4, data=wine.Z,method="lda")

wine.lda.values <- predict(wine.lda, wine.Z)
ldahist(data = wine.lda.values$x[,1], g=wine$V1)

printMeanAndSdByGroup(wine.lda.values$x,wine[1])

calcAllocationRuleAccuracy(wine.lda.values$x[,1], wine[1], c(-1.751108, 2.122505))

## Clustering
dm <- dist(wine[, c("V1", "V2", "V3", "V4", "V5", "V6")])
plot(cs <- hclust(dm, method = "complete"))

## Check multivariate homogeneity of variance
library(vegan)
wine3 <- wine[, 1:8]
dm <- dist(wine[, 2:8])
(MHV <- betadisper(dm, wine$V1))
anova(MHV)
permutest(MHV)
TukeyHSD(MHV)
plot(MHV)
