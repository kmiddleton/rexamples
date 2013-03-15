require(nnet)
require(mda)
require(ape)
require(geiger)
require(lattice)
###----------------------------------------------------------------------
### Internal function from the package mda
###----------------------------------------------------------------------
"contr.fda" <-
function (p = rep(1, d[1]), contrast.default = contr.helmert(length(p)))
{
d <- dim(contrast.default)
sqp <- sqrt(p/sum(p))
x <- cbind(1, contrast.default) * outer(sqp, rep(1, d[2] +
1))
qx <- qr(x)
J <- qx$rank
qr.qy(qx, diag(d[1])[, seq(2, J)])/outer(sqp, rep(1, J -
1))
}
###----------------------------------------------------------------------
### Associated functions modified from the package mda
###----------------------------------------------------------------------
"predict.phylo.fda" <-
function (object, newdata, type = c("class", "variates", "posterior",
"hierarchical", "distances"), prior, dimension = J - 1, ...)
{
dist <- function(x, mean, m = ncol(mean)) (scale(x, mean,
                                                 FALSE)^2) %*% rep(1, m)
type <- match.arg(type)
means <- object$means
Jk <- dim(means)
J <- Jk[1]
k <- Jk[2]
if (type == "hierarchical") {
if (missing(dimension))
dimension.set <- seq(k)
else {
dimension.set <- dimension[dimension <= k]
if (!length(dimension.set))
dimension.set <- k
dimension <- max(dimension.set)
}
}

else dimension <- min(max(dimension), k)
if (missing(newdata))
y <- predict(object$fit)
else {
if (inherits(newdata, "data.frame") || is.list(newdata)) {
Terms <- delete.response(terms(object))
attr(Terms, "intercept") <- 0
newdata <- model.matrix(Terms, newdata)
}
y <- predict(object$fit, newdata)
}
y <- y %*% object$theta[, seq(dimension), drop = FALSE]
lambda <- object$values
alpha <- sqrt(lambda[seq(dimension)])
sqima <- sqrt(1 - lambda[seq(dimension)])
newdata <- scale(y, FALSE, sqima * alpha)
if (missing(prior))
prior <- object$prior
else {
if (any(prior < 0) | round(sum(prior), 5) != 1)
stop("innappropriate prior")
}
means <- means[, seq(dimension), drop = FALSE]
switch(type, variates = return(newdata), class = {
n <- nrow(newdata)
prior <- 2 * log(prior)
mindist <- dist(newdata, means[1, ], dimension) - prior[1]
pclass <- rep(1, n)
for (i in seq(2, J)) {
ndist <- dist(newdata, means[i, ], dimension) - prior[i]
l <- ndist < mindist
pclass[l] <- i
mindist[l] <- ndist[l]
}
## 2001-10-27: Need to provide levels or else if we get an error
## if the predicted classes do no contain all possible classes.
## Reported by Greg Jefferis <jefferis@stanford.edu>, fix by
## Bj/orn-Helge Mevik <bjorn-helge.mevik@matforsk.no>.
return(factor(pclass, levels = seq(J),
labels = dimnames(means)[[1]]))
}, posterior = {
pclass <- matrix(0, nrow(newdata), J)
for (i in seq(J)) pclass[, i] <- exp(-0.5 * dist(newdata, means[i,
], dimension)) * prior[i]
dimnames(pclass) <- list(dimnames(newdata)[[1]], dimnames(means)[[1]])
return(pclass/drop(pclass %*% rep(1, J)))
}, hierarchical = {
prior <- 2 * log(prior)
Pclass <- vector("list", length(dimension.set))
names(Pclass) <- paste("D", dimension.set, sep = "")
for (ad in seq(along = dimension.set)) {
d <- dimension.set[ad]
dd <- seq(d)

mindist <- dist(newdata[, dd, drop = FALSE], means[1, dd, drop = FALSE],
d) - prior[1]
pclass <- rep(1, nrow(newdata))
for (i in seq(2, J)) {
ndist <- dist(newdata[, dd, drop = FALSE], means[i, dd,
drop = FALSE], d) - prior[i]
l <- ndist < mindist
pclass[l] <- i
mindist[l] <- ndist[l]
}
levels(pclass) <- dimnames(means)[[1]]
Pclass[[ad]] <- pclass
}
rownames <- dimnames(newdata)[[1]]
if (is.null(rownames))
rownames <- paste(seq(nrow(newdata)))
return(structure(Pclass, class = "data.frame", row.names = rownames,
dimensions = dimension.set))
}, distances = {
dclass <- matrix(0, nrow(newdata), J)
for (i in seq(J)) dclass[, i] <- dist(newdata, means[i, ],
dimension)
dimnames(dclass) <- list(dimnames(newdata)[[1]], dimnames(means)[[1]])
return(dclass)
})
}
"predict.polyreg.modified" <-
function (object, newdata, ...)
{
if (missing(newdata)) {
z <- fitted(object)
if (is.null(z))
stop("need to supply newdata")
else return(z)
}
degree <- object$degree
monomial <- object$monomial
newdata %*% object$coef
}
"polyreg.modified" <-
function (x, y, w, degree = 1, monomial = FALSE, ...)
{
#x <- polybasis(x, degree, monomial)
y <- as.matrix(y) # just making sure ...
if (iswt <- !missing(w)) {
if (any(w <= 0))
stop("only positive weights")
w <- sqrt(w)
y <- y * w
x <- x * w
}
qrx <- qr(x)
coef <- as.matrix(qr.coef(qrx, y))

fitted <- qr.fitted(qrx, y)
if ((df <- qrx$rank) < ncol(x))
coef[qrx$pivot, ] <- coef
if (iswt)
fitted <- fitted/w
structure(list(fitted.values = fitted, coefficients = coef,
degree = degree, monomial = monomial, df = df), class = "polyreg.modified")
}
"print.phylo.fda" <-
function (x, ...)
{
if (!is.null(cl <- x$call)) {
cat("Call:\n")
dput(cl)
}
cat("\nDimension:", format(x$dimension), "\n")
cat("\nPercent Between-Group Variance Explained:\n")
print(round(x$percent, 2))
error <- x$confusion
df <- x$fit
if (!is.null(df))
df <- df$df
if (!is.null(df)) {
cat("\nDegrees of Freedom (per dimension):", format(sum(df)),
"\n")
}
if (!is.null(error)) {
n <- as.integer(sum(error))
error <- format(round(attr(error, "error"), 5))
cat("\nTraining Misclassification Error:", error, "( N =",
n, ")\n")
}
invisible(x)
}
"plot.phylo.fda" <- function(pfdamodel,gfactor=pfdamodel$g,prdfactor=pfdamodel$prd)
{
pfdavar <- predict(pfdamodel, type="variate")
lim1x <- c(min(pfdavar[,1]),max(pfdavar[,1]))
lim1y <- c(min(pfdavar[,2]),max(pfdavar[,2]))
m1 <- 4;m2 <- 1
oldpar<-par(no.readonly=FALSE);on.exit(par(oldpar));x11(height=8,width=14);par(mfrow=c(1,2),mar=c(m1,m1,m1,m2),oma=c(m2,m2,m2,m2));
matplot(pfdavar[gfactor==levels(gfactor)[1],1], pfdavar[gfactor==levels(gfactor)[1],2], xlab="pFDA1",ylab="pFDA2", xlim=lim1x, ylim=lim1y, pch=1, col=1, main="True Classes",sub=paste("lambda = ",pfdamodel$val," intrcpt=",pfdamodel$intercept," eqprior=",pfdamodel$eqprior,sep=""))
for (i in 2:nlevels(gfactor)) matplot(pfdavar[gfactor==levels(gfactor)[i],1], pfdavar[gfactor==levels(gfactor)[i],2], add=TRUE, pch=i, col=i)
legend(min(lim1x),max(lim1y),levels(gfactor), pch=1:nlevels(gfactor), col=1:nlevels(gfactor))

legend(min(lim1x),min(lim1y)+(max(lim1y)-min(lim1y))*0.1,paste("lambda = ",pfdamodel$val," intrcpt=",pfdamodel$intercept," eqprior=",pfdamodel$eqprior," ",sep=""))
addEllipseGrp(pfdavar[,1],pfdavar[,2],gfactor, pval=0.95, num=30)
matplot(pfdavar[prdfactor==levels(prdfactor)[1],1], pfdavar[prdfactor==levels(prdfactor)[1],2], xlab="pFDA1",ylab="pFDA2", xlim=lim1x, ylim=lim1y, pch=1, col=1, main="Predicted Classes",sub=paste("lambda = ",pfdamodel$val," intercept=",pfdamodel$intercept," eqprior=",pfdamodel$eqprior,sep=""))
for (i in 2:nlevels(prdfactor)) matplot(pfdavar[prdfactor==levels(prdfactor)[i],1], pfdavar[prdfactor==levels(prdfactor)[i],2], add=TRUE, pch=i, col=i)
legend(min(lim1x),max(lim1y),levels(prdfactor), pch=1:nlevels(prdfactor), col=1:nlevels(prdfactor))
legend(min(lim1x),min(lim1y)+(max(lim1y)-min(lim1y))*0.1,paste(levels(prdfactor),"=",pfdamodel$prior," ",sep=""))
legend(max(lim1x)-(max(lim1x)-min(lim1x))*0.2,max(lim1y),signif(attr(pfdamodel$confusion,"error"),4))
invisible()
}
###----------------------------------------------------------------------
### Main pFDA function with training data only
###----------------------------------------------------------------------
"phylo.fda" <-function (data,grp,tretre,val=1,treetrans=lambdaTree,
dimension = J - 1, eps = .Machine$double.eps,
keep.fitted = (n * dimension < 1000), method=polyreg.modified,intercept=TRUE,eqprior=FALSE,priin=1)
{
this.call <- match.call()
if(intercept) data <- cbind(Intercept=rep(1,nrow(data)),data)
data <- as.matrix(data)
tretre <- treetrans(tretre,val)
g <- as.factor(grp)
ng <- nlevels(g)
W <- vcv.phylo(tretre)
invW<-solve(W)
invW.eig <- eigen(invW)
N <- invW.eig$vectors %*% diag(sqrt(invW.eig$values)) %*% solve(invW.eig$vectors)
divnum <-det(N)^(1/nrow(N))
N <- N/divnum
DATA <- N%*%data #Rao (4,57); transforming the data to linear
n <- nrow(DATA)
y <- matrix(0,nrow(data),ng)
for (i in 1:nrow(data)){y[i,g[i]] <- 1}
Y <- N%*%y #Dummy matrix with phylo bias removed
x <- DATA
fg <- factor(g)
prior <- colSums(Y)/sum(colSums(Y))
if(eqprior) prior <- c(rep(1/ng,ng))
if(priin != 1) prior<-priin
cnames <- levels(fg)
g <- as.numeric(fg)
J <- length(cnames)
weights <- rep(1, n)
dp <- tapply(weights, g, sum)/n
theta <- contr.helmert(J)
theta <- contr.fda(dp, theta)

Theta <- Y%*%theta #fda p.7, above eq2
fit <- method(x, Theta, weights)
rss <- t(Theta-fit$fitted) %*% (Theta-fit$fitted)
ssm <- t(Theta) %*% fitted(fit)/n
ed <- svd(ssm, nu = 0)
thetan <- ed$v
lambda <- ed$d
lambda[lambda > 1 - eps] <- 1 - eps
discr.eigen <- lambda/(1 - lambda)
pe <- (100 * cumsum(discr.eigen))/sum(discr.eigen)
dimension <- min(dimension, sum(lambda > eps))
if (dimension == 0) {
warning("degenerate problem; no discrimination")
return(structure(list(dimension = 0, fit = fit, call = this.call),
class = "phylo.fda"))
}
thetan <- thetan[, seq(dimension), drop = FALSE]
pe <- pe[seq(dimension)]
alpha <- sqrt(lambda[seq(dimension)])
sqima <- sqrt(1 - lambda[seq(dimension)])
vnames <- paste("v", seq(dimension), sep = "")
means <- scale(theta %*% thetan, FALSE, sqima/alpha)
dimnames(means) <- list(cnames, vnames)
names(lambda) <- c(vnames, rep("", length(lambda) - dimension))
names(pe) <- vnames
frml <- "grp~"
nc <- ncol(data)
varnam <- colnames(data)
for(i in 1:(nc-1)) frml <- paste(frml,varnam[i],"+", sep="")
frml <- paste(frml,varnam[nc], sep="")
frml <- as.formula(frml)
dset <- as.data.frame(cbind(grp,DATA))
Terms <- as.call(fda(formula = frml, data = dset, weights = weights))
obj <- structure(list(percent.explained = pe, values = lambda,
means = means, theta.mod = thetan, dimension = dimension,
prior = prior, fit = fit, call = this.call, terms = Terms),
class = "phylo.fda")
obj$confusion <- confusion(predict(obj), fg)
obj$prd <- predict(obj)
obj$g <- as.factor(grp)
obj$val <- val
obj$rss <- sum(diag(rss))
obj$intercept <- intercept
obj$eqprior <- eqprior
if (!keep.fitted)
obj$fit$fitted.values <- NULL
obj
}
###----------------------------------------------------------------------
### Main pFDA function with training and test data
###----------------------------------------------------------------------

"phylo.fda.pred" <-function (dataA,grpA,taxtaxA,tretreA,testlistn,val=1,treetrans=lambdaTree,
method=polyreg.modified,sbcls=floor(table(grp)/4),
dimension = J - 1, eps = .Machine$double.eps, keep.fitted = (n * dimension < 1000),intercept=TRUE,eqprior=FALSE,priin=1)
{
## Preparing data
this.call <- match.call()
if(intercept) dataA <- cbind(Intercept=rep(1,nrow(dataA)),dataA)
dataA <- as.data.frame(dataA)
nA <- nrow(dataA)
testlist <- taxtaxA[testlistn]
traininglist <- taxtaxA[-testlistn]
rownames(dataA) <- taxtaxA
tretre <- drop.tip(tretreA,testlistn)
grp <- grpA[-testlistn]
grp <- grp[grp %in% names(table(grp))[table(grp) > 0], drop=TRUE]
g <- as.factor(grp)
ng <- nlevels(g)
grpA <- as.factor(grpA)
ntest <- length(testlist)
dataA <- as.matrix(dataA)
tretreA <- treetrans(tretreA,val)
W <- vcv.phylo(tretreA)
invW<-solve(W)
invW.eig <- eigen(invW)
N <- invW.eig$vectors %*% diag(sqrt(invW.eig$values)) %*% solve(invW.eig$vectors)
divnum <-det(N)^(1/nrow(N))
N <- N/divnum
invN <- solve(N)
y <- matrix(0,nA,nlevels(grpA))
for (i in 1:nA){y[i,grpA[i]] <- 1}
Y <- N%*%y #Dummy matrix with phylo bias removed
Y <- Y[-testlistn,1:ng]
DATAA <- N%*%as.matrix(dataA) #Rao (4,57); transforming the data to linear
DATA <- DATAA[-testlistn,]
DATAtest <- DATAA[testlistn,]
n<-nrow(DATA)
m<-nrow(DATAtest)
x <- DATA
fg <- factor(g)
prior <- colSums(Y)/sum(colSums(Y))
if(eqprior) prior <- c(rep(1/ng,ng))
#prior <- c(0.305, 0.237, 0.458) # Mammalian Prior
#prior <- c(0.288, 0.558, 0.154) # Avian Prior
if(priin != 1) prior<-priin
cnames <- levels(fg)
g <- as.numeric(fg)
J <- length(cnames)
weights <- rep(1, n)
dp <- tapply(weights, g, sum)/n
theta <- contr.helmert(J)
theta <- contr.fda(dp, theta)
Theta <- Y%*%theta #fda p.7, above eq2

fit <- method(x, Theta, weights)
rss <- t(Theta-fit$fitted) %*% (Theta-fit$fitted)
ssm <- t(Theta) %*% fitted(fit)/n
ed <- svd(ssm, nu = 0)
thetan <- ed$v
lambda <- ed$d
lambda[lambda > 1 - eps] <- 1 - eps
discr.eigen <- lambda/(1 - lambda)
pe <- (100 * cumsum(discr.eigen))/sum(discr.eigen)
dimension <- min(dimension, sum(lambda > eps))
if (dimension == 0) {
warning("degenerate problem; no discrimination")
return(structure(list(dimension = 0, fit = fit, call = this.call),
class = "fda"))
}
thetan <- thetan[, seq(dimension), drop = FALSE]
pe <- pe[seq(dimension)]
alpha <- sqrt(lambda[seq(dimension)])
sqima <- sqrt(1 - lambda[seq(dimension)])
vnames <- paste("v", seq(dimension), sep = "")
means <- scale(theta %*% thetan, FALSE, sqima/alpha)
dimnames(means) <- list(cnames, vnames)
names(lambda) <- c(vnames, rep("", length(lambda) - dimension))
names(pe) <- vnames
frml <- "grp~"
nc <- ncol(dataA)
varnam <- colnames(dataA)
for(i in 1:(nc-1)) frml <- paste(frml,varnam[i],"+", sep="")
frml <- paste(frml,varnam[nc], sep="")
frml <- as.formula(frml)
dset <- as.data.frame(cbind(grp,DATA))
Terms <- as.call(fda(formula = frml, data = dset, weights = weights))
obj <- structure(list(percent.explained = pe, values = lambda,
means = means, theta.mod = thetan, dimension = dimension,
prior = prior, fit = fit, call = this.call, terms = Terms),
class = "phylo.fda")
obj$confusion <- confusion(predict(obj), fg)
obj$prd <- predict(obj)
obj$x<-x
obj$g <- as.factor(grp)
obj$val <- val
obj$rss <- sum(diag(rss))
obj$intercept <- intercept
obj$eqprior <- eqprior
obj$DATAtest <- DATAtest
obj$DATA <- DATA
tpred <- predict(obj,DATAtest)
tpredn <- as.numeric(tpred)
tpred <- as.matrix(tpred)
rownames(tpred) <- testlist
obj$testprediction <- tpred
obj$testprediction_numeral <- tpredn
if (!keep.fitted)

obj$fit$fitted.values <- NULL
obj
}
###----------------------------------------------------------------------
### Function for optimal lambda value search
###----------------------------------------------------------------------
"phylo.RSS"<-function (datain,grp,tretre,val=1,treetrans=lambdaTree)
{
datainO <- as.matrix(datain)
datainI <- cbind(Intercept=rep(1,nrow(datainO)),datainO)
tretre <- treetrans(tretre,val)
n <- nrow(datain)
g <- as.factor(grp)
ng <- nlevels(g)
W <- vcv.phylo(tretre)
invW<-solve(W)
y <- matrix(0,n,ng) #Dummy matrix without phylo bias
for (i in 1:n){y[i,g[i]] <- 1}
invW.eig <- eigen(invW)
N <- invW.eig$vectors %*% diag(sqrt(invW.eig$values)) %*% solve(invW.eig$vectors)
Y <- N %*% y # Pretending that there is no phylogenetic bias in y; otherwise Y <- N%*%y
DATAI <- N%*%datainI
# BHAT <- solve(t(DATA)%*%DATA)%*%t(DATA)%*%Y
# YHAT <- DATA%*%BHAT
bhatI <- solve(t(datainI)%*%invW%*%datainI)%*%t(datainI)%*%invW%*%y #Rohlf (9) -- data biased still Rao (4,64)
yhatI <- datainI%*%bhatI #Rohlf (11)
RSSyI <- t(y-yhatI) %*% invW %*% (y-yhatI) #Martins and Hansen 1997 (9)
l0I<- lm(Y~DATAI-1)
# RSSY <- t(Y-YHAT) %*% (Y-YHAT)
list(RSS=sum(diag(RSSyI)),lLY=logLik(l0I),AICY=AIC(l0I),l0I=l0I)
}
#dataA=XA;grpA=gA;taxtaxA=taxaA;tretreA=treA;testlistn=testtaxan;val=0;treetrans=lambdaTree
"phylo.RSS.pred" <-function (dataA,grpA,taxtaxA,tretreA,testlistn,val=1,treetrans=lambdaTree)
{
dataA <- as.data.frame(dataA)
nA <- nrow(dataA)
testlist <- taxtaxA[testlistn]
traininglist <- taxtaxA[-testlistn]
rownames(dataA) <- taxtaxA
tretre <- drop.tip(tretreA,testlistn)
grp <- grpA[-testlistn]
grp <- grp[grp %in% names(table(grp))[table(grp) > 0], drop=TRUE]
g <- as.factor(grp)
ng <- nlevels(g)
grpA <- as.factor(grpA)
icptA <- rep(1,nA)
dataA <- cbind(icptA,dataA)
ntest <- length(testlist)
tretreA <- treetrans(tretreA,val)

W <- vcv.phylo(tretreA)
invW<-solve(W)
invW.eig <- eigen(invW)
N <- invW.eig$vectors %*% diag(sqrt(invW.eig$values)) %*% solve(invW.eig$vectors)
invN <- solve(N)
y <- matrix(0,nA,nlevels(grpA))
for (i in 1:nA){y[i,grpA[i]] <- 1}
Y <- N%*%y #Dummy matrix with phylo bias removed
Y <- Y[-testlistn,1:ng]
DATAA <- N%*%as.matrix(dataA) #Rao (4,57); transforming the data to linear
DATA <- DATAA[-testlistn,]
BHAT <- solve(t(DATA)%*%DATA)%*%t(DATA)%*%Y
YHAT <- DATA%*%BHAT
l0<- lm(Y~DATA-1)
RSSY <- t(Y-YHAT) %*% (Y-YHAT)
list(RSS=sum(diag(RSSY)),lLY=logLik(l0),AICY=AIC(l0))
}
#measurements=X;grps=g;mytree=tre;idc=filename_stem
"optLambda" <- function(measurements,grps,mytree,idc="default",sstep=0.01,srange=c(0,1),fldr="./")
{
lambdalist <- seq(min(srange),max(srange),sstep)
segnum <- length(lambdalist)
rslt<-matrix(,segnum,3)
colnames(rslt) <- c("Lambda","RSS","logLik")
for(i in 1:segnum){
lambdaval <- lambdalist[i]
rss <- phylo.RSS(X,grps,mytree,val=lambdaval)
rslt[i,] <- c(lambdaval,rss$RSS,rss$lLY)
}
optlambda <- matrix(,1,2);colnames(optlambda)<- c("RSS","logLik")
optlambda[1,1]<-max(rslt[which(rslt[,2]==min(rslt[,2])),1])
optlambda[1,2]<-max(rslt[which(rslt[,3]==max(rslt[,3]),1)])
x11();matplot(rslt[,1],rslt[,2],type="l",xlab=expression(lambda),ylab="RSS",main="RSS",lty=1,col=1)
abline(v=optlambda[1,1],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,1],sep=""))
x11();matplot(rslt[,1],rslt[,3],type="l",xlab=expression(lambda),ylab="log Likelihood",main="logLik",lty=1,col=1)
abline(v=optlambda[1,2],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,2],sep=""))
pdf(height=11,width=6,file=paste(fldr,idc,".optLambda.pdf",sep=''));layout(matrix(c(1,2),2,1))
matplot(rslt[,1],rslt[,2],type="l",xlab=expression(lambda),ylab="RSS",main="RSS",lty=1,col=1)
abline(v=optlambda[1,1],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,1],sep=""))
matplot(rslt[,1],rslt[,3],type="l",xlab=expression(lambda),ylab="log Likelihood",main="logLik",lty=1,col=1)
abline(v=optlambda[1,2],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,2],sep=""))
dev.off()
list(optlambda=optlambda,rslt=rslt)
}
# optLambda(X,grps, mytree, "LSSoft2_1000",0.001,c(0,0.2))
# optLambda(X,grps, mytree, "SHF_1000",0.001,c(0,1))
#measurementsA=XA;grpsA=gA;mytreeA=treA;testn=testtaxan;idc=filename_stem
"optLambda.pred" <- function(measurementsA,grpsA,taxaA,mytreeA,testn,idc="default",sstep=0.01,srange=c(0,1),fldr="./")

{
lambdalist <- seq(min(srange),max(srange),sstep)
segnum <- length(lambdalist)
rslt<-matrix(,segnum+1,3)
colnames(rslt) <- c("Lambda","RSS","logLik")
for(i in 1:segnum){
lambdaval <- lambdalist[i]
rss <- phylo.RSS.pred(measurementsA,grpsA,taxaA,mytreeA,testn,val=lambdaval)
rslt[i,] <- c(lambdaval,rss$RSS,rss$lLY)
}
optlambda <- matrix(,1,2);colnames(optlambda)<- c("RSS","logLik")
optlambda[1,1]<-max(rslt[which(rslt[,2]==min(rslt[,2])),1])
optlambda[1,2]<-max(rslt[which(rslt[,3]==max(rslt[,3]),1)])
x11();matplot(rslt[,1],rslt[,2],type="l",xlab=expression(lambda),ylab="RSS",main="RSS",lty=1,col=1)
abline(v=optlambda[1,1],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,1],sep=""))
x11();matplot(rslt[,1],rslt[,3],type="l",xlab=expression(lambda),ylab="log Likelihood",main="logLik",lty=1,col=1)
abline(v=optlambda[1,2],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,2],sep=""))
pdf(height=11,width=6,file=paste(fldr,idc,".optLambda.pred.pdf",sep=''));layout(matrix(c(1,2),2,1))
matplot(rslt[,1],rslt[,2],type="l",xlab=expression(lambda),ylab="RSS",main="RSS",lty=1,col=1)
abline(v=optlambda[1,1],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,1],sep=""))
matplot(rslt[,1],rslt[,3],type="l",xlab=expression(lambda),ylab="log Likelihood",main="logLik",lty=1,col=1)
abline(v=optlambda[1,2],col=2,lty=2);mtext(paste("Optimal Lambda = ",optlambda[1,2],sep=""))
dev.off()
list(optlambda=optlambda,rslt=rslt)
}
###----------------------------------------------------------------------
### Utility functions for plotting
###----------------------------------------------------------------------
addEllipseSer <- function(x, y, series=2, pval=0.95, num=30)
{
acc <- num
alpha <- 1-pval
vx <- var(x)
vy <- var(y)
vxy <- var(x, y)
lambda <- eigen(var(cbind(x, y)))$values
a <- sqrt(vxy^2/((lambda[2]-vx)^2+vxy^2))
b <- (lambda[2]-vx)*a/vxy
theta <- atan(a/b)
k <- sqrt(-2*log(alpha))
l1 <- sqrt(lambda[1])*k
l2 <- sqrt(lambda[2])*k
# x2 <- seq(-l1, l1, l1/acc)
pvec <- 0:num
x2right <- sin((pi*pvec)/(num*2))*l1
x2 <- c(-rev(x2right), x2right )
tmp <- 1-x2^2/l1^2
y2 <- l2*sqrt(ifelse(tmp < 0, 0, tmp))
x2 <- c(x2, rev(x2))
72
y2 <- c(y2, -rev(y2))
s0 <- sin(theta)
c0 <- cos(theta)
xx <- c0*x2+s0*y2+mean(x)
yy <- -s0*x2+c0*y2+mean(y)
#polygon(xx, yy, border=series)
matplot(xx,yy,xlim=range(x), ylim=range(y), type="l", add=TRUE, col=series, cex=1)
epp <- cbind(xx,yy)
return(epp)
}
addEllipseGrp <- function(x,y,grp, pval=0.95, num=30)
{
gnum <- nlevels(grp)
gnames <- levels(grp)
xrange <- cbind(min(x),max(x))
yrange <- cbind(min(y),max(y))
dset <- cbind(grp,x,y)
epnts <- 1:((num+1)*4)
for (i in 1:gnum)
{
dset1 <- dset[grp==gnames[i],]
if(is.vector(dset1)==TRUE){x1 <- dset1[2]; y1 <- dset1[3]} else{x1 <- dset1[,2]; y1 <- dset1[,3]}
epnts2 <- addEllipseSer(x1,y1,i,pval,num)
epnts <- cbind(epnts, epnts2)
}
}

