source('http://biostat.mc.vanderbilt.edu/wiki/pub/Main/TatsukiRcode/TatsukiRcodeTplot.r')

set.seed(100)
age <- rnorm(80,rep(c(26,36),c(70,10)),4)
sex <- sample(c('Female','Male'),80,T)
group <- paste('Group ', sample(1:4,40,prob=c(2,5,4,1),replace=T), sep='')
d <- data.frame(age,sex,group)
tplot(age~group,data=d,show.n=T,type=c('db','db','db','d'),col=c(d$sex),dist=.2,jit=.05,  las=1,pch=19)
legend('topright',pch=19,col=1:2,legend=c('Female','Male'))
