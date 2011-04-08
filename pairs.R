x <- rnorm(150)
y <- x + rnorm(150)

pairs(cbind(x,y), 
    panel=function(x,y){
        points(x,y)
        abline(lm(y~x), lty=2)
        lines(lowess(x,y))
        },
    diag.panel=function(x){
        par(new=T)
        hist(x, main="", axes=F, nclass=12)
        }
    )