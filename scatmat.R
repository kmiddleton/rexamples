x <- rnorm(150)
y <- x + rnorm(150)
z <- y + rnorm(150)

scatmat <- function(..., nclass=NULL) {
    pairs(cbind(...), 
        panel=function(x,y){
            points(x,y)
            abline(lm(y~x), lty=2)
            lines(lowess(x,y))
            },
        diag.panel=function(x){
            par(new=T)
            hist(x, main="", axes=F, nclass=nclass)
            }
        )
    }
    
scatmat(x,y,z, nclass=12)