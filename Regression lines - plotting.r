x1<-c(1,2,3,4)
x2<-c(5,6,7,8)
y1<-c(2,4,5,8)
y2<-c(10,11,12,16)
plot(x1,y1,xlim=c(0,10),ylim=c(0,20),col="blue")
points(x2,y2,col="red")


abline(lm(y1~x1),col="blue")
abline(lm(y2~x2),col="red")

# vs.

lines(x1, fitted(lm(y1 ~ x1)), col = "blue")
lines(x2, fitted(lm(y2 ~ x2)), col = "red")