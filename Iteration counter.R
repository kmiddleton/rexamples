indexGenerator <- function(mue,sigma,duration,numOfSimulations,timeStep = 1/252,nameBasis="Index"){

	m <- mue
	s <- sigma
	T <- duration
	dt <- timeStep
	n <- numOfSimulations

# dataMatrix contains n simulations over T+1 years (from 0 to T)
dataMatrix <- matrix(1,nrow=n,ncol=T+1)

YearEndIndex <- seq(1:T)/dt + 1

PTIME <- proc.time()
for(i in 1:n){

Index_all <- seq(0,T,by=dt) + 1
Index_YE <- seq(1:(T+1))
time <- seq(0,T,by=dt)

ZFZ <- rnorm(length(Index_all)-1,0,1)

for(t in 2:length(Index_all)){
Index_all[t] <- Index_all[t-1] + m*Index_all[t-1]*dt + s*Index_all[t-1]*sqrt(dt)*ZFZ[t-1]
}

print(paste("Simulation",i,"of",n,"done."),quote=FALSE)
dataMatrix[i,2:(T+1)] <- t(Index_all[YearEndIndex])
}

tableName <- "dummy"
write.table(dataMatrix,tableName,sep = ";")

print(proc.time()-PTIME)
}
