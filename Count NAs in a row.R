set.seed(1)
x <- matrix(sample(c(1, NA), 100, TRUE), nrow=10) # creat some data
x

# count number of NAs per row
numNAs <- apply(x, 1, function(z) sum(is.na(z)))
numNAs
 
 # remove rows with more than 3 NAs
x[!(numNAs > 3),]
 
# or

numNAs <- rowSums(is.na(z))
