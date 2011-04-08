expected <- c(202, 202)
observed <- c(201, 203)
 
chisq.test(observed, p = expected, correct = TRUE, rescale.p = TRUE)
