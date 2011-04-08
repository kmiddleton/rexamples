y <- matrix(c(1, 2, NA, 4, 2, 1, NA, NA), ncol = 2)
apply(y, c(1,2), function(x){x[is.na(x)] <- 0; x})