Vcov <- vcov(FM3, useScale = FALSE)
betas <- fixef(FM3)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
###############
cbind(betas, se, zval, pval)

