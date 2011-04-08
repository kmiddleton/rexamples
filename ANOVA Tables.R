library(lme4)
library(memisc)

### create three models
fm1 <- lmer(Reaction ~ 1 + (Days|Subject), sleepstudy)
fm1.1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
fm1.2 <- lmer(Reaction ~ as.factor(Days) + (Days|Subject), sleepstudy)

### note: need to run the code below fro setCoefTemplate and
### getSummary.lmer first

mtable("Model 1"=fm1, "Model 2"=fm1.1, "Model 3"=fm1.2,
	coef.style = "est.ci", # using "homegrown" est.ci, specified above
	summary.stats=c("AIC","BIC"),
	getSummary = "getSummary.lmer")#,

setCoefTemplate(
est.ci=c(
	est = "($est:#)($p:*)",
	ci = "[($lwr:#),($upr:#)]"))

getSummary.lmer <- function (obj, alpha = 0.05, ...){
	require(lme4)
	smry <- summary(obj)
	#N <- if (length(weights(obj))) ### NOTE: how to deal with groups/samp size?
	#    sum(weights(obj))
	#else sum(smry$df[1:2])
	coef <- smry@coefs
	lower <- qnorm(p = alpha/2, mean = coef[, 1], sd = coef[, 2])
	upper <- qnorm(p = 1 - alpha/2, mean = coef[, 1], sd = coef[, 2])
	if (ncol(smry@coefs) == 3) {
		p <- (1 - pnorm(smry@coefs[,3]))*2 # NOTE: no p-values for lmer() due to
										   # unclear dfs; calculate p-values based on z
		coef <- cbind(coef, p, lower, upper)
	} else {
		coef <- cbind(coef, lower, upper) # glmer will have 4 columns with p-values
	}
	colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
	#phi <- smry$dispersion
	#LR <- smry$null.deviance - smry$deviance
	#df <- smry$df.null - smry$df.residual
	ll <- smry@AICtab[3][,1]
	deviance <- smry@AICtab[4][,1]
	#if (df > 0) {
	#    p <- pchisq(LR, df, lower.tail = FALSE)
	#    L0.pwr <- exp(-smry$null.deviance/N)
	#    McFadden <- 1 - smry$deviance/smry$null.deviance
	#    Cox.Snell <- 1 - exp(-LR/N)
	#    Nagelkerke <- Cox.Snell/(1 - L0.pwr)
	#} else {
	#    LR <- NA
	#    df <- NA
	#    p <- NA
	#    McFadden <- NA
	#    Cox.Snell <- NA
	#    Nagelkerke <- NA
	#}
	AIC <- smry@AICtab[1][,1] # NOTE: these are both data.frames? not sure why...
	BIC <- smry@AICtab[2][,1]
	### NOTE: don't see a similar slot for "xlevels" to get levels of
	###		factor variables used as predictors; for time being, force
	###		user to specify explicitly; nope that didn't work...
	#if (fac != NULL) {
	# 	n <- length(fac)
	#	xlevels <- vector(n, mode = "list")
	#	for (i in 1:n) {
	#		xlevels[i] <- levels(obj@frame[,fac[i]])
	#		}
	#	}
	#sumstat <- c(phi = phi, LR = LR, df = df, p = p, logLik = ll,
	#    deviance = deviance, McFadden = McFadden, Cox.Snell = Cox.Snell,
	#    Nagelkerke = Nagelkerke, AIC = AIC, BIC = BIC, N = N)
	sumstat <- c(logLik = ll, deviance = deviance, AIC = AIC, BIC = BIC)
	list(coef = coef, sumstat = sumstat,
		contrasts = attr(model.matrix(obj), "contrasts"),
		xlevels = NULL, call = obj@call)
}
