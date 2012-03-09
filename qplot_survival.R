### create survival/cumulative incidence plot
require(survival)
require(ggplot2)

# define custom function to create a survival data.frame
createSurvivalFrame <- function(f.survfit){
  # initialise frame variable
  f.frame <- NULL
  
  # check if more then one strata
  if(length(names(f.survfit$strata)) == 0){
    # create data.frame with data from survfit
    f.frame <- data.frame(time=f.survfit$time, 
                          n.risk=f.survfit$n.risk, 
                          n.event=f.survfit$n.event, 
                          n.censor = f.survfit$n.censor, 
                          surv=f.survfit$surv, 
                          upper=f.survfit$upper, 
                          lower=f.survfit$lower)
    # create first two rows (start at 1)
    f.start <- data.frame(time=c(0, f.frame$time[1]), 
                          n.risk=c(f.survfit$n, f.survfit$n), 
                          n.event=c(0,0), 
                          n.censor=c(0,0), 
                          surv=c(1,1), 
                          upper=c(1,1), 
                          lower=c(1,1)) 
    # add first row to dataset
    f.frame <- rbind(f.start, f.frame)
    
    # remove temporary data
    rm(f.start)
  } 
  else {
    # create vector for strata identification
    f.strata <- NULL
    for(f.i in 1:length(f.survfit$strata)){
      # add vector for one strata according to number of rows of strata
      f.strata <- c(f.strata, rep(names(f.survfit$strata)[f.i], f.survfit$strata[f.i]))
    }
    
    # create data.frame with data from survfit (create column for strata)
    f.frame <- data.frame(time=f.survfit$time, n.risk=f.survfit$n.risk, n.event=f.survfit$n.event, n.censor = f.survfit$n.censor, surv=f.survfit$surv, upper=f.survfit$upper, lower=f.survfit$lower, strata=factor(f.strata))
    
    # remove temporary data
    rm(f.strata)
    
    # create first two rows (start at 1) for each strata
    for(f.i in 1:length(f.survfit$strata)){
      
      # take only subset for this strata from data
      f.subset <- subset(f.frame, strata==names(f.survfit$strata)[f.i])
      
      # create first two rows (time: 0, time of first event)
      f.start <- data.frame(time=c(0, f.subset$time[1]), n.risk=rep(f.survfit[f.i]$n, 2), n.event=c(0,0), n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1), strata=rep(names(f.survfit$strata)[f.i],2))	
      
      # add first two rows to dataset
      f.frame <- rbind(f.start, f.frame)
      
      # remove temporary data
      rm(f.start, f.subset)
      
    }
    
    # reorder data
    f.frame <- f.frame[order(f.frame$strata, f.frame$time), ]
    
    # rename row.names
    rownames(f.frame) <- NULL   
  }
  # return frame
  return(f.frame)
}

# define custom function to draw kaplan-meier curve with ggplot
qplot_survival <- function(f.frame, f.CI="default", f.shape=3){
  
  # use different plotting commands dependig whether or not strata's are given
  if("strata" %in% names(f.frame) == FALSE){
    
    # confidence intervals are drawn if not specified otherwise
    if(f.CI=="default" | f.CI==TRUE ){
      
      # create plot with 4 layers (first 3 layers only events, last layer only censored)
      # hint: censoring data for multiple censoring events at timepoint are overplotted
      # (unlike in plot.survfit in survival package)
      ggplot(data=f.frame) + geom_step(aes(x=time, y=surv), direction="hv") + geom_step(aes(x=time, y=upper), directions="hv", linetype=2) + geom_step(aes(x=time,y=lower), direction="hv", linetype=2) + geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
      
    }
    else {
      
      # create plot without confidence intervalls
      ggplot(data=f.frame) + geom_step(aes(x=time, y=surv), direction="hv") +  geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)
      
    }
    
  }
  else {
    
    if(f.CI=="default" | f.CI==FALSE){
      
      # without CI 
      ggplot(data=f.frame, aes(group=strata, colour=strata)) + geom_step(aes(x=time, y=surv), direction="hv") + geom_point(data=subset(f.frame, n.censor>0), aes(x=time, y=surv), shape=f.shape)
      
    }
    else {
      # with CI (hint: use alpha for CI)
      ggplot(data=f.frame, aes(colour=strata, group=strata)) + geom_step(aes(x=time, y=surv), direction="hv") + geom_step(aes(x=time, y=upper), directions="hv", linetype=2, alpha=0.5) + geom_step(aes(x=time,y=lower), direction="hv", linetype=2, alpha=0.5) + geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape) 
    } 
  } 
}


# # create frame from survival class (survfit)
# t.survfit <- survfit(t.Surv~1, data=lung)
# t.survframe <- createSurvivalFrame(t.survfit)
# 
# # create kaplan-meier-plot with ggplot
# qplot_survival(t.survframe)
# 
# 
# # drawing survival curves with several strata
# t.Surv <- Surv(lung$time, lung$status)
# t.survfit <- survfit(t.Surv~sex, data=lung)
# plot(t.survfit)
# 
# # two strata
# t.survframe <- createSurvivalFrame(t.survfit)
# qplot_survival(t.survframe)
# # with CI
# qplot_survival(t.survframe, TRUE)
# # add ggplot options, use different shape
# qplot_survival(t.survframe, TRUE, 1) + theme_bw() + scale_colour_manual(value=c("green", "steelblue")) + opts(legend.position="none")
# 
# # multiple stratas
# t.survfit <- survfit(t.Surv~ph.karno, data=lung)
# t.survframe <- createSurvivalFrame(t.survfit)
# qplot_survival(t.survframe)
# # plot without confidence intervals and with different shape
# qplot_survival(t.survframe, FALSE, 20)
