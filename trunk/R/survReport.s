## $Id$
survReport <- function(etime, event, treat,
                       ylabel='Survival Probability',
                       conf=c('bars','bands','none'),
                       n=NULL, labels=NULL, previousActual=NULL, h=4, 
		       append=FALSE, ...) {
  require('survival')
  require('Design')
  
  startPlot('surv-km', h=h)
  S <- Surv(etime, event)
  treat <- as.factor(treat)
  lwd <- c(1,2); lty=c(1,1); col=gray(c(0,.7))
  
  survplot(survfit(S ~ treat), n.risk=TRUE, conf=conf, lwd=lwd,
           lty=lty, col=col, ylabel=ylabel)
  endPlot()
  putFig('surv', 'surv-km',
         'Kaplan-Meier cumulative event-free probability estimates by treatment',
         'Kaplan-Meier cumulative event-free probability estimates.  \\protect\\treatkey', append=append)
  
  if(length(n)) {
    p <- ldBands(n=n, ...)
    i <- is.na(S) | is.na(treat)
    if(any(is.na(i))) {
      S <- S[!i,]
      treat <- treat[!i]
    }
    z <- logrank(S, as.integer(treat))
    startPlot('surv-monitor', h=h)
    plot(p, labels=labels, actual=c(z, previousActual))
    endPlot()
    putFig('surv','surv-monitor',
           'Group-sequential monitoring boundaries',
           paste('Lan-DeMets group-sequential monitoring boundaries using the Obrien-Fleming alpha-spending function with',n,'looks equally spaced in time.  Points indicate observed Cox-logrank Z statistics.'), append=append)
  }
}
