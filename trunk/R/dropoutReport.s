dropoutReport <- function(d.dropout, dropout, treat,
                          time.inc=NULL, ylim=c(0,1)) {
  require('survival')
  require('Design')
  S <- if(length(dropout)) Surv(d.dropout, dropout) else
      Surv(d.dropout)
  startPlot('Odropout', h=4)
  d <- data.frame(S, treat)
  f <- survfit(S, data=d)
  yl <- 'Fraction Remaining in Study'
  lwd <- c(1,2); lty=c(1,1); col=gray(c(0,.7))
  if(length(time.inc))
    survplot(f, time.inc=time.inc, n.risk=TRUE, conf='none', ylab=yl,
      lwd=lwd, lty=lty, col=col)
      else survplot(f, conf='none', ylab=yl, lwd=lwd, lty=lty, col=col)
  endPlot()
  startPlot('dropout', h=4)
  f <- survfit(S ~ treat, data=d)
  if(length(time.inc))
    survplot(f, time.inc=time.inc, n.risk=TRUE, conf='none', ylab=yl,
             lwd=lwd, lty=lty, col=col, label.curves=FALSE)
      else survplot(f, conf='none', ylab=yl,
                    lwd=lwd, lty=lty, col=col, label.curves=FALSE)
  endPlot()
  for(w in c('dropout','Odropout'))
    putFig(w, w,
           'Distribution of time until dropout',
           if(w=='dropout')
           'Distribution of time until dropout. \\protect\\treatkey'
           else 'Distribution of time until dropout',
           append=FALSE)
  invisible()
}
