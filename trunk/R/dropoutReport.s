## $Id$
dropoutReport <- function(d.dropout, dropout, treat,
                          time.inc=NULL, ylim=c(0,1), panel="dropout", ...) {
  require('survival')
  require('Design')
  S <- if(length(dropout)) Surv(d.dropout, dropout) else
      Surv(d.dropout)
  openPanel <- paste("O", panel, sep="")
  startPlot(openPanel, h=4)
  d <- data.frame(treat)
  d$S <- S
  f <- survfit(S, data=d)
  yl <- 'Fraction Remaining in Study'
  lwd <- c(1,2); lty=c(1,1); col=gray(c(0,.7))
  if(length(time.inc))
    survplot(f, time.inc=time.inc, n.risk=TRUE, conf='none', ylab=yl,
      lwd=lwd, lty=lty, col=col, ylim=ylim, ...) else
      survplot(f, conf='none', ylab=yl, lwd=lwd, lty=lty, col=col,
               ylim=ylim, n.risk=TRUE, ...)
               
  endPlot()
  startPlot(panel, h=4)
  f <- survfit(S ~ treat, data=d)
  if(length(time.inc))
    survplot(f, time.inc=time.inc, n.risk=TRUE, conf='none', ylab=yl,
             lwd=lwd, lty=lty, col=col, label.curves=FALSE, ylim=ylim,
             ...)
      else survplot(f, conf='none', ylab=yl, n.risk=TRUE,
                    lwd=lwd, lty=lty, col=col, label.curves=FALSE,
                    ylim=ylim, ...)
  endPlot()
  for(w in c(panel,openPanel))
    putFig(w, w,
           'Distribution of time until dropout',
           if(w==panel)
           'Distribution of time until dropout. \\protect\\treatkey'
           else 'Distribution of time until dropout',
           append=FALSE)
  invisible()
}
