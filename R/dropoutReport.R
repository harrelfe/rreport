#' Dropout Report
#'
#' summary
#'
#' details
#'
#' @param d.dropout NEEDDOC
#' @param dropout NEEDDOC
#' @param treat NEEDDOC
#' @param time.inc NEEDDOC
#' @param ylim NEEDDOC
#' @param panel NEEDDOC
#' @param what NEEDDOC
#' @param \dots NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

## $Id$
dropoutReport <- function(d.dropout, dropout, treat,
                          time.inc=NULL, ylim=c(0,1), panel="dropout", what="study", ...) {
  ### function for capitalizing the first letter of each word
  ### borrowed from function "toupper" help
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
  }
  S <- if(length(dropout)) Surv(d.dropout, dropout) else
      Surv(d.dropout)
  openPanel <- paste("O", panel, sep="")
  startPlot(openPanel, h=4)
  d <- data.frame(treat)
  d$S <- S
  f <- survfit(S, data=d)
  yl <- paste("Fraction Remaining in",.simpleCap(what))
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
  for(w in c(panel,openPanel)){
    figureCaption = paste("Distribution of time until dropout from",what)
    putFig(w, w, figureCaption,
           if(w==panel) paste(figureCaption,". \\protect\\treatkey", sep="")
           else figureCaption, append=FALSE)
  }
  invisible()
}
