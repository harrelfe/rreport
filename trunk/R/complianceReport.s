## $Id$
complianceReport <- function(comply, treat, time, times=NULL) {
  if(!is.numeric(comply))
    comply <-  1*(comply %in% c('Y','Yes','yes','YES'))
  if(length(times)) {
    s <- time %in% times
    comply <- comply[s]
    treat <- treat[s]
    time <- time[s]
  }
  Compliance <- comply
  latex(summary(Compliance ~ time+stratify(treat)), 
        file='compliance.tex', where='hbp!', ncaption=FALSE, ctable=TRUE)
  latex(summary(Compliance ~ time),
        file='Ocompliance.tex', where='hbp!', ncaption=FALSE, ctable=TRUE)
  invisible()
}
