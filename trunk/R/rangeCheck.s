rangeCheck <- function(data, checks, panel, longPanel=panel,
                       vname=c('labels','names'),
                       makeNA=FALSE, append=FALSE) {
  
  vname <- match.arg(vname)
  n <- names(checks)
  if(any(n %nin% names(data))) stop('illegal variable names')

  Lab <- NULL
  Count <- NULL
  for(i in 1:length(checks)) {
    x     <- data[[n[i]]]
    check <- checks[i]
    bad   <- eval(parse(text=paste('x', check)))
    nbad  <- sum(bad, na.rm=TRUE)
    if(nbad) {
      lab <- label(x)
      if(lab == '' || vname=='names') lab <- n[i]
      u <- units(x)
      if(u != '')
        u <- paste(' {\\smaller[2] ', latexTranslate(u), '}', sep='')
      lab <- paste(lab, latexTranslate(check), u, sep='')
      Lab <- c(Lab, lab)
      Count <- c(Count, nbad)
      if(makeNA) data[bad & !is.na(x), n[i]] <- NA
    }
  }
  if(length(Lab)) {
    names(Count) <- Lab
    latex(Count, title=panel,
          colheads=c('Error','Frequency'),
          caption=paste('Frequencies of out of range values in',
            longPanel),
          where='hbp!', ctable=TRUE, append=append)
  } else cat('There were no out of range values for ', longPanel, '.\n',
             file=paste(panel,'tex',sep='.'), append=append, sep='')

  if(makeNA) return(invisible(data))
}
