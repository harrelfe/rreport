## $Id$
mixedvarReport <- function(data, vars, panel, treat,
                           longPanel=panel, test=TRUE, exclude1=TRUE,
                           cdf=FALSE, Ohist=TRUE,
                           bpPrototype=FALSE, digits=3, append=FALSE,
                           Major=NULL, MajorLabel='',
                           Majorvars=NULL, cexMajor=.7, continuous=10,
                           pl=TRUE, size=NULL, h=5, w=6,
                           clearPlots=FALSE, ...) {

  ## h and w pertain to plot.summary.formula.reverse for categorical vars 
  vars  <- unlist(vars)
  Treat <- data[[treat]]

  cp <- function()
    if(clearPlots)
      cat('\\clearpage\n', file=paste('gentex/',panel,'.tex',sep=''),
          append=TRUE)

  form <- as.formula(paste('Treat', paste(vars,collapse='+'), sep='~'))
  d <- summary(form, data=data, method='reverse', test=test, continuous=continuous)
  lp <-  paste(toupper(substring(longPanel,1,1)),
               substring(longPanel,2), sep='')
  latex(d, prtest='P', digits=digits,
        file=paste('gentex/',panel, '.tex', sep=''),
        append=append, middle.bold=TRUE, exclude1=exclude1,
        caption=paste(lp,'variables'), where='hbp!', ctable=TRUE, size=size)
  if(pl) {
    if(any(d$type == 1)) {
      pn <- paste(panel, 'cat', sep='-')
      startPlot(pn, h=h, w=w)
      plot(d, which='cat', main='', ...)
      Key(.8, .02)  #.075)
      endPlot()
      cp()
      putFig(panel, pn,
             paste('Distribution of categorical',longPanel,'variables'),
             paste('Distribution of categorical',longPanel,'variables.',
                   'Proportions on the $x$-axis indicate the',
                   'treatment-specific proportion of subjects in',
                   'the category shown on the $y$-axis.'))
      cp()
    }
    if(any(d$type == 2) || length(Major)) {
      if(bpPrototype) {
        startPlot('bpplot-prototype', h=4)
        bpplt()
        endPlot()
        putFig(panel, 'bpplot-prototype',
               'Prototype box-percentile plot',
               paste('Prototype box-percentile plot to be used to',
                     'interpret the following plots'))
      }

      pn <- paste(panel, 'cont', sep='-')
      startPlot(paste(pn,'%d',sep=''), h=6, w=6)
      np <- plot(d, which='con', conType='bp')
      endPlot()
      for(i in 1:np) {
        putFig(panel, paste(pn, i, sep=''),
               paste('Box-percentile plots for continuous',longPanel,'variables',
                     if(i>1) '(continued)' else ''),
               paste('Box-percentile plots for continuous ',longPanel,
                     ' variables',
                     if(i>1) ' (continued)' else
                     paste('.  $x$-axes are scaled to the $0.025$ and',
                           '$0.975$ quantiles when data are pooled',
                           'over treatments.'),
                     sep=''))
        cp()
      }
  
      if(cdf) {
        pn <- paste(panel, 'ecdf', sep='-')
        startPlot(paste(pn,'%d',sep=''), h=6, w=6)
        mfrowSet(length(vars))
        np <- ecdf(data[vars], group=Treat,
                   lwd=c(1,2), col=gray(c(0,.7)), q=.5,
                   label.curves=FALSE)
        endPlot()
        for(i in 1:np) {
          putFig(panel, paste(pn, i, sep=''),
                 paste('Cumulative distribution plots for',
                       'continuous', longPanel, 'variables',
                       if(i>1)'(continued)' else ''),
                 paste('Empirical cumulative distribution plots for ',
                       'continuous ', longPanel,' variables',
                       if(i>1)' (continued)' else 
                       paste('. Reference lines are drawn at',
                             'treatment-specific median values.',
                             '\\protect\\treatkey'),
                       sep=''))
          cp()
        }
      }
    }
  }

    panel <- paste('O', panel, sep='')
    form <- as.formula(paste('~', paste(vars,collapse='+')))
    d <- summary(form, data=data, method='reverse', continuous=continuous)
    latex(d, digits=digits, file=paste('gentex/',panel, '.tex', sep=''),
        append=append, middle.bold=TRUE, exclude1=exclude1,
        caption=paste(lp,'variables'), where='hbp!', ctable=TRUE, size=size)
  if(!pl) return(invisible())
  if(any(d$type == 1)) {
    pn <- paste(panel, 'cat', sep='-')
    startPlot(pn, h=h, w=w)
    plot(d, which='cat', main='', ...)
    endPlot()
    cp()
    putFig(panel, pn,
           paste('Distribution of categorical', longPanel, 'variables'),
           paste('Distribution of categorical', longPanel, 'variables.',
                 'Proportions on the $x$-axis indicate the',
                 'proportion of subjects in',
                 'the category shown on the $y$-axis.'))
    cp()
  }
  if(any(d$type == 2)) {
    if(cdf) {
      pn <- paste(panel, 'ecdf', sep='-')
      startPlot(paste(pn, '%d', sep=''), h=6, w=6)
      mfrowSet(length(vars))
      np <- ecdf(data[vars], lwd=1, q=(1:3)/4)
      endPlot()
      for(i in 1:np) {
        putFig(panel, paste(pn, i, sep=''),
               paste('Cumulative distribution plots for continuous',
                     longPanel, 'variables',
                     if(i>1)'(continued)' else ''),
               paste('Empirical cumulative distribution plots for ',
                     'continuous ', longPanel, ' variables',
                     if(i>1)' (continued)' else 
                     '. Reference lines are drawn at quartiles.',
                     sep=''))
        cp()
      }
    }
    if(Ohist) {
      pn <- paste(panel, 'hist', sep='-')
      startPlot(paste(pn, '%d', sep=''), h=6, w=6)
      mfrowSet(length(vars))
#    omf <- mfrowSet(nv)
#    on.exit(par(mfrow=omf))
#    mf <- par('mfrow')
      np <- hist.data.frame(data[vars])
      endPlot()
      for(i in 1:np) {
        putFig(panel, paste(pn, i, sep=''),
               paste('Histograms of', longPanel, 'variables',
                     if(i>1)'(continued)' else ''))
        cp()
      }
    }
  }

  nv <- length(Majorvars)
  nm <- length(unique(Major))
  if(nv) {
    pn <- paste(panel, 'bw-major', sep='-')
    startPlot(pn,
              h=if(nv > 10) 7 else if(nm > 7) 6 else if(nv > 4) 5 else 4)
    mf <- mfrowSet(nv, trellis=TRUE)
    rw <- 1; cl <- 1
    for(j in 1:nv) {
      x <- data[[Majorvars[j]]]
      p <- bwplot(Major ~ x, xlab=label(x), panel=panel.bpplot,
                  scales=list(cex=c(cexMajor,1)), means=FALSE)
      print(p, split=c(cl,rw,mf[2],mf[1]),more=j < nv)
      cl <- cl + 1
      if(cl > mf[2]) {
        rw <- rw + 1
        cl <- 1
      }
    }
    endPlot()
    lcap <- paste('Box-percentile plots of selected', longPanel,
                  'variables stratified by',
                  MajorLabel)
    putFig(panel, pn, lcap)
    cp()
    
  }
  invisible()
}
