## $Id$
baselineReport <- function(data, vars, treat, cdf=FALSE, Ohist=TRUE,
                           bpPrototype=TRUE, digits=3, append=FALSE,
                           Major=NULL, MajorLabel='',
                           Majorvars=NULL, cexMajor=.7,
                           ...) {
 
  vars  <- unlist(vars)
  Treat <- data[[treat]]

  form <- as.formula(paste('Treat', paste(vars,collapse='+'), sep='~'))
  d <- summary(form, data=data, method='reverse', test=TRUE)
  latex(d, prtest='P', digits=digits, file='baseline.tex',
        append=append, middle.bold=TRUE,
        caption='Baseline Variables', where='hbp!', ctable=TRUE)
  if(any(d$type == 1)) {
    startPlot('baselinecat', h=5, w=6) # was h=5 w=6
    plot(d, which='cat', main='', ...)
    Key(.8, .02)  #.075)
    endPlot()
    putFig('baseline', 'baselinecat',
           'Distribution of categorical baseline variables',
           paste('Distribution of categorical baseline variables.',
                 'Proportions on the $x$-axis indicate the',
                 'treatment-specific proportion of subjects in',
                 'the category shown on the $y$-axis.'))
  }
  if(any(d$type == 2) || length(Major)) {
    if(bpPrototype) {
      startPlot('bpplot-prototype', h=4)
      bpplt()
      endPlot()
      putFig('baseline', 'bpplot-prototype',
             'Prototype box-percentile plot',
             paste('Prototype box-percentile plot to be used to',
                   'interpret the following plots'))
    }
  
    startPlot('baseline-cont%d', h=6, w=6)
    np <- plot(d, which='con', conType='bp')
    endPlot()
    for(i in 1:np)
      putFig('baseline', paste('baseline-cont',i,sep=''),
             paste('Box-percentile plots for continuous baseline variables',
                   if(i>1) '(continued)' else ''),
             paste('Box-percentile plots for continuous baseline ',
                   'variables',
                   if(i>1) ' (continued)' else
                   paste('.  $x$-axes are scaled to the $0.025$ and',
                         '$0.975$ quantiles when data are pooled',
                         'over treatments.'),
                   sep=''))
  
    if(cdf) {
      startPlot('baseline-ecdf%d', h=6, w=6)
      mfrowSet(length(vars))
      np <- ecdf(data[vars], group=Treat,
                 lwd=c(1,2), col=gray(c(0,.7)), q=.5,
                 label.curves=FALSE)
      ##         label.curves=list(keys='lines'))
      endPlot()
      for(i in 1:np)
        putFig('baseline', paste('baseline-ecdf',i,sep=''),
               paste('Cumulative distribution plots for',
                     'continuous baseline variables',
                     if(i>1)'(continued)' else ''),
               paste('Empirical cumulative distribution plots for ',
                     'continuous baseline variables',
                     if(i>1)' (continued)' else 
                     paste('. Reference lines are drawn at',
                           'treatment-specific median values.',
                           '\\protect\\treatkey'),
                     sep=''))
    }
  }
  form <- as.formula(paste('~', paste(vars,collapse='+')))
  d <- summary(form, data=data, method='reverse')
  latex(d, digits=digits, file='Obaseline.tex',
        append=append, middle.bold=TRUE,
        caption='Baseline Variables', where='hbp!', ctable=TRUE)
  if(any(d$type == 1)) {
    startPlot('Obaselinecat', h=5, w=6) # was h=5 w=6
    plot(d, which='cat', main='', ...)
    endPlot()
    putFig('Obaseline', 'Obaselinecat',
           'Distribution of categorical baseline variables',
           paste('Distribution of categorical baseline variables.',
                 'Proportions on the $x$-axis indicate the',
                 'proportion of subjects in',
                 'the category shown on the $y$-axis.'))
  }
  if(any(d$type == 2)) {
    if(cdf) {
      startPlot('Obaseline-ecdf%d', h=6, w=6)
      mfrowSet(length(vars))
      np <- ecdf(data[vars], lwd=1, q=(1:3)/4)
      endPlot()
      for(i in 1:np)
        putFig('Obaseline', paste('Obaseline-ecdf',i,sep=''),
               paste('Cumulative distribution plots for continuous',
                     'baseline variables',
                     if(i>1)'(continued)' else ''),
               paste('Empirical cumulative distribution plots for ',
                     'continuous baseline variables',
                     if(i>1)' (continued)' else 
                     '. Reference lines are drawn at quartiles.',
                     sep=''))
    }
    if(Ohist) {
      startPlot('Obaseline-hist%d', h=6, w=6)
      mfrowSet(length(vars))
#    omf <- mfrowSet(nv)
#    on.exit(par(mfrow=omf))
#    mf <- par('mfrow')
      np <- hist.data.frame(data[vars])
      endPlot()
      for(i in 1:np)
        putFig('Obaseline', paste('Obaseline-hist',i,sep=''),
               paste('Histograms of baseline variables',
                     if(i>1)'(continued)' else ''))
    }
  }

  nv <- length(Majorvars)
  nm <- length(unique(Major))
  if(nv) {
    startPlot('Obaseline-bw-major',
#              h=if(nv > 7) 7 else if(nm > 7) 6 else 5, w=6)
              h=if(nv > 10) 7 else if(nm > 7) 6 else if(nv > 4) 5 else 4)
    mf <- mfrowSet(nv, trellis=TRUE)
    rw <- 1; cl <- 1
    for(j in 1:nv) {
      x <- data[[Majorvars[j]]]
      p <- bwplot(Major ~ x, xlab=label(x), panel=panel.bpplot,
                  scales=list(cex=c(cexMajor,1)))
      print(p, split=c(cl,rw,mf[2],mf[1]),more=j < nv)
      cl <- cl + 1
      if(cl > mf[2]) {
        rw <- rw + 1
        cl <- 1
      }
    }
    endPlot()
    lcap <- paste('Box-percentile plots of selected baseline variables stratified by',
                  MajorLabel)
    putFig('Obaseline','Obaseline-bw-major', lcap)
    
  }
  invisible()
}
