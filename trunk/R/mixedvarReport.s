## $Id$
mixedvarReport <- function(data,
                           vars,
                           panel,
                           treat,
                           longPanel=panel,
                           test=TRUE,
                           exclude1=TRUE,
                           long=FALSE,
                           npct=c('numerator', 'both', 'denominator', 'none'),
                           prmsd=FALSE,
                           contDataPlotType=c('bp', 'dot', 'raw'),
                           nmin=15,
                           categDataPlot=TRUE,
                           cdfPlot=length(levels(data[[treat]]))<=2,
                           contDataPlot=!cdfPlot,
                           Ohist = TRUE,
                           bpPrototype=FALSE,
                           digits=3,
                           append=FALSE,
                           Major=NULL,
                           MajorLabel='',
                           Majorvars=NULL,
                           cexMajor=0.7,
                           continuous=10,
                           nx=15,
                           keyloc=list(x=0.8, y=0.02),
                           landscape=FALSE,
                           size=NULL,
                           longtable=FALSE,
                           h=5,
                           w=6,
                           lines.page = 40,
                           clearPlots=FALSE,
                           auxCol=NULL,
                           ...) {

  npct <- match.arg(npct)
  contDataPlotType <- match.arg(contDataPlotType)
  pdesc <- switch(contDataPlotType,
                  bp  = 'Box-percentile plots',
                  dot = 'Quartiles',
                  raw = 'Raw data')
                   
  ## h and w pertain to plot.summary.formula.reverse for categorical vars 
  vars  <- unlist(vars)
  Treat <- data[[treat]]

  if(clearPlots) {
    cp <- function() {
      cat('\\clearpage\n',
          file=paste('gentex/',panel,'.tex',sep=''),
          append=TRUE)
    }
  } else {
    cp <- function() NULL
  }

  form <- as.formula(paste('Treat', paste(vars,collapse='+'), sep='~'))
  d <- summary(form, data=data, method='reverse',
               test=test, continuous=continuous, nmin=nmin)
  contVars = vars[d$type==2]
  categVars = vars[d$type==1]
  
  lp <-  paste(toupper(substring(longPanel,1,1)),
               substring(longPanel,2), sep='')
               
  #########################################################
  ### CLOSED REPORT
  #########################################################
  ### create a table
  #########################################################
  latex(d, prtest='P', digits=digits,
        file=paste('gentex/', panel, '.tex', sep=''),
        append=append, middle.bold=TRUE, exclude1=exclude1, long=long,
        npct=npct, prmsd=prmsd, caption=lp, where='hbp!', ctable=!longtable,
        size=size, landscape=landscape,
        longtable=longtable, lines.page=lines.page, auxCol=auxCol)
  #########################################################
  ### plot categorical data
  #########################################################
  if(categDataPlot) {
    if(any(d$type == 1)) {
      pn <- paste(panel, 'cat', sep='-')
      startPlot(pn, h=h, w=w)
      plot(d, which='cat', main='', ...)
      Key(keyloc)
      endPlot()
      cp()
      putFig(panel, pn,
             paste('Distribution of categorical', longPanel, 'variables'),
             paste('Distribution of categorical', longPanel, 'variables.',
                   'Proportions on the $x$-axis indicate the',
                   'treatment-specific proportion of subjects in',
                   'the category shown on the $y$-axis.'))
      cp()
    }
  }
  #########################################################
  ### plot continuous data
  #########################################################
  if (contDataPlot){
    if(any(d$type == 2) || length(Major)) {
      if(bpPrototype & contDataPlotType=='bp') {
        startPlot('bpplot-prototype', h=4)
        bpplt()
        endPlot()
        putFig(panel, 'bpplot-prototype',
               'Prototype box-percentile plot',
               paste('Prototype box-percentile plot to be used to',
                     'interpret the following plots'))
      }
      
      pn <- paste(panel, 'cont', sep='-')
      startPlot(paste(pn, '%d', sep=''), h=6, w=6)
      np <- plot(d, which='con', conType=contDataPlotType)
      endPlot()
      if(np > 0) {
        for(i in 1:np) {
          putFig(panel, paste(pn, i, sep=''),
                 paste(pdesc, 'for continuous', longPanel,
                       if(i>1) {
                         '(continued)'
                       } else {
                         ''
                       }),
                 paste(pdesc, ' for continuous ', longPanel,
                       if(i>1) {
                         ' (continued)'
                       } else {
                         if(contDataPlotType!='raw') {
                           paste('.  $x$-axes are scaled to the $0.025$ and',
                                 '$0.975$ quantiles when data are pooled',
                                 'over treatments.')
                         }
                       }, sep=''))
          cp()
        }
      }
    }
  }
  
  #########################################################
  ### plot CDF
  #########################################################
  if(cdfPlot) {
    pn <- paste(panel, 'ecdf', sep='-')
    startPlot(paste(pn, '%d', sep=''), h=6, w=6)
    mfrowSet(length(vars))
    np <- ecdf(data[vars], group=Treat,
                lwd=c(1, 2), col=gray(c(0, 0.7)), q=0.5,
                label.curves=FALSE)
    endPlot()
    if(np > 0) {
      for(i in 1:np) {
        putFig(panel, paste(pn, i, sep=''),
               paste('Cumulative distribution plots for',
                     'continuous', longPanel,
                     if(i>1) {
                       '(continued)'
                     } else {
                       ''
                     }),
               paste('Empirical cumulative distribution plots for ',
                     'continuous ', longPanel,
                     if(i>1) {
                       ' (continued)'
                     } else {
                       paste('. Reference lines are drawn at',
                             'treatment-specific median values.',
                             '\\protect\\treatkey')
                     }, sep=''))
        cp()
      }
    }
  }
  
  #########################################################
  ### OPEN REPORT
  #########################################################
  ### plot table
  #########################################################
  panel <- paste('O', panel, sep='')
  form <- as.formula(paste('~', paste(vars, collapse='+')))
  d <- summary(form, data=data, method='reverse',
                continuous=continuous, nmin=nmin)
  latex(d, digits=digits, file=paste('gentex/', panel, '.tex', sep=''),
        append=append, middle.bold=TRUE, exclude1=exclude1,
        long=long, npct=npct, prmsd=prmsd,
        caption=lp, where='hbp!', ctable=!longtable,
        size=size, landscape=landscape, longtable=longtable,
        lines.page=lines.page, auxCol=auxCol)
        
  #########################################################
  ### plot categorical data
  #########################################################
  categPanelName <- paste(panel, 'cat', sep='-')
  if(categDataPlot) {
    ##the following dummy data is necessary since function "hist.data.frame"
    ##does not work the way it should for categorical variables
    dummyLength <- 10
    dummyContData <- as.data.frame(matrix(rnorm(dummyLength*length(categVars)),
                                          nrow=dummyLength, ncol=length(categVars)))
    startPlot(paste(categPanelName, '%d', sep=''), h=6, w=6)
    mfrowSet(length(contVars))
    categ <- hist.data.frame(data[categVars])
    np <- hist.data.frame(dummyContData)
    endPlot()
    if(np > 0) {
      for(i in 1:np) {
        putFig(panel, paste(categPanelName, i, sep=''),
               paste('Frequencies of', longPanel, 'variables',
                     if(i>1) {
                       '(continued)'
                     } else {
                       ''
                     }))
        cp()
      }
    }
  }
  #########################################################
  ### plot histogram
  #########################################################
  histPanelName <- paste(panel, 'hist', sep='-')
  if(Ohist){
    startPlot(paste(histPanelName, '%d', sep=''), h=6, w=6)
    mfrowSet(length(contVars))
    np <- hist.data.frame(data[contVars])
    endPlot()
    if(np > 0) {
      for(i in 1:np) {
        putFig(panel, paste(histPanelName, i, sep=''),
               paste('Frequencies of', longPanel, 'variables',
                     if(i>1) {
                       '(continued)'
                     } else {
                       ''
                     }))
        cp()
      }
    }
  }
  #########################################################
  ### plot boxplot for every variable stratified by country
  #########################################################
  nv <- length(Majorvars)
  nm <- length(unique(Major))

  if(nv) {
    pn <- paste(panel, 'bw-major', sep='-')
    startPlot(pn,
              h=if(nv > 10) {
                7
              } else if(nm > 7) {
                6
              } else if(nv > 4) {
                5
              } else {
                4
              })
    mf <- mfrowSet(nv, trellis=TRUE)
    rw <- 1
    cl <- 1
    for(j in 1:nv) {
      x <- data[[Majorvars[j]]]
      p <- bwplot(Major ~ x, xlab=label(x), panel=panel.bpplot,
                  scales=list(cex=c(cexMajor, 1)), means=FALSE)
      print(p, split=c(cl, rw, mf[2], mf[1]), more=j < nv)
      cl <- cl + 1
      if(cl > mf[2]) {
        rw <- rw + 1
        cl <- 1
      }
    }
    endPlot()
    lcap <- paste('Box-percentile plots of selected', longPanel,
                  'variables stratified by', MajorLabel)

    putFig(panel, pn, lcap)
    cp()
  }

  invisible()
}
