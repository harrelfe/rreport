#' Mixed Variable Report
#'
#' summary
#'
#' details
#'
#' @param data NEEDDOC
#' @param vars NEEDDOC
#' @param panel NEEDDOC
#' @param treat NEEDDOC
#' @param longPanel NEEDDOC
#' @param ncaption NEEDDOC
#' @param tableref NEEDDOC
#' @param test NEEDDOC
#' @param exclude1 NEEDDOC
#' @param long NEEDDOC
#' @param npct NEEDDOC
#' @param prmsd NEEDDOC
#' @param contDataPlotType NEEDDOC
#' @param nmin NEEDDOC
#' @param categDataPlot NEEDDOC
#' @param cdfPlot NEEDDOC
#' @param contDataPlot NEEDDOC
#' @param Ohist NEEDDOC
#' @param Odotchart NEEDDOC
#' @param bpPrototype NEEDDOC
#' @param digits NEEDDOC
#' @param append NEEDDOC
#' @param Major NEEDDOC
#' @param MajorLabel NEEDDOC
#' @param Majorvars NEEDDOC
#' @param cexMajor NEEDDOC
#' @param continuous NEEDDOC
#' @param nx NEEDDOC
#' @param keyloc NEEDDOC
#' @param landscape NEEDDOC
#' @param size NEEDDOC
#' @param longtable NEEDDOC
#' @param h NEEDDOC
#' @param w NEEDDOC
#' @param lines.page NEEDDOC
#' @param clearPlots NEEDDOC
#' @param auxCol NEEDDOC
#' @param prn NEEDDOC
#' @param \dots NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

## $Id$
mixedvarReport <-
  function(data,
           vars,
           panel,
           treat,
           longPanel=panel,
           ncaption = '',
           tableref = panel,
           test=TRUE,
           exclude1=TRUE,
           long=TRUE,
           npct=c('numerator', 'both', 'denominator', 'none'),
           prmsd=FALSE,
           contDataPlotType=c('bp', 'dot', 'raw'),
           nmin=15,
           categDataPlot=!Odotchart,
           cdfPlot=length(levels(data[[treat]]))<=2,
           contDataPlot=!cdfPlot,
           Ohist = TRUE,
           Odotchart = TRUE,
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
           prn=TRUE,
           ...)
{
  npct <- match.arg(npct)
  contDataPlotType <- match.arg(contDataPlotType)
  pdesc <- switch(contDataPlotType,
                  bp  = 'Box-percentile plots',
                  dot = 'Quartiles',
                  raw = 'Raw data')
                   
  ## h and w pertain to plot.summary.formula.reverse for categorical vars 
  vars  <- unlist(vars)
  Treat <- data[[treat]]

  cp <- if(clearPlots)
    function()
      cat('\\clearpage\n',
          file=paste('gentex/',panel,'.tex',sep=''),
          append=TRUE)
  else function() NULL

  form <- as.formula(paste('Treat', paste(vars,collapse='+'), sep='~'))
  d <- summary(form, data=data, method='reverse',
               test=test, continuous=continuous, nmin=nmin)
  contVars = vars[d$type==2]
  categVars = vars[d$type==1]
  
  lp.lot <-  paste(toupper(substring(longPanel,1,1)),
               substring(longPanel,2), sep='')
  lp <-  paste(lp.lot, ncaption)

  ## CLOSED REPORT

  ## create a table

  latex(d, prtest='P', digits=digits,
        file=paste('gentex/', panel, '.tex', sep=''),
        append=append, middle.bold=TRUE, exclude1=exclude1, long=long,
        npct=npct, prmsd=prmsd, caption=lp, caption.lot = lp.lot,
        label = tableref,
        where='hbp!', ctable=!longtable,
        size=size, landscape=landscape,
        longtable=longtable, lines.page=lines.page, auxCol=auxCol,
        prn=prn, dotchart=TRUE)

  ## plot categorical data

  if(categDataPlot)
    {
    if(any(d$type == 1))
      {
        pn <- paste(panel, 'cat', sep='-')
        startPlot(pn, h=h, w=w)
        plot(d, which='cat', main='', groupfont=2, ...)
        Key(keyloc)
        endPlot()
        cp()
        putFig(panel, pn,
               paste('Distribution of the categorical variables in the `',
                     longPanel, '\' table', sep = ''),
               paste('Distribution of the categorical variables in the `',
                     longPanel, 
                     '\' table.  Proportions on the $x$-axis indicate the ',
                     'treatment-specific proportion of subjects in ',
                     'the category shown on the $y$-axis.', sep = ''))
        cp()
      }
  }

  ## plot continuous data

  if (contDataPlot)
    {
      if(any(d$type == 2) || length(Major))
        {
          if(bpPrototype & contDataPlotType=='bp')
            {
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
          if(np > 0)
            {
              for(i in 1:np)
                {
                  putFig(panel, paste(pn, i, sep=''),
                         paste(pdesc, 'of continuous continuous variables in the `', longPanel, '\' table',
                               if(i>1)' (continued)' else '',
                               sep = ''),
                         paste(pdesc,
                               ' of continuous continuous variables in the `',
                               longPanel, '\' table',
                               if(i>1) ' (continued)'
                               else if(contDataPlotType!='raw')
                               paste('.  $x$-axes are scaled to the $0.025$ and',
                                     '$0.975$ quantiles when data are pooled',
                                     'over treatments.'),
                               sep=''))
                  cp()
                }
            }
        }
    }
  

  ## plot CDF

  if(cdfPlot)
    {
      pn <- paste(panel, 'ecdf', sep='-')
      startPlot(paste(pn, '%d', sep=''), h=6, w=6)
      mfrowSet(length(vars))
      np <- Ecdf(data[vars], group=Treat,
                 lwd=c(1, 2), col=gray(c(0, 0.7)), q=0.5,
                 label.curves=FALSE)
      endPlot()
      if(np > 0)
        {
          for(i in 1:np)
            {
              putFig(panel, paste(pn, i, sep=''),
                     paste('Cumulative distribution plots of the ',
                           'continuous variables in the `',
                           longPanel, '\' table',
                           if(i>1) ' (continued)' else '',
                         sep = ''),
                     paste('Empirical cumulative distribution plots of the ',
                           'continuous variables in the `',
                           longPanel, '\' table ',
                           if(i>1) ' (continued)' else
                           paste('. Reference lines are drawn at',
                                 'treatment-specific median values.',
                                 '\\protect\\treatkey'),
                           sep=''))
              cp()
            }
        }
    }
  

  ## OPEN REPORT

  ## plot table

  panel <- paste('O', panel, sep='')
  form <- as.formula(paste('~', paste(vars, collapse='+')))
  d <- summary(form, data=data, method='reverse',
               continuous=continuous, nmin=nmin)
  latex(d, digits=digits, file=paste('gentex/', panel, '.tex', sep=''),
        append=append, middle.bold=TRUE, exclude1=exclude1,
        long=long, npct=npct, prmsd=prmsd,
        caption=lp, caption.lot = lp.lot, label = tableref,
        where='hbp!', ctable=!longtable,
        size=size, landscape=landscape, longtable=longtable,
        lines.page=lines.page, auxCol=auxCol, prn=prn, dotchart=Odotchart)
        

  ## plot categorical data

  categPanelName <- paste(panel, 'cat', sep='-')
  if(categDataPlot)
    {
      ##the following dummy data is necessary since function "hist.data.frame"
      ##does not work the way it should for categorical variables
      dummyLength <- 10
      dummyContData <- as.data.frame(matrix(rnorm(dummyLength*length(categVars)),
                                            nrow=dummyLength, ncol=length(categVars)))
      startPlot(paste(categPanelName, '%d', sep=''), h=6, w=6)
      mfrowSet(length(contVars))

      ## NOTE: hist.data.frame automatically uses the variable label or
      ##    variable name as the x-lab depending on the length of the
      ##    variable label
      ##		lab <- attr(v, "label")
      ##		lab <- if (length(lab) && nchar(lab) > 35) nam[j]
      
      categ <- hist.data.frame(data[categVars])
      np <- hist.data.frame(dummyContData)
      endPlot()
      if(np > 0)
        {
          for(i in 1:np)
            {
              putFig(panel, paste(categPanelName, i, sep=''),
                     paste('Distributions of the categorical variables in the `', longPanel, '\' table',
                     if(i>1) ' (continued)' else '',
                     sep = ''))
              cp()
            }
        }
    }

  ## plot histogram

  histPanelName <- paste(panel, 'hist', sep='-')
  if(Ohist)
    {
      startPlot(paste(histPanelName, '%d', sep=''), h=6, w=6)
      mfrowSet(length(contVars))
      ## NOTE: hist.data.frame automatically uses the variable label or variable name
      ##	as the x-lab depending on the length of the variable label
      ##		lab <- attr(v, "label")
      ##		lab <- if (length(lab) && nchar(lab) > 35) nam[j]
      np <- hist.data.frame(data[contVars], xlab = "blah")
      endPlot()
      if(np > 0)
        {
          for(i in 1:np)
            {
              putFig(panel, paste(histPanelName, i, sep=''),
                     paste('Distributions of the continuous variables in the `', longPanel, '\' table',
                     if(i > 1) ' (continued)' else '',
                           sep = ''))
              cp()
            }
        }
    }

  ## plot boxplot for every variable stratified by country

  nv <- length(Majorvars)
  nm <- length(unique(Major))
  
  if(nv)
    {
      pn <- paste(panel, 'bw-major', sep='-')
      startPlot(pn,
                h=if(nv > 10) 7
                else if(nm > 7) 6
                else if(nv > 4) 5
                else 4)
      mf <- mfrowSet(nv, trellis=TRUE)
      rw <- 1
      cl <- 1
      for(j in 1:nv)
        {
          x <- data[[Majorvars[j]]]
          p <- bwplot(Major ~ x, xlab=label(x), panel=panel.bpplot,
                      scales=list(cex=c(cexMajor, 1)), means=FALSE)
          print(p, split=c(cl, rw, mf[2], mf[1]), more=j < nv)
          cl <- cl + 1
          if(cl > mf[2])
            {
              rw <- rw + 1
              cl <- 1
            }
        }
      endPlot()
      lcap <- paste('Box-percentile plots of selected continuous variables in the `',
                    longPanel,
                    '\' table stratified by ', MajorLabel, sep = '')

      putFig(panel, pn, lcap)
      cp()
    }
  
  invisible()
}
