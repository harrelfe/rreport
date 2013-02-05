#' Mixed Variable Report
#'
#' Generate several reports.
#'
#' \itemize{
#' \item Generate a summary table of treatment against all variables.
#' \item Plot categorical data if \code{categDataPlot} is \sQuote{TRUE}.
#' \item Plot continuous data if \code{contDataPlot} is \sQuote{TRUE}.
#' \item Include a prototype box-percentile plot if \code{contDataPlotType}.
#' is \sQuote{bp} and \code{bpPrototype} is \sQuote{TRUE}.
#' \item Plot the cumulative distribution function.
#' \item Plot a histogram for categorical variables if \code{Ohist} is \sQuote{TRUE}.
#' \item Print a box-percentile plot for each variable stratified by \code{Major}.
#' }
#'
#' @param data data.frame. Data used for report.
#' @param vars character vector. Variables to include in analysis.
#' @param panel character. Name for panel.
#' @param treat character. Name of treatment variable within dataset.
#' @param longPanel character. Long name for panel.
#' @param ncaption character. Caption text.
#' @param tableref character. Passed to the function \code{\link[Hmisc]{latex}}.
#' @param test logical. See \code{\link[Hmisc]{summary.formula}}.
#' @param exclude1 logical. See \code{\link[Hmisc]{summary.formula}}.
#' @param long logical. See \code{\link[Hmisc]{summary.formula}}.
#' @param npct character. See \code{\link[Hmisc]{summary.formula}}.
#' @param prmsd logical. See \code{\link[Hmisc]{summary.formula}}.
#' @param contDataPlotType character. Continuous data plot type should be one of the following:
#' \sQuote{bp}, \sQuote{dot}, or \sQuote{raw}.
#' @param nmin numeric. See \code{\link[Hmisc]{summary.formula}}.
#' @param categDataPlot logical. Set to \sQuote{TRUE} to output a categorical
#' data plot in the latex table being generated.
#' @param cdfPlot logical. Set to \sQuote{TRUE} to output a CDF plot
#' in the latex table being generated; only available to closed report.
#' @param contDataPlot logical. Set to \sQuote{TRUE} to output a continous 
#' data plot in the latex table being generated; only available to closed report.
#' @param Ohist logical. Set to \sQuote{TRUE} to output a histogram of the continuous variables.
#' @param Odotchart logical. See \code{\link[Hmisc]{summary.formula}}.
#' @param bpPrototype logical. If \sQuote{TRUE} and \code{contDataPlotType} is \sQuote{bp},
#' output a prototype box-percentile plot.
#' @param digits numeric. Significant digits, passed to the function \code{\link[Hmisc]{latex}}.
#' @param append logical. If \sQuote{TRUE} output will be appended instead of overwritten.
#' @param Major character vector. Major categorical variables for site.
#' @param MajorLabel character. Label for major stratification variable(s).
#' @param Majorvars character vector. Major categorical variables for site. (NEED TO DISTINGUISH)
#' @param cexMajor numeric. Plotting text size magnification, used for boxplots.
#' @param continuous numeric. See \code{\link[Hmisc]{summary.formula}}.
#' @param nx numeric. THIS PARAMETER IS NOT USED.
#' @param keyloc numeric vector. Key placement on the categorical data plot, see \code{\link[Hmisc]{summary.formula}}.
#' @param landscape logical See \code{\link[Hmisc]{latex}}.
#' @param size character. See \code{\link[Hmisc]{latex}}.
#' @param longtable logical. See \code{\link[Hmisc]{latex}}.
#' @param h numeric. Height of plot, only used for categorical data plot. Default is 5in. See \code{\link[Hmisc]{setps}}.
#' @param w numeric. Width of plot, only used for categorical data plot. Default is 6in. See \code{\link[Hmisc]{setps}}.
#' @param lines.page numeric. See \code{\link[Hmisc]{latex}}.
#' @param clearPlots logical. Set to \sQuote{TRUE} to clear the page after each plot.
#' @param auxCol list. See \code{\link[Hmisc]{summary.formula}}.
#' @param prn logical. See \code{\link[Hmisc]{summary.formula}}.
#' @param \dots Additional arguments, passed to \code{\link[graphics]{plot}}. Only used for categorical data plot.
#' @export

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

  closed.file <- file.path(TexDirName(FALSE), sprintf("%s.tex", panel))
  latex(d, prtest='P', digits=digits,
        file=closed.file,
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
  open.file <- file.path(TexDirName(), sprintf("%s.tex", panel))
  latex(d, digits=digits, file=open.file,
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
