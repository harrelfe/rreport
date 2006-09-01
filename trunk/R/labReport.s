
## $Id$
labReport <- function(data, vars, panel, treat, id, time, times,
                      longPanel=panel, h=6.5, w=6.5,
                      diffs=FALSE, cdfPlot=length(levels(data[[treat]]))<=2,
                      tables=TRUE, boxPlot=!cdfPlot,
                      digits=3, append=FALSE, clearPlots=FALSE, open=TRUE) {

  ##################################################
  ### By default the function plots CDFs if the number of treatment
  ### groups is less or equal than two. 
  ### If the number of treatment groups is three or more
  ### boxplots are plotted.
  ##################################################
  ###  FUTURE PLANS:
  ### It is a good idea to implement the same argument as in mixedVarReport()
  ### contDataPlotType=c('bp','dot', 'raw')
  ### contDataPlotType <- match.arg(contDataPlotType)
  ### some lines of the code are already preparing for that.
  
  contDataPlotType!="bp"
  continuous <- 10
  pdesc <- switch(contDataPlotType,
                  bp='Box-percentile plots',
                  dot='Quartiles',
                  raw='Raw data')
  vars  <- unlist(vars)
  Treat <- data[[treat]]
  Time  <- data[[time]]
  subject  <- data[[id]]

  makerep <- function(Treat, panel) {
  g <- function(y) {
    y <- sort(y[!is.na(y)])
    n <- length(y)
    if(n < 4) return(c(median=median(y),q1=NA,q3=NA,variance=NA))
    qu <- quantile(y, c(.5,.25,.75))
    names(qu) <- NULL
    r <- pmin(qbinom(c(.025,.975), n, .5) + 1, n)  ## Exact 0.95 C.L.
    w <- y[r[2]] - y[r[1]]                         ## Width of C.L.
    var.med <- ((w/1.96)^2)/4      ## Approximate variance of median
    c(median=qu[1], q1=qu[2], q3=qu[3], variance=var.med)
  }
  Treat <- as.factor(Treat)
  trlev <- levels(Treat)
  if(!length(trlev)) stop(paste(treat,'is not a factor variable'))
  if(length(trlev) > 2) stop('only handles one or two treatments')

  i <- 0
  nv <- length(vars)
  h <- w <- 4.5*(nv < 7) + 5.5*(nv >= 7 & nv <=9) + 6.5*(nv > 9)

  startPlot(paste(panel,'%d',sep=''), h=h, w=w)
  omf <- mfrowSet(nv)
  mf <- par('mfrow')

  for(y in data[vars]) {
    i <- i + 1
    z <- summarize(y, llist(Treat,Time), g)
    curves <- vector('list', length(trlev)*3)
    names(curves) <- if(length(trlev)==1) c('','q1','q3') else
    c(paste(trlev[1],c('','q1','q3'),sep=''),
      paste(trlev[2],c('','q1','q3'),sep=''))
    
    j <- 0
    for(tr in trlev) {
      s <- z$Treat == tr
      curves[[j+1]] <- list(z$Time[s],z$y [s])
      curves[[j+2]] <- list(z$Time[s],z$q1[s])
      curves[[j+3]] <- list(z$Time[s],z$q3[s])
      j <- j+3
    }
    labcurve(curves, ylim=quantile(y,c(.05,.95),na.rm=TRUE),
             xlab=label(Time), ylab=label(y, units=TRUE, plot=TRUE),
             col=gray(c(rep(0,3),rep(.7,3))),
             lty=1, lwd=rep(c(3,1,1),2),
             method='none', pl=TRUE)
    ## keys='lines', whichLabel=c(1,4)

    ## For each time get an approximate half-width of 0.95 CL for
    ## difference in median y between treatments.  Also save
    ## midpoint of two medians for plotting vertical half-interval.
    if(length(trlev)==2) {
      tt <- sort(times)
      j <- 0
      for(tim in tt) {
        j <- j + 1
        s <- z$Time==tim
        midpt  <- mean(z$y[s])
        clhalf <- 1.96*sqrt(sum(z$variance[s]))
        lines(c(tim,tim), c(midpt-clhalf/2, midpt+clhalf/2),
              col=gray(0.3))
      }
    }
  }
  endPlot()

  for(npage in 1:ceiling(nv/prod(mf))) {
    cap <- paste('Quartiles of ',longPanel,' variables over time',
                 if(npage > 1)' (continued)' else '', sep='')
    confcap <- if(length(trlev) == 1) NULL else
                 paste(' Vertical bars indicate half-widths of',
                   ' approximate 0.95 confidence intervals for',
                   ' differences in medians.',
                   ' When the distance between two medians',
                   ' exceeds the length of the bar, differences are',
                   ' significant at approximately the 0.05 level.',
                   ' \\protect\\treatkey', sep='')
    putFig(panel,paste(panel,npage,sep=''),
           cap,
           if(npage == 1)
           paste(cap,
                 '. Outer lines are $25^{th}$ (lower line)',
                 ' and $75^{th}$ (upper line) percentiles.',
                 ' Thicker middle lines depict medians.',
                 ' $y$-axis is scaled to the pooled $5^{th}$',
                 ' and $95^{th}$ quantiles.',
                 confcap,
                 sep='') else NULL,
           append=npage > 1)
  }

  ##################################################
  ### plot CDF function and include the corresponding *.pdf files
  ### into the relevant *.tex file
  ##################################################
  tf <- factor(paste(label(Time), Time), paste(label(Time), sort(unique(Time))))
  if(cdfPlot) {
    i <- 0
    nt <- length(times)
    h <- w <- 4.5*(nt < 7) + 5.5*(nt >= 7 & nt <=9) + 6.5*(nt > 9)
    for(v in data[vars]) {
      i <- i + 1
      fn <- paste(panel,'-ecdf-',vars[i],sep='')
      startPlot(fn, trellis=TRUE, h=h, w=w)
      d <- data.frame(v,tf,Treat,Time)[Time %in% times,]
      if(length(trlev)==1)
      print(ecdf( ~ v | tf, data=d, q=.5,
                 label.curve=FALSE,
                 as.table=TRUE)) else
      print(ecdf( ~ v | tf, groups=Treat, data=d,
                 lty=c(1,1), lwd=c(1,2), col=gray(c(0,.7)), q=.5,
                 label.curve=FALSE,
                 as.table=TRUE))
            ##   label.curve=list(keys='lines')))
            ##   Key(.79,1)
      endPlot()
      putFig(panel, fn,
             paste('Cumulative distribution of',
                   latexTranslate(label(v)),
                   if(length(trlev) > 1) 'by treatment over time'),
             paste('Empirical cumulative distribution function of',
                   latexTranslate(label(v)),
                   if(length(trlev) > 1) 'by treatment over time. \\protect\\treatkey'))
    }
  }
  ##################################################
  ### plot boxplots and include the corresponding *.pdf files
  ### into the relevant *.tex file
  ##################################################
  if (boxPlot){
    for (varName in vars){
      v <- data[varName]
      test=TRUE
      nmin = 1
      d <- data.frame(subject,v,Time)[Time %in% times,]
      d <- reshape(d, direction="wide", idvar=id, timevar="Time")
      d <- merge(d, data.frame(subject, Treat), by=id, all.x=TRUE)
      timeLabels <- c(id, paste(label(data[[varName]]), times[order(times)], sep=", visit "), treat)
      for (i in 1:length(d)) label(d[,i]) <- timeLabels[i]
      form <- as.formula(paste("Treat", paste(setdiff(names(d), Cs(subject,Treat)),collapse='+'), sep='~'))
      d <- summary(form, data=d, method='reverse',
                   test=test, continuous=continuous, nmin=nmin)
                   
      panelName <- paste(panel,"cont", varName, sep='-')
      startPlot(paste(panelName,'%d',sep=''), trellis=TRUE, h=6, w=6)
      plotNumber <- plot(d, which='con', conType=contDataPlotType)
      endPlot()
      if(plotNumber > 0) for(i in 1:plotNumber) {
        putFig(panel, paste(panelName, i, sep=''),
              paste('Frequencies of', longPanel, 'variables',
                    if(i>1) "(continued)" else ""))
      }
    }
  }
  if(!tables) return(invisible())
  ##################################################
  ### create tables and put them into
  ### the relevant *.tex file
  ##################################################
  form <- as.formula(paste(if(length(trlev)==1)'' else treat,
                           paste(vars,collapse='+'),sep='~'))
  if(clearPlots)
    cat('\\clearpage\n', file=paste('gentex/',panel,'.tex',sep=''),
        append=TRUE)
  for(x in times) {
    s <- summary(form, data=data[Time==x,], method='reverse',
                 test=length(trlev) > 1)
    w <- latex(s, file=paste('gentex/',panel,'.tex',sep=''), append=TRUE,
               middle.bold=TRUE, title='',
               caption=paste(longPanel,'Data at',label(Time),x),
               prtest='P', digits=digits, where='hbp!', ctable=TRUE)
  }
}
  makerep(Treat, panel)
  if(open) makerep(rep('', length(Treat)), paste('O',panel,sep=''))
  invisible()
}