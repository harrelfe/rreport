## $Id$
aeReport <- function(data, vars, treat, time,
                     times=sort(unique(time)),
                     id=NULL, plotprop=FALSE, plotkm=TRUE, etimedata=NULL,
                     tables=TRUE, times.tables=times,
                     forceBinary=FALSE,
                     ylim=c(0,1), h=5, w=5, digits=3) {

  vars  <- unlist(vars)
  data  <- data[data[[time]] %in% times, c(vars,treat,time,id)]
  Treat <- data[[treat]]
  Time  <- data[[time]]
  nt <- length(levels(Treat))

  ## Useful if variables represent counts and we want present/absent
  if(forceBinary)
    for(i in 1:length(vars)) {
      x <- data[[i]]
      if(is.numeric(x)) {
        lab <- label(x)
        x <- 1*(x > 0)
        label(x) <- lab
        data[[i]] <- x
      }
    }

  g <- function(y) {
    y <- y[!is.na(y)]
    c(Mean=mean(y),var=var(y),n=length(y))
  }

  if(plotprop) {
    startPlot('ae%d', h=h, w=w)
    oldmf <- mfrowSet(length(vars))
    mf <- par('mfrow')
    allbin <- TRUE
    for(y in data[vars])
      {
        if(!is.numeric(y)) y <- 1*(y %in% c('Y','Yes','yes','YES'))
        allbin <- allbin & length(unique(y[!is.na(y)])) < 3
        s <- summarize(y, llist(Treat,Time), g)
        curves <- vector('list',nt)
        names(curves) <- levels(Treat)
        for(w in names(curves))
          curves[[w]] <- list(x=s$Time[s$Treat==w], y=s$y[s$Treat==w])
        yl <- c(ylim[1], max(ylim[2], s$y))
        labcurve(curves, lwd=c(1,2), lty=c(1,1), col=gray(c(0,.7)),
                 xlab=label(Time), ylab=label(y),
                 ylim=yl, type='b', method='none', pl=TRUE)
        ## keys='lines'
        
        ## Find out a typical sample size and the pooled variance
        ## to get a typical confidence interval for the
        ## difference in two proportions or two means
      
        n <- tapply(s$n, s$Treat, median)
        pv <- sum((s$n - 1) * s$var)/sum(s$n - 1)
        clhalfwidth <- 1.96*sqrt(pv*(1/n[1] + 1/n[2]))
        u <- par('usr'); par(xpd=NA)
        lines(rep(u[2]+.03*(u[2]-u[1]),2), c(u[3], u[3]+clhalfwidth),
              col=gray(0.7), lwd=0.5)
      }
    endPlot()
    for(i in 1:ceiling(length(vars) / prod(mf))) {
      cap <- paste('Proportions of adverse events by treatment over time',
                   if(i > 1) ' (continued)' else '',sep='')
      st <- if(allbin)'proportions' else 'means'
      putFig('ae', paste('ae',i,sep=''), cap,
             if(i==1) paste(cap,
                  '. Vertical bars to the right of each plot indicate',
                  ' half-widths of typical approximate 0.95 confidence',
                  ' bars for a difference in ',paste(st,'.',sep=''),
                  ' When the distance between two ',st,
                  ' exceeds the length of this bar, differences are',
                  ' significant at approximately the 0.05 level.',
                  ' These confidence bars were computed at the median',
                  ' per-treatment sample size over time',
                  ' and used the pooled variance over treatments and time.',
                  ' \\protect\\treatkey',
                  sep=''),append=i>1)
    }
  }

  if(plotkm) {
    require('Design')
    Id <- data[[id]]
    startPlot('ae-km%d', h=h, w=w)
    oldmf <- mfrowSet(min(4,length(vars)))
    mf <- par('mfrow')
    xpd <- par(xpd=NA)
    on.exit(par(xpd=xpd,mfrow=oldmf))
    
    for(v in vars) {
      if(length(etimedata)) {
        tr <- etimedata[[1]]
        etime <- etimedata[[2]][[v]]
        event <- etimedata[[3]][[v]]
        ylab <- v
      } else {
        y <- data[[v]]
        ylab <- label(y, plot=TRUE)
        if(!is.numeric(y)) y <- 1*(y %in% c('Y','Yes','yes','YES'))
        ## For one subject, compute time to first AE of current type,
        ## time at which AE last assessed, and event indicator
        ## Note: using matrices is MUCH faster than using data frames
        ## with by()
        g <- function(z) {
          y <- z[,1]; time <- z[,2]
          s <- is.na(y)
          if(s[1]) return(c(treat=z[1,3], etime=NA, event=NA))
          ## If any NA in y, restrict attention to y's before first NA
          if(any(s)) {
            k <- min(which(s))-1
            y <- y[1:k]; time <- time[1:k]
          }
          etime <- if(any(y>0)) min(time[y>0]) else NA
          ctime <- max(time)
          event <- !is.na(etime)
          if(!event) etime <- ctime
          c(treat=z[1,3], etime=etime, event=event)
        }
        z <- cbind(y,Time,Treat)
        ## Execute g separately for all subjects
        r <- mApply(z, Id, g)
        tr <- factor(r[,1], 1:length(levels(Treat)), levels(Treat))
        etime <- r[,2]
        event <- r[,3]
      }
      km <- survfit(Surv(etime,event) ~ tr)
      omar <- par('mar')
      mar <- omar; mar[1] <- mar[1]+3
      par(mar=mar)
      lows <- min(km$surv, na.rm=TRUE)
      survplot(km, fun=function(y)1-y,
               xlab=label(Time), ylab=ylab,
               conf='none', label.curves=FALSE,
               ylim=c(0,1-lows),
               time.inc=floor(max(times)/5),
               n.risk=TRUE, y.n.risk=-(1-lows)*.475,
               sep.n.risk=.045,
               lwd=c(1,2), lty=c(1,1), col=gray(c(0,.7)))
      plotKmHalfCL(km, weeks, function(y)1-y, offset=max(times)/50)
      par(mar=omar)
    }
    endPlot()
    for(i in 1:ceiling(length(vars) / prod(mf))) {
      cap <- paste('Kaplan-Meier estimates of cumulative probabilities',
                   ' of adverse events by treatment over time',
                   if(i > 1) ' (continued)' else '',sep='')
      putFig('ae', paste('ae-km',i,sep=''),
             cap,
             if(i==1) paste(cap,
                  '. Dotted vertical bars indicate half-widths of',
                  ' approximate 0.95 confidence intervals for',
                  ' differences in probabilities.',
                  ' When the distance between two proportions',
                  ' exceeds the length of the bar, differences are',
                  ' significant at approximately the 0.05 level.',
                  ' \\protect\\treatkey',
                  sep='') else cap,
             append=i>1)
    }
  }
  if(!tables) return()
  if(!all(times.tables %in% times))
    stop('times.tables must be a subset of times')
  cat('In the following tables $N$ is the number of subjects and',
      'numbers after percents are frequencies.',
      '$P$-values are from Pearson $\\chi^2$ tests.\n',
      file='ae.tex', append=TRUE)
  
  form <-
    as.formula(paste(treat,paste(vars,collapse='+'),sep='~'))
  for(x in times.tables) {
    s <- summary(form, data=data[Time==x,], method='reverse',
                 test=TRUE)
    w <- latex(s, file='gentex/ae.tex', append=TRUE,
               middle.bold=TRUE, title='',
               caption=paste('Adverse Events at',label(Time),x),
               prtest='P', digits=digits, where='hbp!',
               insert.bottom=FALSE, ctable=TRUE)
  }

  ## Make a new data frame unioning AEs over all times
  d <- data[c(vars,treat)]
  m <- asNumericMatrix(d)
  at <- subsAttr(d)
  m <- mApply(m, Id,
              function(y) {
                nc <- ncol(y)
                v <- apply(y[,-nc,drop=FALSE], 2, any, na.rm=TRUE)
                c(v, tr=y[1,nc])
              })
  dm <- dimnames(m)
  dm[[2]][length(dm[[2]])] <- treat
  dimnames(m) <- dm
  v <- matrix2dataFrame(m, at)
  s <- summary(form, data=v, method='reverse', test=TRUE)
  w <- latex(s, file='gentex/ae.tex', append=TRUE,
             middle.bold=TRUE, title='',
             caption='Adverse Events at Any Time',
             prtest='P', digits=digits, where='hbp!',
             insert.bottom=FALSE, ctable=TRUE)
}

freqReport <- function(type, panel, treat, longPanel=panel,
                       typeLabel=label(type),
                       plotprop =FALSE, Ntreat=NULL,
                       omitZeros=TRUE,
                       ylim=c(0,1), h=5, w=5, digits=3, size=NULL,
                       longtable=FALSE, lines.page=40, append=FALSE) {

  longPanel <- paste(longPanel,'by',typeLabel)
  nt <- length(levels(treat))
  if(any(is.na(type)))
     type <- ifelse(is.na(type),'Unspecified',as.character(type))

  tab <- table(type)
  if(omitZeros) tab <- tab[tab > 0]
  tab <- as.matrix(tab)
  pan <- paste('O', panel, sep='')
  w <- latex(tab, file=paste('gentex/',pan,'.tex',sep=''),
             title=pan, append=append, rowlabel=typeLabel,
             caption=paste('Frequencies of',longPanel),
             ctable=TRUE, size=size,
             longtable=longtable, lines.page=lines.page)
  
  tab <- table(type, treat)
  tab <- cbind(tab, Total=rowSums(tab))
  if(omitZeros) tab <- tab[tab[,'Total'] > 0,]
  pan <- panel
  w   <- latex(tab, file=paste('gentex/',pan,'.tex',sep=''),
               title=pan, append=append, rowlabel=typeLabel,
               caption=paste('Frequencies of',longPanel,'and Treatment'),
               extracolheads=if(length(Ntreat))
                paste('N',c(Ntreat,sum(Ntreat)),sep='='),
               ctable=TRUE, size=size,
               longtable=longtable, lines.page=lines.page)
  
  if(plotprop) {
    startPlot(pan, h=h, w=w)
    ## add plotting code here!
    endPlot()
    cap <- paste('Frequencies and proportions of',longPanel)
    putFig(pan, name, cap,
           paste(cap,
                 '. Denominators of proportions are assumed to be constants.',
                 sep=''))
  }

}
