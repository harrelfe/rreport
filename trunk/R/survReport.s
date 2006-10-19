survReport <- function(etime, event, treat, group=NULL, treat.group=NULL, data,
                       ylabel='Survival Probability',
                       conf=c('bars','bands','none'),
                       n=NULL, labels=NULL, previousActual=NULL, h=4, 
                       append=FALSE, fileName="trt", descrip="treatment", ...) {
  require('survival')
  require('Design')

  plotName <- paste('surv-km', fileName, sep=".") 

  if(length(group) & length(treat.group)) {
    # Base K-M plot on treat.group
    startPlot(plotName, h=h)
    lty = rep(c(1,3), 2); col = rep(gray(c(0,.7)), each = 2); lwd = 1
    S <- Surv(data[[etime]], data[[event]])
    treat.group <- as.factor(data[[treat.group]])
    survplot(survfit(S ~ treat.group), n.risk=TRUE, conf=conf, lwd=lwd,
       lty=lty, col=col, ylabel=ylabel, ...)
    endPlot()
    # Calculate the corresponding z for each level of group 
    for(i in levels(as.factor(data[[group]]))) {
      assign(paste("S", i, sep="."), Surv(data[data[[group]]==i, etime], data[data[[group]]==i, event]))
      assign(paste("treat", i, sep="."), data[data[[group]]==i, treat])
      j <- is.na(get(paste("S", i, sep="."))) | is.na(get(paste("treat", i, sep="."))) 
      if(any(j)) {
         assign(paste("S", i, sep="."), get(paste("S", i, sep="."))[!j,])
         assign(paste("treat", i, sep="."), get(paste("treat", i, sep="."))[!j])
      }
      assign(paste("z", i, sep="."), round(sqrt(logrank(get(paste("S", i, sep=".")), as.integer(get(paste("treat", i, sep="."))))), 2))
    }
    putFig(panel = 'surv', name = plotName,
      caption = paste('Kaplan-Meier estimates by ',  descrip, '.', sep=''), 
      longcaption = paste('Kaplan-Meier cumulative event-free probability estimates by ', descrip, '.', 
        '  Based on these data, the unadjusted Cox-logrank $z$ values for ', levels(as.factor(data[[group]]))[1], ' and ', levels(as.factor(data[[group]]))[2],
        ' across treatment were calculated to be ', get(paste("z", levels(as.factor(data[[group]]))[1], sep=".")), ' and ', 
        get(paste("z", levels(as.factor(data[[group]]))[2], sep=".")), 
        ', respectively.  ',
        # Inset treatment key        
        levels(treat.group)[1], ' = solid black; ', levels(treat.group)[2], ' = dotted black; ', levels(treat.group)[3], ' = solid gray; ', 
        levels(treat.group)[4],' = dotted gray.', sep=""), append = append)
  }
  else {
    # Base everything on treat
    startPlot(plotName, h=h)
    lwd = c(1,2); lty = c(1,1); col = gray(c(0,.7))
    S <- Surv(data[[etime]], data[[event]])
    treat <- as.factor(data[[treat]])
    if(attributes(survfit(S ~ treat)$strata)$names[1]=="B") {
       col=gray(c(0.7, 0))
    }
    survplot(survfit(S ~ treat), n.risk=TRUE, conf=conf, lwd=lwd,
       lty=lty, col=col, ylabel=ylabel, ...)
    endPlot()
    # Calculate the corresponding z 
    i <- is.na(S) | is.na(treat) 
    if(any(i)) {
      S <- S[!i,]
      treat <- treat[!i]
    }
    z <- round(sqrt(logrank(S, as.integer(treat))), 2)
    putFig(panel = 'surv', name = plotName,
      caption = paste('Kaplan-Meier estimates by ',  descrip, '.', sep=''), 
      longcaption = paste('Kaplan-Meier cumulative event-free probability estimates by ', descrip, ".", 
           '  Based on these data, an unadjusted Cox-logrank $z$ value was calculated to be ', z, ".  ", 
           # Inset treatment key        
           levels(treat)[1], ':\\rule[.05in]{.25in}{.5pt}; ', levels(treat)[2], ':\\textcolor[gray]{0.7}{\\rule[.05in]{.25in}{1.25pt}}.', sep=""), 
           append = append)
  }

# LEAVE AS IS IN survReport.s
  if(length(n)) {
    p <- ldBands(n=n, ...)
    i <- is.na(S) | is.na(treat)
    if(any(is.na(i))) {
      S <- S[!i,]
      treat <- treat[!i]
    }
    z <- logrank(S, as.integer(treat))
    startPlot('surv-monitor', h=h)
    plot(p, labels=labels, actual=c(z, previousActual))
    endPlot()
    putFig('surv','surv-monitor',
           'Group-sequential monitoring boundaries',
           paste('Lan-DeMets group-sequential monitoring boundaries using the Obrien-Fleming alpha-spending function with',n,'looks equally spaced in time.  Points indicate observed Cox-logrank Z statistics.'), append=append)
  }
}
