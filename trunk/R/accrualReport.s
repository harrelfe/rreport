accrualReport <- function(Site, Entry=NULL, panel='randomized',
                          dateRange, targetN,
                          Major=rep('', length(Site)),
                          MajorLabel='',longMajor=Major,
                          combine=FALSE, append=FALSE) {
  
  nMajor <- length(unique(Major))
  if(nMajor > 1) {
    n <- table(longMajor)
    n <- n[n > 0]
    maj <- factor(names(n),names(n))
    maj <- reorder.factor(maj, n)
    ns <- length(n)
    s <- split(Site, longMajor)
    sitesPerMajor <- sapply(s, function(x) length(unique(x)))

    lb <- paste('accrual',panel,'majorfreq',sep='-')
    startPlot(lb, h=4, trellis=FALSE)
#    print(Dotplot(maj ~ n, xlab='Number of Subjects',
#                  ylab=MajorLabel))
    mm <- -sort(-n)
    dotchart2(mm, auxdata=sitesPerMajor[names(mm)],
              auxtitle='# Sites',
              xlab='Number of Subjects', ylab=MajorLabel)
    endPlot()
    lcap <- paste('Number of subjects',panel,'by',
                 casefold(MajorLabel))
    putFig('accrual',lb,
           lcap,
           paste(lcap, '. Numbers to right of chart show the number of sites within each',
                 casefold(MajorLabel),'.', sep=''),
           append=append)

    Site <- paste(Major, Site, sep='')
  }

  n     <- table(Site)
  nn    <- table(n)
  lb <- paste('accrual',panel,'sitespern',sep='-')
  startPlot(lb, h=3)
  plot(as.numeric(names(nn)),nn,
       xlab='Number of Subjects', ylab='Number of Sites', type='b')
  endPlot()
  lcap <- paste('Number of sites having a given number of subjects',panel)
  putFig('accrual',lb,lcap)
  
  n     <- n[n > 0]
  sites <- factor(names(n),names(n))
  sites <- reorder.factor(sites, n)
  ns    <- length(n)
  if(combine) {
    ce <- combineEqual(n)
    n <- ce$x
    sites <- factor(names(n), names(n))
    sites <- reorder.factor(sites, n)
    ns <- length(n)
  }

  lb <- paste('accrual',panel,'sitefreq',sep='-')
  startPlot(lb, h=if(ns > 25)5 else if(ns > 15) 4 else 3,
            trellis=TRUE)
  print(Dotplot(sites ~ n, xlab='Number of Subjects', ylab='Site'))
  endPlot()

  lcap <- paste('Number of subjects',panel,'by center')
  if(length(ce$codes))
    lcap <- paste(lcap,
                      '.\nLetters in parentheses indicate groups of sites ',
                      'having the same number of subjects, ',
                      'defined as follows: {\\smaller[4]',
                      paste(paste(ce$codes,':',
                            '\\texttt{',ce$defs,'}',sep=''),collapse='; '),
                      '.}\n', sep='')

  putFig('accrual', lb, paste('Numbers of subjects',panel,'by site'), lcap)


  if(length(Entry)) {
    lb <- paste('accrual',panel,'cumulative',sep='-')
    startPlot(lb, h=3)
    ecdf(Entry, what='f', ylab='Cumumlative Number of Subjects',
         xlab=label(Entry), ylim=c(0,max(length(Entry),targetN)),
         xlim=as.POSIXct(dateRange))
    lines(as.POSIXct(dateRange), c(0,targetN), col='gray', lwd=3)
    endPlot()
    putFig('accrual', lb, paste('Subjects',panel,'over time'),
           paste('Subjects',panel,
                 'over time.  Straight line depicts target accrual.'))
  }
 invisible()
}
