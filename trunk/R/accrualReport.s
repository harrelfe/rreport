myaccrualReport <- function(Minor, Major = rep('', length(Minor)), 
                          MajorLabel='', MinorLabel = '', 
                          EntryDate1 = NULL, EntryDate2 = NULL, EntryDateLabel = '',
                          EntryDate1cap, EntryDate2cap,
                          dateRange, format = '%d%b%y', by = 'year', targetN,
                          panel = 'randomized', append = TRUE) {

  ###############################################################
  ##### Plot: Subjects 'panel' (e.g., randomized) over time #####
  ###############################################################
  if(length(EntryDate1)) {
    dr <- as.Date(dateRange)
    lb <- paste('accrual', panel, 'cumulative', sep = '-')
    shortcap <- paste('Subjects', panel, 'over time.')
    if(length(EntryDate1cap)) cap1 <- paste('Solid black line depicts ', EntryDate1cap, '.', sep='')

    startPlot(lb, h = 3)
    # Build plot based on EntryDate1
    ecdf(EntryDate1, what = 'f', 
      xlab = EntryDateLabel, ylab = 'Cumumlative Number of Subjects',
      ylim = c(0, max(length(EntryDate1), targetN)), xlim = dr, axes = FALSE)
    axis(side = 2)
    axis.Date(side = 1, at = seq.Date(dr[1], dr[2], by=by), format = format)
    # Add second line according to EntryDate2 (if appropriate)
    if(length(EntryDate1)) {
      ecdf(EntryDate2, what = 'f', add=TRUE, col='gray',
        xlim = dr, ylim = c(0, max(length(EntryDate1), targetN)))
      if(length(EntryDate2cap)) cap2 <- paste('Solid gray line depicts ', EntryDate2cap, '.', sep='')
    }
    # Add target accrual line
    lines(x = as.Date(dateRange), y = c(0,targetN), lty = 3, lwd=1)
    box()
    cap0 <- 'Dotted straight line depicts target accrual.'
    endPlot()
    putFig(panel = 'accrual', name = lb, caption = shortcap,
      longcaption = paste(shortcap, cap0, cap1, cap2, sep = '  '), append = FALSE)
  }
  ###############################################################

  if(length(unique(Major)) > 1) { ### i.e., if Major != rep('', length(Minor))
    n <- table(Major)
#    n <- n[n > 0]
    sitesPerMajor <- sapply(split(Minor, Major), function(x) length(unique(x)))
    mm <- -sort(-n)
    ##########################################################################
    ##### Plot: Number of subjects randomized by 'Major' (e.g., country) #####
    ##########################################################################
    lb <- paste('accrual',panel,'majorfreq',sep='-')
    startPlot(lb, h = 6, trellis = FALSE)
    dotchart2(mm, auxdata = sitesPerMajor[names(mm)],
      auxtitle = paste('# ', casefold(MinorLabel), 's', sep = ''), xlab = 'Number of Subjects')
    endPlot()
    lcap <- paste('Number of subjects', panel, 'by', casefold(MajorLabel))
    putFig(panel = 'accrual', name = lb, caption = lcap,
      longcaption = paste(lcap, '. Numbers to right of chart show the number of ', 
      casefold(MinorLabel), 's within each ', casefold(MajorLabel), '.', sep=''), append = append)
    ##########################################################################

    Minor <- paste(Major, Minor, sep = '') ### redefine Minor as MajorMinor
  }

  n <- table(Minor) ### Minor if Major was not submitted, or MajorMinor if it was
  singlesitecap <- if(length(unique(Major)) > 1) 'site'
    else casefold(MinorLabel)
  pluralsitecap <- paste(singlesitecap, 's', sep = '')
  shortcap <- paste('Numbers of subjects',panel,'by', singlesitecap)
  figcap <- paste('\\label{fig:abbrev}', shortcap)
  if(length(n) > 40) { ### if Minor or MajorMinor > 40 levels, force a combineEqual
    sites <- factor(names(n),names(n))
    sites <- reorder.factor(sites, n)
    ce <- combineEqual(n)     # combineEqual is a rreport fn. --> see rreport.s
                              # The output of combineEqual is a list with 3 components:
                              #    (1) 'x' --> labeled vector of code frequencies (label is code)
                              #    (2) 'codes' --> character vector of alpha code labels (i.e.
                              #         '(a)', '(b)', etc.)
                              #    (3) 'defs' --> character vector of code definitions
    n <- ce$x
#    ns <- length(n)
    # First, put together the codes and defs
    codes.n.defs <- data.frame(codes = ce$codes, defs = ce$def, row.names = 1:length(ce$codes))
    # Second, put together the no. rand and the codenames 
    freqs.n.names <- data.frame(norand = ce$x, code.infig = names(ce$x), row.names = 1:length(ce$x))
    # Now, merge the two together by the alpha code labels
    rand.data <- merge(freqs.n.names, codes.n.defs, by.x = 'code.infig', by.y = 'codes', all=TRUE)
    rand.data$defs <- ifelse(is.na(rand.data$defs), '', as.character(rand.data$defs))
    # Sort rand.data based on norand
    rand.data <- rand.data[order(rand.data$norand),]

    ##############################################################################################
    ##### Table: Number of subjects 'panel' (e.g., randomized) by site (Minor or MajorMinor) #####
    ##############################################################################################
    listTable(fileName = paste('gentex/accrual', panel, 'sitefreq.tex', sep=''), 
      caption = paste('\\label{table:abbrev}', 'Legend of the', singlesitecap,  
        'groupings used in the', dQuote(shortcap), 'figure (Figure \\ref{fig:abbrev} on page \\pageref{fig:abbrev}).'),
      zebraPattern = 'group', dataframe = subset(rand.data, code.infig %in% ce$codes), by='code.infig',
      colNames = c('Grouping used in figure', 'Definition of grouping'),
      vars = c('code.infig', 'defs'),
      fixedColVars = c('code.infig', 'defs'),
      fixedColWdths = c(50, 350),
      appendix = FALSE)
    ##############################################################################################
  }
  ##############################################################################
  ##### Plot: Number of subjects randomized by site (Minor or MajorMinor) ######
  ##############################################################################
  lb <- paste('accrual',panel,'sitefreq',sep='-')
  medcap <- paste(figcap, '.  Numbers to right of chart show the number of subjects ', panel, 
    ' within each ', singlesitecap, '.', sep = '')
  if(length(unique(Major)) > 1) {
    medcap <- paste(medcap, '  ', sQuote('Site'), 
      ' is defined as the concatenation of ', casefold(MajorLabel), ' and ', casefold(MinorLabel), ' (e.g., ',
      sQuote(sample(setdiff(Minor, 'NANA'), size = 1)), ').', sep = '')
  }
  if(length(ce$codes)) {
    lcap <- paste(medcap,
      '  Letters in parentheses indicate groups of ', pluralsitecap, ' having the same number of subjects,',
      ' which are defined in Table \\ref{table:abbrev} on page \\pageref{table:abbrev}.',
#        sQuote(paste('Legend of the', singlesitecap, 'groupings used in the', dQuote(shortcap), 'figure')), '.',
      sep='')
  } else medcap
  startPlot(lb, h = 6, trellis = FALSE)
  dotchart2(-sort(-n), xlab = 'Number of Subjects', auxdata = -sort(-n), auxtitle = '# subjects')
  endPlot()
  putFig(panel = 'accrual', name = lb, caption = lcap, append=append)
  ##########################################################################

  # Add line in accrual.tex to pull in combineEqual table
  if(length(ce$codes)) cat('\\clearpage', '\n',
    paste('\\input{gentex/accrual', panel, 'sitefreq.tex}', sep=''), '\n',
    file = 'gentex/accrual.tex', append = TRUE)

  n <- table(Minor) ### Minor if Major was not submitted, or MajorMinor if it was
  nn <- table(n)
  #########################################################################################
  ##### Plot: Number of sites (Minor or MajorMinor) having a given number of subjects #####
  #####       'panel' (e.g., randomized) ##################################################
  #########################################################################################
  lb <- paste('accrual',panel,'sitespern',sep='-')
  startPlot(lb, h = 3)
  plot(as.numeric(names(nn)),nn, type = 'b',
    xlab = 'Number of Subjects', ylab = paste('Number of', pluralsitecap))
  endPlot()
  lcap <- paste('Number of ', pluralsitecap, ' having a given number of subjects ', panel, '.', sep='')
  if(length(unique(Major)) > 1) {
    lcap <- paste(lcap, '  ', sQuote('Site'), 
      ' is defined as the concatenation of ', MajorLabel, ' and ', MinorLabel, ' (e.g., ',
      sQuote(sample(setdiff(Minor, 'NANA'), size = 1)), ').', sep = '')
  }
  putFig(panel = 'accrual', name = lb, caption = lcap)
  #########################################################################################

 invisible()
}
