#' Accrual Report
#'
#' quick summary
#'
#' detailed description
#'
#' @param Minor something minor NEEDDOC
#' @param Major NEEDDOC
#' @param MajorLabel NEEDDOC
#' @param MinorLabel NEEDDOC
#' @param EntryDate1 NEEDDOC
#' @param EntryDate2 NEEDDOC
#' @param EntryDateLabel NEEDDOC
#' @param EntryDate1cap NEEDDOC
#' @param EntryDate2cap NEEDDOC
#' @param dateRange NEEDDOC
#' @param dateformat NEEDDOC
#' @param targetN NEEDDOC
#' @param targetDate NEEDDOC
#' @param panel NEEDDOC
#' @param hdotchart NEEDDOC
#' @param append NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

accrualReport <-
  function(Minor, Major = rep('', length(Minor)), 
           MajorLabel='', MinorLabel = '', 
           EntryDate1 = NULL, EntryDate2 = NULL,
           EntryDateLabel = '', EntryDate1cap, EntryDate2cap,
           dateRange, dateformat = 'y-m-d', 
           targetN, targetDate=NULL,
           panel = 'randomized', hdotchart=6, append = TRUE)
{
  
  ## Plot: Subjects 'panel' (e.g., randomized) over time

  lcap <- cap2 <- ""
  if(length(EntryDate1))
    {
      dr <- as.chron(dates(dateRange, format = dateformat, out.format='m/d/y'))
      xlimdr <- c(floor.chron(dr[1], units = "months"),
                  ceiling.chron(dr[2], units = "months")+1)
      drseq <- seq.dates(xlimdr[1], xlimdr[2], by="month")
      axisat <- drseq[drseq %in% drseq[seq(1, length(drseq), by= 6)]]
      
      lb <- paste('accrual', panel, 'cumulative', sep = '-')
      shortcap <- paste('Subjects', panel, 'over time.')
      if(length(EntryDate1cap))
        cap1 <- paste('The solid black line depicts ', EntryDate1cap, '.', sep='')

      startPlot(lb, h = 3)
      ## Build plot based on EntryDate1
      Ecdf(EntryDate1, what = 'f', 
           xlab = EntryDateLabel, ylab = 'Cumumlative Number of Subjects',
           xlim = xlimdr,
           ylim = c(0, max(length(EntryDate1), targetN)), axes = FALSE)
      axis(side = 2)
      axis(side = 1, at = drseq, labels = rep("", length(drseq)), 
           tcl = -0.25) # make the minor tick marks shorter
      axis(side = 1, at = axisat, labels = as.character(axisat), 
           tcl = -0.5) # keep the minor tick marks the default length
      ## Add second line according to EntryDate2 (if appropriate)
      if(length(EntryDate2))
        {
          Ecdf(EntryDate2, what = 'f', add=TRUE, col='gray',
               xlim = xlimdr, ylim = c(0, max(length(EntryDate1), targetN)))
          if(length(EntryDate2cap))
            cap2 <- paste('The solid gray line depicts ',
                          EntryDate2cap, '.', sep='')
        }
      ## Add target accrual line
      targetX <- if (length(targetDate))
        {
          targetDateConv <- dates(targetDate, format = dateformat,
                                  out.format='m/d/y')
          c(dr[1], targetDateConv)
        }
      else dr

      lines(x = targetX, y = c(0,targetN), lty = 3, lwd=1)
      box()
      cap00 <- paste('The target enrollment duration was defined from ',
                     dr[1], ' to ', dr[2], '.', sep = '')
      cap0 <- 'The dotted straight line depicts target accrual.'
      endPlot()
      putFig(panel = 'accrual', name = lb, caption = shortcap,
             longcaption = paste(shortcap, cap00, cap0, cap1, cap2,
               sep = '  '), append = FALSE)
    }
###############################################################

  if(length(unique(Major)) > 1)
    {
      ## i.e., if Major != rep('', length(Minor))
      n <- table(Major)
      sitesPerMajor <- sapply(split(Minor, Major),
                              function(x) length(unique(x)))
      mm <- -sort(-n)

      ## Plot: Number of subjects randomized by 'Major' (e.g., country)

      lb <- paste('accrual',panel,'majorfreq',sep='-')
      startPlot(lb, h = hdotchart, trellis = FALSE)
      dotchart2(mm, auxdata = sitesPerMajor[names(mm)],
                auxtitle = paste('# ', casefold(MinorLabel), 's',
                  sep = ''), xlab = 'Number of Subjects')
      endPlot()
      lcap <- paste('Number of subjects', panel, 'by', casefold(MajorLabel))
      putFig(panel = 'accrual', name = lb, caption = lcap,
             longcaption = paste(lcap,
               '. Numbers to the right of the chart show the number of ', 
               casefold(MinorLabel), 's within each ',
               casefold(MajorLabel), '.', sep=''), append = append)

      Minor <- paste(Major, Minor, sep = '') ### redefine Minor as MajorMinor
  }
  
  n <- table(Minor)
  ## Minor if Major was not submitted, or MajorMinor if it was
  
  singlesitecap <- if(length(unique(Major)) > 1) 'site'
  else casefold(MinorLabel)
  pluralsitecap <- paste(singlesitecap, 's', sep = '')
  shortcap <- paste('Numbers of subjects',panel,'by', singlesitecap)
  figcap <- paste('\\label{fig:abbrev}', shortcap)
  ce <- NULL
  if(length(n) > 40)
    {
      ## if Minor or MajorMinor > 40 levels, force a combineEqual
      sites <- factor(names(n), names(n))
      sites <- reorder(sites, n)
#       sites <- reorder.factor(sites, n)
      ce <- combineEqual(n)
      ## combineEqual is a rreport fn. --> see rreport.s

      ## The output of combineEqual is a list with 3 components:
      ##    (1) 'x' --> labeled vector of code frequencies (label is code)
      ##    (2) 'codes' --> character vector of alpha code labels (i.e.
      ##         '(a)', '(b)', etc.)
      ##    (3) 'defs' --> character vector of code definitions
      n <- ce$x

      ## First, put together the codes and defs
      codes.n.defs <- data.frame(codes = ce$codes, defs = ce$def,
                                 row.names = 1:length(ce$codes))
      ## Second, put together the no. rand and the codenames 
      freqs.n.names <- data.frame(norand = ce$x, code.infig = names(ce$x),
                                  row.names = 1:length(ce$x))
      ## Now, merge the two together by the alpha code labels
      rand.data <- merge(freqs.n.names, codes.n.defs, by.x = 'code.infig', by.y = 'codes', all=TRUE)
      rand.data$defs <- ifelse(is.na(rand.data$defs), '',
                               as.character(rand.data$defs))
      ## Sort rand.data based on norand
      rand.data <- rand.data[order(rand.data$norand),]

      ## Table: Number of subjects 'panel' (e.g., randomized) by site (Minor or MajorMinor)

      listTable(fileName = paste('gentex/accrual', panel, 'sitefreq.tex',
                  sep=''), 
                caption = paste('\\label{table:abbrev}Legend of the ',
                  singlesitecap,
                  ' groupings used in the `', shortcap, 
                  '\' figure', #'(Figure \\ref{fig:abbrev} on page \\pageref{fig:abbrev})', 
                  sep = ''),
                zebraPattern = 'group',
                dataframe = subset(rand.data, code.infig %in% ce$codes),
                by='code.infig',
                colNames = c('Grouping used in figure', 'Definition of grouping'),
                vars = c('code.infig', 'defs'),
                fixedColVars = c('code.infig', 'defs'),
                fixedColWdths = c(50, 350),
                appendix = FALSE)
    }

  ## Plot: Number of subjects randomized by site (Minor or MajorMinor)

  lb <- paste('accrual',panel,'sitefreq',sep='-')
  medcap <- figcap
  lcap <- paste(medcap,
                  '.  Numbers to the right of the chart show the values that each dot represents ', 
                  '(i.e., the number of subjects ', panel,
                  ' within each ', singlesitecap, ').', sep = '')
  if(length(unique(Major)) > 1)
    {
      lcap <- paste(lcap, '  ', sQuote('Site'), 
                    ' is defined as the concatenation of ',
                    casefold(MajorLabel), ' and ', casefold(MinorLabel), ' (e.g., ',
                    sQuote(sample(setdiff(Minor, 'NANA'), size = 1)), ').',
                    sep = '')
    }
  if(length(ce$codes))
    {
      lcap <- paste(lcap,
                    '  Letters in parentheses indicate groups of ',
                    pluralsitecap, ' having the same number of subjects,',
                    ' which are defined in Table \\ref{table:abbrev} on page \\pageref{table:abbrev}.',
                    sep='')
    }
  startPlot(lb, h = hdotchart, trellis = FALSE)
  dotchart2(-sort(-n), xlab = 'Number of Subjects',
            auxdata = -sort(-n), auxtitle = '# subjects')
  endPlot()
  putFig(panel = 'accrual', name = lb,
         caption = figcap, longcaption = lcap, append=append)

  ## Add line in accrual.tex to pull in combineEqual table

  if(length(ce$codes)) cat('\\clearpage', '\n',
                           paste('\\input{gentex/accrual', panel,
                                 'sitefreq.tex}', sep=''), '\n',
                           file = 'gentex/accrual.tex',
                           append = TRUE)

  n <- table(Minor) ### Minor if Major was not submitted, or MajorMinor if it was
  nn <- table(n)

  ## Plot: Number of sites (Minor or MajorMinor) having a given number of subjects
  ##       'panel' (e.g., randomized)

  lb <- paste('accrual',panel,'sitespern',sep='-')
  startPlot(lb, h = 3)
  plot(as.numeric(names(nn)),nn, type = 'b',
       xlab = 'Number of Subjects', ylab = paste('Number of', pluralsitecap))
  endPlot()
  shortcap <- paste('Number of ', pluralsitecap,
                    ' having a given number of subjects ', panel, '.', sep='')
  lcap <- shortcap
  if(length(unique(Major)) > 1)
    {
      lcap <- paste(shortcap, '  ', sQuote('Site'), 
                    ' is defined as the concatenation of ',
                    MajorLabel, ' and ', MinorLabel, ' (e.g., ',
                    sQuote(sample(setdiff(Minor, 'NANA'), size = 1)),
                    ').', sep = '')
    }
  putFig(panel = 'accrual', name = lb, caption = shortcap, longcaption = lcap)
  invisible()
}
