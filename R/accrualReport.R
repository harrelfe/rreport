#' Accrual Report
#'
#' Generate reports with patient accrual numbers.
#'
#' When \code{EntryDate1} and \code{dateRange} are present, generate Ecdf for subject accrual over the enrollment period.
#' When \code{Major} is present, generate dotchart for number of subjects by Major.
#' Generate dotchart for number of subjects by Minor.
#' Generate plot with the count of site having a given number of subjects.
#'
#' @param Minor character vector. Minor categorical variable for site.
#' @param Major character vector. Major categorical variable for site.
#' @param MajorLabel character.  Label for major site variable.
#' @param MinorLabel character.  Label for minor site variable.
#' @param EntryDate1 character vector. Entry date for each subject.
#' @param EntryDate2 character vector.  Entry date for each subject.
#' @param EntryDateLabel character. X-axis label for entry date plot.
#' @param EntryDate1cap character. Caption for first entry date.
#' @param EntryDate2cap character. Caption for second entry date.
#' @param dateRange a vector of dates. Should contain two values specifying a date range.
#' @param dateformat character. Specify date format, see \code{format} argument for \code{\link[chron]{chron}}.
#' @param targetN numeric. Target number of subjects to enroll.
#' @param targetDate character. Target date to end enrollment period.
#' @param panel character. Name for subjects panel, defaults to \sQuote{randomized}.
#' @param hdotchart numeric. Height of plot, passed to \code{\link{startPlot}}.
#' @param append logical. If \sQuote{TRUE} output will be appended instead of overwritten.
#' @export
#' @examples
#' \dontrun{
#'   accrualReport(sample(15, 200, replace=TRUE), MinorLabel='site',
#'     EntryDate1=as.Date("2005-01-01")+round(rgamma(200, 2, .01)),
#'     EntryDate1cap='randomized subjects', dateRange=c('2005-01-01', '2008-12-31'),
#'     targetN=300, targetDate='2008-12-31')
#' }

accrualReport <-
  function(Minor, Major = character(length(Minor)),
           MajorLabel='', MinorLabel = '',
           EntryDate1 = NULL, EntryDate2 = NULL, EntryDateLabel = '',
           EntryDate1cap, EntryDate2cap,
           dateRange, dateformat = 'y-m-d', targetN, targetDate=NULL,
           panel = 'randomized', hdotchart=6, append = TRUE) {
  ## Plot: Subjects 'panel' (e.g., randomized) over time
  lcap <- cap2 <- ""
  if(length(EntryDate1) && !missing(dateRange)) {
    if(missing(targetN)) {
      stop("targetN is required for EntryDate1 plot")
    }
    dr <- as.chron(dates(dateRange, format = dateformat, out.format='m/d/y'))
    xlimdr <- c(floor.chron(dr[1], units = "months"), ceiling.chron(dr[2], units = "months")+1)
    drseq <- seq.dates(xlimdr[1], xlimdr[2], by="month")
    axisat <- drseq[drseq %in% drseq[seq(1, length(drseq), by=6)]]

    lb <- sprintf("accrual-%s-cumulative", panel)
    shortcap <- sprintf("Subjects %s over time", panel)
    if(!missing(EntryDate1cap) && length(EntryDate1cap) > 0) {
      cap1 <- sprintf("The solid black line depicts %s.", EntryDate1cap)
    } else {
      cap1 <- ''
    }

    startPlot(lb, h = 3)
    ## Build plot based on EntryDate1
    Ecdf(EntryDate1, what = 'f', 
      xlab = EntryDateLabel, ylab = 'Cumumlative Number of Subjects',
      xlim = xlimdr, ylim = c(0, max(length(EntryDate1), targetN)), axes = FALSE
    )
    axis(side = 2)
    axis(side = 1, at = drseq, labels = character(length(drseq)), tcl = -0.25) # make the minor tick marks shorter
    axis(side = 1, at = axisat, labels = as.character(axisat), tcl = -0.5) # keep the minor tick marks the default length
    ## Add second line according to EntryDate2 (if appropriate)
    if(length(EntryDate2)) {
      Ecdf(EntryDate2, what = 'f', add=TRUE, col='gray', xlim = xlimdr, ylim = c(0, max(length(EntryDate1), targetN)))
      if(!missing(EntryDate2cap) && length(EntryDate2cap) > 0) {
        cap2 <- sprintf("The solid gray line depicts %s.", EntryDate2cap)
      } else {
        cap2 <- ''
      }
    }

    ## Add target accrual line
    if(length(targetDate)) {
      targetDateConv <- dates(targetDate, format = dateformat, out.format='m/d/y')
      targetX <- c(dr[1], targetDateConv)
    } else {
      targetX <- dr
    }
    lines(x = targetX, y = c(0,targetN), lty = 3, lwd=1)
    box()
    cap00 <- sprintf("The target enrollment duration was defined from %s to %s.", dr[1], dr[2])
    cap0 <- 'The dotted straight line depicts target accrual.'
    endPlot()
    longcap <- paste(shortcap, cap00, cap0, cap1, cap2, sep = '  ')
    putFig(panel = 'accrual', name = lb, caption = shortcap, longcaption = longcap, append = FALSE)
  }

  ## Plot: Number of subjects randomized by 'Major' (e.g., country)
  if(length(unique(Major)) > 1) {
      n <- table(Major)
      sitesPerMajor <- sapply(split(Minor, Major), function(x) length(unique(x)))
      mm <- sort(n, decreasing=TRUE)

      lb <- sprintf("accrual-%s-majorfreq", panel)
      startPlot(lb, h = hdotchart, trellis = FALSE)
      dotchart2(mm, auxdata = sitesPerMajor[names(mm)], auxtitle = sprintf("# %ss", casefold(MinorLabel)), xlab = 'Number of Subjects')
      endPlot()
      lcap <- sprintf("Number of subjects %s by %s", panel, casefold(MajorLabel))
      longcap <- sprintf("%s. Numbers to the right of the chart show the number of %ss within each %s.", lcap, casefold(MinorLabel), casefold(MajorLabel))
      putFig(panel = 'accrual', name = lb, caption = lcap, longcaption = longcap, append = append)
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
  sitefreqname <- file.path(TexDirName(), sprintf("accrual%ssitefreq.tex", panel))
  if(length(n) > 40) {
      ## if Minor or MajorMinor > 40 levels, force a combineEqual
      sites <- factor(names(n), names(n))
      sites <- reorder(sites, n)
      ce <- combineEqual(n)
      n <- ce$x

      ## First, put together the codes and defs
      codes.n.defs <- data.frame(codes = ce$codes, defs = ce$def, row.names = 1:length(ce$codes))
      ## Second, put together the no. rand and the codenames 
      freqs.n.names <- data.frame(norand = ce$x, code.infig = names(ce$x), row.names = 1:length(ce$x))
      ## Now, merge the two together by the alpha code labels
      rand.data <- merge(freqs.n.names, codes.n.defs, by.x = 'code.infig', by.y = 'codes', all=TRUE)
      rand.data$defs <- ifelse(is.na(rand.data$defs), '',
                               as.character(rand.data$defs))
      ## Sort rand.data based on norand
      rand.data <- rand.data[order(rand.data$norand),]

      ## Table: Number of subjects 'panel' (e.g., randomized) by site (Minor or MajorMinor)

      sitefreqcap <- sprintf("\\label{table:abbrev}Legend of the %s groupings used in the `%s\' figure", singlesitecap, shortcap)
      #'(Figure \\ref{fig:abbrev} on page \\pageref{fig:abbrev})',
      listTable(fileName = sitefreqname, 
                caption = sitefreqcap,
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
                           sprintf("\\input{%s}", sitefreqname), '\n',
                           file = file.path(TexDirName(), 'accrual.tex'),
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
