\name{aeReport}
\alias{aeReport}
\title{Adverse Events Report}
\usage{
  aeReport(data = NULL, vars, treat, time,
    times = sort(unique(time)), id = NULL,
    plotprop = FALSE, plotkm = TRUE, etimedata = NULL,
    tables = TRUE, times.tables = times,
    forceBinary = FALSE, ylim = c(0, 1), h = 5, w = 5,
    digits = 3)
}
\arguments{
  \item{data}{data.frame. Data used for report.}

  \item{vars}{character vector. Variables to include in
  analysis.}

  \item{treat}{factor vector. Vector of treatment group for
  each record.}

  \item{time}{character. Name of time variable within
  dataset.}

  \item{times}{numeric vector. Times used to subset data.}

  \item{id}{character. Name of id variable within dataset.}

  \item{plotprop}{logical. If \sQuote{TRUE} plot
  proportions of adverse events.}

  \item{plotkm}{logical. If \sQuote{TRUE} plot Kaplan-Meier
  estimates of cumulative probabilities of adverse events.}

  \item{etimedata}{list. List of time data, containing
  treatment, event time and event indicator for each
  variable.}

  \item{tables}{logical. If \sQuote{TRUE} generate summary
  tables of adverse events by times found in
  \code{times.tables}.}

  \item{times.tables}{numeric vector. Times used to
  generate summary tables.}

  \item{forceBinary}{logical. Set to \sQuote{TRUE} if
  variables are binary.}

  \item{ylim}{numeric vector. Set y-axis limits.}

  \item{h}{numeric. Height of plot. Default is 5in. See
  \code{\link[Hmisc]{setps}}.}

  \item{w}{numeric. Width of plot. Default is 5in. See
  \code{\link[Hmisc]{setps}}.}

  \item{digits}{numeric. Number of significant digits to
  print. Defaults to 3.}
}
\description{
  Generate reports to summarize adverse events.
}
\examples{
\dontrun{
  load(url('http://biostat.mc.vanderbilt.edu/wiki/pub/Main/Rreport/ssafety.rda'))
  ae <- c('headache', 'ab.pain', 'nausea', 'dyspepsia', 'diarrhea', 'upper.resp.infect', 'coad')
  aeReport(ssafety, ae, 'trx', 'week', id='id', times.tables=c(4,12), forceBinary=TRUE, ylim=c(0,0.15))
}
}

