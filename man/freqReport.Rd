\name{freqReport}
\alias{freqReport}
\title{Frequency Report}
\usage{
  freqReport(type, panel, treat, longPanel = panel,
    typeLabel = label(type), plotprop = FALSE,
    Ntreat = NULL, omitZeros = TRUE, ylim = c(0, 1), h = 5,
    w = 5, digits = 3, size = NULL, longtable = FALSE,
    lines.page = 40, append = FALSE)
}
\arguments{
  \item{type}{character vector. Type for each record.}

  \item{panel}{character. Name for panel.}

  \item{treat}{factor vector. Treatment group for each
  record.}

  \item{longPanel}{character. Long name for panel.}

  \item{typeLabel}{character. Label for type variable.}

  \item{plotprop}{logical. Set to \sQuote{TRUE} to output a
  plot of proportions.}

  \item{Ntreat}{numeric vector. Add a column header with
  \sQuote{N} counts.}

  \item{omitZeros}{logical. If \sQuote{TRUE} remove all
  zero counts from the frequency table.}

  \item{ylim}{numeric vector. Set y-axis limits. THIS
  PARAMETER IS NOT USED.}

  \item{h}{numeric. Height of plot. Default is 5in. See
  \code{\link[Hmisc]{setps}}.}

  \item{w}{numeric. Width of plot. Default is 3in. See
  \code{\link[Hmisc]{setps}}.}

  \item{digits}{numeric. Number of significant digits to
  print. Defaults to 3.}

  \item{size}{character. Set LaTeX table font size, see
  \code{\link[Hmisc]{latex}}.}

  \item{longtable}{logical. Use LaTeX \sQuote{longtable}
  style. See \code{\link[Hmisc]{latex}}.}

  \item{lines.page}{numeric. Maximum number of lines in the
  body of a table to be placed on a single page. See
  \code{\link[Hmisc]{latex}}.}

  \item{append}{logical. If \sQuote{TRUE} output will be
  appended instead of overwritten.}
}
\description{
  Generate tables with the frequencies found for
  \code{type}.
}
\details{
  THIS FUNCTION IS INCOMPLETE
}

