\name{completenessReport}
\alias{completenessReport}
\title{Completeness Report}
\usage{
  completenessReport(data, vars, panel, Time, times,
    longPanel = panel, frac = 0.95,
    h = 4 * (nv < 15) + 5 * (nv >= 15), cex = 1,
    fullCaption = FALSE, append = TRUE)
}
\arguments{
  \item{data}{data.frame. Data to report.}

  \item{vars}{character vector. Column names to use.}

  \item{panel}{character. Name of panel.}

  \item{Time}{numeric vector. Times for each record.}

  \item{times}{numeric vector. Subset of times to use.}

  \item{longPanel}{character. Long name of panel.}

  \item{frac}{numeric. Ratio used to determine if a dotplot
  for completeness should be generated for each variable.
  Defaults to 0.95, that is, if the variable with the most
  missing data has 5% more missing than the variable with
  the least missing data, generate the dotplot.}

  \item{h}{numeric. Height of plot, passed to
  \code{\link{startPlot}}.}

  \item{cex}{numeric. Text size within plot, passed to
  \code{\link[Hmisc]{Dotplot}}.}

  \item{fullCaption}{logical. THIS PARAMETER IS NOT USED.}

  \item{append}{logical. If \sQuote{TRUE} output will be
  appended instead of overwritten.}
}
\description{
  Generate report with information about the completeness
  of the data.
}
\details{
  details
}

