\name{startPlot}
\alias{endPlot}
\alias{startPlot}
\title{Plot Initialization}
\usage{
  startPlot(file, ...)

  endPlot()
}
\arguments{
  \item{file}{character.  Character string specifying file
  prefix.}

  \item{\dots}{Arguments to be passed to
  \code{\link[Hmisc]{setps}} or
  \code{\link[Hmisc]{setpdf}}.}
}
\description{
  Toggle plotting.  Sets options by examining
  \code{.Options$rreport.gtype}.
}
\seealso{
  \code{\link[Hmisc]{ps.slide}}
}

