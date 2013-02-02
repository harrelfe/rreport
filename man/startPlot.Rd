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

  \item{\dots}{Arguments to be passed to \code{setps} or
  \code{setpdf}.}
}
\description{
  Toggle plotting.  Sets options by examining
  \code{.Options$rreport.gtype}.
}
\seealso{
  \code{\link{ps.slide}}
}

