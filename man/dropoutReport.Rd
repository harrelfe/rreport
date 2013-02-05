\name{dropoutReport}
\alias{dropoutReport}
\title{Dropout Report}
\usage{
  dropoutReport(d.dropout, dropout, treat, time.inc = NULL,
    ylim = c(0, 1), panel = "dropout", what = "study", ...)
}
\arguments{
  \item{d.dropout}{numeric vector. Dropout date.}

  \item{dropout}{numeric vector. Indicator variable for
  dropout.}

  \item{treat}{factor vector. Vector of treatment group for
  each record.}

  \item{time.inc}{numeric. See
  \code{\link[rms]{survplot}}.}

  \item{ylim}{numeric vector. See
  \code{\link[rms]{survplot}}.}

  \item{panel}{character. Name for panel, defaults to
  \sQuote{dropout}.}

  \item{what}{character. Name of study, defaults to
  \sQuote{study}.}

  \item{\dots}{additional arguments, passed to
  \code{\link[rms]{survplot}}.}
}
\description{
  Generate a survival plot for subjects remaining in the
  study.
}
\examples{
\dontrun{
  d.d <- sample(1:10, 200, replace=TRUE, prob=c(rep(0.03,9), 0.73))
  dropout <- as.numeric(d.d < 10)
  dropoutReport(d.d, dropout, as.factor(sample(c('A','B'), 200, replace=TRUE)), time.inc=2)
}
}

