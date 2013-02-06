\name{complianceReport}
\alias{complianceReport}
\title{Compliance Report}
\usage{
  complianceReport(comply, treat, time, times = NULL)
}
\arguments{
  \item{comply}{numeric or character vector. Indicator
  variable for compliance. Should be 1/0 or yes/no.}

  \item{treat}{factor vector. Treatment group for each
  record.}

  \item{time}{numeric vector. Time for each record.}

  \item{times}{numeric vector. Subset of times to use.}
}
\description{
  Generate compliance report by time across treatment
  groups.
}
\examples{
\dontrun{
  complianceReport(rbinom(200, 1, 0.8), as.factor(sample(c('A','B'), 200, replace=TRUE)), sample(10, 200, replace=TRUE))
}
}

