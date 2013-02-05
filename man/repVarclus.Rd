\name{repVarclus}
\alias{repVarclus}
\title{Variable Clustering Diagrams}
\usage{
  repVarclus(data, time, times = sort(unique(time)),
    nmin = 10)
}
\arguments{
  \item{data}{data.frame. Data used for report.}

  \item{time}{numeric vector. Time for each record.}

  \item{times}{numeric vector. Subset of times to use.}

  \item{nmin}{numeric. Variables must have four responses
  or at least}
}
\description{
  Generate variable clustering diagrams for each time
  period.
}
\details{
  Variables are grouped according to how they are
  correlated with one another, as measured by the square of
  the Spearman \eqn{\rho} rank correlation coefficient
  computed on all pairs of variables.  Variables connected
  on lower branches are more highly correlated with one
  another. Variables missing in more than 75% of the
  observations or categorical variables having more than 20
  levels are ignored. Categories less than 0.1 prevalent
  are pooled with other rare categories.
}
\examples{
\dontrun{
  dat <- data.frame(age=sample(41:80, replace=TRUE, 1000), height=round(rnorm(1000, 68, 4)))
  dat$weight <- sample(140:200, replace=TRUE, 1000)+(dat$height-60)*3
  repVarclus(dat, numeric(1000))
}
}

