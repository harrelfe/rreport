\name{plotKmHalfCL}
\alias{plotKmHalfCL}
\title{Kaplan-Meier Estimates}
\usage{
  plotKmHalfCL(fit, times, fun = function(x) x, offset = 0,
    lwd = 0.5, lty = 1)
}
\arguments{
  \item{fit}{survfit object. See
  \code{\link[rms]{survfit.formula}}.}

  \item{times}{numeric vector. Time value for each record.}

  \item{fun}{function. Function to plot estimates.}

  \item{offset}{numeric. Offset value to apply to
  \sQuote{x} coordinate points.}

  \item{lwd}{numeric. The line width, passed to
  \code{lines}.}

  \item{lty}{numeric. The line type, passed to
  \code{lines}.}
}
\description{
  For two strata, estimates the standard error of the
  difference in two Kaplan-Meier estimates at each value of
  times, and plots half-width of confidence level for the
  difference, centered at the midpoint of the survival
  estimates.
}
\details{
  details
}
\examples{
set.seed(20)
time <- rep(365, 50)
event <- rbinom(50, 1, 1/3)
time[event == 1] <- sample(365, sum(event == 1), replace=TRUE)
trt <- sample(1:2, 50, replace=TRUE)
require('rms')
fit <- survfit.formula(Surv(time, event) ~ trt)
survplot.survfit(fit)
plotKmHalfCL(fit, time)
}

