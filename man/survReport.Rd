\name{survReport}
\alias{survReport}
\title{Survival Report}
\usage{
  survReport(etime, event, treat, group = NULL,
    treat.group = NULL, data,
    ylabel = "Survival Probability",
    conf = c("bars", "bands", "none"), n = NULL,
    labels = NULL, previousActual = NULL, h = 4,
    append = FALSE, fileName = "trt",
    descrip = "treatment", ndescrip = "", ...)
}
\arguments{
  \item{etime}{character. Name of column with event time
  data.}

  \item{event}{character. Name of column with event status
  data.}

  \item{treat}{character. Name of column with treatment
  data.}

  \item{group}{character. Name of column with group data.}

  \item{treat.group}{character. Name of column with
  treatment group data.}

  \item{data}{data.frame. Data used for report.}

  \item{ylabel}{character. Passed to
  \code{\link[rms]{survplot.survfit}} as the \code{ylab}
  argument.}

  \item{conf}{character. See
  \code{\link[rms]{survplot.survfit}}.}

  \item{n}{numeric. See \code{\link[Hmisc]{ldBands}}.}

  \item{labels}{character vector.  See
  \code{\link[Hmisc]{plot.ldBands}}.}

  \item{previousActual}{Passed as the second component to
  the \code{actual} argument in the
  \code{\link[Hmisc]{plot.ldBands}} function.}

  \item{h}{numeric. Height of plot. Default is 4in. See
  \code{\link[Hmisc]{setps}}.}

  \item{append}{logical. If \sQuote{TRUE} output will be
  appended instead of overwritten.}

  \item{fileName}{character. File name suffix.}

  \item{descrip}{character. Used in caption to describe the
  predictor of interest. The default is
  \sQuote{treatment}.}

  \item{ndescrip}{character. Used in longcaption to provide
  further detail.}

  \item{\dots}{additional arguments, passed to
  \code{\link[rms]{survplot}} and
  \code{\link[Hmisc]{ldBands}}.}
}
\description{
  Generate a survival report with Kaplan-Meier estimates,
}
\details{
  If both \code{group} and \code{treat.group} are present,
  plots will be generated for each level of group.
}
\examples{
\dontrun{
  set.seed(47)
  mydata <- data.frame(time=sapply(round(rnorm(1000, 36, 5)), min, 36), treat=rep(0:1, each=500))
  mydata$event <- ifelse(mydata$time < 36, 1, 0)
  survReport("time", "event", "treat", data=mydata)
}
}

