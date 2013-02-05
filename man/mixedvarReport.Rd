\name{baselineReport}
\alias{baselineReport}
\alias{mixedvarReport}
\title{Mixed Variable Report}
\usage{
  baselineReport(...)

  mixedvarReport(data, vars, panel, treat,
    longPanel = panel, ncaption = "", tableref = panel,
    test = TRUE, exclude1 = TRUE, long = TRUE,
    npct = c("numerator", "both", "denominator", "none"),
    prmsd = FALSE,
    contDataPlotType = c("bp", "dot", "raw"), nmin = 15,
    categDataPlot = !Odotchart,
    cdfPlot = length(levels(data[[treat]])) <= 2,
    contDataPlot = !cdfPlot, Ohist = TRUE,
    Odotchart = TRUE, bpPrototype = FALSE, digits = 3,
    append = FALSE, Major = NULL, MajorLabel = "",
    Majorvars = NULL, cexMajor = 0.7, continuous = 10,
    nx = 15, keyloc = list(x = 0.8, y = 0.02),
    landscape = FALSE, size = NULL, longtable = FALSE,
    h = 5, w = 6, lines.page = 40, clearPlots = FALSE,
    auxCol = NULL, prn = TRUE, ...)
}
\arguments{
  \item{data}{data.frame. Data used for report.}

  \item{vars}{character vector. Variables to include in
  analysis.}

  \item{panel}{character. Name for panel.}

  \item{treat}{character. Name of treatment variable within
  dataset.}

  \item{longPanel}{character. Long name for panel.}

  \item{ncaption}{character. Caption text.}

  \item{tableref}{character. Passed to the function
  \code{\link[Hmisc]{latex}}.}

  \item{test}{logical. See
  \code{\link[Hmisc]{summary.formula}}.}

  \item{exclude1}{logical. See
  \code{\link[Hmisc]{summary.formula}}.}

  \item{long}{logical. See
  \code{\link[Hmisc]{summary.formula}}.}

  \item{npct}{character. See
  \code{\link[Hmisc]{summary.formula}}.}

  \item{prmsd}{logical. See
  \code{\link[Hmisc]{summary.formula}}.}

  \item{contDataPlotType}{character. Continuous data plot
  type should be one of the following: \sQuote{bp},
  \sQuote{dot}, or \sQuote{raw}.}

  \item{nmin}{numeric. See
  \code{\link[Hmisc]{summary.formula}}.}

  \item{categDataPlot}{logical. Set to \sQuote{TRUE} to
  output a categorical data plot in the latex table being
  generated.}

  \item{cdfPlot}{logical. Set to \sQuote{TRUE} to output a
  CDF plot in the latex table being generated; only
  available to closed report.}

  \item{contDataPlot}{logical. Set to \sQuote{TRUE} to
  output a continous data plot in the latex table being
  generated; only available to closed report.}

  \item{Ohist}{logical. Set to \sQuote{TRUE} to output a
  histogram of the continuous variables.}

  \item{Odotchart}{logical. See
  \code{\link[Hmisc]{summary.formula}}.}

  \item{bpPrototype}{logical. If \sQuote{TRUE} and
  \code{contDataPlotType} is \sQuote{bp}, output a
  prototype box-percentile plot.}

  \item{digits}{numeric. Significant digits, passed to the
  function \code{\link[Hmisc]{latex}}.}

  \item{append}{logical. If \sQuote{TRUE} output will be
  appended instead of overwritten.}

  \item{Major}{character vector. Major categorical
  variables for site.}

  \item{MajorLabel}{character. Label for major
  stratification variable(s).}

  \item{Majorvars}{character vector. Major categorical
  variables for site. (NEED TO DISTINGUISH)}

  \item{cexMajor}{numeric. Plotting text size
  magnification, used for boxplots.}

  \item{continuous}{numeric. See
  \code{\link[Hmisc]{summary.formula}}.}

  \item{nx}{numeric. THIS PARAMETER IS NOT USED.}

  \item{keyloc}{numeric vector. Key placement on the
  categorical data plot, see
  \code{\link[Hmisc]{summary.formula}}.}

  \item{landscape}{logical See \code{\link[Hmisc]{latex}}.}

  \item{size}{character. See \code{\link[Hmisc]{latex}}.}

  \item{longtable}{logical. See
  \code{\link[Hmisc]{latex}}.}

  \item{h}{numeric. Height of plot, only used for
  categorical data plot. Default is 5in. See
  \code{\link[Hmisc]{setps}}.}

  \item{w}{numeric. Width of plot, only used for
  categorical data plot. Default is 6in. See
  \code{\link[Hmisc]{setps}}.}

  \item{lines.page}{numeric. See
  \code{\link[Hmisc]{latex}}.}

  \item{clearPlots}{logical. Set to \sQuote{TRUE} to clear
  the page after each plot.}

  \item{auxCol}{list. See
  \code{\link[Hmisc]{summary.formula}}.}

  \item{prn}{logical. See
  \code{\link[Hmisc]{summary.formula}}.}

  \item{\dots}{Additional arguments, passed to
  \code{\link[graphics]{plot}}. Only used for categorical
  data plot.}
}
\description{
  Generate several reports.
}
\details{
  \itemize{ \item Generate a summary table of treatment
  against all variables. \item Plot categorical data if
  \code{categDataPlot} is \sQuote{TRUE}. \item Plot
  continuous data if \code{contDataPlot} is \sQuote{TRUE}.
  \item Include a prototype box-percentile plot if
  \code{contDataPlotType}. is \sQuote{bp} and
  \code{bpPrototype} is \sQuote{TRUE}. \item Plot the
  cumulative distribution function. \item Plot a histogram
  for categorical variables if \code{Ohist} is
  \sQuote{TRUE}. \item Print a box-percentile plot for each
  variable stratified by \code{Major}. }
}

