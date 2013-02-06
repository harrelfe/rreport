\name{labReport}
\alias{labReport}
\title{Lab Report}
\usage{
  labReport(data, vars, panel, treat, id, time, times,
    longPanel = panel, h = 6.5, w = 6.5, diffs = FALSE,
    cdfPlot = length(levels(data[[treat]])) <= 2,
    tables = TRUE,
    boxPlot = is.logical(cdfPlot) && !cdfPlot, digits = 3,
    append = FALSE, clearPlots = FALSE, open = TRUE, ...)
}
\arguments{
  \item{data}{data.frame. Data used for report.}

  \item{vars}{character vector. Variables to include in
  analysis.}

  \item{panel}{character. Name for panel.}

  \item{treat}{character. Name of treatment variable within
  dataset.}

  \item{id}{character. Name of id variable within dataset.}

  \item{time}{character. Name of time variable within
  dataset.}

  \item{times}{numeric vector. Subset of times to use.}

  \item{longPanel}{character. Long name for panel.}

  \item{h}{numeric. Height of plot, see
  \code{\link[Hmisc]{setps}}. THIS PARAMETER IS NOT USED.}

  \item{w}{numeric. Width of plot, see
  \code{\link[Hmisc]{setps}}. THIS PARAMETER IS NOT USED.}

  \item{diffs}{logical. THIS PARAMETER IS NOT USED.}

  \item{cdfPlot}{logical or character vector. If
  \sQuote{TRUE} generate CDF plots for variables in
  \code{vars}. If this is a vector of variables names,
  generate CDF plots for this variables.}

  \item{tables}{logical. If \sQuote{TRUE} generate latex to
  output summary tables.}

  \item{boxPlot}{logical. If \sQuote{TRUE} generate
  boxplots for variables in \code{vars}.}

  \item{digits}{numeric. Significant digits, passed to the
  function \code{\link[Hmisc]{latex}}.}

  \item{append}{logical. If \sQuote{TRUE} output will be
  appended instead of overwritten.}

  \item{clearPlots}{logical. Set to \sQuote{TRUE} to clear
  the page after each plot.}

  \item{open}{logical. Set to \sQuote{TRUE} to additionally
  create the \sQuote{open} report.}

  \item{\dots}{Additional arguments, unused at this time.}
}
\description{
  summary
}
\details{
  By default the function plots CDFs if the number of
  treatment groups is less or equal than two. If the number
  of treatment groups is three or more boxplots are
  created.
}

