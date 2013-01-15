\name{mixedvarReport}
\alias{mixedvarReport}
\title{Mixed Variable Report}
\usage{
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
  \item{data}{NEEDDOC}

  \item{vars}{NEEDDOC}

  \item{panel}{NEEDDOC}

  \item{treat}{NEEDDOC}

  \item{longPanel}{NEEDDOC}

  \item{ncaption}{NEEDDOC}

  \item{tableref}{NEEDDOC}

  \item{test}{NEEDDOC}

  \item{exclude1}{NEEDDOC}

  \item{long}{NEEDDOC}

  \item{npct}{NEEDDOC}

  \item{prmsd}{NEEDDOC}

  \item{contDataPlotType}{NEEDDOC}

  \item{nmin}{NEEDDOC}

  \item{categDataPlot}{NEEDDOC}

  \item{cdfPlot}{NEEDDOC}

  \item{contDataPlot}{NEEDDOC}

  \item{Ohist}{NEEDDOC}

  \item{Odotchart}{NEEDDOC}

  \item{bpPrototype}{NEEDDOC}

  \item{digits}{NEEDDOC}

  \item{append}{NEEDDOC}

  \item{Major}{NEEDDOC}

  \item{MajorLabel}{NEEDDOC}

  \item{Majorvars}{NEEDDOC}

  \item{cexMajor}{NEEDDOC}

  \item{continuous}{NEEDDOC}

  \item{nx}{NEEDDOC}

  \item{keyloc}{NEEDDOC}

  \item{landscape}{NEEDDOC}

  \item{size}{NEEDDOC}

  \item{longtable}{NEEDDOC}

  \item{h}{NEEDDOC}

  \item{w}{NEEDDOC}

  \item{lines.page}{NEEDDOC}

  \item{clearPlots}{NEEDDOC}

  \item{auxCol}{NEEDDOC}

  \item{prn}{NEEDDOC}

  \item{\dots}{NEEDDOC}
}
\value{
  return something
}
\description{
  summary
}
\details{
  details
}
\examples{
1
}

