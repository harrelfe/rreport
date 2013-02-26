\name{plotEvents}
\alias{plotEvents}
\title{Plot Events}
\usage{
  plotEvents(major, minor, stratLevels, width = 0.5,
    breakWidth = 0.5, sparseKoef = 1.5,
    minorToMajorKoef = 5, plotGrid = TRUE,
    majorGrid = NULL, minorGrid = NULL, title = "",
    fileName = NULL, titleOffsetKoef = 3, titleOffsetX = 0,
    graphWidth = 8, graphHeight = 11, gridCex = 0.5,
    labelCex = 0.5, labelLen = 10)
}
\arguments{
  \item{major}{data.frame. Subset of data by major
  variable.}

  \item{minor}{data.frame. Subset of data by minor
  variable.}

  \item{stratLevels}{character vector. Vector of
  stratification levels.}

  \item{width}{numeric.}

  \item{breakWidth}{numeric.}

  \item{sparseKoef}{numeric. How farther away the graphs
  for different \code{stratLevels} values should be located
  on the diagram.}

  \item{minorToMajorKoef}{numeric. The distance the bars
  for the minor category should be longer than the bars for
  the major category.}

  \item{plotGrid}{logical. Set to \sQuote{TRUE} to plot
  grid lines.}

  \item{majorGrid}{numeric vector. Grid of major category.}

  \item{minorGrid}{numeric vector. Grid of minor category.}

  \item{title}{named character vector. Titles for each
  frequency display.}

  \item{fileName}{character. Name of output file, defaults
  to \sQuote{NULL}.}

  \item{titleOffsetKoef}{numeric. The vertical distance the
  title should stay from the graph.}

  \item{titleOffsetX}{numeric. The horizontal distance the
  title should stay from the graph.}

  \item{graphWidth}{numeric. Width of plot, the default is
  \sQuote{8}.}

  \item{graphHeight}{numeric. Height of plot, the default
  is \sQuote{11}.}

  \item{gridCex}{numeric. Relative size of the grid
  digits.}

  \item{labelCex}{numeric. Relative size of the labels.}

  \item{labelLen}{numeric. Maximum length of grid labels.}
}
\description{
  summary
}

