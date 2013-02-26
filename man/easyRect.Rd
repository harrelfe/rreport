\name{easyRect}
\alias{easyRect}
\title{Plot Rectangle}
\usage{
  easyRect(startXY, xLen, yLen, corner = TRUE, ...)
}
\arguments{
  \item{startXY}{numeric vector. X- and Y-coordinates to
  start plot.}

  \item{xLen}{numeric. Horizontal length of rectangle.}

  \item{yLen}{numeric. Vertical length of rectangle.}

  \item{corner}{logical. When \sQuote{TRUE}, \code{startXY}
  refers to the top left corner of the plot, otherwise the
  center.}

  \item{\dots}{additional arguments, passed to \code{lines}
  and \code{polygon}.}
}
\description{
  Plots a rectangle.
}
\details{
  Call \code{plot.new} prior to usage.
}

