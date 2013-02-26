\name{plotVect}
\alias{plotVect}
\title{Vector Plot}
\usage{
  plotVect(vect, xlim = NULL, ylim = NULL, newGraph = TRUE,
    plotGrid = TRUE, gridVal = NULL, gridCex = 0.5,
    startXYCoor = c(0, 0), vertical = TRUE,
    alignment = c(1, 2), maxLen = 1, minLen = 0.1,
    width = 0.1, breakLen = 0.5,
    labels = as.character(vect), labelMaxLen = 15,
    labelCex = 0.5, col = gray(0.8), lwd = NULL,
    title = "", titleCex = 1, titleXY = startXYCoor)
}
\arguments{
  \item{vect}{numeric vector. Data points to plot.}

  \item{xlim}{numeric vector. x-axis limits.}

  \item{ylim}{numeric vector. y-axis limits.}

  \item{newGraph}{logical. Defaults to \sQuote{TRUE}, which
  creates a new graph.}

  \item{plotGrid}{logical. Set to \sQuote{TRUE} to plot
  grid lines.}

  \item{gridVal}{numeric vector. Location of grid lines.}

  \item{gridCex}{numeric. Grid label text magnification.}

  \item{startXYCoor}{numeric vector. Starting points for X-
  and Y-coordinates. Defaults to \code{c(0,0)}.}

  \item{vertical}{logical. Defaults to \sQuote{TRUE}, grid
  lines are vertical.}

  \item{alignment}{numeric vector. Set alignment.  If
  \code{vertical} is \sQuote{TRUE}, \sQuote{1} =
  \sQuote{left} and \sQuote{2} = \sQuote{right}. Otherwise
  \sQuote{1} = \sQuote{upper} and \sQuote{2} =
  \sQuote{lower}.}

  \item{maxLen}{numeric. Defaults to \sQuote{1}.}

  \item{minLen}{numeric. Defaults to \sQuote{0.1}.}

  \item{width}{numeric. Set rectangle width, defaults to
  \sQuote{0.1}.}

  \item{breakLen}{numeric. Defaults to \sQuote{0.5}.}

  \item{labels}{character vector. Labels for each data
  point.}

  \item{labelMaxLen}{numeric. Maximum character length for
  labels.}

  \item{labelCex}{numeric. Label text magnification.}

  \item{col}{character vector. Specify colors for
  rectangles.}

  \item{lwd}{numeric. Specify line width.}

  \item{title}{character. Title for plot.}

  \item{titleCex}{numeric. Title text magnification.}

  \item{titleXY}{numeric vector. Set X- and Y-coordinates
  for the title.}
}
\description{
  This function plots a (positive) vector with non-zero
  elements displayed as rectangles aligned to either left
  or right. The length of a rectangle is proportional to
  the value of the vector.
}

