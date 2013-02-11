\name{mfrowSet}
\alias{mfrowSet}
\title{Set mfrow Parameter}
\usage{
  mfrowSet(n, trellis = FALSE, small = FALSE)
}
\arguments{
  \item{n}{numeric. Totoal number of figures to place in
  layout.}

  \item{trellis}{logical. Set to \sQuote{TRUE} when a
  \sQuote{trellis} plot is requested.}

  \item{small}{logical. Set to \sQuote{TRUE} if the plot
  area should be smaller to accomodate many plots.}
}
\value{
  return numeric vector. If \code{trellis = TRUE} the
  suggested \sQuote(mfrow) is returned. Otherwise the
  original \sQuote{mfrow} is returned invisibly.
}
\description{
  Compute and set a good \code{par(\sQuote{mfrow})} given
  the number of figures to plot.
}
\details{
  \code{trellis} and \code{small} may not both be specified
  as \sQuote{TRUE}.
}
\examples{
oldmfrow <- mfrowSet(8)
}

