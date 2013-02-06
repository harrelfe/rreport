\name{makeNA}
\alias{makeNA}
\title{Make NA}
\usage{
  makeNA(data, mins, maxs)
}
\arguments{
  \item{data}{data.frame. Dataset with numerical values to
  range check.}

  \item{mins}{named numeric vector. Minimum value for each
  named column.}

  \item{maxs}{named numeric vector. Maximum value for each
  named column.}
}
\value{
  Returns modified data.frame invisibly.
}
\description{
  Examine a dataset for numeric values outside of a desired
  range. Bad values will be replaced with NAs.
}
\details{
  \code{mins} and \code{maxs} should be named vectors,
  where the names are columns found in \code{data}. They
  should contain the same names and have the same length.
}
\examples{
set.seed(100)
df <- data.frame(x=rnorm(100), y=rnorm(100, sd=0.5))
na.df <- makeNA(df, c(x=-2,y=-1), c(x=2,y=1))
}

