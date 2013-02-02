\name{combineEqual}
\alias{combineEqual}
\title{Combine Equal}
\usage{
  combineEqual(x, maxChar = 24)
}
\arguments{
  \item{x}{numeric. Contingency table or matrix of names
  and counts, see \code{table}.}

  \item{maxChar}{numeric. Maximum length of character
  string.  Names exceeding this will be replaced with a
  letter-code.}
}
\value{
  a list with three elements \item{x}{Named vector of code
  frequencies.  The name corresponds to the code.}

  \item{codes}{Character vector of alpha-code labels.}

  \item{defs}{Character vector of code definitions.}
}
\description{
  Given a contingency table of counts, combine factors with
  equal counts.
}
\details{
  Factor names will be pasted together to make new names.
  A code and definition will be generated if the new name
  should exceed \code{maxChar}.
}
\examples{
combineEqual(table(rep(991:1010, times=rep(1:4, each=5))))
combineEqual(table(rep(991:1010, times=rep(1:4, each=5))), maxChar=10)
}

