\name{floor.chron}
\alias{ceiling.chron}
\alias{floor.chron}
\title{Round Chronological Objects}
\usage{
  floor.chron(x,
    units = c("minutes", "hours", "days", "months", "years"))

  ceiling.chron(x,
    units = c("minutes", "hours", "days", "months", "years"))
}
\arguments{
  \item{x}{a chron vector.}

  \item{units}{character.  Round to nearest unit, defined
  as minutes, hours, days, months, or years.}
}
\description{
  Given a vector of chron objects, return a vector with
  values rounded to the given unit. \code{floor.chron} will
  round down and \code{ceiling.chron) will round up.
}
\details{
  details
}
\examples{
floor.chron(chron(dates = dates(c("01/25/13", "02/03/13", "11/15/13")), times = times(c("06:30:15", "12:19:51", "17:11:13"))), units='hours')
ceiling.chron(chron(dates = dates(c("01/25/13", "02/03/13", "11/15/13")), times = times(c("06:30:15", "12:19:51", "17:11:13"))), units='months')
}

