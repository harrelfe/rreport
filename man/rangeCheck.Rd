\name{rangeCheck}
\alias{rangeCheck}
\title{Out-of-Range Report}
\usage{
  rangeCheck(data, colheader = "Variable", panel,
    append = FALSE)
}
\arguments{
  \item{data}{data.frame. Contains information required to
  perform checks. \sQuote{dataframe} should be the name of
  the data.frame to check. \sQuote{variable} is the name of
  the variable to check. \sQuote{label} is the variable
  label. \sQuote{min} is the minimum value. \sQuote{max} is
  the maximum value. \sQuote{units} is the unit of
  measurement.}

  \item{colheader}{character. Column header for table.}

  \item{panel}{character. Name for panel.}

  \item{append}{append logical. If \sQuote{TRUE} output
  will be appended instead of overwritten.}
}
\description{
  Generate a report on the frequency of variables found
  outside the user-defined range.
}
\examples{
\dontrun{
  load(url('http://biostat.mc.vanderbilt.edu/wiki/pub/Main/Rreport/ssafety.rda'))
  rules <- data.frame(
    dataframe = rep('ssafety', 4),
    variable = c('age', 'height', 'weight', 'bmi'),
    label = c('age', 'height', 'weight', 'bmi'),
    min = c(45, 145, 50, 15),
    max = c(80, 180, 140, 40),
    units = c('years', 'cms', 'kgs', 'cm/kg')
  )
  rangeCheck(rules, panel='check')
}
}

