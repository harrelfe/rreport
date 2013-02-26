\name{mockTable}
\alias{mockTable}
\title{Mock Table}
\usage{
  mockTable(control, append = TRUE)
}
\arguments{
  \item{control}{list. Components used to generate report.
  \sQuote{datafile} is the input file. \sQuote{keyfile} is
  the key output file. \sQuote{treatments} is the treatment
  value for each record. \sQuote{byVar} is the byVar.
  \sQuote{file} is the output file.
  \sQuote{defaultOrientation} specifies page orientation.
  \sQuote{pos} specifies caption location. \sQuote{CLs}
  contains lower and upper confidence levels.
  \sQuote{Pvalues} contains P-values.}

  \item{append}{logical. If \sQuote{TRUE} output will be
  appended instead of overwritten.}
}
\description{
  Generates a LaTeX report from the \code{control}.
}

