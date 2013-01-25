\name{putparams}
\alias{putparams}
\title{Put Params}
\usage{
  putparams(..., paramFile)
}
\arguments{
  \item{\dots}{list of name-value pairs. parameter names
  and their associated formats}

  \item{paramFile}{character. params filename}
}
\description{
  Define parameters and provide LaTeX formatting.
}
\details{
  Parameters will be saved to the parameter LaTeX file.
}
\examples{
putparams(go=1, fish='blue', paramFile='') #prints to standard output
}

