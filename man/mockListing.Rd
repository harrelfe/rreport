\name{mockListing}
\alias{mockListing}
\title{Mock Listing}
\usage{
  mockListing(control, caption, fields, orientation = "",
    size = "")
}
\arguments{
  \item{control}{list. Components used to generate report.
  \sQuote{file} is the output file. \sQuote{baseListVar}
  \sQuote{defaultOrientation} \sQuote{pos}}

  \item{caption}{character. Table caption.}

  \item{fields}{NEEDDOC}

  \item{orientation}{character. Specify page orientation.}

  \item{size}{NEEDDOC}
}
\description{
  Generate a LaTeX table from the \code{control}.
}

