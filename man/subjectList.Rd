\name{subjectList}
\alias{subjectList}
\title{Subject List}
\usage{
  subjectList(data, panel, caption = NULL,
    vname = c("labels", "names"), colheads = NULL,
    size = "smaller", longtable = TRUE, landscape = TRUE)
}
\arguments{
  \item{data}{data.frame. Data used for report.}

  \item{panel}{character. Name of panel.}

  \item{caption}{character. See
  \code{\link[Hmisc]{latex}}.}

  \item{vname}{character. Specifies how to generate column
  headings, either through variable \sQuote{labels} or
  \sQuote{names}.}

  \item{colheads}{character vector. Column headings for
  each variable.}

  \item{size}{character. Set LaTeX table font size, see
  \code{\link[Hmisc]{latex}}.}

  \item{longtable}{logical. See
  \code{\link[Hmisc]{latex}}.}

  \item{landscape}{logical. See
  \code{\link[Hmisc]{latex}}.}
}
\description{
  Generate a LaTeX table from a dataset.
}
\examples{
\dontrun{
  load(url('http://biostat.mc.vanderbilt.edu/wiki/pub/Main/Rreport/ssafety.rda'))
  subjectList(ssafety[1:10,1:10], "datalist", vname='names')
}
}

