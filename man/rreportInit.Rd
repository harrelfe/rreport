\name{rreportInit}
\alias{rreportInit}
\title{Initialize R Report}
\usage{
  rreportInit(dir.open.tex = getOption("rreport.open.generated.tex.dir"),
    dir.closed.tex = getOption("rreport.closed.generated.tex.dir"),
    dir.open.graph = getOption("rreport.open.graphics.dir"),
    dir.closed.graph = getOption("rreport.closed.graphics.dir"),
    open.mask = getOption("rreport.open.filename.mask"),
    closed.mask = getOption("rreport.closed.filename.mask"),
    appendix.name = getOption("rreport.appendix.file.name"),
    empty.dir = FALSE)
}
\arguments{
  \item{dir.open.tex}{character. directory name for open
  report tex files}

  \item{dir.closed.tex}{character. directory name for
  closed report tex files}

  \item{dir.open.graph}{character. directory name for open
  report graphic files}

  \item{dir.closed.graph}{character. directory name for
  closed report graphic files}

  \item{open.mask}{character. mask for open report
  filenames}

  \item{closed.mask}{character. mask for closed report
  filenames}

  \item{appendix.name}{character. filename for appendix}

  \item{empty.dir}{logical. clean out all files found in
  tex and graph directories}
}
\description{
  Initialize an \sQuote{rreport} by setting package
  options.
}
\examples{
\dontrun{
  rreportInit(empty.dir=TRUE)
}
}
\seealso{
  \code{\link{rreport.options}}
}

