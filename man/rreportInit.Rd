\name{rreportInit}
\alias{rreportInit}
\title{Initialize R Report}
\usage{
  rreportInit(dir.tex = getOption("rreport.generated.tex.dir"),
    dir.graph = getOption("rreport.graphics.dir"),
    open.mask = getOption("rreport.open.filename.mask"),
    closed.mask = getOption("rreport.closed.filename.mask"),
    appendix.name = getOption("rreport.appendix.file.name"),
    empty.dir = FALSE)
}
\arguments{
  \item{dir.tex}{character. directory name for report tex
  files}

  \item{dir.graph}{character. directory name for report
  graphic files}

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

