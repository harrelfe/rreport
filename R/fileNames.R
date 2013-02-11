#' Initialize R Report
#'
#' Initialize an \sQuote{rreport} by setting package options.
#'
#' @param dir.tex character. directory name for report tex files
#' @param dir.graph character. directory name for report graphic files
#' @param open.mask character. mask for open report filenames
#' @param closed.mask character. mask for closed report filenames
#' @param appendix.name character. filename for appendix
#' @param empty.dir logical. clean out all files found in tex and graph directories
#' @export
#' @seealso \code{\link{rreport.options}}
#' @examples
#' \dontrun{
#'   rreportInit(empty.dir=TRUE)
#' }

rreportInit <- function(dir.tex=getOption("rreport.generated.tex.dir"),
                        dir.graph=getOption("rreport.graphics.dir"),
                        open.mask=getOption("rreport.open.filename.mask"),
                        closed.mask=getOption("rreport.closed.filename.mask"),
                        appendix.name=getOption("rreport.appendix.file.name"),
                        empty.dir=FALSE) {
  dt <- file.info(dir.tex)$isdir
  dg <- file.info(dir.graph)$isdir
  if(is.na(dt) || !dt) {
    stop('Directory for report tex files ', dir.tex, ' does not exist')
  }
  if(is.na(dg) || !dg) {
    stop('Directory for graphics output ', dir.graph, ' does not exist')
  }
  options(rreport.generated.tex.dir = dir.tex)
  options(rreport.graphics.dir = dir.graph)
  options(rreport.open.filename.mask = open.mask)
  options(rreport.closed.filename.mask = closed.mask)
  options(rreport.appendix.file.name = appendix.name)

  if(empty.dir) {
    unlink(file.path(dir.tex, '*'))
    unlink(file.path(dir.graph, '*'))
  }
  cat('', file=AppendixPath(open.report=FALSE))
  cat('', file=AppendixPath(open.report=TRUE))
  NULL
}

FilenameMask <- function(filename, open.report=TRUE) {
  if(open.report) {
    mask <- getOption("rreport.open.filename.mask")
  } else {
    mask <- getOption("rreport.closed.filename.mask")
  }
  if(!missing(filename) && !is.null(filename)) {
    if(is.null(mask)) {
      filename
    } else {
      sprintf(mask, filename)
    }
  } else {
    mask
  }
}

GraphDirName <- function() {
  getOption("rreport.graphics.dir")
}

TexDirName <- function() {
  getOption("rreport.generated.tex.dir")
}

AppendixName <- function(open.report=TRUE) {
  FilenameMask(getOption("rreport.appendix.file.name"), open.report)
}

AppendixPath <- function(open.report=TRUE) {
  file.path(TexDirName(), AppendixName(open.report))
}

paramTexFile <- function() {
  file.path(TexDirName(), 'params.tex')
}
