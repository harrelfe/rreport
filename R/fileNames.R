#' Initialize R Report
#'
#' Initialize an \sQuote{rreport} by setting package options.
#'
#' @param dir.open.tex character. directory name for open report tex files
#' @param dir.closed.tex character. directory name for closed report tex files
#' @param dir.open.graph character. directory name for open report graphic files
#' @param dir.closed.graph character. directory name for closed report graphic files
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

rreportInit <- function(dir.open.tex=getOption("rreport.open.generated.tex.dir"),
                        dir.closed.tex=getOption("rreport.closed.generated.tex.dir"),
                        dir.open.graph=getOption("rreport.open.graphics.dir"),
                        dir.closed.graph=getOption("rreport.closed.graphics.dir"),
                        open.mask=getOption("rreport.open.filename.mask"),
                        closed.mask=getOption("rreport.closed.filename.mask"),
                        appendix.name=getOption("rreport.appendix.file.name"),
                        empty.dir=FALSE) {
  dot <- file.info(dir.open.tex)$isdir
  dct <- file.info(dir.closed.tex)$isdir
  dog <- file.info(dir.open.graph)$isdir
  dcg <- file.info(dir.closed.graph)$isdir
  if(is.na(dct) || !dct) {
    stop('Directory for closed report tex files ', dir.closed.tex, ' does not exist')
  }
  if(is.na(dot) || !dot) {
    stop('Directory for open report tex files ', dir.open.tex, ' does not exist')
  }
  if(is.na(dcg) || !dcg) {
    stop('Directory for closed graphics output ', dir.closed.graph, ' does not exist')
  }
  if(is.na(dog) || !dog) {
    stop('Directory for open graphics output ', dir.open.graph, ' does not exist')
  }
  options(rreport.open.generated.tex.dir = dir.open.tex)
  options(rreport.closed.generated.tex.dir = dir.closed.tex)
  options(rreport.open.graphics.dir = dir.open.graph)
  options(rreport.closed.graphics.dir = dir.closed.graph)
  options(rreport.open.filename.mask = open.mask)
  options(rreport.closed.filename.mask = closed.mask)
  options(rreport.appendix.file.name = appendix.name)

  if(empty.dir) {
    unlink(file.path(c(dir.closed.tex, dir.open.tex), '*'))
    unlink(file.path(c(dir.closed.graph, dir.open.graph), '*'))   
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

GraphDirName <- function(open.report=TRUE) {
  if(open.report) {
    getOption("rreport.open.graphics.dir")
  } else {
    getOption("rreport.closed.graphics.dir")
  }
}

TexDirName <- function(open.report=TRUE) {
  if(open.report) {
    getOption("rreport.open.generated.tex.dir")
  } else {
    getOption("rreport.closed.generated.tex.dir")
  }
}

AppendixName <- function(open.report=TRUE) {
  FilenameMask(getOption("rreport.appendix.file.name"), open.report)
}

AppendixPath <- function(open.report=TRUE) {
  file.path(TexDirName(open.report), AppendixName(open.report))
}

paramTexFile <- function(open.report=TRUE) {
  file.path(TexDirName(open.report), 'params.tex')
}
