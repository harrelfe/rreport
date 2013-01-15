options('rreport.appendix.file.name' = 'app.tex')
options('rreport.closed.generated.tex.dir' = 'gentex')
options('rreport.open.generated.tex.dir' = 'gentex')
options('rreport.closed.graphics.dir' = 'pdf')
options('rreport.open.graphics.dir' = 'pdf')
options('rreport.closed.filename.mask' = NULL)
options('rreport.open.filename.mask' = 'O%s')

#' Initialize R Report
#'
#' summary
#'
#' details
#'
#' @param dir.open.tex NEEDDOC
#' @param dir.closed.tex NEEDDOC
#' @param dir.open.graph NEEDDOC
#' @param dir.closed.graph NEEDDOC
#' @param open.mask NEEDDOC
#' @param closed.mask NEEDDOC
#' @param appendix.name NEEDDOC
#' @param empty.dir NEEDDOC
#' @export
#' @examples
#' 1

rreportInit <- function(dir.open.tex, dir.closed.tex,
                        dir.open.graph,dir.closed.graph,
                        open.mask, closed.mask,
                        appendix.name, empty.dir=FALSE) {
  openTexDir(dir.open.tex)
  closedTexDir(dir.closed.tex)
  openGraphDir(dir.open.graph)
  closedGraphDir(dir.closed.graph)
  openNameMask(mask=open.mask)
  closedNameMask(mask=closed.mask)
  appendixName(appendix.name)


  ## Test directory existance
  if(is.na(file.info(closedTexDir())$isdir) || !file.info(closedTexDir())$isdir) {
    stop('Directory for closed report tex files', closedTexDir(), 'does not exist')
  }

  if(is.na(file.info(openTexDir())$isdir) || !file.info(openTexDir())$isdir) {
    stop('Directory for open report tex files', openTexDir(), 'does not exist')
  }

  if(is.na(file.info(closedGraphDir())$isdir) || !file.info(closedGraphDir())$isdir) {
    stop('Directory for closed graphics output', closedGraphDir(), 'does not exist')
  }

  if(is.na(file.info(openGraphDir())$isdir) || !file.info(openGraphDir())$isdir) {
    stop('Directory for open graphics output', openGraphDir(), 'does not exist')
  }

  if(empty.dir) {
    unlink(file.path(c(closedTexDir(), openTexDir()), '*'))
    unlink(file.path(c(closedGraphDir(), openGraphDir()), '*'))   
  }

  cat('', file=AppendixPath(open.report=FALSE))
  cat('', file=AppendixPath(open.report=TRUE))

  NULL
}

#' @rdname rreportInit
#' @param filename NEEDDOC
#' @param mask NEEDDOC
#' @export

openNameMask <- function(filename, mask) {
  if(!missing(mask) && !is.null(mask)) {
    options(rreport.open.filename.mask = mask)
  }

  mask <- options("rreport.open.filename.mask")[[1]]

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

#' @rdname rreportInit
#' @export

closedNameMask <- function(filename, mask) {
  if(!missing(mask) && !is.null(mask)) {
    options(rreport.closed.filename.mask = mask)
  }

  mask <- options("rreport.closed.filename.mask")[[1]]

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

#' @rdname rreportInit
#' @param open.report NEEDDOC
#' @export

FilenameMask <- function(filename, open.report) {
  if(open.report) {
    openNameMask(filename=filename)
  } else {
    closedNameMask(filename=filename)
  }
}

#' @rdname rreportInit
#' @param name NEEDDOC
#' @export

openGraphDir <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.open.graphics.dir = name)
  }
  
  options("rreport.open.graphics.dir")[[1]]
}

#' @rdname rreportInit
#' @export

closedGraphDir <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.closed.graphics.dir = name)
  }
  
  options("rreport.closed.graphics.dir")[[1]]
}

#' @rdname rreportInit
#' @export

GraphDirName <- function(open.report) {
  if(open.report) {
    openGraphDir()
  } else {
    closedGraphDir()
  }
}

#' @rdname rreportInit
#' @export

openTexDir <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.open.generated.tex.dir = name)
  }
  
  options("rreport.open.generated.tex.dir")[[1]]
}

#' @rdname rreportInit
#' @export

closedTexDir <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.closed.generated.tex.dir = name)
  }
  
  options("rreport.closed.generated.tex.dir")[[1]]
}

#' @rdname rreportInit
#' @export

TexDirName <- function(open.report) {
  if(open.report) {
    openTexDir()
  } else {
    closedTexDir()
  }
}

#' @rdname rreportInit
#' @export

appendixName <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.appendix.file.name = name)
  }
  
  options("rreport.appendix.file.name")[[1]]
}

#' @rdname rreportInit
#' @export

AppendixName <- function(open.report) {
  FilenameMask(appendixName(), open.report)
}

#' @rdname rreportInit
#' @export

AppendixPath <- function(open.report) {
  file.path(TexDirName(open.report), AppendixName(open.report))
}
