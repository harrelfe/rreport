options('rreport.appendix.file.name' = 'app.tex')
options('rreport.closed.generated.tex.dir' = 'gentex')
options('rreport.open.generated.tex.dir' = 'gentex')
options('rreport.graphics.dir' = 'pdf')
options('rreport.open.filename.mask' = 'O%s')

joinFiles <- function(...) {
  args <- unname(list(...))
  args$sep = '/'
  
  do.call('paste', args)
}

filenameMask <- function(filename, name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.open.filename.mask = name)
  }

  if(!missing(filename) && !is.null(filename)) {
    sprintf(options("rreport.open.filename.mask")[[1]], filename)
  } else {
    options("rreport.open.filename.mask")[[1]]
  }
}

graphicsDir <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.graphics.dir = name)
  }
  
  options("rreport.graphics.dir")[[1]]
}


openDirName <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.open.generated.tex.dir = name)
  }
  
  options("rreport.open.generated.tex.dir")[[1]]
}

closedDirName <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.closed.generated.tex.dir = name)
  }
  
  options("rreport.closed.generated.tex.dir")[[1]]
}


appendixName <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.appendix.file.name = name)
  }
  
  options("rreport.appendix.file.name")[[1]]
}
