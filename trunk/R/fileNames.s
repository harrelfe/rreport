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

filenameMask <- function(filename, mask) {
  if(!missing(mask) && !is.null(mask)) {
    options(rreport.open.filename.mask = mask)
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


rreportInit <- function(dir.closed, dir.open, dir.graph, open.mask, appendixName) {
  closedDirName(dir.closed)
  openDirName(dir.open)
  graphicsDir(dir.graph)
  filenameMask(mask=open.mask)
  appendixName(appendixName)


  ## Test directory existance
  if(is.na(file.stat(closedDirName())$isdir) || !file.stat(closedDirName())$isdir) {
    stop('Directory for closed report tex files', closedDirName(), 'does not exist')
  }

  if(is.na(file.stat(openDirName())$isdir) || !file.stat(openDirName())$isdir) {
    stop('Directory for open report tex files', openDirName(), 'does not exist')
  }

  if(is.na(file.stat(graphicsDir())$isdir) || !file.stat(graphicsDir())$isdir) {
    stop('Directory for graphics output', graphicsDir(), 'does not exist')
  }

  cat('', file=joinPath(closedDirName(), appendixName()))
  cat('', file=joinPath(openDirName(), appendixName()))

  NULL
}
