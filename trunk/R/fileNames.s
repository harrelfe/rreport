options('rreport.appendix.file.name' = 'app.tex')
options('rreport.closed.generated.tex.dir' = 'gentex')
options('rreport.open.generated.tex.dir' = 'gentex')
options('rreport.closed.graphics.dir' = 'pdf')
options('rreport.open.graphics.dir' = 'pdf')
options('rreport.closed.filename.mask' = NULL)
options('rreport.open.filename.mask' = 'O%s')

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

closedNameMask <- function(filename, mask) {
  if(!missing(mask) && !is.null(mask)) {
    options(rreport.closed.filename.mask = mask)
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

FilenameMask <- function(filename, open.report) {
  if(open.report) {
    openNameMask(filename=filename)
  } else {
    closedNameMask(filename=filename)
  }
}

openGraphDir <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.open.graphics.dir = name)
  }
  
  options("rreport.open.graphics.dir")[[1]]
}

closedGraphDir <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.closed.graphics.dir = name)
  }
  
  options("rreport.closed.graphics.dir")[[1]]
}

GraphDirName <- function(open.report) {
  if(open.report) {
    openGraphDir()
  } else {
    closedGraphDir()
  }
}

openTexDir <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.open.generated.tex.dir = name)
  }
  
  options("rreport.open.generated.tex.dir")[[1]]
}

closedTexDir <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.closed.generated.tex.dir = name)
  }
  
  options("rreport.closed.generated.tex.dir")[[1]]
}

TexDirName <- function(open.report) {
  if(open.report) {
    openTexDir()
  } else {
    closedTexDir()
  }
}

appendixName <- function(name) {
  if(!missing(name) && !is.null(name)) {
    options(rreport.appendix.file.name = name)
  }
  
  options("rreport.appendix.file.name")[[1]]
}


rreportInit <- function(dir.closed.tex, dir.open.tex, dir.open.graph, dir.closed.graph,
                        open.mask, closed.mask, appendixName, empty.dir=FALSE) {
  closedDirName(dir.closed)
  openDirName(dir.open)
  closedGraphDir(dir.closed.graph)
  openGraphDir(dir.open.graph)
  openNameMask(mask=open.mask)
  closedNameMask(mask=closed.mask)
  appendixName(appendixName)


  ## Test directory existance
  if(is.na(file.stat(closedTexDir())$isdir) || !file.stat(closedTexDir())$isdir) {
    stop('Directory for closed report tex files', closedTexDir(), 'does not exist')
  }

  if(is.na(file.stat(openTexDir())$isdir) || !file.stat(openTexDir())$isdir) {
    stop('Directory for open report tex files', openTexDir(), 'does not exist')
  }

  if(is.na(file.stat(closedGraphDir())$isdir) || !file.stat(closedGraphDir())$isdir) {
    stop('Directory for closed graphics output', closedGraphDir(), 'does not exist')
  }

  if(is.na(file.stat(openGraphDir())$isdir) || !file.stat(openGraphDir())$isdir) {
    stop('Directory for open graphics output', openGraphDir(), 'does not exist')
  }

  if(empty.dir) {
    unlink(file.path(c(closedDirName(), openDirName()), '*'))
  }

  cat('', file=file.path(closedDirName(), closedNameMask(appendixName())))
  cat('', file=file.path(openDirName(), openNameMask(appendixName())))

  NULL
}
