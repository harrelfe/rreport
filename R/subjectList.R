#' Subject List
#'
#' summary
#'
#' details
#'
#' @param data NEEDDOC
#' @param panel NEEDDOC
#' @param caption NEEDDOC
#' @param vname NEEDDOC
#' @param colheads NEEDDOC
#' @param size NEEDDOC
#' @param longtable NEEDDOC
#' @param landscape NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

## $Id$
subjectList <- function(data, panel, caption=NULL,
                        vname=c('labels','names'),
                        colheads=NULL,
                        size='smaller',
                        longtable=TRUE, landscape=TRUE) {

  vname <- match.arg(vname)
  if(length(colheads)) lab <- colheads else {
    lab <- names(data)
    if(vname == 'labels') {
      lab <- sapply(data,label)
      lab <- ifelse(lab=='', names(data), lab)
    }
  }
  ## For chron date-time variables remove surrounding ( ) and seconds
  for(i in 1:length(data)) {
    x <- data[[i]]
    if(all(c('chron','dates','times') %in% class(x))) {
      x <- format(x)
      x <- substring(x, 2, nchar(x)-4)
      data[[i]] <- x
    }
  }
    
  w <- latex(data, file=paste('gentex/',panel,'.tex',sep=''),
             title=panel, colheads=lab,
             longtable=longtable, size=size, caption=caption,
             landscape=landscape, rowname=NULL)
  invisible()
}
