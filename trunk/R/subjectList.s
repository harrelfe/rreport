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
  for(i in 1:length(data))
    if(inherits(data[[i]],'POSIXct'))
      data[[i]] <- format(data[[i]], format='%Y-%m-%d')
  w <- latex(data, title=panel, colheads=lab,
             longtable=longtable, size=size, caption=caption,
             landscape=landscape, rowname=NULL)
  invisible()
}
