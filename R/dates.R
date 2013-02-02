## round.chron <- function(x, units=c("minutes", "hours", "days", "months", "years")) {
##   if(missing(units)) {
##     return(floor(unclass(x)))
##   }

##   units <- match.arg(units)

##   time.units <- c("minutes", "hours", "days")
##   date.units <- c("months", "years")
  
##   if(units %in% time.units && !inherits(x, what=c('dates', 'times'))) {
##     stop("when 'units' is 'minutes', or 'hours' 'x' must be of class 'times' or 'dates'")
##   }
  
##   if(units %in% date.units  && !inherits(x, what=c('dates'))) {
##     stop("when 'units' is 'days', 'months', or 'years' 'x' must be of class 'dates'")
##   }

##   attribs <- attributes(x)

##   switch(units,
##          minutes = x + times('0:0:30', format='h:m:s')
##          hours = x + times('0:30:0', format='h:m:s')
##          days = x + times('12:0:0', format='h:m:s')
##          months = x + dates(paste('0:0',nlevels(days)/2,sep=':'), format='y:m:d')
##          years = x + dates(paste('0:

##   time <- c(seconds(x), minutes(x), hours(x), as.integer(days(x)),
##             as.integer(months(x)), as.integer(as.character(years(x))))

##   max.time <- c(59,59,23,nlevels(days),nlevels(months
##   set <- switch(units,
##          minutes = if(time[1] >= 30) {
##            if(time[4,
##          hours = time[2]/30 >= 1,
##          days = if(time[3]/12 >= 1) { 
##            if(time[4] == nlevels(days)) {
##              if(time[5] == nlevels(months)) {
##                time[6] <- time[6] + 1
##                5
##              } else {
##                time[5] <- time[5] + 1
##                4
##              }
##            } else {
##              time[4] <- time[4] + 1
##              3
##            }
##          },
## }

#' Round Chronological Objects
#'
#' Given a vector of chron objects, return a vector with values rounded to the given unit.
#' \code{floor.chron} will round down and \code{ceiling.chron} will round up.
#'
#' details
#'
#' @rdname round.chron
#' @param x a chron vector.
#' @param units character.  Round to nearest unit, defined as minutes, hours, days, months, or years.
#' @export
#' @examples
#' \dontrun{
#'   floor.chron(chron(dates = dates(c("01/25/13", "02/03/13", "11/15/13")), times = times(c("06:30:15", "12:19:51", "17:11:13"))), units='hours')
#'   ceiling.chron(chron(dates = dates(c("01/25/13", "02/03/13", "11/15/13")), times = times(c("06:30:15", "12:19:51", "17:11:13"))), units='months')
#' }

floor.chron <- function(x, units=c("minutes", "hours", "days", "months", "years")) {
  if(missing(units)) {
    return(floor(unclass(x)))
  }
  if(length(x) > 1) {
    return(chron(sapply(x, floor.chron, units)))
  }

  units <- match.arg(units)
  dt.units <- c("minutes", "hours", "days", "months", "years")
  dt.i <- match(units, dt.units)
  if(!inherits(x, what=c('dates', 'times'))) {
    x <- as.chron(x)
  }
  attribs <- attributes(x)
  time <- c(seconds(x), minutes(x), hours(x), as.integer(days(x)), as.integer(months(x)), as.integer(as.character(years(x))))
  start.time <- c(0,0,0,1,1)
  time <- c(start.time[seq(dt.i)], time[seq(dt.i+1, 6)])
  args <- list(format=c(dates='d:m:y', times='s:m:h'), out.format=attribs$format, origin=attribs$origin)

  if(!inherits(x, what='dates')) {
    time[4:6] <- NA
  } else if(length(attribs$format) == 1) {
    time[1:3] <- NA
  }

  if(!all(is.na(time[4:6]))) {
    args$dates. <- paste(time[4:6], collapse=':')
  }
  if(!all(is.na(time[1:3]))) {
    args$times. <- paste(time[1:3], collapse=':')
  }

  do.call('chron', args)
}

#' @rdname round.chron
#' @export

ceiling.chron <- function(x, units=c("minutes", "hours", "days", "months", "years")) {
  if(missing(units)) {
    return(ceiling(unclass(x)))
  }
  if(length(x) > 1) {
    return(chron(sapply(x, ceiling.chron, units)))
  }

  units <- match.arg(units)
  dt.units <- c("minutes", "hours", "days", "months", "years")
  dt.i <- match(units, dt.units)
  if(!inherits(x, what=c('dates', 'times'))) {
    x <- as.chron(x)
  }
  attribs <- attributes(x)
  nDays <- monthDays(x)
  time <- c(seconds(x), minutes(x), hours(x), as.integer(days(x)), as.integer(months(x)), as.integer(as.character(years(x))))
  end.time <- c(59,59,23,nDays,12)
  time <- c(end.time[seq(dt.i)], time[seq(dt.i+1, 6)])
  args <- list(format=c(dates='d:m:y', times='s:m:h'), out.format=attribs$format, origin=attribs$origin)
  
  if(!inherits(x, what='dates')) {
    time[4:6] <- NA
  } else if(length(attribs$format) == 1) {
    time[1:3] <- NA
  }

  if(!all(is.na(time[4:6]))) {
    args$dates. <- paste(time[4:6], collapse=':')
  }

  if(!all(is.na(time[1:3]))) {
    args$times. <- paste(time[1:3], collapse=':')
  }

  do.call('chron', args)
}
