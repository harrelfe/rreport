makeNA <- function(data, mins, maxs) {

  # Make sure length(mins) = length(maxs)
  if(length(mins) != length(maxs)) stop('Min and max not specified for every variable')

  # If length(mins) = length(maxs), make sure names(mins) = names(maxs)
  if(any(sort(names(mins)) != sort(names(maxs)))) {
    stop(paste('Variable names specified in', sQuote('mins'), 
      'and', sQuote('maxs'), 'do not match'))
  }

  # Make sure valid column names were specified in mins and maxs for the data frame data
  if(any(names(mins) %nin% names(data))) {
    stop(paste('Illegal variable names specified in',
      sQuote('mins'), 'and', sQuote('maxs')))
  }

  n <- names(mins)
  for(i in 1:length(n)) {
    x     <- data[[n[i]]]
    check <- paste('datta <', mins[i], '| datta >', maxs[i])
    checkfn <- eval(parse(text = paste('function(datta) {', check, '}')))
    bad <- checkfn(x)
    nbad <- sum(bad, na.rm = TRUE)
    if(nbad) {
      data[bad & !is.na(x), n[i]] <- NA
    }
  }
  invisible(data)
}
