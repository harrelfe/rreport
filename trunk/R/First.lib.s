## $Id$
.noGenenerics <- TRUE  # faster loading as new methods not used

.First.lib <- function(lib, pkg, verbose=TRUE) {
  if(verbose)
    cat("rreport library by Frank E Harrell Jr\n\n",
        "Type library(help='rreport') to see overall documentation.\n\n",
        sep='')
  require('Hmisc') || stop('Hmisc package not available')
  invisible()
}
