#' Kaplan-Meier Estimates
#'
#' For two strata, estimates the standard error of the difference in two
#' Kaplan-Meier estimates at each value of times, and plots half-width
#' of confidence level for the difference, centered at the midpoint
#' of the survival estimates.
#'
#' details
#'
#' @param fit survfit object. See \code{\link[rms]{survfit.formula}}.
#' @param times numeric vector. Time value for each record.
#' @param fun function. Function to plot estimates.
#' @param offset numeric. Offset value to apply to \sQuote{x} coordinate points.
#' @param lwd numeric. The line width, passed to \code{lines}.
#' @param lty numeric. The line type, passed to \code{lines}.
#' @export
#' @examples
#' set.seed(20)
#' time <- rep(365, 50)
#' event <- rbinom(50, 1, 1/3)
#' time[event == 1] <- sample(365, sum(event == 1), replace=TRUE)
#' trt <- sample(1:2, 50, replace=TRUE)
#' require('rms')
#' fit <- survfit.formula(Surv(time, event) ~ trt)
#' survplot.survfit(fit)
#' plotKmHalfCL(fit, time)

plotKmHalfCL <- function(fit, times, fun=function(x) x,
                         offset=0, lwd=0.5, lty=1) {
  s <- summary(fit, times=times)
  st <- s$strata
  lev <- levels(st)

  if(length(lev) != 2) {
    stop('only handles 2 strata')
  }

  s1 <- s$surv[st==lev[1]]
  s2 <- s$surv[st==lev[2]]
  se1 <- s$std.err[st==lev[1]]
  se2 <- s$std.err[st==lev[2]]
  se.diff <- sqrt(se1^2 + se2^2)
  clhalf <- 1.96*se.diff
  midpt <- (s1 + s2)/2
  for(i in 1:length(times)) {
    lines(offset+c(times[i],times[i]),
          fun(c(midpt[i]-clhalf[i]/2, midpt[i]+clhalf[i]/2)),
          lwd=lwd, lty=lty, col=gray(0.7))
  }
}

#' Set mfrow Parameter
#'
#' Compute and set a good \code{par("mfrow")} given the
#' number of figures to plot.
#'
#' \code{trellis} and \code{small} may not both be specified as \sQuote{TRUE}.
#'
#' @param n numeric. Total number of figures to place in layout.
#' @param trellis logical. Set to \sQuote{TRUE} when a \sQuote{trellis} plot
#' is requested.
#' @param small logical. Set to \sQuote{TRUE} if the plot area should be
#' smaller to accomodate many plots.
#' @return return numeric vector.
#' If \code{trellis = TRUE} the suggested \sQuote{mfrow} is returned.
#' Otherwise the original \sQuote{mfrow} is returned invisibly.
#' @export
#' @examples
#' oldmfrow <- mfrowSet(8)

mfrowSet <- function(n, trellis=FALSE, small=FALSE) {
  if(small && trellis) stop('may not specify small=T when trellis=T')
  
  omf <- mf <- if(trellis)NULL else par('mfrow')
  if(length(mf)==0) mf <- c(1,1)
  if(n > 1 & max(mf)==1) {
    if(small) {
      mf <- if(n <= 4) {
        c(2,2)
      } else if(n <= 6) {
        c(2,3)
      } else if(n <= 12) {
        c(3,4)
      } else if(n <= 16) {
        c(4,4)
      } else if(n <= 20) {
        c(4,5)
      } else if(n <= 24) {
        c(4,6)
      } else if(n <= 25) {
        c(5,5)
      } else if(n <= 30) {
        c(5,6)
      } else if(n <= 36) {
        c(6,6)
      } else if(n <= 42) {
        c(6,7)
      } else {
        c(6,8)
      }
    } else {
      mf <- if(n <= 4) {
        c(2,2)
      } else if(n <= 6) {
        c(2,3)
      } else if(n <= 9) {
        c(3,3)
      } else {
        c(4,3)
      }

      if(n > 12 & n <= 16) {
        mf <- c(4,4)
      }
    }
    if(!trellis) {
      par(mfrow=mf)
    }
  }
  if(trellis) {
    mf
  } else {
    invisible(omf)
  }
}

#' Put Figure
#'
#' Included a generated figure within LaTex document.
#'
#' @param panel character. Panel name.
#' @param name character. Name for figure.
#' @param caption character. Short caption for figure.
#' @param longcaption character. Long caption for figure.
#' @param append logical. If \sQuote{TRUE} output will be appended instead of overwritten.
#' @export

putFig <- function(panel, name, caption=NULL, longcaption=NULL, append=TRUE) {
  gtype <- getOption('rreport.gtype')
  if(gtype=='interactive') {
    return(invisible())
  }
  panel <- paste(translate(panel, '.', '-'), '.tex', sep='')
  name <- translate(name, '.', '-')
  file <- file.path(TexDirName(), panel)
  suffix <- paste('.', gtype, sep='')

  ## if(length(caption)) caption <- latexTranslate(caption)
  ## if(length(longcaption)) longcaption <- latexTranslate(longcaption)

  capt1 <- capt2 <- ""
  if(length(longcaption)) {
    capt1 <- sprintf("\\caption[%s]{%s}\n", caption, longcaption)
  } else if(length(caption)) {
    capt1 <- sprintf("\\caption{%s}\n", caption)
  }
  if(length(caption)) {
    capt2 <- sprintf("\\label{fig:%s}\n", name)
  }
  mytex <- sprintf("\\begin{figure}[hbp!]\\leavevmode\\centerline{\\includegraphics{%s%s}}\n%s%s\\end{figure}\n", name, suffix, capt1, capt2)
  cat(mytex, file=file, append=append)
  invisible()
}

#' Plot Initialization
#'
#' Toggle plotting.  Sets options by examining \code{.Options$rreport.gtype}.
#'
#' @param file character.  Character string specifying file prefix.
#' @param \dots Arguments to be passed to \code{\link[Hmisc]{setps}} or \code{\link[Hmisc]{setpdf}}.
#' @export
#' @seealso \code{\link[Hmisc]{ps.slide}}

startPlot <- function(file, ...) {
  gtype <- getOption('rreport.gtype')
  file <- translate(file,'.','-')
  if(gtype == 'pdf') {
    options(setpdfPrefix=file.path('pdf',''))
    setpdf(file, ..., type='char')
  } else if(gtype == 'ps') {
    options(setpsPrefix=file.path('ps',''))
    setps(file, ..., type='char')
  }
  invisible()
}

#' @rdname startPlot
#' @export

endPlot <- function() {
  gtype <- getOption('rreport.gtype')
  if(gtype != 'interactive') {
    dev.off()
  }
  invisible()
}

#' Combine Equal
#'
#' Given a contingency table of counts, combine factors with equal counts.
#'
#' Factor names will be pasted together to make new names.  A code and definition will be generated
#' if the new name should exceed \code{maxChar}.
#'
#' @param x numeric. Contingency table or matrix of names and counts, see \code{\link[base]{table}}.
#' @param maxChar numeric. Maximum length of character string.  Names exceeding this will be replaced with a letter-code.
#' @return a list with three elements
#' \item{x}{Named vector of code frequencies.  The name corresponds to the code.}
#'
#' \item{codes}{Character vector of alpha-code labels.}
#'
#' \item{defs}{Character vector of code definitions.}
#'
#' @export
#' @examples
#' combineEqual(table(rep(991:1010, times=rep(1:4, each=5))))
#' combineEqual(table(rep(991:1010, times=rep(1:4, each=5))), maxChar=10)

combineEqual <- function(x, maxChar=24) {
  xorig <- x
  if(is.matrix(x)) {
    x <- apply(x, 2, paste, collapse=',')
  }

  if(!any(duplicated(x))) {
    return(xorig)
  }

  z <- split(names(x), x)

  v <- if(is.matrix(xorig)) {
    names(z)
  } else {
    as.numeric(names(z))
  }

  nam <- codes <- defs <- character(0)
  j <- 0

  all.letters <- c(letters,LETTERS)
  for(i in 1:length(z)) {
    a <- z[[i]]
    ac <- paste(a, collapse=', ')
    if(nchar(ac) <= maxChar) {
      nam <- c(nam,ac)
    } else {
      j <- j + 1
      k <- paste('(',all.letters[j],')',sep='')
      nam <- c(nam, k)
      codes <- c(codes, k)
      defs  <- c(defs, ac)
    }
  }
  names(v) <- nam
  if(is.matrix(xorig)) {
    v <- matrix(as.numeric(unlist(strsplit(v,','))),ncol=length(v),
                dimnames=list(dimnames(xorig)[[1]], nam))
  }

  list(x=v, codes=codes, defs=defs)
}

#' Make Treatment Key
#'
#' Use treatment levels to generate treatment key in LaTeX.
#'
#' Typically will be called with \code{paramFile} missing as the filename
#' will be generated by package options.
#'
#' @param tlevels vector. unique treatment levels, expected to have length two
#' @param paramFile character. params filename
#' @param append logical.  If \sQuote{TRUE} output will be appended to \sQuote{file},
#' otherwise, contents will be overwritten
#' @export
#' @examples
#' makeTreatKey(c('A', 'B'), paramFile='') # prints to standard output

makeTreatKey <- function(tlevels, paramFile, append=FALSE) {
  if(length(tlevels) != 2) {
    stop('expected two levels of treatment')
  }
  if(missing(paramFile)) {
    paramFile <- paramTexFile()
  }
  cat('\\def\\treatkey{', tlevels[1], ':\\rule[.05in]{.25in}{.5pt}; ', tlevels[2], ':\\textcolor[gray]{0.7}{\\rule[.05in]{.25in}{1.25pt}}.}\n',
      sep='', file=paramFile, append=append)
  invisible()
}

#' Put Params
#'
#' Define parameters and provide LaTeX formatting.
#'
#' Parameters will be saved to the parameter LaTeX file.
#'
#' @param \dots list of name-value pairs. parameter names and their associated formats
#' @param paramFile character. params filename
#' @export
#' @examples
#' putparams(go=1, fish='blue', paramFile='') #prints to standard output

putparams <- function(..., paramFile) {
  if(missing(paramFile)) {
    paramFile <- paramTexFile()
  }
  x <- list(...)
  if(!length(x)) {
    cat('% $Id$\n', file=paramFile)
  } else {
    for(n in names(x)) {
      cat('\\def\\', n, '{', format(x[[n]]), '}\n', sep='', file=paramFile, append=TRUE)
    }
  }
  invisible()
}

#' PS-to-PDF Directory
#'
#' Used if want to create PS files and later convert all to PDF
#'
#' Utilize the \sQuote{epstopdf} command for conversion.
#'
#' @export

dirps2pdf <- function() {
  files <- dir('ps/', pattern='\\.ps$')
  nfiles <- sub('ps$', 'pdf', files)
  i <- 0
  for(file in files) {
    i <- i + 1
    psname <- file.path('ps', file)
    pdfname <- file.path('pdf', nfiles[i])
    input.date <- file.info(psname)$mtime
    output.date <- file.info(pdfname)$mtime

    if(is.na(output.date) || output.date < input.date) {
      cat('Converting file:',file,'\n')
      cmd <- sprintf("epstopdf %s -o %s", shQuote(psname), shQuote(pdfname))
      system(cmd)
    }
  }
  invisible()
}

#' Publish PDF
#'
#' summary
#'
#' details
#'
#' @param reports NEEDDOC
#' @param minutes NEEDDOC
#' @param title NEEDDOC
#' @param server NEEDDOC
#' @param path NEEDDOC
#' @param extension NEEDDOC
#' @param upload NEEDDOC
#' @param email NEEDDOC
#' @param uid NEEDDOC
#' @param passwd NEEDDOC
#' @param to NEEDDOC
#' @param cc NEEDDOC
#' @param bcc NEEDDOC
#' @param sig NEEDDOC
#' @param hardcopies NEEDDOC
#' @param verbose NEEDDOC
#' @param mailer NEEDDOC
#' @param extra NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

publishPdf <- function(reports, minutes=NULL, title, server, path, extension="pdf",
                       upload=TRUE, email=FALSE, uid=NULL, passwd=NULL,
                       to=NULL, cc=NULL, bcc=NULL, sig=NULL,
                       hardcopies=TRUE, verbose=TRUE,
                       mailer=c('mail','kmail'), extra=NULL) {

  ## E.g. publishPdf(c(report='Closed Meeting Report',
  ##                   Oreport='Open Meeting Report'),title='My Project',
  ##                 server='myserver.edu', path='/home/www/html/myproject')
  ## Be sure to put something like export REPLYTO=foo@place.edu in ~/.bashrc
  ## if using mailer='mail'
  
  mailer <- match.arg(mailer)
  nl <- ifelse(mailer=='kmail','\n','\\n')
  
  if(upload) {
    f <- tempfile()

    if(file.exists(f) && !file.info(f)$isdir) {
      file.remove(f)
    }

    dir.create(f, recursive=TRUE)
    if (extension=="") {sep=""} else {sep="."}
    rn <- paste(names(c(reports,minutes)), extension, sep=sep)
    paths <- file.path(f, c('index.html', basename(rn)))

    info <- file.info(rn)[,c('size','mtime')]
    cat('<html><body bgcolor=white>',
        paste('<h2>', title, '</h2>', sep=''),
        sep='\n', file=paths[1])
    i <- with(info, data.frame(Bytes=size, 'Date Created'=mtime,
                               Description=c(reports,minutes),
                               row.names=basename(row.names(info)),
                               check.names=FALSE))
    z <- html(i, file=paths[1], append=TRUE, link=basename(rn), linkCol='Name',
              linkType='href')

    file.copy(rn, paths[-1], overwrite=TRUE)
    
    system(paste('chmod u=rw,g=rw,o=', paste(shQuote(paths), collapse=' ')))
    system(paste('scp ', paste(shQuote(paths), collapse=' '), ' ', server, ':', path, sep=''))

    #file.remove(paths, f)
  }
  if(email) {
    url <- strsplit(path, '/')[[1]]
    url <- url[length(url)]
    url <- paste('http://', server, '/', url, sep='')
    cmd <- paste(
                 if(length(c(reports,minutes)) ==1) {
                   'The following document has'
                 } else {
                   'The following documents have'
                 },
                 ' been placed or updated on a secure web page:',nl,#nl,
                 paste(c(reports,minutes), collapse=nl), nl, nl,
                 'Point your browser to ', url, #nl,
                 ' and use the username ', uid,
                 ' and the password that will be in the next email. ',
                 'For accuracy, copy the password from the e-mail and',
                 ' paste it in the proper field in your browser.',nl,nl,
                 'Please confirm your ability to open the pdf files within 24 hours by replying to this message.',nl,nl,
                 if(hardcopies) {
                   'I will bring final hard copies to the meeting.'
                 },
                 if(length(extra)) {
                   paste(nl,nl, extra,sep='')
                 },
                 sep='')
    
    if(length(sig)) {
      sig <- paste(sig, collapse=nl)
      cmd <- paste(cmd, nl, '----------', nl, sig, sep='')
    }

    if(mailer=='kmail') {
      tf <- tempfile()
      cat(cmd, file=tf)
      to <- paste('"', paste(to, collapse=','), '"', sep='')
      if(length(cc)) {
        cc <- paste(' -c "', paste(cc, collapse=','),'"',sep='')
      }

      if(length(bcc)) {
        bcc <- paste(' -b "', paste(bcc, collapse=','),'"',sep='')
      }
    } else {
      to <- paste(to, collapse=' ')
      if(length(cc))  {
        cc  <- paste(paste(' -c', cc), collapse='')
      }

      if(length(bcc)) {
        bcc <- paste(paste(' -b', bcc),collapse='')
      }
    }
    cmd <- if(mailer=='kmail') {
      paste('kmail -s "', title, '"', cc,
            bcc, ' --msg ', tf, ' ', to, sep='')
    } else {
      paste('echo "', cmd, '" | mail -s "',
            title, ' Reports"', cc, bcc, ' ', to, sep='')
    }

    system(cmd)
    if(verbose) {
      cat('\n\nMail command sent:\n', cmd, '\n')
    }

    prn(passwd)

    if(length(passwd)) {
      cmd <- if(mailer=='kmail') {
        paste('kmail -s "Additional information"', cc, bcc,
              ' --body "', passwd, '" ', to, sep='')
      } else {
        paste('echo ', passwd, ' | mail -s "Additional information"',
              cc, bcc, ' ', to, sep='')
      }

      system(cmd)
      if(verbose) {
        cat('\n\nMail command sent:\n', cmd, '\n')
      }
    }
  }
  invisible()
}
