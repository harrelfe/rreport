#' Plot K-M Half-Width Confidence Levels
#'
#' summary
#'
#' details
#'
#' @param fit NEEDDOC
#' @param times NEEDDOC
#' @param fun NEEDDOC
#' @param offset NEEDDOC
#' @param lwd NEEDDOC
#' @param lty NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

## $Id$
## For 2 strata estimates the std. error. of the difference in two K-M
## estimates at each value of times, and plots half-width of CL for
## difference, centered at midpoint of survival estimates
## fit is a survfit object
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
#' summary
#'
#' details
#'
#' @param n NEEDDOC
#' @param trellis NEEDDOC
#' @param small NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

mfrowSet <- function(n, trellis=FALSE, small=FALSE) {
  ## Usage: oldmfrow <- mfrowSet(total number of plots)
  ## Tries to compute and set a good par('mfrow')
  ## Original mfrow returned

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
#' summary
#'
#' details
#'
#' @param panel NEEDDOC
#' @param name NEEDDOC
#' @param caption NEEDDOC
#' @param longcaption NEEDDOC
#' @param append NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

putFig <- function(panel, name, caption=NULL, longcaption=NULL,
                   append=TRUE) {

  if(gtype=='interactive') {
    return(invisible())
  }
  
  file <- paste('gentex/',translate(panel,'.','-'),'.tex',sep='')
  name <- translate(name, '.', '-')
  suffix <- paste('.',gtype,sep='')

  ## if(length(caption)) caption <- latexTranslate(caption)
  ## if(length(longcaption)) longcaption <- latexTranslate(longcaption)

  cat('\\begin{figure}[hbp!]',
      '\\leavevmode\\centerline{\\includegraphics{',name,suffix,'}}\n',
      if(length(longcaption)) {
        paste('\\caption[',caption,']{',
              longcaption,'}\n',sep='')
      } else if(length(caption)) {
        paste('\\caption{',caption,'}\n',sep='')
      },

      if(length(caption)) {
        paste('\\label{fig:',name,'}\n',sep='')
      },
      '\\end{figure}\n',sep='', file=file, append=append)
  
  invisible()
}

#' Start Plot
#'
#' summary
#'
#' details
#'
#' @param file NEEDDOC
#' @param \dots NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

startPlot <- function(file, ...) {
  if(gtype=='interactive') {
    return(invisible())
  }

  file <- translate(file,'.','-')
  switch(gtype,
         ps={options(setpsPrefix='ps/')
             setps(file, ..., type='char')},
         pdf={options(setpdfPrefix='pdf/')
              setpdf(file,, ..., type='char')}
         )
  invisible()
}

#' End Plot
#'
#' summary
#'
#' details
#'
#' @export
#' @examples
#' 1

endPlot <- function() {
  if(gtype != 'interactive') {
    dev.off()
  }

  invisible()
}

#' Combine Equal
#'
#' summary
#'
#' details
#'
#' @param x NEEDDOC
#' @param maxChar NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

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

  for(i in 1:length(z)) {
    a <- z[[i]]
    ac <- paste(a, collapse=', ')
    if(nchar(ac) <= maxChar) {
      nam <- c(nam,ac)
    } else {
      j <- j + 1
      k <- paste('(',c(letters,LETTERS)[j],')',sep='')
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
#' summary
#'
#' details
#'
#' @param tlevels NEEDDOC
#' @param append NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

makeTreatKey <- function(tlevels, append=FALSE) {
  cat('\\def\\treatkey{',tlevels[1],
      ':\\rule[.05in]{.25in}{.5pt}; ',
      tlevels[2],':\\textcolor[gray]{0.7}{\\rule[.05in]{.25in}{1.25pt}}.}\n',
      sep='', file='gentex/params.tex', append=append)
  invisible()
}

#' Put Params
#'
#' Save various constants in LaTeX variable definitions in file params.tex
#'
#' details
#'
#' @param \dots Additional arguments
#' @return return something
#' @export
#' @examples
#' 1

putparams <- function(...) {
  x <- list(...)
  if(!length(x)) {
    cat('% $Id$\n',
        file='gentex/params.tex')
  } else {
    for(n in names(x)) {
      cat('\\def\\',n,'{',format(x[[n]]),'}\n',
          sep='', file='gentex/params.tex', append=TRUE)
    }
  }
  invisible()
}

#' PS-to-PDF Directory
#'
#' Used if want to create PS files and later convert all to PDF
#'
#' details
#'
#' @return return something
#' @export
#' @examples
#' 1

dirps2pdf <- function() {
  files <- dir('ps/', pattern='\\.ps')
  bases <- sub('\\.ps','',files)
  i <- 0
  for(file in files) {
    i <- i + 1
    base <- bases[i]
    input.date <- file.info(paste('ps',file,sep='/'))$mtime
    output.date <- file.info(paste('pdf/',base,'.pdf',sep=''))$mtime

    if(is.na(output.date) || output.date < input.date) {
      cat('Converting file:',file,'\n')
      system(paste('epstopdf ps/', shQuote(file),' -o ',
                   shQuote(paste('pdf/', bases[i],'.pdf',sep='')),
                   sep=''))
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
