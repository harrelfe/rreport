#' Mock Table
#'
#' Generates a LaTeX report from the \code{control}.
#'
#' @param control list. Components used to generate report.
#' \sQuote{datafile} is the input file.
#' \sQuote{keyfile} is the key output file.
#' \sQuote{treatments} is the treatment value for each record.
#' \sQuote{byVar} is the byVar.
#' \sQuote{file} is the output file.
#' \sQuote{defaultOrientation} specifies page orientation.
#' \sQuote{pos} specifies caption location.
#' \sQuote{CLs} contains lower and upper confidence levels.
#' \sQuote{Pvalues} contains P-values.
#' @param append logical. If \sQuote{TRUE} output will be appended instead of
#' overwritten.
#' @export

mockTable <- function(control, append=TRUE) {
  w <- scan(control$datafile, what=list(''), sep='\n')[[1]]
  type <- substring(w,1,1)
  noncom <- type != '%'
  w      <- w[noncom]   # ignore comments
  type   <- type[noncom]

  if(type[1] != '*') stop('first record must start with *')

  cat('In the tables that follow,',
      '{\\smaller[2] \\#\\#} refers to frequency counts,',
      '{\\smaller[2] \\#\\#\\% $(n)$} refers to whole number',
      'percents and numerators ($n$) of percents,',
      '{\\smaller[2] a}~\\textbf{b}{\\smaller[2]~c} refers to',
      'three quartiles ($25^{th}$ percentile,',
      'median or $50^{th}$ percentile, $75^{th}$ percentile),',
      'and {\\smaller[2] a}~\\textbf{b}{\\smaller[2]~c}~$\\bar{x}\\pm s$',
      'refers to three quartiles plus the mean $\\pm$ the standard deviation.',
      file=control$keyfile)

  dotab <- function(type, w, caption, specials) {
    trt   <- control$treatments
    byvar <- control$byVar
    file  <- control$file

    tnote <- specials$tnote; Pvalues <- specials$Pvalues; CLs <- specials$CLs
    extraFields <- specials$extraFields
    ef <- if(lef <- length(extraFields)) lef - 1 else 0
    if(!length(extraFields)) extraFields <- ''
    
    if(!length(byvar)) byvar <- '' else
    caption <- paste(caption, byvar[1], sep=', ')
    n <- length(trt)
    ls <- control$defaultOrientation=='landscape'
    if(ls) cat('\\begin{landscape}\n', file=file, append=TRUE)
    cat('\\centering\n', file=file, append=TRUE)
    cat('\\ctable[caption={', caption, '}, pos=', control$pos,
        ']{',
        paste(rep('l',1+ef),collapse=''),
        paste(rep('c',n+length(Pvalues[[1]])),collapse=''),
        '}{',
        if(tnote != '') paste('\\tnote{',tnote,'}',sep=''),
      '}{\\FL\n', sep='', file=file, append=TRUE)
    cat(paste(c(extraFields,trt,Pvalues[[1]]), collapse='&'), '\\NN\n',
        paste(c(rep('',1+ef),rep('{\\smaller $N$={\\smaller[2] \\#\\#}}',n),
                if(length(Pvalues[[2]])) Pvalues[[2]]),
              collapse='&'), '\\ML\n',
        file=file, append=TRUE)
    for(j in 1:length(w)) {
      u <- w[j]
      k <- regexpr('!', u)
      if(k > 0) u <- paste(substring(u, 1, k-1),'\\hfill{\\smaller ',
                           substring(u, k+1),'}',sep='')
      if(substring(u,1,2)=='  ') u <- paste('~~~~',substring(u,3),sep='')
      if(substring(u,1,1)==' ')  u <- paste('~~',  substring(u,2),sep='')
      b <- switch(type[j],
                  m='',
                  p='{\\smaller[2] \\#\\#\\% $(n)$}',
                  a='{\\smaller[2] \\#\\#\\% $(n)\\: \\bar{x}\\: (N)$}',
                  c='{\\smaller[2] a} \\textbf{b} {\\smaller[2] c}',
                  C='{\\smaller[2] a} \\textbf{b} {\\smaller[2] c} $\\bar{x}\\pm s$')

      cat(paste(c(u,rep(b,n),
                  if(length(Pvalues))
                  rep('{\\smaller[2] \\#.\\#\\#\\#}',length(Pvalues[[1]]))),
                collapse='&'),
          if(j==length(w)) '\\LL}' else '\\NN',
          '\n',
          file=file, append=TRUE)
    }

    if(length(CLs)) {
      clinfo <- control$CLs
      if(!length(clinfo))
        stop(paste('CLs specified for table',caption,
                   'but CLs not defined in control'))
      cldesc   <- clinfo[[1]]
      whichCls <- clinfo[[2]]
      if(whichCls != 'all')
        stop('only "all" implemented for CLs')
      if(length(CLs)==1)
        cat(cldesc,'for pairwise differences in',CLs,'\n',
            file=file, append=append) else {
          cat(cldesc,'for pairwise differences in the following:\\\\\n',
              file=file, append=TRUE)
          cat('\\begin{tabular}{l}\\hline',
              paste(CLs,collapse='\\\\ '),
              '\\\\ \\hline\\end{tabular}\\\\ \n',
              file=file, append=TRUE)
        }
      cat('\\begin{tabular}{l',paste(rep('c',n-1),collapse=''),'}\\hline',
          sep='', file=file, append=append)
      cat(paste(c('',trt[-n]), collapse='&'), '\\\\ \\hline\n',
          file=file, append=TRUE)
      for(i in 2:n) {
        s <- rep('$\\Delta$ [{\\smaller \\#\\#, \\#\\#}]',n-1)
        s[(1:(n-1)) >= i] <- ''
        cat(paste(c(trt[i],s),collapse='&'),'\\\\',file=file,append=append)
      }
      cat('\\hline\\end{tabular}\n', file=file, append=append)
    }

    if(length(byvar) > 1)
      for(k in 2:length(byvar))
        cat('\\begin{table}\\caption{Same for',byvar[k],
            '}\\end{table}\n',
            file=file, append=TRUE)

    
    if(ls) cat('\\end{landscape}\n', file=file, append=TRUE)
  }

  captionSpecials <- function(caption, control) {
    pv <- regexpr(',Pvalues', caption)
    Pvalues <- NULL
    if(pv > 0) {
      if(!length(cpv <- control$Pvalues))
        stop(paste('Pvalues specified for table',caption,
                   'but control did not define Pvalues'))
      Pvalues <- get2rowHeads(cpv)
      caption <- gsub(',Pvalues', '', caption)
    }
    CLs <- NULL
    cl <- regexpr(',CLs{.*}', caption)
    if(cl > 0) {
      CLs <- strsplit(substring(caption, cl+5,
                                cl+attr(cl,'match.length')-2), ',')[[1]]
      caption <- gsub(',CLs{.*}', '', caption)
    }
    extraFields <- NULL
    ef <- regexpr(',extraFields{.*}', caption)
    if(ef > 0) {
      extraFields <- strsplit(substring(caption, ef+13,
                                        ef+attr(ef,'match.length')-2),',')[[1]]
      caption <- gsub(',extraFields{.*}', '', caption)
    }
    k <- regexpr('!', caption)
    if(k > 0) {
      tnote   <- substring(caption, k+1)
      caption <- substring(caption, 1, k-1)
    } else tnote <- ''
    list(caption=caption, Pvalues=Pvalues, CLs=CLs,
         extraFields=extraFields, tnote=tnote)
  }
  
  starred <- (1:length(w))[type=='*']
  start   <- starred+1
  end     <- c(starred[-1]-1, length(w))
  for(i in 1:length(starred)) {
    caption  <- substring(w[starred[i]],2)
    specials <- captionSpecials(caption, control)
    caption  <- specials$caption
    
    is <- start[i]; ie <- end[i]
    dotab(type[is:ie], substring(w[is:ie],3), caption, specials)
  }
}

#' Mock Listing
#'
#' Generate a LaTeX table from the \code{control}.
#'
#' @param control list. Components used to generate report.
#' \sQuote{file} is the output file.
#' \sQuote{baseListVar}
#' \sQuote{defaultOrientation}
#' \sQuote{pos}
#' @param caption character. Table caption.
#' @param fields NEEDDOC
#' @param orientation character. Specify page orientation.
#' @param size NEEDDOC
#' @export

mockListing <- function(control, caption, fields,
                        orientation='', size='') {
  f <- control$file
  b <- control$baseListVar
  n <- length(b) + length(fields)
  heads <- get2rowHeads(c(b, fields))
  ls <- orientation == 'landscape' || (orientation=='' &&
          control$defaultOrientation=='landscape')
  if(ls)  cat('\\begin{landscape}\n',
              file=f, append=TRUE)
  cat('\\centering\n', file=f, append=TRUE)
  if(size != '') cat('{', size, file=f, append=TRUE)
  cat('\\ctable[caption={',caption,'}, pos=',control$pos,']{',
      paste(rep('c',n),collapse=''),
      '}{}{\\FL\n',
      paste(heads[[1]], collapse='&'),
      if(any(heads[[2]] != '')) paste('\\NN\n',
                                      paste(heads[[2]], collapse='&'),
                                      sep=''),
      '\\ML\n',
      paste(rep('',n), collapse='&'),'\n\\LL}\n',
      sep='', file=f, append=TRUE)
  if(size != '') cat('}', file=f, append=TRUE)
  if(ls) cat('\\end{landscape}\n', file=f, append=TRUE)
  invisible()
}
