## $Id$
listTable <- function(fileName,
                       longtable=TRUE, landscape=FALSE,
                       caption = "", fontSize="small",
                       zebra=FALSE, dataframe, zebraPattern=NULL, by=NULL,
                       colNames = names(dataframe),
                       vars =names(dataframe), fixedColVars=c(), fixedColWdths=c(),
                       markVar="", markVarVal=""){
                       
  #internal constants and functions definitions
      
  fontSizes <- c("tiny","scriptsize","footnotesize","small",
                 "normalsize","large","Large","LARGE","huge","Huge")
  zebraPatterns <- c("plain", "group", "plaingroup")
  #NOTE: pattern "plaingroup" is recommended only for large groups (more than 4 objects in a group)

  #!!! not to remove: adjusted pallet white&gray
  pallet1 <- list(lightwhite=c(0,0,0,0), darkwhite=c(0,0,0,0.07),
                  lightgray =c(0,0,0,0.2), darkgray=c(0,0,0,0.27),
                  middlegray=c(0,0,0,0.18), red=c(0,0.9,0.3,0))
  #!!! not to remove: adjusted pallet: orange&blue1
  pallet2 <- list(lightwhite=c(0,0.05,0.15,0), darkwhite=c(0,0.1,0.3,0),
                  lightgray=c(0.3,0.15,0.075,0), darkgray=c(.4,0.2,0.1,0),
                  middlegray=c(0.3,0.15,0.075,0), red=c(0,0.7,1,0))
  #!!! not to remove: adjusted pallet: orange&blue2
  pallet3 <- list(lightwhite=c(0,0.04,0.12,0), darkwhite=c(0,0.08,0.24,0),
                  lightgray=c(0.22,0.11,0.055,0), darkgray=c(0.3,0.15,0.075,0),
                  middlegray=c(0.22,0.11,0.055,0), red=c(0,0.7,1,0))
  #!!! not to remove: adjusted pallet: purple&yellow
  pallet4 <- list(lightwhite=c(0.0025,0.05,0.2,0), darkwhite=c(0.05,0.1,0.4,0),
                  lightgray=c(.235,0.235,0.1125,0), darkgray=c(.3,0.3,0.15,0),
                  middlegray=c(.235,0.235,0.1125,0), red=c(0,0.9,0.3,0))
  pallet <- pallet1

  charDates <- function(chronDates, outFormat= c(dates = "year month day", times = "h:m:s")) {
    #Returns as.character(date) out of chron object
    chdates <- as.character(dates(chronDates, out.format=outFormat))
    as.character(chdates)
  }
  
  latexTextMode <- function(str){
    #internal constants and functions definitions
    
    latexTextModeSpec <- function(char){
      if (char == "\\"){
        retStr <- "$\\backslash$"
      }else{
        if (char == "^" | char == "~"){
          retStr <- paste("\\verb*+",char,"+",sep="")
        }else{
          retStr <- paste("\\",char,sep="")
        }
      }
      retStr 
    }
    latexTextModeMath <- function(char){
      retStr <- paste("$",char,"$",sep="")
      retStr
    }
    charToLatexTextChar <- function(char){
      if (is.na(latexCharText[char])){
        char
      }else{
        latexCharText[char]
      }
    }
    
    latexTextModeText <- function(char){
      if (char %in% latexSpecialChar){
        retStr <- latexTextModeSpec(char)
      }else{
        if (char %in% latexMathChar){
          retStr <- latexTextModeMath(char)
        }else{
          retStr <- NULL
        }
      }
      retStr
    }
    
    latexSpecialChar <- c("#","$","%","^","_","{","}","~","&","\\")
    latexMathChar <- c("<", ">", "|")
    latexSpecAndMath <- c(latexSpecialChar,latexMathChar)
    latexCharText <- sapply(X = latexSpecAndMath, FUN=latexTextModeText)
    
  #beginning of the function latexTextMode
    spl <- unlist(strsplit(str,""))
    paste(sapply(X = spl, FUN=charToLatexTextChar),collapse="")    
  }

  processBeginCommand <- function(beginCom=c(), outFile,
                                  caption = "", fontSize="small", colNames = c(),
                                  zebra=FALSE, dataframe, zebraPattern,
                                  fixedColVars=c(), fixedColWdths=c(),
                                  markVar="", markVarVal=""){
    
    #internal constants and functions definitions
        
    commandBegin <- function(command, outFile){
      if (command %in% fontSizes){
        cat("\n{\\",command,"\n",sep="", file=outFile)
      }else{
      cat("\n\\begin{",command,"}",sep="", file=outFile)
      }
    }
    commandEnd <- function(command, outFile){
      if (command %in% fontSizes){
        cat("}\n", file=outFile)
      }else{
      cat("\\end{",command,"}\n",sep="", file=outFile)
      }
    }
    latexCaption <- function(captionFill, longtable, outFile){
      cap <- paste("\n\\caption{",captionFill,"}",sep="")
      if (longtable){
        cat(cap,"\\\\\n", sep="", file=outFile)
      }else{
        cat(cap,"\n", sep="", file=outFile)
      }
    }
    processColsFormat <- function(data, fixedColVars=c(), fixedColWdths=c(), outFile){
      colFormat <- c()
      for (n in names(data)){
        if (n %in% fixedColVars){
          colFormat <- c(colFormat,paste("p{",fixedColWdths[fixedColVars==n],"pt}", sep=""))
        }else{
          colFormat <- c(colFormat,"l")
        }
      }
      cat(" {",paste(colFormat, collapse=""),"}", sep="", file=outFile)
    }
    hline <- function(outFile){
      cat("\\hline\n", file=outFile)
    }

    processColsHead <- function(colNames, longtable, outFile){
      headStyle <- "\\bfseries"
      colNum <- length(colNames)
      hline(outFile)
      hline(outFile)
      cat(headStyle, colNames[1], file=outFile)
      for (i in 2:length(colNames)){
        cat("&", headStyle, colNames[i], file=outFile)
      }
      cat("\\\\\n", file=outFile)
      hline(outFile)
      hline(outFile)
      if (longtable){
        cat("\\endhead\n", file=outFile)
        hline(outFile)
        cat("\\multicolumn{",colNum,"}{r}{\\itshape Continued on next page}\\\\\n",
            sep="", file=outFile)
        cat("\\endfoot\n", file=outFile)
        hline(outFile)
        cat("\\multicolumn{",colNum,"}{r}{\\itshape The end}\\\\\n",
            sep="", file=outFile)
        cat("\\endlastfoot\n", file=outFile)
      }
    }
    processRows <- function(data, zebra, zebraPattern, outFile, markVar, markVarVal){
      #internal constants and functions definitions
          
      processRow <- function(row, zebra, color, outFile, markVarIndex){
        if (zebra==TRUE){
          cat("\\rowcolor{",color,"}\n",sep="", file=outFile)
        }
        cat(row[[1]], file=outFile)
        for (i in c(2:length(row))){
          if (i==markVarIndex){
            markVarStr <- "\\color{red}\\bfseries\\itshape"
          }else{
            markVarStr <- ""
          }
          cat(" &", markVarStr, row[[i]], file=outFile)
        }
        cat("\\\\\n", file=outFile)
      }
      
    #beginning of the function processRows
      markVarIndex <- match(markVar, names(data))
      if (markVar!="" & is.na(markVarIndex)) stop("Error: Variable to mark is not in dataframe names\n")
      if (length(data[[1]]) != 0){
        for (i in c(1:length(data[[1]]))){
          if (!is.na(markVarIndex)){
            if (data[i,markVarIndex]==markVarVal){
              processRow(data[i,], zebra, zebraPattern[i], outFile, markVarIndex)
            }else{
              processRow(data[i,], zebra, zebraPattern[i], outFile, -1)
            }
          }else{
            processRow(data[i,], zebra, zebraPattern[i], outFile, -1)
          }
        }
      }
      hline(outFile)
    }

        
  #beginning of the function processBeginCommand
    if (length(beginCom[!is.na(beginCom)])>0){
      commandBegin(beginCom[1],outFile)
      if (beginCom[1] == "table"){
        latexCaption(caption, beginCom[1] == "longtable",outFile)
      }
      if (beginCom[1] == "tabular"){
        processColsFormat(data=dataframe, fixedColVars, fixedColWdths, outFile)
        processColsHead(colNames = colNames, beginCom[1] == "longtable", outFile)
        processRows(data=dataframe, zebra=zebra, zebraPattern=zebraPattern, outFile,
                    markVar, markVarVal)
      }
      if (beginCom[1] == "longtable"){
        processColsFormat(data=dataframe, fixedColVars, fixedColWdths, outFile)
        latexCaption(caption, beginCom[1] == "longtable",outFile)
        processColsHead(colNames = colNames, beginCom[1] == "longtable", outFile)
        processRows(data=dataframe, zebra=zebra, zebraPattern=zebraPattern, outFile,
                    markVar, markVarVal)
      }
      processBeginCommand(beginCom[2:(length(beginCom)+1)], outFile,
                          caption, fontSize, colNames,
                          zebra, dataframe, zebraPattern,
                          fixedColVars, fixedColWdths,
                          markVar, markVarVal)
      commandEnd(beginCom[1], outFile)
    }
  }

  makePattern <- function(zebra, zebraPattern, by){
    #internal constants and functions definitions
        
    plain <- function(col1, col2, len){
      pattern <- rep(c(col1,col2), len)
      pattern <- pattern[1:len]  
      pattern
    }
    group <- function(by){
      col <- 1
      pattern <- rep(col, length(by))
      current <- by[1]
      for (i in 2:length(by)){
        nextg <- by[i]
        if (current==nextg) {
          pattern[i] <- col          
        }else{
          col <- col*(-1)
          pattern[i] <- col          
        }
        current <- nextg
      }
      pattern
    }
    
  #beginning of the function makePattern
    if (zebra){
      if (!(zebraPattern %in% zebraPatterns)) stop("Error: Illigal Zebra Pattern\n")
      if (zebraPattern=="plain"){
        pattern <- plain("lightwhite","middlegray",length(by))
      }
      if (zebraPattern=="group"){
        pattern <- group(by)
        pattern <- ifelse(pattern>0,"middlegray","lightwhite")
      }
      if (zebraPattern=="plaingroup"){
        pattern <- group(by)
        light <- plain("darkwhite","lightwhite",length(by))
        dark  <- plain("lightgray","darkgray",length(by))
        pattern <- ifelse(pattern>0,dark,light)
      }
      pattern
    }else{
      NULL
    }
  }

  defineColors <- function(outFile){
    for (n in names(pallet)){
      cat("\\definecolor{",n,"}{cmyk}{",pallet[[n]][1],",",
                                        pallet[[n]][2],",",
                                        pallet[[n]][3],",",
                                        pallet[[n]][4],"}\n",sep="",file=outFile)
    }
  }
    
#beginning of the function listTable
  outFile <- file(paste("gentex/",fileName,sep=""), open="wt")
  on.exit(close(outFile))
  dataframe <- dataframe[,vars]
  for (n in names(dataframe)){
    dataframe[[n]] <- as.character(as.character(dataframe[[n]]))
    dataframe[[n]] <- sapply(X = dataframe[[n]], FUN=latexTextMode)
  }
  if (zebra && (zebraPattern=="group")){
    dataframe <- dataframe[order(dataframe[[by]]),]
  }
  pattern <- makePattern(zebra, zebraPattern, dataframe[[by]])
  beginCommands <- c()
  if (landscape) beginCommands <- c(beginCommands,"landscape")
  beginCommands <- c(beginCommands, fontSize)
  if (!landscape) beginCommands <- c(beginCommands,"center")
  if (longtable) beginCommands <- c(beginCommands,"longtable")
  else beginCommands <- c(beginCommands,c("table","tabular"))
  defineColors(outFile)
  processBeginCommand(beginCommands, outFile,
                      caption, fontSize, colNames,
                      zebra, dataframe, pattern,
                      fixedColVars, fixedColWdths,
                      markVar, markVarVal)
}#end of the function listTable
