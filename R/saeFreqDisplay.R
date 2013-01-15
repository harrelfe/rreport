#' Dummy SAE Frequency Example
#'
#' summary
#'
#' details
#'
#' @return return something
#' @export
#' @examples
#' 1

## $Id: saeFreqDisplay.s 5 2007-07-26 15:44:24Z edensk $

###----Compact display of AE frequencies: function displayFreq()
###-----------------------------------------------

###----Example for using displayFreq()
dummySAEFreqExample <- function(){
  pts = c("Myocardial infarction", "Coronary artery disease", "Arrhythmia", "Cardiac arrest", "Myocardial ischaemia", "Cardio-respiratory arrest", "Cardiogenic shock", "Hypertrophic cardiomyopathy", "Pericarditis", "Sinus bradycardia", "Tachycardia", "Pneumonia", "Sepsis", "Abscess", "Cellulitis", "Post procedural sepsis", "Sinusitis", "Dyspnoea", "Pulmonary hypertension", "Respiratory distress", "Sleep apnoea syndrome", "Pleural effusion", "Joint dislocation", "Post procedural complication", "Road traffic accident", "Tibia fracture", "Gastritis", "Abdominal pain","Inguinal hernia", "Haemorrhoids","Ileus","Pancreatitis","Rectal polyp")
  socs = c(rep("Cardiac disorders", 11), rep("Infections and infestations", 6), rep("Respiratory, thoracic and mediastinal disorders", 5), rep("Injury, poisoning and procedural complications", 4), rep("Gastrointestinal disorders", 7))
  socdict = socs
  names(socdict) = pts
  numA = c(41, 23, 13, 5, 8, 4, 5, 3, 4, 2, 1,
           29, 11, 14, 14, 6, 7,
           6, 1, 4, 2, 3,
           12, 8, 5, 2,
           11, 7, 8, 3, 2, 6, 3)
  numB = c(35, 20, 27, 5, 9, 3, 7, 3, 2, 3, 0,
           26, 6, 5, 7, 4, 3,
           11, 14, 3, 10, 4,
           8, 7, 3, 3,
           15, 6, 13, 2, 2, 7, 5)
  dummysae = rbind(
             data.frame(pt = rep(pts, numA), txcode=rep("A", sum(numA))),
             data.frame(pt = rep(pts, numB), txcode=rep("B", sum(numB))))
  dummysae$subject = 1:(sum(numA)+sum(numB))
  dummysae$soc = socdict[as.character(dummysae$pt)]
  dummysae$occ = 1
  
  pValue1 = 0.05
  pValue2 = 1
  minSAENum = 0
  denomSub = c(2005, 2032)
  names(denomSub) = levels(dummysae$txcode)
  titles = paste(paste("    Treatment", names(denomSub), sep=" "), paste(" (N=", denomSub, sep=""), ")", sep="")
  names(titles) = levels(dummysae$txcode)
  
  startPlot("saeDummySAEFreqDispl1", h=5, w=7)
  
  displayFreq(dummysae, "subject", "pt", "soc", "occ", "txcode", denomSub=denomSub, fileName=NULL, pvalue=pValue1, keepPvalue=pValue2, minDisplayNum=minSAENum, titleOffsetKoef=4, titleOffsetX=1.5, sparseKoef=2, gridCex=0.5, labelCex=0.5, labelLen=35, majorGrid=c(20, 40, 60, 80, 100, 120), minorGrid=c(0, 10, 20, 30, 40), titles = titles)
  
  putFig("saeDummySAEFreqDispl1","saeDummySAEFreqDispl1",
        "Nonserious AE frequency display.",
        paste("Nonserious AE frequency display by body system and preferred term. The horizontal dimension of the body system rectangles is proportional to the number of subject who had an event in that body system. The bar charts give the number of subjects who had a particular event within that body system. If the difference in proportions of subjects who had events in two treatment groups is significant (P-value is less than ",pValue1,"), the corresponding rectangles/bar charts in both treatment groups are pink/red.", sep=""), append=FALSE)
  endPlot()
}

#' Display AE Frequencies
#'
#' summary
#'
#' details
#'
#' @param dataframe NEEDDOC
#' @param subjectVar NEEDDOC
#' @param minorVar NEEDDOC
#' @param majorVar NEEDDOC
#' @param occurrenceVar NEEDDOC
#' @param stratVar NEEDDOC
#' @param denomSub NEEDDOC
#' @param fileName NEEDDOC
#' @param labelLen NEEDDOC
#' @param pvalue NEEDDOC
#' @param keepPvalue NEEDDOC
#' @param minDisplayNum NEEDDOC
#' @param majorGrid NEEDDOC
#' @param minorGrid NEEDDOC
#' @param plotGrid NEEDDOC
#' @param gridDig NEEDDOC
#' @param titleOffsetKoef NEEDDOC
#' @param titleOffsetX NEEDDOC
#' @param minorToMajorKoef NEEDDOC
#' @param sparseKoef NEEDDOC
#' @param graphWidth NEEDDOC
#' @param graphHeight NEEDDOC
#' @param gridCex NEEDDOC
#' @param labelCex NEEDDOC
#' @param titles NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

displayFreq = function(dataframe, subjectVar, minorVar, majorVar, occurrenceVar, stratVar,
                       denomSub=NULL,
                       fileName=NULL, labelLen=10, pvalue=0.05, keepPvalue=0.5,
                       minDisplayNum=2, majorGrid=NULL, minorGrid=NULL, plotGrid=TRUE, gridDig = 0,
                       titleOffsetKoef=10, titleOffsetX=0, minorToMajorKoef=5, sparseKoef=1.5,
                       graphWidth=8, graphHeight=11, gridCex=0.5, labelCex=0.5, titles=NULL){
### dataframe: data in the dataframe format (adverse events)
### subjectVar: variable classified to major and minor category (subject id)
### minorVar: minor category variable (specific adverse event)
### majorVar: major category variable (body system adverse event belongs to)
### occurrenceVar: variable indicating the different occurrences of
###                a minor category for a given subjectVar (date or order in which event happened).
###                This variable is assumed to be unique for given subject and given minorVar.
### stratVar: stratification variable (treatment for example)
### denomSub: a vector the same length as levels(stratVar) and has the same names.
###           it contains the number of unique subjects in each stratVar level.
###           if not defined, calculated according from the "dataframe"
### pvalue: if for a given major and minor category the proportion test gives p-value less
###         than pvalue then this category will be highlighted 
### keepPvalue: only categories with p-value (according to the proportion test) less than keepPvalue
###             will be displayed
### minDisplayNum: only categories with total frequency more than minDisplayNum will be displayed
### plotGrid: plot grid or not
### majorGrid: grid of major category. if not supplied and plotGrid=TRUE plots a default one
### minorGrid: grid of minor category. if not supplied and plotGrid=TRUE plots a default one
### gridDig: by how many digits to round the grid digits
### titleOffsetKoef: how far (vertically) the title should stay from the graph
### titleOffsetX: how far (horizontally) the title should stay from the graph
### minorToMajorKoef: how much longer the bars for minor category than for major category
### sparseKoef: how farther away the graphs for different stratVar values are located on the diagram
### gridCex: relative size of the grid digits
### labelCex: relative size of the labels
### title: character vector of titles for each frequency display. It has to have names which are levels of treatment


  ###----------------------make sure that levels(stratVar) are different from names(dataframe)
  if (any(levels(dataframe[[stratVar]]) %in% names(dataframe))){
    stop("Make sure that levels(stratVar) are different from names(dataframe)")
  }
  ###----------------------calculate the number of subjects in each "strat" category
  if (is.null(denomSub)){
    denomSub = tapply(dataframe[[subjectVar]], dataframe[[stratVar]], function(x){length(unique(x))})
  }else{
    if (!setequal(names(denomSub), levels(dataframe[[stratVar]]))) stop("Names of 'denomSub' have to be the same as levels of dataframe[[stratVar]].")
  }
  ###----------------------make sure the titles are correct
  if (is.null(titles)){
    titles = paste("Treatment", levels(dataframe[[stratVar]]), sep=" ")
    names(titles) = levels(dataframe[[stratVar]])
  }else{
    if (!setequal(names(titles), levels(dataframe[[stratVar]]))) stop("Names of 'titles' have to be the same as levels of dataframe[[stratVar]].")
  }
  ###----------------------choosing colors for the diagram
  colAllMajor=gray(0.92)
  colAllMinor=gray(0.85)
  colBadMajor="#FFC8E8"
  colBadMinor="#FF88AB"
  
  ### ---------------------filling in NA major and minor category
  codeNA = "Not Available"
  for (v in c(majorVar, minorVar)){
    if (class(dataframe[[v]]) != "factor")
      dataframe[[v]] = factor(dataframe[[v]])
  }
  majorLevels = levels(dataframe[[majorVar]])
  minorLevels = levels(dataframe[[minorVar]])
  dataframe[[majorVar]] = as.character(dataframe[[majorVar]])
  dataframe[[minorVar]] = as.character(dataframe[[minorVar]])
  dataframe[[majorVar]][is.na(dataframe[[majorVar]])] = codeNA
  dataframe[[minorVar]][is.na(dataframe[[minorVar]])] = codeNA
  dataframe[[majorVar]]=factor(dataframe[[majorVar]], levels=c(majorLevels, codeNA))
  dataframe[[minorVar]]=factor(dataframe[[minorVar]], levels=c(minorLevels, codeNA))
  
  ### ---------------------prepare minor/major look-up table
  majorMinor = unique(subset(dataframe, select=c(majorVar, minorVar)))
  majorByMinor = majorMinor[[majorVar]]
  names(majorByMinor) = majorMinor[[minorVar]]
  
  ### ---------------------prepare some general data
  uniqueEv = unique(subset(dataframe, select=c(subjectVar, minorVar, majorVar, occurrenceVar, stratVar)))
  uniqueSub = unique(subset(dataframe, select=c(subjectVar, minorVar, majorVar, stratVar)))
  
  ###----------------------prepare major data
  majorSub = as.data.frame(tapply(uniqueSub[[subjectVar]], list(uniqueSub[[majorVar]],
                           uniqueSub[[stratVar]]), function(x){length(unique(x))}))
  ###----------------------changing names to make sure that new names are not in levels(stratVar)
  names(majorSub) = paste("strat", names(majorSub), sep="")
  stratNames = names(majorSub)
  majorSub$label = row.names(majorSub)
  row.names(majorSub) = NULL
  
  ###----------------------substitute NA occurrences with 0
  for (n in stratNames) {
    majorSub[is.na(majorSub[[n]]),n] = 0
  }
  majorSub$all = apply(majorSub[,stratNames], MARGIN=1, FUN=sum, na.rm=TRUE)
  majorSub$pvalue = propTestVect1(as.matrix(majorSub[,stratNames]), matrix(denomSub, nrow=length(majorSub$label), ncol=length(stratNames), byrow=TRUE))
  majorSub = subset(majorSub, subset=!is.na(all)&(all>0))
  majorSub$col = colAllMajor
  majorSub$col[majorSub$pvalue<pvalue] = colBadMajor
  
  ###----------------------prepare minor data
  minorSub = as.data.frame(tapply(uniqueSub[[minorVar]], list(uniqueSub[[minorVar]],
                           uniqueSub[[stratVar]]), length))
  ### changing names to make sure that new names are not in levels(stratVar)
  names(minorSub) = stratNames
  minorSub$label = row.names(minorSub)
  row.names(minorSub) = NULL
  ### substitute NA occurrences with 0
  for (n in stratNames) {
    minorSub[is.na(minorSub[[n]]),n] = 0
  }
  minorSub$major = majorByMinor[minorSub$label]
  minorSub$all = apply(minorSub[,stratNames], MARGIN=1, FUN=sum, na.rm=TRUE)
  minorSub$pvalue = propTestVect1(as.matrix(minorSub[,stratNames]), matrix(denomSub, nrow=length(minorSub$label), ncol=length(stratNames), byrow=TRUE))
  
  minorSub = subset(minorSub, subset=!is.na(all)&(all>0))
  minorSub$major = as.character(minorSub$major)
  minorSub = merge(minorSub, data.frame(major=majorSub$label, majorAll=majorSub$all), by="major", all.x=TRUE)
  
  minorSub$col = colAllMinor
  minorSub$col[minorSub$pvalue<pvalue] = colBadMinor
  
  ### get rid of non-informative categories
  ###---------------------------------------------------------
  minorSub = minorSub[minorSub$pvalue<keepPvalue & minorSub$all>minDisplayNum,]
  if (length(minorSub$all)==0) {stop("No data to display. Posible problems: empty 'dataframe', low 'keepPvalue', low minDisplayNum.\n ")}
  majorSub = subset(majorSub, majorSub$label %in% minorSub$major)  
  
  ### prepare the grid
  ###---------------------------------------------------------
  gridDens = 5
  if (is.null(majorGrid)){majorGrid=seq(min(majorSub[stratNames]),max(majorSub[stratNames]),
                                        (max(majorSub[stratNames])-min(majorSub[stratNames]))/gridDens)[2:(gridDens+1)]}
  if (is.null(minorGrid)){minorGrid=seq(min(minorSub[stratNames]),max(minorSub[stratNames]),
                                        (max(minorSub[stratNames])-min(minorSub[stratNames]))/gridDens)[2:(gridDens+1)]}
  #major = majorSub; minor = minorSub;stratLevels=levels(dataframe[[stratVar]]); width=0.5; breakWidth=0.5; sparseKoef=1.5; majorGrid=NULL; minorGrid=NULL; title=""
  plotEvents(majorSub, minorSub, levels(dataframe[[stratVar]]), width=0.5, breakWidth=0.5, sparseKoef=sparseKoef, minorToMajorKoef=minorToMajorKoef, majorGrid=round(majorGrid,gridDig), minorGrid=round(minorGrid,gridDig), title=titles, fileName=fileName, titleOffsetKoef=titleOffsetKoef, titleOffsetX=titleOffsetX, graphWidth=graphWidth, graphHeight=graphHeight, gridCex=gridCex, labelCex=labelCex, labelLen=labelLen)
}

#' Plot Events
#'
#' summary
#'
#' details
#'
#' @param major NEEDDOC
#' @param minor NEEDDOC
#' @param stratLevels NEEDDOC
#' @param width NEEDDOC
#' @param breakWidth NEEDDOC
#' @param sparseKoef NEEDDOC
#' @param minorToMajorKoef NEEDDOC
#' @param plotGrid NEEDDOC
#' @param majorGrid NEEDDOC
#' @param minorGrid NEEDDOC
#' @param title NEEDDOC
#' @param fileName NEEDDOC
#' @param titleOffsetKoef NEEDDOC
#' @param titleOffsetX NEEDDOC
#' @param graphWidth NEEDDOC
#' @param graphHeight NEEDDOC
#' @param gridCex NEEDDOC
#' @param labelCex NEEDDOC
#' @param labelLen NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

plotEvents = function(major, minor, stratLevels, width=0.5, breakWidth=0.5, sparseKoef=1.5, minorToMajorKoef=5, plotGrid=TRUE, majorGrid=NULL, minorGrid=NULL, title="", fileName=NULL, titleOffsetKoef=3, titleOffsetX=0, graphWidth=8, graphHeight=11, gridCex=0.5, labelCex=0.5, labelLen=10){
  labelMaker = function(str1, str2, cut1=15, cut2=15){
    dots="..."
    len1 = nchar(str1)
    len2 = nchar(str2)
    label1 = substr(str1, 1, cut1)
    label1 = ifelse(nchar(label1)<len1, paste(label1,dots, sep=""), label1)
    label2 = substr(str2, 1, cut2)
    label2 = ifelse(nchar(label2)<len2, paste(label2,dots, sep=""), label2)
    paste(label1, label2)
  }
  recPerSheet = 100
  stratN = length(stratLevels)
  strat="strat"
  varNames = paste(strat,stratLevels, sep="")
  
  ### define how scale the length (y-axes dimention) of the rectangles)
  ###------------------------------------------------------------------
  globalMajorMaxLen = max(major[,varNames])
  majorMaxLen = rep(NA, stratN)
  for (i in 1:length(varNames)){majorMaxLen[i]=max(major[[varNames[i]]])/globalMajorMaxLen}
  globalMinorMaxLen = max(minor[,varNames])
  minorMaxLenKoef = minorToMajorKoef*globalMinorMaxLen/globalMajorMaxLen
  minorMaxLen = rep(NA, stratN)
  for (i in 1:length(varNames)){minorMaxLen[i]=minorMaxLenKoef*max(minor[[varNames[i]]])/globalMinorMaxLen}
  partition = (c(majorMaxLen, 0)+c(0,minorMaxLen))*sparseKoef
  titleOffsetY=titleOffsetKoef*(width+breakWidth)
  
  ### define start coordinates and xlim, ylim coord.
  ### assumption: the very first graph starts at (0,0)
  ###--------------------------------------------------
  startXY = matrix(0, nrow=stratN, ncol=2)
  for (i in 2:stratN){ startXY[i,1] = sum(partition[2:i]) }
  
  ### ordering major and minor
  ### note: the line majorOrder = match(minor$major, major$label) is necessary
  ### if major$all has a duplicated value than it won't be sorted the same way in minor
  ###-------------------------------------------------------
  major = major[order(-major$all),]
  majorOrder = match(minor$major, major$label)
  minor = minor[order(majorOrder, -minor$all),]
  
  ### choosing x limits for the layout
  #xlim = c(startXY[1,1]-partition[1], startXY[stratN,1]+partition[length(partition)])
  xlim = c(startXY[1,1], startXY[stratN,1])+c(-1,1)*sum(partition[c(1,length(partition))])/2
  #xlim = c(startXY[1,1], startXY[stratN,1])+c(-1,1)*min(partition[c(1,length(partition))])
  
  titleXY = startXY
  titleXY[,2] = min(startXY[,2])+titleOffsetY
  titleXY[,1] = startXY[,1]+titleOffsetX
  
  ###-----------------------ploting
  if (!is.null(fileName)){
    pdf(fileName, width=graphWidth, height=graphHeight)
  }
  ### if the graph is too long it can be layed out on several pages
  ### the following code does that
  ###--------------------------------------------------
  for (p in 1:ceiling(length(minor$all)/recPerSheet)){
    rowRange = ((p-1)*recPerSheet+1) : (p*recPerSheet)
    if (p>1){
      printMinor = minor[rowRange,]
    }else{
      printMinor = subset(minor[rowRange,], !is.na(minor$all[rowRange]))
    }
    printMajor = subset(major, major$label %in% printMinor$major)
    newGraph=TRUE
    cutMajor=labelLen
    par(mar=c(0,0,0,0))

    width = rep(width, length(printMinor$all), length.out=length(printMinor$all))
    breakWidth = rep(breakWidth, length(printMinor$all), length.out=length(printMinor$all))
    majorWidth = tapply(printMinor$label, printMinor$major, function(x){length(unique(x))})*(width[1]+breakWidth[1])-breakWidth[1]
    majorWidth = majorWidth[printMajor$label]
    majorBreakWidth = breakWidth[1]
    ylim = min(startXY[,2])+c(titleOffsetY,-sum(width+breakWidth)-titleOffsetY/2)
  
    majorMaxLen = rep(NA, stratN)
    for (i in 1:length(varNames)){majorMaxLen[i]=max(printMajor[[varNames[i]]])/globalMajorMaxLen}
    minorMaxLen = rep(NA, stratN)
    for (i in 1:length(varNames)){minorMaxLen[i]=minorMaxLenKoef*max(printMinor[[varNames[i]]])/globalMinorMaxLen}
    
    for (i in 1:stratN){
      plotDataMaj = printMajor[[varNames[i]]]
      plotDataMin = printMinor[[varNames[i]]]
      plotVect(plotDataMaj, newGraph=newGraph, plotGrid = plotGrid, gridVal=majorGrid, startXYCoor = startXY[i,], alignment = 2, maxLen = majorMaxLen[i], minLen = majorMaxLen[i]*min(plotDataMaj)/max(plotDataMaj), width = majorWidth, breakLen = majorBreakWidth, xlim=xlim, ylim=ylim, title=title[stratLevels[i]], titleXY=titleXY[i,], col=printMajor$col, labels=labelMaker(printMajor$label, plotDataMaj, cutMajor), gridCex=gridCex, labelCex=labelCex)
      
      minorLabels = labelMaker(plotDataMin, printMinor$label, labelLen, labelLen)
      minorLabels[minorLabels=="NA NA"] = ""
      plotVect(plotDataMin, newGraph=FALSE, plotGrid = plotGrid, gridVal=minorGrid, startXYCoor = startXY[i,], alignment = 1, maxLen = minorMaxLen[i], minLen = minorMaxLen[i]*min(plotDataMin)/max(plotDataMin), width = width, breakLen = breakWidth, labels=minorLabels, col = printMinor$col, gridCex=gridCex, labelCex=labelCex)
      newGraph=FALSE
      cutMajor=7
    }
  }
  if (!is.null(fileName)){
    dev.off()
  }
}

#' Plot Rectangle
#'
#' summary
#'
#' details
#'
#' @param startXY NEEDDOC
#' @param xLen NEEDDOC
#' @param yLen NEEDDOC
#' @param corner NEEDDOC
#' @param \dots NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

easyRect = function(startXY, xLen, yLen, corner=TRUE, ...){
### plots rectangle with geven coordinates startXY=c(. , .)
### where startXY is its left upper corner if corner==TRUE
### or center otherwize
  if (corner){
    if(xLen==0){lines(x = startXY[1] + c(0, 0),
                      y = startXY[2] + c(0, -1)*yLen, ...)
    }else{
      if(yLen==0){
          lines(x = startXY[1] + c(0, 1)*xLen,
                  y = startXY[2] + c(0, 0), ...)
      }else{
        polygon(x = startXY[1] + c(0, 1, 1, 0)*xLen,
                y = startXY[2] + c(0, 0, -1, -1)*yLen, ...)
      }
    }
  }else{
    if(xLen==0){lines(x = startXY[1] + c(-1, -1),
                      y = startXY[2] + c(1, -1)*yLen, ...)
    }else{
      if(yLen==0){
          lines(x = startXY[1] + c(-1, 1)*xLen,
                  y = startXY[2] + c(1, 1), ...)
      }else{
        polygon(x = startXY[1] + c(-1, 1, 1, -1)*xLen/2,
                y = startXY[2] + c(1, 1, -1, -1)*yLen/2, ...)
      }
    }
  }
}

#' Plot Vector
#'
#' summary
#'
#' details
#'
#' @param vect NEEDDOC
#' @param xlim NEEDDOC
#' @param ylim NEEDDOC
#' @param newGraph NEEDDOC
#' @param plotGrid NEEDDOC
#' @param gridVal NEEDDOC
#' @param gridCex NEEDDOC
#' @param startXYCoor NEEDDOC
#' @param vertical NEEDDOC
#' @param alignment NEEDDOC
#' @param maxLen NEEDDOC
#' @param minLen NEEDDOC
#' @param width NEEDDOC
#' @param breakLen NEEDDOC
#' @param labels NEEDDOC
#' @param labelMaxLen NEEDDOC
#' @param labelCex NEEDDOC
#' @param col NEEDDOC
#' @param lwd NEEDDOC
#' @param title NEEDDOC
#' @param titleCex NEEDDOC
#' @param titleXY NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

plotVect <- function(vect,
                     xlim=NULL,
                     ylim=NULL,
                     newGraph=TRUE,
                     plotGrid = TRUE,
                     gridVal = NULL,
                     gridCex = 0.5,
                     startXYCoor = c(0,0),
                     vertical = TRUE,
                     alignment = c(1, 2), ### 1 - left or upper, 2 - right or lower, depends on vertical==TRUE or FALSE
                     maxLen = 1,
                     minLen = 0.1,
                     width = 0.1,  # is a positive vector
                     breakLen = 0.5,   #is a positive vector
                     labels = as.character(vect),
                     labelMaxLen = 15,
                     labelCex = 0.5,
                     col = gray(0.8),  # is a vector
                     lwd = NULL,
                     title = "",
                     titleCex = 1,
                     titleXY=startXYCoor
                     ){
### This function plots a vector (positive vector)
### non-zero elements are displayed as a rectangles
### aligned to either left or right.
### The length a rectangle is proportional to the value of the vector.

### !!! CHECK that vect and gridVal are non-negative and non NA
### !!! WARN if xlim and ylim are small
### !!! CHECK that width is non-negative and not NA
  if (length(vect)==0) {stop("'vect' is empty.\n")}
  if (length(vect)==1) {
    absMin = 0
  }else{
    absMin = min(vect, na.rm=TRUE)
  }
  lineWidth = 0.5
  vectMax = max(vect, na.rm=TRUE)
  nonZeroMin = min(vect[vect>0], na.rm=TRUE)
  vecLen = length(vect)
  width = rep(width, ceiling(vecLen/length(width)), length.out=vecLen)
  breakLen = rep(breakLen, ceiling(vecLen/length(breakLen)), length.out=vecLen)
  col = rep(col, ceiling(vecLen/length(col)), length.out=vecLen)
  triagMatr = matrix(0, nrow=vecLen, ncol=vecLen)
  for (i in 1:vecLen) {triagMatr[i,1:i] = 1}
  step = triagMatr %*% (width + breakLen)
  ###--------old way of defining scaling koef.
  #if (vectMax==nonZeroMin){
  #  scaleKoef = 0
  #}else{
  #  scaleKoef = (maxLen-minLen)/(vectMax-nonZeroMin)
  #}
  ###--------old way of defining scaling koef.
  scaleKoef = (maxLen-minLen)/(vectMax-absMin)
  scaleInter = maxLen - scaleKoef*vectMax
  
  recLen = vect*scaleKoef + scaleInter
  recLen[is.na(recLen) | recLen<0]=0
  wholeWidth = sum(width) + sum(breakLen[1:(vecLen-1)])
  
  if (is.null(gridVal) & plotGrid){
    gridSet = 0:4
    gridVal = nonZeroMin + gridSet*(vectMax-nonZeroMin)/(length(gridSet)-1)
  }
  gridLen = gridVal*scaleKoef + scaleInter 
  labelLocKoef=0.3
  if (vertical){
    recCornerX = startXYCoor[1] - (alignment-1)*recLen
    recCornerY = startXYCoor[2] - c(0, step[1:(length(step)-1)])
    len = recLen
    wid = width
    xRange = min(recCornerX, na.rm=TRUE)+c(0, max(recLen, na.rm=TRUE))
    #yRange = startXYCoor[2] + c(-wholeWidth-max(width), max(width))
    yRange = startXYCoor[2] + (wholeWidth*c(-1,0)+2*c(-1,1)*min(breakLen, na.rm=TRUE))
    
    x0Grid = x1Grid = startXYCoor[1] - (2*alignment-3)*gridLen
    #y0Grid = rep(yRange[2], 2)
    y0Grid = rep(yRange[2], 2)-breakLen[1]
    #y1Grid = rep(yRange[1], 2)
    y1Grid = rep(yRange[1], 2)+2*breakLen[1]
    gridLabelRot = 90
    gridLabelPos = 3 #to the right
    #labelsX = startXYCoor[1] - (2*alignment-3)*recLen
    labelsX = startXYCoor[1] - (2*alignment-3)*(max(recLen)+min(recLen))*labelLocKoef
    labelsY = (recCornerY - wid/2)[1:length(vect)]
    labelsRot = 0
    labelsPos = -2*alignment+6
  }else{
    recCornerX = startXYCoor[1] + c(0, step[1:(length(step)-1)])
    recCornerY = startXYCoor[2] + (alignment-1)*recLen
    len = width
    wid = recLen
    xRange = startXYCoor[1] + c(0-max(width), wholeWidth+max(width))
    yRange = c(min(recCornerY-recLen, na.rm=TRUE), max(recCornerY, na.rm=TRUE))
    x0Grid = rep(xRange[2], 2)
    x1Grid = rep(xRange[1], 2)
    y0Grid = y1Grid = startXYCoor[2] - (-2*alignment+3)*gridLen
    gridLabelRot = 0
    gridLabelPos = 4 #to the right
    labelsX = recCornerX + len*(-1*alignment+2)
    #labelsY = startXYCoor[2] - (-2*alignment+3)*recLen
    labelsY = startXYCoor[2] - (-2*alignment+3)*(max(recLen)+min(recLen))*labelLocKoef
    labelsRot = 90
    labelsPos = 2*alignment #to the left
  }

  gridCol = gray(0.8)
  ### Setting graph if necessary (newGraph=TRUE)
  if (newGraph){
    ### FIX THE BUG: xlim and ylim should depend on the alignment
    if (is.null(xlim)) xlim=xRange + c(-1,1)*(diff(xRange))*0.2
    if (is.null(ylim)) ylim=yRange + c(-1,1)*(diff(yRange))*0.2
    #plot(xlim, ylim, type="n", axes = TRUE, ann=FALSE)
    plot(xlim, ylim, type="n", axes = FALSE, ann=FALSE)
  }
  ### Plotting grid if necessary (plotGrid=TRUE)
  if (plotGrid){
    segments(x0=x0Grid, y0=y0Grid, x1=x1Grid, y1=y1Grid, col=gridCol, lwd=lineWidth)
    text(x=x0Grid, y=y0Grid, labels=as.character(round(gridVal,5)), srt=gridLabelRot, pos=gridLabelPos, cex=gridCex, family="Times")
    text(x=x1Grid, y=y1Grid, labels=as.character(round(gridVal,5)), srt=gridLabelRot, pos=1, cex=gridCex, family="Times")
  }
  for (i in 1:vecLen){
    easyRect(c(recCornerX[i], recCornerY[i]), len[i], wid[i], col=col[i], border=FALSE)
  }
  if (TRUE){  ### labels
    text(x=labelsX, y=labelsY, labels=labels, srt=labelsRot, pos=labelsPos, cex=labelCex, col="black",
         family="Times")
  }
  if (title!=""){
    text(x=titleXY[1], y=titleXY[2], labels=title, pos=2, cex=titleCex)
  }
}

#' Propensity Test Vector
#'
#' summary
#'
#' details
#'
#' @param vMatr NEEDDOC
#' @param vNMatr NEEDDOC
#' @return return something
#' @export
#' @examples
#' 1

propTestVect1 <- function(vMatr, vNMatr){
  if (any(dim(vMatr)!=dim(vNMatr))) stop("vMatr, vNMatr should have the same dimentions.")
  res = rep(NA, dim(vMatr)[1])
  for (i in 1:dim(vMatr)[1]){
    res[i] = prop.test(vMatr[i,], vNMatr[i,])$p.value
  }
  res
}



