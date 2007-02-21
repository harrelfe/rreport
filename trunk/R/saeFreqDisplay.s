#library(Hmisc)

if (FALSE){

Load(sae)
Load(ae)
Load(Codes)

pValue1 = 0.05
pValue2 = 0.1
aeSocPtDf = unique(subset(ae, select=c("aesoc", "aept")))
aeSocPt = as.character(aeSocPtDf$aesoc)
names(aeSocPt) = aeSocPtDf$aept
nInA = length(subset(Codes, txcode=="A")$subject)
nInB = length(subset(Codes, txcode=="B")$subject)
major = "aesoc"
lookBy="aept"
id = "subject"
sep = "  "
#what = sae
what = subset(ae, aeser=="No")
labelLen = 30

colAllMinor=gray(0.85)
colAllMajor=gray(0.92)
colBadMinor="#FF88AB"
colBadMajor="#FFC8E8"


source("saeFreqDisplay.s")
Load(sae)
Load(Codes)
Load(ae)
source("saeFreqDisplay.s")
tmpsae = merge(subset(ae, subset=subject %in% Codes$subject, select=c("subject", "aesoc", "aept", "aeondt")),
               subset(Codes, select=c("subject", "txcode")),
               by = "subject",
               all.x=TRUE)
tmpsae = tmpsae
#tmpsae$txcode = factor(sample(c("A","B","C"), length(tmpsae$txcode), replace=TRUE))
displayFreq(tmpsae, "subject", "aept", "aesoc", "aeondt", "txcode", fileName="newTMP.pdf", keepPvalue=0.2, minDisplayNum=10)
#displayFreq(tmpsae, "subject", "aept", "aesoc", "date.of.onset", "txcode", fileName="newTMP.pdf", minDisplayNum=2)

dataframe=tmpsae; subjectVar="subject"; minorVar="aept"; majorVar="aesoc"
#occurrenceVar="date.of.onset"
occurrenceVar="aeondt"
stratVar="txcode";pvalue=0.05;keepPvalue=0.2;minDisplayNum=5

}

displayFreq = function(dataframe, subjectVar, minorVar, majorVar, occurrenceVar, stratVar,
                       fileName=NULL, minorPerPage, labelLen=10, pvalue=0.05, keepPvalue=0.5, minDisplayNum=2, ...){
### dataframe: data in the dataframe format (adverse events)
### subjectVar: variable classified to major and minor category (subject id)
### minorVar: minor category variable (specific adverse event)
### majorVar: major category variable (body system adverse event belongs to)
### occurrenceVar: variable indicating the different occurrences of
###                a minor category for a given subjectVar (date or order in which event happened).
###                This variable is assumed to be unique for given subject and given minorVar.
### stratVar: stratification variable (treatment for example)
### make sure that levels(stratVar) are different from names(dataframe)

  colAllMajor=gray(0.92)
  colAllMinor=gray(0.85)
  colBadMajor="#FFC8E8"
  colBadMinor="#FF88AB"
  
  if (any(levels(dataframe[[stratVar]]) %in% names(dataframe))){
    stop("Make sure that levels(stratVar) are different from names(dataframe)")
  }
  ### ---------------------filling in NA major and minor category
  codeNA = "Not Available"
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
  denomSub = tapply(uniqueSub[[majorVar]], uniqueSub[[stratVar]], length)
  
  ###----------------------prepare major data
  ###
  majorSub = as.data.frame(tapply(uniqueSub[[majorVar]], list(uniqueSub[[majorVar]],
                           uniqueSub[[stratVar]]), length))
  ### changing names to make sure that new names are not in levels(stratVar)
  names(majorSub) = paste("strat", names(majorSub), sep="")
  stratNames = names(majorSub)
  #majorSub$label = substr(row.names(majorSub), 1, labelLen)
  majorSub$label = row.names(majorSub)
  row.names(majorSub) = NULL
  ### substitute NA occurrences with 0
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
  minorSub = minorSub[minorSub$pvalue<keepPvalue | minorSub$all>minDisplayNum,]
  if (length(minorSub$all)==0) {stop("No data to display. Posible problems: empty 'dataframe', low 'keepPvalue', low minDisplayNum.\n ")}
  majorSub = subset(majorSub, majorSub$label %in% minorSub$major)  
  
  #major = majorSub; minor = minorSub;stratLevels=levels(dataframe[[stratVar]]); width=0.5; breakWidth=0.5; sparseKoef=1.5; gridMajor=NULL; gridMinor=NULL; title=""
  plotEvents(majorSub, minorSub, levels(dataframe[[stratVar]]), width=0.5, breakWidth=0.5, colMajor=NULL, colMinor=NULL, sparseKoef=1.5, gridMajor=NULL, gridMinor=NULL, title="Treatment", fileName=fileName)
}

plotEvents = function(major, minor, stratLevels, width=0.5, breakWidth=0.5, sparseKoef=1.5, gridMajor=NULL, gridMinor=NULL, title="", fileName=NULL, ...){
  labelMaker = function(str1, str2, cut1=15, cut2=15){
    dots="..."
    len1 = nchar(str1)
    len2 = nchar(str2)
    label1 = substr(str1, 1, cut1)
    label1 = ifelse(nchar(label1)<len1, paste(label1,"...", sep=""), label1)
    label2 = substr(str2, 1, cut2)
    label2 = ifelse(nchar(label2)<len2, paste(label2,"...", sep=""), label2)
    paste(label1, label2)
  }
  stratN = length(stratLevels)
  strat="strat"
  varNames = paste(strat,stratLevels, sep="")
  
  ### define how scale the length (y-axes dimention) of the rectangles)
  ###------------------------------------------------------------------
  majorMaxLenKoef = 10
  globalMajorMaxLen = max(major[,varNames])
  majorMaxLen = rep(NA, stratN)
  for (i in 1:length(varNames)){majorMaxLen[i]=majorMaxLenKoef*max(major[[varNames[i]]])/globalMajorMaxLen}
  globalMinorMaxLen = max(major[,varNames])
  minorMaxLenKoef = majorMaxLenKoef*globalMinorMaxLen/globalMajorMaxLen
  minorMaxLen = rep(NA, stratN)
  for (i in 1:length(varNames)){minorMaxLen[i]=minorMaxLenKoef*max(minor[[varNames[i]]])/globalMinorMaxLen}
  partition = (c(majorMaxLen, 0)+c(0,minorMaxLen))*sparseKoef
  titleOffset=3*(width+breakWidth)
  
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
  width = rep(width, length(minor$all), length.out=length(minor$all))
  breakWidth = rep(breakWidth, length(minor$all), length.out=length(minor$all))
  
  xlim = c(startXY[1,1]-partition[1], startXY[stratN,1]+partition[length(partition)])
  ylim = min(startXY[,2])+c(titleOffset,-sum(width+breakWidth)-titleOffset/2)
  
  titleXY = startXY
  titleXY[,2] = min(startXY[,2])+titleOffset
  
  majorWidth = tapply(minor$label, minor$major, function(x){length(unique(x))})*(width[1]+breakWidth[1])-breakWidth[1]
  majorWidth = majorWidth[major$label]
  majorBreakWidth = breakWidth[1]
  
  if (fileName!=""){ pdf(file=fileName) }
  
  newGraph=TRUE
  cutMajor=20
  if (!is.null(fileName){
    pdf(fileName, width=8, height=11)
  }
  for (i in 1:stratN){
    plotDataMaj = major[[varNames[i]]]
    plotDataMin = minor[[varNames[i]]]
  
    plotVect(plotDataMaj, newGraph=newGraph, plotGrid = TRUE, startXYCoor = startXY[i,], alinement = 2, maxLen = majorMaxLen[i], minLen = 0, width = majorWidth, breakLen = majorBreakWidth, xlim=xlim, ylim=ylim, title=paste(title,stratLevels[i]), titleXY=titleXY[i,], col=major$col, labels=labelMaker(major$label, plotDataMaj, cutMajor))
    
    plotVect(plotDataMin, newGraph=FALSE, plotGrid = TRUE, startXYCoor = startXY[i,], alinement = 1, maxLen = minorMaxLen[i], minLen = 0, width = width, breakLen = breakWidth, labels=labelMaker(plotDataMin, minor$label), col = minor$col)
    
    newGraph=FALSE
    cutMajor=7
  }
  if (!is.null(fileName){
    dev.off()
  }
}


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

plotVect <- function(vect,
                     xlim=NULL,
                     ylim=NULL,
                     newGraph=TRUE,
                     plotGrid = TRUE,
                     gridVal = NULL,
                     startXYCoor = c(0,0),
                     vertical = TRUE,
                     alinement = c(1, 2), ### 1 - left or upper, 2 - right or lower, depends on vertical==TRUE or FALSE
                     maxLen = 1,
                     minLen = 0.1,
                     width = 0.1,  # is a positive vector
                     breakLen = 0.5,   #is a positive vector
                     labels = as.character(vect),
                     labelMaxLen = 15,
                     labelCex = NULL,
                     col = gray(0.8),  # is a vector
                     lwd = NULL,
                     title = "",
                     titleCex = 1,
                     titleXY=startXYCoor
                     ){
### This function plots a vector (positive vector)
### non-zero elements are displayed as a rectangles
### alined to either left or right.
### The length a rectangle is proportional to the value of the vector.
### ... arguments for plot function

### !!! CHECK that vect and gridVal are non-negative and non NA
### !!! WARN if xlim and ylim are small
### !!! CHECK that width is non-negative and not NA
  if (length(vect)==0) {stop("'vect' is empty.\n")}
  if (length(vect)==1) {
    absMin = 0
  }else{
    absMin = min(vect, na.rm=TRUE)
  }
  gridCex = 0.5
  labelCex = 0.5
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
  #labels = substr(labels, 1, labelMaxLen)
  labelLocKoef=0.3
  if (vertical){
    recCornerX = startXYCoor[1] - (alinement-1)*recLen
    recCornerY = startXYCoor[2] - c(0, step[1:(length(step)-1)])
    len = recLen
    wid = width
    xRange = min(recCornerX, na.rm=TRUE)+c(0, max(recLen, na.rm=TRUE))
    #yRange = startXYCoor[2] + c(-wholeWidth-max(width), max(width))
    yRange = startXYCoor[2] + (wholeWidth*c(-1,0)+2*c(-1,1)*min(breakLen, na.rm=TRUE))
    
    x0Grid = x1Grid = startXYCoor[1] - (2*alinement-3)*gridLen
    #y0Grid = rep(yRange[2], 2)
    y0Grid = rep(yRange[2], 2)-breakLen[1]
    #y1Grid = rep(yRange[1], 2)
    y1Grid = rep(yRange[1], 2)+2*breakLen[1]
    gridLabelRot = 90
    gridLabelPos = 3 #to the right
    #labelsX = startXYCoor[1] - (2*alinement-3)*recLen
    labelsX = startXYCoor[1] - (2*alinement-3)*(max(recLen)+min(recLen))*labelLocKoef
    labelsY = (recCornerY - wid/2)[1:length(vect)]
    labelsRot = 0
    labelsPos = -2*alinement+6
  }else{
    recCornerX = startXYCoor[1] + c(0, step[1:(length(step)-1)])
    recCornerY = startXYCoor[2] + (alinement-1)*recLen
    len = width
    wid = recLen
    xRange = startXYCoor[1] + c(0-max(width), wholeWidth+max(width))
    yRange = c(min(recCornerY-recLen, na.rm=TRUE), max(recCornerY, na.rm=TRUE))
    x0Grid = rep(xRange[2], 2)
    x1Grid = rep(xRange[1], 2)
    y0Grid = y1Grid = startXYCoor[2] - (-2*alinement+3)*gridLen
    gridLabelRot = 0
    gridLabelPos = 4 #to the right
    labelsX = recCornerX + len*(-1*alinement+2)
    #labelsY = startXYCoor[2] - (-2*alinement+3)*recLen
    labelsY = startXYCoor[2] - (-2*alinement+3)*(max(recLen)+min(recLen))*labelLocKoef
    labelsRot = 90
    labelsPos = 2*alinement #to the left
  }

  gridCol = gray(0.8)
  ### Setting graph if necessary (newGraph=TRUE)
  if (newGraph){
    ### FIX THE BUG: xlim and ylim should depend on the alinement
    if (is.null(xlim)) xlim=xRange + c(-1,1)*(diff(xRange))*0.2
    if (is.null(ylim)) ylim=yRange + c(-1,1)*(diff(yRange))*0.2
    plot(xlim, ylim, type="n", axes = TRUE, ann=FALSE)
  }
  ### Plotting grid if necessary (plotGrid=TRUE)
  if (plotGrid){
    gridCol = gray(0.9)
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

propTestVect <- function(v1, v2, vN1, vN2){
  if (length(v1)!=length(v2)) stop("v1 and v2 should be the same length.")
  n1 = rep(vN1, length(v1), length.out=length(v1))
  n2 = rep(vN2, length(v1), length.out=length(v1))
  res = rep(NA, length(v1))
  for (i in 1:length(v1)){
    res[i] = prop.test(c(v1[i], v2[i]), c(n1[i], n2[i]))$p.value
  }
  res
}
propTestVect1 <- function(vMatr, vNMatr){
  if (any(dim(vMatr)!=dim(vNMatr))) stop("vMatr, vNMatr should have the same dimentions.")
  res = rep(NA, dim(vMatr)[1])
  for (i in 1:dim(vMatr)[1]){
    res[i] = prop.test(vMatr[i,], vNMatr[i,])$p.value
  }
  res
}



