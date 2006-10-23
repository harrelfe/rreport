initMarkerList <- function(){
  ### initiates a list of markers
  refD = data.frame(marker=c(), keyword=c(), label = c())
  refD$marker <- as.character(refD$marker)
  refD$keyword <- as.character(refD$keyword)
  refD$label <- as.character(refD$label)
  class(refD) <- "latexReference"
  options(rreport.reference.list = refD)
}

initMarkerList()



if (FALSE){
source("refManager.s")
#updateMarkers("newm3")
#updateMarkers("newm2")
#updateMarkers("newm1")

getReference("sae", "ser.adv withdr")
getReference("Osae", "ser.advWITHDR")
getReference("Osae", "ser.advCARDIO")
getReference("sae", "ser.adv cardio")

getRefsByKey("sae")
getLabelsByKey("sae")

getReferenceString("Osae")

getReferenceObject()
}

print.latexReference <- function(refD){
  cat("class", class(refD), "\n")
  for (i in 1:length(refD$marker)){
    cat(refD$marker[i], refD$keyword[i], refD$label[i], "++++++\n")
  }
  cat("\n")
}

getReferenceObject <- function(){
  options("rreport.reference.list")[[1]]
}

putReferenceObject <- function(refD){
  options(rreport.reference.list=refD)
}

updateMarkers <- function(newMarker, keyword="", label=""){
  ### puts a new marker into the dataframe of the existing ones
  ### checks if it is different from the existing ones
  ### returns updated latexReference
  refD = getReferenceObject()
  if (class(refD)!= "latexReference"){
    stop("the type of the argument 'refD' should be 'latexReference'. Use function initMarkerList() to create it\n")
  }
  if (newMarker %in% refD$marker){
    stop(paste("Duplicated marker", newMarker))
  }
  newM <-data.frame(marker=newMarker, keyword=keyword, label=label)
  newM$marker <- as.character(newM$marker)
  newM$keyword <- as.character(newM$keyword)
  newM$label <- as.character(newM$label)
  refD = rbind(refD, newM)
  class(refD) <- "latexReference"
  putReferenceObject(refD)
}

generateRef <- function(){
  existingMarkers <- getRefsByKey()
  koef <- 10^3
  newMarker <- abs(round(rnorm(1)*koef))
  while (newMarker %in% existingMarkers){
    newMarker <- abs(round(rnorm(1)*koef))
  }
  newMarker
}

getReference <- function(keyword="", label=""){
  newMarker <- generateRef()
  updateMarkers(newMarker = newMarker, keyword=keyword, label=label)
  newMarker
}

getRefsByKey <- function(keyword=NULL){
  ### returns all markers with a given keyword 
  ### if keyword==NULL returns all markers
  refD = getReferenceObject()
  if (class(refD)!= "latexReference"){
    stop("the type of the argument 'refD' should be latexReference. Use function initMarkerList() to create it\n")
  }
  if (!is.null(keyword)){
    refD$marker[refD$keyword==keyword]
  }else{
    refD$marker
  }
}

getLabelsByKey <- function(keyword=NULL){
  ### returns all markers with a given keyword 
  refD = getReferenceObject()
  if (class(refD)!= "latexReference"){
    stop("the type of the argument 'refD' should be latexReference. Use function initMarkerList() to create it\n")
  }
  if (!is.null(keyword)){
    refD$label[refD$keyword==keyword]
  }else{
    refD$label
  }
}

getReferenceString <- function(keyword){
  ### returns a vector of strings "see section \\ref{m1} (page\\pageref{m1})"
  ### for all markers with a given keyword 
  markers <- getRefsByKey(keyword)
  labels <- getLabelsByKey(keyword)
  keys <- paste(labels," in section ", "\\ref{", markers, "}", " (page ", "\\pageref{",markers,"}",")", sep="")
  paste("See", paste(keys, collapse=", "))
}

