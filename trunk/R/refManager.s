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
  print(data.frame(marker=refD$marker, keyword=refD$keyword, label=refD$label))
}

getReferenceObject <- function(){
  refD <- options("rreport.reference.list")[[1]]
  if (class(refD)!= "latexReference"){
    stop("the type of the argument 'refD' should be 'latexReference'. Use function initMarkerList() to create it\n")
  }
  refD
}

putReferenceObject <- function(refD){
  options(rreport.reference.list=refD)
}

updateMarkers <- function(newMarker, keyword="", label=""){
  ### puts a new marker into the dataframe of the existing ones
  ### checks if it is different from the existing ones
  ### returns updated latexReference
  refD = getReferenceObject()
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
  generate <- function(){paste("marker",abs(round(rnorm(1)*(10^8))), sep="")}
  existingMarkers <- getRefsByKey()
  newMarker <- generate()
  while (newMarker %in% existingMarkers){
    newMarker <- generate()
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
  if (!is.null(keyword)){
    refD$marker[refD$keyword==keyword]
  }else{
    refD$marker
  }
}

getLabelsByKey <- function(keyword=NULL){
  ### returns all labels with a given keyword 
  ### if keyword==NULL returns all labels
  refD = getReferenceObject()
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

