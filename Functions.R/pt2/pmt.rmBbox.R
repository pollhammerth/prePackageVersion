# remove extreme values from binned data (medians ...), by clicking on one or
# more points in a plot. 
# The adjusted data is automatically stored as b and the function with located
# coordinates is prompted for function repeat.

pmt.rmBbox <- function(data = b, lx = NA, ly = NA){
  
  # internal definitions
  "%!in%" <- Negate("%in%")
  
  # locate x coordinates if not provided
  if (is.na(lx[1]) == T) {
  l <- locator()
  lx <- l$x
  ly <- l$y
  }
  
  # add unique row id field
  data$uniqueId <- seq.int(nrow(data))
  
  # select data within bbox
    rmdata <- data
    rmdata <- rmdata[which(rmdata$x > min(lx[1:2])),]
    rmdata <- rmdata[which(rmdata$x < max(lx[1:2])),]
    rmdata <- rmdata[which(rmdata$y > min(ly[1:2])),] 
    rmdata <- rmdata[which(rmdata$y < max(ly[1:2])),]
    
  # remove selected data from input data
  data <- data[which(data$uniqueId %!in% rmdata$uniqueId),]
  
  # remove unique row id field
  data$uniqueId <- NULL
    
  # output function call for repeat
  cat("pmt.rmBbox(lx = c(",paste0(round(lx),collapse = ','),"), ly = c(",paste0(round(ly),collapse = ','),"))")
  return(data)

}




