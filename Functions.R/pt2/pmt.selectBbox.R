# remove extreme values from binned data (medians ...), by clicking on one or
# more points in a plot. 
# The adjusted data is automatically stored as b and the function with located
# coordinates is prompted for function repeat.

pmt.selectBbox <- function(data = b, lx = NA, ly = NA){
  
  # locate x coordinates if not provided
  if (is.na(lx[1]) == T) {
  l <- locator()
  lx <- l$x
  ly <- l$y
  }
  
  # select data within bbox
    data <- data[which(data$x > min(lx[1:2])),]
    data <- data[which(data$x < max(lx[1:2])),]
    data <- data[which(data$y > min(ly[1:2])),] 
    data <- data[which(data$y < max(ly[1:2])),]

  # output function call for repeat
  cat("pmt.selectBbox(lx = c(",paste0(round(lx),collapse = ','),"), ly = c(",paste0(round(ly),collapse = ','),"))")
  return(data)

}




