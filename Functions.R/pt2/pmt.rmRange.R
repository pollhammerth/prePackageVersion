# remove extreme values from binned data (medians ...), by clicking on one or
# more points in a plot. 
# The adjusted data is automatically stored as b and the function with located
# coordinates is prompted for function repeat.

pmt.rmRange <- function(data = b, lx = NA){
  
  # locate x coordinates if not provided
  if (is.na(lx[1]) == T) {
  l <- locator()
  lx <- l$x
  }
  
  # remove data within x axis range(s)
  data <- rbind(
    data[which(data$x < min(lx[1:2])),], 
    data[which(data$x > max(lx[1:2])),]
    )
  if (length(lx) > 2) {
    for (i in seq(from = 3, to = length(lx), by = 2)) {
      data <- rbind(
        data[which(data$x < min(lx[i:(i+1)])),], 
        data[which(data$x > max(lx[i:(i+1)])),]
      )
    }
  }
  
  # output function call for repeat
  cat("pmt.rmRange(lx = c(",paste0(round(lx),collapse = ','),"))")
  return(data)

}




