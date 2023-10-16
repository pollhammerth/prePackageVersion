# create points manually
# the data is stored analog to automatically created mean/max values per x interval
# can be used for pmt.model


pmt.createPoints <- function(x = NA, y = NA){
  
  # locate point coordinates if not provided
  if (is.na(x[1]) == T) {
    l <- locator()
    x <- l$x; y <- l$y
  }
  
  # store points
  data <- data.frame(x = x, y = y, c = 0, s = 0, id = 0)
  if (length(x) > 1) {
    for (i in 2:length(x)){
      data <- rbind(data,c(x[i],y[i],0,0,0))
    }
  }
  
  # output function call for repeat
  cat("b <- pmt.createPoints(x = c(",paste0(round(x),collapse = ','),"), y = c(",paste0(round(y,digits = 1),collapse = ','),"))\n")
  return(data)

}


