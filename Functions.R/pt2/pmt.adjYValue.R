# adjust the elevation of a binned point, by selecting a point (1st click) and
# selecting desired elevation (2nd click)
# The adjusted data is automatically stored as b and the function with located
# coordinates is prompted for function repeat.


pmt.adjYValue <- function(data = b, l = NA) {
  
  # locate x coordinates if not provided
  if (is.na(l[1]) == T) {
    l <- locator()
    l <- c(l$x[1], l$y[2])
  }
  
  # change elevation of point closest to given x coordinate  
  w <- which.min(abs(data$x - l[1]))
  data[which(data$x == data$x[w]),]$y <- l[2]
  
  # output function call for repeat
  cat("pmt.adjYValue(l = c(",paste0(round(l,digits = 1),collapse = ','),"))")
  return(data)
  
}

