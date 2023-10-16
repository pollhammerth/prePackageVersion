# add points to binned values by clicking locations in plot
# The adjusted data is automatically stored as b and the function with located
# coordinates is prompted for function repeat.


pmt.addPoint <- function(data = b, x = NA, y = NA){
  
  # locate point coordinates if not provided
  if (is.na(x[1]) == T) {
    l <- locator()
    x <- l$x; y <- l$y
  }
  
  # add points
  data <- rbind(data,c(x[1],y[1],0,0,0))
  if (length(x) > 1) {
    for (i in 2:length(x)){
      data <- rbind(data,c(x[i],y[i],0,0,0))
    }
  }
  

  # output function call for repeat
  cat("pmt.addPoint(x = c(",paste0(round(x),collapse = ','),"), y = c(",paste0(round(y,digits = 2),collapse = ','),"))")
  return(data)

    
}




