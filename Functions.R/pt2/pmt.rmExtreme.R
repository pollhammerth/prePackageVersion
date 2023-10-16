# remove extreme values from binned data (medians ...), by clicking on one or
# more points in a plot. 
# The adjusted data is automatically stored as b and the function with located
# coordinates is prompted for function repeat.

pmt.rmExtreme <- function(data = b, l = NA){
  
  # define internal stuff
  '%!in%' <- Negate('%in%')

  # locate x coordinates if not provided
  if (is.na(l[1]) == T) {
  l <- locator()
  l <- l$x
  }

  # remove points closest to given x coordinates  
  w <- which.min(abs(data$x - l[1]))
  if (length(l) > 1) {
  for (i in 2:length(l)) {
    w <- c(w, which.min(abs(data$x - l[i])))
  }
  }
  data <- data[which(data$x %!in% data$x[w]),]

  # output function call for repeat
  cat("b <- b %>% pmt.rmExtreme(l = c(",paste0(round(l),collapse = ','),"))\n")
  return(data)

}




