# a function to draw polygons with
# the polygon dataframe is returned to copy paste it into the script, for recalling later
# the dataframe can then be used to plot a polygon with polygon()

pmt.drawLine <- function(plot = T) {
  # click line in existing plot
  loc <- locator()
  # create dataframe with line points
  bline <- data.frame(x = loc$x, y = loc$y)
  # plot the line if desired
  if (isTRUE(plot)) {
    lines(bline$x, bline$y)
  }
  # create a string, that can be used to paste in script for recalling the polygon data, and return it
  string <- paste0("l <- data.frame(x = c(",paste0(round(loc$x,digits = 0), collapse = ","),"), y = c(", paste0(round(loc$y,digits = 0), collapse = ","), "))")
  cat(string)
  # return the line object
  return(bline)
  
}  


# pmt.extendLine <- function(l) {
# if (length(l$x) == 2) {
#   
#   fun <- approxfun(l) # convert line to function
#   
#   a <- l$x[1] < l$x[2]
#   b <- l$y[1] < l$y[2]
#   if(identical(a,b)){ k <- fun(l$x[1]+1)-l$y[1] # calculate the functions slope
#   } else { k <- fun(l$x[1]+1)+l$y[1] } # calculate the functions slope
#   
#   int <- l$y[1]-k*l$x[1] # calculate the intercept
#   
#   xmax <- k*max(extent$xlim)+int # calculate the y value for the max available x value
#   
#   l <- data.frame(x = c(0,max(extent$xlim)), y = c(int,xmax)) # create a new line across the whole plot
#   
#   return(l)
#   
# } else {cat("> Line must be defined by 2 points! Polylines cannot be extended!")}
# }



pmt.extendLine <- function(l) {
  if (length(l$x) == 2) {
    
    fun <- approxfun(l) # convert line to function

    y0 <- fun( min(l$x) ) # get y for min x
    y1 <- fun( (min(l$x)+1) ) # get y for min x + 1
    
    k <- y1 - y0 # get slope
    
    c <- y0 - k * min(l$x) # get intercept
    
    xmaxY <- k * max(extent$xlim) + c # get y for max x axis extent
    
    newLine <- data.frame(x = c(0,max(extent$xlim)), y = c(c,xmaxY)) # define new line from x = 0 to xmax
    
    return(newLine) # output the line

  } else {cat("> Line must be defined by 2 points! Polylines cannot be extended!")}
}
