# quickly measuring slope of line between two points to be clicked in plot

pmt.measureSlope <- function(x = NA, y = NA, extrapol = F, addText = NA){
  
  # locate points
  if (is.na(x[1]) == T) {
    l <- locator()
  } else {
    l <- list(x = x, y = y)
  }
  
  # draw extrapolated line, if desired
  if (isTRUE(extrapol) == T) {
    coefficients <- line(l)[[2]]
    abline(a = coefficients[1], b = coefficients[2], lty = 3, lwd = 1)
  }
  
  # draw line
  points(l,type = "l",lty = 1, col ="yellow", lwd=2)
  points(l,type = "l",lty = 3, col ="black", lwd=2)
  
  # get slope of line
  s <- round(abs(1000 * (max(l$y) - min(l$y))/(max(l$x) - min(l$x))),1)
  
  # print slope to plot
  ty <- (max(l$y) - min(l$y))/2+min(l$y)
  tx <- (max(l$x) - min(l$x))/2+min(l$x)
  # t <- (max(l$y) - min(l$y))/(max(l$x) - min(l$x))
  # a <- max(l$y) - min(l$y)
  # b <- max(l$x) - min(l$x)
  # c <- sqrt(a**2+b**2)
  # g <- asin(a/c)/pi*180
  text(tx,ty,paste0(" slope [permil] = ",s," ",if(is.na(addText)==F){addText}), srt = 90, adj = 0, cex = 0.84)
  
  # recall 
  cat("pmt.measureSlope(x = c(",paste0(round(l$x), collapse = ", "),"), y = c(",paste0(round(l$y,1), collapse = ", "),"), extrapol = F, addText = NA)")
  
}

