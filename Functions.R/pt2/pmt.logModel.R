# this is a preliminary function to create a log model

pmt.logModel <- function(data = b,...) {
  
  m <- lm(b$y ~ log(b$x))
  points(y = m$fitted.values, x = b$x, type = "l", lwd = 5, col = "yellow")
  points(y = m$fitted.values, x = b$x, type = "l", lwd = 3, col = "white")
  points(y = m$fitted.values, x = b$x, type = "l",...)
  
  cat("> The model has been plotted. \n> Coefficients: ")
  cat("", paste0(m$coefficients, collapse = ", "), "\n")
  cat("> r^2: ",round(summary(m)$r.squared,2))
  
  return(m)
  
}





