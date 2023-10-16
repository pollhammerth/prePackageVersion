# function profiler and two helper functions
# can be used to create topographic profiles along the profile line
# data = the projected profile data
# column = column that contains the distance (from the profile line) values
# radius = should be set to regard only the closest points to the profile
# color = color of the line to be created
# ... = Arguments passed to pmt.plot()


pmt.profiler <- function(data, column = "z", radius = analysis.resolution/2, color = "black", ...){
  pmt.setRadius(data,column,radius) %>% pmt.order() %>% pmt.plot(type = "l", main = "", col = color, ...)
}


# Helper functions
pmt.setRadius <- function(data, column = "z", threshold = c(0,50000)) {
  if (length(threshold) > 1) {
    data <- data[which(data[[column]] > min(threshold)),]
  }
  data <- data[which(data[[column]] < max(threshold)),]
  return(data)
}
pmt.order <- function(data, column = "x") {
  data[order(data[[column]]),]
}
