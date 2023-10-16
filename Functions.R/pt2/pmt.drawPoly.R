# a function to draw polygons with
# the polygon dataframe is returned to copy paste it into the script, for recalling later
# the dataframe can then be used to plot a polygon with polygon()

pmt.drawPoly <- function(plot = T) {
  # click polygon in existing plot
  loc <- locator()
  # create dataframe with first point coppied as last row to close the polygon
  bpoly <- data.frame(x = loc$x, y = loc$y)
  bpoly <- rbind(bpoly, bpoly[1,])
  # plot the polygon
  if (isTRUE(plot) == T) {
    polygon(bpoly$x, bpoly$y)
  }
  # create a string, that can be used to paste in script for recalling the polygon data, and return it
  string <- paste0("bpoly <- data.frame(x = c(",paste0(round(loc$x,digits = 0), collapse = ","),"), y = c(", paste0(round(loc$y,digits = 0), collapse = ","), "))")
  cat(string)
}  



