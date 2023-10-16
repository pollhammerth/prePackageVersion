# functions for placing coordinates in a plot, using plot extent from par("usr")
# and moving by a factor
# 0 = center; -0.99 = leftmost or lowermost; 0.99 = rightmost or uppermost
xloc <- function(xfactor) {
  par("usr")[2] - ((par("usr")[2] - par("usr")[1])/2)*(-xfactor + 1)
}
yloc <- function(yfactor) {
  par("usr")[4] - ((par("usr")[4] - par("usr")[3])/2)*(-yfactor + 1)
}
