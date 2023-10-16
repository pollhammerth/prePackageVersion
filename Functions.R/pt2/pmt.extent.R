# get plot extent, creates objects needed for empty.plot()
# specify the desired dataframe and the columns containing x, y and z values

#### OLD VERSION ####
# pmt.extent <- function(data, x = "x", y = "y", z = "z") {
# # set optimised plot extent regarding first y argument (e.g. lidar)
# x.limit <<- c(min(data[x]), max(data[x]))
# y.limit <<- c(min(data[y]), max(data[y]))
# z.limit <<- c(min(data[z]), max(data[z]))
# }
####


pmt.extent <- function(data, x = "x", y = "y", z = "z", xfact = NA, yfact = NA) {

# create dataframe with extent values the data fits in perfectly
x <- data[x]
x <- x[is.na(x) == FALSE]
xlim <- c(min(x),max(x))
y <- data[c(y)]
y <- y[is.na(y) == FALSE]
ylim <- c(min(y),max(y))
z <- data[z]
z <- z[is.na(z) == FALSE]
zlim <- c(min(z),max(z))

e <- data.frame(xlim,ylim,zlim)


# if yfact of xfact is given, extend the plot limits, to create some blank space for annotations
if (is.na(yfact) == TRUE) {} else if (yfact > 0) {
  strech <- e$ylim[2] - e$ylim[1]
  e$ylim[1] <- e$ylim[1] - (strech * yfact)
} else if (yfact < 0) {
  strech <- e$ylim[2] - e$ylim[1]
  e$ylim[1] <- e$ylim[2] + (strech * yfact)
}

if (is.na(xfact) == TRUE) {} else if (xfact > 0) {
  strech <- e$xlim[2] - e$xlim[1]
  e$xlim[1] <- e$xlim[1] - (strech * xfact)
} else if (xfact < 0) {
  strech <- e$xlim[2] - e$xlim[1]
  e$xlim[1] <- e$xlim[2] + (strech * xfact)
}

# function output
return(e)

}


