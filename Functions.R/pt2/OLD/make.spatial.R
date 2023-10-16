# converts all terrace objects to spatial objects
# and keeps x and y columns
# requires: td (vector of object names to be converted)
make.spatial <- function(td, y = "y") {
  # set coordinates for all created terrace objects
  for (i in 1:length(td)) {
    # make spatial
    eval(parse(text = paste0("coordinates(", td[i], ") <<- ~x+", y)))
    # recover x and y columns
    eval(parse(text = paste0(td[i], " <<- cbind(", td[i], ", coordinates(", td[i], "))")))
  }
}
