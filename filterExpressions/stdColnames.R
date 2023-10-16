# this function renames p3d colnames to pt1 colnames, so that std filter expressions can be used (note: y.source needs to be set first)
stdColnames <- function(data) {
  if ("location" %in% names(data) & "distance" %in% names(data) == FALSE) {
    names(data)[names(data) == "x"] <- "location"
    names(data)[names(data) == "z"] <- "distance"
    names(data)[names(data) == "y"] <- y.source
  } else {cat("colnames are already named location, distance and ",y.source)}
  return(data)
}