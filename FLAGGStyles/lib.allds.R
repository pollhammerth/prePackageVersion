# helper to extract all terrace ids of one stratigraphic level from dsNames


lib.allds <- function (dsNames = hdsNames) {
  lib <- dsNames[[1]]
  for ( i in 2:length(dsNames)) {
    lib <- c(lib, dsNames[[i]])
  }
  return(lib)
}
