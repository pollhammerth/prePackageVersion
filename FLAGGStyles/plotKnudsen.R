




plotKnudsen <- function(addToExisting = T, pointcex = 1, thCol = NA, th = NA, disCol = NA, disElem = NA, regCol = NA, regElem = NA,hds_tds_all = "all", annoColumn = "Bayesi_age", pointsName = "DatedOutcrops_Knudsen2020", ...) {
  age <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, pointsName) == TRUE)]]], 
                               regard.column = regCol, #"Site", 
                               regard.elements = regElem, #c("Iberig","Tromsberg","Feusi","Siglistorf","Irchel Steig"), 
                               disregard.column = disCol, 
                               disregard.elements = disElem, 
                               thresh.column = thCol, threshold = th, 
                               x = "x", y = "alt", z = "z", drop.unused = FALSE))
  
  
  # filter only hds or tds, if desired
  if (hds_tds_all == "hds") {
    age2 <- expression(pmt.filter(eval(age), 
                                  regard.column = "terrace", 
                                  regard.elements = c(dsIds$hds[[1]], if(is.na(hds.add)==FALSE){hds.add}else{NULL}), 
                                  disregard.column = "terrace", 
                                  disregard.elements = hds.remove,
                                  thresh.column = NA, 
                                  threshold = NA, 
                                  x = "x", 
                                  y = "y", 
                                  z = "z", 
                                  drop.unused = FALSE))
  } else if (hds_tds_all == "tds") {
    age2 <- expression(pmt.filter(eval(age), 
                                  regard.column = "terrace", 
                                  regard.elements = c(dsIds$tds[[1]], if(is.na(tds.add)==FALSE){tds.add}else{NULL}), 
                                  disregard.column = "terrace", 
                                  disregard.elements = tds.remove,
                                  thresh.column = NA, 
                                  threshold = NA, 
                                  x = "x", 
                                  y = "y", 
                                  z = "z", 
                                  drop.unused = FALSE))
  } else {age2 <- age}
  
  
  # plot the data
  if (length(eval(age2)[[1]]) > 0 ) {
  pmt.plotBin(eval(age2), x = "x", y = "y", add = addToExisting, pcex = pointcex, anno = T, column = annoColumn,
              ppch = 18, border = "black", fill = "#ffb700", ...) # V1 fill color: "#8d8fff"
  } else {cat("no values within range\n")}
}
