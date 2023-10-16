# This is a function, that can be used to plot the cluster analysis data from Graf (1993), using Grafs Symbols.
# {requires: pmt.filter(), pmt.plotBin()}



plotLocalities <- function(addToExisting = T, pointcex = 1, thCol = NA, th = NA, disCol = NA, disElem = NA, 
                           regCol = NA, regElem = NA, hds_tds_all = "all", 
                           labelColumn = "kuerzel", pointsName = "localNames",...) {
  # filter individual clusters from grafs heavy mineral cluster analysis
  localities <- expression(pmt.filter(points[[pointsName]], 
                                      regCol, 
                                      regElem, 
                                      disregard.column = disCol, 
                                      disregard.elements = disElem, 
                                      thresh.column = thCol, 
                                      threshold = th, 
                                      x = "x", y = "lidar", z = "z", 
                                      drop.unused = FALSE))
  
  
  #### filter only hds or tds, if desired
  if (hds_tds_all == "hds") {
    localities2 <- expression(pmt.filter(eval(localities), 
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
    localities2 <- expression(pmt.filter(eval(localities), 
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
  } else {localities2 <- localities}
  ####
  
  
  # plot the localities
  pmt.plotBin(eval(localities2), add = addToExisting, x = "x", y = "y", 
              border = "black", fill = "yellow", ppch = 17, anno = T, column = labelColumn, pcex = pointcex,...)
}


