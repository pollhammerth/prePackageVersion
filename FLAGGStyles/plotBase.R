



plotBaseHDS <- function(addToExisting = T, pointcex = 1, thCol = NA, th = NA, disCol = NA, disElem = NA, regCol = NA, regElem = NA, pointsName = "baseHDS") {
  # filter individual clusters from grafs heavy mineral cluster analysis
  baseDS <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, pointsName) == TRUE)]]], 
                                    regCol, 
                                    regElem, 
                                    disregard.column = disCol, 
                                    disregard.elements = disElem, 
                                    thresh.column = thCol, 
                                    threshold = th, 
                                    x = "x", y = "alt", z = "z", 
                                    drop.unused = FALSE))
  # plot the clusters
  pmt.plotBin(eval(baseDS), add = addToExisting, ppch = 95, border = "#dd4243", fill = "#dd4243", anno = F, column = "Symbol",pcex = pointcex)
}


plotBaseTDS <- function(addToExisting = T, pointcex = 1, thCol = NA, th = NA, disCol = NA, disElem = NA, regCol = NA, regElem = NA, pointsName = "baseTDS") {
  # filter individual clusters from grafs heavy mineral cluster analysis
  baseDS <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, pointsName) == TRUE)]]], 
                                  regCol, 
                                  regElem, 
                                  disregard.column = disCol, 
                                  disregard.elements = disElem, 
                                  thresh.column = thCol, 
                                  threshold = th, 
                                  x = "x", y = "alt", z = "z", 
                                  drop.unused = FALSE))
  # plot the clusters
  pmt.plotBin(eval(baseDS), add = addToExisting, ppch = 95, border = "#907a58", fill = "#907a58", anno = F, column = "Symbol",pcex = pointcex)
}

