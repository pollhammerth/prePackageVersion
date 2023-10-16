



binBaseHDS <- function(thCol = NA, th = NA, disCol = NA, disElem = NA, regCol = NA, regElem = NA, pointsName = "baseHDS") {
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
  # return filtered base points
  return(eval(baseDS))
}


binBaseTDS <- function(thCol = NA, th = NA, disCol = NA, disElem = NA, regCol = NA, regElem = NA, pointsName = "baseTDS") {
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
  # return filtered base points
  return(eval(baseDS))
}

