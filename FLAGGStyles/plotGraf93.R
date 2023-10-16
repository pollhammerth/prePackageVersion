# This is a function, that can be used to plot the cluster analysis data from Graf (1993), using Grafs Symbols.
# {requires: pmt.filter(), pmt.plotBin()}


plotGraf93 <- function(addToExisting = T, pointcex = 1, thCol = NA, th = NA, disCol = NA, disElem = NA, regCluster = c(1,2,3), regCol = NA, regElem = NA, vQ_gQ_lQ_lK_vK_Pl = c(T,T,T,T,T,T), hds_tds_all = "all", pointsName = "HeavyMinderals_Graf1993") {

  # Prefiltering e.g. Clusters, and/or localities
  graf93dataA <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, pointsName) == TRUE)]]], "Cluster", 
                                         regCluster, disregard.column = disCol, disregard.elements = disElem, 
                                         thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
                                         drop.unused = FALSE))
  if (is.na(regCol) == F) {
  graf93dataB <- expression(pmt.filter(eval(graf93dataA), regCol, 
                                       regElem, disregard.column = disCol, disregard.elements = disElem, 
                                       thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
                                       drop.unused = FALSE))
  } else {graf93dataB <- graf93dataA}
  
  
  
  
  # filter only hds or tds, if desired
  if (hds_tds_all == "hds") {
    graf93data <- expression(pmt.filter(eval(graf93dataB), 
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
    graf93data <- expression(pmt.filter(eval(graf93dataB), 
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
  } else {graf93data <- graf93dataB}
  
  
  
  

  if (isTRUE(vQ_gQ_lQ_lK_vK_Pl[1]) == T) { 
  # filter individual clusters from grafs heavy mineral cluster analysis
  vollesQuadrat <- expression(pmt.filter(eval(graf93data), "Symbole", 
                                          c("Volles Quadrat"), disregard.column = NA, disregard.elements = NA, 
                                          thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                          drop.unused = FALSE))
  # plot the clusters
  pmt.plotBin(eval(vollesQuadrat), add = addToExisting, ppch = 15, border = "black", fill = "black", anno = F, column = "Symbol",pcex = pointcex)
  }
  
  if (isTRUE(vQ_gQ_lQ_lK_vK_Pl[2]) == T) { 
  grauesQuadrat <- expression(pmt.filter(eval(graf93data), "Symbole", 
                                         c("Graues Quadrat"), disregard.column = NA, disregard.elements = NA, 
                                         thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                         drop.unused = FALSE))
  pmt.plotBin(eval(grauesQuadrat), add = T, ppch = 15, border = "black", fill = "grey80", anno = F, column = "Symbol",pcex = pointcex)
  }
  
  if (isTRUE(vQ_gQ_lQ_lK_vK_Pl[3]) == T) { 
  leeresQuadrat <- expression(pmt.filter(eval(graf93data), "Symbole", 
                                          c("Leeres Quadrat"), disregard.column = NA, disregard.elements = NA, 
                                          thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                          drop.unused = FALSE))
  pmt.plotBin(eval(leeresQuadrat), add = T, ppch = 15, border = "black", fill = "white", anno = F, column = "Symbol",pcex = pointcex)
  }
  
  if (isTRUE(vQ_gQ_lQ_lK_vK_Pl[4]) == T) { 
  leererKreis <- expression(pmt.filter(eval(graf93data), "Symbole", 
                                        c("Leerer Kreis"), disregard.column = NA, disregard.elements = NA, 
                                        thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                        drop.unused = FALSE))
  pmt.plotBin(eval(leererKreis), add = T, ppch = 16, border = "black", fill = "white", anno = F, column = "Symbol",pcex = pointcex)
  }
  
  if (isTRUE(vQ_gQ_lQ_lK_vK_Pl[5]) == T) { 
    vollerKreis <- expression(pmt.filter(eval(graf93data), "Symbole", 
                                         c("Voller Kreis"), disregard.column = NA, disregard.elements = NA, 
                                         thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                         drop.unused = FALSE))
    pmt.plotBin(eval(vollerKreis), add = T, ppch = 16, border = "black", fill = "black", anno = F, column = "Symbol",pcex = pointcex)
  }
  
  if (isTRUE(vQ_gQ_lQ_lK_vK_Pl[6]) == T) { 
  plus <- expression(pmt.filter(eval(graf93data), "Symbole", 
                                 c("Plus"), disregard.column = NA, disregard.elements = NA, 
                                 thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                 drop.unused = FALSE))
  pmt.plotBin(eval(plus), add = T, ppch = 3, border = "black", fill = "black", anno = F, column = "Symbol",pcex = pointcex)
}
}




# # BACKUP working version
# 
# plotGraf93 <- function(addToExisting = T, pointcex = 1, thCol = NA, th = NA, disCol = NA, disElem = NA) {
#   # filter individual clusters from grafs heavy mineral cluster analysis
#   vollesQuadrat <- expression(pmt.filter(points[["Graf1993HeavyMinerals"]], "Symbole", 
#                                          c("Volles Quadrat"), disregard.column = disCol, disregard.elements = disElem, 
#                                          thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
#                                          drop.unused = FALSE))
#   leeresQuadrat <- expression(pmt.filter(points[["Graf1993HeavyMinerals"]], "Symbole", 
#                                          c("Leeres Quadrat"), disregard.column = disCol, disregard.elements = disElem, 
#                                          thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
#                                          drop.unused = FALSE))
#   leererKreis <- expression(pmt.filter(points[["Graf1993HeavyMinerals"]], "Symbole", 
#                                        c("Leerer Kreis"), disregard.column = disCol, disregard.elements = disElem, 
#                                        thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
#                                        drop.unused = FALSE))
#   plus <- expression(pmt.filter(points[["Graf1993HeavyMinerals"]], "Symbole", 
#                                 c("Plus"), disregard.column = disCol, disregard.elements = disElem, 
#                                 thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
#                                 drop.unused = FALSE))
#   vollerKreis <- expression(pmt.filter(points[["Graf1993HeavyMinerals"]], "Symbole", 
#                                        c("Voller Kreis"), disregard.column = disCol, disregard.elements = disElem, 
#                                        thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
#                                        drop.unused = FALSE))
#   grauesQuadrat <- expression(pmt.filter(points[["Graf1993HeavyMinerals"]], "Symbole", 
#                                          c("Graues Quadrat"), disregard.column = disCol, disregard.elements = disElem, 
#                                          thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
#                                          drop.unused = FALSE))
#   # plot the clusters
#   pmt.plotBin(eval(vollesQuadrat), add = addToExisting, ppch = 15, border = "black", fill = "black", anno = F, column = "Symbol",pcex = pointcex)
#   pmt.plotBin(eval(leeresQuadrat), add = T, ppch = 15, border = "black", fill = "white", anno = F, column = "Symbol",pcex = pointcex)
#   pmt.plotBin(eval(leererKreis), add = T, ppch = 16, border = "black", fill = "white", anno = F, column = "Symbol",pcex = pointcex)
#   pmt.plotBin(eval(plus), add = T, ppch = 3, border = "black", fill = "black", anno = F, column = "Symbol",pcex = pointcex)
#   pmt.plotBin(eval(vollerKreis), add = T, ppch = 16, border = "black", fill = "black", anno = F, column = "Symbol",pcex = pointcex)
#   pmt.plotBin(eval(grauesQuadrat), add = T, ppch = 15, border = "black", fill = "grey80", anno = F, column = "Symbol",pcex = pointcex)
# }


