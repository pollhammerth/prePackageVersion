# This is a function, that can be used to plot the cluster analysis data from Graf (1993), using Grafs Symbols.
# {requires: pmt.filter(), pmt.plotBin()}

# -> Clusters 1 and 2 (red, green) are within one large cluster, opposed to 3 (blue)
#red
#ff1601
#green
#01ff01
#blue
#011aff

plotGraf93Cluster <- function(addToExisting = T, pointcex = 1, thCol = NA, th = NA, disCol = NA, disElem = NA, regCol = NA, regElem = NA, 
                              plotCluster = c(T,T,T), clusterColor = c("#ff1601","#01ff01","#011aff"), hds_tds_all = "all") {
  #### CLUSTER 1 ####
  if (isTRUE(plotCluster[1]) == T) { 
    # filter individual clusters from grafs heavy mineral cluster analysis
    A <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, "Graf1993HeavyMinerals") == TRUE)]]], "Cluster", 
                                         c("1"), disregard.column = disCol, disregard.elements = disElem, 
                                         thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
                                         drop.unused = FALSE))
    A2 <- expression(pmt.filter(eval(A), regCol, 
                                          regElem, disregard.column = NA, disregard.elements = NA, 
                                          thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                          drop.unused = FALSE))
    
    #### filter only hds or tds, if desired
    if (hds_tds_all == "hds") {
      A3 <- expression(pmt.filter(eval(A2), 
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
      A3 <- expression(pmt.filter(eval(A2), 
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
    } else {A3 <- A2}
    ####
    
    # plot the clusters
    pmt.plotBin(eval(A3), add = addToExisting, ppch = 15, border = "black", fill = clusterColor[1], anno = F, column = "Symbol",pcex = pointcex)
  }
  
  #### CLUSTER 2 ####
  if (isTRUE(plotCluster[2]) == T) { 
    B <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, "Graf1993HeavyMinerals") == TRUE)]]], "Cluster", 
                                     c("2"), disregard.column = disCol, disregard.elements = disElem, 
                                     thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
                                     drop.unused = FALSE))
    B2 <- expression(pmt.filter(eval(B), regCol, 
                                      regElem, disregard.column = NA, disregard.elements = NA, 
                                      thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                      drop.unused = FALSE))
    
    #### filter only hds or tds, if desired
    if (hds_tds_all == "hds") {
      B3 <- expression(pmt.filter(eval(B2), 
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
      B3 <- expression(pmt.filter(eval(B2), 
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
    } else {B3 <- B2}
    ####
    
    # plot the clusters
    pmt.plotBin(eval(B3), add = T, ppch = 15, border = "black", fill = clusterColor[2], anno = F, column = "Symbol",pcex = pointcex)
  }
  
  #### CLUSTER 3 ####
  if (isTRUE(plotCluster[3]) == T) { 
    C <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, "Graf1993HeavyMinerals") == TRUE)]]], "Cluster", 
                               c("3","5"), disregard.column = disCol, disregard.elements = disElem, 
                               thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
                               drop.unused = FALSE))
    C2 <- expression(pmt.filter(eval(C), regCol, 
                                regElem, disregard.column = NA, disregard.elements = NA, 
                                thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                drop.unused = FALSE))
    
    #### filter only hds or tds, if desired
    if (hds_tds_all == "hds") {
      C3 <- expression(pmt.filter(eval(C2), 
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
      C3 <- expression(pmt.filter(eval(C2), 
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
    } else {C3 <- C2}
    ####
    
    # plot the clusters
    pmt.plotBin(eval(C3), add = T, ppch = 15, border = "black", fill = clusterColor[3], anno = F, column = "Symbol",pcex = pointcex)
  }
  
  
  
}
