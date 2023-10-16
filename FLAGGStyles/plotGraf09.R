# This is a function, that can be used to plot the cluster analysis data from Graf (1993), using Grafs Symbols.
# {requires: pmt.filter(), pmt.plotBin()}




plotGraf09Pepples <- function(addToExisting = T, pointcex = 1, thCol = NA, th = NA, disCol = NA, disElem = NA, regCol = NA, regElem = NA,plotCluster = c(T,T), hds_tds_all = "all", pointsName = "Pepples_Graf2009") {
  if (isTRUE(plotCluster[1]) == T) { 
    # filter individual clusters from grafs heavy mineral cluster analysis
    Rheintal <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, pointsName) == TRUE)]]], "pbFazies", 
                                      c("Rheintal Schuettung"), disregard.column = disCol, disregard.elements = disElem, 
                                      thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
                                      drop.unused = FALSE))
    Rheintal2 <- expression(pmt.filter(eval(Rheintal), regCol, 
                                       regElem, disregard.column = NA, disregard.elements = NA, 
                                       thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                       drop.unused = FALSE))
    
    #### filter only hds or tds, if desired
    if (hds_tds_all == "hds") {
      Rheintal3 <- expression(pmt.filter(eval(Rheintal2), 
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
      Rheintal3 <- expression(pmt.filter(eval(Rheintal2), 
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
    } else {Rheintal3 <- Rheintal2}
    ####
  
    # plot the clusters
    if ( length(eval(Rheintal3)[[1]]) > 0 ) {
    pmt.plotBin(eval(Rheintal3), add = addToExisting, ppch = 15, border = "black", fill = "#ffff70", anno = F, column = "Symbol",pcex = pointcex)
    } else {cat("> no Rheintal values within range\n")}
  }
  if (isTRUE(plotCluster[2]) == T) { 
    Thurtal <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, pointsName) == TRUE)]]], "pbFazies", 
                                     c("Thurtal Shuettung"), disregard.column = disCol, disregard.elements = disElem, 
                                     thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
                                     drop.unused = FALSE))
    Thurtal2 <- expression(pmt.filter(eval(Thurtal), regCol, 
                                      regElem, disregard.column = NA, disregard.elements = NA, 
                                      thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                      drop.unused = FALSE))
    
    #### filter only hds or tds, if desired
    if (hds_tds_all == "hds") {
      Thurtal3 <- expression(pmt.filter(eval(Thurtal2), 
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
      Thurtal3 <- expression(pmt.filter(eval(Thurtal2), 
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
    } else {Thurtal3 <- Thurtal2}
    ####

    # plot the clusters
    if ( length(eval(Thurtal3)[[1]]) > 0 ) {
      pmt.plotBin(eval(Thurtal3), add = T, ppch = 15, border = "black", fill = "#6ea6ff", anno = F, column = "Symbol",pcex = pointcex)
    } else {cat("> no Thurtal values within range\n")}
  }
}





plotGraf09HeavyMinerals <- function(addToExisting = T, pointcex = 1, thCol = NA, th = NA, disCol = NA, disElem = NA, regCol = NA, regElem = NA,plotCluster = c(T,T), hds_tds_all = "all", pointsName = "HeavyMinerals_Graf2009") {
  if (isTRUE(plotCluster[1]) == T) { 
    # filter individual clusters from grafs heavy mineral cluster analysis
    Glimmersand <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, pointsName) == TRUE)]]], "hmFazies", 
                                         c("Glimmersand Fazies"), disregard.column = disCol, disregard.elements = disElem, 
                                         thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
                                         drop.unused = FALSE))
    Glimmersand2 <- expression(pmt.filter(eval(Glimmersand), regCol, 
                                          regElem, disregard.column = NA, disregard.elements = NA, 
                                          thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                          drop.unused = FALSE))
    
    #### filter only hds or tds, if desired
    if (hds_tds_all == "hds") {
      Glimmersand3 <- expression(pmt.filter(eval(Glimmersand2), 
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
      Glimmersand3 <- expression(pmt.filter(eval(Glimmersand2), 
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
    } else {Glimmersand3 <- Glimmersand2}
    ####
    
    # plot the clusters
    if ( length(eval(Glimmersand3)[[1]]) > 0 ) {
      pmt.plotBin(eval(Glimmersand3), add = addToExisting, ppch = 15, border = "black", fill = "#ffb066", anno = F, column = "Symbol",pcex = pointcex)
    } else {cat("> no 'Glimmersand' values within range\n")}
  }
  
  if (isTRUE(plotCluster[2]) == T) { 
    Hoernli <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, pointsName) == TRUE)]]], "hmFazies", 
                                     c("Hoernli Fazies"), disregard.column = disCol, disregard.elements = disElem, 
                                     thresh.column = thCol, threshold = th, x = "x", y = "alt", z = "z", 
                                     drop.unused = FALSE))
    Hoernli2 <- expression(pmt.filter(eval(Hoernli), regCol, 
                                      regElem, disregard.column = NA, disregard.elements = NA, 
                                      thresh.column = NA, threshold = NA, x = "x", y = "y", z = "z", 
                                      drop.unused = FALSE))
    
    #### filter only hds or tds, if desired
    if (hds_tds_all == "hds") {
      Hoernli3 <- expression(pmt.filter(eval(Hoernli2), 
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
      Hoernli3 <- expression(pmt.filter(eval(Hoernli2), 
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
    } else {Hoernli3 <- Hoernli2}
    ####
    
    # plot the clusters
    if ( length(eval(Hoernli3)[[1]]) > 0 ) {
      pmt.plotBin(eval(Hoernli3), add = T, ppch = 15, border = "black", fill = "#99e377", anno = F, column = "Symbol",pcex = pointcex)
    } else {cat("> no 'Hoernli' values within range\n")}
  }
}
