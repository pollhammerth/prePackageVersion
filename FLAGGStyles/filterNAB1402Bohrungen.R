# This is a function, that can be used to plot the cluster analysis data from Graf (1993), using Grafs Symbols.
# {requires: pmt.filter(), pmt.plotBin()}


filterNAB1402Bohrungen <- function(ht_nt_all = "all", thCol = NA, th = NA, disCol = NA, disElem = NA, regCol = NA, regElem = NA, pointsName = "baseHtNt") {
  # filter individual clusters from grafs heavy mineral cluster analysis
  bohrungen <- expression(pmt.filter(points[[pointsName]], 
                                      regCol, 
                                      regElem, 
                                      disregard.column = disCol, 
                                      disregard.elements = disElem, 
                                      thresh.column = thCol, 
                                      threshold = th, 
                                      x = "x", y = "baseElev", z = "z", 
                                      drop.unused = FALSE))
  
  
  #### filter only hds or tds, if desired
  if (ht_nt_all == "ht") {
    bohrungen2 <- expression(pmt.filter(eval(bohrungen), 
                                  regard.column = "terrace", 
                                  regard.elements = c(htIds, if(is.na(ht.add)==FALSE){ht.add}else{NULL}), 
                                  disregard.column = "terrace", 
                                  disregard.elements = ht.remove,
                                  thresh.column = NA, 
                                  threshold = NA, 
                                  x = "x", 
                                  y = "y", 
                                  z = "z", 
                                  drop.unused = FALSE))
  } else if (ht_nt_all == "nt") {
    bohrungen2 <- expression(pmt.filter(eval(bohrungen), 
                                  regard.column = "terrace", 
                                  regard.elements = c(ntIds, if(is.na(nt.add)==FALSE){nt.add}else{NULL}), 
                                  disregard.column = "terrace", 
                                  disregard.elements = nt.remove,
                                  thresh.column = NA, 
                                  threshold = NA, 
                                  x = "x", 
                                  y = "y", 
                                  z = "z", 
                                  drop.unused = FALSE))
  } else {bohrungen2 <- bohrungen}
  ####
  
  # function output
  return(eval(bohrungen2))
  
}


