# This is a function, that can be used to plot the cosmogenic nuclide dating results, as compiled by Gaudenz Deplazes
# {requires: pmt.filter(), pmt.plotBin()}

plotGaudenzCosmo <- function(addToExisting = T, pointcex = 1, thCol = NA, th = NA, disCol = NA, disElem = NA, regCol = NA, regElem = NA,hds_tds_all = "all",annoColumn = "CosmoAgeMa",...) {
  cosmoA <- expression(pmt.filter(points[[parameters$points.names[which(str_detect(parameters$points.names, "GaudenzDatierungen") == TRUE)]]], 
                                 regard.column = "CosmoAgeMa", # this is done, so that all other than cosmo are removed
                                 regard.elements = c("0.97-1.27","1.8 - 4.6","0.5 - 1.3","0.2 - 1.7","1.24-1.56","1.2 - 4.7","0.4 - 2.2","0.5-1.3","1.1 - 1.9","0.56-0.84"), 
                                 disregard.column = NA, 
                                 disregard.elements = NA, thresh.column = NA, threshold = NA, 
                                 x = "x", y = "lidar", z = "z", drop.unused = FALSE))
  cosmo <- expression(pmt.filter(eval(cosmoA), 
                                  regard.column = regCol, 
                                  regard.elements = regElem, 
                                  disregard.column = disCol, 
                                  disregard.elements = disElem, thresh.column = thCol, threshold = th, 
                                  x = "x", y = "y", z = "z", drop.unused = FALSE))
  # filter only hds or tds, if desired
  if (hds_tds_all == "hds") {
    cosmo2 <- expression(pmt.filter(eval(cosmo), 
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
    cosmo2 <- expression(pmt.filter(eval(cosmo), 
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
  } else {cosmo2 <- cosmo}
  
  pmt.plotBin(eval(cosmo2), x = "x", y = "y", add = addToExisting, pcex = pointcex, anno = T, column = annoColumn,
              ppch = 18, border = "black", fill = "#00ac8d",...)
}
