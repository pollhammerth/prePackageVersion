################################################################################
#### preset filter-expressions for terrace data for different maps #############
################################################################################




# for standardised manual quick map: manual.gpkg, field: NAME
ROI <- expression(
  pmt.filter(data, 
             regard.column = "manual.NAME", regard.elements = "roi", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))



