################################################################################
#### preset filter-expressions for terrace data for different maps #############
################################################################################



# for map: Christian Kuersten - Terrassen_EPSG32632.gpkg, field: Bez

N1 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = "1", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
NSF <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = "SF", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
N3 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = "3", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
N2 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = "2", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
N4 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = "4", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
N6 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = "6", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
NBSF <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = "BSF", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
N5 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = "5", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
NAntr. <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = "Antr.", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))







