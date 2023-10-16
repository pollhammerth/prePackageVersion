


rm(list=ls())


#install.packages("rLFT")

require(rLFT)
require(terra)
require(sf)

setwd("H:/GIS/PhD/profiles/IllerLechPlatte/GuenzHaeuselmann") # Win

# load profile
profile_spatVector <- terra::vect(x = "profile/profile.gpkg")
profile_sf <- sf::st_as_sf(profile_spatVector) # convert spatVector to sf
profile_SpatialLinesDF <- as_Spatial(profile_sf)


# load points
points <- terra::vect(x = "H:/GIS/PhD/maps/literatureFigs/haeuselmann2007/Haeuselmann2007_locations.gpkg")
points <- sf::st_as_sf(points)
points

# load raster, clip to reasonable extent and convert to points
buffer <- st_buffer(st_zm(st_as_sf(profile_sf)), dist = 500, endCapStyle = "FLAT")
dem <- rast("H:/GIS/DEMs/50m/alps_50m.tif")
dem <- terra::crop(x = dem, y = buffer)
dem <- terra::mask(x = dem, mask = buffer)
fem_spatVector <- terra::as.points(x = dem)
fem_sf <- sf::st_as_sf(fem_spatVector) # takes long...!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! but needed for s2 package!
fem_SpatialPointsDF <- sf::as_Spatial(fem_sf) # takes long...!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! but needed for old PMT Version!



#### now lets develop a new spatial referencing method:



#install.packages("s2")
require(s2)
# distance <- s2_project(x = profile, y = fem)

start.time <- Sys.time()
x <- s2_project(x = profile_sf, y = fem_sf) # 7 s for 50m (~ 0.5 MPx) / 53 s for 20m (~ 3 MPx) / 4 min for 10m (~ 12 MPx)
z <- s2_distance(profile_sf, fem_sf)


#projData <- calculateStationing(fem_SpatialPointsDF,profile_SpatialLinesDF) # 18 min for 50m (~ 0.5 MPx)
end.time <- Sys.time(); noquote(paste0("processing finished. Beginn: ", start.time, "; End: ", end.time)); time <- end.time - start.time; time



plot(y = terra::as.data.frame(fem_spatVector)[["alps_50m"]], x = distance, pch = 46, cex = 0.1)












length(fem_spatVector$alps_50m)


names(fem_spatVector)[2]


length(fem_sf$alps_50m)
length(fem_SpatialPointsDF$alps_50m)


dev.new()
plot(x = distance, y = fem$alps_20m, pch = 46, cex = 0.01)
plot(x = distance, y = fem$alps_10m, pch = 46, cex = 0.01)
dev.off()



require(gdalUtilities)

addM()

profile_sf_m <- addMValues(profile_sf)


p <-  as.matrix(profile_sf)
l <- p[3]
l <- l[[1]]
l <- as.numeric(l)
dim <- length(l)/2
x <- l[1:dim]
y <- l[(dim+1):(dim*2)]
m <- matrix(nrow = dim*2, ncol = 2)
m[,1] <- x
m[,2] <- y
profile_m <- addM(m)
profile_m




terra::as.lines(profile_stars_m)

profile_stars_m <- stars::st_as_stars(profile_sf_m)

asS4(profile_SpatialLinesDF)

?snap




snapped <- snap(x = fem_spatVector, y = profile_spatVector, tolerance = 0)
snapped <- snap(x = fem_spatVector, tolerance = 8000)

plot(snapped)




plot(x = df[[1]], y = df[[2]])

showMethods(snap)


install.packages("leaflet")
require(leaflet)
terra::plet(dem)



persp(dem)




###### OLD VERSION #############################################################

require(maptools)


# linear referencing serial processing version (still used for outcrop data, the tem is working with multicoreversion)
linear.reference.points <- function(points, profile = profile) {
  
  # NEW  
  #### check if data is in the correct format for projection and convert it if necessary
  if (isFALSE(is(points, "SpatialPointsDataFrame"))) {points <- as_Spatial(points)}
  if (isFALSE(is(profile, "SpatialLinesDataFrame"))) {profile <- as_Spatial(st_zm(profile))}
  #### execute projection
  points.projected <- calculateStationing(points, profile)
  # OLD  
  #  # linear reference points
  #  points.projected <- calculateStationing(as_Spatial(points), as_Spatial(st_zm(profile)))
  
  
  # add distance field (side output from calculateStationing())
  points.projected <- cbind(points.projected, as.vector(distance.field))
  
  # return function output
  return(points.projected) # es scheint auch ohne diese Zeile funktioniert zu haben!?
  
}





###############################################################################
#### These linear referencing functions have been taken from: #################
#### http://rstudio-pubs-static.s3.amazonaws.com/12524_7de6eb887f2945389c5d12869388be14.html
###############################################################################



# determine if point is on line (Output TRUE or FALSE for single point)
pointOnLine <- function(point, line_start, line_end) {
  # each of input parameters is pair of coordinates [x,y]
  if (identical(point, line_start) | identical(point, line_end)) 
  {
    return(TRUE)
  }  # check, if the points cooincides with start/end point
  if (point[1] > max(c(line_start[1], line_end[1])) | point[1] < min(c(line_start[1], 
                                                                       line_end[1])) | point[2] > max(c(line_start[2], line_end[2])) | point[2] < 
      min(c(line_start[2], line_end[2]))) {
    return(FALSE)  # if the point is out of the bounding box of the line, return false
  }
  if (line_start[2] == line_end[2]) {
    slope <- 0
  } else if (line_start[1] == line_end[1]) {
    return(T)
  } else {
    slope <- (line_start[2] - line_end[2])/(line_start[1] - line_end[1])
  }
  intercept <- -slope * line_start[1] + line_start[2]
  onLine <- round(point[2], digits = 0) == round((slope * point[1] + intercept), 
                                                 digits = 0)
  return(onLine)
}

# snapping points to lines
snapPointsToLines <- function(points, lines, maxDist = NA, withAttrs = TRUE) {
  require("rgeos")
  if (is(points, "SpatialPoints") && missing(withAttrs)) 
    withAttrs = FALSE
  if (!is.na(maxDist)) {
    w = gWithinDistance(points, lines, dist = maxDist, byid = TRUE)
    validPoints = apply(w, 2, any)
    validLines = apply(w, 1, any)
    points = points[validPoints, ]
    lines = lines[validLines, ]
  }
  
  d = gDistance(points, lines, byid = TRUE) #### parallelise !?
  
  # store d (orthogonal distance) to global env, to append it later to the projected data. Comment out, if not needed.
  distance.field <<- d  
  
  nearest_line_index = apply(d, 2, which.min)
  coordsLines = coordinates(lines)
  coordsPoints = coordinates(points)
  mNewCoords = vapply(1:length(points), function(x) nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]], 
                                                                       coordsPoints[x, ]), FUN.VALUE = c(0, 0))
  if (!is.na(maxDist)) 
    nearest_line_id = as.numeric(rownames(d)[nearest_line_index]) + 1 else nearest_line_id = nearest_line_index
  if (withAttrs) 
    df = cbind(points@data, nearest_line_id) else df = data.frame(nearest_line_id, row.names = names(nearest_line_index))
  SpatialPointsDataFrame(coords = t(mNewCoords), data = df, proj4string = CRS(proj4string(points)))
}




# calculate stationing (same as calculating the position of points on line?)
calculateStationing <- function(points, lines, maxDist = NA) {
  
  # snap points to lines from package maptools
  snapped <- snapPointsToLines(points, lines, maxDist, withAttrs = TRUE)
  
  stationing <- c()
  
  for (i in 1:length(snapped)) {
    crds_p <- coordinates(snapped[i, ])
    line <- lines[snapped[i, ]$nearest_line_id, ]
    crds_l <- coordinates(line)[[1]][[1]]
    d <- 0
    for (j in 2:nrow(crds_l)) {
      onLine <- pointOnLine(point = crds_p, line_start = crds_l[j - 1,  # maybe parallelize this!!? foreach?
      ], line_end = crds_l[j, ])
      if (onLine) {
        d0 <- sqrt((crds_p[1] - crds_l[j - 1, 1])^2 + (crds_p[2] - crds_l[j - 
                                                                            1, 2])^2)
        stationing <- c(stationing, round(d + d0))
        break
      }
      d <- d + sqrt((crds_l[j, 1] - crds_l[j - 1, 1])^2 + (crds_l[j, 2] - 
                                                             crds_l[j - 1, 2])^2)
    }
  }
  
  snapped$stationing <- stationing
  return(snapped)
}




