# parallel processing version of linear.reference.points
# this needs to include a copy of LinearReferencing.R functions. So the latter can be deleted, 
# when everything is transferred to multicore



linear.reference.points.multicore <- function(tem, profile, numCores) {

  projected.data <- foreach (i = 1:numCores, .combine = rbind, .packages = c("rgeos","sf","maptools")) %dopar% {
    #### define projection functions (foreach creates an own environment and I do not know how to pass my functions there...)
    
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
      #snappedPoints <- cbind(snappedPoints, distance.field)
      #return(snappedPoints)
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
          onLine <- pointOnLine(point = crds_p, line_start = crds_l[j - 1,
          ], line_end = crds_l[j, ])
          if (onLine) {
            d0 <- sqrt((crds_p[1] - crds_l[j - 1, 1])^2 + (crds_p[2] - crds_l[j - 
                                                                                1, 2])^2)
            stationing <- c(stationing, round(d + d0, digits = 1))
            break
          }
          d <- d + sqrt((crds_l[j, 1] - crds_l[j - 1, 1])^2 + (crds_l[j, 2] - 
                                                                 crds_l[j - 1, 2])^2)
        }
      }
      snapped$stationing <- stationing
      return(snapped)
    }

    
# NEW        
    #### check if data is in the correct format for projection and convert it if necessary
    if (isFALSE(is(tem, "SpatialPointsDataFrame"))) {tem <- as_Spatial(tem)}
    if (isFALSE(is(profile, "SpatialLinesDataFrame"))) {profile <- as_Spatial(st_zm(profile))}
    # create bin boundaries for each parallel processing thread
    # c(0,130,260,...)
    bins <- c( round(seq(0,length(tem[,1]),by=(length(tem[,1])/numCores))) ) 
    # add point number to vector c(0,130,260,...,max)
    if ( isFALSE(tail(bins, n = 1) == length(tem[,1])) ) {    
      bins <- c( bins, length(tem[,1]) )
    }
    #### execute projection
    projected.data <- calculateStationing(tem[(bins[i]+1):bins[i+1],], 
                                          profile)
# OLD 
#    # create bin boundaries for each parallel processing thread
#    # c(0,130,260,...)
#    bins <- c( round(seq(0,length(as_Spatial(tem)[,1]),by=(length(as_Spatial(tem)[,1])/numCores))) ) 
#    # add point number to vector c(0,130,260,...,max)
#    if ( isFALSE(tail(bins, n = 1) == length(as_Spatial(tem)[,1])) ) {    
#      bins <- c( bins, length(as_Spatial(tem)[,1]) )
#    }
#    #### execute projection
#    projected.data <- calculateStationing(as_Spatial(tem)[(bins[i]+1):bins[i+1],], 
#                                          as_Spatial(st_zm(profile)))
    # convert to data.frame, so .combine = rbind works
    projected.data <- cbind(coordinates(projected.data),projected.data@data)
    
    # add orthogonal distance field
    cbind(projected.data, as.vector(distance.field))
  }
  
  # set a meaningful name to the added field
  names(projected.data)[length(names(projected.data))] <- "distance"
  
  # convert projected data to SpatialPointsDataFrame
  coordinates(projected.data) <- ~X+Y
  
  # return projected data
  return(projected.data)
  
}
