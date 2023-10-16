#### TRASH #####################################################################
# p
# p <- eval(DEM)
# l <- data.frame(x = c(9441,10792,12111,13261,16171,17514,18624,19508,20730), y = c(402,396,391,387,380,377,375,375,374))
# head(l)
# head(p)
# th = 2
# 
# st_as_sf(l)
# 
# dt <- s2_distance(st_as_sf(l), st_as_sf(p))
# head(dt)
################################################################################

# select terrace pixels with th around line or model and return them
# l = "SpatialLinesDataFrame", "lm", or data.frame with line coordinates, as created by pmt.drawLine()
# p = projected points as data.frame
# th = distance threshold araound l, up to which pixels should be regarded as terrace
pmt.modelToPixels <- function(l, p, th) {
  #### check the line input format and convert it to SpatialLinesDataFrame, if necessary
  if (is(l, "data.frame")) {
    # execute some conversions, to create a spatial lines object (if not already the case)
    l <- l %>% as.matrix() %>% # convert to matrix
      st_linestring() %>% # convert matrix to linestring
      st_sfc() %>% # convert linestring to simple feature collection
      as_Spatial() # convert simple feature collection to Spatial Lines Data Frame
  } else if (is(l, "lm")) {
    #### convert model to a spatial line
    l <- l %>% 
      pmt.modelToLine(extrapol = F) # use extrapolated model, if you like, by setting extrapol = T
  } else if (isFALSE(is(l, "SpatialLinesDataFrame"))) {
    cat("> l must be a SpatialLinesDataFrame, a lm object, or the output of pmt.drawLine")
  } else {}
  
  
  #### delete pixels left or right of the line
  ldf <- as.data.frame(l@lines[[1]]@Lines[[1]]@coords) # reconvert line to data.frame for subsetting below and approxfun later
  p <- p[which(p$x >= min(ldf$x)),]
  p <- p[which(p$x <= max(ldf$x)),]
  #### convert projected pixel data to SpatialPointsDataFrame, if provided as data.frame
  if (is(p, "data.frame")) {
    p <- p %>% pmt.spdf()
  } else {}
  
  #### calculate distance of each point to the line (terrace top surface) ######
  #### serial version - working ##############################################
    dt <- s2_distance(st_as_sf(l), st_as_sf(p))
  # #### multicore version - testing #############################################
     # require("foreach")
     # require("parallel")
     # require("doParallel")
     # # prepare for parallel processing
     # numCores <- detectCores()
     # numCores <- numCores-2 # reduce the number of cores, so some power remains for other tasks
     # registerDoParallel(cores = numCores) # register the cluster for multicore processing with %dopar%
  #   # create bin boundaries for each parallel processing thread
  #   # c(0,130,260,...)
  #   b <- c( round(seq(0,length(p[,1]),by=(length(p[,1])/numCores))) )
  #   # add max to vector c(0,130,260,...,max)
  #   if ( isFALSE(tail(b, n = 1) == length(p[,1])) ) {
  #     b <- c( b, length(coordinates(p) <- ~x+y[,1]) )
  #   }
  #   dt <- foreach (i = 1:numCores, .combine = c, .packages = c("rgeos")) %dopar% {
  #     pi <- p[(b[i]+1):b[i+1],]
  #     gDistance(pi, l, byid = TRUE)
  #   }
  # ##############################################################################
  
  # add distance field to projected points
  df <- p %>% pmt.spdf() %>% cbind(as.vector(dt))
  names(df)[names(df) == "as.vector(dt)"] <- "vDistance"
  
  
  
  #### check whether a point is above or below a line ##########################
  # create a function, that will linearly connect given points, defining a line
  lfun <- approxfun(ldf$x,ldf$y)
  # calculate the y value for points x value and check if its > or < than the points y coordinate
  require("foreach")
  require("parallel")
  require("doParallel")
  # prepare for parallel processing
  numCores <- detectCores()
  numCores <- numCores-2 # reduce the number of cores, so some power remains for other tasks
  registerDoParallel(cores = numCores) # register the cluster for multicore processing with %dopar%
  
    # create bin boundaries for each parallel processing thread
    # c(0,130,260,...)
    b <- c( round(seq(0,length(p[,1]),by=(length(p[,1])/numCores))) )
    # add max to vector c(0,130,260,...,max)
    if ( isFALSE(tail(b, n = 1) == length(p[,1])) ) {
      b <- c( b, length(coordinates(p) <- ~x+y[,1]) )
    }

  above <- foreach (i = 1:numCores, .combine = c, .packages = c("stats")) %dopar% {
    # split the pixels to bins
    dfi <- df[(b[i]+1):b[i+1],]
    # check whether the pixel is above or below line
    a <- lfun(dfi$x[1]) > dfi$y[1]
    for (j in 2:length(dfi$x)) {
      a <- c(a, lfun(dfi$x[j]) > dfi$y[j])
    }; a
  }
  # add result as new column
  df <- cbind(df, above = above)
  ##############################################################################
  
  registerDoSEQ() # reregister sequential processing after foreach is finished

  
    
  # subset points up to a max distance, if a threshold is given
  if (isFALSE(missing(th))) {
    df <- df[which(df$vDistance <= th),]
  }
  # return selected points
  return(df)
}  


pmt.vPixelTh <- function (p, above = F, below = F, th) {
  
  # remove all points outside a threshold distance, if th is given
  if (isFALSE(missing(th)) && isFALSE(above) && isFALSE(below)) {
    p <- p[which(p$vDistance <= th),]
  }
  
  # remove all points below line
  if (above) { 
    if (isFALSE(missing(th))) { # remove points within th if given
      p <- p[which(p$vDistance > th),]
    }
    p <- p[which(p$above == F),] 
  }
  
  # remove all points above line
  if (below) { 
    if (isFALSE(missing(th))) { # remove points within th if given
      p <- p[which(p$vDistance > th),]
    }
    p <- p[which(p$above),] 
  }
  
  return(p)
  
}




# takes the mapped terrace pixels (from pmt.modelAsPixels) and polygonizes the data
pmt.pixelsToPolygons <- function(p) { 
  #### convert projected pixels to map view
  ds <- p %>% pmt.spdf(~Xmap+Ymap) # make spatial with map coordinates
  require("raster")

  #### convert to raster of mapped terraces
  # get rid of all attributes and add attribute specifying terrace y/n
  ds@data <- data.frame(t = rep(1,length(coordinates(ds)[,1])))
  # convert to spatialpixeldataframe
  gridded(ds) = T
  # convert to a raster
  ds <- raster(ds)
  # define the projection of the data
#  crs(ds) <- parameters$projection
  
  
  #### convert to polygons
  ds <- rasterToPolygons(x = ds, na.rm = T, dissolve = T)
  # set the projection of the final polygon map
  crs(ds) <- parameters$projection # set projection
  
  #### return polygons of mapped terraces
  return(ds)

}




# helper function to convert projected data.frame to spatial object (S4) and vice versa
pmt.spdf <- function(data,coords = ~x+y){
  if(is(data,"data.frame")){
    s4 <- data
    s4$y[is.na(s4$y)] <- -9999 # set NA values (points where no dem is available) to -9999, so that coordinates() works
    coordinates(s4) <- coords
    return(s4)
  } else if (is(data,"SpatialPointsDataFrame")){
    df <- cbind(coordinates(data),data@data)
    df$y[df$y == -9999] <- NA # reset -9999 values to NA, as in the original data.frame
    return(df)
  } else {print("> data is neither data.frame, nor SpatialPointsDataFrame")}
}



