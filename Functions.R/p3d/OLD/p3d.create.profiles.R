# Function to create a series of radial straight lines, defined with:
# {requires: base; dplyr, sp}
# centerpoint (lon,lat), 
# radius (half line length), 
# and coordinate reference system, 
# count of desired lines or alternatively, 
# degrees between neighbouring lines
# if several deg values are supplied, only those specific profiles are created
#   note: East = 0, counterclockwise
p3d.create.profiles <- function(lon, lat, radius, CRS, cnt = NA, deg = NA){
  if (is.na(cnt) == FALSE) {deg <- 180/cnt}
  
  if (length(deg) > 1) {
  # calculate coordinates of specific degree values
  x1 <- cos(pi/180*deg)*radius
  y1 <- sin(pi/180*deg)*radius
  x2 <- cos(pi/180*deg)*radius*(-1)
  y2 <- sin(pi/180*deg)*radius*(-1)
  # create attributes for the lines, to be added later
  id <- as.character(seq(1001,1000+length(deg)))
  id <- sub("^.", "", id)
  deg.id <- deg
  id <- data.frame(id = id, degrees = deg.id) #, " degr., E = 0, counterclockwise"))
  # how many profiles will this be? (needed for create lines)
  profile.count <- length(deg)
  
  } else {
  # calculate coordinates of radial profiles, covering 180 degrees
  x1 <- cos(pi/180*deg*0)*radius
  for (i in 1:(floor(180/deg-1))) {
    x1 <- c(x1, cos(pi/180*deg*(i))*radius)
  }
  y1 <- sin(pi/180*deg*0)*radius
  for (i in 1:(floor(180/deg-1))) {
    y1 <- c(y1, sin(pi/180*deg*(i))*radius)
  }
  x2 <- cos(pi/180*deg*0)*radius*(-1)
  for (i in 1:(floor(180/deg-1))) {
    x2 <- c(x2, cos(pi/180*deg*(i))*radius*(-1))
  }
  y2 <- sin(pi/180*deg*0)*radius*(-1)
  for (i in 1:(floor(180/deg-1))) {
    y2 <- c(y2, sin(pi/180*deg*(i))*radius*(-1))
  }
  # create attributes for the lines, to be added later
  id <- as.character(1001:(1001+floor(180/deg)))
  id <- sub("^.", "", id); deg.id <- round((as.integer(id)-1)*deg, digits = 1)
  id <- data.frame(id = id, degrees = deg.id) #, " degr., E = 0, counterclockwise"))
  # how many profiles will this be? (needed for create lines)
  profile.count <- floor(180/deg)
  }
  
  # translate coordinates to match search radius
  x1 <- x1 + lon
  x2 <- x2 + lon
  y1 <- y1 + lat
  y2 <- y2 + lat
  # create Lines from coordinates as single objects
  e <- expression(if (i < 10) {paste0("00")} else if (i < 100) {paste0("0")} else {})
  for (i in 1:profile.count) {
    profile.points <- data.frame(lon = c(x1[i],x2[i]), lat = c(y1[i],y2[i]))
    assign(
      paste0("temporary.ln_",eval(e),i),
      Line(profile.points) %>% Lines(ID = paste0(eval(e),i))
    )
  }
  # put those lines in a single SpatialLines object
  get.lines <- expression(str2lang(paste0("list(",paste0(ls(pattern = "temporary.ln_"),collapse=", "),")")))
  temporary.lns <- SpatialLines(eval(eval(get.lines)))
  # add attributes to the lines
  p3d.profiles <- SpatialLinesDataFrame(temporary.lns, id, match.ID = "id")
  # set coordinate system to the spatial lines object
  proj4string(p3d.profiles) <- CRS
  
  # function output
  return(p3d.profiles)
  
}
