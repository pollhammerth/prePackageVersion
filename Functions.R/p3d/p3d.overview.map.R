# plot an overview map of profiles, only useable with p3d
# {requires: raster}

p3d.overview.map <- function() {
  # plot lidar
  dem <- raster(paste0(location.name,"/",out.dir,"/","lidar",".tif"))
  plot(dem, col = grey.colors(100, start = 0, end = 1), sub = as.character(CRS))
  
  # plot map
  plot(map_1, add = T)
  
  # plot mask, if one exists
  if (use.mask == T) {
    plot(tem, add = T, col = "yellow", pch = 16, cex = 1)
    plot(masker, add = T, border = "yellow", lwd = 2)
    plot(masker, add = T, border = "black", lwd = 1, lty = 3)
  }
  
  # plot profiles
  plot(profiles, col = c("white"), add = T, lwd = 1.5)
  plot(profiles, col = c("black"), add = T, lwd = 0.7)
  # plot buffer
  plot(buffer, add = T, lwd = 2, border = "red", lty = 3)
  # plot centerpoint
  plot(centerpoint, add = 1, cex = 1, pch = 1, col = "white")
  plot(centerpoint, add = 1, cex = 0.7, pch = 3, col = "white")
  #### plot profile labels, showing degrees ##############################start
  # get profile start coordinates
  s <- as.double(profiles@lines[[1]]@Lines[[1]]@coords[1,])
  for (i in 2:length(profiles@lines)) {
    s <- rbind(s,as.double(profiles@lines[[i]]@Lines[[1]]@coords[1,]))
  }
  s <- data.frame(x = s[,1], y = s[,2])
  # add label to values
  text(s[1,], "degrees   ", pos = 2)
  # get text to be plottet on profile start coordinates
  t <- profiles@data$degrees
  # thin the label count, if a critical density of labels is reached
  if (length(t) > 64) {i = 4} 
  else if (length(t) > 32) {i = 3} 
  else if (length(t) > 24) {i = 2} 
  else {i = 1}
  l <- length(s[,1])
  s <- s[seq(1,l, by = i),]
  t <- t[seq(1,l, by = i)]
  # convert label coordinates to SpatialPoints
  coordinates(s) <- ~x + y
  s <- SpatialPoints(s, proj4string = CRS)
  # plot profile labels
  plot(s, add = T, pch = 16, cex = 4.5, col = "black")
  plot(s, add = T, pch = 16, cex = 4, col = "white")
  text(s, t, cex = 0.7)
  #### labelling finished ##################################################end
}


