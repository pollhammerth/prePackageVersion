#### plot all units for overview, using random rainbow colors 
# {requires: p3d::p3d.getAvailableUnits; pt2::pmt.filter; pt2::pmt.plot; p3d::p3d.getStyle; p3d::p3d.geAvailableUnits}

# instead of selected profile data, you may supply a degree value of a specific profile as argument data



# arguments: data, dir, add.lidar
p3d.plot.data.allUnits <- function(data, add.lidar = T, ...) {
  
  # get profile data, if argument data is a degree value
  if (length(data) == 1) {
    ids.degrees <- data.frame(id = profiles@data$id, degrees = profiles@data$degrees)
    index <- as.integer(ids.degrees[ids.degrees[2] == data][1])
    data <- profile.data[[index]]
  } else {}
  
  # set up empty plot
  pmt.empty(sub = paste0("slope threshold = ", max.slope, " [degrees]; E = 0 [degrees, counterclockwise]"),
            ...)
  
  # get units of selected profile
  units <- p3d.getAvailableUnits(data, single.profile = T)
  for (j in 1:length(units)) {
    data.filter <- expression(
      pmt.filter(
        #          profile.data[[profiles@data$id[i]]], 
        data,
        regard.column = map1.infield.outfield[2], 
        regard.elements = units[j], 
        disregard.column = NA, 
        disregard.elements = NA, 
        thresh.column = "slope", 
        threshold = max.slope, 
        x = NA, y = NA, z = NA, 
        drop.unused = FALSE))
    filtered.data <- eval(data.filter) # filter single unit
    if (nrow(filtered.data) == 0) {} else { #### dont plot, if the data content is zero ########################
    style.plot <- p3d.getStyle(filtered.data, "plot")
    if (style.plot == F) {} else {
    style.col <- p3d.getStyle(filtered.data, "col") # get defined color style for selected unit
    pmt.plot( # plot the unit
      filtered.data, 
      #        main = paste0("ID ",id," | ",select.profile.by.degree, " degrees"), 
      add = T, 
      col = if (is.na(style.col) == TRUE) {rainbow(n=10)[j]} else {style.col},
      cex = cex*0.5,
      sub = paste0("slope threshold = ", max.slope, " [degrees]; E = 0 [degrees, counterclockwise]"),
      ...
    )
    }
    } ##########################################################################################################
  }
  if (add.lidar == T) {
    pmt.plot(
      #        profile.data[[profiles@data$id[i]]], 
      data,
      add = T, 
      col = "grey50", 
      cex=cex*0.2
    )
  }
  

}







