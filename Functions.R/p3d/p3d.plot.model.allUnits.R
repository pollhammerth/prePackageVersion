#### plot models 
# {requires: p3d::p3d.getAvailableUnits; pt2::pmt.filter; pt2::pmt.plot; p3d::p3d.getStyle; p3d::p3d.geAvailableUnits}

# instead of selected profile data, you may supply a degree value of a specific profile as argument data

#p3d.plot.models(data = 90, main ="")



# arguments: data, dir, add.lidar, and pmt.plot arguments
# model.thresh defines a threshold of point count above which a model will be created
#   has to be larger than 1
p3d.plot.model.allUnits <- function(data, add.lidar = F, model.thresh = 2, ...) {
  
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
          style.col2 <- p3d.getStyle(filtered.data, "col2") # get defined color style for selected unit
          
          # plot a units data points
          pmt.plot(
            filtered.data, 
            add = T, 
            col = if (is.na(style.col) == TRUE) {rainbow(n=10)[j]} else {style.col2},
            cex = cex*0.5
          )
          
          # create and plot a units model (only done if data/binned points are at least as high as model.thresh)     
          if (length(pmt.bin(filtered.data, mode="bin", interval=50)[["y"]]) >= model.thresh) {
            filtered.data %>% 
              pmt.bin(mode = plot.model.allUnits[["pmt.bin"]][["mode"]], 
                      interval = plot.model.allUnits[["pmt.bin"]][["interval"]], 
                      value = plot.model.allUnits[["pmt.bin"]][["value"]], 
                      cth = plot.model.allUnits[["pmt.bin"]][["cth"]],
                      sth = plot.model.allUnits[["pmt.bin"]][["sth"]]) %>%
              pmt.model(deg = 1) %>% 
              pmt.plotModel(add = T, lev = 0.95, col = style.col, elev = 0, conf = T)
          } else {}
        }
      } ##########################################################################################################
    }
    
    # add lidar pixels on top of the plot
    if (add.lidar == T) {
      pmt.plot(
        data,
        add = T, 
        col = "grey70", 
        cex=cex*0.1
        )
    }
    
  }
  






