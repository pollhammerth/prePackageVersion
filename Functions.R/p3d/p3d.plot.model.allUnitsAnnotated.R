#### plot models 
# {requires: p3d::p3d.getAvailableUnits; pt2::pmt.filter; pt2::pmt.plot; p3d::p3d.getStyle; p3d::p3d.geAvailableUnits}

# instead of selected profile data, you may supply a degree value of a specific profile as argument data

#p3d.plot.models(data = 90, main ="")



# arguments: data, dir, add.lidar, and pmt.plot arguments
# model.thresh defines a threshold of point count above which a model will be created
#   has to be larger than 1
p3d.plot.model.allUnitsAnnotated <- function(data, add.lidar = F, model.thresh = 2, ...) {
  
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
      if (length(pmt.bin(filtered.data, mode="bin", interval=50, cth = NA)[["y"]]) >= model.thresh) {
          m <- filtered.data %>%
          pmt.bin(mode = plot.model.allUnitsAnnotated[["pmt.bin"]][["mode"]], 
                  interval = plot.model.allUnitsAnnotated[["pmt.bin"]][["interval"]], 
                  value = plot.model.allUnitsAnnotated[["pmt.bin"]][["value"]], 
                  cth = plot.model.allUnitsAnnotated[["pmt.bin"]][["cth"]],
                  sth = plot.model.allUnitsAnnotated[["pmt.bin"]][["sth"]]) %>%
          pmt.model(deg = 1)
          pmt.plotModel(m, add = T, lev = 0.95, col = style.col, elev = 0, conf = T)
      } else {}
      # Add model properties to the plot:
      mfact <- 0.05 * j
      text(x = xloc(-0.5), y = yloc(-0.95 + mfact),
           paste0("R^2 = ",round(summary.lm(m)$r.squared,4)),
           pos = 4,
           col = style.col,
           cex = 0.7)
      text(x = xloc(0), y = yloc(-0.95 + mfact),
           paste0("slope [permil] = ",round(abs(m$coefficients[["x"]])*1000,1)),
           pos = 4,
           col = style.col,
           cex = 0.7)
        }
      } ##########################################################################################################
    }
    
    # add lidar pixels on top of the plot
    if (add.lidar == T) {
      pmt.plot(
        data,
        add = T, 
        col = "grey50", 
        cex=cex*0.1
        )
    }
    
  }
  

# 
# data <- profile.data[["001"]]
# pmt.select("data")
# class(data)
# names(data)
# 
# 
# 
# e <- expression(pmt.filter(data, "mapped.NAME_KURZ", c("03c_TDS_Mindel_Guenz_1800_780_ka"), 
#                       disregard.column = NA, disregard.elements = NA, thresh.column = NA, 
#                       threshold = NA, x = NA, y = NA, z = NA, drop.unused = FALSE))
# d <- eval(e)
# extent <- pmt.extent(d)
# pmt.plot(d, add = F)
# b <- pmt.bin(d,100,"median","bin",NA,NA)
# pmt.plotBin(b)
# m <- pmt.model(d,1)
# pmt.plotModel(m)
# 
# 
# extent <- pmt.extent(d)
# 
# pmt.extentAdd <- function(data, y = NA, x = NA) {
# 
#   e <- pmt.extent()
#   
#   if (is.na(y) == TRUE) {} else if (y > 0) {
#     strech <- e$ylim[2] - e$ylim[1]
#     e$ylim[1] <- e$ylim[1] - (strech * 0.5)
#   } else if (y < 0) {
#     strech <- e$ylim[2] - e$ylim[1]
#     e$ylim[1] <- e$ylim[2] + (strech * 0.5)
#   }
#   
#   if (is.na(x) == TRUE) {} else if (x > 0) {
#     strech <- e$xlim[2] - e$xlim[1]
#     e$xlim[1] <- e$xlim[1] - (strech * 0.5)
#   } else if (x < 0) {
#     strech <- e$xlim[2] - e$xlim[1]
#     e$xlim[1] <- e$xlim[2] + (strech * 0.5)
#   }
#   
#   return(e)
# }
# 
# 
# extent <- pmt.extent(d,yfact = 1)

#op <- par()
#par(mgp=c(1.5,0.5,0),mar=c(3,3,2,1)+0.1) 
#pmt.plot(d, add = F)

#par(op)
 
# 
# y = 0.5
# 
# 
# 
# 
# str(m)
# s <- summary.lm(m)
# s$r.squared
# s
# m$coefficients
# 
# m <- lm(d$y ~ d$x)
# 
# abs(m$coefficients[["x"]])
# 
# 
# x = seq(0,6000,by = 100)
# y = 531.066537552 + x * -0.00442261
# points(x,y)
# 
# text(x = xloc(-0.9), y = yloc(-0.9), paste0("R^2 = ",as.character(summary.lm(m)$r.squared)), pos = 4)
# text(x = xloc(0.9), y = yloc(-0.9), paste0("slope = ",as.character(abs(m$coefficients[["x"]]))), pos = 2)
# 
# 
# 
# m <- glm(d$y ~ d$x)
# summary.glm(m)
# 
# 
