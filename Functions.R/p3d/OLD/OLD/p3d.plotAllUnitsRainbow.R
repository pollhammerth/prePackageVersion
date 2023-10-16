#### plot all units for overview, using random rainbow colors 
# {requires: p3d::p3d.getAvailableUnits; pt2::pmt.filter; pt2::pmt.plot}
p3d.plotAllUnitsRainbow <- function(dir = "all.mapped.units.rainbow", add.lidar = T){
  # create output directory
  dir.create(paste0(getwd(),"/",out.dir,"/",profile.dir), showWarnings = FALSE)
  dir.create(paste0(getwd(),"/",out.dir,"/",profile.dir,"/",dir), showWarnings = FALSE)
  
  for (i in 1:length(profile.data)) {
    # get units of selected profile
    units <- p3d.getAvailableUnits(profile.data[[i]], single.profile = T)
    # plot all units
    png(filename=paste0(out.dir,"/",profile.dir,"/",dir,"/",profiles@data$id[i],".png"),width=16,height=8,units="cm",res=300)
    for (j in 1:length(units)) {
      data.filter <- expression(
        pmt.filter(
          profile.data[[profiles@data$id[i]]], 
          regard.column = map1.infield.outfield[2], 
          regard.elements = units[j], 
          disregard.column = disregard.column, 
          disregard.elements = disregard.elements, 
          thresh.column = "slope", 
          threshold = max.slope, 
          x = NA, y = NA, z = NA, 
          drop.unused = FALSE))
      pmt.plot(
        eval(data.filter), 
        main = paste0("ID ",profiles@data$id[i]," | ",
                      profiles@data$degrees[i], " degrees"), 
        add = if (j==1) {F} else {T}, 
        col = rainbow(n=10)[j],
        cex = cex*0.5,
        sub = paste0("slope threshold = ", max.slope, " [degrees]; E = 0 [degrees, counterclockwise]")
      )
    }
      if (add.lidar == T) {
      pmt.plot(
        profile.data[[profiles@data$id[i]]], 
        add = T, 
        col = "grey70", 
        cex=cex*0.1)
      }
      
    
    dev.off()
  }
  
}
