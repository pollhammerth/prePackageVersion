# saves one or a series of profile plots to disk
# uses p3d.plot functions like p3d.plot.data.allUnits
# if all profiles should be exported, keep first argument to standard,
# if selected profiles should be plotted, use e.g.: c(18,90)


p3d.export.plots <- function(
  select.profile.by.degree = profiles@data$degrees,  
  dir = "data.allUnits", 
  plot.function = "p3d.plot.data.allUnits", 
  ...) 
{  
  # create output directory
  dir.create(paste0(getwd(),"/",out.dir,"/",profile.dir), showWarnings = FALSE)
  dir.create(paste0(getwd(),"/",out.dir,"/",profile.dir,"/",dir), showWarnings = FALSE)
  
  
  for (i in 1:length(select.profile.by.degree)) {
    # get profile id from supplied degree value
    id <- profiles[which(profiles@data$degrees == select.profile.by.degree[i]),]@data$id

    
    # create the desired plots
    if (plot.function == "p3d.plot.data.allUnits") {
      png(filename=paste0(out.dir,"/",profile.dir,"/",dir,"/","id.",id,".png"),width=16,height=8,units="cm",res=300)
      p3d.plot.data.allUnits(profile.data[[id]], 
                             main = paste0("ID ",id," | ",
                                           select.profile.by.degree[i], " degrees"),
                             ...
      )
    }
    if (plot.function == "p3d.plot.model.allUnits") {
      png(filename=paste0(out.dir,"/",profile.dir,"/",dir,"/","id.",id,".png"),width=16,height=8,units="cm",res=300)
      p3d.plot.model.allUnits(profile.data[[id]], 
                             main = paste0("ID ",id," | ",
                                           select.profile.by.degree[i], " degrees"),
                             ...
      )
    }
    if (plot.function == "p3d.plot.model.allUnitsAnnotated") {
      png(filename=paste0(out.dir,"/",profile.dir,"/",dir,"/","id.",id,".png"),width=16,height=16,units="cm",res=300)
      p3d.plot.model.allUnitsAnnotated(profile.data[[id]], 
                              main = paste0("ID ",id," | ",
                                            select.profile.by.degree[i], " degrees"),
                              ...
      )
    }
    
    
    # stop plotting
    dev.off()
  }
  
}

