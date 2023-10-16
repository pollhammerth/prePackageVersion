# outsourced expression for pmt pt1 only

list.parameters <- expression(
  # create and save list with ingested parameters,
  list(
    dem.count =
      length(ls(pattern = "dem_")[seq(1, by = 2, len = length(ls(pattern = "dem_")))]),
    map.count = 
      length(ls(pattern = "map_")[seq(1, by = 2, len = length(ls(pattern = "map_")))]),
    profile.name = 
      profile.path.name[2],
    dem.names = 
      eval(expression(
        dem.names <- get(ls(pattern = "dem_")[1])[2],
        if (length(ls(pattern = "dem_")) > 1) {
          for (i in 2:length(ls(pattern = "dem_"))) {dem.names <- c(dem.names,get(ls(pattern = "dem_")[i])[2])}
        },
        dem.names
      )),
    map.names = 
      eval(expression(
        map.names <- get(ls(pattern = "map_")[1])[2],
        if (length(ls(pattern = "map_")) > 1) {
          for (i in 2:length(ls(pattern = "map_"))) {
            map.names <- c(map.names,get(ls(pattern = "map_")[i])[2])}
        },
        map.names
      )),
    map.infields = 
      eval(expression(
        map.infields <- get(ls(pattern = ".infield.outfield")[1])[1],
        if (length(ls(pattern = ".infield.outfield")) > 1) {
          for (i in 2:length(ls(pattern = ".infield.outfield"))) {
            map.infields <- c(map.infields,get(ls(pattern = ".infield.outfield")[i])[1])}
        },
        map.infields
      )),
    map.outfields = 
      eval(expression(
        map.outfields <- get(ls(pattern = ".infield.outfield")[1])[2],
        if (length(ls(pattern = ".infield.outfield")) > 1) {
          for (i in 2:length(ls(pattern = ".infield.outfield"))) {
            map.outfields <- c(map.outfields,get(ls(pattern = ".infield.outfield")[i])[2])}
        },
        map.outfields
      )),
    analysis.resolution = 
      analysis.resolution,
    search.radius = 
      search.radius, 
    working.directory = 
      getwd(), 
    projected.data = 
      paste0(pt1.out.dir,"/",output.name),
    points.count = 
      if (exists("points_1.path.name") == FALSE) { 0 } else {
        length(ls(pattern = "points_")[seq(1, by = 2, len = length(ls(pattern = "points_")))])
      },
    points.names = 
      eval(expression(
        points.names <- get(ls(pattern = "points_")[1])[2],
        if (length(ls(pattern = "points_")) > 1) {
          for (i in 2:length(ls(pattern = "points_"))) {
            points.names <- c(points.names,get(ls(pattern = "points_")[i])[2])}
        },
        points.names
      ))
  )  
)


# Execute as follows:

# # 00 get list of ingested parameters and save it to disk
# parameters <- eval(list.parameters)
# # cleanup workspace
# rm(map.names); rm(dem.names); rm(points.names)
# save(list = "parameters", file = paste0(pt1.out.dir,"/","parameters.Rdata"))
# ####





# #### OLD WORKING
# 
# 
# # outsourced expression for pmt pt1 only
# 
# list.parameters <- expression(
# # create and save list with ingested parameters,
# list(
#   dem.count =
#     length(ls(pattern = "dem_")[seq(1, by = 2, len = length(ls(pattern = "dem_")))]),
#   map.count = 
#     length(ls(pattern = "map_")[seq(1, by = 2, len = length(ls(pattern = "map_")))]),
#   profile.name = 
#     profile.path.name[2],
#   dem.names = 
#     eval(expression(
#       dem.names <- get(ls(pattern = "dem_")[1])[2],
#       for (i in 2:length(ls(pattern = "dem_"))) {dem.names <- c(dem.names,get(ls(pattern = "dem_")[i])[2])},
#       dem.names
#     )),
#   map.names = 
#     eval(expression(
#       map.names <- get(ls(pattern = "map_")[1])[2],
#       for (i in 2:length(ls(pattern = "map_"))) {
#         map.names <- c(map.names,get(ls(pattern = "map_")[i])[2])},
#       map.names
#     )),
#   map.infields = 
#     eval(expression(
#       map.infields <- get(ls(pattern = ".infield.outfield")[1])[1],
#       for (i in 2:length(ls(pattern = ".infield.outfield"))) {
#         map.infields <- c(map.infields,get(ls(pattern = ".infield.outfield")[i])[1])},
#       map.infields
#     )),
#   map.outfields = 
#     eval(expression(
#       map.outfields <- get(ls(pattern = ".infield.outfield")[1])[2],
#       for (i in 2:length(ls(pattern = ".infield.outfield"))) {
#         map.outfields <- c(map.outfields,get(ls(pattern = ".infield.outfield")[i])[2])},
#       map.outfields
#     )),
#   analysis.resolution = 
#     analysis.resolution,
#   search.radius = 
#     search.radius, 
#   working.directory = 
#     getwd(), 
#   projected.data = 
#     paste0(pt1.out.dir,"/",output.name),
#   points.count = 
#     if (exists("points_1.path.name") == FALSE) { 0 } else {
#       length(ls(pattern = "points_")[seq(1, by = 2, len = length(ls(pattern = "points_")))])
#     },
#   points.names = 
#     if(exists("points_1.path.name")==FALSE){points.names=NA} else {
#       points.names <- get(ls(pattern = "points_")[1])[2]
#       for(i in 2:length(ls(pattern="points_"))){points.names<-c(points.names,get(ls(pattern ="points_")[i])[2])}
#       points.names
#     }
# )
# )
# 
# 
# # Execute as follows:
# 
# # # 00 get list of ingested parameters and save it to disk
# # parameters <- eval(list.parameters)
# # # cleanup workspace
# # rm(map.names); rm(dem.names); rm(points.names)
# # save(list = "parameters", file = paste0(pt1.out.dir,"/","parameters.Rdata"))
# # ####
