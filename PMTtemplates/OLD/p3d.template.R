###############################################################################
#### P3D (Pseudo 3D) ##########################################################
###############################################################################
# clear all data from workspace!
rm(list=ls())
# set path where PMT functions are stored
pmtPath <- "H:/PMT3"

# set the working directory
setwd("H:/GIS/Coops/Ewelina/p3d")
# set the number of threads to be used for parallel processing
numCores = 26 # NA means all threads are used. Dont do that, if you want to do something else too

#### Parameters for data preparation and projection ######################start

# Parameters to specify the profile locations
# location name (will be part of output folder name)
location.name <- "HaweaFlat"
# coordinates of centerpoint
latlon <- c(5050263.67577266, 363258.21596366)
# radius (half length) of the profiles
radius = 6000
# spatial resolution in [m] (choose reasonyble value for available rasters: for STRG_ALT: 5, 10, 15, 20, 30, 40, 50, 60, 80, 100 or higher)
analysis.resolution <- 20

# alternatively set above params in one list (ordered like above. the above entered are ignored then):
#variable.params <- list("HaweaFlat",5050263.67577266, 363258.21596366,6000,30)
variable.params <- list("Schiener_M_DEVEL_TBD",5281027.91360976, 490284.77543343,13500,40)
if(exists('variable.params')){x=variable.params;location.name=x[[1]];latlon=c(x[[2]],x[[3]]);radius=x[[4]];analysis.resolution=x[[5]];rm(x)}

# count of the profiles (if you use this parameter, set deg = NA)
cnt = 6
# instead of cnt, deg (degrees between profiles) may be provided (set cnt = NA then)
# or also specific profile orientations can be set, by providing a vector of degrees (geographic degrees)
deg = NA #c(0,2,4,6,8)

# specify the path, name and extension of a raster (lidar) file (only 1 raster file supported)
#dem_1.path.name.extension <- c("H:/GIS/DEMs/5m", "alps_5m", ".tif")
if (analysis.resolution < 10 | analysis.resolution == 15) { 
  dem_1.path.name.extension = c("H:/GIS/DEMs/5m", "alps_5m", ".tif") 
} else if (analysis.resolution < 20 | analysis.resolution == 30) {
  dem_1.path.name.extension = c("H:/GIS/DEMs/10m", "alps_10m", ".tif") 
} else if (analysis.resolution < 50 | analysis.resolution %in% c(60, 80)) {
  dem_1.path.name.extension = c("H:/GIS/DEMs/20m", "alps_20m", ".tif") 
} else if (analysis.resolution >= 50) {
  dem_1.path.name.extension = c("H:/GIS/DEMs/50m", "alps_50m", ".tif") 
}


# specify the path and name of a map (polygons) only 1 map at a time supported
map_1.path.name <- c("H:/GIS/PhD/maps/flagg", "terraces")
# Specify which field should be used and how it should be renamed (optional. may be commented out.)
# If it should not be renamed, type the original name again
# If more cols should be used, specify c("old.colname.1", "new.colname.1", "old.colname.2", "new.colname2", ...)
map1.infield.outfield <- c("NAME_KURZ", "terraces.NAME_KURZ",
                           "terrace", "terraces.terrace")

# point shapefile path and name, max. of 9 point files (simply provide nothing here to not do this step at all)
points_1.path.name <- c("H:/GIS/Coops/Ewelina/ages", "DS_sites_coordinates_epsg32632")
points_2.path.name <- c("H:/GIS/PhD/maps/orientation/Localities", "localities_CH")


# mask the data, so that only areas matching selected polygon(s) is used for calculation (reduces calculation time)
use.mask = F
mask.field = NA # "mapped.terrace" # type NA, if you want to use all polygons in the shapefile
mask.polygon = 1280

# set a swath width, that is used for each profile, if desired. If not, all pixels are projected (time consuming)
use.swath = F
swath.width = 200

# specify a directory name, where all created data should be stored (is created if not existing)
#out.dir <- paste0(location.name,".",analysis.resolution,"m",".","mask",use.mask,".","swath",use.swath) # OLD
out.dir <- paste0(analysis.resolution,"m",".","mask",use.mask,".","swath",use.swath)

#### Parameters for data preparation and projection ########################end










# plotting is not done within this script anymore. Use pt2 script!
#### Parameters for Plotting ##############################################start

# if projecting has been done before, you may skip this step
projecting.already.done <- F

# slope threshold in [degrees] (terraces will be reduced to areas underneath a given max.slope value)
# type 90 degrees for no filtering
max.slope <- 90

# create plots of all units
# this is useful for a first overview, before setting specific styles
plot.data.allUnits = list(
  execute = F, grid = T, add.lidar = T
  )
plot.model.allUnits = list(
  execute = F, grid = F, add.lidar = F, model.thresh = 2, # sets min. count of bin values to create model from
  pmt.bin = list(mode = "bin", interval = 50, value = "median", cth = 40, sth = NA)
  )
plot.model.allUnitsAnnotated = list(
  execute = F, grid = T, add.lidar = F, model.thresh = 2, # sets min. count of bin values to create model from
  pmt.bin = list(mode = "bin", interval = 50, value = "median", cth = 40, sth = NA)
  )
plot.lidar = list(
  execute = F, grid = T, add.lidar = T
)

# define plot styles. (check object units, or browse profile.data)
cex <- 1.4 # factor to change point size in profiles -> increase for lower analysis.resolution
styles <- list(
  nt = list(plot = T,
    name = "01_NT",
    col = "#98c872",
    col2 = "#dff5cd"),
  ht = list(plot = T,
    name = "02_HT",
    col = "#5b8cbb",
    col2 = "#b5def7"),
  tds = list(plot = T,
    name = "03c_TDS_Mindel_Guenz_1800_780_ka",
    col = "#907a58",
    col2 = "#dbcd9e"),
  hds = list(plot = T,
    name = "06a_HDS_Donau_Biber_1800_780_ka",
    col = "#dd4243",
    col2 = "#f39a96"),
  loess = list(plot = F,
    name = "12_Deckschi",
    col = "grey30",
    col2 = "grey80"),
  modern = list(plot = T,
    name = "river.buffer450",
    col = "#292c78",
    col2 = "#bec1fc"),
  mindel = list(plot = T,
    name = "03ba_ODM",
    col = "#ffaf01",
    col2 = "#ffe8b6"),
  guenz = list(plot = T,
    name = "03bb_ODG",
    col = "#ff0101",
    col2 = "#ffbbbb"),
  donau = list(plot = T,
    name ="03bc_ODD",
    col = "#ad3a01",
    col2 = "#febfa0"),
  osds = list(plot = T, # meaningful colors to be chosen
    name ="03b_OSDS_undiff_1800_780_ka",
    col = c("#ad3a01","#012875"),
    col2 = c("#febfa0","#012875")),
  hs = list(plot = T, # meaningful colors to be set
    name ="10a_Hoehenschotter",
    col = c("#d7ffc3","#012875"),
    col2 = c("#d7ffc3","#012875")),
  # old FLAGG color code
  flagg.colors = list(biber = "#012875",
                      donau = "#ad3a01",
                      guenz = "#ff0101",
                      mindel = "#ffaf01",
                      riss = "#ffff01",
                      wuerm = "#d7ffc3",
                      modern = "#0000d1",
                      altpleistoPlio = "#398017",
                      ht_Schweiz = "#e6e600")
)

#### Parameters for Plotting ###############################################end






###############################################################################
#### Start of calculations ####################################################
###############################################################################

###### time protocol
start.time <- Sys.time()

require(readtext)
require(raster) # requires sp
require(dplyr)
require(sf)
require(rgeos)
require(maptools)
require(s2)

# prepare for parallel processing ############################################
require("doParallel")
require("foreach")
require("parallel")
if (is.na(numCores)) {
  numCores <- detectCores()
}
registerDoParallel(cores = numCores)
##############################################################################

# round lat and lon and store as separate objects
lon = round(latlon, digits = 0)[2]
lat = round(latlon, digits = 0)[1]

# import p3d functions {requires: readtext}
temp<-readtext(paste0(pmtPath,"/","Functions.R/p3d/*.R"), verbosity = 0);for (i in 1:length(temp[,2])){eval(parse(text=temp[i,2]))};rm(temp)

# import selected pmt functions {requires: readtext}
# NEW TESTING
eval(parse(text = readtext(paste0(pmtPath,"/","Functions.R/pt1/PMT3_linear.reference.fem.R"),verbosity = 0)[[2]]))
eval(parse(text = readtext(paste0(pmtPath,"/","Functions.R/pt1/linear.reference.points.R"),verbosity = 0)[[2]]))
eval(parse(text = readtext(paste0(pmtPath,"/","Functions.R/pt1/get.points.R"),verbosity = 0)[[2]]))
# OLD WORKING
#eval(parse(text = readtext(paste0(pmtPath,"/","Functions.R/pt1/linear.reference.points.multicore.R"),verbosity = 0)[[2]]))
temp<-readtext(paste0(pmtPath,"/","Functions.R/pt2/*.R"), verbosity = 0);for (i in 1:length(temp[,2])){eval(parse(text=temp[i,2]))};rm(temp)

# create general output directory
dir.create(paste0(getwd(),"/",location.name), showWarnings = FALSE)
dir.create(paste0(getwd(),"/",location.name,"/",out.dir), showWarnings = FALSE)

# export ingested parameters as parameters.Rdata
parameters <- list(
  # dem.names
  dem.names = dem_1.path.name.extension[2],
  # map.names
  map.names = map_1.path.name[2],
  # map.infields
  map.outfields = map1.infield.outfield[seq(1, length(map1.infield.outfield)-1, by = 2)], 
  # map.outfields
  map.outfields = map1.infield.outfield[seq(2, length(map1.infield.outfield), by = 2)], 
  # analysis.resolution
  analysis.resolution = analysis.resolution,
  # search.radius
  search.radius = if (use.swath) {swath.width/2} else {radius},
  # p3d.centroid
  p3d.centroid = list(lat = lat, lon = lon),
  # p3d.radius
  p3d.radius = radius,
  # point data count
  points.count = if (exists("points_1.path.name") == FALSE) { 0 } else { length(ls(pattern = "points_")[seq(1, by = 2, len = length(ls(pattern = "points_")))]) },
  # point data names
  points.names = if (exists("points_1.path.name") == FALSE) { NA } else {
    eval(expression(
      points.names <- get(ls(pattern = "points_")[1])[2],
      if (length(ls(pattern = "points_")) > 1) {
        for (i in 2:length(ls(pattern = "points_"))) {
          points.names <- c(points.names,get(ls(pattern = "points_")[i])[2])}
      },
      points.names
    ))
  }
) # the projection info is extracted by pt2 script from created profiles
save(parameters, file = paste0(location.name,"/",out.dir,"/","parameters.Rdata"))


################################################################################
################################################################################
#### execute projecting (time consuming! skip if allready done) ###########start
if (projecting.already.done == T) {
  load(file = paste0(out.dir,"/","profile.data",".Rdata"))
  lidar <- raster(paste0(out.dir,"/","lidar",".tif"))
  } else if (projecting.already.done == F) {
eval(parse(text = readtext(paste0(pmtPath,"/","Functions.R/p3d/scripts/p3d.executeProjection.R"),verbosity = 0)[[2]]))
}
#############################################################################end
################################################################################


# Plotting is not done within this script anymore. Use pt2 script instead.
# ################################################################################
# #### execute plotting #####################################################start
# 
# # create profile illustrations and export them 
# # {requires: pmt::pmt.filter; pmt::pmt.plot; pmt::pmt.extent}
# 
# #### preparations for profile illustrations ####
# # get profile extent
# extent <- pmt.extent(profile.data[["001"]])
# 
# # get available units (can be run, after projection is finished)
# units <- p3d.getAvailableUnits(profile.data)
# 
# # create profiles directory
# profile.dir <- "profiles"
# dir.create(paste0(location.name,"/",out.dir,"/",profile.dir), showWarnings = FALSE)
# ################################################
# 
# 
# 
# # plot data of all units, color coded
# # if styles are not defined (for all or a single unit), a random rainbow color is used
# 
# if (plot.data.allUnits[["execute"]] == T) {
#   p3d.export.plots(dir = "data.allUnits", 
#                    add.lidar = plot.data.allUnits[["add.lidar"]], 
#                    plot.function = "p3d.plot.data.allUnits",
#                    textcex = 1,
#                    grid = plot.data.allUnits[["grid"]])
# }
# 
# if (plot.model.allUnits[["execute"]] == T) {
#   # creates models with pmt.bin(mode = "bin", interval = 50, value = "median", cth = NA, sth = NA)
#   p3d.export.plots(dir = "model.allUnits", 
#                    add.lidar = plot.model.allUnits[["add.lidar"]], 
#                    plot.function = "p3d.plot.model.allUnits", 
#                    model.thresh = plot.model.allUnits[["model.thresh"]], # sets min. count of bin values to create model from
#                    textcex = 1,
#                    grid = plot.model.allUnits[["grid"]])
# }
# 
# if (plot.model.allUnitsAnnotated[["execute"]] == T) {
#   # creates models with pmt.bin(mode = "bin", interval = 50, value = "median", cth = 40, sth = NA)
#   backup <- extent
#   extent <- pmt.extent(profile.data[["001"]], yfact = 1) # create space for annotations
#   p3d.export.plots(dir = "model.allUnitsAnnotated", 
#                    add.lidar = plot.model.allUnitsAnnotated[["add.lidar"]], 
#                    plot.function = "p3d.plot.model.allUnitsAnnotated", 
#                    model.thresh = plot.model.allUnitsAnnotated[["model.thresh"]],
#                    textcex = 1,
#                    grid = plot.model.allUnitsAnnotated[["grid"]])
#   extent <- backup; rm("backup")
# }
# 
# if (plot.lidar[["execute"]] == T) {
#   p3d.export.plots(dir = "lidar", 
#                    add.lidar = plot.data.allUnits[["add.lidar"]], 
#                    plot.function = "p3d.plot.lidar",
#                    textcex = 1,
#                    grid = plot.data.allUnits[["grid"]])
# }







# # example for plotting single profiles
# max.slope <- 2     # slope can be adjusted here
# p3d.plot.data.allUnits(data = profile.data[[1]], main ="")
# p3d.plot.data.allUnits(data = 90, main ="")
# 
# # add points or text in plot with location as factor (-0.9 = left/downmost, 0.9 = right/upmost, 0 = center)
# points(x = xloc(0), 
#        y = yloc(0), 
#        pch = 16, 
#        cex = 2)
# 
# text(x = xloc(-0.9), 
#      y = yloc(-0.9),
#      labels = "test",
#      pos = 4)





#### plotting finished ######################################################end
################################################################################


# undo parallel processing setup #############################################
#registerDoSEQ() # reregister sequential processing after foreach is finished
##############################################################################


# time protocol
end.time <- Sys.time()
noquote(paste0("processing finished. Beginn: ", start.time, "; End: ", end.time))
end.time - start.time

########
