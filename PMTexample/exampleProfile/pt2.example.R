###################################################################################################################################
#### Vorbereitung des Workspace; Datenimport ######################################################################################
###################################################################################################################################

#### setup working directory
rm(list=ls())
# set path where PMT functions are stored
pmtPath <- "H:/PMT"

# working directory
setwd("H:/PMTexample/exampleProfile"); list.files("projectedData");list.files()

# specify whether the projected data comes from "p3d" or "pt1"
data.type <- "pt1"
# if data.type is "p3d", specify the profile id, that should be used for plotting
p3d.profileId <- "006"
# specify the subfolder(s), where projected data is stored
dataFolder <- "Outcrops_Yes.Radius_800.Map_terraces.Resolution_10.Mask_FALSE"
# outcropsFolder <- "Outcrops_Only.Radius_5000" # optional. Will replace outcrop data. Comment out if not needed.


#### Prepare workspace with custom functions and stuff #########################
require("readtext")
# prepare workspace with functions and import projected data
eval(parse(text = readtext(paste0(pmtPath,"/Functions.R/pt2/Helper/workspacePreparation.R"),verbosity = 0)[[2]]))

# check the parameters used for projection
parameters

# optionally set a plot zoom
#extent <- pmt.zoom(); pmt.zoomPaster()
pmt.zoomPaster()
# fullExtent  <- list(xlim = c(0,28000), ylim = c(300,860), zlim = c(0,14015))
# extent <- fullExtent




################################################################################
#### set data filter expressions for uniquely attributed pixels ################
################################################################################

# check available units
unique(data$Niveau)

# filter-expressions for terrace data
DEM <- expression(
  pmt.filter(data, 
             regard.column = NA, regard.elements = NA, 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
N1 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = c("1"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
N2 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = c("2"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
N3 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = "3", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
N4 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = c("4"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
N5 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = c("5"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
N6 <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = c("6"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
SF <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = c("SF"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
BSF <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = c("BSF"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
Antr <- expression(
  pmt.filter(data, 
             regard.column = "Niveau", regard.elements = c("Antr"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))



################################################################################
#### create plot ###############################################################
################################################################################

# open a new graphics device outside rstudio
dev.new()

# setup a plot
plot01 <- function(){
  # setup empty plot
  pmt.empty(grid = T, main = "")
  
  # plot data
  #max.slope <- 90
  #max.distance <- 1000
  
  eval(N1) %>% pmt.plot(col = eval(N1)[["Color"]][1], cex = 20, add = T)
  eval(N2) %>% pmt.plot(col = eval(N2)[["Color"]][1], cex = 20, add = T)
  eval(N3) %>% pmt.plot(col = eval(N3)[["Color"]][1], cex = 20, add = T)
  eval(N4) %>% pmt.plot(col = eval(N4)[["Color"]][1], cex = 20, add = T)
  eval(N5) %>% pmt.plot(col = eval(N5)[["Color"]][1], cex = 20, add = T)
  eval(N6) %>% pmt.plot(col = eval(N6)[["Color"]][1], cex = 20, add = T)
  eval(SF) %>% pmt.plot(col = eval(SF)[["Color"]][1], cex = 20, add = T)
  eval(BSF) %>% pmt.plot(col = eval(BSF)[["Color"]][1], cex = 20, add = T)
  eval(Antr) %>% pmt.plot(col = eval(Antr)[["Color"]][1], cex = 20, add = T)
  
  eval(DEM) %>% pmt.plot(col = "black", cex = 10, add = T)
  
  max.slope = 2
  max.distance = 800
  eval(N1) %>% pmt.plot(col = "red", add = F, grid = T)

  # optionally set a plot zoom
  #extent <- pmt.zoom(); pmt.zoomPaster()
 # pmt.zoomPaster()
 # fullExtent  <- list(xlim = c(0,7999), ylim = c(514,898), zlim = c(0,810))
 # extent = list(xlim = c(0,7999), ylim = c(514,560), zlim = c(0,810))
 # extent <- fullExtent
  
  
  
  # create model
  b <- eval(N1) %>% pmt.bin(interval = 100, value = "median", mode = "bin", idfield = "terraceID", cth = 50, sth = 1)
  pmt.plotBin(b, fill = "red")
  
  m <- b %>% pmt.model(deg = 2)
  m %>% pmt.plotModel(col = "green", conf = F)
  
  # pmt.drawSlope()
  m %>% pmt.drawSlope(locate=FALSE,x1=1692.2,y1=525.5,x2=1960.5,y2=552.6,x3=2364.7,y3=552.6,ann=TRUE)
  
}

plot01()

# make a png of the plot (pt1 profile or single preselected p3d profile)
png("profilePlots/plot01.png", width = 21*2/2.54, height = 15*2/2.54, res = 400, units = "in"); plot01(); dev.off()






# if the source data is p3d, create a series of plots of all profiles
for (i in 1:length(profile.data)) {
  data <- profile.data[[names(profile.data)[i]]]
  png(paste0(workingTitle,"/","Plot01_id",names(profile.data)[i],".png"), width = 21/2.54, height = 15/2.54, res = 400, units = "in"); plot01(); dev.off()
}; data <- profile.data[[p3d.profileId]]










################################################################################
#### execute mapping within current plot #######################################
################################################################################

#### delineate terrace level to be mapped with a line and distance threshold
# use a model stored as l or manually draw a line along features

# example - manual delineation
l <- pmt.drawLine()
l <- data.frame(x = c(3084,3130,3280,3369,3477,3611,3723,3831), y = c(545,545,546,547,547,548,548,549))
#l <- l %>% pmt.model(deg = 1); pmt.plotModel(l, extrapol = T, conf = F); l <- l %>% pmt.modelToLine(extrapol = T)

# example2 - modelling
orange <- expression(
  pmt.filter(data, 
             regard.column = "terraceID", regard.elements = 27, 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
max.slope <- 2
b <- eval(orange) %>% pmt.bin(interval = 40, value = "median", mode = "bin", idfield = "terrace", cth = NA, sth = NA); b %>% pmt.plotBin(fill = "red")
l <- b %>% pmt.model(deg = 1); l %>% pmt.plotModel(col = "red", conf = F)




#### create polygons from line or model
# create a swath within which pixels are selected around the line
max.slope <- 90
max.distance <- 1000 #parameters$p3d.radius
max.slope.terrace <- 90 # max. local slope for terrace polygon creation only
th <- 3 # distance to line threshold up to which pixels are regarded as potential terrace

p <- pmt.modelToPixels(l = l, p = eval(DEM)) # supply th, if you want to get terrace pixels only
pabove <- pmt.vPixelTh(p = p[which(p$slope <= 90),], above = T, th = th) # filter for pixels above terrace
pbelow <- pmt.vPixelTh(p = p[which(p$slope <= 90),], below = T, th = th) # filter for pixels below terrace
pterrace <- pmt.vPixelTh(p = p[which(p$slope <= max.slope.terrace),], th = th) # filter for terrace pixels
#pmt.plot(pterrace, col = "green", add = F); pmt.plot(pabove, col = "red"); pmt.plot(pbelow, col = "blue")


# convert the selected pixels to a map
mapTerrace <- pmt.pixelsToPolygons(pterrace) # map terrace
mapBarrier <- pmt.pixelsToPolygons(pabove) # map barriers, OPTIONAL
mapErosion <- pmt.pixelsToPolygons(pbelow) # map barriers, OPTIONAL
#plot(mapTerrace, col = "green")
#plot(mapBarrier)
#plot(mapErosion)


# export the map
mapName <- "example" # specify a name for the map!
writeOGR(mapTerrace, dsn = "maps", layer = paste0(mapName,".","ID",p3d.profileId,".",th,"m",".","sl",max.slope.terrace,".","Terrace"), driver = "ESRI Shapefile", overwrite_layer = T)
writeOGR(mapBarrier, dsn = "maps", layer = paste0(mapName,".","ID",p3d.profileId,".",th,"m",".","sl",max.slope,".","Barriers"), driver = "ESRI Shapefile", overwrite_layer = T)
writeOGR(mapErosion, dsn = "maps", layer = paste0(mapName,".","ID",p3d.profileId,".",th,"m",".","sl",max.slope,".","Erosion"), driver = "ESRI Shapefile", overwrite_layer = T)



