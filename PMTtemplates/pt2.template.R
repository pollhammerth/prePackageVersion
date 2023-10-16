###################################################################################################################################
#### Vorbereitung des Workspace; Datenimport ######################################################################################
###################################################################################################################################

#### setup working directory
rm(list=ls())
# set path where PMT functions are stored
pmtPath <- "H:/PMT3"

# working directory
setwd("H:/GIS/PhD/profiles/IllerLechPlatte/KressenbachRoth"); list.files("projectedData");list.files()

# specify whether the projected data comes from "p3d" or "pt1"
data.type <- "pt1"
# if data.type is "p3d", specify the profile id, that should be used for plotting
p3d.profileId <- "009"
# specify the subfolder(s), where projected data is stored
dataFolder <- "Outcrops_NA.Radius_5000.Map_terraces.Resolution_20.Mask_FALSE.profil"
dataFolder <- "Outcrops_NA.Radius_5000.Map_terraces.Resolution_30.Mask_FALSE.profil02"
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
# and first rename column for all profiles if necessary...
for (i in 1:length(profile.data)) {
  if(i<10){temp=paste0("00",i)}else{temp=paste0("0",i)}
  names(profile.data[[temp]])[which(names(profile.data[[temp]])=="NAME_KURZ")] <- "terraces.NAME_KURZ"
}; data <- profile.data[[p3d.profileId]]

unique(data$terraces.NAME_KURZ)



################################################################################
#### create plot ###############################################################
################################################################################

# open a new graphics device outside rstudio
dev.new()
dev.off()


fullExtent  <- list(xlim = c(0,310061), ylim = c(300,720), zlim = c(0,15100))
extent <- fullExtent
extent  <- list(xlim = c(0,310061), ylim = c(300,650), zlim = c(0,15100))


# setup a plot
plot01 <- function(){
  # setup empty plot
  pmt.empty(grid = T, main = "")
  
  # plot data
  # max.slope <- 90
   max.distance <- 5000
  
  max.slope <- 2; eval(NT) %>% pmt.plot(col = styles$nt$col, cex = 20, add = F, grid = T)
  max.slope <- 90; eval(LM) %>% pmt.plot(col = styles$tdst$col, cex = 20, add = T)
  max.slope <- 90; eval(HT) %>% pmt.plot(col = styles$ht$col, cex = 20, add = T)
  max.slope <- 90; eval(JDS) %>% pmt.plot(col = styles$flagg.colors$mindel, cex = 20, add = T)
  max.slope <- 90; eval(MDS) %>% pmt.plot(col = styles$flagg.colors$modern, cex = 20, add = T)
  max.slope <- 90; eval(ADS) %>% pmt.plot(col = styles$flagg.colors$guenz, cex = 20, add = T)
  max.slope <- 90; eval(MP) %>% pmt.plot(col = styles$flagg.colors$MINDELice, cex = 20, add = T)
  max.slope <- 90; eval(HADS) %>% pmt.plot(col = styles$flagg.colors$donau, cex = 20, add = T)
  max.slope <- 90; eval(AADS) %>% pmt.plot(col = styles$flagg.colors$biber, cex = 20, add = T)
  max.slope <- 90; eval(OD) %>% pmt.plot(col = styles$flagg.colors$riss, cex = 20, add = T)
  max.slope <- 90; eval(AP) %>% pmt.plot(col = styles$flagg.colors$altpleistoPlio, cex = 40, add = T)
  max.slope <- 90; eval(HS) %>% pmt.plot(col = "#9629d0", cex = 40, add = T)
  max.slope <- 90; eval(TDS) %>% pmt.plot(col = styles$tds$col, cex = 20, add = T)
  max.slope <- 90; eval(HDS) %>% pmt.plot(col = styles$hds$col, cex = 20, add = T)
  max.slope <- 90; eval(PL) %>% pmt.plot(col = styles$flagg.colors$LGMiceDARK, cex = 20, add = T)
  max.slope <- 90; eval(DEM) %>% pmt.plot(col = "black", cex = 1, add = T)
  
  # create model
  # b <- eval(NT) %>% pmt.bin(interval = 500, value = "median", mode = "bin", idfield = "terrace", cth = 20, sth = NA)
  # pmt.plotBin(b, fill = "red")
  # m <- pmt.model(b, deg = 2)
  # m %>% pmt.plotModel(col = "red")
  
  # pmt.drawSlope()
  
  # lines(HT_profil02, lwd = 2, col = "red")
}

# make a png of the plot (pt1 profile or single preselected p3d profile)
png("profilePlots/profile02.png", width = 21*3/2.54, height = 15*3/2.54, res = 400, units = "in"); plot01(); dev.off()


# if the source data is p3d, create a series of plots of all profiles
for (i in 1:length(profile.data)) {
  data <- profile.data[[names(profile.data)[i]]]
  png(paste0(workingTitle,"/","Plot01_id",names(profile.data)[i],".png"), width = 21*2/2.54, height = 15*2/2.54, res = 400, units = "in"); plot01(); dev.off()
}; data <- profile.data[[p3d.profileId]]



# testing a new type of plot
d = eval(DEM)[,c(7,1)]
max.slope = 2
extent  <- list(xlim = c(0,107294), ylim = c(0,500), zlim = c(0,19941)) 

specialPlot = function(){
  pmt.empty(grid = F, main = "")
  points(d,
         cex = 1.0,
         pch = 16,
         col = "#00000008"
  )
}
png("profilePlots/specialPlot.png", width = 21*3/2.54, height = 15*3/2.54, res = 400, units = "in"); specialPlot(); dev.off(); dev.set(dev.prev())







################################################################################
#### execute mapping within current plot #######################################
################################################################################

# use a model stored as l or manually draw a line along features
l <- pmt.drawLine()
lines(l, lwd = 2, col = "red")


#### maps ######################################################################
mapName <- "template" # specify a name for the map!
#data <- profile.data[[names(profile.data)[35]]]
l <- data.frame(x = c(361,1569,2619,3985,5668,7093,9192,11489,13845,16558,18617,19805,21271,22439,25587,28478,30339,33210,36359,39923,42457,44952,46536,49308,51704), y = c(362,351,345,337,333,325,317,311,303,298,292,289,287,283,272,268,262,252,245,239,234,228,228,221,217))
th <- 2; mapModel = F; modelDegree = 1; max.slope <- 90; max.slope.terrace <- 90; max.distance <- parameters$search.radius



################################################################################

# param legend:
# max.slope.terrace = max. local slope for terrace polygon creation only
# th = distance to line threshold up to which pixels are regarded as potential terrace

#### Execute mapping - no need to edit, just execute ###########################
# get slope via linear regression and use line or model
lm.slope <- round(abs(pmt.model(data = l, deg = 1)$coefficients[["x"]])*1000, digits = 1); lm.slope # get slope in permil via linear modelling
assign(mapName,l); l.backup <- l
if (mapModel) { l <- l %>% pmt.model(deg = modelDegree); pmt.plotModel(l, extrapol = T, conf = F); l <- l %>% pmt.modelToLine(extrapol = T); assign(mapName,l) }

# get pixels around line or model
p <- pmt.modelToPixels(l = l, p = eval(DEM)) # supply th, if you want to get terrace pixels only
pabove <- pmt.vPixelTh(p = p[which(p$slope <= 90),], above = T, th = th) # filter for pixels above terrace
pbelow <- pmt.vPixelTh(p = p[which(p$slope <= 90),], below = T, th = th) # filter for pixels below terrace
pterrace <- pmt.vPixelTh(p = p[which(p$slope <= max.slope.terrace),], th = th) # filter for terrace pixels
#pmt.plot(pterrace, col = "green", add = F); pmt.plot(pabove, col = "red"); pmt.plot(pbelow, col = "blue")

# convert the selected pixels to a map
mapTerrace <- pmt.pixelsToPolygons(pterrace) # map terrace
mapBarrier <- pmt.pixelsToPolygons(pabove) # map barriers, OPTIONAL
mapErosion <- pmt.pixelsToPolygons(pbelow) # map barriers, OPTIONAL

# export the map
dir.create(paste0("maps/")); dir.create(paste0("maps/",mapName))
writeOGR(mapTerrace, dsn = paste0("maps/",mapName), layer = paste0(mapName,".",if(data.type=="pt1"){paste0("profile",parameters$profile.name,".")}else if(data.type=="p3d"){paste0("p3dID",p3d.profileId,".")},if(exists("lm.slope")){paste0(lm.slope,"pm",".")},th,"m",".","sl",max.slope.terrace,".","Terrace"), driver = "ESRI Shapefile", overwrite_layer = T)
writeOGR(mapBarrier, dsn = paste0("maps/",mapName), layer = paste0(mapName,".",if(data.type=="pt1"){paste0("profile",parameters$profile.name,".")}else if(data.type=="p3d"){paste0("p3dID",p3d.profileId,".")},if(exists("lm.slope")){paste0(lm.slope,"pm",".")},th,"m",".","sl",max.slope,".","Barrier"), driver = "ESRI Shapefile", overwrite_layer = T)
writeOGR(mapErosion, dsn = paste0("maps/",mapName), layer = paste0(mapName,".",if(data.type=="pt1"){paste0("profile",parameters$profile.name,".")}else if(data.type=="p3d"){paste0("p3dID",p3d.profileId,".")},if(exists("lm.slope")){paste0(lm.slope,"pm",".")},th,"m",".","sl",max.slope,".","Erosion"), driver = "ESRI Shapefile", overwrite_layer = T)

# export profile to map
png(paste0("maps/",mapName,"/",mapName,"_profile.png"), width = 21*1/2.54, height = 15*1/2.54, res = 400, units = "in")
pmt.empty(grid=T,main=paste0(mapName, if(mapModel){" - map via model"}else{" - map via line"}))
l <- l.backup
lines(l, lwd = 3, col = "yellow")
l <- l %>% pmt.model(deg = 1); pmt.plotModel(l, extrapol = T, conf = F); l <- l %>% pmt.modelToLine(extrapol = T); assign(mapName,l)
max.slope <- 90; eval(DEM) %>% pmt.plot(col = "black", cex = 1, add = T)
dev.off(); dev.prev()

