
#### This is for importing projected data and setting up the workspace for pt.2
# it is not really standardized! And might need profile-specific editing.
# developing a further standardized workflow is an open todo!


#### Prepare workspace with custom functions and stuff #########################
#require("readtext")
require("sf")
require("rgdal")
# create output directory for plots
workingTitle <- "profilePlots"; dir.create(paste0(getwd(),"/",workingTitle), showWarnings = FALSE)
# a little helper (defines the opposite of %in%)
"%!in%" <- Negate("%in%")
# get FLAGG specific styles and filter expressions:
eval(parse(text = readtext(paste0(pmtPath,"/","FLAGGStyles/FLAGGSpecific.R"),verbosity = 0)[[2]]))
temp<-readtext(paste0(pmtPath,"/","filterExpressions/*.R"), verbosity = 0);for (i in 1:length(temp[,2])){eval(parse(text=temp[i,2]))};rm(temp)
# ... and specific functions:
temp<-readtext(paste0(pmtPath,"/","FLAGGStyles/*.R"), verbosity = 0);for (i in 1:length(temp[,2])){eval(parse(text=temp[i,2]))};rm(temp)

# Laden des PMT pt.2 Toolsets und aller noetigen R Packages
temp<-readtext(paste0(pmtPath,"/","Functions.R/pt2/*.R"), verbosity = 0);for (i in 1:length(temp[,2])){eval(parse(text=temp[i,2]))};rm(temp)
pmt.packages()
eval(parse(text = readtext(paste0(pmtPath,"/","Functions.R/p3d/xloc_yloc.R"),verbosity = 0)[[2]]))

# read original map, to check up properties/attributes, if needed
#fullMap <- read_sf(paste0("H:/GIS/PhD/maps/flagg", "/", "terraces", ".gpkg"))
#unique(fullMap$NAME_KURZ)
#unique(fullMap$NAME_LANG)

# load list of DS terrace ids for filtering purposes
input.path <- paste0(pmtPath,"/","FLAGGStyles/TerraceIds_exportFromQGIS")
load(paste0(input.path,"/","dsIds.Rdata"));load(paste0(input.path,"/","htIds.Rdata"));load(paste0(input.path,"/","ntIds.Rdata"))

#### Import projected data #####################################################
if (data.type == "pt1") {
  
  if (exists("outcropsFolder")) {
    # A1: import von Punktdaten, falls diese nachtraeglich berechnet wurden
    input.path <- paste0("projectedData","/",outcropsFolder)
    list.files(input.path)
    load(paste0(input.path,"/","data.Rdata"))
    # copy point data, to avoid (overwriting) when main data is imported
    points.names <- parameters$points.names; points.count <- parameters$points.count; pointsBackup <- points
  }
  
  # B: import main terrace data
  input.path <- paste0("projectedData","/",dataFolder)
  load(paste0(input.path,"/","data.Rdata"))
  
  if (exists("outcropsFolder")) {
    # A2: wenn vorhanden, separat neu berechnete Punktdaten mit Alten austauschen
    if (exists("points.names") == TRUE) {
      points <- pointsBackup; rm(pointsBackup)
      parameters$points.count <- points.count; rm(points.count)
      parameters$points.names <- points.names; rm(points.names)
    }
  }
} else if (data.type == "p3d") {
  input.path <- paste0(dataFolder)
  list.files(input.path)
  load(paste0(input.path,"/","parameters.Rdata")) # load p3d parameters
  load(paste0(input.path,"/","profile.data.Rdata")) # load all profiles
  data <- profile.data[[p3d.profileId]] # extract single profile data for plotting
  parameters$projection <- proj4string(profiles) # read used projection string
  # extract single points.per.profile data for plotting, via a function
  p3d.points.prep = function (p.id = p3d.profileId, ppp = points.per.profile) {
    points = ppp[[p.id]]
    for (i in 1:length(names(points))) { assign( paste0(names(points)[i]), as.data.frame(points[[names(points)[i]]]) ) }
    points = eval(parse(text = paste0("lst( ", paste0(names(points),collapse = ", "), " )") ))
    return(points)
  }; points = p3d.points.prep(p3d.profileId)
}



#### Prepare standards for plotting ############################################
# set standard plot parameters. max.slope will filter data by lidar slope threshold, even if y.source is different.
parameters
max.slope <- 90
if (data.type == "p3d") {
  max.distance <- parameters$p3d.radius
} else {
  max.distance <- parameters$search.radius
}
#if (data.type == "p3d") {
#    y.source <- "y"
#  } else {
if (isFALSE(parameters$dem.names[1] %in% colnames(data))) { # check if dem name is correctly set in parameters
  cat("\n \n WARNING!\n \n The DEM name specified in parameters$dem.names[1] does not exist in data colnames!\n
      The data colnames are:\n")
  cat(colnames(data),"\n
      Parameters has the following entry:\n",
      parameters$dem.names[1],"\n
      I will rename the third column according to parameters, assuming this is the DEM!\n \n warning end...\n \n")
  names(data)[3] <- parameters$dem.names[1]
}

    y.source <- parameters$dem.names[1]
#    }
    y.source

# get plot extent
#if (data.type == "pt1") {
  extent <- pmt.extent(data, x = "location", y = c(y.source), z = "distance")
#} else if (data.type == "p3d") {
#  extent <- pmt.extent(data)
#}
# optionally set a plot zoom
#extent <- pmt.zoom(); pmt.zoomPaster()
pmt.zoomPaster()
#fullExtent  <- list(xlim = c(0,28000), ylim = c(300,860), zlim = c(0,14015))
#extent <- fullExtent


################################################################################
#### parameters for plot adjusting #############################################
################################################################################

#### general plot parameters (originally defined for Synthesis Plot E-W)
# A3 landscape, not using full height
canvas.height <- 29.7/2 # 16 # 29.7
canvas.width <- 42.0/2
canvas.sizer <- 4 # factor for canvas size. 1 = standard
pcSizer <- 0.5 # adjust the size of pixels
ocSizer <- 0.5 # adjust the size of outcrop points (Base, Sedimentologic Data) - 0.3 is too small!
liSizer <- 1
#extent <- fullExtent # full extent
# adjust extent, so that full available area is used for plotting
#extent$xlim[[1]] <- extent$xlim[[1]] + 2500 # left boundary
#extent$xlim[[2]] <- extent$xlim[[2]] - 2500 # right boundary
#extent$ylim[[1]] <- extent$ylim[[1]] + 20 # lower boundary
#extent$ylim[[2]] <- extent$ylim[[2]] - 350 # upper boundary
# reset some parameters to make sure nothing is set wrong
lib.add=NA;lib.remove=NA;tds.add=NA;tds.remove=NA;hds.add=NA;hds.remove=NA
max.distance=parameters$search.radius;max.slope=90;y.source=parameters$dem.names[[1]];anno.srt=90
# The following is defined within the plot functions, but can also be set here, to plot outside of functions:
mlwd <- 1.5; tlwd <- 1.5 # this is for changing the lwd of models (m) and traces along profile of several rasters (t). Standard is 1.5.


