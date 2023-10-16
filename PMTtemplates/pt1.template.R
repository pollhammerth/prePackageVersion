###############################################################################
#### PMT2 -> R code to replace former ArcGIS routine ##########################
###############################################################################

# cleanup workspace (this deletes the whole workspace! if you want to save sth, do it before!)
rm(list=ls())
# set path where PMT functions are stored
pmtPath <- "H:/PMT3"


#### Set user specific variables here:
# Working directory (all data will be stored in a folder named projectedData)
setwd("H:/GIS/PhD/profiles/IllerLechPlatte/GuenzHaeuselmann") # Win
# set the number of threads to be used for parallel processing
# more cores needs more ram! If you are running out of memory, the code will stop with error!
numCores = 26 # NA means all cores are used. That is ok, but do not do that, if you want to do something else too.


# buffer (search radius) around profile in [m]
search.radius <- 8000 # 6500 in FLAGG 1
# spatial resolution in [m] (choose reasonyble value for available rasters: for STRG_ALT: 5, 10, 15, 20, 30, 40, 50, 60, 80, 100 or higher)
analysis.resolution <- 100
# do you want to calculate all lidar pixels within radius (F) or only polygon covered areas (T). F is cooler, T saves time!
use.mask <- F

# Route (only one route) (Please supply Geopackage. Extension must not be written!)
profile.path.name <- c("profile",
                       "profile")

# Raster(s) - one or more rasters can be used. rasters should be uniquely named.
if (analysis.resolution < 10 | analysis.resolution == 15) { 
  dem_1.path.name.extension = c("H:/GIS/DEMs/5m", "alps_5m", ".tif") 
} else if (analysis.resolution < 20 | analysis.resolution == 30) {
  dem_1.path.name.extension = c("H:/GIS/DEMs/10m", "alps_10m", ".tif") 
} else if (analysis.resolution < 50 | analysis.resolution %in% c(60, 80)) {
  dem_1.path.name.extension = c("H:/GIS/DEMs/20m", "alps_20m", ".tif") 
} else if (analysis.resolution >= 50) {
  dem_1.path.name.extension = c("H:/GIS/DEMs/50m", "alps_50m", ".tif") 
}

#dem_2.path.name.extension <- c("../../../../../IN/sternai2012",
#                               "QuaternaryUpliftGradient", # Quaternary erosion-deposition uplift gradient, Sternai et al. (2012)
#                               ".tif")

# Map(s) - multiple maps are supported, the fields of all maps should be uniquely named.
map_1.path.name <- c("H:/GIS/PhD/maps/flagg",
                     "terraces")
#map_2.path.name <- c("maps",
#                     "manual_nt_quick_split")

# Specify which field should be used and how it should be renamed (optional. may be commented out.)
# If it should not be renamed, type the original name again
# If more cols should be used, specify c("old.colname.1", "new.colname.1", "old.colname.2", "new.colname2", ...)
map1.infield.outfield <- c("NAME_KURZ", "terraces.NAME_KURZ",
                           "terrace", "terraces.terrace")
#map2.infield.outfield <- c("NAME", 
#                           "manual.NAME")



#### Optional/Alternatively:
# linear reference point shapefile to the given profile
# specify if this should be done only (TRUE), additionally (FALSE) or skip this step (NULL)
only.point.shp <- NULL
# point shapefile path and name (simply provide nothing here to not do this step at all)
points_1.path.name <- c("H:/GIS/PhD/maps/literatureFigs/haeuselmann2007", "Haeuselmann2007_locations")
#points_2.path.name <- c("../../../../../IN/maps/Qbase/nagra"
#                        , "BasisTDS_pts_update20141212_NNJoin" # used version for flagg II report
#)


# specify the name of the final 2D data file (besides this csv, the data is also stored inside the *.Rdata output)
# this is done for backwards compatibility. export.csv may be set FALSE to save disk space.
export.csv <- FALSE
output.name <- "data.csv"
# specify a directory name, where all created data should be stored (is created if not existing)
pt1.out.dir <- paste0(
  "Outcrops_",if(is.null(only.point.shp)){"NA"}else if(isTRUE(only.point.shp)){"Only"}else if(isFALSE(only.point.shp)){"Yes"},".",
  "Radius_",search.radius,
  if(isTRUE(only.point.shp) == FALSE) {paste0(".",
    "Map_",map_1.path.name[2],".",
    "Resolution_",analysis.resolution,".",
    "Mask_",use.mask,".",
    profile.path.name[2]
  )}
)

###############################################################################
#### Start of calculation #####################################################
###############################################################################

###### time protocol
start.time <- Sys.time()

# create working folder and change wd to that folder
workingTitle <- "projectedData" # all data will be stored within a subfolder within this folder
dir.create(paste0(getwd(),"/",workingTitle), showWarnings = FALSE)#; setwd(paste0(getwd(),"/",workingTitle))

# create output directory (subfolder within "projectedData")
dir.create(paste0(workingTitle,"/",pt1.out.dir), showWarnings = FALSE)

# import packages
require(rgdal)
require(sf)
require(raster)
require(maptools)
require(rgeos)
require(data.table)
require(dplyr)
#require(terra) # terra is used, but called explicitly within the script via terra:: where needed. This is to avoid problems where raster package versions have to be executed.
# PMT3
require(s2)

# # some additional for parallel processing
# require("parallel")
# require("foreach")
# require("doParallel")
# # prepare for parallel processing
# if (is.na(numCores)) {
#   numCores <- detectCores()
# }
# registerDoParallel(cores = numCores)


# import pmt functions (and expressions)
require(readtext)
temp<-readtext(paste0(pmtPath,"/","Functions.R/pt1/*.R"), verbosity = 0);for (i in 1:length(temp[,2])){eval(parse(text=temp[i,2]))};rm(temp)

#### 0. Output ingested parameters for documentation as list {requires: base; pt1::list.parameters}
parameters <- eval(list.parameters); parameters
# cleanup workspace
rm(map.names); rm(dem.names); rm(points.names); rm(list.parameters)
save(list = "parameters", file = paste0(workingTitle,"/",pt1.out.dir,"/","parameters.Rdata"))
####





#### 1. import line, create route and buffer
# import line
noquote(
  paste0("importing profile named: ", 
         as.character(profile.path.name[2]), 
         ".shp"
  )
)
# profile <- sf::st_read(dsn = profile.path.name[1], layer = profile.path.name[2]) # OLD Version!
profile <- terra::vect(x = paste0(profile.path.name[1],"/",profile.path.name[2],".gpkg"))
profile <- sf::st_as_sf(profile) # convert spatVector to sf

noquote(
  paste0("done. profile length: ", 
         as.character(
           round(
             st_length(
               st_as_sf(profile)
             ),
             digits = 0
           )
         )
  )
)
# check if profile has M values (M values are not needed currently. Referencing starts according to line digitising direction.)
if (rownames(data.frame(summary(profile$geometry)))[1] == "LINESTRING M") {
  noquote("M values are encoded in source profile line.")  
} else {
  noquote("M values are not present in profile line.")
}
# create buffer
noquote(
  paste0("calculating a ",
         search.radius,
         "m buffer around the profile"
  )
)
buffer <- sf::st_buffer(st_zm(st_as_sf(profile)), 
                        dist = search.radius, 
                        endCapStyle = "FLAT"
)
noquote("buffer is done and will be stored as buffer.shp")
write_sf(buffer,
         dsn = paste0(workingTitle,'/',pt1.out.dir,'/','buffer','.gpkg'), 
         driver = "GPKG"
)
noquote("buffer stored")
####





####################### BEGIN OF TEM CREATION AND PROJECTION ##################
if (isTRUE(only.point.shp) == FALSE) { # (only.point.shp != TRUE)
###############################################################################

#### 2. import DEM, clip to buffer bbox and mask by buffer
# execute load.dem function for all required dems (the function is inside the functions folder)
noquote("import, clip, mask and resample dem(s)...")
  
# load and prepare first dem (should be lidar)
dem_1 <- load.dem(dem_1.path.name.extension) %>%
  crop.mask.dem(buffer,analysis.resolution)
dem_1 <- resample.dem(dem_1, analysis.resolution) %>%
  extend.raster.to.roi(buffer,analysis.resolution)
# save clipped dem_1 to disk
raster::writeRaster(dem_1, paste0(workingTitle,"/",pt1.out.dir,"/","dem_1.tif"), overwrite = TRUE)
# get projection
projection <- proj4string(dem_1)

# create slope raster from first dem
# neighbours = 8 for rough surfaces, 4 for smoother surfaces
slope <- raster::terrain(dem_1, 
                 opt="slope", 
                 unit="degrees", 
                 neighbors=4 
) # optional argument: filename = "" for direct output to disk

# get additional dem layers
if (parameters$dem.count > 1) {
for (i in 2:parameters$dem.count) {
  assign(paste0("dem_",i),
         load.dem(get(paste0("dem_",i,".path.name.extension"))) %>%
           crop.mask.dem(buffer,analysis.resolution) %>%
           raster::resample(dem_1, method = "ngb") %>% # produziert manchmal Fehler... checkup!
           #  resample.dem(analysis.resolution) %>%
           extend.raster.to.roi(buffer,analysis.resolution)
  )
}
}
noquote("done")
####




#### 3. import one or series of maps and clip them to buffer geometry
noquote("importing map(s)...")
for (i in 1:parameters$map.count) {
  # import map
  assign(
    paste0("map_",i),
    read_sf(dsn = paste0(get(paste0("map_",i,".path.name"))[1], "/",get(paste0("map_",i,".path.name"))[2],".gpkg"), 
            layer = get(paste0("map_",i,".path.name"))[2]))
  # clip map to buffer geometry
  assign(
    paste0("map_",i),
    st_intersection(get(paste0("map_",i)), buffer)
  )
}
noquote("done")
# strap unused columns and rename used column
# get map names
map.names <- ls(pattern = "map_")[seq(1, by = 2, len = length(ls(pattern = "map_"))/2)]
if (length(ls(pattern = ".infield.outfield")) > 0) {
  map.id <- as.integer(gsub("map", "", gsub(".infield.outfield", "", ls(pattern = ".infield.outfield"))))
for (i in map.id) {
  # check how many cols should be used for map i
  eval(parse(text = paste0("col.count <- length(map",i,".infield.outfield)/2")))
  # remove unused columns for map i
  eval(parse(text = paste0(map.names[i], " <- subset(", map.names[i],", select = map",i,".infield.outfield[seq(1, by = 2, len = col.count)])")))
  # rename columns for map i
  eval(parse(text = paste0("data.table::setnames(",map.names[i] ,", old = map",i,".infield.outfield[seq(1, by = 2, len = col.count)], new = map",i,".infield.outfield[seq(1, by = 2, len = col.count) + 1 ])")))
}
}
# save clipped maps to disk
name <- ls(pattern = "^map_")
for (i in 1:length(name)) {
if (is.list(get(name[i])) == TRUE) {
  write_sf(get(name[i]),
           dsn = paste0(workingTitle,'/',pt1.out.dir,'/',name[i],'.gpkg'), 
           driver = "GPKG"
  )
}
}
# cleanup workspace
rm(map.names)
####




#### 4. create fem and spatial join with map
# stack all dems
dem.stack <- stack(c(dem_1, slope)) # first dem and associated slope raster
if (parameters$dem.count > 1) { # all additional dems if provided
for (i in 2:parameters$dem.count) {
  temp <- paste0("dem.stack <- stack(c(dem.stack, dem_",i,"))"); eval(parse(text = temp))
}} else {}
########## masking ###################
# mask the dem stack, if desired
if (use.mask == T) {
  
  # union all supplied polygons (attributes are lost) for masking only
  if (parameters$map.count > 1) {
    masker <- union(st_as_sf(map_1$geometry),st_as_sf(map_2$geometry))
  }
  if (parameters$map.count > 2) {
    for (i in 1:parameters$map.count){
      masker <- union(masker,st_as_sf(get(paste0("map_",i))$geometry))
    }
  } 
  if (parameters$map.count == 1){
    masker <- st_as_sf(map_1$geometry)
  }
  masker <- st_as_sf(masker)
  #  masker <- map_1[map_1@data[[mask.field]] == mask.polygon,]
  # mask dem
  dem.stack <- mask(x = dem.stack, 
                    mask = masker)
}
####################################
# create fem (fishnet elevation model, or SpatialPoints) from dem.stack
fem <-rasterToPoints(dem.stack, spatial = T)
# spatial join attributes from map(s) to points (create terrace elevation model (tem))
tem <- st_join(x = st_as_sf(fem), y = map_1)
if (parameters$map.count > 1) { # new, makes sense. Return to old, if it creates trouble!
# if (length(parameters$map.count > 1)) { # old working; but makes no sense!
  for (i in 2:parameters$map.count) {
    temp <- paste0("tem <- st_join(x = st_as_sf(tem), y = map_",i,")"); eval(parse(text = temp))
  }
} else {}

# cleanup workspace
rm(fem); rm(dem.stack); rm(slope)
####



#### 5. export a quick map of data for supervision
png(filename = paste0(workingTitle,"/",pt1.out.dir,"/","quick_map.png"), units = "cm", width = 29, height = 21, res = 300)
plot(dem_1, col = grey.colors(100, start = 0, end = 1))
plot(buffer, add = T, col = "transparent", border = "red", lty = 3)
plot(map_1, add = T)
plot(st_zm(profile[2]), add = T, col = "black", lwd = 2)
dev.off()
####




#### 6. linear referencing 
# extract map coordinates, so they dont get lost during projection
ctem <- as_Spatial(tem) %>% coordinates() # extract coords
Xmap <- ctem[,1] # convert to vectors
Ymap <- ctem[,2]
tem <- cbind(tem,Xmap,Ymap) # add map coords to tem
rm(list = c("ctem","Xmap","Ymap")) # cleanup workspace

#### NEW parallel processing version ###########################################
# project tem to profile PMT 3.0
projected.data <- linear.reference.fem(tem, profile)

# # project tem to profile PMT V2.8
# projected.data <- linear.reference.points.multicore(tem, profile, numCores)

#### OLD serial processing version #############################################
# M values need to be dropped, to get this working (st_zm()) ! It takes lines only and uses digitising direction.
#projected.data <- calculateStationing(as_Spatial(tem), as_Spatial(st_zm(profile)))
# add calculateStationing() side output to global environment to the functions return()
#projected.data <- cbind(projected.data, as.vector(distance.field))
# set a meaningful name to the added field
#names(projected.data)[length(names(projected.data))] <- "distance"
################################################################################

# cleanup workspace
rm(tem)
#rm(distance.field) # inside a function now
####




#### 7. prepare data for output *.csv for PMT2 pt.2 (some compatibility issues with PMT1 are addressed here)
# rename some columns
#names(projected.data)[names(projected.data) == dem_1.path.name.extension[2]] <- 'lidar'
names(projected.data)[names(projected.data) == "stationing"] <- 'location'
# name column RID is necessary for separate.csv function of PMT1
names(projected.data)[names(projected.data) == "nearest_line_id"] <- "RID"

# write projected 2D profile data to csv for import in PMT1 pt.2
if (isTRUE(export.csv)) {
# info: snapped to profile line coordinates on map view are stored in cols X and Y
write.table(projected.data, paste0(workingTitle,"/",pt1.out.dir,'/',output.name))
noquote(paste0("the 2D profile data has been stored at '", pt1.out.dir,"/",output.name,"'"))
} else {cat("> data export as *.csv has been skipped.")}
####

####################### END OF TEM CREATION AND PROJECTION ####################
} else { cat("> TEM creation and projection has been skipped.") }
###############################################################################
  






####################### BEGIN OF POINT IMPORT AND PROJECTION ##################
if (is.null(only.point.shp) == FALSE) {
###############################################################################

# 8. load, clip to buffer, extract lidar and linear reference a point shapefile (optional)
  # load first dem
  dem_1 <- raster(paste0(dem_1.path.name.extension[1], 
                       "/", 
                       dem_1.path.name.extension[2], 
                       dem_1.path.name.extension[3])
                )
  # get projection
  projection <- proj4string(dem_1)

  for (i in 1:parameters$points.count) {
  # get point shapefile and prepare it for referencing (clip, extract lidar values)
    assign(
  paste0("points_",i),
  get.points(get(paste0("points_",i,".path.name")),
             buffer,
             dem_1,
             dem_1.path.name.extension)
    )
  
  # linear referencing
    if (length(get(paste0("points_",i))$geom) != 0) { # if there are features present, project them
      assign(
        paste0("points_",i,".projected"),
        linear.reference.points(get(paste0("points_",i)), 
                                profile)
      )
    } else { # if there are no features present, just copy the empty point object <- this part creates an error! check!!
      assign(
        paste0("points_",i,".projected"),
        get(paste0("points_",i)) # this should be as_Spatial(get(paste0("points_",i))), to be the same class for all. But does not work with empty object. Maybe it does not matter...
      )
    }
    # set a meaningful name to the added distance field and other columns
    temp <- get(paste0("points_",i,".projected"))
    names(temp)[[length(names(temp))]] <- "distance"
    names(temp)[names(temp) == "stationing"] <- "x"
    names(temp)[names(temp) == "distance"] <- "z"
    names(temp)[names(temp) == dem_1.path.name.extension[2]] <- "lidar"
    names(temp)[names(temp) == "nearest_line_id"] <- "RID"
    assign(paste0("points_",i,".projected"),
           temp
    ); rm(temp)
    
  # save clipped point shapefile to disk 
  eval(parse(text = paste0("points_",i,"$id <- NULL"))) # (avoid problem, if 2 id / ID columns exist (id from Route??))
  write_sf(get(paste0("points_",i)),
           dsn = paste0(workingTitle,'/',pt1.out.dir,'/','points_',i,'.gpkg'), 
           driver = "GPKG"
  )
  
  # save referenced data to disk
  write.table(get(paste0("points_",i,".projected")), paste0(workingTitle,"/",pt1.out.dir,"/","points_",i,".csv"))
  
  # clean workspace -> not needed anymore, since wrapped in function
  rm(distance.field)
  
  }
  
####################### END OF POINT IMPORT AND PROJECTION ####################
} else { cat("> Optional point shapefile projection has been skipped.") }
###############################################################################
    

#### convert and save all needed data for pmt pt.2
# converting
if (exists("projected.data") == TRUE) {
  data <- as.data.frame(projected.data)
} else {data <- NA}

point.maps.list <- function(){
  if (parameters$points.count > 1) {
    points <- list(points_1,points_2)
  } else {points <- points_1}
  if (parameters$points.count > 2) {
    for (i in 3:parameters$points.count) {
      points[[i]] <- get(paste0("points_",i))
    }
  }
  return(points)
}
if (exists("points_1") == TRUE) {
point.maps <- point.maps.list()
} else {point.maps <- NA}

points.list <- function(){
  if (parameters$points.count > 1) {
    points <- list(as.data.frame(points_1.projected),as.data.frame(points_2.projected))
  } else {points <- list(as.data.frame(points_1.projected))}
  if (parameters$points.count > 2) {
    for (i in 3:parameters$points.count) {
      points[[i]] <- as.data.frame(get(paste0("points_",i,".projected")))
    }
  }
  return(points)
}
if (exists("points_1.projected") == TRUE) {
points <- points.list(); names(points) <- parameters$points.names
} else {points <- NA}

map.list <- function(){
  if (parameters$map.count > 1) {
    points <- list(map_1,map_2)
  } else {points <- list(map_1)}
  if (parameters$map.count > 2) {
    for (i in 3:parameters$map.count) {
      points[[i]] <- as.data.frame(get(paste0("map_",i)))
    }
  }
  return(points)
}
if (exists("map_1") == TRUE) {
maps <- map.list(); names(maps) <- parameters$map.names
} else {maps <- NA}


# # undo parallel processing setup #############################################
# registerDoSEQ() # reregister sequential processing after foreach is finished
##############################################################################


# time protocol
end.time <- Sys.time()
noquote(paste0("processing finished. Beginn: ", start.time, "; End: ", end.time))
time <- end.time - start.time
time


# add projection info to parameters list
parameters[(length(parameters)+1)] <- projection # add
names(parameters)[length(names(parameters))] <- "projection" # name
# saving
save(list = c("parameters",
              "data",
              "points",
              "parameters",
              "profile",
              "buffer",
              "point.maps",
              "maps",
              "dem_1",
              "analysis.resolution",
              "pt1.out.dir",
              "time",
              "projection"), 
     file = paste0(workingTitle,"/",pt1.out.dir,"/","data",".Rdata")
     )

# final workspace cleaning
rm(list = ls(pattern = "^dem_"))
rm(list = ls(pattern = ".path.name"))
rm(list = ls(pattern = "points_"))
#rm(list = ls(pattern = ""))




######

