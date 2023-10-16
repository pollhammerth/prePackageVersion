###############################################################################
#### p3d execute projection with given parameters #############################
###############################################################################



# 00 load first raster and get coordinate system (all supplied data must use the same CRS!) {requires: raster}
dem_1 <- raster(paste0(dem_1.path.name.extension[1], 
                       "/", 
                       dem_1.path.name.extension[2],
                       dem_1.path.name.extension[3]))
# get coordinate reference system
CRS <- crs(dem_1); CRS





#### 01 create routes from input parameters and create a buffer (for masking/clipping to ROI) {requires: sp; sf; dplyr}
# create profiles {sp; dplyr}
profiles <- p3d.create.profiles(
  lon = lon,
  lat = lat,
  radius = radius,
  CRS = CRS,
  cnt = cnt, # alternatively, instead of cnt (count of profiles), deg (degrees between profiles) may be provided
  deg = deg # degrees may also be set as a vector of several values, specifying unique profile orientations
)
# create centerpoint and buffer {sf; sp}
centerpoint <- data.frame(lon = lon,lat = lat)
coordinates(centerpoint) <- ~lon+lat
centerpoint <- SpatialPoints(centerpoint, proj4string = CRS)
buffer <- sf::st_buffer(st_as_sf(centerpoint), 
                        #endCapStyle = "FLAT",
                        dist = radius 
)
# save profiles, centerpoint and buffer to disk as shapefiles {sf}
write_sf(buffer,
         dsn = paste0(location.name,'/',out.dir,'/','buffer','.gpkg'), 
         driver = "GPKG"
)
write_sf(st_as_sf(profiles),
         dsn = paste0(location.name,'/',out.dir,'/','profiles','.gpkg'), 
         driver = "GPKG"
)
write_sf(st_as_sf(centerpoint),
         dsn = paste0(location.name,'/',out.dir,'/','centerpoint','.gpkg'), 
         driver = "GPKG"
)





# 02 load maps {requires: sf}
# read original map
#map_1 <- read_sf(dsn = map_1.path.name[1], layer = map_1.path.name[2]) # OLD !!!
  assign(
    "map_1",
    read_sf(dsn = paste0(map_1.path.name[1], "/",map_1.path.name[2],".gpkg"), 
            layer = map_1.path.name[2])
    )
# clip map, rename used columns and strap unused columns
# if renaming and strapping should be skipped, set 3rd argument to NA
map_1 <- p3d.prepare.map(map_1, 
                         buffer, 
                         map1.infield.outfield)








# 03 clip and resample, raster
lidar <- p3d.prepare.dem(dem_1); rm(dem_1)
# save clipped dem_1 to disk
writeRaster(lidar, paste0(location.name,"/",out.dir,"/","lidar.tif"), overwrite = TRUE)

# create slope raster and stack it to lidar data {requires: raster}
# neighbours = 8 for rough surfaces, 4 for smoother surfaces
slope <- terrain(lidar, 
                 opt="slope", 
                 unit="degrees", 
                 neighbors=4 
) # provide filename = "" for direct output to disk
writeRaster(slope, paste0(location.name,"/",out.dir,"/","slope.tif"), overwrite = TRUE)

################################################################# test start
# get additional dem layers
if (parameters$dem.count > 1) {
  for (i in 2:parameters$dem.count) {
    assign(paste0("dem_",i),
           
           raster(paste0( 
             get(paste0("dem_",i,".path.name.extension"))[1],
             "/", 
             get(paste0("dem_",i,".path.name.extension"))[2],
             get(paste0("dem_",i,".path.name.extension"))[3])) %>%
             p3d.crop.mask.dem(buf = buffer, analysis.res = analysis.resolution) %>% 
              raster::resample(lidar, method = "ngb") %>% # produziert manchmal Fehler... checkup!
           #  resample.dem(analysis.resolution) %>%
              p3d.extend.raster.to.roi(buffer,analysis.resolution)
    )
  }
}

# stack all dems
lidar <- stack(c(lidar, slope)) # first dem and associated slope raster
if (parameters$dem.count > 1) { # all additional dems if provided
  for (i in 2:parameters$dem.count) {
    temp <- paste0("lidar <- stack(c(lidar, dem_",i,"))"); eval(parse(text = temp))
  }} else {}
################################################################## test end

#lidar <- stack(c(lidar, slope)) # if "test" is commented out, uncomment this single line!

# clean workspace
rm(slope)

# mask the dem stack, if desired
if (use.mask == T) {
  if (is.na(mask.field) == TRUE) { 
    # mask using all polygons
    masker <- map_1 # copy map to masker for compatibility with following script parts
    lidar <- mask(x = lidar, mask = map_1) 
  } else {
    # select desired mask polygons
    masker <- map_1[map_1@data[[mask.field]] == mask.polygon,]
    # mask dem
    lidar <- mask(x = lidar, 
                  mask = masker)
  }
}









# 04 load and project point shapes

if (parameters$points.count > 0) { # only attempt to project points, if a point shapefile reference is given

# get unique profile ids
p.ids <- profiles@data$id; p.ids

for (j in 1:length(p.ids)) {
  profile = profiles[profiles@data[["id"]] == p.ids[j],]

for (i in 1:parameters$points.count) {
  # get point shapefile and prepare it for referencing (clip, extract lidar values)
  assign(
    paste0("points_",i),
    get.points(get(paste0("points_",i,".path.name")),
               buffer,
               lidar[[1]],
               dem_1.path.name.extension)
  )
  
  # linear referencing
  if (length(get(paste0("points_",i))$geom) != 0) { # if there are features present, project them
    assign(
      paste0("projected_points_",i),
      linear.reference.points(get(paste0("points_",i)), 
                              profile)
    )
  } else { # if there are no features present, just copy the empty point object <- this part creates an error! check!!
    assign(
      paste0("projected_points_",i),
      get(paste0("points_",i)) # this should be as_Spatial(get(paste0("points_",i))), to be the same class for all. But does not work with empty object. Maybe it does not matter...
    )
  }
  # set a meaningful name to the added distance field and other columns
  temp <- get(paste0("projected_points_",i))
  names(temp)[[length(names(temp))]] <- "distance"
  names(temp)[names(temp) == "stationing"] <- "x"
  names(temp)[names(temp) == "distance"] <- "z"
  names(temp)[names(temp) == dem_1.path.name.extension[2]] <- "lidar"
  names(temp)[names(temp) == "nearest_line_id"] <- "RID"
  assign(paste0("projected_points_",i),
         temp
  ); rm(temp)
  
  # clean workspace -> not needed anymore, since wrapped in function
  rm(distance.field)
}
  
  # put all point files in one list
  point.object.names <- ls(pattern = "projected_points_")
  profile.point.data = lst(projected_points_1)
  if (parameters$points.count > 1) {
  for (k in 2:length(point.object.names)) {
    profile.point.data <- append(
      profile.point.data,
      get(paste0("projected_points_",k))
    )
  }
  }
  # clean workspace
  rm(list=ls(pattern = "projected_points_")); rm(point.object.names)
  # name the projected points list according to input files
  names(profile.point.data) = parameters$points.names
  # store final point data of one profile in an object named according to the profile id
  assign(paste0("point.data.","profile_",p.ids[j]),
         profile.point.data)
  # clean workspace
  rm(profile.point.data)
    
}

# put all profile data into one list
points.per.profile = lst(point.data.profile_001)

for (i in 2:length(p.ids)) {
points.per.profile <- append(
  points.per.profile,
  lst( get(paste0("point.data.profile_",p.ids[i])) )
)
}
names(points.per.profile) = p.ids # name list entries according to profile ids

# clean workspace
rm(list=ls(pattern="point.data.profile_"))

# save point data to disk
# save(points.per.profile, file = paste0(location.name,"/",out.dir, "/", "points.per.profile", ".Rdata") ) # that is done at the end of this script

}















# 05 create tem {requires: raster, dplyr}
tem <- st_as_sf(rasterToPoints(lidar, spatial = T)) %>% st_join(y = st_as_sf(map_1))
# extract map coordinates, so they dont get lost during projection
ctem <- as_Spatial(tem) %>% coordinates() # extract coords
Xmap <- ctem[,1] # convert to vectors
Ymap <- ctem[,2]
tem <- cbind(tem,Xmap,Ymap) # add map coords to tem
rm(list = c("ctem","Xmap","Ymap")) # cleanup workspace
# convert to SpatialPointsDataFrame
##### tem <- as_Spatial(tem)
# save tem to disk
save(list = c("tem"), file = paste0(location.name,"/",out.dir, "/", "tem", ".Rdata"))






# 06 plot overview map
png(filename=paste0(location.name,"/",out.dir,"/","quick.map",".png"),width=16,height=16,units="cm",res=300)
p3d.overview.map()
dev.off()








# 07 project tem on profiles

# get unique profile ids
p.ids <- profiles@data$id

# project tem on each individual profile
for (i in 1:length(p.ids)) {
  
  ###### loop time protocol
  loop.start.time <- Sys.time()
  
  # select a single profile
  p <- profiles[profiles@data[["id"]] == p.ids[i],]
  # clip tem to profile swath width, if desired
  if (use.swath == T) {
    swath <- st_buffer(st_as_sf(profiles[profiles@data$id == p.ids[i],]), 
                       endCapStyle = "FLAT",
                       dist = swath.width/2 
    )
    load(file = paste0(location.name,"/",out.dir,"/","tem",".Rdata"))
    tem <- crop(tem, as_Spatial(swath))
  }
  
# NEW  
  #### NEW parallel processing version ###########################################
  # project tem to profile
  # NEW TESTING
#  projected.data <- linear.reference.fem(st_as_sf(tem), st_as_sf(p))
  projected.data <- linear.reference.fem(tem, st_as_sf(p))
  # OLD WORKING 
  #projected.data <- linear.reference.points.multicore(tem, p, numCores)


  # a last column was added after projection, anmed geometry, which is not needed and deleted here:
  projected.data[,length(names(projected.data))] = NULL
    
  
  # set a meaningful name to the added distance field
#  names(projected.data)[length(names(projected.data))] <- "distance" #"z" # needs to be named z if plots should be created automatically!
  # and rename some fields for better handling with pmt
#  names(projected.data)[names(projected.data) == "nearest_line_id"] <- "RID"
  names(projected.data)[names(projected.data) == "stationing"] <- "location" #"x" # needs to be named x if plots should be created automatically!
#  names(projected.data)[1] <- "y" # first raster data ( = lidar) is set as y coordinate # needs to be named y if plots should be created automatically!
  # save the profile data as data.frames
  assign(paste0("p3d.profile.",p.ids[i]),
         as.data.frame(projected.data))
  # clean workspace
  rm(p)
  # rm(distance.field) # only inside function now, TBD
  
  # inform user
  cat(paste0("Profile number ",i," of ",cnt," done.\n"))
  
  ###### loop time protocol
  loop.end.time <- Sys.time()
  cat(paste0("Elapsed time: ",loop.end.time - loop.start.time,"\n"))
  
}
# clean workspace
rm(tem); rm(p.ids)

# put all projected data in one list
data.names <- ls(pattern = "p3d.profile.")
profile.data <- eval(parse(text = paste0("list('",sub("p3d.profile.","",data.names[1]),"' = ",data.names[1],")")))
for (i in 2:length(data.names)) {
  profile.data <- append(
    profile.data,
    eval(parse(text = paste0(
      "list('",
      sub("p3d.profile.","",data.names[i]),
      "' = ",
      data.names[i],
      ")"
    )))
  )
}
# save profile data to disk
save(list = c("profile.data","map_1","profiles","buffer","centerpoint","points.per.profile"), file = paste0(location.name,"/",out.dir,"/","profile.data.Rdata"))
# clean.workspace
rm(list = ls(pattern = "p3d.profile."))
rm(data.names)
rm(projected.data)






# Not needed anymore, since plotting is now done within pt2 script!
# # 08 prepare for profile illustrations
# # get profile extent
# extent <- pmt.extent(profile.data[["001"]])
# 
# # get available stratigraphic units (can be run, after projection is finished)
# units <- p3d.getAvailableUnits(profile.data)

