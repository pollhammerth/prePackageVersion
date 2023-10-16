#### 2. import DEM, clip to buffer bbox and mask by buffer
# pack the dem importing, clipping, masking, resampling routine in a function
load.dem <- function(p.n.e. = dem_1.path.name.extension, 
                     buf = buffer, 
                     additional.raster = FALSE, 
                     analysis.res = analysis.resolution,
                     lidar = dem_1) {
  
  # load dem
  dem <- raster(paste0(p.n.e.[1], "/", p.n.e.[2], p.n.e.[3]))
  
  # add more buffer to buffer, to make sure to cover full buffer area
  buffer.extended <- sf::st_buffer(buf, dist = xres(dem) + analysis.res)
  
  # clip dem to buffer bbox
  dem <- crop(x = dem, y = buffer.extended)
  
  # mask dem with buffer (i.e. reduce calculation to within buffer shape)
  dem <- mask(x = dem, mask = buffer.extended)
  
  # resample additional raster to match the resolution and origin of the first imported raster
  if (additional.raster == TRUE) {
    dem <- resample(dem, lidar, method = "ngb")
  } else {}
  
  # resample dem
  if (xres(dem) < analysis.res) {
    dem <- aggregate(x = dem, 
                     fact = analysis.res/xres(dem), 
                     method = "median"
    )
  } else if (xres(dem) > analysis.res) {
    dem <- disaggregate(x = dem,
                        fact = xres(dem)/analysis.res
    )
  } else {
    dem <- dem
  }
  
  
  # extend rasters to cover full buffer bbox
  buffer.extended.2 <- sf::st_buffer(buf, dist = analysis.res)
  dem <- extend(dem, buffer.extended.2)
  dem <- mask(dem, buffer.extended.2)
  
  # output dem
  return(dem)
}
