#### 2. import DEM, clip to buffer bbox and mask by buffer
# pack the dem importing, clipping, masking, resampling routine in several helper functions

load.dem <- function(dem.path.name.extension) {
  raster(paste0(dem.path.name.extension[1], 
                "/", 
                dem.path.name.extension[2], 
                dem.path.name.extension[3]))
}




crop.mask.dem <- function(dem = dem_1, buf = buffer, analysis.res = analysis.resolution) {
  # add more buffer to buffer, to make sure to cover full buffer area
  buffer.extended <- sf::st_buffer(buf, dist = raster::xres(dem) + analysis.res)
  # clip to buffer bbox and mask with buffer geometry
  dem <- raster::crop(x = dem, y = buffer.extended) %>% raster::mask(mask = buffer.extended)
  # function output
  return(dem) 
}



resample.dem <- function(dem, analysis.res) {
  # downsample
  if (raster::xres(dem) < analysis.res) {
    dem <- aggregate(x = dem, 
                     fact = analysis.res/raster::xres(dem), 
                     method = "median"
    )
    # or upsample
  } else if (raster::xres(dem) > analysis.res) {
    dem <- disaggregate(x = dem,
                        fact = raster::xres(dem)/analysis.res
    )
    # or leave as is
  } else {}
  return(dem)
}



extend.raster.to.roi <- function(dem, buf,analysis.res) {
  # extend rasters to cover full buffer bbox
  # needed for rasters, whose area does not cover the whole roi (region of interest)
  buffer.extended.2 <- sf::st_buffer(buf, dist = analysis.res)
  dem <- raster::extend(dem, buffer.extended.2) %>% raster::mask(buffer.extended.2)
  # output dem
  return(dem)
}

