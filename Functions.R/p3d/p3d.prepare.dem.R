# prepare lidar data for p3d {requires: raster; sp}
# resamples to analysis resolution
# clips and masks raster to fit the exact calculation area size
p3d.prepare.dem <- function(dem = dem_1, 
                            buf = buffer, 
                            analysis.res = analysis.resolution
) {
  # add more buffer to buffer, to make sure to cover full buffer area
  buffer.extended <- sf::st_buffer(buf, dist = xres(dem) + analysis.res)
  # clip dem to buffer bbox
  dem <- crop(x = dem, y = buffer.extended)
  # mask dem with buffer (i.e. reduce calculation to within buffer shape)
  dem <- mask(x = dem, mask = buffer.extended)
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
  # extend raster to cover full buffer bbox
  buffer.extended.2 <- sf::st_buffer(buf, dist = analysis.res)
  dem <- extend(dem, buffer.extended.2)
  dem <- mask(dem, buffer.extended.2)
  # function output
  return(dem)
}


p3d.extend.raster.to.roi <- function(dem, buf,analysis.res) {
  # extend rasters to cover full buffer bbox
  # needed for rasters, whose area does not cover the whole roi (region of interest)
  buffer.extended.2 <- sf::st_buffer(buf, dist = analysis.res)
  dem <- raster::extend(dem, buffer.extended.2) %>% raster::mask(buffer.extended.2)
  # output dem
  return(dem)
}




p3d.crop.mask.dem <- function(dem = dem_1, buf = buffer, analysis.res = analysis.resolution) {
  # add more buffer to buffer, to make sure to cover full buffer area
  buffer.extended <- sf::st_buffer(buf, dist = raster::xres(dem) + analysis.res)
  # clip to buffer bbox and mask with buffer geometry
  dem <- raster::crop(x = dem, y = buffer.extended) %>% raster::mask(mask = buffer.extended)
  # function output
  return(dem) 
}

