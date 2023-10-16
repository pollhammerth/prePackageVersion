
get.points <- function(points.path.name, buffer = buffer, dem = dem_1, dem.path.name.extension = dem_1.path.name.extension){

# load point shapefile
points <- sf::st_read(dsn = paste0(points.path.name[1],"/",points.path.name[2],".gpkg"),
                        layer = points.path.name[2]
)

# clip shapefile to buffer
points <- st_intersection(points, buffer)

if (length(points$geom) != 0) {
# extract lidar values to points
points.dem <- raster::extract(dem,
                                points,
                                buffer = 0,
                                fun=mean,
                                df=TRUE)
points <- cbind(points, points.dem)

# rename coordinate columns
names(points)[names(points) == dem.path.name.extension[2]] <- "lidar"
#names(points)[names(points) == "stationing"] <- "x"
#names(points)[names(points) == "distance"] <- "z"
} else {"no points within search radius"}

# function output
return(points)

}





