

# linear referencing serial processing version
linear.reference.fem <- function(points, profile = profile) {

x <- s2_project(x = profile, y = points) # 7 s for 50m (~ 0.5 MPx) / 53 s for 20m (~ 3 MPx) / 4 min for 10m (~ 12 MPx)
z <- s2_distance(profile, points)


# add distance field (side output from calculateStationing())
points.projected <- cbind(points, as.vector(x), as.vector(z))

names(points.projected)[names(points.projected) == "as.vector.x."] <- "stationing"
names(points.projected)[names(points.projected) == "as.vector.z."] <- "distance"

# return function output
return(points.projected) # es scheint auch ohne diese Zeile funktioniert zu haben!?


}

