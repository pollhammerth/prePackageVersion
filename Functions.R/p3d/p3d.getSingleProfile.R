# returns a single profiles data by supplying the profiles degree value,
# as indicated on the map

p3d.getSingleProfile <- function(profile.by.degree) {
  single.profile <- profile.data[[
    profiles[which(profiles@data$degrees == profile.by.degree),]@data$id
  ]]
  return(single.profile)
}


