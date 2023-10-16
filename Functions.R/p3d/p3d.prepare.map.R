# {requires: sf}
# Function for preparing a newly imported map (polygons) by:
# clipping to buffer, renaming used columns and strapping unused columns

p3d.prepare.map <- function(map = map_1, buffer = buffer, infield.outfield = NA){
  # clip map
  map <- st_intersection(map, buffer)
  # convert to S4 class
  map <- as_Spatial(map)
  
  if (is.na(infield.outfield[1]) == FALSE) {
    
  # indexing map1.infields.outfields
  # index all outfields
  out.ids <- seq(1, by = 2, len = length(infield.outfield)/2) + 1
  # index all infields
  in.ids <- seq(1, by = 2, len = length(infield.outfield)/2)
  
  # rename @data columns
  for (i in 1:length(in.ids)) {
    names(map@data)[names(map@data) == infield.outfield[in.ids[i]]] <- infield.outfield[out.ids[i]]
  }
  
  # strap unused @data columns
  map <- map[names(map@data) %in% infield.outfield[out.ids]]
  
  }
  
  # function output
  return(map)
  
}

