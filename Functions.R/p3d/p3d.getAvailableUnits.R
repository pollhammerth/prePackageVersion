# get available units (can be run, after profection is finished)
p3d.getAvailableUnits <- function(profile.data, single.profile = F) {
  # get all individual units from every profile
  for (i in 1:length(profile.data)) {
    if (single.profile == T) {
      units <- unique(
        profile.data[names(profile.data) == map1.infield.outfield[2]]
        )[,1]
    } else if (single.profile == F) {
      if (i == 1) {
        units <- unique(
          profile.data[[i]][names(profile.data[[i]]) == map1.infield.outfield[2]]
          )[,1]
      } else {
        units <- c(units,
                   unique(
                     profile.data[[i]][names(profile.data[[i]]) == map1.infield.outfield[2]]
                     )[,1]
                   )
      }
    }
    # only keep unique units
    units <- unique(units)
  }
  # remove NAs
  units <- units[which(!is.na(units) == TRUE)]
  # function output
  return(units)
}