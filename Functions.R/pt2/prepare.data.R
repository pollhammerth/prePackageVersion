

# required packages: 

# prepares data, splits stratigraphic units for one or multiple raster layers
prepare.data <- function(data = data, x = x, y = y[1], z = z, field = field) {
  
  # get unit names function (exclude NA)
  unit.names <- function(data, fields) {
    unique(data[[fields]])[is.na(unique(data[[fields]])) == FALSE]
  }
  
  # store unit names in terrace object
  terrace <- paste0(field[1], ".", unit.names(data, field[1]))
  if (length(field) > 1) {
    for (i in 2:length(field)) {
      terrace <- c(terrace, paste0(field[i], ".", unit.names(data, field[i])))
    }} else {}
  # create td vector to store output object names
  td <- paste0(y[1], ".", terrace)
  if (length(y) > 1) {
    for (i in 2:length(y)) {
      td <- c(td, paste0(y[i], ".", terrace))
    }} else {}


  # rename cols for compatibility with PMT 1
  names(data)[names(data) == x] <- "x"
  names(data)[names(data) == z] <- "z"

  # create subsets for each raster and unit (works without terrace and td objects now)
  for (i in 1:length(y)) {
    for (j in 1:length(field)) {
      for (k in 1:length(unit.names(data, field[j]))) {
        # write object
        eval(parse(text = paste0(y[i], ".", field[j], ".", unit.names(data, field[j])[k],
                                 " <<- subset(data, data$", field[j], " == '", unit.names(data, field[j])[k], "')")))
        # rename defined y column to "y" in created object
        eval(parse(text = paste0("names(", y[i], ".", field[j], ".", unit.names(data, field[j])[k],
                                 ")[names(",y[i], ".", field[j], ".", unit.names(data, field[j])[k], ") == '", y[i], "'] <<- 'y'")))
      }}}

  # output full DEM to global environment (regards first y argument)
  eval(parse(text = paste0("names(data)[names(data) == '", y[1], "'] <- 'y'")))
  eval(parse(text = paste0(y[1], " <<- data")))
  # and output additional objects
  td <<- td
  terrace <<- terrace
  # set optimised plot extent regarding first y argument (e.g. lidar)
  x.limit <<- c(min(data$x), max(data$x))
  y.limit <<- c(min(data$y), max(data$y))
  z.limit <<- c(min(data$z), max(data$z))
  
  # protocol
  print("the function created 'td' and 'terrace' objects")
  return(terrace)
}

