#####################################################################################################################################
#### Import data ####################################################################################################################
#####################################################################################################################################
#### Content:
# import.data() - output: td_* Dataframes, td, terrace, x.limit, y.limit, z.limit
# make.spatial() - makes terrace objects spatial - requires: td

# OLD VERSION: takes only one raster layer
import.data.old.version <- function(file = "data.csv", x = "location", y = "lidar", z = "distance", attribute.field = "map_1.name_short") {
  # read data
  data <- read.table(file)
  # rename cols for compatibility with PMT 1
  names(data)[names(data) == y] <- "y"
  names(data)[names(data) == x] <- "x"
  names(data)[names(data) == z] <- "z"
  # get stratigraphic units
  terrace <- unique(data[[attribute.field]])
  terrace <- terrace[!is.na(terrace) == TRUE]
  # create terrace unit object names
  td <- NULL
  for (i in 1:length(terrace)) {
    td <- c(td, paste0("td_", i, ".", terrace[i]))
  }
  # write terrace objects. one per stratigraphic unit.
  for (i in 1:length(terrace)) {
    eval(parse(text = paste0(td[i]," <<- subset(data, data$", attribute.field, " == '", terrace[i], "')" )))
  }
  # output to global environment
  lidar <<- data
  td <<- td
  terrace <<- terrace
  # additionally do the plot dimensioning now
  x.limit <<- c(min(data$x), max(data$x))
  y.limit <<- c(min(data$y), max(data$y))
  z.limit <<- c(min(data$z), max(data$z))
  
  # protocol
  print("the function created 'td' and 'terrace' objects")
  return(td)
}



# NEW VERSION: imports data and splits stratigraphic units for one or multiple raster layers
import.data <- function(file = "data.csv", x = "location", y = "lidar", z = "distance", attribute.field = "map_1.name_short") {
  # read data
  data <- read.table(file)
  # get stratigraphic units
  terrace <- unique(data[[attribute.field]])
  terrace <- terrace[!is.na(terrace) == TRUE]
  # create td vector to store output object names
  td <- NULL
  # create terrace unit object names
  for (j in 1:length(y)) {
    for (i in 1:length(terrace)) {
      eval(parse(text = paste0("td <- c(td , '", y[j], "_", i, ".", terrace[i], "')" )))
    }
  }
  # rename cols for compatibility with PMT 1
  names(data)[names(data) == x] <- "x"
  names(data)[names(data) == z] <- "z"
  # write terrace objects. one per stratigraphic unit.
  for (i in 1:length(td)) {
    for (j in 1:length(y)) {
      eval(parse(text = paste0(td[i]," <- subset(data, data$", attribute.field, " == '", rep(terrace, length(y))[i], "')" )))
    }
  }
  for (j in 1:length(y)) {
      for (i in (length(terrace)*(j-1)+1):(length(terrace)*j)) {
        eval(parse(text = paste0("names(", td[i], ")[names(", td[i], ") == '", y[j], "'] <- 'y'" )))
        eval(parse(text = paste0(td[i]," <<- ", td[i])))
    }
  }
  # output to global environment
  eval(parse(text = paste0("names(data)[names(data) == '", y[1], "'] <- 'y'")))
  eval(parse(text = paste0(y[1], " <<- data")))
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

