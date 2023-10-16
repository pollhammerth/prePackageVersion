# needed packages for PMT3
packages = c(
  "terra",
  "data.table",
  "rgdal",
  "maptools",
  "rgeos",
  "raster",
  "stringr",
  "shape",
  "sp",
  "dplyr",
  "plyr",
  "sf",
  "readtext",
  "doParallel",
  "foreach",
  "iterators",
  "s2"
)

# install all packages
install.packages(packages)

# require all packages
for (i in 1:length(packages)) { eval(parse(text = paste0("require('",packages[i],"')") )) }

