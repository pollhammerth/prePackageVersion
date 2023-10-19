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
for (i in 1:length(packages)) { install.packages(packages[i]) }

# require all packages
for (i in 1:length(packages)) { eval(parse(text = paste0("require('",packages[i],"')") )) }


# these packages are not available for my R version!
renv::install("rgdal")
renv::install("maptools")
renv::install("rgeos")
utils::install.packages("rgdal")
utils::install.packages("maptools")
utils::install.packages("rgeos")
# all these packages have been removed from cran. Suggested alternatives are sf and terra


require(sessioninfo)
sessionInfo()



