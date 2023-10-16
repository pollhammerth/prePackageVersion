
# R Package creation
# this is based on "https://ourcodingclub.github.io/tutorials/writing-r-package/"

# 1
# install packages for package creation
install.packages("devtools") # tools for package development
install.packages("roxygen2") # documetation writing

# 2
# create folder, that will hold the package
setwd("H:/PMT3")
dir.create("MAMOT_R_Package") # holds all package and documentation files
dir.create("MAMOT_R_package/R") # in here all packageFunction.Rs go

# dummy function for test package (save as "flump.R" in "R/")
flump = function(x){
  cat("what the **** do you want me to do with that?\n")
  cat("what is that anyway? take it back!\n")
  return(x)
}

# this test package can already be loaded like this
setwd("H:/PMT3/MAMOT_R_package")
library(devtools)
load_all(".")

# 3
# start a new R project in RStudio
# menu: "new project" -> "in existing directory" -> choose ./MAMMOT_R_package/

# 4
# add help files (documentation)
# these can later be called like this
help(lm)
?lm
# such "markdown" files can be created with roxygen2 (http://kbroman.org/pkg_primer/pages/docs.html)
# the flump.R file needs to follow a certain structure and use special tags for roxygen2:
#### flump.R start
#' Dummy function
#'
#' Promt some shit and return the input provided
#' @param x Any object or numeric or character or whatever
#' @return The very object, that is provided to the function
#' @examples 
#' temp1 <- flump(50);
#' temp2 <- flump( c(50, 63, 23) );
#' @export
flump = function(x){
  cat("what the **** do you want me to do with that?\n")
  cat("what is that anyway? take it back!\n")
  return(x)
}
#### flump.R end
# now create the markdown (make sure the wd is set to package folder MAMOT_R_package/)
library(roxygen2)
roxygenise()
# test the documentation
load_all(".")
?flump

# 5
# put on GitHub for easy package installing and then loading via library() or require()
# to be able to put files/notes or whatever on the repository, that should not be part of the package,
# add a foldere.g. ./notInPackageNotes/
# and add this last line to .Rbuildignore:
####
# ^.*\.Rproj$
# ^\.Rproj\.user$
# notInPackageNotes*
####
# everything starting with notInPackageNotes will be ignored (so everything in that folder)
# after uploading the files on GitHub, install the package like this:
library(devtools)
install_github("pollhammerth/MAMOT_R_Package")
library(MAMOT)
?flump
flump("take this")

