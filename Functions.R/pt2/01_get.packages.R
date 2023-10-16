####################################################################################################################################
#### Get required packages #########################################################################################################
####################################################################################################################################
#### Content:
# get.packages()                                                # Laedt alle noetigen Packages [1]"require","install","library"



pmt.packages <-function(mode="require"){
    pkgs <- c("plyr", 
              "shape", 
              "sp", 
              "dplyr")

  if (mode=="require"){
    require(pkgs)
  } else if (mode=="library"){
    library(pkgs)
  } else if (mode=="install"){
    install.packages(pkgs)	
  } else {}
  
}