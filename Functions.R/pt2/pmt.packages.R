####################################################################################################################################
#### Get required packages #########################################################################################################
####################################################################################################################################

pmt.packages <-function(){

    require("plyr")
    require("dplyr") # pipe operator %>%
    require("sp")
    require("shape") # drawing arrows
    require("stringr") # used in FLAGG specific plot functions (e.g. plotGraf09())
    require("sm") # for pmt.kernelReg()
    
}