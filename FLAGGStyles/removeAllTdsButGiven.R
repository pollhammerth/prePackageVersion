

removeAllTdsButGiven <- function(terrace) {
"%!in%" <- Negate("%in%")
tds.remove <- tdsNames[[1]];for (i in 2:length(tdsNames)) {tds.remove <- c(tds.remove, tdsNames[[i]])} # remove all
tds.remove <- tds.remove[which(tds.remove %!in% terrace)] # add Ifang
return(tds.remove)
}

