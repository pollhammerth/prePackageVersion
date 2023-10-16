# this function creates a data frame containing a polygon from terrace max and min values, calculated with pmt.bin()
# supply two dataframes: top = terrace data, with lidar
# base = terrace data with base raster
# the remaining params are passed to pmt.bin()
# depends on pmt::pmt.bin()


pmt.polygon <- function(top = h, base = l, ...) {

# get max vales per terrace from first given elevation column
#b1 <- top %>% pmt.bin(interval = int, value = "max", mode = mod, cth = fringeth, sth = NA)
b1 <- top %>% pmt.bin(value = "max", ...)

# get min values from second given elevation column
#b2 <- base %>% pmt.bin(interval = int, value = "min", mode = mod, cth = fringeth, sth = NA)
b2 <- base %>% pmt.bin(value = "min", ...)

# combine to one df and sort by ascending x values
bc <- rbind(b1, b2)
bc <- bc[order(bc$x),]

# define function for filtering rows with max or min y values per x location
bf <- function(i, mode, b) {
  df <- b[b$x == unique(b$x)[i],]
  if (mode == "max") {
    return( df[df$y == max(df[['y']]),] )
  } else if (mode == "min") {
    return( df[df$y == min(df[['y']]),] )
  }
}

# execute function, creating a df with max y values per x location
bmax <- bf(1,"max",bc)
for ( i in 2:length(unique(bc$x)) ) {
  bmax <- rbind(bmax, bf(i,"max",bc))
}

# execute function, creating a df with min y values per x location
# and sort it with decreasing x values
bmin <- bf(1,"min",bc)
for ( i in 2:length(unique(bc$x)) ) {
  bmin <- rbind(bmin, bf(i,"min",bc))
}
bmin <- bmin[order(bmin$x, decreasing = TRUE),]

# combine the sorted dataframes and copy the first row as last row
bpoly <- rbind(bmax, bmin)
bpoly <- rbind(bpoly, bpoly[1,])

# function output, to be used for plotting a polygon
return(bpoly)

}


#### example usage

# prepare input data
# y.source <- parameters$dem.names[[1]]; h <- eval(ds$hds)
# y.source <- parameters$dem.names[[4]]; l <- eval(ds$hds)

# execute function
# bpoly <- pmt.polygon(top = h, base = l, mod = 'bin', int = 500, fringeth = 5)

# plot function output
# polygon(bpoly$x,bpoly$y, col = paste0(styles$hds$col,"66"), border = styles$hds$col)
# points(bpoly$x, bpoly$y, type = "l")


