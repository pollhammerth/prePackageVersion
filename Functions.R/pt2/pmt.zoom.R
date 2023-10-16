# set a new plot extent by clicking two points into existing profileview
# save output as "extent". This will automatically be used by pmt.plot()

pmt.zoom <- function(){
  cat("> click two points in the plot and press esc.")
  e <- locator()
  e <- list(xlim = c(min(e$x),max(e$x)), ylim = c(min(e$y),max(e$y)), zlim = c(extent$zlim[1],extent$zlim[2]))
  return(e)
}
