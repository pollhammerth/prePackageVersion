####################################################################################################################################
#### Funktion zur Erstellung eines leeren Plots ####################################################################################
####################################################################################################################################

#### Content:
# empty.plot()                 # [1]main,[2:5]xy,[6]locate,[7]ylab,[8]xlab,[9]sub,[10]cex,[11]grid,[12]bg,[13]grid.col

#### Benoetigt:
# x.limit, y.limit             # Wird durch plot.dimensioning() oder import.terraces() erstellt





pmt.empty <- function (ext = extent, 
                       main="Plot Multiple Terraces (pmt)",
                       textcex = 1,
                       zoom = F,
                       ylab="elevation [m]",
                       xlab="distance along profile [m]",
                       sub=NULL,
                       grid=F,
                       bg.col=NA,
                       grid.col="grey80",
                       grid.wd = 1,
                       mirx=F
                       , ...) {
  
  # adjust label positioning
  a <- textcex/1.75+1
  par(mgp=c(1.5*a,0.5*a,0),mar=c(4,3,2,1)+0.1)
  
  # interactively set a zoomed plot extent, by clicking two points in existing plot
  if (zoom == T){
    b <- round(data.frame(locator()),1)
    ylim <- b[["y"]]
    xlim <- b[["x"]]
    cat("> Recall zoom by:\npmt.empty(main='zoomed plot',\nylim = c(",
        paste0(ylim, collapse = ", "),
        "), xlim = c(",
        paste0(xlim, collapse = ", "),
        ")\n)")
  } else {}
  
  # create empty plot
  plot(NULL, NULL,
       xlim=if(mirx){xlim=rev(range(extent[["xlim"]]))}else{extent[["xlim"]]}, 
       ylim=extent[["ylim"]], 
       main=main,
       ylab=ylab, 
       xlab=xlab, 
       sub=sub,       
       cex.main=textcex*1.4*0.7,
       cex.lab=textcex*0.7,
       cex.axis=textcex*0.7,
       cex.sub=textcex*0.7*0.7,
       type = "n",
       ...)
  
  # create grid
  if (is.na(bg.col) == F) {
    rect(-10000,-500,1000000,10000, col = bg.col, border = NA)
  }
  if (grid == T) {
    
    abline(h = seq(-500,4000,10), col=grid.col, lwd=grid.wd*0.4, lty=3)
    abline(h = seq(-500,4000,50), col=grid.col, lwd=grid.wd*0.4)
    abline(h = seq(-500,4000,100), col=grid.col, lwd=grid.wd*0.7)
    
    abline(v = seq(-1000,1000000,1000), col=grid.col, lwd=grid.wd*0.4, lty=3)
    abline(v = seq(-5000,1000000,5000), col=grid.col, lwd=grid.wd*0.7)
    
  } else {}
}
#pmt.empty(grid = T, textcex = 3)

