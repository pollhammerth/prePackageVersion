




pmt.measureSpacing<-function(x1=NA,y1=NA,x2=NA,y2=NA,x3=NA,y3=NA,lwd=1,tcex=1){
  
  if (is.na(x1) == F) {
      l <- list(x = c(x1,x2,x3), y = c(y1,y2,y3) )
  }
  
  if ( is.na(x1) == T ) {
    cat("> click 3 points in plot: \n> 1 = arrow tip\n> 2 = annotation y position\n> 3 = anno. offset to the right\n> or one point, to not draw an arrow\n\n")
    l <- locator()
    l <- list(x = round(l$x, digits = 1), y = round(l$y, digits = 1))
  }
  
  # draw arrows - alternative placing
  # arrows(l$x[1],l$y[1],l$x[2],l$y[2], lwd=lwd, angle = 20)
  # arrows(l$x[2],l$y[2],l$x[1],l$y[1], lwd=lwd, angle = 20)
  # segments(l$x[2],l$y[2],l$x[3],l$y[2], lwd=lwd)

  # draw arrows
  arrows(l$x[3],l$y[1],l$x[3],l$y[2], lwd=lwd, angle = 20)
  arrows(l$x[3],l$y[2],l$x[3],l$y[1], lwd=lwd, angle = 20)
  segments(l$x[2],l$y[2],l$x[3],l$y[2], lwd=lwd)
  segments(l$x[1],l$y[1],l$x[3],l$y[1], lwd=lwd)
  
  # calculate and plot spacing
  y.spacing<-c(l$y[1],l$y[2])
  string<-as.character(round(max(y.spacing) - min(y.spacing), digits=1))
  text(l$x[3],l$y[2],
    paste0(string," m"),
    pos=if(l$x[3] >= l$x[2]){4}else{2},
    offset=1,cex=tcex*0.84)
  
  # prompt function call for repeating
  cat("pmt.measureSpacing(x1=",l$x[1],",y1=",l$y[1],",x2=",l$x[2],",y2=",l$y[2],",x3=",l$x[3],",y3=",l$y[3],",lwd=",lwd,",tcex=",tcex,")\n")
  
  
}




