


pmt.measureVertical<-function(x1=NA,y1=NA,x2=NA,y2=NA,lwd=1,tcex=1,string = NA){
  # load coordinates, if given as arguments
  if (is.na(x1) == F) {
    l <- list(x = c(x1,x2), y = c(y1,y2) )
  }
  # click coordinates, if not provided
  if ( is.na(x1) == T ) {
    cat("> click 2 points in plot:\nand confirm with ESC")
    l <- locator()
    l <- list(x = round(l$x, digits = 1), y = round(l$y, digits = 1))
  }
  # calculate arrow y placement
  if (l$y[2] < l$y[1]) {
    yanno <- l$y[2] + (abs(l$y[1] - l$y[2]) * 0.05)
  } else {
    yanno <- l$y[2] - (abs(l$y[1] - l$y[2]) * 0.05)
  }
  
  # draw arrows
  arrows(l$x[1],yanno,l$x[2],yanno, lwd=lwd, angle = 20)
  arrows(l$x[2],yanno,l$x[1],yanno, lwd=lwd, angle = 20)
  segments(l$x[1],l$y[2],l$x[1],l$y[1], lwd=lwd)
  segments(l$x[2],l$y[2],l$x[2],l$y[1], lwd=lwd)
  
  # calculate and plot spacing
  
  if (is.na(string) == T) {
    cat("> please enter a text, or just type enter, to not add a text\n")
    string <- readLines(n = 1)
  }
  
  text(x = (max(l$x)-min(l$x))/2 + min(l$x), 
       y = yanno,
       string,
       pos = if(l$y[2] >= l$y[1]){3}else{1},
       offset = 0.3,
       cex = tcex*0.84
       )
  
  # prompt function call for repeating
  cat("pmt.measureVertical(x1=",l$x[1],",y1=",l$y[1],",x2=",l$x[2],",y2=",l$y[2],",lwd=",lwd,",tcex=",tcex,", string = '",string,"')\n")
  
  
}

#pmt.measureVSpacing(x1= 15515.7 ,y1= 801.4 ,x2= 29244.4 ,y2= 690.3 ,lwd= 1 ,tcex= 1 , string =  'aha' )


