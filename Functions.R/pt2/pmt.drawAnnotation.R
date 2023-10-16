

pmt.drawAnnotation<-function(x1=NA,y1=NA,x2=NA,y2=NA,x3=NA,y3=NA,lwd=1,string = NA,tcex=1){

  if (is.na(x1) == F) {
    if (is.na(x2) == F) {
        l <- list(x = c(x1,x2,x3), y = c(y1,y2,y3) )
      } else {
        l <- list(x = c(x1), y = c(y1))
      }
    }

  if ( is.na(x1) == T ) {
    cat("> click 3 points in plot: \n> 1 = arrow tip\n> 2 = annotation y position\n> 3 = anno. offset to the right\n> or one point, to not draw an arrow\n\n")
    l <- locator()
    l <- list(x = round(l$x, digits = 0), y = round(l$y, digits = 0))
  }

  # draw an arrow, if 3 points are given
  if (isTRUE(length(l$x) >= 3) == TRUE) {
    arrows(l$x[2],l$y[2],l$x[1],l$y[1], lwd=lwd, angle = 20)
    segments(l$x[2],l$y[2],l$x[3],l$y[2], lwd=lwd)
  }
  
  # annotate
  if (is.na(string) == T) {
    cat("> please enter a text, or just type enter, to not add a text\n")
    string <- readLines(n = 1)
  }
  text(if(length(l$x)==1){l$x[1]}else{l$x[3]},
       if(length(l$y)==1){l$y[1]}else{l$y[2]},
       string,
       pos=if(isTRUE(l$x[3] >= l$x[2]) == T){4}else{2}, # 4
       offset=1,
       cex=tcex*0.84)
  
  # prompt function call to repeat the function
  string<-paste0("pmt.drawAnnotation(x1=",l$x[1],",y1=",l$y[1],",x2=",l$x[2],",y2=",l$y[2],",x3=",l$x[3],",y3=",l$y[3],",string='",string,"',tcex=",tcex,",lwd=",lwd,")")
  cat(string)
}



#if (isTRUE(length(l$x) >= 3) == TRUE) {cat("aso")}


