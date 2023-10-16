####################################################################################################################################
#### Funktion zur Erstellung eines leeren Plots ####################################################################################
####################################################################################################################################

#### Content:
# empty.plot()                 # [1]main,[2:5]xy,[6]locate,[7]ylab,[8]xlab,[9]sub,[10]cex,[11]grid,[12]bg,[13]grid.col

#### Benoetigt:
# x.limit, y.limit             # Wird durch plot.dimensioning() oder import.terraces() erstellt


empty.plot<-function(main="first argument",
                     y.min=y.limit[1],
                     y.max=y.limit[2],
                     x.min=x.limit[1],
                     x.max=x.limit[2],
                     locate=FALSE,
                     ylab="elevation",
                     xlab="distance along profile",
                     sub=NULL,
                     cex=2,
                     grid=TRUE,
                     bg="white",
                     grid.col="grey70",
                     grid.wd = 1,
                     clipboard.windows=FALSE){
  print("empty.plot(): Passt die Achsenausdehnung an xyz.daten an. Alternativ kann ein Zoom eingestellt werden.")
  if (locate==TRUE){
    b<-data.frame(locator())
    x.min<-min(b[1])
    x.max<-max(b[1])
    y.min<-min(b[2])
    y.max<-max(b[2])
    xy.vector<-c(y.min,y.max,x.min,x.max)
    print("Ausdehnung des gezoomten empty.plots:")
    string=paste0("empty.plot('Zoomed plot',y.min=",round(xy.vector[1]),
    ",y.max=",round(xy.vector[2]),
    ",x.min=",round(xy.vector[3]),
    ",x.max=",round(xy.vector[4]),")") 
    #collapse=", ");
    print(string)
    if (clipboard.windows==TRUE){
      writeClipboard(print(string))
    } else {}
  } else {}
  
  plot(NULL, NULL, 
       xlim=c(x.min,x.max), 
       ylim=c(y.min,y.max), 
       ylab=ylab, 
       xlab=xlab, 
       main=main,
       sub=sub,       
       cex.axis=cex,
       cex.main=cex,
       cex.lab=cex,
       cex.sub=cex)
  
  # Plot Hintergrund [6]<-grid=TRUE/FALSE
  if (grid==TRUE){
    rect(-10000,-500,1000000,10000,col=bg,border=NA)
    for (i in seq(-500,4000,10)){
      abline(NULL,NULL,i,NULL, col=grid.col, lwd=grid.wd*0.25, lty=4)
    }
    for (i in seq(-500,4000,50)){
      abline(NULL,NULL,i,NULL, col=grid.col, lwd=grid.wd*0.3)
    }
    for (i in seq(-500,4000,100)){
      abline(NULL,NULL,i,NULL, col=grid.col, lwd=grid.wd*0.45)
    }
    for (i in seq(-5000,1000000,1000)){
      abline(NULL,NULL,NULL,i, col=grid.col, lwd=grid.wd*0.25, lty=4)
    }
    for (i in seq(-5000,1000000,5000)){
      abline(NULL,NULL,NULL,i, col=grid.col, lwd=grid.wd*0.45)
    }
    
  } else {}
}

