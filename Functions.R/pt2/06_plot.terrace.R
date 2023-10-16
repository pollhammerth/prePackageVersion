####################################################################################################################################
#### Funktionen zum Plotten anzugebender Daten #####################################################################################
####################################################################################################################################

#### Content:
# plot.terrace()                            # Datenplot: [1]<-datatype,[2]<-i,[3]<-y,[4]<-col,[5]<-type,[6]<-cex,[7]<-pch,[8]<-x,[9]<-ppt
# plot.terrace.greyscale()                  # Erzeugt Graustufen entlang z-Achse


# NEW VERSION
plot.terrace<-function(data=lidar,y="y",col="black",type="p",cex=0.3,pch=16,lwd=1,x="x",range=NULL, single = NULL){
  if (!is.null(range) == TRUE) {
    data <- subset(data, z < max(range))
    data <- subset(data, z > min(range))
  } else {}
  if (!is.null(single) == TRUE) {
    data <- subset(data,  eval(parse(text = paste0(single[1]))) == single[2])
  } else {}
if (mode(data) == "list") {
  points(data$x, eval(parse(text = paste0("data$", y))), type = type, pch = pch, cex = cex, col = col, lwd = lwd)
} else if (mode(data) == "S4") {
  points(data, type = type, pch = pch, cex = cex, col = col, lwd = lwd)
} else {print("no supported object type. please supply list or S4 object")}
}


#### plot.terrace.greyscale() Erzeugt Graustufen entlang z-Achse - NEW VERSION
plot.terrace.greyscale <- function(data="lidar",y="y",cex=0.3,pch=16,lwd=1,x="x",range=NULL, single = NULL){
  if (!is.null(range) == TRUE) {
    data <- subset(data, z < max(range))
    data <- subset(data, z > min(range))
  } else {}
  if (!is.null(single) == TRUE) {
    data <- subset(data,  eval(parse(text = paste0(single[1]))) == single[2])
  } else {}
  
  stufen<-100
  z.range<-z.limit[2]-z.limit[1]
  step.depth<-z.range/stufen
  z.intervals<-z.limit[1]
  for (j in 1:stufen){
    z.intervals<-c(z.intervals,z.intervals[length(z.intervals)]+step.depth)
  }
  for (k in 1:length(z.intervals)-1){
    temp<-paste0('plot.terrace(data,y,col="grey',100-k,'","p",cex,pch,lwd,x,range=c(z.intervals[101-k],z.intervals[101-k-1]))')
    eval(parse(text=temp))
  }
}
