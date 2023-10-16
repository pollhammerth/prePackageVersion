####################################################################################################################################
#### Funktionen zum Plotten anzugebender Daten #####################################################################################
####################################################################################################################################

#### Content:
# plot.terrace()                            # Datenplot: [1]<-datatype,[2]<-i,[3]<-y,[4]<-col,[5]<-type,[6]<-cex,[7]<-pch,[8]<-x,[9]<-ppt
# plot.terrace.greyscale()                  # Erzeugt Graustufen entlang z-Achse

# Remark: for OSX use pch=16, for Windows use pch=46, since 16 cannot be scaled down enough for high resolutions.


# NEW VERSION
pmt.plot <- function(data,y="y",col="black",type="p",cex=1,pch=46,lwd=1,lty=1,x="x",range=NULL, single = NULL,add = T,...){
  cexfact <- 0.1
  if (add == F) {
    pmt.empty(...)  
  }
  
  if (!is.null(range) == TRUE) {
    data <- subset(data, z < max(range))
    data <- subset(data, z > min(range))
  } else {}
  if (!is.null(single) == TRUE) {
    data <- subset(data,  eval(parse(text = paste0(single[1]))) == single[2])
  } else {}
if (mode(data) == "list") {
  points(data[[x]], data[[y]], type = type, pch = pch, cex = cexfact*cex, col = col, lwd = lwd, lty = lty)
} else if (mode(data) == "S4") {
  points(data, type = type, pch = pch, cex = cexfact*cex, col = col, lwd = lwd, lty = lty)
} else {print("no supported object type. please supply list or S4 object")}
}


#### plot.terrace.greyscale() Erzeugt Graustufen entlang z-Achse - NEW VERSION
pmt.plotGreyscale <- function(data,y="y",cex=1,pch=46,lwd=1,x="x",range=NULL, single = NULL, add = T,z = "z",...){
  cexfact <- 0.1
  if (add == F) {
    pmt.empty(...)  
  }
  
  if (!is.null(range) == TRUE) {
    data <- subset(data, z < max(range))
    data <- subset(data, z > min(range))
  } else {}
  if (!is.null(single) == TRUE) {
    data <- subset(data,  eval(parse(text = paste0(single[1]))) == single[2])
  } else {}
  
  stufen<-100
  z.limit <- c(min(data[[z]]),max(data[[z]]))
  z.range<-z.limit[2]-z.limit[1]
  step.depth<-z.range/stufen
  z.intervals<-z.limit[1]
  for (j in 1:stufen){
    z.intervals<-c(z.intervals,z.intervals[length(z.intervals)]+step.depth)
  }
  for (k in 1:length(z.intervals)-1){
    pmt.plot(data,y,col = paste0("grey",100-k), "p", cex, pch, lwd, 1, x, range = c(z.intervals[101-k],z.intervals[101-k-1]), add = T)
    #OLD# temp<-paste0('plot.terrace(data,y,col="grey',100-k,'","p",cexfact*cex,pch,lwd,x,range=c(z.intervals[101-k],z.intervals[101-k-1]))')
    #OLD# eval(parse(text=temp))
  }
}






